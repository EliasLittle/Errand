//! SIR → Cranelift lowering pass.
//!
//! Converts a typed `SIRModule` (produced by `SirGen`) directly into a native
//! object file via Cranelift, operating on typed SIR instructions rather than
//! the raw AST.

use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use cranelift::prelude::*;
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{AbiParam, BlockArg, Function, InstBuilder, UserFuncName, Value};
use cranelift_codegen::settings::{self, Configurable};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::{Linkage, Module};
use cranelift_module::FuncId;
use cranelift_object::ObjectModule;

use crate::backend::built_in_methods::{
    emit_ffi, emit_getfield, emit_mem_load, emit_mem_store, emit_new, emit_printf,
};
use crate::backend::preir::{instr_index, Instr, LiteralPl};
use crate::backend::sir::{SIREnumLayout, SIRFunctionInfo, SIRModule, SIR};
use crate::backend::sir_gen::errand_type_name;
use crate::backend::structs::{Field as BackendField, Struct as BackendStruct, Type as BackendType};
use crate::backend::worklist::ErrandType;
use crate::frontend::ast::{BinaryOperator, UnaryOperator};

// ─── Core struct ─────────────────────────────────────────────────────────────

pub struct SIRLoweringPass {
    module: Option<ObjectModule>,
    isa: Option<Arc<dyn cranelift_codegen::isa::TargetIsa>>,
    /// True when compiling a function that makes calls (non-leaf) or is foreign.
    /// Used to avoid stack-allocating structs in the red zone.
    is_non_leaf: bool,
    /// Maps a canonical lookup key → FuncId.
    ///
    /// Keys:
    ///   - Non-overloaded / foreign builtins:  plain name  (e.g. `"malloc"`)
    ///   - User-declared foreign functions:     plain name  (e.g. `"strcpy"`)
    ///   - User-defined Errand functions:       mangled key (e.g. `"__strcpy_String_String"`)
    func_ids: HashMap<String, FuncId>,
    func_sigs: HashMap<String, cranelift_codegen::ir::Signature>,
    /// Overload table: plain name → list of (param-type-name strings, func_ids key).
    /// Only populated for functions declared by user code (foreign or Errand).
    func_overloads: HashMap<String, Vec<(Vec<String>, String)>>,
    string_counter: u32,
    struct_registry: HashMap<String, BackendStruct>,
    /// enum_name → full enum layout (includes variant field offsets and total size).
    enum_registry: HashMap<String, SIREnumLayout>,
    next_func_idx: u32,
}

impl SIRLoweringPass {
    pub fn new() -> Self {
        SIRLoweringPass {
            module: None,
            isa: None,
            is_non_leaf: false,
            func_ids: HashMap::new(),
            func_sigs: HashMap::new(),
            func_overloads: HashMap::new(),
            string_counter: 0,
            struct_registry: HashMap::new(),
            enum_registry: HashMap::new(),
            next_func_idx: 0,
        }
    }

    /// Compile a fully-typed `SIRModule` into native machine code.
    pub fn compile_sir_module(sir_module: &SIRModule) -> Result<Vec<u8>, String> {
        let mut pass = SIRLoweringPass::new();
        pass.run(sir_module)
    }

    fn run(&mut self, sir_module: &SIRModule) -> Result<Vec<u8>, String> {
        self.build_struct_registry(sir_module);
        self.build_enum_registry(sir_module);
        self.initialize_module()?;
        self.declare_builtins()?;
        self.declare_all_functions(sir_module)?;

        // Compile each non-foreign function that has a body.
        let all_overloads: Vec<(String, Vec<String>, SIRFunctionInfo)> = sir_module
            .functions
            .iter()
            .flat_map(|(name, overloads)| {
                overloads.iter().map(move |(key, info)| {
                    (name.clone(), key.clone(), info.clone())
                })
            })
            .collect();

        for (name, _key, info) in all_overloads {
            if info.is_foreign || info.body.is_none() {
                continue;
            }
            self.compile_function(&name, &info)?;
        }

        // Compile the main body.
        let main_func = self.compile_main(&sir_module.main)?;

        // Declare and define main in the module.
        let module = self.module.as_mut().ok_or("Module not initialised")?;
        let mut sig = module.make_signature();
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        let main_id = module
            .declare_function("main", Linkage::Export, &sig)
            .map_err(|e| format!("Failed to declare main: {}", e))?;
        let mut ctx = cranelift_codegen::Context::for_function(main_func);
        module
            .define_function(main_id, &mut ctx)
            .map_err(|e| format!("Failed to define main: {}", e))?;

        self.finalize_module()
    }

    // ─── Phase 1: build struct registry from SIRModule ───────────────────────

    fn build_struct_registry(&mut self, sir_module: &SIRModule) {
        for (name, layout) in &sir_module.structs {
            let backend_fields: Vec<BackendField> = layout
                .fields
                .iter()
                .map(|f| {
                    let ty = errand_type_to_backend(&f.ty);
                    BackendField::new(&f.name, f.byte_offset, ty)
                })
                .collect();
            let s = BackendStruct::new(name, backend_fields, layout.total_size);
            self.struct_registry.insert(name.clone(), s);
        }
    }

    fn build_enum_registry(&mut self, sir_module: &SIRModule) {
        for (name, layout) in &sir_module.enums {
            self.enum_registry.insert(name.clone(), layout.clone());
        }
    }

    /// Allocate `size` bytes on the stack and return a pointer (i64) to the slot.
    fn alloca_bytes(&self, size: i64, builder: &mut FunctionBuilder) -> cranelift_codegen::ir::Value {
        let slot = builder.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            size as u32,
            0,
        ));
        builder.ins().stack_addr(types::I64, slot, 0)
    }

    // ─── Phase 2: initialise Cranelift ObjectModule ───────────────────────────

    fn initialize_module(&mut self) -> Result<(), String> {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "true").unwrap();
        flag_builder.set("enable_verifier", "false").unwrap();
        flag_builder.set("enable_llvm_abi_extensions", "true").unwrap();

        let isa = cranelift_native::builder()
            .unwrap()
            .finish(settings::Flags::new(flag_builder))
            .unwrap();
        self.isa = Some(isa.clone());

        let object_builder = cranelift_object::ObjectBuilder::new(
            isa,
            "main",
            Box::new(|libcall| format!("{}", libcall)),
        )
        .map_err(|e| format!("Failed to create ObjectBuilder: {}", e))?;
        self.module = Some(cranelift_object::ObjectModule::new(object_builder));
        Ok(())
    }

    // ─── Phase 4: declare FFI builtins ───────────────────────────────────────

    fn declare_builtins(&mut self) -> Result<(), String> {
        let builtins: Vec<(&str, Vec<types::Type>, types::Type)> = vec![
            ("malloc", vec![types::I64], types::I64),
            ("free", vec![types::I64], types::I64),
            ("strlen", vec![types::I64], types::I64),
            ("strcpy", vec![types::I64, types::I64], types::I64),
            ("strcmp", vec![types::I64, types::I64], types::I32),
            // printf is declared with a minimal signature; emit_printf adjusts it at each call site.
            ("printf", vec![types::I64], types::I32),
        ];
        let module = self.module.as_mut().ok_or("Module not initialised")?;
        for (name, param_types, ret_type) in builtins {
            let mut sig = cranelift_codegen::ir::Signature::new(
                cranelift_codegen::isa::CallConv::SystemV,
            );
            for pt in &param_types {
                sig.params.push(AbiParam::new(*pt));
            }
            sig.returns.push(AbiParam::new(ret_type));
            // Attempt to declare; if already declared (e.g. as user foreign fn), skip.
            if let Ok(func_id) = module.declare_function(name, Linkage::Import, &sig) {
                self.func_ids.entry(name.to_string()).or_insert(func_id);
                self.func_sigs.entry(name.to_string()).or_insert(sig);
            }
        }
        Ok(())
    }

    // ─── Phase 5: declare user-defined functions from SIRModule ─────────────

    fn declare_all_functions(&mut self, sir_module: &SIRModule) -> Result<(), String> {
        for (name, overloads) in &sir_module.functions {
            for (type_keys, info) in overloads {
                let sig = self.make_signature_from_info(info)?;

                // Mangle names to avoid collision with C builtins and the synthesized
                // C `main` entry point. Foreign functions keep their plain C symbol.
                let mangled = if info.is_foreign {
                    name.clone()
                } else if type_keys.is_empty() {
                    format!("__{}__", name)
                } else {
                    format!("__{}_{}", name, type_keys.join("_"))
                };
                let sym = mangled.as_str();
                let linkage = if info.is_foreign { Linkage::Import } else { Linkage::Export };

                let module = self.module.as_mut().ok_or("Module not initialised")?;
                let func_id = match module.declare_function(sym, linkage, &sig) {
                    Ok(id) => id,
                    Err(_) => match module.get_name(sym) {
                        Some(cranelift_module::FuncOrDataId::Func(id)) => id,
                        _ => continue,
                    },
                };

                self.func_ids.entry(mangled.clone()).or_insert(func_id);
                self.func_sigs.entry(mangled.clone()).or_insert(sig);
                self.func_overloads
                    .entry(name.clone())
                    .or_default()
                    .push((type_keys.clone(), mangled));
            }
        }
        Ok(())
    }

    fn make_signature_from_info(
        &self,
        info: &SIRFunctionInfo,
    ) -> Result<cranelift_codegen::ir::Signature, String> {
        let mut sig =
            cranelift_codegen::ir::Signature::new(cranelift_codegen::isa::CallConv::SystemV);
        for (_, ty) in &info.params {
            sig.params.push(AbiParam::new(errand_type_to_cranelift(ty)));
        }
        sig.returns
            .push(AbiParam::new(errand_type_to_cranelift(&info.return_type)));
        Ok(sig)
    }

    /// Resolve a function call to a `FuncId` using SIR argument types for
    /// overload discrimination.
    fn resolve_func_id(&self, name: &str, arg_type_keys: &[String]) -> Option<FuncId> {
        // 1. Exact overload match.
        if let Some(overloads) = self.func_overloads.get(name) {
            for (param_types, key) in overloads {
                if param_types == arg_type_keys {
                    if let Some(&id) = self.func_ids.get(key) {
                        return Some(id);
                    }
                }
            }
            // 2. Fall back to first registered overload.
            for (_, key) in overloads {
                if let Some(&id) = self.func_ids.get(key) {
                    return Some(id);
                }
            }
        }
        // 3. Plain-name lookup (non-overloaded builtins).
        self.func_ids.get(name).copied()
    }

    // ─── Phase 6: compile a named function body from SIR ─────────────────────

    fn compile_function(&mut self, name: &str, info: &SIRFunctionInfo) -> Result<(), String> {
        let sir = match &info.body {
            Some(s) => s.clone(),
            None => return Ok(()), // foreign — no body to compile
        };

        // Compute the mangled key the same way declare_all_functions does.
        let type_keys: Vec<String> =
            info.params.iter().map(|(_, ty)| errand_type_name(ty)).collect();
        let mangled = if type_keys.is_empty() {
            format!("__{}__", name)
        } else {
            format!("__{}_{}", name, type_keys.join("_"))
        };

        let sig = match self.func_sigs.get(&mangled).or_else(|| self.func_sigs.get(name)) {
            Some(s) => s.clone(),
            None => return Err(format!("No signature for function '{}' (mangled: '{}')", name, mangled)),
        };

        let func_id = match self.func_ids.get(&mangled).or_else(|| self.func_ids.get(name)) {
            Some(&id) => id,
            None => return Err(format!("Function '{}' (mangled: '{}') not declared in module", name, mangled)),
        };

        let func_idx = self.next_func_idx;
        self.next_func_idx += 1;
        let mut func = Function::with_name_signature(UserFuncName::user(0, func_idx), sig.clone());
        let mut builder_ctx = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut func, &mut builder_ctx);

        let entry_block = builder.create_block();
        for (_, ty) in &info.params {
            builder.append_block_param(entry_block, errand_type_to_cranelift(ty));
        }
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        // Map function parameters to Cranelift Variables.
        let mut var_map: HashMap<String, Variable> = HashMap::new();
        let mut var_counter = 0usize;
        for (i, (param_name, param_ty)) in info.params.iter().enumerate() {
            let cl_ty = errand_type_to_cranelift(param_ty);
            let var = Variable::new(var_counter);
            var_counter += 1;
            builder.declare_var(var, cl_ty);
            let param_val = builder.block_params(entry_block)[i];
            builder.def_var(var, param_val);
            var_map.insert(param_name.clone(), var);
        }

        // Foreign functions and non-leaf functions must heap-allocate structs to avoid
        // red zone corruption when calling into C library code (e.g. printf).
        self.is_non_leaf = info.is_foreign || sir_contains_calls(&sir);

        let mut value_map: Vec<Option<Value>> = vec![None; sir.instructions.len()];
        let nested = compute_nested(&sir);

        for i in 0..sir.instructions.len() {
            if nested.contains(&i) {
                continue;
            }
            self.emit_instr(i, &sir, &mut builder, &mut value_map, &mut var_map, &mut var_counter)?;
            if block_is_terminated(&builder) {
                break;
            }
        }

        // Append a return if the body didn't terminate (e.g. implicit-return functions).
        if !block_is_terminated(&builder) {
            let ret_val = value_map[sir.return_loc as usize]
                .unwrap_or_else(|| builder.ins().iconst(types::I64, 0));
            let ret_type = sig
                .returns
                .get(0)
                .map(|a| a.value_type)
                .unwrap_or(types::I64);
            let coerced = coerce_value(ret_val, ret_type, &mut builder);
            builder.ins().return_(&[coerced]);
        }

        builder.finalize();

        let module = self.module.as_mut().ok_or("Module not initialised")?;
        let mut ctx = cranelift_codegen::Context::for_function(func);
        module
            .define_function(func_id, &mut ctx)
            .map_err(|e| format!("Failed to define function '{}': {}", name, e))?;
        Ok(())
    }

    // ─── Phase 7: compile the main body ──────────────────────────────────────

    fn compile_main(&mut self, sir: &SIR) -> Result<Function, String> {
        let func_idx = self.next_func_idx;
        self.next_func_idx += 1;

        let mut sig =
            cranelift_codegen::ir::Signature::new(cranelift_codegen::isa::CallConv::SystemV);
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));

        let mut func = Function::with_name_signature(UserFuncName::user(0, func_idx), sig);
        let mut builder_ctx = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut func, &mut builder_ctx);

        let entry_block = builder.create_block();
        builder.append_block_param(entry_block, types::I64);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        // Main body typically has calls (printf, main, etc.); treat as non-leaf.
        self.is_non_leaf = sir_contains_calls(sir);

        let mut value_map: Vec<Option<Value>> = vec![None; sir.instructions.len()];
        let mut var_map: HashMap<String, Variable> = HashMap::new();
        let mut var_counter = 0usize;

        let nested = compute_nested(sir);
        let mut last_val = builder.ins().iconst(types::I64, 0);

        for i in 0..sir.instructions.len() {
            if nested.contains(&i) {
                continue;
            }
            last_val = self.emit_instr(
                i,
                sir,
                &mut builder,
                &mut value_map,
                &mut var_map,
                &mut var_counter,
            )?;
            if block_is_terminated(&builder) {
                break;
            }
        }

        if !block_is_terminated(&builder) {
            let ret_val = if sir.return_loc >= 0 && (sir.return_loc as usize) < value_map.len() {
                value_map[sir.return_loc as usize].unwrap_or(last_val)
            } else {
                last_val
            };
            builder.ins().return_(&[ret_val]);
        }

        builder.finalize();
        Ok(func)
    }

    // ─── Finalise the module ──────────────────────────────────────────────────

    fn finalize_module(&mut self) -> Result<Vec<u8>, String> {
        let module = self.module.take().ok_or("Module not initialised")?;
        let product = module.finish();
        product
            .emit()
            .map(|b| b.to_vec())
            .map_err(|e| format!("Failed to emit object: {}", e))
    }

    // ─── Core instruction emitter ─────────────────────────────────────────────

    fn emit_instr(
        &mut self,
        idx: usize,
        sir: &SIR,
        builder: &mut FunctionBuilder,
        value_map: &mut Vec<Option<Value>>,
        var_map: &mut HashMap<String, Variable>,
        var_counter: &mut usize,
    ) -> Result<Value, String> {
        // Memoisation: already emitted in the current context.
        if let Some(v) = value_map[idx] {
            return Ok(v);
        }

        // Clone the instruction to avoid borrow conflicts.
        let instr = sir.instructions[idx].instr.clone();

        let val = match instr {
            // ─── Literals ─────────────────────────────────────────────────────
            Instr::Literal(ref lit) => match lit {
                LiteralPl::Int(n) => builder.ins().iconst(types::I64, *n),
                LiteralPl::Float(f) => builder.ins().f64const(*f),
                LiteralPl::Boolean(b) => builder.ins().iconst(types::I8, *b as i64),
                LiteralPl::Unit => builder.ins().iconst(types::I64, 0),
                LiteralPl::String(s) | LiteralPl::Symbol(s) => {
                    self.emit_string_literal(s, builder)?
                }
            },

            // ─── Variable reference ────────────────────────────────────────────
            Instr::VarRef(ref data) => {
                if let Some(&var) = var_map.get(&data.name) {
                    builder.use_var(var)
                } else {
                    return Err(format!("Undefined variable: {}", data.name));
                }
            }

            // ─── Variable declaration ──────────────────────────────────────────
            Instr::VarDecl(ref data) => {
                let rhs = self.emit_instr(
                    data.value as usize,
                    sir,
                    builder,
                    value_map,
                    var_map,
                    var_counter,
                )?;
                let rhs_type = builder.func.dfg.value_type(rhs);
                if let Some(&var) = var_map.get(&data.name) {
                    // Re-assignment: coerce to the existing variable's type.
                    let current_val = builder.use_var(var);
                    let var_type = builder.func.dfg.value_type(current_val);
                    let coerced = coerce_value(rhs, var_type, builder);
                    builder.def_var(var, coerced);
                    coerced
                } else {
                    let var = Variable::new(*var_counter);
                    *var_counter += 1;
                    builder.declare_var(var, rhs_type);
                    builder.def_var(var, rhs);
                    var_map.insert(data.name.clone(), var);
                    rhs
                }
            }

            // ─── Unary operation ───────────────────────────────────────────────
            Instr::UnOp(ref data) => {
                let operand = self.emit_instr(
                    data.operand as usize,
                    sir,
                    builder,
                    value_map,
                    var_map,
                    var_counter,
                )?;
                match data.op {
                    UnaryOperator::Negate => builder.ins().ineg(operand),
                    UnaryOperator::Not => builder.ins().bnot(operand),
                }
            }

            // ─── Binary operation ──────────────────────────────────────────────
            Instr::BinOp(ref data) => {
                if data.op == BinaryOperator::Assignment {
                    // Assignment: LHS is a VarRef, RHS is the new value.
                    let rhs = self.emit_instr(
                        data.right as usize,
                        sir,
                        builder,
                        value_map,
                        var_map,
                        var_counter,
                    )?;
                    if let Instr::VarRef(ref vr) = sir.instructions[data.left as usize].instr {
                        let rhs_type = builder.func.dfg.value_type(rhs);
                        if let Some(&var) = var_map.get(&vr.name) {
                            let current_val = builder.use_var(var);
                            let var_type = builder.func.dfg.value_type(current_val);
                            let coerced = coerce_value(rhs, var_type, builder);
                            builder.def_var(var, coerced);
                            coerced
                        } else {
                            let var = Variable::new(*var_counter);
                            *var_counter += 1;
                            builder.declare_var(var, rhs_type);
                            builder.def_var(var, rhs);
                            var_map.insert(vr.name.clone(), var);
                            rhs
                        }
                    } else {
                        return Err("Assignment LHS must be a VarRef".to_string());
                    }
                } else {
                    let lhs = self.emit_instr(
                        data.left as usize,
                        sir,
                        builder,
                        value_map,
                        var_map,
                        var_counter,
                    )?;
                    let rhs = self.emit_instr(
                        data.right as usize,
                        sir,
                        builder,
                        value_map,
                        var_map,
                        var_counter,
                    )?;
                    emit_binop(data.op.clone(), lhs, rhs, builder)
                }
            }

            // ─── Function call ─────────────────────────────────────────────────
            Instr::FnCall(ref data) => {
                let name = data.name.clone();
                let args = data.arguments.clone();
                self.emit_fn_call(&name, &args, sir, builder, value_map, var_map, var_counter)?
            }

            // ─── If statement ──────────────────────────────────────────────────
            Instr::IfStatement(ref data) => {
                let cond = self.emit_instr(
                    data.condition as usize,
                    sir,
                    builder,
                    value_map,
                    var_map,
                    var_counter,
                )?;
                let then_branch = data.then_branch;
                let else_branch = data.else_branch;

                let then_block = builder.create_block();
                let else_block = builder.create_block();
                let merge_block = builder.create_block();
                builder.append_block_param(merge_block, types::I64);
                builder.ins().brif(cond, then_block, &[], else_block, &[]);

                // Then branch.
                builder.switch_to_block(then_block);
                let then_val = self.emit_region_body(
                    then_branch as usize,
                    sir,
                    builder,
                    value_map,
                    var_map,
                    var_counter,
                )?;
                if !block_is_terminated(builder) {
                    builder
                        .ins()
                        .jump(merge_block, &[BlockArg::Value(then_val)]);
                }
                builder.seal_block(then_block);

                // Else branch.
                builder.switch_to_block(else_block);
                let else_val = if let Some(else_idx) = else_branch {
                    self.emit_region_body(
                        else_idx as usize,
                        sir,
                        builder,
                        value_map,
                        var_map,
                        var_counter,
                    )?
                } else {
                    builder.ins().iconst(types::I64, 0)
                };
                if !block_is_terminated(builder) {
                    builder
                        .ins()
                        .jump(merge_block, &[BlockArg::Value(else_val)]);
                }
                builder.seal_block(else_block);

                builder.switch_to_block(merge_block);
                builder.seal_block(merge_block);
                builder.block_params(merge_block)[0]
            }

            // ─── While loop ────────────────────────────────────────────────────
            Instr::WhileLoop(ref data) => {
                let condition_idx = data.condition;
                let body_idx = data.body;

                let loop_block = builder.create_block();
                let body_block = builder.create_block();
                let exit_block = builder.create_block();

                builder.ins().jump(loop_block, &[]);

                // Loop header: re-evaluate the condition fresh on every iteration.
                builder.switch_to_block(loop_block);
                let cond = self.emit_fresh(condition_idx as usize, sir, builder, var_map)?;
                builder.ins().brif(cond, body_block, &[], exit_block, &[]);
                builder.seal_block(loop_block);

                // Loop body.
                builder.switch_to_block(body_block);
                self.emit_region_body(
                    body_idx as usize,
                    sir,
                    builder,
                    value_map,
                    var_map,
                    var_counter,
                )?;
                if !block_is_terminated(builder) {
                    builder.ins().jump(loop_block, &[]);
                }
                builder.seal_block(body_block);

                // Exit.
                builder.switch_to_block(exit_block);
                builder.seal_block(exit_block);
                builder.ins().iconst(types::I64, 0)
            }

            // ─── For loop (basic stub) ─────────────────────────────────────────
            Instr::ForLoop(_) => {
                // ForLoop lowering is not yet implemented; emit 0.
                builder.ins().iconst(types::I64, 0)
            }

            // ─── Return ────────────────────────────────────────────────────────
            Instr::Return(ref data) => {
                let ret_val = if let Some(v_idx) = data.value {
                    self.emit_instr(
                        v_idx as usize,
                        sir,
                        builder,
                        value_map,
                        var_map,
                        var_counter,
                    )?
                } else {
                    builder.ins().iconst(types::I64, 0)
                };
                let func_ret_type = builder
                    .func
                    .signature
                    .returns
                    .get(0)
                    .map(|a| a.value_type)
                    .unwrap_or(types::I64);
                let coerced = coerce_value(ret_val, func_ret_type, builder);
                builder.ins().return_(&[coerced]);
                coerced
            }

            // ─── Region (block expression) ─────────────────────────────────────
            Instr::Region(ref data) => {
                let start = data.instr_start as usize;
                let end = data.instr_end as usize;
                let return_loc = data.return_loc;
                let nested_local = compute_nested_for_range(sir, start, end);
                for i in start..end {
                    if nested_local.contains(&i) {
                        continue;
                    }
                    self.emit_instr(i, sir, builder, value_map, var_map, var_counter)?;
                    if block_is_terminated(builder) {
                        break;
                    }
                }
                value_map[return_loc as usize]
                    .unwrap_or_else(|| builder.ins().iconst(types::I64, 0))
            }

            // ─── Enum variant access (unit variant) ───────────────────────────
            Instr::EnumVariantAccess(ref data) => {
                let layout = self
                    .enum_registry
                    .get(&data.enum_name)
                    .ok_or_else(|| format!("unknown enum `{}` at codegen", data.enum_name))?
                    .clone();
                let tag = layout
                    .variants
                    .iter()
                    .position(|v| v.name == data.variant)
                    .ok_or_else(|| {
                        format!(
                            "unknown enum variant `{}::{}` at codegen",
                            data.enum_name, data.variant
                        )
                    })? as i64;

                if layout.is_simple {
                    // Pure unit-tag enum — bare integer is sufficient.
                    builder.ins().iconst(types::I64, tag)
                } else {
                    // Mixed enum — allocate tagged union; unit variant has empty payload.
                    let total = layout.total_size as i64;
                    let enum_ptr = self.alloca_bytes(total, builder);
                    let tag_val = builder.ins().iconst(types::I64, tag);
                    builder.ins().store(MemFlags::new(), tag_val, enum_ptr, 0);
                    enum_ptr
                }
            }

            // ─── Enum variant construction (data-carrying variant) ─────────────
            Instr::EnumVariantConstruct(ref data) => {
                let layout = self
                    .enum_registry
                    .get(&data.enum_name)
                    .ok_or_else(|| format!("unknown enum `{}` at codegen", data.enum_name))?
                    .clone();
                let (tag_idx, variant_layout) = layout
                    .variants
                    .iter()
                    .enumerate()
                    .find(|(_, v)| v.name == data.variant)
                    .ok_or_else(|| {
                        format!(
                            "unknown enum variant `{}::{}` at codegen",
                            data.enum_name, data.variant
                        )
                    })?;

                let total = layout.total_size as i64;
                let enum_ptr = self.alloca_bytes(total, builder);

                // Write the tag at byte 0.
                let tag_val = builder.ins().iconst(types::I64, tag_idx as i64);
                builder.ins().store(MemFlags::new(), tag_val, enum_ptr, 0);

                // Write each field value at offset 8 + field.byte_offset.
                let field_layouts = variant_layout.fields.clone();
                for (i, field) in field_layouts.iter().enumerate() {
                    let arg_idx = data.arg_indices[i] as usize;
                    let field_val = self.emit_instr(arg_idx, sir, builder, value_map, var_map, var_counter)?;
                    let offset = 8 + field.byte_offset as i32;
                    builder.ins().store(MemFlags::new(), field_val, enum_ptr, offset);
                }

                enum_ptr
            }

            // ─── Match expression ─────────────────────────────────────────────
            Instr::Match(ref data) => {
                let enum_layout = self
                    .enum_registry
                    .get(&data.enum_name)
                    .ok_or_else(|| format!("match: unknown enum `{}` at codegen", data.enum_name))?
                    .clone();

                // Obtain the tag value from the scrutinee.
                // For simple (unit-only) enums the scrutinee IS the i64 tag.
                // For mixed enums it is a pointer; load the tag from byte 0.
                let scrutinee_val = self.emit_instr(
                    data.scrutinee as usize, sir, builder, value_map, var_map, var_counter,
                )?;
                let tag_val = if enum_layout.is_simple {
                    scrutinee_val
                } else {
                    builder.ins().load(types::I64, MemFlags::new(), scrutinee_val, 0)
                };

                // Create one Cranelift block per arm, one check block per arm, a
                // no-match block (fallthrough when no arm matches), and a merge block.
                let merge_block = builder.create_block();
                builder.append_block_param(merge_block, types::I64);
                let no_match_block = builder.create_block();

                let arm_blocks: Vec<_> = data.arms.iter().map(|_| builder.create_block()).collect();
                let check_blocks: Vec<_> = (0..data.arms.len()).map(|_| builder.create_block()).collect();

                // Jump from current block to the first check block.
                builder.ins().jump(check_blocks[0], &[]);

                for (i, arm) in data.arms.iter().enumerate() {
                    let arm_block = arm_blocks[i];
                    let next = if i + 1 < data.arms.len() { check_blocks[i + 1] } else { no_match_block };

                    // ── Check block ───────────────────────────────────────────
                    builder.switch_to_block(check_blocks[i]);
                    if let Some(tag) = arm.tag {
                        let expected = builder.ins().iconst(types::I64, tag);
                        let cmp = builder.ins().icmp(IntCC::Equal, tag_val, expected);
                        builder.ins().brif(cmp, arm_block, &[], next, &[]);
                    } else {
                        // Wildcard: always branch to arm, no fallthrough.
                        builder.ins().jump(arm_block, &[]);
                    }
                    builder.seal_block(check_blocks[i]);

                    // ── Arm body block ────────────────────────────────────────
                    builder.switch_to_block(arm_block);

                    // Bind extracted fields as Cranelift variables so that VarRef
                    // instructions in the body resolve correctly.
                    if !arm.bindings.is_empty() {
                        if let Some(tag) = arm.tag {
                            if let Some(variant_layout) = enum_layout.variants.get(tag as usize) {
                                for (field_idx, binding_name) in arm.bindings.iter().enumerate() {
                                    if let Some(field) = variant_layout.fields.get(field_idx) {
                                        let byte_off = 8 + field.byte_offset as i32;
                                        let field_val = builder.ins().load(
                                            types::I64, MemFlags::new(), scrutinee_val, byte_off,
                                        );
                                        let var = if let Some(&existing) = var_map.get(binding_name) {
                                            existing
                                        } else {
                                            let v = Variable::new(*var_counter);
                                            *var_counter += 1;
                                            builder.declare_var(v, types::I64);
                                            var_map.insert(binding_name.clone(), v);
                                            v
                                        };
                                        builder.def_var(var, field_val);
                                    }
                                }
                            }
                        }
                    }

                    let arm_val = self.emit_region_body(
                        arm.body as usize, sir, builder, value_map, var_map, var_counter,
                    )?;
                    if !block_is_terminated(builder) {
                        builder.ins().jump(merge_block, &[BlockArg::Value(arm_val)]);
                    }
                    builder.seal_block(arm_block);
                }

                // No-match block: no arm was taken (missing wildcard).
                builder.switch_to_block(no_match_block);
                builder.seal_block(no_match_block);
                let zero = builder.ins().iconst(types::I64, 0);
                builder.ins().jump(merge_block, &[BlockArg::Value(zero)]);

                builder.switch_to_block(merge_block);
                builder.seal_block(merge_block);
                builder.block_params(merge_block)[0]
            }

            // ─── Declarations (handled in collection pass; skipped here) ────────
            Instr::FuncDecl(_) | Instr::StructDecl(_) | Instr::EnumDecl(_) => builder.ins().iconst(types::I64, 0),
        };

        value_map[idx] = Some(val);
        Ok(val)
    }

    // ─── Emit a Region's body into the current Cranelift block ───────────────

    fn emit_region_body(
        &mut self,
        region_idx: usize,
        sir: &SIR,
        builder: &mut FunctionBuilder,
        value_map: &mut Vec<Option<Value>>,
        var_map: &mut HashMap<String, Variable>,
        var_counter: &mut usize,
    ) -> Result<Value, String> {
        if let Instr::Region(ref data) = sir.instructions[region_idx].instr.clone() {
            let start = data.instr_start as usize;
            let end = data.instr_end as usize;
            let return_loc = data.return_loc as usize;
            let nested_local = compute_nested_for_range(sir, start, end);
            for i in start..end {
                if nested_local.contains(&i) {
                    continue;
                }
                self.emit_instr(i, sir, builder, value_map, var_map, var_counter)?;
                if block_is_terminated(builder) {
                    break;
                }
            }
            // SirGen sometimes produces regions with an empty instr_start..instr_end
            // range when sub-instructions were pre-cached during emission. In that case
            // return_loc points outside the range. Emit it (and its dependencies) now
            // if it hasn't been computed yet.
            if value_map[return_loc].is_none() && !block_is_terminated(builder) {
                self.emit_instr(return_loc, sir, builder, value_map, var_map, var_counter)?;
            }
            Ok(value_map[return_loc]
                .unwrap_or_else(|| builder.ins().iconst(types::I64, 0)))
        } else {
            // Not a Region — emit the instruction directly.
            self.emit_instr(region_idx, sir, builder, value_map, var_map, var_counter)
        }
    }

    /// Re-emit an instruction and its dependencies without caching (used for
    /// while-loop conditions so that `VarRef` always calls `use_var`, picking
    /// up any mutations made by the loop body).
    fn emit_fresh(
        &mut self,
        idx: usize,
        sir: &SIR,
        builder: &mut FunctionBuilder,
        var_map: &HashMap<String, Variable>,
    ) -> Result<Value, String> {
        match &sir.instructions[idx].instr.clone() {
            Instr::Literal(ref lit) => Ok(match lit {
                LiteralPl::Int(n) => builder.ins().iconst(types::I64, *n),
                LiteralPl::Float(f) => builder.ins().f64const(*f),
                LiteralPl::Boolean(b) => builder.ins().iconst(types::I8, *b as i64),
                LiteralPl::Unit => builder.ins().iconst(types::I64, 0),
                LiteralPl::String(s) | LiteralPl::Symbol(s) => {
                    self.emit_string_literal(s, builder)?
                }
            }),
            Instr::VarRef(ref data) => {
                if let Some(&var) = var_map.get(&data.name) {
                    Ok(builder.use_var(var))
                } else {
                    Err(format!("Undefined variable in loop condition: {}", data.name))
                }
            }
            Instr::BinOp(ref data) => {
                let lhs = self.emit_fresh(data.left as usize, sir, builder, var_map)?;
                let rhs = self.emit_fresh(data.right as usize, sir, builder, var_map)?;
                Ok(emit_binop(data.op.clone(), lhs, rhs, builder))
            }
            Instr::UnOp(ref data) => {
                let operand = self.emit_fresh(data.operand as usize, sir, builder, var_map)?;
                Ok(match data.op {
                    UnaryOperator::Negate => builder.ins().ineg(operand),
                    UnaryOperator::Not => builder.ins().bnot(operand),
                })
            }
            Instr::FnCall(ref data) => {
                // For function calls in loop conditions, use cached values from value_map
                // if available; otherwise emit fresh. This is a simplification.
                let mut compiled_args = Vec::new();
                for &arg_idx in &data.arguments.clone() {
                    compiled_args.push(self.emit_fresh(arg_idx as usize, sir, builder, var_map)?);
                }
                let name = data.name.clone();
                if let Some(&func_id) = self.func_ids.get(&name) {
                    let func_ref = self
                        .module
                        .as_mut()
                        .unwrap()
                        .declare_func_in_func(func_id, &mut builder.func);
                    let call_inst = builder.ins().call(func_ref, &compiled_args);
                    let results = builder.inst_results(call_inst);
                    Ok(results
                        .first()
                        .copied()
                        .unwrap_or_else(|| builder.ins().iconst(types::I64, 0)))
                } else {
                    Ok(builder.ins().iconst(types::I64, 0))
                }
            }
            _ => Ok(builder.ins().iconst(types::I64, 0)),
        }
    }

    // ─── Function call dispatch ───────────────────────────────────────────────

    fn emit_fn_call(
        &mut self,
        name: &str,
        arg_indices: &[instr_index],
        sir: &SIR,
        builder: &mut FunctionBuilder,
        value_map: &mut Vec<Option<Value>>,
        var_map: &mut HashMap<String, Variable>,
        var_counter: &mut usize,
    ) -> Result<Value, String> {
        // Compile all arguments first.
        let mut compiled_args = Vec::new();
        for &arg_idx in arg_indices {
            compiled_args.push(self.emit_instr(
                arg_idx as usize,
                sir,
                builder,
                value_map,
                var_map,
                var_counter,
            )?);
        }

        match name {
            "new" => {
                if arg_indices.is_empty() {
                    return Err("'new' requires at least a type-name argument".to_string());
                }
                let type_name =
                    match &sir.instructions[arg_indices[0] as usize].instr {
                        Instr::Literal(LiteralPl::Symbol(s))
                        | Instr::Literal(LiteralPl::String(s)) => s.clone(),
                        _ => {
                            return Err(
                                "First argument to 'new' must be a symbol/string type name"
                                    .to_string(),
                            )
                        }
                    };
                let struct_info = self
                    .struct_registry
                    .get(&type_name)
                    .ok_or_else(|| format!("Type '{}' not found in struct registry", type_name))?
                    .clone();
                let malloc_id = self.func_ids.get("malloc").copied();
                let val = if let Some(id) = malloc_id {
                    let module = self.module.as_mut().unwrap();
                    let func_ptr: *mut _ = &mut *builder.func;
                    emit_new(
                        builder,
                        &struct_info,
                        &compiled_args,
                        Some((id, module, unsafe { &mut *func_ptr })),
                        self.is_non_leaf,
                    )
                } else {
                    emit_new(builder, &struct_info, &compiled_args, None, self.is_non_leaf)
                };
                Ok(val)
            }

            "printf" => {
                let printf_id = self.func_ids.get("printf").copied();
                let val = if let Some(id) = printf_id {
                    let module = self.module.as_mut().unwrap();
                    let func_ptr: *mut _ = &mut *builder.func;
                    emit_printf(
                        builder,
                        &compiled_args,
                        Some((id, module, unsafe { &mut *func_ptr })),
                    )
                } else {
                    return Err("printf not declared".to_string());
                };
                Ok(val)
            }

            "_mem_load" => {
                if compiled_args.len() != 2 {
                    return Err("_mem_load expects 2 arguments".to_string());
                }
                Ok(emit_mem_load(builder, compiled_args[0], compiled_args[1]))
            }

            "_mem_store" => {
                if compiled_args.len() != 3 {
                    return Err("_mem_store expects 3 arguments".to_string());
                }
                emit_mem_store(builder, compiled_args[0], compiled_args[1], compiled_args[2]);
                Ok(compiled_args[2])
            }

            "getfield" => {
                if arg_indices.len() < 3 {
                    return Err("getfield expects 3 arguments".to_string());
                }
                let field_sym = match &sir.instructions[arg_indices[1] as usize].instr {
                    Instr::Literal(LiteralPl::Symbol(s))
                    | Instr::Literal(LiteralPl::String(s)) => s.clone(),
                    _ => return Err("Second argument to getfield must be a symbol".to_string()),
                };
                let type_name = match &sir.instructions[arg_indices[2] as usize].instr {
                    Instr::Literal(LiteralPl::Symbol(s))
                    | Instr::Literal(LiteralPl::String(s)) => s.clone(),
                    _ => return Err("Third argument to getfield must be a symbol".to_string()),
                };
                let registry = self.struct_registry.clone();
                emit_getfield(builder, &registry, compiled_args[0], &field_sym, &type_name)
            }

            "ffi" => {
                if arg_indices.is_empty() {
                    return Err("'ffi' requires a function name argument".to_string());
                }
                let func_name = match &sir.instructions[arg_indices[0] as usize].instr {
                    Instr::Literal(LiteralPl::Symbol(s))
                    | Instr::Literal(LiteralPl::String(s)) => s.clone(),
                    _ => return Err("First argument to 'ffi' must be a string/symbol".to_string()),
                };
                let ffi_args: Vec<Value> = compiled_args.iter().skip(1).cloned().collect();
                let module = self.module.as_mut().unwrap();
                let func_ptr: *mut _ = &mut *builder.func;
                Ok(emit_ffi(
                    builder,
                    &func_name,
                    &ffi_args,
                    module,
                    unsafe { &mut *func_ptr },
                ))
            }

            "as_ptr" | "as_string" => {
                if compiled_args.len() != 1 {
                    return Err(format!("{} expects 1 argument", name));
                }
                Ok(compiled_args[0])
            }

            _ => {
                // Resolve using SIR argument types for overload disambiguation.
                let arg_type_keys: Vec<String> = arg_indices
                    .iter()
                    .map(|&idx| sir_type_key(sir.instructions[idx as usize].ty.as_ref()))
                    .collect();
                let func_id = self
                    .resolve_func_id(name, &arg_type_keys)
                    .ok_or_else(|| format!("Function not found: {} (arg types: {:?})", name, arg_type_keys))?;
                let func_ref = self
                    .module
                    .as_mut()
                    .unwrap()
                    .declare_func_in_func(func_id, &mut builder.func);
                let call_inst = builder.ins().call(func_ref, &compiled_args);
                let results = builder.inst_results(call_inst);
                Ok(results
                    .first()
                    .copied()
                    .unwrap_or_else(|| builder.ins().iconst(types::I64, 0)))
            }
        }
    }

    // ─── String/symbol literal ────────────────────────────────────────────────

    fn emit_string_literal(
        &mut self,
        s: &str,
        builder: &mut FunctionBuilder,
    ) -> Result<Value, String> {
        self.string_counter += 1;
        let sym_name = format!("static_str_{}", self.string_counter);
        let bytes: Vec<u8> = format!("{}\0", s).into_bytes();
        let module = self.module.as_mut().ok_or("Module not initialised")?;
        let data_id = module
            .declare_data(&sym_name, Linkage::Local, false, false)
            .map_err(|e| format!("Failed to declare string data: {}", e))?;
        let mut desc = cranelift_module::DataDescription::new();
        desc.define(bytes.into_boxed_slice());
        module
            .define_data(data_id, &desc)
            .map_err(|e| format!("Failed to define string data: {}", e))?;
        let gv = module.declare_data_in_func(data_id, &mut builder.func);
        Ok(builder.ins().global_value(types::I64, gv))
    }
}

// ─── Control-flow nested-set helpers ─────────────────────────────────────────

/// Compute the set of SIR instruction indices that are nested inside branch
/// bodies and must NOT be emitted during the top-level linear walk.
fn compute_nested(sir: &SIR) -> HashSet<usize> {
    compute_nested_for_range(sir, 0, sir.instructions.len())
}

fn compute_nested_for_range(sir: &SIR, start: usize, end: usize) -> HashSet<usize> {
    let mut nested = HashSet::new();
    for i in start..end {
        match &sir.instructions[i].instr {
            Instr::IfStatement(data) => {
                mark_nested_region(sir, data.then_branch as usize, &mut nested);
                if let Some(e) = data.else_branch {
                    mark_nested_region(sir, e as usize, &mut nested);
                }
            }
            Instr::WhileLoop(data) => {
                mark_nested_region(sir, data.body as usize, &mut nested);
            }
            Instr::ForLoop(data) => {
                mark_nested_region(sir, data.body as usize, &mut nested);
            }
            Instr::Match(data) => {
                for arm in &data.arms {
                    mark_nested_region(sir, arm.body as usize, &mut nested);
                }
            }
            _ => {}
        }
    }
    nested
}

/// Recursively mark a Region and all its contained instructions as nested.
///
/// SirGen sometimes produces Regions with empty `instr_start..instr_end` ranges when
/// sub-instructions were already cached (emitted earlier in the flat list) — in that
/// case the Region's `return_loc` still points to the correct result instruction,
/// which lives *before* the Region in the SIR array. We therefore also transitively
/// mark the `return_loc` chain as nested, regardless of the explicit range.
fn mark_nested_region(sir: &SIR, region_idx: usize, nested: &mut HashSet<usize>) {
    if region_idx >= sir.instructions.len() || nested.contains(&region_idx) {
        return;
    }
    nested.insert(region_idx);
    if let Instr::Region(ref data) = sir.instructions[region_idx].instr.clone() {
        let start = data.instr_start as usize;
        let end = data.instr_end as usize;
        let return_loc = data.return_loc as usize;

        // Mark every instruction explicitly listed in the range.
        //
        // Calling `mark_nested_deps_children` for every instruction is required
        // for correctness: SirGen may emit a dependency (e.g. a VarRef that is a
        // match-arm binding variable) earlier in the flat SIR list, *before* the
        // Region is emitted. That instruction's SIR index is therefore outside
        // `instr_start..instr_end`, but it still logically belongs to this Region
        // and must be marked as nested so the top-level loop doesn't try to
        // evaluate it before the binding is established.
        for i in start..end {
            if i >= sir.instructions.len() {
                break;
            }
            nested.insert(i);
            match &sir.instructions[i].instr {
                Instr::IfStatement(d) => {
                    mark_nested_region(sir, d.then_branch as usize, nested);
                    if let Some(e) = d.else_branch {
                        mark_nested_region(sir, e as usize, nested);
                    }
                }
                Instr::WhileLoop(d) => {
                    mark_nested_region(sir, d.body as usize, nested);
                }
                Instr::ForLoop(d) => {
                    mark_nested_region(sir, d.body as usize, nested);
                }
                Instr::Match(d) => {
                    for arm in &d.arms {
                        mark_nested_region(sir, arm.body as usize, nested);
                    }
                }
                _ => {}
            }
            // Transitively mark operand dependencies that may have been
            // pre-cached at SIR indices outside this range.
            mark_nested_deps_children(sir, i, nested);
        }

        // Additionally, transitively mark the return_loc and all its SIR-level
        // operand dependencies. This handles the common SirGen pattern where the
        // Region range is empty (instructions were pre-cached) but return_loc refers
        // to an instruction outside the range that still belongs to this branch.
        mark_nested_deps(sir, return_loc, nested);
    } else {
        // Non-Region arm body (e.g. a bare FnCall or BinOp expression).
        // `region_idx` is already in `nested`; mark its operand dependencies so
        // that binding variables (e.g. `x`, `y` from `Message::Move(x, y)`) are
        // not processed by the top-level region loop before the match arm has had
        // a chance to introduce them into `var_map`.
        mark_nested_deps_children(sir, region_idx, nested);
    }
}

/// Mark `idx` and all its transitive SIR operand dependencies as nested.
///
/// This is intentionally conservative: it only follows edges that are stored
/// *inside* the `Instr` (BinOp operands, FnCall args, etc.) and stops at leaf
/// instructions (Literal, VarRef). It does not follow Region or control-flow
/// instructions — those are handled by `mark_nested_region`.
fn mark_nested_deps(sir: &SIR, idx: usize, nested: &mut HashSet<usize>) {
    if idx >= sir.instructions.len() || nested.contains(&idx) {
        return;
    }
    nested.insert(idx);
    mark_nested_deps_children(sir, idx, nested);
}

/// Mark the transitive operand dependencies of the instruction at `idx` as
/// nested. Unlike `mark_nested_deps`, this does NOT guard on `idx` itself —
/// it is intended for use when `idx` is already in `nested` (e.g. a non-Region
/// match arm body that was inserted by `mark_nested_region`).
fn mark_nested_deps_children(sir: &SIR, idx: usize, nested: &mut HashSet<usize>) {
    match &sir.instructions[idx].instr.clone() {
        Instr::BinOp(d) => {
            mark_nested_deps(sir, d.left as usize, nested);
            mark_nested_deps(sir, d.right as usize, nested);
        }
        Instr::UnOp(d) => {
            mark_nested_deps(sir, d.operand as usize, nested);
        }
        Instr::FnCall(d) => {
            for &arg in &d.arguments {
                mark_nested_deps(sir, arg as usize, nested);
            }
        }
        Instr::Return(d) => {
            if let Some(v) = d.value {
                mark_nested_deps(sir, v as usize, nested);
            }
        }
        Instr::VarDecl(d) => {
            mark_nested_deps(sir, d.value as usize, nested);
        }
        // Leaf nodes — no further dependencies.
        Instr::Literal(_) | Instr::VarRef(_) => {}
        // Region and control-flow — handled by mark_nested_region; stop here.
        _ => {}
    }
}

/// Returns true if the SIR contains any function call (including inside regions,
/// if/while/for bodies). Used to determine if a function is non-leaf for stack
/// allocation (avoid red zone corruption).
fn sir_contains_calls(sir: &SIR) -> bool {
    fn contains_calls_in_range(sir: &SIR, start: usize, end: usize, seen: &mut HashSet<usize>) -> bool {
        for i in start..end.min(sir.instructions.len()) {
            if seen.contains(&i) {
                continue;
            }
            seen.insert(i);
            match &sir.instructions[i].instr {
                Instr::FnCall(_) => return true,
                Instr::Region(data) => {
                    if contains_calls_in_range(sir, data.instr_start as usize, data.instr_end as usize, seen) {
                        return true;
                    }
                    if contains_calls_in_range(sir, data.return_loc as usize, data.return_loc as usize + 1, seen) {
                        return true;
                    }
                }
                Instr::IfStatement(d) => {
                    if contains_calls_in_range(sir, d.then_branch as usize, d.then_branch as usize + 1, seen) {
                        return true;
                    }
                    if let Some(e) = d.else_branch {
                        if contains_calls_in_range(sir, e as usize, e as usize + 1, seen) {
                            return true;
                        }
                    }
                }
                Instr::WhileLoop(d) => {
                    if contains_calls_in_range(sir, d.condition as usize, d.condition as usize + 1, seen) {
                        return true;
                    }
                    if contains_calls_in_range(sir, d.body as usize, d.body as usize + 1, seen) {
                        return true;
                    }
                }
                Instr::ForLoop(d) => {
                    if contains_calls_in_range(sir, d.range as usize, d.range as usize + 1, seen) {
                        return true;
                    }
                    if contains_calls_in_range(sir, d.body as usize, d.body as usize + 1, seen) {
                        return true;
                    }
                }
                Instr::Match(d) => {
                    for arm in &d.arms {
                        if contains_calls_in_range(sir, arm.body as usize, arm.body as usize + 1, seen) {
                            return true;
                        }
                    }
                }
                _ => {}
            }
        }
        false
    }
    let mut seen = HashSet::new();
    contains_calls_in_range(sir, 0, sir.instructions.len(), &mut seen)
}

// ─── Small helpers ────────────────────────────────────────────────────────────

fn block_is_terminated(builder: &FunctionBuilder) -> bool {
    if let Some(block) = builder.current_block() {
        if let Some(last_inst) = builder.func.layout.last_inst(block) {
            return builder.func.dfg.insts[last_inst].opcode().is_terminator();
        }
    }
    false
}

fn coerce_value(val: Value, target: types::Type, builder: &mut FunctionBuilder) -> Value {
    let src = builder.func.dfg.value_type(val);
    if src == target {
        return val;
    }
    if src.is_int() && target.is_int() {
        return if target.bits() > src.bits() {
            builder.ins().uextend(target, val)
        } else {
            builder.ins().ireduce(target, val)
        };
    }
    val
}

fn emit_binop(op: BinaryOperator, lhs: Value, rhs: Value, builder: &mut FunctionBuilder) -> Value {
    match op {
        BinaryOperator::Add => builder.ins().iadd(lhs, rhs),
        BinaryOperator::Subtract => builder.ins().isub(lhs, rhs),
        BinaryOperator::Multiply => builder.ins().imul(lhs, rhs),
        BinaryOperator::Divide => builder.ins().sdiv(lhs, rhs),
        BinaryOperator::LessThan => {
            let r = builder.ins().icmp(IntCC::UnsignedLessThan, lhs, rhs);
            builder.ins().uextend(types::I64, r)
        }
        BinaryOperator::LessThanEqual => {
            let r = builder.ins().icmp(IntCC::UnsignedLessThanOrEqual, lhs, rhs);
            builder.ins().uextend(types::I64, r)
        }
        BinaryOperator::GreaterThan => {
            let r = builder.ins().icmp(IntCC::UnsignedGreaterThan, lhs, rhs);
            builder.ins().uextend(types::I64, r)
        }
        BinaryOperator::GreaterThanEqual => {
            let r = builder.ins().icmp(IntCC::UnsignedGreaterThanOrEqual, lhs, rhs);
            builder.ins().uextend(types::I64, r)
        }
        BinaryOperator::Equal => {
            let r = builder.ins().icmp(IntCC::Equal, lhs, rhs);
            builder.ins().uextend(types::I64, r)
        }
        BinaryOperator::NotEqual => {
            let r = builder.ins().icmp(IntCC::NotEqual, lhs, rhs);
            builder.ins().uextend(types::I64, r)
        }
        _ => builder.ins().iconst(types::I64, 0),
    }
}

/// Map an `ErrandType` to the corresponding Cranelift machine type.
fn errand_type_to_cranelift(ty: &ErrandType) -> types::Type {
    match ty {
        ErrandType::Con(n) => match n.as_str() {
            "Float" => types::F64,
            "Bool" => types::I8,
            "Int32" => types::I32,
            _ => types::I64, // Int, String, structs, pointers, Void, etc.
        },
        _ => types::I64,
    }
}

/// Map an `ErrandType` to the `BackendType` used by the struct registry.
fn errand_type_to_backend(ty: &ErrandType) -> BackendType {
    match ty {
        ErrandType::Con(n) => match n.as_str() {
            "Int32" => BackendType::Int32,
            "Float" => BackendType::Float,
            "Bool" => BackendType::Bool,
            "String" => BackendType::String,
            _ => BackendType::Int, // Int, pointers, Void, struct refs
        },
        _ => BackendType::Int,
    }
}

/// Convert an `ErrandType` (from SIR instruction type) to the overload key string.
fn sir_type_key(ty: Option<&ErrandType>) -> String {
    match ty {
        Some(ErrandType::Con(n)) => n.clone(),
        Some(ErrandType::Var(n)) | Some(ErrandType::ETVar(n)) => n.clone(),
        _ => "Any".to_string(),
    }
}
