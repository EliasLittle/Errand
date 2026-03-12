use std::collections::HashMap;

use crate::backend::errand_builtins::type_expr_to_errand_type;
use crate::backend::preir::{
    BinOpPl, FnCallPl, ForLoopData, IfStatementData, Instr, LiteralPl, RegionData, ReturnData,
    UnOpPl, VarDeclData, FuncData, instr_index,
};
use crate::backend::sir::{SIR, SIRFunctionInfo, SIRInstr, SIRModule, SIRStructField, SIRStructLayout};
use crate::backend::worklist::{ErrandInference, ErrandType};
use crate::frontend::ast::{Parameter, Program};
use crate::backend::preir::PreIR;

/// Generates SIR from PreIR in a single interleaved pass: each instruction is
/// typed and emitted simultaneously, mirroring Zig's Sema analyzeBody.
pub struct SirGen {
    pub inference: ErrandInference,
    /// Maps global PreIR instruction index -> local SIR index for the current
    /// function being processed. Reset for each function / main.
    preir_to_sir: HashMap<instr_index, instr_index>,
}

impl SirGen {
    fn new(inference: ErrandInference) -> Self {
        SirGen {
            inference,
            preir_to_sir: HashMap::new(),
        }
    }

    /// Main entry point. Builds a complete `SIRModule` from a `PreIR` in a
    /// single interleaved pass (typing + emission together).
    pub fn emit_sir_module(preir: PreIR, program: &Program) -> Result<SIRModule, String> {
        let inference = ErrandInference::with_preir_and_program(preir, program);
        let mut gen = SirGen::new(inference);

        // Collect FuncDecl and StructDecl metadata before any mutable borrows.
        struct FuncMeta {
            name: String,
            body_index: instr_index,
            parameters: Vec<Parameter>,
            return_type: Option<crate::frontend::ast::TypeExpression>,
            is_foreign: bool,
        }
        let function_meta: Vec<FuncMeta> = gen
            .inference
            .preir
            .instructions
            .iter()
            .filter_map(|instr| {
                if let Instr::FuncDecl(fd) = instr {
                    Some(FuncMeta {
                        name: fd.name.clone(),
                        body_index: fd.body_index,
                        parameters: fd.parameters.clone(),
                        return_type: fd.return_type.clone(),
                        is_foreign: fd.is_foreign,
                    })
                } else {
                    None
                }
            })
            .collect();

        // Build struct layouts from StructDecl instructions.
        let struct_layouts: HashMap<String, SIRStructLayout> = gen
            .inference
            .preir
            .instructions
            .iter()
            .filter_map(|instr| {
                if let Instr::StructDecl(sd) = instr {
                    let mut offset = 0usize;
                    let fields = sd
                        .fields
                        .iter()
                        .map(|f| {
                            let ty = type_expr_to_errand_type(&f.field_type);
                            let size = errand_type_size(&ty);
                            let field = SIRStructField {
                                name: f.id.name.clone(),
                                ty,
                                byte_offset: offset,
                            };
                            offset += size;
                            field
                        })
                        .collect();
                    Some((sd.name.clone(), SIRStructLayout { fields, total_size: offset }))
                } else {
                    None
                }
            })
            .collect();

        // Process main FIRST so module-level variables are in module_context
        // before functions are analyzed.
        gen.inference.setup_function_context(&[]);
        let region_data = if let Instr::Region(rd) = &gen.inference.preir.main.clone() {
            rd.clone()
        } else {
            return Err("Main is not a Region".to_string());
        };
        let main_sir = gen.emit_region_sir(&region_data)?;
        gen.inference.promote_var_context_to_module();

        // Process each function as an independent entry point.
        let mut functions: HashMap<String, HashMap<Vec<String>, SIRFunctionInfo>> = HashMap::new();
        for meta in function_meta {
            gen.inference.setup_function_context(&meta.parameters);
            let func_sir = gen.emit_body_sir(meta.body_index)?;

            // Resolve parameter types from the inference var_context (populated by
            // setup_function_context).
            let params: Vec<(String, ErrandType)> = meta
                .parameters
                .iter()
                .map(|p| {
                    let ty = gen
                        .inference
                        .var_context
                        .get(&p.id.name)
                        .cloned()
                        .unwrap_or_else(|| ErrandType::ETVar("?".to_string()));
                    (p.id.name.clone(), ty)
                })
                .collect();

            // Return type: the type annotation on the return_loc instruction, falling
            // back to the declared return type expression, then Void.
            let return_type = func_sir
                .instructions
                .get(func_sir.return_loc as usize)
                .and_then(|i| i.ty.clone())
                .or_else(|| meta.return_type.as_ref().map(type_expr_to_errand_type))
                .unwrap_or_else(|| ErrandType::Con("Void".to_string()));

            let type_key: Vec<String> =
                params.iter().map(|(_, ty)| errand_type_name(ty)).collect();

            let info = SIRFunctionInfo {
                params,
                return_type,
                is_foreign: meta.is_foreign,
                body: if meta.is_foreign { None } else { Some(func_sir) },
            };

            functions
                .entry(meta.name)
                .or_default()
                .insert(type_key, info);
        }

        Ok(SIRModule { main: main_sir, functions, structs: struct_layouts })
    }

    /// Emit SIR for a function body rooted at `root_idx`.
    /// Resets `preir_to_sir` so local indices start at 0.
    fn emit_body_sir(&mut self, root_idx: instr_index) -> Result<SIR, String> {
        self.preir_to_sir.clear();
        let mut sir = SIR { instructions: Vec::new(), return_loc: 0 };
        let local_root = self.analyze_and_emit(root_idx, &mut sir)?;
        sir.return_loc = local_root;
        Ok(sir)
    }

    /// Emit SIR for the main region. Walks `instr_start..instr_end` in forward
    /// order (same ordering as `analyze_region` in worklist.rs).
    fn emit_region_sir(&mut self, region_data: &RegionData) -> Result<SIR, String> {
        self.preir_to_sir.clear();
        let mut sir = SIR { instructions: Vec::new(), return_loc: 0 };

        for i in region_data.instr_start..region_data.instr_end {
            // Skip top-level declarations — they get their own SIR entry points.
            let instr = self.inference.preir.get_instruction(i).cloned();
            match &instr {
                Some(Instr::FuncDecl(_)) | Some(Instr::StructDecl(_)) => continue,
                _ => {}
            }
            self.analyze_and_emit(i, &mut sir)?;
        }

        sir.return_loc = self
            .preir_to_sir
            .get(&region_data.return_loc)
            .copied()
            .unwrap_or(0);
        Ok(sir)
    }

    /// Core per-instruction handler: type the instruction at `global_idx`, then
    /// emit a `SIRInstr` with remapped local operand indices into `sir`.
    ///
    /// Returns the local index assigned to this instruction.
    fn analyze_and_emit(
        &mut self,
        global_idx: instr_index,
        sir: &mut SIR,
    ) -> Result<instr_index, String> {
        // Already emitted — return cached local index (inst_map hit).
        if let Some(&local) = self.preir_to_sir.get(&global_idx) {
            return Ok(local);
        }

        // Clone the instruction to avoid borrow conflicts with inference.
        let instr = self
            .inference
            .preir
            .get_instruction(global_idx)
            .cloned()
            .ok_or_else(|| format!("Invalid PreIR instruction index: {}", global_idx))?;

        // For Region instructions, emit the contained instructions first so
        // their local indices are known before we emit the Region itself.
        if let Instr::Region(ref rd) = instr {
            return self.emit_region_instr(global_idx, rd.clone(), sir);
        }

        // For FuncDecl / StructDecl encountered inside a body (nested), skip
        // emission — they are handled as top-level entry points.
        if matches!(instr, Instr::FuncDecl(_) | Instr::StructDecl(_)) {
            // Still type-check them, but don't emit into the current SIR.
            let _ = self
                .inference
                .analyze_instr_index(global_idx)
                .map_err(|e| format!("{:?}", e))?;
            // Return a sentinel; callers that walk regions skip these.
            return Ok(-1);
        }

        // Comptime fold: typeof(arg) → Literal(Symbol(type_name)) : Type
        if let Instr::FnCall(ref call) = instr {
            if call.name == "typeof" && call.arguments.len() == 1 {
                let arg_global_idx = call.arguments[0];
                self.analyze_and_emit(arg_global_idx, sir)?;
                let resolved_ty = self.inference
                    .analyze_instr_index(arg_global_idx)
                    .unwrap_or(ErrandType::ETVar("?".to_string()));
                let type_name = errand_type_name(&resolved_ty);
                let local_idx = sir.instructions.len() as instr_index;
                sir.instructions.push(SIRInstr {
                    instr: Instr::Literal(LiteralPl::Symbol(type_name)),
                    ty: Some(ErrandType::Con("Type".to_string())),
                });
                self.preir_to_sir.insert(global_idx, local_idx);
                return Ok(local_idx);
            }
        }

        // Recursively emit all operand dependencies first, collecting their
        // local indices for remapping.
        let remapped = self.remap_operands(instr, sir)?;

        // Type the instruction (caches result in analysis_cache).
        let ty = self
            .inference
            .analyze_instr_index(global_idx)
            .map_err(|e| format!("{:?}", e))
            .ok();

        // Emit into SIR.
        let local_idx = sir.instructions.len() as instr_index;
        sir.instructions.push(SIRInstr { instr: remapped, ty });
        self.preir_to_sir.insert(global_idx, local_idx);

        Ok(local_idx)
    }

    /// Special handling for `Region` instructions: emit contained instructions
    /// in forward order, then emit the Region itself with remapped bounds.
    fn emit_region_instr(
        &mut self,
        global_idx: instr_index,
        rd: RegionData,
        sir: &mut SIR,
    ) -> Result<instr_index, String> {
        let new_start = sir.instructions.len() as instr_index;

        for i in rd.instr_start..rd.instr_end {
            let instr = self.inference.preir.get_instruction(i).cloned();
            match &instr {
                Some(Instr::FuncDecl(_)) | Some(Instr::StructDecl(_)) => {
                    // Type them but don't emit into this Region's SIR.
                    let _ = self.inference.analyze_instr_index(i);
                    continue;
                }
                _ => {}
            }
            self.analyze_and_emit(i, sir)?;
        }

        let new_end = sir.instructions.len() as instr_index;
        let new_return_loc = self.preir_to_sir.get(&rd.return_loc).copied().unwrap_or(new_start);

        let remapped_region = Instr::Region(RegionData {
            instr_start: new_start,
            instr_end: new_end,
            return_loc: new_return_loc,
        });

        // Type the Region instruction itself.
        let ty = self
            .inference
            .analyze_instr_index(global_idx)
            .map_err(|e| format!("{:?}", e))
            .ok();

        let local_idx = sir.instructions.len() as instr_index;
        sir.instructions.push(SIRInstr { instr: remapped_region, ty });
        self.preir_to_sir.insert(global_idx, local_idx);

        Ok(local_idx)
    }

    /// Recursively emit operand instructions and return a new `Instr` with all
    /// operand indices remapped to local SIR indices.
    fn remap_operands(&mut self, instr: Instr, sir: &mut SIR) -> Result<Instr, String> {
        match instr {
            // No operand indices — emit as-is.
            Instr::Literal(_) | Instr::VarRef(_) | Instr::StructDecl(_) => Ok(instr),

            Instr::VarDecl(data) => {
                let value = self.analyze_and_emit(data.value, sir)?;
                Ok(Instr::VarDecl(VarDeclData { name: data.name, value }))
            }

            Instr::UnOp(data) => {
                let operand = self.analyze_and_emit(data.operand, sir)?;
                Ok(Instr::UnOp(UnOpPl { op: data.op, operand }))
            }

            Instr::BinOp(data) => {
                let left = self.analyze_and_emit(data.left, sir)?;
                let right = self.analyze_and_emit(data.right, sir)?;
                Ok(Instr::BinOp(BinOpPl { op: data.op, left, right }))
            }

            Instr::FnCall(data) => {
                let mut arguments = Vec::new();
                for arg_idx in data.arguments {
                    arguments.push(self.analyze_and_emit(arg_idx, sir)?);
                }
                Ok(Instr::FnCall(FnCallPl { name: data.name, arguments }))
            }

            Instr::IfStatement(data) => {
                let condition = self.analyze_and_emit(data.condition, sir)?;
                let then_branch = self.analyze_and_emit(data.then_branch, sir)?;
                let else_branch = if let Some(e) = data.else_branch {
                    Some(self.analyze_and_emit(e, sir)?)
                } else {
                    None
                };
                Ok(Instr::IfStatement(IfStatementData { condition, then_branch, else_branch }))
            }

            Instr::WhileLoop(data) => {
                let condition = self.analyze_and_emit(data.condition, sir)?;
                let body = self.analyze_and_emit(data.body, sir)?;
                Ok(Instr::WhileLoop(crate::backend::preir::WhileLoopData { condition, body }))
            }

            Instr::ForLoop(data) => {
                let range = self.analyze_and_emit(data.range, sir)?;
                let body = self.analyze_and_emit(data.body, sir)?;
                Ok(Instr::ForLoop(ForLoopData { iterator: data.iterator, range, body }))
            }

            Instr::Return(data) => {
                let value = if let Some(v) = data.value {
                    Some(self.analyze_and_emit(v, sir)?)
                } else {
                    None
                };
                Ok(Instr::Return(ReturnData { value }))
            }

            Instr::FuncDecl(data) => {
                let body_index = self.analyze_and_emit(data.body_index, sir)?;
                Ok(Instr::FuncDecl(FuncData {
                    name: data.name,
                    parameters: data.parameters,
                    body_index,
                    return_type: data.return_type,
                    is_foreign: data.is_foreign,
                }))
            }

            // Region is handled separately in emit_region_instr.
            Instr::Region(_) => unreachable!("Region should be handled before remap_operands"),
        }
    }
}

pub(crate) fn errand_type_name(ty: &ErrandType) -> String {
    match ty {
        ErrandType::Con(n) | ErrandType::Var(n) | ErrandType::ETVar(n) => n.clone(),
        ErrandType::Arrow(_, _) => "Function".to_string(),
        ErrandType::Forall(_, _) => "Forall".to_string(),
        ErrandType::Product(_) => "Product".to_string(),
        ErrandType::Sum(_) => "Sum".to_string(),
    }
}

/// Returns the byte size of an `ErrandType` for struct field layout computation.
fn errand_type_size(ty: &ErrandType) -> usize {
    match ty {
        ErrandType::Con(n) => match n.as_str() {
            "Int32" => 4,
            "Bool" => 1,
            _ => 8, // Int, Float, String, pointers, structs, etc.
        },
        _ => 8,
    }
}
