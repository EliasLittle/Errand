use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};

use crate::backend::analysis::Analyzer;
use crate::backend::errand_builtins::{
    type_expr_to_errand_type, type_expr_to_errand_type_with_params,
};
use crate::backend::fir::FIR;
use crate::backend::fir::{
    BinOpPl, EnumData, EnumVariantConstructData, EnumVariantData, FnCallPl, ForLoopData, FuncData,
    IfStatementData, Instr, InstrIndex, LiteralPl, MatchArmData, MatchData, RegionData, ReturnData,
    StructData, UnOpPl, VarDeclData, WhileLoopData,
};
use crate::backend::sir::{
    SIREnumLayout, SIREnumVariantLayout, SIRFunctionInfo, SIRInstr, SIRModule, SIRStructField,
    SIRStructLayout, SIR,
};
use crate::backend::worklist::ErrandType;
use crate::frontend::ast::{GenericArg, Id, Parameter, Program, TypeExpression};
use tracing::instrument;

/// Generates SIR from FIR in a single interleaved pass: each instruction is
/// typed via `Analyzer` and emitted simultaneously.
pub struct SirGen {
    pub analyzer: Analyzer,
    /// Maps global FIR instruction index -> local SIR index for the function
    /// currently being processed.  Reset for each function / main.
    fir_to_sir: HashMap<InstrIndex, InstrIndex>,
}

impl SirGen {
    // ── Public API ─────────────────────────────────────────────────────────────

    #[instrument(
        skip(analyzer),
        name = "sir_gen.new",
        target = "sir_gen",
        level = "trace"
    )]
    fn new(analyzer: Analyzer) -> Self {
        SirGen {
            analyzer,
            fir_to_sir: HashMap::new(),
        }
    }

    /// Build a complete `SIRModule` from a `FIR`.
    #[instrument(
        skip(fir, program),
        fields(fir_len = fir.instructions.len()),
        name = "sir_gen.emit_sir_module",
        target = "sir_gen",
        level = "debug"
    )]
    pub fn emit_sir_module(fir: FIR, program: &Program) -> Result<SIRModule, String> {
        let analyzer = Analyzer::new(fir, program);
        let mut gen = SirGen::new(analyzer);

        // Collect function and struct metadata before any mutable borrows.
        struct FuncMeta {
            name: String,
            body_index: InstrIndex,
            parameters: Vec<Parameter>,
            return_type: Option<crate::frontend::ast::TypeExpression>,
            is_foreign: bool,
        }

        let function_meta: Vec<FuncMeta> = gen
            .analyzer
            .fir
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
        let struct_layouts: HashMap<String, SIRStructLayout> =
            gen.analyzer
                .fir
                .instructions
                .iter()
                .filter_map(|instr| {
                    if let Instr::StructDecl(sd) = instr {
                        if !sd.type_params.is_empty() {
                            return None;
                        }
                        let layout =
                            sir_layout_struct_fields_from_types(sd.fields.iter().map(|f| {
                                (f.id.name.clone(), type_expr_to_errand_type(&f.field_type))
                            }));
                        Some((sd.name.clone(), layout))
                    } else {
                        None
                    }
                })
                .collect();

        // Type-check user function bodies before main so call sites (e.g.
        // `make_pair(10, 20)`) resolve against real signatures instead of
        // the existential placeholders registered during FIR metadata scan.
        let struct_names: HashSet<String> = gen
            .analyzer
            .fir
            .instructions
            .iter()
            .filter_map(|instr| {
                if let Instr::StructDecl(sd) = instr {
                    Some(sd.name.clone())
                } else {
                    None
                }
            })
            .collect();
        let user_func_indices: Vec<(InstrIndex, Vec<Parameter>)> = gen
            .analyzer
            .fir
            .instructions
            .iter()
            .enumerate()
            .filter_map(|(idx, instr)| {
                if let Instr::FuncDecl(fd) = instr {
                    if fd.is_foreign || struct_names.contains(&fd.name) {
                        None
                    } else if fd.parameters.iter().any(|p| p.type_expr.is_none()) {
                        Some((idx as InstrIndex, fd.parameters.clone()))
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .collect();
        for (idx, parameters) in user_func_indices {
            gen.analyzer.setup_function_context(&parameters);
            let _ = gen.analyzer.analyze_instr(idx);
        }

        // Process main so module-level variables enter module_context
        // before any function body is emitted.
        gen.analyzer.setup_function_context(&[]);
        let region_data = match gen.analyzer.fir.main.clone() {
            Instr::Region(rd) => rd,
            _ => return Err("main is not a Region".into()),
        };
        let main_sir = gen.emit_region_sir(&region_data)?;
        gen.analyzer.promote_to_module();

        // Process each function as an independent entry point.
        let mut functions: HashMap<String, HashMap<Vec<String>, SIRFunctionInfo>> = HashMap::new();

        for meta in function_meta {
            gen.analyzer.setup_function_context(&meta.parameters);
            let func_sir = gen.emit_body_sir(meta.body_index)?;

            // Resolve parameter types directly from this overload's own FuncDecl.
            // Using the meta.parameters directly (rather than searching by name) is
            // critical for multiple dispatch: different overloads of the same function
            // have different parameter types and must produce different type_keys.
            let params: Vec<(String, ErrandType)> = meta
                .parameters
                .iter()
                .map(|p| {
                    let ty = match &p.type_expr {
                        Some(te) => type_expr_to_errand_type(te),
                        None => ErrandType::ETVar(format!("param_{}", p.id.name)),
                    };
                    (p.id.name.clone(), ty)
                })
                .collect();

            let return_type = func_sir
                .instructions
                .get(func_sir.return_loc as usize)
                .and_then(|i| i.ty.clone())
                .or_else(|| meta.return_type.as_ref().map(type_expr_to_errand_type))
                .unwrap_or_else(|| ErrandType::Con("Void".into()));

            let type_key: Vec<String> = params.iter().map(|(_, ty)| errand_type_name(ty)).collect();

            let info = SIRFunctionInfo {
                params,
                return_type,
                is_foreign: meta.is_foreign,
                body: if meta.is_foreign {
                    None
                } else {
                    Some(func_sir)
                },
            };

            functions
                .entry(meta.name)
                .or_default()
                .insert(type_key, info);
        }

        // Collect enum layouts from EnumDecl instructions, computing tagged-union sizes.
        let enum_layouts: HashMap<String, SIREnumLayout> = gen
            .analyzer
            .fir
            .instructions
            .iter()
            .filter_map(|instr| {
                if let Instr::EnumDecl(ed) = instr {
                    if !ed.type_params.is_empty() {
                        return None;
                    }
                    let mut max_payload = 0usize;
                    let variant_layouts: Vec<SIREnumVariantLayout> = ed
                        .variants
                        .iter()
                        .map(|v| {
                            let inner = sir_layout_struct_fields_from_types(v.fields.iter().map(
                                |(field_name, field_type)| {
                                    (field_name.clone(), type_expr_to_errand_type(field_type))
                                },
                            ));
                            let payload_size = inner.total_size;
                            if payload_size > max_payload {
                                max_payload = payload_size;
                            }
                            SIREnumVariantLayout {
                                name: v.name.clone(),
                                fields: inner.fields,
                                payload_size,
                            }
                        })
                        .collect();

                    let is_simple = variant_layouts.iter().all(|v| v.fields.is_empty());
                    // total_size = 8 bytes for tag + max payload; 0 when is_simple (bare int).
                    let total_size = if is_simple { 0 } else { 8 + max_payload };
                    Some((
                        ed.name.clone(),
                        SIREnumLayout {
                            variants: variant_layouts,
                            total_size,
                            is_simple,
                        },
                    ))
                } else {
                    None
                }
            })
            .collect();

        let mut module = SIRModule {
            main: main_sir,
            functions,
            structs: struct_layouts,
            enums: enum_layouts,
        };
        gen.finalize_generics(&mut module)?;
        Ok(module)
    }

    // ── Emission (FIR → SIR) ─────────────────────────────────────────────────

    /// Emit SIR for a function body rooted at `root_idx`.
    /// Resets the index map so local indices start at 0.
    #[instrument(
        skip(self),
        fields(root_idx),
        name = "sir_gen.emit_body_sir",
        target = "sir_gen",
        level = "trace"
    )]
    fn emit_body_sir(&mut self, root_idx: InstrIndex) -> Result<SIR, String> {
        self.fir_to_sir.clear();
        let mut sir = SIR {
            instructions: Vec::new(),
            return_loc: 0,
        };
        let local_root = self.analyze_and_emit(root_idx, &mut sir)?;
        sir.return_loc = local_root;
        Ok(sir)
    }

    /// Emit SIR for the main region: walk `instr_start..instr_end` in order,
    /// skipping top-level declarations (they have their own entry points).
    #[instrument(
        skip(self, region_data),
        fields(
            instr_start = region_data.instr_start,
            instr_end = region_data.instr_end,
            return_loc = region_data.return_loc
        ),
        name = "sir_gen.emit_region_sir",
        target = "sir_gen",
        level = "trace"
    )]
    fn emit_region_sir(&mut self, region_data: &RegionData) -> Result<SIR, String> {
        self.fir_to_sir.clear();
        let mut sir = SIR {
            instructions: Vec::new(),
            return_loc: 0,
        };

        // Instructions that belong to match arm body Regions are deferred so
        // they are emitted lazily inside their Region's SIR range. This ensures
        // that binding variables (e.g. `x`, `y` from `Message::Move(x, y)`)
        // have SIR indices that fall inside the arm body Region's range, which
        // is required for the lowering pass to correctly mark them as nested.
        self.emit_region_instruction_range(
            region_data.instr_start,
            region_data.instr_end,
            &mut sir,
            /* nested_decls */ false,
        )?;

        sir.return_loc = self
            .fir_to_sir
            .get(&region_data.return_loc)
            .copied()
            .unwrap_or(0);
        Ok(sir)
    }

    /// Emit the instructions contained in a `Region`, then emit the Region
    /// node itself with adjusted local bounds.
    #[instrument(
        skip(self, rd, sir),
        fields(global_idx, instr_start = rd.instr_start, instr_end = rd.instr_end),
        name = "sir_gen.emit_region_instr",
        target = "sir_gen",
        level = "trace"
    )]
    fn emit_region_instr(
        &mut self,
        global_idx: InstrIndex,
        rd: RegionData,
        sir: &mut SIR,
    ) -> Result<InstrIndex, String> {
        let new_start = sir.instructions.len() as InstrIndex;

        self.emit_region_instruction_range(rd.instr_start, rd.instr_end, sir, true)?;

        let new_end = sir.instructions.len() as InstrIndex;
        let new_return_loc = self
            .fir_to_sir
            .get(&rd.return_loc)
            .copied()
            .unwrap_or(new_start);

        let remapped_region = Instr::Region(RegionData {
            instr_start: new_start,
            instr_end: new_end,
            return_loc: new_return_loc,
        });

        let ty = self
            .analyzer
            .analyze_instr(global_idx)
            .map(|idx| {
                self.analyzer
                    .collapse_apps_in_type(self.analyzer.expanded_pool_type(idx))
            })
            .ok();

        let local_idx = sir.instructions.len() as InstrIndex;
        sir.instructions.push(SIRInstr {
            instr: remapped_region,
            ty,
        });
        self.fir_to_sir.insert(global_idx, local_idx);

        Ok(local_idx)
    }

    /// Collect all FIR instruction indices that are "owned" by match arm body
    /// Regions within the given FIR range. These must not be pre-emitted in
    /// a sequential scan; instead they must be emitted lazily inside the arm
    /// body's `emit_region_instr` call so that their SIR indices fall within
    /// the Region's `instr_start..instr_end` range.
    #[instrument(
        skip(self),
        fields(start, end),
        name = "sir_gen.collect_arm_owned_fir",
        target = "sir_gen",
        level = "trace"
    )]
    fn collect_arm_owned_fir(&self, start: InstrIndex, end: InstrIndex) -> HashSet<InstrIndex> {
        let mut owned = HashSet::new();
        for i in start..end {
            if let Some(Instr::Match(data)) = self.analyzer.fir.get_instruction(i) {
                for arm in &data.arms {
                    self.mark_arm_owned_fir(arm.body, &mut owned);
                }
            }
        }
        owned
    }

    /// Recursively mark `idx` and all FIR instructions inside it as arm-owned.
    #[instrument(
        skip(self, owned),
        fields(idx),
        name = "sir_gen.mark_arm_owned_fir",
        target = "sir_gen",
        level = "trace"
    )]
    fn mark_arm_owned_fir(&self, idx: InstrIndex, owned: &mut HashSet<InstrIndex>) {
        if owned.contains(&idx) {
            return;
        }
        owned.insert(idx);
        match self.analyzer.fir.get_instruction(idx) {
            Some(Instr::Region(data)) => {
                let start = data.instr_start;
                let end = data.instr_end;
                for i in start..end {
                    self.mark_arm_owned_fir(i, owned);
                }
            }
            Some(Instr::Match(data)) => {
                for arm in &data.arms {
                    self.mark_arm_owned_fir(arm.body, owned);
                }
            }
            _ => {}
        }
    }

    /// FIR lays out `while cond do (block)` as: `cond…`, `block` statements, the block's
    /// `Region` node, then the `WhileLoop`. A linear scan would emit the block twice (once
    /// while walking the parent range, again when lowering the `WhileLoop`), leaving the
    /// `WhileLoop`'s body `Region` empty in SIR because `fir_to_sir` is already populated.
    /// Skip the operand `Region` and its `instr_start..instr_end` range whenever we see a
    /// control-flow instruction that owns them.
    #[instrument(
        skip(self),
        fields(start, end),
        name = "sir_gen.control_flow_operand_skip_indices",
        target = "sir_gen",
        level = "trace"
    )]
    fn control_flow_operand_skip_indices(
        &self,
        start: InstrIndex,
        end: InstrIndex,
    ) -> HashSet<InstrIndex> {
        let mut skip = HashSet::new();
        let fir = &self.analyzer.fir;
        for i in start..end {
            match fir.get_instruction(i) {
                Some(Instr::WhileLoop(d)) => Self::mark_region_body_skip(fir, d.body, &mut skip),
                Some(Instr::ForLoop(d)) => Self::mark_region_body_skip(fir, d.body, &mut skip),
                Some(Instr::IfStatement(d)) => {
                    Self::mark_region_body_skip(fir, d.then_branch, &mut skip);
                    if let Some(e) = d.else_branch {
                        Self::mark_region_body_skip(fir, e, &mut skip);
                    }
                }
                _ => {}
            }
        }
        skip
    }

    #[instrument(
        skip(fir, skip),
        fields(body),
        name = "sir_gen.mark_region_body_skip",
        target = "sir_gen",
        level = "trace"
    )]
    fn mark_region_body_skip(fir: &FIR, body: InstrIndex, skip: &mut HashSet<InstrIndex>) {
        if let Some(Instr::Region(rd)) = fir.get_instruction(body) {
            skip.insert(body);
            for j in rd.instr_start..rd.instr_end {
                skip.insert(j);
            }
        }
    }

    /// Walk `instr_start..instr_end` in FIR order for region emission: defer match-arm-owned
    /// instructions and control-flow-owned region bodies, optionally skip or analyze nested
    /// module-level declarations.
    #[instrument(
        skip(self, sir),
        fields(instr_start, instr_end, nested_decls),
        name = "sir_gen.emit_region_instruction_range",
        target = "sir_gen",
        level = "trace"
    )]
    fn emit_region_instruction_range(
        &mut self,
        instr_start: InstrIndex,
        instr_end: InstrIndex,
        sir: &mut SIR,
        nested_decls: bool,
    ) -> Result<(), String> {
        let arm_owned = self.collect_arm_owned_fir(instr_start, instr_end);
        let cf_skip = self.control_flow_operand_skip_indices(instr_start, instr_end);
        for i in instr_start..instr_end {
            match self.analyzer.fir.get_instruction(i) {
                Some(Instr::FuncDecl(_))
                | Some(Instr::StructDecl(_))
                | Some(Instr::EnumDecl(_)) => {
                    if nested_decls {
                        let _ = self.analyzer.analyze_instr(i);
                    }
                    continue;
                }
                _ => {}
            }
            if arm_owned.contains(&i) || cf_skip.contains(&i) {
                continue;
            }
            self.analyze_and_emit(i, sir)?;
        }
        Ok(())
    }

    /// Type the instruction at `global_idx` via `Analyzer`, emit a `SIRInstr`
    /// with remapped local indices, and return the local index.
    #[instrument(
        skip(self, sir),
        fields(global_idx),
        name = "sir_gen.analyze_and_emit",
        target = "sir_gen",
        level = "trace"
    )]
    fn analyze_and_emit(
        &mut self,
        global_idx: InstrIndex,
        sir: &mut SIR,
    ) -> Result<InstrIndex, String> {
        if let Some(&local) = self.fir_to_sir.get(&global_idx) {
            return Ok(local);
        }

        let instr = self
            .analyzer
            .fir
            .get_instruction(global_idx)
            .cloned()
            .ok_or_else(|| format!("invalid FIR index: {global_idx}"))?;

        // Region instructions: emit contained body first, then the Region node.
        if let Instr::Region(ref rd) = instr {
            return self.emit_region_instr(global_idx, rd.clone(), sir);
        }

        // Declarations nested inside a body: analyze only, don't emit into
        // this SIR (they are independent entry points at the module level).
        if matches!(
            instr,
            Instr::FuncDecl(_) | Instr::StructDecl(_) | Instr::EnumDecl(_)
        ) {
            let _ = self
                .analyzer
                .analyze_instr(global_idx)
                .map_err(|e| format!("{e:?}"));
            return Ok(-1);
        }

        // Track up front whether this is a var declaration with an explicit
        // type annotation; those have to surface type errors loudly so
        // mismatches between the annotation and the initializer (e.g.
        // `d::Pair<Int, Int> = Pair("hello", "world")`) actually fail
        // compilation. Errors on other instructions are swallowed because
        // the type system has known gaps (enums-as-int coercion, raw
        // pointer arithmetic, untyped parameters) that legitimate programs
        // rely on.
        let is_annotated_var_decl = matches!(
            &instr,
            Instr::VarDecl(data) if data.declared_type.is_some()
        );

        // Recursively emit operand instructions and remap their indices.
        let remapped = self.remap_operands(instr, sir)?;

        let analysis = self.analyzer.analyze_instr(global_idx);
        if is_annotated_var_decl {
            analysis.as_ref().map_err(|e| format!("{e:?}"))?;
        }
        let ty = analysis
            .map(|idx| {
                self.analyzer
                    .collapse_apps_in_type(self.analyzer.expanded_pool_type(idx))
            })
            .ok();

        let local_idx = sir.instructions.len() as InstrIndex;
        sir.instructions.push(SIRInstr {
            instr: remapped,
            ty,
        });
        self.fir_to_sir.insert(global_idx, local_idx);

        Ok(local_idx)
    }

    // ── Operand remapping (during emission) ───────────────────────────────────

    /// Return a new `Instr` with all operand `InstrIndex` values remapped from
    /// global FIR space to local SIR space, emitting dependencies first.
    #[instrument(
        skip(self, instr, sir),
        name = "sir_gen.remap_operands",
        target = "sir_gen",
        level = "trace"
    )]
    fn remap_operands(&mut self, instr: Instr, sir: &mut SIR) -> Result<Instr, String> {
        match instr {
            Instr::Literal(_)
            | Instr::VarRef(_)
            | Instr::StructDecl(_)
            | Instr::EnumDecl(_)
            | Instr::EnumVariantAccess(_) => Ok(instr),
            Instr::EnumVariantConstruct(data) => self.remap_enum_variant_construct(data, sir),
            Instr::Match(data) => self.remap_match_instr(data, sir),
            Instr::VarDecl(data) => self.remap_var_decl(data, sir),
            Instr::UnOp(data) => self.remap_un_op(data, sir),
            Instr::BinOp(data) => self.remap_bin_op(data, sir),
            Instr::FnCall(data) => self.remap_fn_call(data, sir),
            Instr::Typeof(operand) => self.remap_typeof(operand, sir),
            Instr::IfStatement(data) => self.remap_if_statement(data, sir),
            Instr::WhileLoop(data) => self.remap_while_loop(data, sir),
            Instr::ForLoop(data) => self.remap_for_loop(data, sir),
            Instr::Return(data) => self.remap_return(data, sir),
            Instr::FuncDecl(data) => self.remap_func_decl(data, sir),
            Instr::Region(_) => unreachable!("Region handled before remap_operands"),
            // Builtin operations are produced by `remap_fn_call`, never present
            // in the FIR fed into remapping.
            Instr::New(_)
            | Instr::Printf(_)
            | Instr::MemLoad(_)
            | Instr::MemStore(_)
            | Instr::GetField(_)
            | Instr::Ffi(_)
            | Instr::AsPtr(_)
            | Instr::AsString(_) => {
                unreachable!("builtin operations are produced by remap_fn_call, not present in FIR")
            }
        }
    }

    #[instrument(
        skip(self, data, sir),
        fields(enum_name = %data.enum_name, variant = %data.variant),
        name = "sir_gen.remap_enum_variant_construct",
        target = "sir_gen",
        level = "trace"
    )]
    fn remap_enum_variant_construct(
        &mut self,
        data: EnumVariantConstructData,
        sir: &mut SIR,
    ) -> Result<Instr, String> {
        let mut arg_indices = Vec::new();
        for idx in data.arg_indices {
            arg_indices.push(self.analyze_and_emit(idx, sir)?);
        }
        Ok(Instr::EnumVariantConstruct(EnumVariantConstructData {
            enum_name: data.enum_name,
            variant: data.variant,
            arg_indices,
        }))
    }

    #[instrument(
        skip(self, data, sir),
        fields(enum_name = %data.enum_name),
        name = "sir_gen.remap_match_instr",
        target = "sir_gen",
        level = "trace"
    )]
    fn remap_match_instr(&mut self, data: MatchData, sir: &mut SIR) -> Result<Instr, String> {
        let scrutinee = self.analyze_and_emit(data.scrutinee, sir)?;

        // Obtain variant info so we can register binding variable types
        // into global_defs before emitting each arm body.
        let variants: Vec<_> = self
            .analyzer
            .enum_variants
            .get(&data.enum_name)
            .cloned()
            .unwrap_or_default();
        let enum_tparams = self
            .analyzer
            .enum_type_params
            .get(&data.enum_name)
            .cloned()
            .unwrap_or_default();
        let scrut_ty = self
            .analyzer
            .cached_type(data.scrutinee)
            .map(|idx| self.analyzer.expanded_pool_type(idx));
        let mut scrut_subst: HashMap<String, ErrandType> = HashMap::new();
        if let Some(ErrandType::App(head, args)) = &scrut_ty {
            if let ErrandType::Con(en) = head.as_ref() {
                if en == &data.enum_name && args.len() == enum_tparams.len() {
                    for (p, a) in enum_tparams.iter().zip(args.iter()) {
                        scrut_subst.insert(p.clone(), a.clone());
                    }
                }
            }
        }

        let mut arms = Vec::new();
        for arm in &data.arms {
            // Register binding names as typed globals so that VarRef
            // instructions in the arm body resolve during analysis.
            if !arm.bindings.is_empty() {
                if let Some(tag) = arm.tag {
                    if let Some(variant_info) = variants.get(tag as usize) {
                        for (i, binding_name) in arm.bindings.iter().enumerate() {
                            if let Some((_, field_type)) = variant_info.fields.get(i) {
                                let mut ty =
                                    type_expr_to_errand_type_with_params(field_type, &enum_tparams);
                                ty = self.analyzer.apply_substs_to_type(&ty, &scrut_subst);
                                let ty_idx = self.analyzer.pool.intern(ty);
                                self.analyzer
                                    .global_defs
                                    .insert(binding_name.clone(), ty_idx);
                            }
                        }
                    }
                }
            }
            let body = self.analyze_and_emit(arm.body, sir)?;
            arms.push(MatchArmData {
                tag: arm.tag,
                bindings: arm.bindings.clone(),
                body,
            });
        }
        Ok(Instr::Match(MatchData {
            scrutinee,
            enum_name: data.enum_name.clone(),
            arms,
        }))
    }

    #[instrument(
        skip(self, data, sir),
        fields(name = %data.name),
        name = "sir_gen.remap_var_decl",
        target = "sir_gen",
        level = "trace"
    )]
    fn remap_var_decl(&mut self, data: VarDeclData, sir: &mut SIR) -> Result<Instr, String> {
        let value = self.analyze_and_emit(data.value, sir)?;
        Ok(Instr::VarDecl(VarDeclData {
            name: data.name,
            value,
            declared_type: data.declared_type.clone(),
        }))
    }

    #[instrument(
        skip(self, data, sir),
        name = "sir_gen.remap_un_op",
        target = "sir_gen",
        level = "trace"
    )]
    fn remap_un_op(&mut self, data: UnOpPl, sir: &mut SIR) -> Result<Instr, String> {
        let operand = self.analyze_and_emit(data.operand, sir)?;
        Ok(Instr::UnOp(UnOpPl {
            op: data.op,
            operand,
        }))
    }

    #[instrument(
        skip(self, data, sir),
        name = "sir_gen.remap_bin_op",
        target = "sir_gen",
        level = "trace"
    )]
    fn remap_bin_op(&mut self, data: BinOpPl, sir: &mut SIR) -> Result<Instr, String> {
        let left = self.analyze_and_emit(data.left, sir)?;
        let right = self.analyze_and_emit(data.right, sir)?;
        Ok(Instr::BinOp(BinOpPl {
            op: data.op,
            left,
            right,
        }))
    }

    #[instrument(
        skip(self, data, sir),
        fields(name = %data.name),
        name = "sir_gen.remap_fn_call",
        target = "sir_gen",
        level = "trace"
    )]
    fn remap_fn_call(&mut self, data: FnCallPl, sir: &mut SIR) -> Result<Instr, String> {
        let mut arguments = Vec::new();
        for arg_idx in data.arguments {
            arguments.push(self.analyze_and_emit(arg_idx, sir)?);
        }
        // Rewrite calls to compiler intrinsics into their dedicated builtin
        // instructions so codegen dispatches on them like any other
        // instruction, instead of matching on the function name. Analysis has
        // already typed this as a `FnCall`, so the rewrite is purely
        // structural and happens only after typing.
        match Instr::builtin_from_call(&data.name, &arguments) {
            Some(builtin) => Ok(builtin),
            None => Ok(Instr::FnCall(FnCallPl {
                name: data.name,
                arguments,
            })),
        }
    }

    #[instrument(
        skip(self, sir),
        fields(operand),
        name = "sir_gen.remap_typeof",
        target = "sir_gen",
        level = "trace"
    )]
    fn remap_typeof(&mut self, operand: InstrIndex, sir: &mut SIR) -> Result<Instr, String> {
        // Emit the operand so its analysis cache is populated, then
        // resolve its (mangled) type name and replace this instruction
        // with a Symbol literal carrying that name.  The operand SIR
        // instruction may end up dead but that's fine — `getfield`
        // and friends only consume the resulting symbol.
        self.analyze_and_emit(operand, sir)?;
        let ty = self
            .analyzer
            .cached_type(operand)
            .map(|idx| {
                self.analyzer
                    .collapse_apps_in_type(self.analyzer.expanded_pool_type(idx))
            })
            .ok_or_else(|| format!("typeof: cannot resolve type of operand %{}", operand))?;
        let name = errand_type_name(&ty);
        Ok(Instr::Literal(LiteralPl::Symbol(name)))
    }

    #[instrument(
        skip(self, data, sir),
        name = "sir_gen.remap_if_statement",
        target = "sir_gen",
        level = "trace"
    )]
    fn remap_if_statement(
        &mut self,
        data: IfStatementData,
        sir: &mut SIR,
    ) -> Result<Instr, String> {
        let condition = self.analyze_and_emit(data.condition, sir)?;
        let then_branch = self.analyze_and_emit(data.then_branch, sir)?;
        let else_branch = data
            .else_branch
            .map(|e| self.analyze_and_emit(e, sir))
            .transpose()?;
        Ok(Instr::IfStatement(IfStatementData {
            condition,
            then_branch,
            else_branch,
        }))
    }

    #[instrument(
        skip(self, data, sir),
        name = "sir_gen.remap_while_loop",
        target = "sir_gen",
        level = "trace"
    )]
    fn remap_while_loop(&mut self, data: WhileLoopData, sir: &mut SIR) -> Result<Instr, String> {
        let condition = self.analyze_and_emit(data.condition, sir)?;
        let body = self.analyze_and_emit(data.body, sir)?;
        Ok(Instr::WhileLoop(WhileLoopData { condition, body }))
    }

    #[instrument(
        skip(self, data, sir),
        fields(iterator = %data.iterator),
        name = "sir_gen.remap_for_loop",
        target = "sir_gen",
        level = "trace"
    )]
    fn remap_for_loop(&mut self, data: ForLoopData, sir: &mut SIR) -> Result<Instr, String> {
        let range = self.analyze_and_emit(data.range, sir)?;
        let body = self.analyze_and_emit(data.body, sir)?;
        Ok(Instr::ForLoop(ForLoopData {
            iterator: data.iterator,
            range,
            body,
        }))
    }

    #[instrument(
        skip(self, data, sir),
        name = "sir_gen.remap_return",
        target = "sir_gen",
        level = "trace"
    )]
    fn remap_return(&mut self, data: ReturnData, sir: &mut SIR) -> Result<Instr, String> {
        let value = data
            .value
            .map(|v| self.analyze_and_emit(v, sir))
            .transpose()?;
        Ok(Instr::Return(ReturnData { value }))
    }

    #[instrument(
        skip(self, data, sir),
        fields(name = %data.name, is_foreign = data.is_foreign),
        name = "sir_gen.remap_func_decl",
        target = "sir_gen",
        level = "trace"
    )]
    fn remap_func_decl(&mut self, data: FuncData, sir: &mut SIR) -> Result<Instr, String> {
        let body_index = self.analyze_and_emit(data.body_index, sir)?;
        Ok(Instr::FuncDecl(FuncData {
            name: data.name,
            type_params: data.type_params,
            parameters: data.parameters,
            context_params: data.context_params,
            body_index,
            return_type: data.return_type,
            is_foreign: data.is_foreign,
        }))
    }

    // ── Generic finalization (module post-pass) ─────────────────────────────────

    /// Collapse `App` types, patch `new`/enum names, and add mangled struct/enum layouts.
    #[instrument(
        skip(self, module),
        name = "sir_gen.finalize_generics",
        target = "sir_gen",
        level = "debug"
    )]
    fn finalize_generics(&mut self, module: &mut SIRModule) -> Result<(), String> {
        // Each monomorph pass emits/clones bodies with fresh `App` types and
        // un-mangled symbols, and may add new constraints, so it is bracketed by
        // a `normalize` (collapse → solve → collapse). Struct constructors are
        // monomorphized before functions because function bodies construct and
        // consume those generic structs.
        self.normalize(module);
        self.monomorph_generic_struct_constructors(module)?;
        self.normalize(module);
        let monomorphized = self.monomorph_generic_functions(module)?;
        self.normalize(module);
        // Repair callers whose baked-in types are now stale (e.g. a call that
        // used to return `Int` now returns a struct), so `getfield` type-name
        // symbols match the corrected receiver types. Runs after both monomorph
        // passes since it reads their final return types.
        self.repair_caller_types(module, &monomorphized);
        // Last: build layouts for every mangled symbol any prior pass injected.
        Self::ensure_mangled_layouts(module, &self.analyzer.fir)?;
        Ok(())
    }

    /// Settle a module's types after emitting/cloning bodies: collapse `App`
    /// types and patch mangled symbol names, resolve any pending constraints,
    /// then collapse/patch again so the freshly resolved types propagate. Both
    /// collapse passes are pure and idempotent, so the leading one is harmless
    /// even when there is nothing yet to collapse.
    fn normalize(&mut self, module: &mut SIRModule) {
        Self::apply_collapse_patch(module, &self.analyzer);
        let _ = self.analyzer.solve_pending();
        Self::apply_collapse_patch(module, &self.analyzer);
    }

    #[instrument(
        skip(module, analyzer),
        name = "sir_gen.apply_collapse_patch",
        target = "sir_gen",
        level = "trace"
    )]
    fn apply_collapse_patch(module: &mut SIRModule, analyzer: &Analyzer) {
        for_each_sir_body_mut(module, |sir| {
            Self::collapse_sir_types(sir, analyzer);
            Self::patch_mangled_names(sir);
        });
    }

    #[instrument(
        skip(sir, analyzer),
        name = "sir_gen.collapse_sir_types",
        target = "sir_gen",
        level = "trace"
    )]
    fn collapse_sir_types(sir: &mut SIR, analyzer: &Analyzer) {
        for si in &mut sir.instructions {
            if let Some(ty) = si.ty.take() {
                si.ty = Some(analyzer.collapse_apps_in_type(ty));
            }
        }
    }

    #[instrument(
        skip(sir),
        name = "sir_gen.patch_mangled_names",
        target = "sir_gen",
        level = "trace"
    )]
    fn patch_mangled_names(sir: &mut SIR) {
        let mut new_symbol_patches: Vec<(usize, String)> = Vec::new();
        for (_i, si) in sir.instructions.iter_mut().enumerate() {
            if let Some(ErrandType::Con(m)) = &si.ty {
                if !m.contains("__") {
                    continue;
                }
                match &mut si.instr {
                    Instr::EnumVariantConstruct(EnumVariantConstructData {
                        ref mut enum_name,
                        ..
                    }) => {
                        *enum_name = m.clone();
                    }
                    Instr::EnumVariantAccess(EnumVariantData {
                        ref mut enum_name, ..
                    }) => {
                        *enum_name = m.clone();
                    }
                    Instr::Match(MatchData {
                        ref mut enum_name, ..
                    }) => {
                        *enum_name = m.clone();
                    }
                    Instr::New(arguments) if !arguments.is_empty() => {
                        new_symbol_patches.push((arguments[0] as usize, m.clone()));
                    }
                    _ => {}
                }
            }
        }

        // Match instructions are usually typed `Unit`; derive the mangled enum
        // name from the scrutinee's collapsed type instead.
        let mut match_enum_patches: Vec<(usize, String)> = Vec::new();
        for (i, si) in sir.instructions.iter().enumerate() {
            let Instr::Match(MatchData {
                enum_name,
                scrutinee,
                ..
            }) = &si.instr
            else {
                continue;
            };
            if enum_name.contains("__") {
                continue;
            }
            let Some(scrut_si) = sir.instructions.get(*scrutinee as usize) else {
                continue;
            };
            let Some(mangled) = scrut_si
                .ty
                .as_ref()
                .and_then(Self::mangled_nominal_type_name)
            else {
                continue;
            };
            match_enum_patches.push((i, mangled));
        }
        for (i, mangled) in match_enum_patches {
            if let Instr::Match(MatchData {
                ref mut enum_name, ..
            }) = sir.instructions[i].instr
            {
                *enum_name = mangled;
            }
        }

        for (arg_idx, m) in new_symbol_patches {
            if let Some(arg_si) = sir.instructions.get_mut(arg_idx) {
                if let Instr::Literal(LiteralPl::Symbol(ref mut s)) = arg_si.instr {
                    if m == s.as_str() || m.starts_with(&format!("{}__", s)) {
                        *s = m;
                    }
                }
            }
        }
    }

    /// Nominal type name after generic monomorphization, e.g. `Option__Int`.
    /// The type of a body's return location, defaulting to `Void` when absent.
    fn sir_return_type(body: &SIR) -> ErrandType {
        body.instructions
            .get(body.return_loc as usize)
            .and_then(|i| i.ty.clone())
            .unwrap_or_else(|| ErrandType::Con("Void".into()))
    }

    fn mangled_nominal_type_name(ty: &ErrandType) -> Option<String> {
        match ty {
            ErrandType::Con(n) if n.contains("__") => Some(n.clone()),
            ErrandType::App(h, _) => match h.as_ref() {
                ErrandType::Con(n) if n.contains("__") => Some(n.clone()),
                _ => None,
            },
            _ => None,
        }
    }

    /// Same key scheme as [`crate::backend::sir_lowering`] overload resolution.
    #[instrument(
        skip(ty),
        name = "sir_gen.sir_type_key_for_dispatch",
        target = "sir_gen",
        level = "trace"
    )]
    fn sir_type_key_for_dispatch(ty: Option<&ErrandType>) -> String {
        match ty {
            Some(ErrandType::Con(n)) => n.clone(),
            Some(ErrandType::Var(n)) | Some(ErrandType::ETVar(n)) => n.clone(),
            _ => "Any".to_string(),
        }
    }

    #[instrument(
        skip(ty, subst),
        name = "sir_gen.subst_errand_type",
        target = "sir_gen",
        level = "trace"
    )]
    fn subst_errand_type(ty: &ErrandType, subst: &HashMap<String, ErrandType>) -> ErrandType {
        match ty {
            ErrandType::Var(n) => subst.get(n).cloned().unwrap_or_else(|| ty.clone()),
            ErrandType::Arrow(a, b) => ErrandType::Arrow(
                Box::new(Self::subst_errand_type(a, subst)),
                Box::new(Self::subst_errand_type(b, subst)),
            ),
            ErrandType::App(h, args) => ErrandType::App(
                Box::new(Self::subst_errand_type(h, subst)),
                args.iter()
                    .map(|a| Self::subst_errand_type(a, subst))
                    .collect(),
            ),
            ErrandType::Forall(v, b) => {
                if subst.contains_key(v) {
                    Self::subst_errand_type(b, subst)
                } else {
                    ErrandType::Forall(v.clone(), Box::new(Self::subst_errand_type(b, subst)))
                }
            }
            ErrandType::Product(ts) => ErrandType::Product(
                ts.iter()
                    .map(|t| Self::subst_errand_type(t, subst))
                    .collect(),
            ),
            _ => ty.clone(),
        }
    }

    #[instrument(
        skip(te, arg, type_params, subst),
        fields(type_param_count = type_params.len()),
        name = "sir_gen.unify_field_type_with_arg_ty",
        target = "sir_gen",
        level = "trace"
    )]
    fn unify_field_type_with_arg_ty(
        te: &TypeExpression,
        arg: &ErrandType,
        type_params: &[String],
        subst: &mut HashMap<String, ErrandType>,
    ) -> Option<()> {
        match te {
            TypeExpression::Int => match arg {
                ErrandType::Con(n) if n == "Int" => Some(()),
                _ => None,
            },
            TypeExpression::Int32 => match arg {
                ErrandType::Con(n) if n == "Int32" => Some(()),
                _ => None,
            },
            TypeExpression::Float => match arg {
                ErrandType::Con(n) if n == "Float" => Some(()),
                _ => None,
            },
            TypeExpression::Bool => match arg {
                ErrandType::Con(n) if n == "Bool" => Some(()),
                _ => None,
            },
            TypeExpression::String => match arg {
                ErrandType::Con(n) if n == "String" => Some(()),
                _ => None,
            },
            TypeExpression::Void => match arg {
                ErrandType::Con(n) if n == "Unit" || n == "Void" => Some(()),
                _ => None,
            },
            TypeExpression::Struct(id, _, generic_args) => {
                if type_params.iter().any(|p| p == &id.name) {
                    match subst.entry(id.name.clone()) {
                        Entry::Vacant(e) => {
                            e.insert(arg.clone());
                            Some(())
                        }
                        Entry::Occupied(e) => (e.get() == arg).then_some(()),
                    }
                } else if let Some(args) = generic_args {
                    let ErrandType::App(h, inner) = arg else {
                        return None;
                    };
                    let ErrandType::Con(hn) = h.as_ref() else {
                        return None;
                    };
                    if hn != &id.name || args.len() != inner.len() {
                        return None;
                    }
                    for (ga, it) in args.iter().zip(inner.iter()) {
                        let GenericArg::Type(t) = ga;
                        Self::unify_field_type_with_arg_ty(t, it, type_params, subst)?;
                    }
                    Some(())
                } else {
                    match arg {
                        ErrandType::Con(c) if c == &id.name => Some(()),
                        ErrandType::App(h, _) => {
                            let ErrandType::Con(hn) = h.as_ref() else {
                                return None;
                            };
                            (hn == &id.name).then_some(())
                        }
                        _ => None,
                    }
                }
            }
        }
    }

    #[instrument(
        skip(sd, arg_tys),
        fields(struct_name = %sd.name, arg_count = arg_tys.len()),
        name = "sir_gen.infer_subst_from_generic_ctor_call",
        target = "sir_gen",
        level = "trace"
    )]
    fn infer_subst_from_generic_ctor_call(
        sd: &StructData,
        arg_tys: &[ErrandType],
    ) -> Option<HashMap<String, ErrandType>> {
        if sd.fields.len() != arg_tys.len() {
            return None;
        }
        let mut subst = HashMap::new();
        for (field, arg) in sd.fields.iter().zip(arg_tys.iter()) {
            Self::unify_field_type_with_arg_ty(
                &field.field_type,
                arg,
                &sd.type_params
                    .iter()
                    .map(|p| p.name.clone())
                    .collect::<Vec<String>>(),
                &mut subst,
            )?;
        }
        if subst.len() != sd.type_params.len() {
            return None;
        }
        for tp in &sd.type_params {
            if !subst.contains_key(&tp.name) {
                return None;
            }
        }
        Some(subst)
    }

    #[instrument(
        skip(sd, subst),
        fields(struct_name = %sd.name),
        name = "sir_gen.mangle_generic_struct_name",
        target = "sir_gen",
        level = "trace"
    )]
    fn mangle_generic_struct_name(sd: &StructData, subst: &HashMap<String, ErrandType>) -> String {
        let mut s = sd.name.clone();
        for tp in &sd.type_params {
            if let Some(ty) = subst.get(&tp.name) {
                s.push_str("__");
                s.push_str(&errand_type_name(ty));
            }
        }
        s
    }

    #[instrument(
        skip(sir, subst, analyzer),
        name = "sir_gen.apply_subst_to_sir_body",
        target = "sir_gen",
        level = "trace"
    )]
    fn apply_subst_to_sir_body(
        sir: &mut SIR,
        subst: &HashMap<String, ErrandType>,
        analyzer: &Analyzer,
    ) {
        for si in &mut sir.instructions {
            if let Some(ty) = si.ty.take() {
                let expanded = analyzer.expand_type(&ty);
                let sub = Self::subst_errand_type(&expanded, subst);
                si.ty = Some(analyzer.collapse_apps_in_type(sub));
            }
        }
    }

    #[instrument(
        skip(sir),
        fields(base = %base, mangled = %mangled),
        name = "sir_gen.patch_new_symbol_in_body",
        target = "sir_gen",
        level = "trace"
    )]
    fn patch_new_symbol_in_body(sir: &mut SIR, base: &str, mangled: &str) {
        let mut patches: Vec<(usize, String)> = Vec::new();
        for si in &sir.instructions {
            let type_arg_idx = match &si.instr {
                Instr::FnCall(fc) if fc.name == "new" && !fc.arguments.is_empty() => {
                    Some(fc.arguments[0] as usize)
                }
                Instr::New(arguments) if !arguments.is_empty() => Some(arguments[0] as usize),
                _ => None,
            };
            if let Some(arg_idx) = type_arg_idx {
                if let Some(arg_si) = sir.instructions.get(arg_idx) {
                    if let Instr::Literal(LiteralPl::Symbol(s)) = &arg_si.instr {
                        if s == base {
                            patches.push((arg_idx, mangled.to_string()));
                        }
                    }
                }
            }
        }
        for (idx, m) in patches {
            if let Some(arg_si) = sir.instructions.get_mut(idx) {
                if let Instr::Literal(LiteralPl::Symbol(ref mut s)) = arg_si.instr {
                    *s = m;
                }
            }
        }
    }

    #[instrument(
        skip_all,
        fields(ctor_name = %ctor_name),
        name = "sir_gen.collect_generic_ctor_call_shapes",
        target = "sir_gen",
        level = "trace"
    )]
    fn collect_generic_ctor_call_shapes(
        sir: &SIR,
        ctor_name: &str,
        expand: impl Fn(&ErrandType) -> ErrandType,
        out: &mut HashMap<Vec<String>, Vec<ErrandType>>,
    ) {
        for si in &sir.instructions {
            let Instr::FnCall(fc) = &si.instr else {
                continue;
            };
            if fc.name != ctor_name {
                continue;
            }
            let mut keys = Vec::new();
            let mut tys = Vec::new();
            let mut ok = true;
            for &idx in &fc.arguments {
                let Some(op) = sir.instructions.get(idx as usize) else {
                    ok = false;
                    break;
                };
                let Some(ty) = op.ty.as_ref() else {
                    ok = false;
                    break;
                };
                let et = expand(ty);
                keys.push(Self::sir_type_key_for_dispatch(Some(&et)));
                tys.push(et);
            }
            if !ok || tys.is_empty() {
                continue;
            }
            if keys
                .iter()
                .any(|k| k == "Any" || k.starts_with("param_") || k.starts_with("^α"))
            {
                continue;
            }
            out.entry(keys).or_insert(tys);
        }
    }

    #[instrument(
        skip_all,
        fields(ctor_name = %ctor_name),
        name = "sir_gen.gather_all_generic_ctor_call_shapes",
        target = "sir_gen",
        level = "trace"
    )]
    fn gather_all_generic_ctor_call_shapes(
        module: &SIRModule,
        ctor_name: &str,
        expand: impl Fn(&ErrandType) -> ErrandType + Copy,
        out: &mut HashMap<Vec<String>, Vec<ErrandType>>,
    ) {
        for_each_sir_body(module, |sir| {
            Self::collect_generic_ctor_call_shapes(sir, ctor_name, expand, out);
        });
    }

    #[instrument(
        skip(self, module),
        name = "sir_gen.monomorph_generic_struct_constructors",
        target = "sir_gen",
        level = "debug"
    )]
    fn monomorph_generic_struct_constructors(&self, module: &mut SIRModule) -> Result<(), String> {
        let generic_structs: Vec<StructData> = self
            .analyzer
            .fir
            .instructions
            .iter()
            .filter_map(|i| {
                if let Instr::StructDecl(sd) = i {
                    if sd.type_params.is_empty() {
                        None
                    } else {
                        Some(sd.clone())
                    }
                } else {
                    None
                }
            })
            .collect();
        for sd in generic_structs {
            let fname = sd.name.clone();
            let Some(overloads) = module.functions.get(&fname) else {
                continue;
            };
            // Must match `emit_sir_module` type keys: `type_expr_to_errand_type` with **no**
            // struct type-parameter scope (parameters are plain `T` nominal until monomorph).
            let template_key: Vec<String> = sd
                .fields
                .iter()
                .map(|f| errand_type_name(&type_expr_to_errand_type(&f.field_type)))
                .collect();
            let (actual_template_key, template_info) =
                if let Some(ti) = overloads.get(&template_key).cloned() {
                    (template_key.clone(), ti)
                } else if overloads.len() == 1 {
                    let (k, v) = overloads.iter().next().expect("len==1");
                    (k.clone(), v.clone())
                } else {
                    continue;
                };
            let Some(template_body_src) = template_info.body.as_ref() else {
                continue;
            };

            let mut shapes: HashMap<Vec<String>, Vec<ErrandType>> = HashMap::new();
            Self::gather_all_generic_ctor_call_shapes(
                module,
                &fname,
                |t| self.analyzer.expand_type(t),
                &mut shapes,
            );

            let mut new_overloads: Vec<(Vec<String>, SIRFunctionInfo)> = Vec::new();
            for (arg_keys, arg_tys) in shapes {
                if arg_keys == actual_template_key {
                    continue;
                }
                let Some(subst) = Self::infer_subst_from_generic_ctor_call(&sd, &arg_tys) else {
                    continue;
                };
                let mangled = Self::mangle_generic_struct_name(&sd, &subst);
                let already = module
                    .functions
                    .get(&fname)
                    .map(|o| o.contains_key(&arg_keys))
                    .unwrap_or(false);
                if already {
                    continue;
                }
                let mut body = template_body_src.clone();
                Self::apply_subst_to_sir_body(&mut body, &subst, &self.analyzer);
                Self::patch_new_symbol_in_body(&mut body, &fname, &mangled);
                Self::collapse_sir_types(&mut body, &self.analyzer);
                Self::patch_mangled_names(&mut body);

                let params: Vec<(String, ErrandType)> = template_info
                    .params
                    .iter()
                    .zip(arg_tys.iter())
                    .map(|((n, _), ty)| (n.clone(), ty.clone()))
                    .collect();
                let return_ty = Self::sir_return_type(&body);

                let info = SIRFunctionInfo {
                    params,
                    return_type: return_ty,
                    is_foreign: template_info.is_foreign,
                    body: Some(body),
                };
                new_overloads.push((arg_keys, info));
            }

            if !new_overloads.is_empty() {
                let ov = module.functions.entry(fname.clone()).or_default();
                ov.remove(&actual_template_key);
                for (k, info) in new_overloads {
                    ov.insert(k, info);
                }
            }
        }
        Ok(())
    }

    /// Monomorphize generic *functions* (those with declared type parameters,
    /// e.g. `fn unwrap<T>(...)`, or at least one untyped parameter) per concrete
    /// call shape. The single template body that the
    /// initial pass produced has its argument types baked in from whichever call
    /// the worklist happened to unify first, so:
    ///   - `getfield` inside the body defaulted its result to `Int` when the
    ///     receiver was still an existential (gap A), and
    ///   - a call inside the body resolved one overload for every instantiation
    ///     (gap B).
    /// Re-analyzing the body with each call site's concrete argument types fixes
    /// both: `getfield` resolves the real field type, and inner dispatch keys on
    /// the real argument types. Each instantiation becomes its own overload keyed
    /// by the concrete argument type keys (matching call-site dispatch in
    /// `sir_lowering`), and the generic template overload is dropped.
    #[instrument(
        skip(self, module),
        name = "sir_gen.monomorph_generic_functions",
        target = "sir_gen",
        level = "debug"
    )]
    fn monomorph_generic_functions(
        &mut self,
        module: &mut SIRModule,
    ) -> Result<HashSet<String>, String> {
        let struct_names: HashSet<String> = self
            .analyzer
            .fir
            .instructions
            .iter()
            .filter_map(|i| match i {
                Instr::StructDecl(sd) => Some(sd.name.clone()),
                _ => None,
            })
            .collect();
        // Generic user functions: non-foreign, not struct constructors, and
        // generic in one of two ways:
        //   - declared type parameters (e.g. `fn unwrap<T>(b::Box<T>):: T`), or
        //   - an untyped parameter (e.g. `make_pair(a, b)`), which is treated as
        //     an implicit per-call type variable.
        // Note: dispatch is never on the return type — only on argument types.
        let generic_funcs: Vec<(String, InstrIndex, Vec<Parameter>)> = self
            .analyzer
            .fir
            .instructions
            .iter()
            .filter_map(|instr| {
                let Instr::FuncDecl(fd) = instr else {
                    return None;
                };
                let is_generic = !fd.type_params.is_empty()
                    || fd.parameters.iter().any(|p| p.type_expr.is_none());
                if fd.is_foreign || struct_names.contains(&fd.name) || !is_generic {
                    return None;
                }
                Some((fd.name.clone(), fd.body_index, fd.parameters.clone()))
            })
            .collect();

        // Read phase: gather concrete call shapes for each generic function from
        // every SIR body (main + all overloads) before mutating the module.
        let mut plans: Vec<(
            String,
            InstrIndex,
            Vec<Parameter>,
            HashMap<Vec<String>, Vec<ErrandType>>,
        )> = Vec::new();
        for (name, body_index, params) in &generic_funcs {
            let mut shapes: HashMap<Vec<String>, Vec<ErrandType>> = HashMap::new();
            Self::collect_generic_ctor_call_shapes(
                &module.main,
                name,
                |t| self.analyzer.expand_type(t),
                &mut shapes,
            );
            for overloads in module.functions.values() {
                for info in overloads.values() {
                    if let Some(body) = &info.body {
                        Self::collect_generic_ctor_call_shapes(
                            body,
                            name,
                            |t| self.analyzer.expand_type(t),
                            &mut shapes,
                        );
                    }
                }
            }
            if !shapes.is_empty() {
                plans.push((name.clone(), *body_index, params.clone(), shapes));
            }
        }

        // Write phase: re-emit each instantiation and register it as an overload.
        let mut monomorphized: HashSet<String> = HashSet::new();
        for (name, body_index, params, shapes) in plans {
            let mut new_overloads: Vec<(Vec<String>, SIRFunctionInfo)> = Vec::new();
            for (arg_keys, arg_tys) in shapes {
                if params.len() != arg_tys.len() {
                    continue;
                }
                let typed_params: Vec<(String, ErrandType)> = params
                    .iter()
                    .zip(arg_tys.iter())
                    .map(|(p, ty)| (p.id.name.clone(), ty.clone()))
                    .collect();

                // Re-type the body against this concrete call shape. The cache is
                // keyed by FIR index (shared across instantiations) so it must be
                // cleared first.
                self.analyzer.clear_analysis_cache();
                self.analyzer
                    .setup_function_context_with_types(&typed_params);
                let body = self.emit_body_sir(body_index)?;

                let return_type = Self::sir_return_type(&body);

                tracing::debug!(
                    target: "sir_gen",
                    function = %name,
                    shape = ?arg_keys,
                    return_type = %errand_type_name(&return_type),
                    "monomorphized generic function instance"
                );

                new_overloads.push((
                    arg_keys,
                    SIRFunctionInfo {
                        params: typed_params,
                        return_type,
                        is_foreign: false,
                        body: Some(body),
                    },
                ));
            }

            if !new_overloads.is_empty() {
                monomorphized.insert(name.clone());
                let template_key = Self::template_overload_key(&params);
                let overloads = module.functions.entry(name.clone()).or_default();
                // Drop the generic template: the overload keyed by the original
                // (uninstantiated) parameter signature — either `param_*`
                // placeholders for untyped params or a free-type-var key like
                // `Box__T`. Only the concrete instantiations remain.
                overloads.retain(|k, _| !k.iter().any(|s| s.starts_with("param_")));
                overloads.remove(&template_key);
                for (k, info) in new_overloads {
                    overloads.insert(k, info);
                }
            }
        }
        Ok(monomorphized)
    }

    /// The overload key for a function's *uninstantiated* signature, matching how
    /// the initial emit pass keys overloads: typed params use the mangled type
    /// name (`Box<T>` → `Box__T`), untyped params use the `param_<name>`
    /// placeholder. Used to evict the template once concrete instances exist.
    fn template_overload_key(params: &[Parameter]) -> Vec<String> {
        params
            .iter()
            .map(|p| match &p.type_expr {
                Some(te) => errand_type_name(&type_expr_to_errand_type(te)),
                None => format!("param_{}", p.id.name),
            })
            .collect()
    }

    /// After generic functions are monomorphized, callers still carry the stale
    /// types the template baked in (e.g. a call that now returns a struct was
    /// typed `Int`). Codegen reads the *embedded* type-name symbol of a
    /// `getfield`, so a stale caller crashes ("struct type 'Int' not found").
    /// This forward pass re-derives, per body, the result type of calls to
    /// monomorphized functions, propagates it through `var`/`varref`, and then
    /// rewrites each `getfield`'s type-name symbol (and result type) from the
    /// corrected receiver type. SIR is emitted in dependency order, so a single
    /// forward sweep observes already-corrected operands.
    #[instrument(
        skip(self, module, monomorphized),
        name = "sir_gen.repair_caller_types",
        target = "sir_gen",
        level = "debug"
    )]
    fn repair_caller_types(&self, module: &mut SIRModule, monomorphized: &HashSet<String>) {
        if monomorphized.is_empty() {
            return;
        }
        let fn_returns: HashMap<String, Vec<(Vec<String>, ErrandType)>> = module
            .functions
            .iter()
            .filter(|(name, _)| monomorphized.contains(*name))
            .map(|(name, overloads)| {
                let rets = overloads
                    .iter()
                    .map(|(key, info)| (key.clone(), info.return_type.clone()))
                    .collect();
                (name.clone(), rets)
            })
            .collect();

        let struct_defs: Vec<StructData> = self
            .analyzer
            .fir
            .instructions
            .iter()
            .filter_map(|i| match i {
                Instr::StructDecl(sd) => Some(sd.clone()),
                _ => None,
            })
            .collect();

        for_each_sir_body_mut(module, |sir| {
            Self::repair_body_types(sir, &fn_returns, &struct_defs);
        });
    }

    fn repair_body_types(
        sir: &mut SIR,
        fn_returns: &HashMap<String, Vec<(Vec<String>, ErrandType)>>,
        struct_defs: &[StructData],
    ) {
        let mut var_types: HashMap<String, ErrandType> = HashMap::new();
        for i in 0..sir.instructions.len() {
            let instr = sir.instructions[i].instr.clone();
            match instr {
                Instr::FnCall(fc) => {
                    if let Some(overloads) = fn_returns.get(&fc.name) {
                        let arg_keys: Vec<String> = fc
                            .arguments
                            .iter()
                            .map(|&a| {
                                Self::sir_type_key_for_dispatch(
                                    sir.instructions.get(a as usize).and_then(|x| x.ty.as_ref()),
                                )
                            })
                            .collect();
                        if let Some((_, ret)) = overloads.iter().find(|(key, _)| *key == arg_keys) {
                            sir.instructions[i].ty = Some(ret.clone());
                        }
                    }
                }
                Instr::VarDecl(vd) => {
                    if let Some(t) = sir
                        .instructions
                        .get(vd.value as usize)
                        .and_then(|x| x.ty.clone())
                    {
                        var_types.insert(vd.name.clone(), t);
                    }
                }
                Instr::VarRef(vr) => {
                    if let Some(t) = var_types.get(&vr.name) {
                        sir.instructions[i].ty = Some(t.clone());
                    }
                }
                Instr::GetField(args) if args.len() == 3 => {
                    let Some(recv_ty) = sir
                        .instructions
                        .get(args[0] as usize)
                        .and_then(|x| x.ty.clone())
                    else {
                        continue;
                    };
                    let field_name = match sir.instructions.get(args[1] as usize) {
                        Some(SIRInstr {
                            instr: Instr::Literal(LiteralPl::Symbol(s)),
                            ..
                        }) => s.clone(),
                        _ => continue,
                    };
                    let Some(field_ty) =
                        Self::sir_struct_field_type(&recv_ty, &field_name, struct_defs)
                    else {
                        continue;
                    };
                    let recv_name = errand_type_name(&recv_ty);
                    if let Some(sym) = sir.instructions.get_mut(args[2] as usize) {
                        if let Instr::Literal(LiteralPl::Symbol(s)) = &mut sym.instr {
                            *s = recv_name;
                        }
                    }
                    sir.instructions[i].ty = Some(field_ty);
                }
                _ => {}
            }
        }
    }

    /// Resolve `recv_ty.field` to a (collapsed) field type using FIR struct
    /// definitions. Returns `None` when the receiver is not a known struct (e.g.
    /// a primitive), which is the signal to leave a `getfield` untouched.
    fn sir_struct_field_type(
        recv_ty: &ErrandType,
        field_name: &str,
        struct_defs: &[StructData],
    ) -> Option<ErrandType> {
        let (name, type_args) = Self::sir_nominal_parts(recv_ty)?;
        let sd = struct_defs.iter().find(|s| s.name == name)?;
        let type_param_names: Vec<String> = sd.type_params.iter().map(|p| p.name.clone()).collect();
        // Only act on a *fully applied* nominal type. An incompletely applied
        // generic (e.g. a bare `Con("Box")` with no arguments) is imprecise and
        // must not clobber an already-correct mangled symbol like `Box__Int`.
        if type_args.len() != type_param_names.len() {
            return None;
        }
        let subst: HashMap<String, ErrandType> = type_param_names
            .iter()
            .cloned()
            .zip(type_args.into_iter())
            .collect();
        for field in &sd.fields {
            if field.id.name == field_name {
                let ft = type_expr_to_errand_type_with_params(&field.field_type, &type_param_names);
                let ft = Self::subst_errand_type(&ft, &subst);
                return Some(match &ft {
                    ErrandType::App(..) => ErrandType::Con(errand_type_name(&ft)),
                    other => other.clone(),
                });
            }
        }
        None
    }

    /// Split a nominal struct type into base name + type arguments, accepting
    /// both `App(Con(Foo), [..])` and the collapsed mangled `Con("Foo__Bar")`.
    fn sir_nominal_parts(ty: &ErrandType) -> Option<(String, Vec<ErrandType>)> {
        match ty {
            ErrandType::App(h, args) => match h.as_ref() {
                ErrandType::Con(name) => Some((name.clone(), args.clone())),
                _ => None,
            },
            ErrandType::Con(name) if name.contains("__") => {
                let base = name.split("__").next()?.to_string();
                let rest = &name[base.len() + 2..];
                let mut args = Vec::new();
                for part in rest.split("__") {
                    args.push(ErrandType::Con(part.to_string()));
                }
                Some((base, args))
            }
            ErrandType::Con(name) => Some((name.clone(), Vec::new())),
            _ => None,
        }
    }

    #[instrument(
        skip(sir, mangled),
        name = "sir_gen.collect_mangled_symbols_from_sir",
        target = "sir_gen",
        level = "trace"
    )]
    fn collect_mangled_symbols_from_sir(sir: &SIR, mangled: &mut HashSet<String>) {
        for si in &sir.instructions {
            if let Instr::Literal(LiteralPl::Symbol(s)) = &si.instr {
                if s.contains("__") {
                    mangled.insert(s.clone());
                }
            }
        }
    }

    #[instrument(
        skip(module, fir),
        fields(fir_len = fir.instructions.len()),
        name = "sir_gen.ensure_mangled_layouts",
        target = "sir_gen",
        level = "trace"
    )]
    fn ensure_mangled_layouts(module: &mut SIRModule, fir: &FIR) -> Result<(), String> {
        let mut mangled: HashSet<String> = HashSet::new();
        for_each_sir_body(module, |sir| {
            for si in &sir.instructions {
                if let Some(ErrandType::Con(m)) = &si.ty {
                    if m.contains("__") {
                        mangled.insert(m.clone());
                    }
                }
            }
            Self::collect_mangled_symbols_from_sir(sir, &mut mangled);
        });

        // Collect *all* struct/enum declarations, not just generic templates: a
        // mangled name like `Box__Apple` only matches a generic template by its
        // prefix, but resolving its type *arguments* (e.g. the non-generic struct
        // `Apple`) requires the concrete definitions too. Restricting these lists
        // to generic defs is what made `Box__Int` (primitive arg) work while
        // `Box__Apple` (struct arg) failed to register a layout.
        let struct_defs: Vec<StructData> = fir
            .instructions
            .iter()
            .filter_map(|i| {
                if let Instr::StructDecl(sd) = i {
                    Some(sd.clone())
                } else {
                    None
                }
            })
            .collect();
        let enum_defs: Vec<EnumData> = fir
            .instructions
            .iter()
            .filter_map(|i| {
                if let Instr::EnumDecl(ed) = i {
                    Some(ed.clone())
                } else {
                    None
                }
            })
            .collect();

        for m in mangled {
            if module.structs.contains_key(&m) || module.enums.contains_key(&m) {
                continue;
            }
            if let Some((sd, subst)) = parse_mangled_struct(&m, &struct_defs, &enum_defs) {
                let layout = build_struct_layout_subst(&sd, &subst)?;
                module.structs.insert(m, layout);
                continue;
            }
            if let Some((ed, subst)) = parse_mangled_enum(&m, &enum_defs, &struct_defs) {
                let layout = build_enum_layout_subst(&ed, &subst)?;
                module.enums.insert(m, layout);
            }
        }
        Ok(())
    }
}

// ── Module-wide SIR body iteration ───────────────────────────────────────────

#[instrument(
    skip_all,
    name = "sir_gen.for_each_sir_body_mut",
    target = "sir_gen",
    level = "trace"
)]
fn for_each_sir_body_mut(module: &mut SIRModule, mut f: impl FnMut(&mut SIR)) {
    f(&mut module.main);
    for overloads in module.functions.values_mut() {
        for info in overloads.values_mut() {
            if let Some(ref mut body) = info.body {
                f(body);
            }
        }
    }
}

#[instrument(
    skip_all,
    name = "sir_gen.for_each_sir_body",
    target = "sir_gen",
    level = "trace"
)]
fn for_each_sir_body(module: &SIRModule, mut f: impl FnMut(&SIR)) {
    f(&module.main);
    for overloads in module.functions.values() {
        for info in overloads.values() {
            if let Some(ref body) = info.body {
                f(body);
            }
        }
    }
}

#[instrument(
    skip(n),
    fields(name = %n),
    name = "sir_gen.primitive_name_to_type_expr",
    target = "sir_gen",
    level = "trace"
)]
fn primitive_name_to_type_expr(n: &str) -> Result<TypeExpression, String> {
    Ok(match n {
        "Int" => TypeExpression::Int,
        "Int32" => TypeExpression::Int32,
        "Bool" => TypeExpression::Bool,
        "Float" => TypeExpression::Float,
        "String" => TypeExpression::String,
        "Void" => TypeExpression::Void,
        other => TypeExpression::Struct(
            Id {
                name: other.to_string(),
            },
            None,
            None,
        ),
    })
}

/// Parse one component of a mangled generic name, e.g. `Int` or `Box__Int`.
fn parse_mangled_type_component(
    s: &str,
    struct_defs: &[StructData],
    enum_defs: &[EnumData],
) -> Option<TypeExpression> {
    match s {
        "Int" | "Int32" | "Bool" | "Float" | "String" | "Void" => {
            primitive_name_to_type_expr(s).ok()
        }
        _ => {
            for sd in struct_defs {
                if sd.type_params.is_empty() && sd.name == s {
                    return Some(TypeExpression::Struct(
                        Id {
                            name: sd.name.clone(),
                        },
                        None,
                        None,
                    ));
                }
                let prefix = format!("{}__", sd.name);
                if s.starts_with(&prefix) {
                    let rest = &s[sd.name.len() + 2..];
                    let args = parse_mangled_type_args(
                        rest,
                        sd.type_params.len(),
                        struct_defs,
                        enum_defs,
                    )?;
                    let generic_args: Vec<GenericArg> =
                        args.into_iter().map(GenericArg::Type).collect();
                    return Some(TypeExpression::Struct(
                        Id {
                            name: sd.name.clone(),
                        },
                        None,
                        Some(generic_args),
                    ));
                }
            }
            for ed in enum_defs {
                if ed.type_params.is_empty() && ed.name == s {
                    return Some(TypeExpression::Struct(
                        Id {
                            name: ed.name.clone(),
                        },
                        None,
                        None,
                    ));
                }
                let prefix = format!("{}__", ed.name);
                if s.starts_with(&prefix) {
                    let rest = &s[ed.name.len() + 2..];
                    let args = parse_mangled_type_args(
                        rest,
                        ed.type_params.len(),
                        struct_defs,
                        enum_defs,
                    )?;
                    let generic_args: Vec<GenericArg> =
                        args.into_iter().map(GenericArg::Type).collect();
                    return Some(TypeExpression::Struct(
                        Id {
                            name: ed.name.clone(),
                        },
                        None,
                        Some(generic_args),
                    ));
                }
            }
            None
        }
    }
}

/// Split a mangled argument suffix into `count` type expressions, respecting
/// nested `Foo__Bar__Baz` components.
fn parse_mangled_type_args(
    rest: &str,
    count: usize,
    struct_defs: &[StructData],
    enum_defs: &[EnumData],
) -> Option<Vec<TypeExpression>> {
    if count == 0 {
        return if rest.is_empty() {
            Some(Vec::new())
        } else {
            None
        };
    }
    if count == 1 {
        return Some(vec![parse_mangled_type_component(
            rest,
            struct_defs,
            enum_defs,
        )?]);
    }
    let mut search_from = 0;
    while let Some(rel) = rest[search_from..].find("__") {
        let split_at = search_from + rel;
        let head = &rest[..split_at];
        let tail = &rest[split_at + 2..];
        if !head.is_empty() {
            if let Some(first) = parse_mangled_type_component(head, struct_defs, enum_defs) {
                if let Some(mut tail_args) =
                    parse_mangled_type_args(tail, count - 1, struct_defs, enum_defs)
                {
                    let mut out = vec![first];
                    out.append(&mut tail_args);
                    return Some(out);
                }
            }
        }
        search_from = split_at + 2;
    }
    None
}

#[instrument(
    skip(te, m),
    name = "sir_gen.subst_type_expr",
    target = "sir_gen",
    level = "trace"
)]
fn subst_type_expr(te: &TypeExpression, m: &HashMap<String, TypeExpression>) -> TypeExpression {
    match te {
        TypeExpression::Struct(id, None, None) if m.contains_key(&id.name) => m[&id.name].clone(),
        TypeExpression::Struct(id, Some(inner), gen) => TypeExpression::Struct(
            id.clone(),
            Some(inner.iter().map(|x| subst_type_expr(x, m)).collect()),
            gen.as_ref().map(|g| {
                g.iter()
                    .map(|ga| match ga {
                        GenericArg::Type(t) => GenericArg::Type(subst_type_expr(t, m)),
                    })
                    .collect()
            }),
        ),
        TypeExpression::Struct(id, None, Some(g)) => TypeExpression::Struct(
            id.clone(),
            None,
            Some(
                g.iter()
                    .map(|ga| match ga {
                        GenericArg::Type(t) => GenericArg::Type(subst_type_expr(t, m)),
                    })
                    .collect(),
            ),
        ),
        _ => te.clone(),
    }
}

#[instrument(
    skip(m, defs),
    fields(mangled_len = m.len(), def_count = defs.len()),
    name = "sir_gen.parse_mangled_struct",
    target = "sir_gen",
    level = "trace"
)]
fn parse_mangled_struct(
    m: &str,
    defs: &[StructData],
    enum_defs: &[EnumData],
) -> Option<(StructData, HashMap<String, TypeExpression>)> {
    for sd in defs {
        let prefix = format!("{}__", sd.name);
        if !m.starts_with(&prefix) {
            continue;
        }
        let rest = &m[sd.name.len() + 2..];
        let arg_tys = parse_mangled_type_args(rest, sd.type_params.len(), defs, enum_defs)?;
        let mut subst = HashMap::new();
        for (tp, te) in sd.type_params.iter().zip(arg_tys.iter()) {
            subst.insert(tp.name.clone(), te.clone());
        }
        return Some((sd.clone(), subst));
    }
    None
}

#[instrument(
    skip(sd, subst),
    fields(struct_name = %sd.name),
    name = "sir_gen.build_struct_layout_subst",
    target = "sir_gen",
    level = "trace"
)]
fn build_struct_layout_subst(
    sd: &StructData,
    subst: &HashMap<String, TypeExpression>,
) -> Result<SIRStructLayout, String> {
    Ok(sir_layout_struct_fields_from_types(sd.fields.iter().map(
        |f| {
            let ft = subst_type_expr(&f.field_type, subst);
            (f.id.name.clone(), type_expr_to_errand_type(&ft))
        },
    )))
}

#[instrument(
    skip(m, defs),
    fields(mangled_len = m.len(), def_count = defs.len()),
    name = "sir_gen.parse_mangled_enum",
    target = "sir_gen",
    level = "trace"
)]
fn parse_mangled_enum(
    m: &str,
    defs: &[EnumData],
    struct_defs: &[StructData],
) -> Option<(EnumData, HashMap<String, TypeExpression>)> {
    for ed in defs {
        let prefix = format!("{}__", ed.name);
        if !m.starts_with(&prefix) {
            continue;
        }
        let rest = &m[ed.name.len() + 2..];
        let arg_tys = parse_mangled_type_args(rest, ed.type_params.len(), struct_defs, defs)?;
        let mut subst = HashMap::new();
        for (tp, te) in ed.type_params.iter().zip(arg_tys.iter()) {
            subst.insert(tp.name.clone(), te.clone());
        }
        return Some((ed.clone(), subst));
    }
    None
}

#[instrument(
    skip(ed, subst),
    fields(enum_name = %ed.name),
    name = "sir_gen.build_enum_layout_subst",
    target = "sir_gen",
    level = "trace"
)]
fn build_enum_layout_subst(
    ed: &EnumData,
    subst: &HashMap<String, TypeExpression>,
) -> Result<SIREnumLayout, String> {
    let mut max_payload = 0usize;
    let variant_layouts: Vec<SIREnumVariantLayout> = ed
        .variants
        .iter()
        .map(|v| {
            let inner = sir_layout_struct_fields_from_types(v.fields.iter().map(
                |(field_name, field_type)| {
                    let ft = subst_type_expr(field_type, subst);
                    (field_name.clone(), type_expr_to_errand_type(&ft))
                },
            ));
            let payload_size = inner.total_size;
            if payload_size > max_payload {
                max_payload = payload_size;
            }
            SIREnumVariantLayout {
                name: v.name.clone(),
                fields: inner.fields,
                payload_size,
            }
        })
        .collect();
    let is_simple = variant_layouts.iter().all(|v| v.fields.is_empty());
    let total_size = if is_simple { 0 } else { 8 + max_payload };
    Ok(SIREnumLayout {
        variants: variant_layouts,
        total_size,
        is_simple,
    })
}

// ─── Utilities ────────────────────────────────────────────────────────────────

/// Field order, `byte_offset`, and `total_size` for a struct or enum variant payload.
#[instrument(
    skip_all,
    name = "sir_gen.sir_layout_struct_fields_from_types",
    target = "sir_gen",
    level = "trace"
)]
fn sir_layout_struct_fields_from_types(
    field_name_ty: impl Iterator<Item = (String, ErrandType)>,
) -> SIRStructLayout {
    let mut offset = 0usize;
    let fields: Vec<SIRStructField> = field_name_ty
        .map(|(name, ty)| {
            let size = errand_type_size(&ty);
            let field = SIRStructField {
                name,
                ty,
                byte_offset: offset,
            };
            offset += size;
            field
        })
        .collect();
    SIRStructLayout {
        fields,
        total_size: offset,
    }
}

/// Stable mangling / dispatch key for an [`ErrandType`]. Differs from
/// [`ErrandType`](crate::backend::worklist::ErrandType)'s `Display` output used in SIR dumps;
/// do not use `Display` for overload keys.
pub(crate) fn errand_type_name(ty: &ErrandType) -> String {
    match ty {
        ErrandType::Con(n) | ErrandType::Var(n) | ErrandType::ETVar(n) => n.clone(),
        ErrandType::Arrow(_, _) => "Function".into(),
        ErrandType::Forall(_, _) => "Forall".into(),
        ErrandType::Product(_) => "Product".into(),
        ErrandType::App(head, args) => {
            let h = match head.as_ref() {
                ErrandType::Con(n) => n.as_str(),
                _ => "App",
            };
            let mut s = h.to_string();
            for a in args {
                s.push_str("__");
                s.push_str(&errand_type_name(a));
            }
            s
        }
    }
}

#[instrument(
    skip(ty),
    name = "sir_gen.errand_type_size",
    target = "sir_gen",
    level = "trace"
)]
fn errand_type_size(ty: &ErrandType) -> usize {
    match ty {
        ErrandType::Con(n) => match n.as_str() {
            "Int32" => 4,
            "Bool" => 1,
            _ => 8,
        },
        _ => 8,
    }
}
