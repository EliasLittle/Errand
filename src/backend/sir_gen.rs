use std::collections::{HashMap, HashSet};

use crate::backend::analysis::Analyzer;
use crate::backend::errand_builtins::type_expr_to_errand_type;
use crate::backend::preir::{
    BinOpPl, FnCallPl, ForLoopData, IfStatementData, Instr, MatchArmData, MatchData,
    RegionData, ReturnData, UnOpPl, VarDeclData, FuncData, instr_index,
};
use crate::backend::sir::{SIR, SIREnumLayout, SIREnumVariantLayout, SIRFunctionInfo, SIRInstr, SIRModule, SIRStructField, SIRStructLayout};
use crate::backend::worklist::ErrandType;
use crate::frontend::ast::{Parameter, Program};
use crate::backend::preir::PreIR;

/// Generates SIR from PreIR in a single interleaved pass: each instruction is
/// typed via `Analyzer` and emitted simultaneously.
pub struct SirGen {
    pub analyzer: Analyzer,
    /// Maps global PreIR instruction index -> local SIR index for the function
    /// currently being processed.  Reset for each function / main.
    preir_to_sir: HashMap<instr_index, instr_index>,
}

impl SirGen {
    fn new(analyzer: Analyzer) -> Self {
        SirGen { analyzer, preir_to_sir: HashMap::new() }
    }

    /// Build a complete `SIRModule` from a `PreIR`.
    pub fn emit_sir_module(preir: PreIR, program: &Program) -> Result<SIRModule, String> {
        let analyzer = Analyzer::new(preir, program);
        let mut gen = SirGen::new(analyzer);

        // Collect function and struct metadata before any mutable borrows.
        struct FuncMeta {
            name: String,
            body_index: instr_index,
            parameters: Vec<Parameter>,
            return_type: Option<crate::frontend::ast::TypeExpression>,
            is_foreign: bool,
        }

        let function_meta: Vec<FuncMeta> = gen
            .analyzer
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
            .analyzer
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
                            let field =
                                SIRStructField { name: f.id.name.clone(), ty, byte_offset: offset };
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

        // Process main first so module-level variables enter module_context
        // before any function body is analyzed.
        gen.analyzer.setup_function_context(&[]);
        let region_data = match gen.analyzer.preir.main.clone() {
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

            let type_key: Vec<String> =
                params.iter().map(|(_, ty)| errand_type_name(ty)).collect();

            let info = SIRFunctionInfo {
                params,
                return_type,
                is_foreign: meta.is_foreign,
                body: if meta.is_foreign { None } else { Some(func_sir) },
            };

            functions.entry(meta.name).or_default().insert(type_key, info);
        }

        // Collect enum layouts from EnumDecl instructions, computing tagged-union sizes.
        let enum_layouts: HashMap<String, SIREnumLayout> = gen
            .analyzer
            .preir
            .instructions
            .iter()
            .filter_map(|instr| {
                if let Instr::EnumDecl(ed) = instr {
                    let mut max_payload = 0usize;
                    let variant_layouts: Vec<SIREnumVariantLayout> = ed.variants.iter().map(|v| {
                        let mut payload_offset = 0usize;
                        let fields: Vec<SIRStructField> = v.fields.iter().map(|(field_name, field_type)| {
                            let ty = type_expr_to_errand_type(field_type);
                            let size = errand_type_size(&ty);
                            let f = SIRStructField { name: field_name.clone(), ty, byte_offset: payload_offset };
                            payload_offset += size;
                            f
                        }).collect();
                        let payload_size = payload_offset;
                        if payload_size > max_payload { max_payload = payload_size; }
                        SIREnumVariantLayout { name: v.name.clone(), fields, payload_size }
                    }).collect();

                    let is_simple = variant_layouts.iter().all(|v| v.fields.is_empty());
                    // total_size = 8 bytes for tag + max payload; 0 when is_simple (bare int).
                    let total_size = if is_simple { 0 } else { 8 + max_payload };
                    Some((ed.name.clone(), SIREnumLayout { variants: variant_layouts, total_size, is_simple }))
                } else {
                    None
                }
            })
            .collect();

        Ok(SIRModule { main: main_sir, functions, structs: struct_layouts, enums: enum_layouts })
    }

    // ── Emission ──────────────────────────────────────────────────────────────

    /// Emit SIR for a function body rooted at `root_idx`.
    /// Resets the index map so local indices start at 0.
    fn emit_body_sir(&mut self, root_idx: instr_index) -> Result<SIR, String> {
        self.preir_to_sir.clear();
        let mut sir = SIR { instructions: Vec::new(), return_loc: 0 };
        let local_root = self.analyze_and_emit(root_idx, &mut sir)?;
        sir.return_loc = local_root;
        Ok(sir)
    }

    /// Emit SIR for the main region: walk `instr_start..instr_end` in order,
    /// skipping top-level declarations (they have their own entry points).
    fn emit_region_sir(&mut self, region_data: &RegionData) -> Result<SIR, String> {
        self.preir_to_sir.clear();
        let mut sir = SIR { instructions: Vec::new(), return_loc: 0 };

        // Instructions that belong to match arm body Regions are deferred so
        // they are emitted lazily inside their Region's SIR range. This ensures
        // that binding variables (e.g. `x`, `y` from `Message::Move(x, y)`)
        // have SIR indices that fall inside the arm body Region's range, which
        // is required for the lowering pass to correctly mark them as nested.
        let arm_owned = self.collect_arm_owned_preir(
            region_data.instr_start, region_data.instr_end,
        );

        for i in region_data.instr_start..region_data.instr_end {
            match self.analyzer.preir.get_instruction(i) {
                Some(Instr::FuncDecl(_)) | Some(Instr::StructDecl(_)) | Some(Instr::EnumDecl(_)) => continue,
                _ => {}
            }
            if arm_owned.contains(&i) {
                continue;
            }
            self.analyze_and_emit(i, &mut sir)?;
        }

        sir.return_loc = self.preir_to_sir.get(&region_data.return_loc).copied().unwrap_or(0);
        Ok(sir)
    }

    /// Collect all PreIR instruction indices that are "owned" by match arm body
    /// Regions within the given PreIR range. These must not be pre-emitted in
    /// a sequential scan; instead they must be emitted lazily inside the arm
    /// body's `emit_region_instr` call so that their SIR indices fall within
    /// the Region's `instr_start..instr_end` range.
    fn collect_arm_owned_preir(
        &self,
        start: instr_index,
        end: instr_index,
    ) -> HashSet<instr_index> {
        let mut owned = HashSet::new();
        for i in start..end {
            if let Some(Instr::Match(data)) = self.analyzer.preir.get_instruction(i) {
                for arm in &data.arms {
                    self.mark_arm_owned_preir(arm.body, &mut owned);
                }
            }
        }
        owned
    }

    /// Recursively mark `idx` and all PreIR instructions inside it as arm-owned.
    fn mark_arm_owned_preir(&self, idx: instr_index, owned: &mut HashSet<instr_index>) {
        if owned.contains(&idx) {
            return;
        }
        owned.insert(idx);
        match self.analyzer.preir.get_instruction(idx) {
            Some(Instr::Region(data)) => {
                let start = data.instr_start;
                let end = data.instr_end;
                for i in start..end {
                    self.mark_arm_owned_preir(i, owned);
                }
            }
            Some(Instr::Match(data)) => {
                for arm in &data.arms {
                    self.mark_arm_owned_preir(arm.body, owned);
                }
            }
            _ => {}
        }
    }

    /// Type the instruction at `global_idx` via `Analyzer`, emit a `SIRInstr`
    /// with remapped local indices, and return the local index.
    fn analyze_and_emit(
        &mut self,
        global_idx: instr_index,
        sir: &mut SIR,
    ) -> Result<instr_index, String> {
        if let Some(&local) = self.preir_to_sir.get(&global_idx) {
            return Ok(local);
        }

        let instr = self
            .analyzer
            .preir
            .get_instruction(global_idx)
            .cloned()
            .ok_or_else(|| format!("invalid PreIR index: {global_idx}"))?;

        // Region instructions: emit contained body first, then the Region node.
        if let Instr::Region(ref rd) = instr {
            return self.emit_region_instr(global_idx, rd.clone(), sir);
        }

        // Declarations nested inside a body: analyze only, don't emit into
        // this SIR (they are independent entry points at the module level).
        if matches!(instr, Instr::FuncDecl(_) | Instr::StructDecl(_) | Instr::EnumDecl(_)) {
            let _ = self.analyzer.analyze_instr(global_idx).map_err(|e| format!("{e:?}"));
            return Ok(-1);
        }

        // Recursively emit operand instructions and remap their indices.
        let remapped = self.remap_operands(instr, sir)?;

        // Type via the Analyzer (cached after the first call).
        let ty = self
            .analyzer
            .analyze_instr(global_idx)
            .map(|idx| self.analyzer.pool.to_errand_type(idx))
            .ok();

        let local_idx = sir.instructions.len() as instr_index;
        sir.instructions.push(SIRInstr { instr: remapped, ty });
        self.preir_to_sir.insert(global_idx, local_idx);

        Ok(local_idx)
    }

    /// Emit the instructions contained in a `Region`, then emit the Region
    /// node itself with adjusted local bounds.
    fn emit_region_instr(
        &mut self,
        global_idx: instr_index,
        rd: RegionData,
        sir: &mut SIR,
    ) -> Result<instr_index, String> {
        let new_start = sir.instructions.len() as instr_index;

        // Same deferral as `emit_region_sir`: skip instructions owned by any
        // match arm body within this Region so they are emitted lazily at SIR
        // indices inside the arm body Region's range.
        let arm_owned = self.collect_arm_owned_preir(rd.instr_start, rd.instr_end);

        for i in rd.instr_start..rd.instr_end {
            match self.analyzer.preir.get_instruction(i) {
                Some(Instr::FuncDecl(_)) | Some(Instr::StructDecl(_)) | Some(Instr::EnumDecl(_)) => {
                    let _ = self.analyzer.analyze_instr(i);
                    continue;
                }
                _ => {}
            }
            if arm_owned.contains(&i) {
                continue;
            }
            self.analyze_and_emit(i, sir)?;
        }

        let new_end = sir.instructions.len() as instr_index;
        let new_return_loc =
            self.preir_to_sir.get(&rd.return_loc).copied().unwrap_or(new_start);

        let remapped_region = Instr::Region(RegionData {
            instr_start: new_start,
            instr_end: new_end,
            return_loc: new_return_loc,
        });

        let ty = self
            .analyzer
            .analyze_instr(global_idx)
            .map(|idx| self.analyzer.pool.to_errand_type(idx))
            .ok();

        let local_idx = sir.instructions.len() as instr_index;
        sir.instructions.push(SIRInstr { instr: remapped_region, ty });
        self.preir_to_sir.insert(global_idx, local_idx);

        Ok(local_idx)
    }

    /// Return a new `Instr` with all operand `instr_index` values remapped from
    /// global PreIR space to local SIR space, emitting dependencies first.
    fn remap_operands(&mut self, instr: Instr, sir: &mut SIR) -> Result<Instr, String> {
        match instr {
            Instr::Literal(_)
            | Instr::VarRef(_)
            | Instr::StructDecl(_)
            | Instr::EnumDecl(_)
            | Instr::EnumVariantAccess(_) => Ok(instr),

            Instr::EnumVariantConstruct(data) => {
                let mut arg_indices = Vec::new();
                for idx in data.arg_indices {
                    arg_indices.push(self.analyze_and_emit(idx, sir)?);
                }
                Ok(Instr::EnumVariantConstruct(crate::backend::preir::EnumVariantConstructData {
                    enum_name: data.enum_name,
                    variant: data.variant,
                    arg_indices,
                }))
            }

            Instr::Match(data) => {
                let scrutinee = self.analyze_and_emit(data.scrutinee, sir)?;

                // Obtain variant info so we can register binding variable types
                // into global_defs before emitting each arm body.
                let variants: Vec<_> = self.analyzer.enum_variants
                    .get(&data.enum_name)
                    .cloned()
                    .unwrap_or_default();

                let mut arms = Vec::new();
                for arm in &data.arms {
                    // Register binding names as typed globals so that VarRef
                    // instructions in the arm body resolve during analysis.
                    if !arm.bindings.is_empty() {
                        if let Some(tag) = arm.tag {
                            if let Some(variant_info) = variants.get(tag as usize) {
                                for (i, binding_name) in arm.bindings.iter().enumerate() {
                                    if let Some((_, field_type)) = variant_info.fields.get(i) {
                                        let ty = type_expr_to_errand_type(field_type);
                                        let ty_idx = self.analyzer.pool.intern(ty);
                                        self.analyzer.global_defs.insert(binding_name.clone(), ty_idx);
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
                let else_branch = data.else_branch
                    .map(|e| self.analyze_and_emit(e, sir))
                    .transpose()?;
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
                let value = data.value.map(|v| self.analyze_and_emit(v, sir)).transpose()?;
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

            Instr::Region(_) => unreachable!("Region handled before remap_operands"),
        }
    }
}

// ─── Utilities ────────────────────────────────────────────────────────────────

pub(crate) fn errand_type_name(ty: &ErrandType) -> String {
    match ty {
        ErrandType::Con(n) | ErrandType::Var(n) | ErrandType::ETVar(n) => n.clone(),
        ErrandType::Arrow(_, _) => "Function".into(),
        ErrandType::Forall(_, _) => "Forall".into(),
        ErrandType::Product(_) => "Product".into(),
        ErrandType::Sum(_) => "Sum".into(),
    }
}

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
