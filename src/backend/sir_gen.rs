use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};

use crate::backend::analysis::Analyzer;
use crate::backend::errand_builtins::{
    type_expr_to_errand_type, type_expr_to_errand_type_with_params,
};
use crate::backend::preir::PreIR;
use crate::backend::preir::{
    instr_index, BinOpPl, EnumData, EnumVariantConstructData, FnCallPl, ForLoopData, FuncData,
    IfStatementData, Instr, LiteralPl, MatchArmData, MatchData, RegionData, ReturnData, StructData,
    UnOpPl, VarDeclData,
};
use crate::backend::sir::{
    SIREnumLayout, SIREnumVariantLayout, SIRFunctionInfo, SIRInstr, SIRModule, SIRStructField,
    SIRStructLayout, SIR,
};
use crate::backend::worklist::ErrandType;
use crate::frontend::ast::{GenericArg, Id, Parameter, Program, TypeExpression};

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
        SirGen {
            analyzer,
            preir_to_sir: HashMap::new(),
        }
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
                    if !sd.type_params.is_empty() {
                        return None;
                    }
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
                    Some((
                        sd.name.clone(),
                        SIRStructLayout {
                            fields,
                            total_size: offset,
                        },
                    ))
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
            .preir
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
                            let mut payload_offset = 0usize;
                            let fields: Vec<SIRStructField> = v
                                .fields
                                .iter()
                                .map(|(field_name, field_type)| {
                                    let ty = type_expr_to_errand_type(field_type);
                                    let size = errand_type_size(&ty);
                                    let f = SIRStructField {
                                        name: field_name.clone(),
                                        ty,
                                        byte_offset: payload_offset,
                                    };
                                    payload_offset += size;
                                    f
                                })
                                .collect();
                            let payload_size = payload_offset;
                            if payload_size > max_payload {
                                max_payload = payload_size;
                            }
                            SIREnumVariantLayout {
                                name: v.name.clone(),
                                fields,
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

    /// Collapse `App` types, patch `new`/enum names, and add mangled struct/enum layouts.
    fn finalize_generics(&mut self, module: &mut SIRModule) -> Result<(), String> {
        Self::collapse_sir_types(&mut module.main, &self.analyzer);
        for overloads in module.functions.values_mut() {
            for info in overloads.values_mut() {
                if let Some(ref mut body) = info.body {
                    Self::collapse_sir_types(body, &self.analyzer);
                }
            }
        }
        Self::patch_mangled_names(&mut module.main);
        for overloads in module.functions.values_mut() {
            for info in overloads.values_mut() {
                if let Some(ref mut body) = info.body {
                    Self::patch_mangled_names(body);
                }
            }
        }
        self.monomorph_generic_struct_constructors(module)?;
        Self::collapse_sir_types(&mut module.main, &self.analyzer);
        for overloads in module.functions.values_mut() {
            for info in overloads.values_mut() {
                if let Some(ref mut body) = info.body {
                    Self::collapse_sir_types(body, &self.analyzer);
                }
            }
        }
        Self::patch_mangled_names(&mut module.main);
        for overloads in module.functions.values_mut() {
            for info in overloads.values_mut() {
                if let Some(ref mut body) = info.body {
                    Self::patch_mangled_names(body);
                }
            }
        }
        Self::ensure_mangled_layouts(module, &self.analyzer.preir)?;
        Ok(())
    }

    fn collapse_sir_types(sir: &mut SIR, analyzer: &Analyzer) {
        for si in &mut sir.instructions {
            if let Some(ty) = si.ty.take() {
                si.ty = Some(analyzer.collapse_apps_in_type(ty));
            }
        }
    }

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
                    Instr::Match(MatchData {
                        ref mut enum_name, ..
                    }) => {
                        *enum_name = m.clone();
                    }
                    Instr::FnCall(fc) if fc.name == "new" && !fc.arguments.is_empty() => {
                        new_symbol_patches.push((fc.arguments[0] as usize, m.clone()));
                    }
                    _ => {}
                }
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

    /// Same key scheme as [`crate::backend::sir_lowering`] overload resolution.
    fn sir_type_key_for_dispatch(ty: Option<&ErrandType>) -> String {
        match ty {
            Some(ErrandType::Con(n)) => n.clone(),
            Some(ErrandType::Var(n)) | Some(ErrandType::ETVar(n)) => n.clone(),
            _ => "Any".to_string(),
        }
    }

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

    fn patch_new_symbol_in_body(sir: &mut SIR, base: &str, mangled: &str) {
        let mut patches: Vec<(usize, String)> = Vec::new();
        for si in &sir.instructions {
            if let Instr::FnCall(fc) = &si.instr {
                if fc.name == "new" && !fc.arguments.is_empty() {
                    if let Some(arg0) = sir.instructions.get(fc.arguments[0] as usize) {
                        if let Instr::Literal(LiteralPl::Symbol(s)) = &arg0.instr {
                            if s == base {
                                patches.push((fc.arguments[0] as usize, mangled.to_string()));
                            }
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
            if keys.iter().any(|k| k == "Any") {
                continue;
            }
            out.entry(keys).or_insert(tys);
        }
    }

    fn gather_all_generic_ctor_call_shapes(
        module: &SIRModule,
        ctor_name: &str,
        expand: impl Fn(&ErrandType) -> ErrandType + Copy,
        out: &mut HashMap<Vec<String>, Vec<ErrandType>>,
    ) {
        Self::collect_generic_ctor_call_shapes(&module.main, ctor_name, expand, out);
        for overloads in module.functions.values() {
            for info in overloads.values() {
                if let Some(body) = &info.body {
                    Self::collect_generic_ctor_call_shapes(body, ctor_name, expand, out);
                }
            }
        }
    }

    fn monomorph_generic_struct_constructors(&self, module: &mut SIRModule) -> Result<(), String> {
        let generic_structs: Vec<StructData> = self
            .analyzer
            .preir
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
                let return_ty = body
                    .instructions
                    .get(body.return_loc as usize)
                    .and_then(|i| i.ty.clone())
                    .unwrap_or_else(|| ErrandType::Con("Void".into()));

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

    fn collect_mangled_symbols_from_sir(sir: &SIR, mangled: &mut HashSet<String>) {
        for si in &sir.instructions {
            if let Instr::Literal(LiteralPl::Symbol(s)) = &si.instr {
                if s.contains("__") {
                    mangled.insert(s.clone());
                }
            }
        }
    }

    fn ensure_mangled_layouts(module: &mut SIRModule, preir: &PreIR) -> Result<(), String> {
        let mut mangled: HashSet<String> = HashSet::new();
        for si in &module.main.instructions {
            if let Some(ErrandType::Con(m)) = &si.ty {
                if m.contains("__") {
                    mangled.insert(m.clone());
                }
            }
        }
        Self::collect_mangled_symbols_from_sir(&module.main, &mut mangled);
        for overloads in module.functions.values() {
            for info in overloads.values() {
                if let Some(body) = &info.body {
                    for si in &body.instructions {
                        if let Some(ErrandType::Con(m)) = &si.ty {
                            if m.contains("__") {
                                mangled.insert(m.clone());
                            }
                        }
                    }
                    Self::collect_mangled_symbols_from_sir(body, &mut mangled);
                }
            }
        }

        let struct_defs: Vec<StructData> = preir
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
        let enum_defs: Vec<EnumData> = preir
            .instructions
            .iter()
            .filter_map(|i| {
                if let Instr::EnumDecl(ed) = i {
                    if ed.type_params.is_empty() {
                        None
                    } else {
                        Some(ed.clone())
                    }
                } else {
                    None
                }
            })
            .collect();

        for m in mangled {
            if module.structs.contains_key(&m) || module.enums.contains_key(&m) {
                continue;
            }
            if let Some((sd, subst)) = parse_mangled_struct(&m, &struct_defs) {
                let layout = build_struct_layout_subst(&sd, &subst)?;
                module.structs.insert(m, layout);
                continue;
            }
            if let Some((ed, subst)) = parse_mangled_enum(&m, &enum_defs) {
                let layout = build_enum_layout_subst(&ed, &subst)?;
                module.enums.insert(m, layout);
            }
        }
        Ok(())
    }

    // ── Emission ──────────────────────────────────────────────────────────────

    /// PreIR lays out `while cond do (block)` as: `cond…`, `block` statements, the block's
    /// `Region` node, then the `WhileLoop`. A linear scan would emit the block twice (once
    /// while walking the parent range, again when lowering the `WhileLoop`), leaving the
    /// `WhileLoop`'s body `Region` empty in SIR because `preir_to_sir` is already populated.
    /// Skip the operand `Region` and its `instr_start..instr_end` range whenever we see a
    /// control-flow instruction that owns them.
    fn control_flow_operand_skip_indices(
        &self,
        start: instr_index,
        end: instr_index,
    ) -> HashSet<instr_index> {
        let mut skip = HashSet::new();
        let preir = &self.analyzer.preir;
        for i in start..end {
            match preir.get_instruction(i) {
                Some(Instr::WhileLoop(d)) => Self::mark_region_body_skip(preir, d.body, &mut skip),
                Some(Instr::ForLoop(d)) => Self::mark_region_body_skip(preir, d.body, &mut skip),
                Some(Instr::IfStatement(d)) => {
                    Self::mark_region_body_skip(preir, d.then_branch, &mut skip);
                    if let Some(e) = d.else_branch {
                        Self::mark_region_body_skip(preir, e, &mut skip);
                    }
                }
                _ => {}
            }
        }
        skip
    }

    fn mark_region_body_skip(preir: &PreIR, body: instr_index, skip: &mut HashSet<instr_index>) {
        if let Some(Instr::Region(rd)) = preir.get_instruction(body) {
            skip.insert(body);
            for j in rd.instr_start..rd.instr_end {
                skip.insert(j);
            }
        }
    }

    /// Emit SIR for a function body rooted at `root_idx`.
    /// Resets the index map so local indices start at 0.
    fn emit_body_sir(&mut self, root_idx: instr_index) -> Result<SIR, String> {
        self.preir_to_sir.clear();
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
    fn emit_region_sir(&mut self, region_data: &RegionData) -> Result<SIR, String> {
        self.preir_to_sir.clear();
        let mut sir = SIR {
            instructions: Vec::new(),
            return_loc: 0,
        };

        // Instructions that belong to match arm body Regions are deferred so
        // they are emitted lazily inside their Region's SIR range. This ensures
        // that binding variables (e.g. `x`, `y` from `Message::Move(x, y)`)
        // have SIR indices that fall inside the arm body Region's range, which
        // is required for the lowering pass to correctly mark them as nested.
        let arm_owned =
            self.collect_arm_owned_preir(region_data.instr_start, region_data.instr_end);
        let cf_skip =
            self.control_flow_operand_skip_indices(region_data.instr_start, region_data.instr_end);

        for i in region_data.instr_start..region_data.instr_end {
            match self.analyzer.preir.get_instruction(i) {
                Some(Instr::FuncDecl(_))
                | Some(Instr::StructDecl(_))
                | Some(Instr::EnumDecl(_)) => continue,
                _ => {}
            }
            if arm_owned.contains(&i) || cf_skip.contains(&i) {
                continue;
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

        let local_idx = sir.instructions.len() as instr_index;
        sir.instructions.push(SIRInstr {
            instr: remapped,
            ty,
        });
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
        let cf_skip = self.control_flow_operand_skip_indices(rd.instr_start, rd.instr_end);

        for i in rd.instr_start..rd.instr_end {
            match self.analyzer.preir.get_instruction(i) {
                Some(Instr::FuncDecl(_))
                | Some(Instr::StructDecl(_))
                | Some(Instr::EnumDecl(_)) => {
                    let _ = self.analyzer.analyze_instr(i);
                    continue;
                }
                _ => {}
            }
            if arm_owned.contains(&i) || cf_skip.contains(&i) {
                continue;
            }
            self.analyze_and_emit(i, sir)?;
        }

        let new_end = sir.instructions.len() as instr_index;
        let new_return_loc = self
            .preir_to_sir
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

        let local_idx = sir.instructions.len() as instr_index;
        sir.instructions.push(SIRInstr {
            instr: remapped_region,
            ty,
        });
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
                Ok(Instr::EnumVariantConstruct(
                    crate::backend::preir::EnumVariantConstructData {
                        enum_name: data.enum_name,
                        variant: data.variant,
                        arg_indices,
                    },
                ))
            }

            Instr::Match(data) => {
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
                                        let mut ty = type_expr_to_errand_type_with_params(
                                            field_type,
                                            &enum_tparams,
                                        );
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

            Instr::VarDecl(data) => {
                let value = self.analyze_and_emit(data.value, sir)?;
                Ok(Instr::VarDecl(VarDeclData {
                    name: data.name,
                    value,
                    declared_type: data.declared_type.clone(),
                }))
            }

            Instr::UnOp(data) => {
                let operand = self.analyze_and_emit(data.operand, sir)?;
                Ok(Instr::UnOp(UnOpPl {
                    op: data.op,
                    operand,
                }))
            }

            Instr::BinOp(data) => {
                let left = self.analyze_and_emit(data.left, sir)?;
                let right = self.analyze_and_emit(data.right, sir)?;
                Ok(Instr::BinOp(BinOpPl {
                    op: data.op,
                    left,
                    right,
                }))
            }

            Instr::FnCall(data) => {
                let mut arguments = Vec::new();
                for arg_idx in data.arguments {
                    arguments.push(self.analyze_and_emit(arg_idx, sir)?);
                }
                Ok(Instr::FnCall(FnCallPl {
                    name: data.name,
                    arguments,
                }))
            }

            Instr::Typeof(operand) => {
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
                    .ok_or_else(|| {
                        format!("typeof: cannot resolve type of operand %{}", operand)
                    })?;
                let name = errand_type_name(&ty);
                Ok(Instr::Literal(LiteralPl::Symbol(name)))
            }

            Instr::IfStatement(data) => {
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

            Instr::WhileLoop(data) => {
                let condition = self.analyze_and_emit(data.condition, sir)?;
                let body = self.analyze_and_emit(data.body, sir)?;
                Ok(Instr::WhileLoop(crate::backend::preir::WhileLoopData {
                    condition,
                    body,
                }))
            }

            Instr::ForLoop(data) => {
                let range = self.analyze_and_emit(data.range, sir)?;
                let body = self.analyze_and_emit(data.body, sir)?;
                Ok(Instr::ForLoop(ForLoopData {
                    iterator: data.iterator,
                    range,
                    body,
                }))
            }

            Instr::Return(data) => {
                let value = data
                    .value
                    .map(|v| self.analyze_and_emit(v, sir))
                    .transpose()?;
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

fn parse_mangled_struct(
    m: &str,
    defs: &[StructData],
) -> Option<(StructData, HashMap<String, TypeExpression>)> {
    for sd in defs {
        let prefix = format!("{}__", sd.name);
        if !m.starts_with(&prefix) {
            continue;
        }
        let rest = &m[sd.name.len() + 2..];
        let arg_strs: Vec<&str> = rest.split("__").filter(|s| !s.is_empty()).collect();
        if arg_strs.len() != sd.type_params.len() {
            continue;
        }
        let mut subst = HashMap::new();
        for (tp, an) in sd.type_params.iter().zip(arg_strs.iter()) {
            subst.insert(tp.name.clone(), primitive_name_to_type_expr(an).ok()?);
        }
        return Some((sd.clone(), subst));
    }
    None
}

fn build_struct_layout_subst(
    sd: &StructData,
    subst: &HashMap<String, TypeExpression>,
) -> Result<SIRStructLayout, String> {
    let mut offset = 0usize;
    let mut fields = Vec::new();
    for f in &sd.fields {
        let ft = subst_type_expr(&f.field_type, subst);
        let ty = type_expr_to_errand_type(&ft);
        let size = errand_type_size(&ty);
        fields.push(SIRStructField {
            name: f.id.name.clone(),
            ty,
            byte_offset: offset,
        });
        offset += size;
    }
    Ok(SIRStructLayout {
        fields,
        total_size: offset,
    })
}

fn parse_mangled_enum(
    m: &str,
    defs: &[EnumData],
) -> Option<(EnumData, HashMap<String, TypeExpression>)> {
    for ed in defs {
        let prefix = format!("{}__", ed.name);
        if !m.starts_with(&prefix) {
            continue;
        }
        let rest = &m[ed.name.len() + 2..];
        let arg_strs: Vec<&str> = rest.split("__").filter(|s| !s.is_empty()).collect();
        if arg_strs.len() != ed.type_params.len() {
            continue;
        }
        let mut subst = HashMap::new();
        for (tp, an) in ed.type_params.iter().zip(arg_strs.iter()) {
            subst.insert(tp.name.clone(), primitive_name_to_type_expr(an).ok()?);
        }
        return Some((ed.clone(), subst));
    }
    None
}

fn build_enum_layout_subst(
    ed: &EnumData,
    subst: &HashMap<String, TypeExpression>,
) -> Result<SIREnumLayout, String> {
    let mut max_payload = 0usize;
    let variant_layouts: Vec<SIREnumVariantLayout> = ed
        .variants
        .iter()
        .map(|v| {
            let mut payload_offset = 0usize;
            let fields: Vec<SIRStructField> = v
                .fields
                .iter()
                .map(|(field_name, field_type)| {
                    let ft = subst_type_expr(field_type, subst);
                    let ty = type_expr_to_errand_type(&ft);
                    let size = errand_type_size(&ty);
                    let f = SIRStructField {
                        name: field_name.clone(),
                        ty,
                        byte_offset: payload_offset,
                    };
                    payload_offset += size;
                    f
                })
                .collect();
            let payload_size = payload_offset;
            if payload_size > max_payload {
                max_payload = payload_size;
            }
            SIREnumVariantLayout {
                name: v.name.clone(),
                fields,
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
