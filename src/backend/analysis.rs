use std::collections::HashMap;

use crate::backend::errand_builtins::{
    add_builtin_data_constructors, add_builtin_functions, type_expr_to_errand_type,
    type_expr_to_errand_type_with_params,
};
use crate::backend::preir::{
    BinOpPl, EnumData, EnumVariantData, EnumVariantConstructData, EnumVariantInfo, FnCallPl,
    FuncData, IfStatementData, Instr, LiteralPl, MatchData, PreIR, RegionData, ReturnData,
    StructData, UnOpPl, VarDeclData, VarRefData, WhileLoopData, instr_index,
};
use crate::backend::worklist::{
    ErrandType, ErrandTypeError, Judgment, TyVarKind, TypeResult, Worklist, WorklistEntry,
};
use crate::frontend::ast::{BinaryOperator, Parameter, Program, UnaryOperator};

// ─── TypePool ────────────────────────────────────────────────────────────────

/// A cheap, `Copy` handle into the `TypePool`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeIndex(u32);

/// Stable intern pool for `ErrandType` values.
///
/// Primitive types are pre-populated at construction time so they can be
/// referenced by their well-known `TypeIndex` handles without any allocation.
pub struct TypePool {
    types: Vec<ErrandType>,
    // Well-known pre-populated handles
    pub int: TypeIndex,
    pub float: TypeIndex,
    pub bool_: TypeIndex,
    pub string: TypeIndex,
    pub unit: TypeIndex,
}

impl TypePool {
    pub fn new() -> Self {
        let mut types = Vec::new();

        let push = |types: &mut Vec<ErrandType>, ty: ErrandType| -> TypeIndex {
            let idx = TypeIndex(types.len() as u32);
            types.push(ty);
            idx
        };

        let int = push(&mut types, ErrandType::Con("Int".into()));
        let float = push(&mut types, ErrandType::Con("Float".into()));
        let bool_ = push(&mut types, ErrandType::Con("Bool".into()));
        let string = push(&mut types, ErrandType::Con("String".into()));
        let unit = push(&mut types, ErrandType::Con("Unit".into()));

        TypePool { types, int, float, bool_, string, unit }
    }

    /// Store a type in the pool and return its stable handle.
    pub fn intern(&mut self, ty: ErrandType) -> TypeIndex {
        let idx = TypeIndex(self.types.len() as u32);
        self.types.push(ty);
        idx
    }

    /// Borrow the type behind a handle.
    pub fn get(&self, idx: TypeIndex) -> &ErrandType {
        &self.types[idx.0 as usize]
    }

    /// Clone the type behind a handle (used when passing to SIR structures that
    /// own `ErrandType` values directly).
    pub fn to_errand_type(&self, idx: TypeIndex) -> ErrandType {
        self.get(idx).clone()
    }

    /// Intern an `ErrandType` that came from outside the pool (e.g. from
    /// `errand_builtins`).  Arrow / compound types are stored recursively.
    pub fn intern_errand_type(&mut self, ty: ErrandType) -> TypeIndex {
        self.intern(ty)
    }
}

// ─── Analyzer ─────────────────────────────────────────────────────────────────

/// Demand-driven semantic analysis over `PreIR`, analogous to Zig's `Sema`.
///
/// The `Analyzer` owns the `PreIR`, a `TypePool` for interned types, per-function
/// and module variable contexts, and the constraint-solving `Worklist`.
/// Results are cached by instruction index so each instruction is typed at most once.
pub struct Analyzer {
    pub pool: TypePool,
    pub preir: PreIR,
    /// Term variable context for the current function (cleared per function).
    var_context: HashMap<String, TypeIndex>,
    /// Module-level variable context (survives function boundaries).
    module_context: HashMap<String, TypeIndex>,
    /// Cache: PreIR `instr_index` → resolved `TypeIndex`.
    analysis_cache: HashMap<instr_index, TypeIndex>,
    /// Global symbol table: function / struct / builtin names → `TypeIndex`.
    pub global_defs: HashMap<String, TypeIndex>,
    /// Enum variant info: enum_name → ordered variant descriptors.
    /// Used to validate `EnumVariantAccess` and `EnumVariantConstruct` instructions.
    pub enum_variants: HashMap<String, Vec<EnumVariantInfo>>,
    /// Generic type parameter names per enum (empty for non-generic enums).
    pub enum_type_params: HashMap<String, Vec<String>>,
    worklist: Worklist,
    trace: Vec<String>,
}

impl Analyzer {
    /// Build an `Analyzer` from a compiled `PreIR` and the source `Program`.
    ///
    /// Performs the global definition-collection pass (builtins + function
    /// placeholders + struct constructors) before returning.
    pub fn new(preir: PreIR, _program: &Program) -> Self {
        let mut pool = TypePool::new();
        let mut worklist = Worklist::new();
        let global_defs = Self::collect_global_defs(&preir, &mut pool, &mut worklist);

        // Build enum_variants from EnumDecl instructions.
        let mut enum_variants: HashMap<String, Vec<EnumVariantInfo>> = HashMap::new();
        let mut enum_type_params: HashMap<String, Vec<String>> = HashMap::new();
        for instr in &preir.instructions {
            if let Instr::EnumDecl(EnumData {
                name,
                type_params,
                variants,
            }) = instr
            {
                enum_type_params.insert(name.clone(), type_params.iter().map(|p| p.name.clone()).collect());
                enum_variants.insert(name.clone(), variants.clone());
            }
        }

        Analyzer {
            pool,
            preir,
            var_context: HashMap::new(),
            module_context: HashMap::new(),
            analysis_cache: HashMap::new(),
            global_defs,
            enum_variants,
            enum_type_params,
            worklist,
            trace: Vec::new(),
        }
    }

    /// Expand existential solutions; use when reading types after `solve()`.
    pub fn expand_type(&self, ty: &ErrandType) -> ErrandType {
        self.worklist.expand_type(ty)
    }

    pub fn expanded_pool_type(&self, idx: TypeIndex) -> ErrandType {
        self.expand_type(&self.pool.to_errand_type(idx))
    }

    /// Turn a fully-instantiated `App(Con(Foo), …)` into `Con(Foo__…)` for codegen / mangling.
    pub fn collapse_apps_in_type(&self, ty: ErrandType) -> ErrandType {
        let t = self.expand_type(&ty);
        match &t {
            ErrandType::App(h, args)
                if matches!(h.as_ref(), ErrandType::Con(_))
                    && args.iter().all(|a| !Self::type_contains_etvar(a)) =>
            {
                if let ErrandType::Con(head) = h.as_ref() {
                    ErrandType::Con(Self::mangle_type_app(head, args))
                } else {
                    t
                }
            }
            _ => t,
        }
    }

    fn mangle_type_app(head: &str, args: &[ErrandType]) -> String {
        let mut s = head.to_string();
        for a in args {
            s.push_str("__");
            s.push_str(&Self::mangle_type_component(a));
        }
        s
    }

    fn mangle_type_component(ty: &ErrandType) -> String {
        match ty {
            ErrandType::Con(n) | ErrandType::Var(n) | ErrandType::ETVar(n) => n.clone(),
            ErrandType::App(h, inner) => {
                if let ErrandType::Con(hn) = h.as_ref() {
                    Self::mangle_type_app(hn, inner)
                } else {
                    "App".to_string()
                }
            }
            _ => "Ty".to_string(),
        }
    }

    fn type_contains_etvar(ty: &ErrandType) -> bool {
        match ty {
            ErrandType::ETVar(_) => true,
            ErrandType::Arrow(a, b) => {
                Self::type_contains_etvar(a) || Self::type_contains_etvar(b)
            }
            ErrandType::App(h, a) => {
                Self::type_contains_etvar(h) || a.iter().any(Self::type_contains_etvar)
            }
            ErrandType::Forall(_, b) => Self::type_contains_etvar(b),
            ErrandType::Product(ts) => {
                ts.iter().any(Self::type_contains_etvar)
            }
            ErrandType::Var(_) | ErrandType::Con(_) => false,
        }
    }

    // ── Context management ────────────────────────────────────────────────────

    /// Populate `var_context` from function parameters, clearing any previous
    /// per-function state.  Module-level context is not touched.
    pub fn setup_function_context(&mut self, parameters: &[Parameter]) {
        self.var_context.clear();
        for param in parameters {
            let ty = match &param.type_expr {
                Some(te) => {
                    let et = type_expr_to_errand_type(te);
                    self.pool.intern(et)
                }
                None => {
                    let et = ErrandType::ETVar(format!("param_{}", param.id.name));
                    self.pool.intern(et)
                }
            };
            self.var_context.insert(param.id.name.clone(), ty);
        }
    }

    /// Move all entries from `var_context` into `module_context`.
    /// Call after analyzing the main region so top-level bindings are visible
    /// inside function bodies.
    pub fn promote_to_module(&mut self) {
        self.module_context.extend(self.var_context.drain());
    }

    // ── Public analysis entry points ──────────────────────────────────────────

    /// Analyze every instruction in a region in forward order.
    ///
    /// `VarDecl` instructions extend `var_context` as a side-effect so that
    /// later instructions in the same region can resolve the declared names.
    /// Returns the type of the region's return location.
    pub fn analyze_body(&mut self, region: &RegionData) -> TypeResult<TypeIndex> {
        let mut last_ty = self.pool.unit;
        for i in region.instr_start..region.instr_end {
            if let Some(instr) = self.preir.get_instruction(i) {
                match instr {
                    Instr::FuncDecl(_) | Instr::StructDecl(_) | Instr::EnumDecl(_) => continue,
                    Instr::VarDecl(_) => {
                        self.analyze_instr(i)?;
                    }
                    _ => {
                        last_ty = self.analyze_instr(i)?;
                    }
                }
            }
        }
        self.solve()?;
        Ok(last_ty)
    }

    /// Type an instruction by index.  Results are memoized.
    pub fn analyze_instr(&mut self, idx: instr_index) -> TypeResult<TypeIndex> {
        if let Some(&cached) = self.analysis_cache.get(&idx) {
            return Ok(cached);
        }

        let instr = self
            .preir
            .get_instruction(idx)
            .ok_or_else(|| {
                ErrandTypeError::UnsupportedOperation(format!("invalid instr index {idx}"))
            })?
            .clone();

        let ty = self.analyze_instr_inner(&instr)?;
        self.analysis_cache.insert(idx, ty);
        Ok(ty)
    }

    /// Look up the cached `TypeIndex` for an already-analyzed instruction, or
    /// return the pool's `unit` handle if not yet analyzed.
    pub fn cached_type(&self, idx: instr_index) -> Option<TypeIndex> {
        self.analysis_cache.get(&idx).copied()
    }

    // ── Core dispatch ─────────────────────────────────────────────────────────

    fn analyze_instr_inner(&mut self, instr: &Instr) -> TypeResult<TypeIndex> {
        match instr {
            Instr::Literal(lit) => Ok(self.infer_literal(lit)),
            Instr::VarRef(data) => self.analyze_var_ref(data),
            Instr::BinOp(data) => self.analyze_binop(data),
            Instr::UnOp(data) => self.analyze_unop(data),
            Instr::FnCall(data) => self.analyze_call(data),
            Instr::IfStatement(data) => self.analyze_if(data),
            Instr::WhileLoop(data) => self.analyze_while(data),
            Instr::Return(data) => self.analyze_return(data),
            Instr::Region(data) => self.analyze_region_instr(data),
            Instr::VarDecl(data) => self.analyze_var_decl(data),
            Instr::FuncDecl(data) => self.analyze_func_decl(data),
            Instr::StructDecl(_) => Ok(self.pool.unit),
            Instr::EnumDecl(_) => Ok(self.pool.unit),
            Instr::EnumVariantAccess(data) => self.analyze_enum_variant_access(data),
            Instr::EnumVariantConstruct(data) => self.analyze_enum_variant_construct(data),
            Instr::Match(data) => self.analyze_match(data),
            Instr::Typeof(operand) => {
                // `typeof(x)` evaluates to a string holding the type name of
                // `x`.  Type-check the operand so its type is known by SIR
                // emission, but the result is always `String`.
                self.analyze_instr(*operand)?;
                Ok(self.pool.string)
            }
            Instr::ForLoop(_) => {
                Err(ErrandTypeError::UnsupportedOperation("ForLoop".into()))
            }
        }
    }

    // ── Per-instruction analysis arms ─────────────────────────────────────────

    fn infer_literal(&self, lit: &LiteralPl) -> TypeIndex {
        match lit {
            LiteralPl::Int(_) => self.pool.int,
            LiteralPl::Float(_) => self.pool.float,
            LiteralPl::Boolean(_) => self.pool.bool_,
            LiteralPl::String(_) => self.pool.string,
            LiteralPl::Symbol(_) => self.pool.string,
            LiteralPl::Unit => self.pool.unit,
        }
    }

    fn analyze_var_ref(&mut self, data: &VarRefData) -> TypeResult<TypeIndex> {
        if let Some(&ty) = self.var_context.get(&data.name) {
            return Ok(ty);
        }
        if let Some(&ty) = self.module_context.get(&data.name) {
            return Ok(ty);
        }
        if let Some(&ty) = self.global_defs.get(&data.name) {
            return Ok(ty);
        }
        Err(ErrandTypeError::UnboundVariable(data.name.clone()))
    }

    /// Type a unit enum variant access, e.g. `Direction::North`.
    ///
    /// Validates that the enum and variant both exist, then returns `Con(enum_name)`.
    fn analyze_enum_variant_access(&mut self, data: &EnumVariantData) -> TypeResult<TypeIndex> {
        let variants = self.enum_variants.get(&data.enum_name).cloned().ok_or_else(|| {
            ErrandTypeError::UnboundVariable(format!("unknown enum `{}`", data.enum_name))
        })?;
        if !variants.iter().any(|v| v.name == data.variant) {
            return Err(ErrandTypeError::UnboundVariable(format!(
                "unknown variant `{}` on enum `{}`",
                data.variant, data.enum_name
            )));
        }
        let type_params = self
            .enum_type_params
            .get(&data.enum_name)
            .cloned()
            .unwrap_or_default();
        let mut ctor_ty = ErrandType::App(
            Box::new(ErrandType::Con(data.enum_name.clone())),
            type_params
                .iter()
                .map(|p| ErrandType::Var(p.clone()))
                .collect(),
        );
        for tp in type_params.iter().rev() {
            ctor_ty = ErrandType::Forall(tp.clone(), Box::new(ctor_ty));
        }
        let instantiated = self.instantiate_constructor_type(&ctor_ty)?;
        let ty = self.pool.intern(instantiated);
        Ok(ty)
    }

    /// Type a data-carrying enum variant construction, e.g. `Message::Move(1, 2)`.
    ///
    /// Validates enum name, variant name, and argument count.
    /// Returns `Con(enum_name)` — the enum's own type.
    fn analyze_enum_variant_construct(
        &mut self,
        data: &EnumVariantConstructData,
    ) -> TypeResult<TypeIndex> {
        let variants = self.enum_variants.get(&data.enum_name).cloned().ok_or_else(|| {
            ErrandTypeError::UnboundVariable(format!("unknown enum `{}`", data.enum_name))
        })?;
        let variant_info = variants
            .iter()
            .find(|v| v.name == data.variant)
            .cloned()
            .ok_or_else(|| {
                ErrandTypeError::UnboundVariable(format!(
                    "unknown variant `{}` on enum `{}`",
                    data.variant, data.enum_name
                ))
            })?;

        if data.arg_indices.len() != variant_info.fields.len() {
            return Err(ErrandTypeError::UnboundVariable(format!(
                "enum variant `{}::{}` expects {} argument(s), got {}",
                data.enum_name,
                data.variant,
                variant_info.fields.len(),
                data.arg_indices.len()
            )));
        }

        // Type-check each argument — just analyze them (types will be inferred).
        let type_params = self
            .enum_type_params
            .get(&data.enum_name)
            .cloned()
            .unwrap_or_default();

        let ret_app = ErrandType::App(
            Box::new(ErrandType::Con(data.enum_name.clone())),
            type_params
                .iter()
                .map(|p| ErrandType::Var(p.clone()))
                .collect(),
        );

        let mut ctor_ty = if variant_info.fields.is_empty() {
            ret_app
        } else {
            variant_info.fields.iter().rev().fold(ret_app, |acc, (_, ft)| {
                ErrandType::Arrow(
                    Box::new(type_expr_to_errand_type_with_params(ft, &type_params)),
                    Box::new(acc),
                )
            })
        };
        for tp in type_params.iter().rev() {
            ctor_ty = ErrandType::Forall(tp.clone(), Box::new(ctor_ty));
        }

        let mut arg_types = Vec::new();
        for &idx in &data.arg_indices {
            arg_types.push(self.analyze_instr(idx)?);
        }

        let mut remaining = self.instantiate_constructor_type(&ctor_ty)?;
        for arg_ty in &arg_types {
            match remaining {
                ErrandType::Arrow(param_ty, result_ty) => {
                    self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                        left: self.pool.to_errand_type(*arg_ty),
                        right: *param_ty,
                    }));
                    remaining = *result_ty;
                }
                ErrandType::ETVar(ref evar_name) => {
                    let evar = evar_name.clone();
                    self.worklist
                        .push(WorklistEntry::TVar(evar.clone(), TyVarKind::Existential));
                    let result_evar = self.worklist.fresh_evar();
                    self.worklist
                        .push(WorklistEntry::TVar(result_evar.clone(), TyVarKind::Existential));
                    let expected_arrow = ErrandType::Arrow(
                        Box::new(self.pool.to_errand_type(*arg_ty)),
                        Box::new(ErrandType::ETVar(result_evar.clone())),
                    );
                    self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                        left: ErrandType::ETVar(evar),
                        right: expected_arrow,
                    }));
                    remaining = ErrandType::ETVar(result_evar);
                }
                other => return Err(ErrandTypeError::NotAFunction(other)),
            }
        }

        let ty = self.pool.intern(remaining);
        Ok(ty)
    }

    /// Type a `match` expression.
    ///
    /// For each arm that has binding variables, temporarily registers the binding
    /// names with the corresponding field types from `enum_variants` so that
    /// `VarRef` instructions in the arm body can resolve correctly.
    fn analyze_match(&mut self, data: &MatchData) -> TypeResult<TypeIndex> {
        let scrut_idx = self.analyze_instr(data.scrutinee)?;
        let scrut_ty = self.expanded_pool_type(scrut_idx);
        let enum_tparams = self
            .enum_type_params
            .get(&data.enum_name)
            .cloned()
            .unwrap_or_default();
        let mut scrut_subst: HashMap<String, ErrandType> = HashMap::new();
        if let ErrandType::App(head, args) = &scrut_ty {
            if let ErrandType::Con(en) = head.as_ref() {
                if en == &data.enum_name && args.len() == enum_tparams.len() {
                    for (p, a) in enum_tparams.iter().zip(args.iter()) {
                        scrut_subst.insert(p.clone(), a.clone());
                    }
                }
            }
        }

        let variants = self.enum_variants.get(&data.enum_name).cloned().unwrap_or_default();

        let mut last_ty = self.pool.unit;
        for arm in &data.arms {
            // Register binding variables for this arm.
            if !arm.bindings.is_empty() {
                if let Some(tag) = arm.tag {
                    if let Some(variant_info) = variants.get(tag as usize) {
                        for (i, binding_name) in arm.bindings.iter().enumerate() {
                            if let Some((_, field_type)) = variant_info.fields.get(i) {
                                let mut ty = type_expr_to_errand_type_with_params(field_type, &enum_tparams);
                                ty = self.subst_vars_in_type(&ty, &scrut_subst);
                                let ty_idx = self.pool.intern(ty);
                                // Register the binding as a global def so VarRef can find it.
                                self.global_defs.insert(binding_name.clone(), ty_idx);
                            }
                        }
                    }
                }
            }
            let body_ty = self.analyze_instr(arm.body)?;
            last_ty = body_ty;
        }

        Ok(last_ty)
    }

    fn analyze_binop(&mut self, data: &BinOpPl) -> TypeResult<TypeIndex> {
        let left_ty = self.analyze_instr(data.left)?;
        let right_ty = self.analyze_instr(data.right)?;

        let (expected_left, expected_right, result) = self.binop_signature(&data.op);

        self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
            left: self.pool.to_errand_type(left_ty),
            right: self.pool.to_errand_type(expected_left),
        }));
        self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
            left: self.pool.to_errand_type(right_ty),
            right: self.pool.to_errand_type(expected_right),
        }));

        Ok(result)
    }

    fn analyze_unop(&mut self, data: &UnOpPl) -> TypeResult<TypeIndex> {
        let operand_ty = self.analyze_instr(data.operand)?;
        let (expected, result) = self.unop_signature(&data.op);

        self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
            left: self.pool.to_errand_type(operand_ty),
            right: self.pool.to_errand_type(expected),
        }));

        Ok(result)
    }

    fn analyze_call(&mut self, data: &FnCallPl) -> TypeResult<TypeIndex> {
        let func_ty_idx = self
            .global_defs
            .get(&data.name)
            .copied()
            .ok_or_else(|| ErrandTypeError::UnboundVariable(data.name.clone()))?;

        let mut arg_types = Vec::new();
        for &arg_idx in &data.arguments {
            arg_types.push(self.analyze_instr(arg_idx)?);
        }

        let mut remaining = self.instantiate_constructor_type(&self.pool.to_errand_type(func_ty_idx))?;
        for arg_ty in &arg_types {
            match remaining {
                ErrandType::Arrow(param_ty, result_ty) => {
                    self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                        left: self.pool.to_errand_type(*arg_ty),
                        right: *param_ty,
                    }));
                    remaining = *result_ty;
                }
                ErrandType::ETVar(ref evar_name) => {
                    let evar = evar_name.clone();
                    self.worklist.push(WorklistEntry::TVar(evar.clone(), TyVarKind::Existential));
                    let result_evar = self.worklist.fresh_evar();
                    self.worklist
                        .push(WorklistEntry::TVar(result_evar.clone(), TyVarKind::Existential));
                    let expected_arrow = ErrandType::Arrow(
                        Box::new(self.pool.to_errand_type(*arg_ty)),
                        Box::new(ErrandType::ETVar(result_evar.clone())),
                    );
                    self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                        left: ErrandType::ETVar(evar),
                        right: expected_arrow,
                    }));
                    remaining = ErrandType::ETVar(result_evar);
                }
                other => return Err(ErrandTypeError::NotAFunction(other)),
            }
        }

        let result_idx = self.pool.intern(remaining);
        Ok(result_idx)
    }

    fn analyze_if(&mut self, data: &IfStatementData) -> TypeResult<TypeIndex> {
        let cond_ty = self.analyze_instr(data.condition)?;
        self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
            left: self.pool.to_errand_type(cond_ty),
            right: ErrandType::Con("Bool".into()),
        }));

        let then_ty = self.analyze_instr(data.then_branch)?;

        if let Some(else_idx) = data.else_branch {
            let else_ty = self.analyze_instr(else_idx)?;
            self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                left: self.pool.to_errand_type(else_ty),
                right: self.pool.to_errand_type(then_ty),
            }));
        }

        Ok(then_ty)
    }

    fn analyze_while(&mut self, data: &WhileLoopData) -> TypeResult<TypeIndex> {
        let cond_ty = self.analyze_instr(data.condition)?;
        self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
            left: self.pool.to_errand_type(cond_ty),
            right: ErrandType::Con("Bool".into()),
        }));
        let _ = self.analyze_instr(data.body)?;
        Ok(self.pool.unit)
    }

    fn analyze_return(&mut self, data: &ReturnData) -> TypeResult<TypeIndex> {
        match data.value {
            Some(idx) => self.analyze_instr(idx),
            None => Ok(self.pool.unit),
        }
    }

    fn analyze_region_instr(&mut self, data: &RegionData) -> TypeResult<TypeIndex> {
        let mut last_ty = self.pool.unit;
        for i in data.instr_start..data.instr_end {
            if let Some(instr) = self.preir.get_instruction(i) {
                match instr {
                    Instr::FuncDecl(_) | Instr::StructDecl(_) | Instr::EnumDecl(_) => continue,
                    Instr::VarDecl(_) => {
                        self.analyze_instr(i)?;
                    }
                    _ => {
                        last_ty = self.analyze_instr(i)?;
                    }
                }
            }
        }
        self.solve()?;
        Ok(last_ty)
    }

    fn analyze_var_decl(&mut self, data: &VarDeclData) -> TypeResult<TypeIndex> {
        let value_ty = self.analyze_instr(data.value)?;
        // When the variable has an explicit type annotation, the annotation is
        // the source of truth for the binding's type. The init expression must
        // satisfy `value_ty <: expected`, but `varref name` should produce the
        // declared type — otherwise things like `typeof(b)` for `b::Point =
        // new(:Point, ...)` would resolve to the more general return type of
        // `new` (e.g. `_result`) instead of `Point`.
        let bound_ty = if let Some(ref decl_te) = data.declared_type {
            let expected = type_expr_to_errand_type(decl_te);
            // Drain everything pending so that existential variables in
            // the value type get bound by the constraints from the value
            // expression (e.g. `String <: ^α0` from `Pair("hello", ...)`).
            // We swallow errors here because pre-existing module-level
            // type errors that the codebase tolerates (e.g. enum-as-int
            // arithmetic in `next = d + 1`) shouldn't be surfaced through
            // a downstream var decl. Then push and solve our own
            // subtyping constraint, propagating any error from THAT.
            self.drain_silently();
            let value_et = self.worklist.expand_type(&self.pool.to_errand_type(value_ty));
            self.solve_subtype(value_et, expected.clone())?;
            // `solve_subtype` may push decomposed sub-judgments onto the
            // worklist (e.g. `App<App>` decomposes per-argument). Drain
            // those now so the error fires here, not silently elsewhere.
            self.solve()?;
            self.pool.intern(expected)
        } else {
            value_ty
        };
        self.var_context.insert(data.name.clone(), bound_ty);
        Ok(self.pool.unit)
    }

    fn analyze_func_decl(&mut self, data: &FuncData) -> TypeResult<TypeIndex> {
        let body_ty = self.analyze_instr(data.body_index)?;
        let func_ty = self.build_function_type(&data.parameters, body_ty);
        let func_ty_idx = self.pool.intern(func_ty.clone());

        if let Some(&placeholder_idx) = self.global_defs.get(&data.name) {
            let placeholder = self.pool.to_errand_type(placeholder_idx);
            if let ErrandType::ETVar(ref evar) = placeholder {
                self.worklist
                    .push(WorklistEntry::TVar(evar.clone(), TyVarKind::Existential));
            }
            self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                left: func_ty.clone(),
                right: placeholder.clone(),
            }));
            self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                left: placeholder,
                right: func_ty,
            }));
            self.solve()?;
        }

        // Update global_defs with the resolved type.
        self.global_defs.insert(data.name.clone(), func_ty_idx);
        Ok(self.pool.unit)
    }

    // ── Type signatures for operators ─────────────────────────────────────────

    /// Returns `(expected_left, expected_right, result)` as `TypeIndex` triples.
    fn binop_signature(&mut self, op: &BinaryOperator) -> (TypeIndex, TypeIndex, TypeIndex) {
        match op {
            BinaryOperator::Add
            | BinaryOperator::Subtract
            | BinaryOperator::Multiply
            | BinaryOperator::Divide
            | BinaryOperator::Modulo
            | BinaryOperator::Power
            | BinaryOperator::Ampersand
            | BinaryOperator::Pipe => (self.pool.int, self.pool.int, self.pool.int),

            BinaryOperator::LessThan
            | BinaryOperator::LessThanEqual
            | BinaryOperator::GreaterThan
            | BinaryOperator::GreaterThanEqual
            | BinaryOperator::Equal
            | BinaryOperator::NotEqual => (self.pool.int, self.pool.int, self.pool.bool_),

            BinaryOperator::And | BinaryOperator::Or => {
                (self.pool.bool_, self.pool.bool_, self.pool.bool_)
            }

            BinaryOperator::Dot => {
                let struct_ty = self.pool.intern(ErrandType::Con("Struct".into()));
                (struct_ty, self.pool.string, self.pool.int)
            }

            BinaryOperator::Assignment => (self.pool.int, self.pool.int, self.pool.unit),
        }
    }

    /// Returns `(expected_operand, result)` as `TypeIndex` pairs.
    fn unop_signature(&mut self, op: &UnaryOperator) -> (TypeIndex, TypeIndex) {
        match op {
            UnaryOperator::Negate => (self.pool.int, self.pool.int),
            UnaryOperator::Not => (self.pool.bool_, self.pool.bool_),
        }
    }

    // ── Function type construction ────────────────────────────────────────────

    fn build_function_type(&mut self, params: &[Parameter], return_ty: TypeIndex) -> ErrandType {
        params.iter().rev().fold(self.pool.to_errand_type(return_ty), |acc, param| {
            let param_ty = match &param.type_expr {
                Some(te) => type_expr_to_errand_type(te),
                None => ErrandType::ETVar(format!("param_{}", param.id.name)),
            };
            ErrandType::Arrow(Box::new(param_ty), Box::new(acc))
        })
    }

    // ── Constraint solving ────────────────────────────────────────────────────

    /// Drain all pending judgments from the worklist.  Public so that
    /// callers (e.g. `SirGen` after walking the main region) can ensure
    /// module-level constraints are checked, since the per-`Region`
    /// `solve()` only fires when an `Instr::Region` node is analyzed.
    pub fn solve_pending(&mut self) -> TypeResult<()> {
        self.solve()
    }

    fn solve(&mut self) -> TypeResult<()> {
        while let Some(entry) = self.worklist.pop() {
            match entry {
                WorklistEntry::TVar(_, _) | WorklistEntry::Var(_, _) => continue,
                WorklistEntry::Judgment(j) => self.solve_judgment(j)?,
            }
        }
        Ok(())
    }

    /// Drain every pending entry on the worklist, silently discarding any
    /// errors. Used by `analyze_var_decl` to ensure existential bindings
    /// from prior expression analysis are applied before the new
    /// declaration's subtyping check, without surfacing pre-existing
    /// module-level type errors that the codebase still tolerates (e.g.
    /// enum-as-int arithmetic).
    fn drain_silently(&mut self) {
        while let Some(entry) = self.worklist.pop() {
            match entry {
                WorklistEntry::TVar(_, _) | WorklistEntry::Var(_, _) => continue,
                WorklistEntry::Judgment(j) => {
                    let _ = self.solve_judgment(j);
                }
            }
        }
    }

    fn solve_judgment(&mut self, j: Judgment) -> TypeResult<()> {
        match j {
            Judgment::Sub { left, right } => self.solve_subtype(left, right),
            Judgment::Inf { instr, ty } => self.solve_inference(instr, ty),
            Judgment::Chk { instr, ty } => self.solve_checking(instr, ty),
            Judgment::InfApp {
                func_ty,
                arg_idx,
                result_ty,
            } => self.solve_inf_app(func_ty, arg_idx, result_ty),
        }
    }

    fn solve_inf_app(
        &mut self,
        func_ty: ErrandType,
        arg_idx: instr_index,
        result_ty: ErrandType,
    ) -> TypeResult<()> {
        let arg_instr = self
            .preir
            .get_instruction(arg_idx)
            .cloned()
            .ok_or_else(|| {
                ErrandTypeError::UnsupportedOperation(format!("InfApp: bad arg index {arg_idx}"))
            })?;
        match func_ty {
            ErrandType::Arrow(param_ty, ret_ty) => {
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: *ret_ty,
                    right: result_ty,
                }));
                self.worklist.push(WorklistEntry::Judgment(Judgment::Chk {
                    instr: arg_instr,
                    ty: *param_ty,
                }));
                Ok(())
            }
            ErrandType::Forall(v, body) => {
                let e = self.worklist.fresh_evar();
                self.worklist
                    .push(WorklistEntry::TVar(e.clone(), TyVarKind::Existential));
                let subbed = self.apply_substs_to_type(
                    &body,
                    &HashMap::from([(v, ErrandType::ETVar(e))]),
                );
                self.solve_inf_app(subbed, arg_idx, result_ty)
            }
            ErrandType::ETVar(a) => {
                let p = self.worklist.fresh_evar();
                let r = self.worklist.fresh_evar();
                self.worklist
                    .push(WorklistEntry::TVar(p.clone(), TyVarKind::Existential));
                self.worklist
                    .push(WorklistEntry::TVar(r.clone(), TyVarKind::Existential));
                let arr = ErrandType::Arrow(
                    Box::new(ErrandType::ETVar(p.clone())),
                    Box::new(ErrandType::ETVar(r.clone())),
                );
                self.worklist.solve_evar(&a, arr)?;
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: ErrandType::ETVar(r),
                    right: result_ty,
                }));
                self.worklist.push(WorklistEntry::Judgment(Judgment::Chk {
                    instr: arg_instr,
                    ty: ErrandType::ETVar(p),
                }));
                Ok(())
            }
            other => Err(ErrandTypeError::NotAFunction(other)),
        }
    }

    fn solve_subtype(&mut self, left: ErrandType, right: ErrandType) -> TypeResult<()> {
        // Substitute any already-solved existentials on both sides before
        // making subtyping decisions. Without this, a constraint like
        // `^α <: Int` for an existential that has already been solved to
        // `String` would silently no-op in `solve_evar` (TyVarKind::Solved
        // arm), letting bogus programs through. After expansion the same
        // constraint becomes `String <: Int`, which correctly fails.
        let left = self.worklist.expand_type(&left);
        let right = self.worklist.expand_type(&right);

        self.trace.push(format!("Sub {} <: {}", fmt_ty(&left), fmt_ty(&right)));

        if left == right {
            return Ok(());
        }

        match (&left, &right) {
            (ErrandType::Var(a), ErrandType::Var(b)) if a == b => Ok(()),
            (ErrandType::ETVar(a), ErrandType::ETVar(b)) if a == b => Ok(()),
            (ErrandType::Con(a), ErrandType::Con(b)) if a == b => Ok(()),
            // `Var(_)` is used by builtins (e.g. `getfield`'s `_struct`,
            // `new`'s `_a`/`_b`/`_result`) as an untyped placeholder. Treat
            // it as both top and bottom so any concrete type satisfies it
            // and it satisfies any concrete type. This is a temporary hack
            // until builtin signatures are properly polymorphic (`Forall`).
            (_, ErrandType::Var(_)) => Ok(()),
            (ErrandType::Var(_), _) => Ok(()),
            (ErrandType::App(f1, args1), ErrandType::App(f2, args2)) => {
                if args1.len() != args2.len() {
                    return Err(ErrandTypeError::ArityMismatch {
                        expected: args2.len(),
                        actual: args1.len(),
                    });
                }
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: *f1.clone(),
                    right: *f2.clone(),
                }));
                for (l, r) in args1.iter().zip(args2.iter()) {
                    self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                        left: l.clone(),
                        right: r.clone(),
                    }));
                }
                Ok(())
            }
            (_, ErrandType::Forall(v, body)) => {
                let fresh = self.worklist.fresh_var();
                self.worklist.push(WorklistEntry::TVar(
                    fresh.clone(),
                    TyVarKind::Universal,
                ));
                let subbed = self.apply_substs_to_type(
                    body,
                    &HashMap::from([(v.clone(), ErrandType::Var(fresh))]),
                );
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: left.clone(),
                    right: subbed,
                }));
                Ok(())
            }
            (ErrandType::Forall(v, body), _) => {
                let fresh_e = self.worklist.fresh_evar();
                self.worklist.push(WorklistEntry::TVar(
                    fresh_e.clone(),
                    TyVarKind::Marker,
                ));
                self.worklist.push(WorklistEntry::TVar(
                    fresh_e.clone(),
                    TyVarKind::Existential,
                ));
                let subbed = self.apply_substs_to_type(
                    body,
                    &HashMap::from([(v.clone(), ErrandType::ETVar(fresh_e))]),
                );
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: subbed,
                    right: right.clone(),
                }));
                Ok(())
            }
            (ErrandType::Arrow(a1, a2), ErrandType::Arrow(b1, b2)) => {
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: *b1.clone(),
                    right: *a1.clone(),
                }));
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: *a2.clone(),
                    right: *b2.clone(),
                }));
                Ok(())
            }
            (ErrandType::ETVar(a), _) if !self.occurs_check(a, &right) => {
                let a = a.clone();
                self.instantiate_left(&a, &right.clone())
            }
            (_, ErrandType::ETVar(a)) if !self.occurs_check(a, &left) => {
                let a = a.clone();
                self.instantiate_right(&left.clone(), &a)
            }
            _ => Err(ErrandTypeError::SubtypingError { left, right }),
        }
    }

    fn solve_inference(&mut self, instr: Instr, ty: ErrandType) -> TypeResult<()> {
        self.trace.push(format!("Inf {} ⊢ {}", fmt_instr(&instr), fmt_ty(&ty)));
        match instr {
            Instr::Literal(lit) => {
                let lit_ty = self.infer_literal(&lit);
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: self.pool.to_errand_type(lit_ty),
                    right: ty,
                }));
                Ok(())
            }
            Instr::VarRef(data) => {
                if let Some(&var_ty) = self.var_context.get(&data.name) {
                    self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                        left: self.pool.to_errand_type(var_ty),
                        right: ty,
                    }));
                    return Ok(());
                }
                if let Some(&var_ty) = self.global_defs.get(&data.name) {
                    self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                        left: self.pool.to_errand_type(var_ty),
                        right: ty,
                    }));
                    return Ok(());
                }
                Err(ErrandTypeError::UnboundVariable(data.name))
            }
            Instr::BinOp(data) => {
                let (_, _, result) = self.binop_signature(&data.op);
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: self.pool.to_errand_type(result),
                    right: ty,
                }));
                Ok(())
            }
            Instr::UnOp(data) => {
                let (_, result) = self.unop_signature(&data.op);
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: self.pool.to_errand_type(result),
                    right: ty,
                }));
                Ok(())
            }
            Instr::WhileLoop(_) | Instr::VarDecl(_) | Instr::FuncDecl(_) | Instr::StructDecl(_) => {
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: self.pool.to_errand_type(self.pool.unit),
                    right: ty,
                }));
                Ok(())
            }
            Instr::FnCall(data) => {
                if let Some(&func_ty) = self.global_defs.get(&data.name) {
                    self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                        left: self.pool.to_errand_type(func_ty),
                        right: ty,
                    }));
                    Ok(())
                } else {
                    Err(ErrandTypeError::UnboundVariable(data.name))
                }
            }
            Instr::Return(data) => {
                if data.value.is_none() {
                    self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                        left: self.pool.to_errand_type(self.pool.unit),
                        right: ty,
                    }));
                }
                Ok(())
            }
            Instr::IfStatement(_) | Instr::Region(_) | Instr::ForLoop(_)
            | Instr::EnumDecl(_) | Instr::EnumVariantAccess(_)
            | Instr::EnumVariantConstruct(_) | Instr::Match(_) => Ok(()),
            Instr::Typeof(_) => {
                // `typeof` is always typed `String`.
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: self.pool.to_errand_type(self.pool.string),
                    right: ty,
                }));
                Ok(())
            }
        }
    }

    fn solve_checking(&mut self, instr: Instr, ty: ErrandType) -> TypeResult<()> {
        self.trace.push(format!("Chk {} ⇐ {}", fmt_instr(&instr), fmt_ty(&ty)));
        if let ErrandType::Forall(var, body_ty) = &ty {
            let fresh = self.worklist.fresh_var();
            self.worklist.push(WorklistEntry::TVar(
                fresh.clone(),
                TyVarKind::Universal,
            ));
            let substituted = self.apply_substs_to_type(
                body_ty,
                &HashMap::from([(var.clone(), ErrandType::Var(fresh))]),
            );
            return self.solve_checking(instr, substituted);
        }
        let inferred_evar = self.worklist.fresh_evar();
        self.worklist.push(WorklistEntry::TVar(inferred_evar.clone(), TyVarKind::Existential));
        let inferred_ty = ErrandType::ETVar(inferred_evar);
        self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
            left: inferred_ty.clone(),
            right: ty,
        }));
        self.worklist.push(WorklistEntry::Judgment(Judgment::Inf {
            instr,
            ty: inferred_ty,
        }));
        Ok(())
    }

    // ── Instantiation ─────────────────────────────────────────────────────────

    fn instantiate_left(&mut self, var: &str, ty: &ErrandType) -> TypeResult<()> {
        match ty {
            ErrandType::ETVar(b) if self.worklist.before(var, b) => {
                self.worklist.solve_evar(b, ErrandType::ETVar(var.to_string()))?;
                Ok(())
            }
            ErrandType::Arrow(t1, t2) => {
                let a1 = self.worklist.fresh_evar();
                let a2 = self.worklist.fresh_evar();
                self.worklist.solve_evar(
                    var,
                    ErrandType::Arrow(
                        Box::new(ErrandType::ETVar(a1.clone())),
                        Box::new(ErrandType::ETVar(a2.clone())),
                    ),
                )?;
                self.worklist.push(WorklistEntry::TVar(a1.clone(), TyVarKind::Existential));
                self.worklist.push(WorklistEntry::TVar(a2.clone(), TyVarKind::Existential));
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: *t1.clone(),
                    right: ErrandType::ETVar(a1),
                }));
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: ErrandType::ETVar(a2),
                    right: *t2.clone(),
                }));
                Ok(())
            }
            ErrandType::App(f, args) => {
                let hf = self.worklist.fresh_evar();
                let mut arg_evars = Vec::new();
                let mut app_args = Vec::new();
                for _ in args {
                    let e = self.worklist.fresh_evar();
                    arg_evars.push(e.clone());
                    app_args.push(ErrandType::ETVar(e));
                }
                let app_ty = ErrandType::App(Box::new(ErrandType::ETVar(hf.clone())), app_args);
                self.worklist.solve_evar(var, app_ty)?;
                self.worklist
                    .push(WorklistEntry::TVar(hf.clone(), TyVarKind::Existential));
                for e in &arg_evars {
                    self.worklist
                        .push(WorklistEntry::TVar(e.clone(), TyVarKind::Existential));
                }
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: ErrandType::ETVar(hf),
                    right: *f.clone(),
                }));
                for (e, arg_t) in arg_evars.iter().zip(args.iter()) {
                    self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                        left: ErrandType::ETVar(e.clone()),
                        right: arg_t.clone(),
                    }));
                }
                Ok(())
            }
            ErrandType::Forall(bound, inner) => {
                let fresh = self.worklist.fresh_var();
                self.worklist.push(WorklistEntry::TVar(
                    fresh.clone(),
                    TyVarKind::Universal,
                ));
                let substituted = self.apply_substs_to_type(
                    inner,
                    &HashMap::from([(bound.clone(), ErrandType::Var(fresh))]),
                );
                self.instantiate_left(var, &substituted)
            }
            _ if self.is_monotype(ty) => {
                self.worklist.solve_evar(var, ty.clone())?;
                Ok(())
            }
            _ => Err(ErrandTypeError::UnsupportedOperation(format!(
                "instantiate_left: {}",
                fmt_ty(ty)
            ))),
        }
    }

    fn instantiate_right(&mut self, ty: &ErrandType, var: &str) -> TypeResult<()> {
        match ty {
            ErrandType::ETVar(a) if self.worklist.before(var, a) => {
                self.worklist.solve_evar(a, ErrandType::ETVar(var.to_string()))?;
                Ok(())
            }
            ErrandType::Arrow(t1, t2) => {
                let a1 = self.worklist.fresh_evar();
                let a2 = self.worklist.fresh_evar();
                self.worklist.solve_evar(
                    var,
                    ErrandType::Arrow(
                        Box::new(ErrandType::ETVar(a1.clone())),
                        Box::new(ErrandType::ETVar(a2.clone())),
                    ),
                )?;
                self.worklist.push(WorklistEntry::TVar(a1.clone(), TyVarKind::Existential));
                self.worklist.push(WorklistEntry::TVar(a2.clone(), TyVarKind::Existential));
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: ErrandType::ETVar(a1),
                    right: *t1.clone(),
                }));
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: *t2.clone(),
                    right: ErrandType::ETVar(a2),
                }));
                Ok(())
            }
            ErrandType::App(f, args) => {
                let hf = self.worklist.fresh_evar();
                let mut arg_evars = Vec::new();
                let mut app_args = Vec::new();
                for _ in args {
                    let e = self.worklist.fresh_evar();
                    arg_evars.push(e.clone());
                    app_args.push(ErrandType::ETVar(e));
                }
                let app_ty = ErrandType::App(Box::new(ErrandType::ETVar(hf.clone())), app_args);
                self.worklist.solve_evar(var, app_ty)?;
                self.worklist
                    .push(WorklistEntry::TVar(hf.clone(), TyVarKind::Existential));
                for e in &arg_evars {
                    self.worklist
                        .push(WorklistEntry::TVar(e.clone(), TyVarKind::Existential));
                }
                self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                    left: *f.clone(),
                    right: ErrandType::ETVar(hf),
                }));
                for (e, arg_t) in arg_evars.iter().zip(args.iter()) {
                    self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
                        left: arg_t.clone(),
                        right: ErrandType::ETVar(e.clone()),
                    }));
                }
                Ok(())
            }
            ErrandType::Forall(bound, inner) => {
                let fresh_e = self.worklist.fresh_evar();
                self.worklist.push(WorklistEntry::TVar(
                    fresh_e.clone(),
                    TyVarKind::Marker,
                ));
                self.worklist.push(WorklistEntry::TVar(
                    fresh_e.clone(),
                    TyVarKind::Existential,
                ));
                let substituted = self.apply_substs_to_type(
                    inner,
                    &HashMap::from([(bound.clone(), ErrandType::ETVar(fresh_e))]),
                );
                self.instantiate_right(&substituted, var)
            }
            _ if self.is_monotype(ty) => {
                self.worklist.solve_evar(var, ty.clone())?;
                Ok(())
            }
            _ => Err(ErrandTypeError::UnsupportedOperation(format!(
                "instantiate_right: {}",
                fmt_ty(ty)
            ))),
        }
    }

    // ── Utilities ─────────────────────────────────────────────────────────────

    fn occurs_check(&self, var: &str, ty: &ErrandType) -> bool {
        match ty {
            ErrandType::ETVar(n) | ErrandType::Var(n) => n == var,
            ErrandType::Con(_) => false,
            ErrandType::Arrow(t1, t2) => self.occurs_check(var, t1) || self.occurs_check(var, t2),
            ErrandType::Forall(_, t) => self.occurs_check(var, t),
            ErrandType::Product(ts) => {
                ts.iter().any(|t| self.occurs_check(var, t))
            }
            ErrandType::App(h, args) => {
                self.occurs_check(var, h) || args.iter().any(|t| self.occurs_check(var, t))
            }
        }
    }

    fn is_monotype(&self, ty: &ErrandType) -> bool {
        match ty {
            ErrandType::Var(_) | ErrandType::ETVar(_) | ErrandType::Con(_) => true,
            ErrandType::Arrow(t1, t2) => self.is_monotype(t1) && self.is_monotype(t2),
            ErrandType::Product(ts) => {
                ts.iter().all(|t| self.is_monotype(t))
            }
            ErrandType::Forall(_, _) => false,
            ErrandType::App(h, args) => self.is_monotype(h) && args.iter().all(|t| self.is_monotype(t)),
        }
    }

    /// Peel top-level `∀` into fresh existentials (DK instantiation for constructors).
    fn instantiate_constructor_type(&mut self, ctor_ty: &ErrandType) -> TypeResult<ErrandType> {
        let mut subs: HashMap<String, ErrandType> = HashMap::new();
        let mut cur = ctor_ty.clone();
        while let ErrandType::Forall(v, body) = cur {
            let e = self.worklist.fresh_evar();
            self.worklist
                .push(WorklistEntry::TVar(e.clone(), TyVarKind::Existential));
            subs.insert(v, ErrandType::ETVar(e));
            cur = *body;
        }
        Ok(self.apply_substs_to_type(&cur, &subs))
    }

    pub(crate) fn apply_substs_to_type(&self, ty: &ErrandType, subs: &HashMap<String, ErrandType>) -> ErrandType {
        match ty {
            ErrandType::Var(name) => subs.get(name).cloned().unwrap_or_else(|| ty.clone()),
            ErrandType::Arrow(a, b) => ErrandType::Arrow(
                Box::new(self.apply_substs_to_type(a, subs)),
                Box::new(self.apply_substs_to_type(b, subs)),
            ),
            ErrandType::App(h, args) => ErrandType::App(
                Box::new(self.apply_substs_to_type(h, subs)),
                args.iter()
                    .map(|t| self.apply_substs_to_type(t, subs))
                    .collect(),
            ),
            ErrandType::Forall(v, b) => {
                let mut inner_subs = subs.clone();
                inner_subs.remove(v);
                ErrandType::Forall(
                    v.clone(),
                    Box::new(self.apply_substs_to_type(b, &inner_subs)),
                )
            }
            ErrandType::Product(ts) => ErrandType::Product(
                ts.iter()
                    .map(|t| self.apply_substs_to_type(t, subs))
                    .collect(),
            ),
            ErrandType::ETVar(_) | ErrandType::Con(_) => ty.clone(),
        }
    }

    fn subst_vars_in_type(&self, ty: &ErrandType, map: &HashMap<String, ErrandType>) -> ErrandType {
        self.apply_substs_to_type(ty, map)
    }

    pub fn get_trace(&self) -> &[String] {
        &self.trace
    }

    // ── Global definition collection (one-time pre-pass) ──────────────────────

    /// Pre-populate the `global_defs` table with builtins, function placeholders,
    /// and struct constructor arrow types — before any per-instruction analysis.
    fn collect_global_defs(
        preir: &PreIR,
        pool: &mut TypePool,
        worklist: &mut Worklist,
    ) -> HashMap<String, TypeIndex> {
        let mut defs: HashMap<String, TypeIndex> = HashMap::new();

        // Builtins: data constructors + functions
        for (name, ty) in add_builtin_data_constructors().into_iter().chain(add_builtin_functions()) {
            let idx = pool.intern(ty);
            defs.insert(name, idx);
        }

        // User-defined functions: existential placeholder for mutual recursion.
        for instr in &preir.instructions {
            if let Instr::FuncDecl(FuncData { name, .. }) = instr {
                if defs.contains_key(name) {
                    continue;
                }
                let evar = worklist.fresh_evar();
                let idx = pool.intern(ErrandType::ETVar(evar));
                defs.insert(name.clone(), idx);
            }
        }

        // Enum types: register each enum name as a Con type.
        // Variants are compiled away to integer literals during lowering,
        // so we only need the type name here for parameter type annotations.
        for instr in &preir.instructions {
            if let Instr::EnumDecl(EnumData { name, .. }) = instr {
                let ty = ErrandType::Con(name.clone());
                let idx = pool.intern(ty);
                defs.insert(name.clone(), idx);
            }
        }

        // Struct constructors: field1 -> field2 -> ... -> App(Con(Name), [Var T…])
        for instr in &preir.instructions {
            if let Instr::StructDecl(StructData {
                name,
                type_params,
                fields,
            }) = instr
            {
                let ret_vars: Vec<ErrandType> = type_params
                    .iter()
                    .map(|p| ErrandType::Var(p.name.clone()))
                    .collect();
                let mut ctor_ty = fields.iter().rev().fold(
                    ErrandType::App(Box::new(ErrandType::Con(name.clone())), ret_vars),
                    |acc, field| {
                        ErrandType::Arrow(
                            Box::new(type_expr_to_errand_type_with_params(
                                &field.field_type,
                                &type_params.iter().map(|p| p.name.clone()).collect::<Vec<String>>(),
                            )),
                            Box::new(acc),
                        )
                    },
                );
                for tp in type_params.iter().rev() {
                    ctor_ty = ErrandType::Forall(tp.name.clone(), Box::new(ctor_ty));
                }
                let idx = pool.intern(ctor_ty);
                defs.insert(name.clone(), idx);
            }
        }

        defs
    }


}

// ─── Formatting helpers (private) ─────────────────────────────────────────────

fn fmt_ty(ty: &ErrandType) -> String {
    match ty {
        ErrandType::Var(n) | ErrandType::Con(n) | ErrandType::ETVar(n) => n.clone(),
        ErrandType::Arrow(a, b) => format!("{} -> {}", fmt_ty(a), fmt_ty(b)),
        ErrandType::Forall(v, t) => format!("∀{}. {}", v, fmt_ty(t)),
        ErrandType::Product(ts) => ts.iter().map(fmt_ty).collect::<Vec<_>>().join(" × "),
        ErrandType::App(h, args) => {
            let inner = args.iter().map(fmt_ty).collect::<Vec<_>>().join(", ");
            format!("{}<{}>", fmt_ty(h), inner)
        }
    }
}

fn fmt_instr(instr: &Instr) -> &'static str {
    match instr {
        Instr::Literal(_) => "literal",
        Instr::VarRef(_) => "varref",
        Instr::BinOp(_) => "binop",
        Instr::UnOp(_) => "unop",
        Instr::FnCall(_) => "call",
        Instr::IfStatement(_) => "if",
        Instr::WhileLoop(_) => "while",
        Instr::ForLoop(_) => "for",
        Instr::Return(_) => "return",
        Instr::Region(_) => "region",
        Instr::VarDecl(_) => "var_decl",
        Instr::FuncDecl(_) => "func_decl",
        Instr::StructDecl(_) => "struct_decl",
        Instr::EnumDecl(_) => "enum_decl",
        Instr::EnumVariantAccess(_) => "enum_variant",
        Instr::EnumVariantConstruct(_) => "enum_construct",
        Instr::Match(_) => "match",
        Instr::Typeof(_) => "typeof",
    }
}
