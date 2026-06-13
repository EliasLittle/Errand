use std::collections::HashMap;

use crate::backend::errand_builtins::{
    add_builtin_data_constructors, add_builtin_functions, type_expr_to_errand_type,
    type_expr_to_errand_type_with_params,
};
use crate::backend::preir::{
    instr_index, BinOpPl, EnumData, EnumVariantConstructData, EnumVariantData, EnumVariantInfo,
    FnCallPl, FuncData, IfStatementData, Instr, LiteralPl, MatchData, PreIR, RegionData,
    ReturnData, StructData, UnOpPl, VarDeclData, VarRefData, WhileLoopData,
};
use crate::backend::worklist::{
    ErrandType, ErrandTypeError, Judgment, TyVarKind, TypeResult, Worklist, WorklistEntry,
};
use crate::frontend::ast::{BinaryOperator, Parameter, Program, TypeExpression, UnaryOperator};
use tracing::instrument;

mod solve;

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
    #[instrument(name = "analysis.type_pool.new", target = "analysis", level = "trace")]
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

        TypePool {
            types,
            int,
            float,
            bool_,
            string,
            unit,
        }
    }

    /// Store a type in the pool and return its stable handle.
    #[instrument(
        skip(self, ty),
        name = "analysis.type_pool.intern",
        target = "analysis",
        level = "trace"
    )]
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
    #[instrument(
        skip(self, ty),
        name = "analysis.type_pool.intern_errand_type",
        target = "analysis",
        level = "trace"
    )]
    pub fn intern_errand_type(&mut self, ty: ErrandType) -> TypeIndex {
        self.intern(ty)
    }
}

// ─── Analyzer ─────────────────────────────────────────────────────────────────

/// Metadata gathered in a single pre-pass over the `PreIR` before analysis.
struct PreIRMetadata {
    global_defs: HashMap<String, TypeIndex>,
    enum_variants: HashMap<String, Vec<EnumVariantInfo>>,
    enum_type_params: HashMap<String, Vec<String>>,
}

/// Demand-driven semantic analysis over `PreIR`, analogous to Zig's `Sema`.
///
/// The `Analyzer` owns the `PreIR`, a `TypePool` for interned types, per-function
/// and module variable contexts, and the constraint-solving `Worklist`.
/// Results are cached by instruction index so each instruction is typed at most once.
///
/// The constraint solver itself (worklist driving, subtyping, instantiation) lives
/// in the [`solve`] submodule.
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
    // ── Public API ─────────────────────────────────────────────────────────────

    /// Build an `Analyzer` from a compiled `PreIR` and the source `Program`.
    ///
    /// Performs the global definition-collection pass (builtins + function
    /// placeholders + struct constructors) before returning.
    #[instrument(
        skip(preir, _program),
        fields(preir_len = preir.instructions.len()),
        name = "analysis.new",
        target = "analysis",
        level = "debug"
    )]
    pub fn new(preir: PreIR, _program: &Program) -> Self {
        let mut pool = TypePool::new();
        let mut worklist = Worklist::new();
        let PreIRMetadata {
            global_defs,
            enum_variants,
            enum_type_params,
        } = Self::scan_preir_metadata(&preir, &mut pool, &mut worklist);

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

    /// Type an instruction by index.  Results are memoized.
    #[instrument(
        skip(self),
        fields(idx),
        name = "analysis.analyze_instr",
        target = "analysis",
        level = "trace"
    )]
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

    /// Look up the cached `TypeIndex` for an already-analyzed instruction.
    #[instrument(
        skip(self),
        fields(idx),
        name = "analysis.cached_type",
        target = "analysis",
        level = "trace"
    )]
    pub fn cached_type(&self, idx: instr_index) -> Option<TypeIndex> {
        self.analysis_cache.get(&idx).copied()
    }

    /// Analyze every instruction in a region in forward order.
    ///
    /// `VarDecl` instructions extend `var_context` as a side-effect so that
    /// later instructions in the same region can resolve the declared names.
    /// Returns the type of the region's return location.
    #[instrument(
        skip(self, region),
        fields(
            instr_start = region.instr_start,
            instr_end = region.instr_end,
            return_loc = region.return_loc
        ),
        name = "analysis.analyze_body",
        target = "analysis",
        level = "debug"
    )]
    pub fn analyze_body(&mut self, region: &RegionData) -> TypeResult<TypeIndex> {
        self.analyze_region_range(region.instr_start, region.instr_end)
    }

    /// Populate `var_context` from function parameters, clearing any previous
    /// per-function state.  Module-level context is not touched.
    #[instrument(
        skip(self, parameters),
        fields(param_count = parameters.len()),
        name = "analysis.setup_function_context",
        target = "analysis",
        level = "debug"
    )]
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
    #[instrument(
        skip(self),
        name = "analysis.promote_to_module",
        target = "analysis",
        level = "debug"
    )]
    pub fn promote_to_module(&mut self) {
        self.module_context.extend(self.var_context.drain());
    }

    /// Drain all pending judgments from the worklist.  Public so that
    /// callers (e.g. `SirGen` after walking the main region) can ensure
    /// module-level constraints are checked, since the per-`Region`
    /// `solve()` only fires when an `Instr::Region` node is analyzed.
    #[instrument(
        skip(self),
        name = "analysis.solve_pending",
        target = "analysis",
        level = "debug"
    )]
    pub fn solve_pending(&mut self) -> TypeResult<()> {
        self.solve()
    }

    /// Expand existential solutions; use when reading types after `solve()`.
    #[instrument(
        skip(self, ty),
        name = "analysis.expand_type",
        target = "analysis",
        level = "trace"
    )]
    pub fn expand_type(&self, ty: &ErrandType) -> ErrandType {
        self.worklist.expand_type(ty)
    }

    #[instrument(
        skip(self),
        fields(idx = idx.0),
        name = "analysis.expanded_pool_type",
        target = "analysis",
        level = "trace"
    )]
    pub fn expanded_pool_type(&self, idx: TypeIndex) -> ErrandType {
        self.expand_type(&self.pool.to_errand_type(idx))
    }

    /// Turn a fully-instantiated `App(Con(Foo), …)` into `Con(Foo__…)` for codegen / mangling.
    #[instrument(
        skip(self, ty),
        name = "analysis.collapse_apps_in_type",
        target = "analysis",
        level = "trace"
    )]
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

    pub(crate) fn apply_substs_to_type(
        &self,
        ty: &ErrandType,
        subs: &HashMap<String, ErrandType>,
    ) -> ErrandType {
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

    #[instrument(
        skip(self),
        name = "analysis.get_trace",
        target = "analysis",
        level = "trace"
    )]
    pub fn get_trace(&self) -> &[String] {
        &self.trace
    }

    // ── PreIR metadata scan (one-time pre-pass) ───────────────────────────────

    /// Single forward pass over the `PreIR` that pre-populates the `global_defs`
    /// table (builtins + function placeholders + struct constructors + enum
    /// names) together with the enum variant / type-parameter tables, before any
    /// per-instruction analysis.
    ///
    /// Builtins are registered first so user function placeholders never shadow
    /// them.  Struct and enum declarations are emitted before their synthetic
    /// constructor functions (see `preir_gen`), so a struct's constructor arrow
    /// type wins over the function placeholder of the same name.
    #[instrument(
        skip(preir, pool, worklist),
        fields(preir_len = preir.instructions.len()),
        name = "analysis.scan_preir_metadata",
        target = "analysis",
        level = "debug"
    )]
    fn scan_preir_metadata(
        preir: &PreIR,
        pool: &mut TypePool,
        worklist: &mut Worklist,
    ) -> PreIRMetadata {
        let mut global_defs: HashMap<String, TypeIndex> = HashMap::new();
        let mut enum_variants: HashMap<String, Vec<EnumVariantInfo>> = HashMap::new();
        let mut enum_type_params: HashMap<String, Vec<String>> = HashMap::new();

        Self::register_builtin_defs(&mut global_defs, pool);

        for instr in &preir.instructions {
            match instr {
                Instr::FuncDecl(FuncData { name, .. }) => {
                    Self::register_function_placeholder(name, &mut global_defs, pool, worklist);
                }
                Instr::EnumDecl(ed) => {
                    Self::register_enum_def(
                        ed,
                        &mut global_defs,
                        &mut enum_variants,
                        &mut enum_type_params,
                        pool,
                    );
                }
                Instr::StructDecl(sd) => {
                    Self::register_struct_constructor(sd, &mut global_defs, pool);
                }
                _ => {}
            }
        }

        PreIRMetadata {
            global_defs,
            enum_variants,
            enum_type_params,
        }
    }

    /// Builtin data constructors and functions, e.g. `true`, `printf`, `malloc`.
    fn register_builtin_defs(defs: &mut HashMap<String, TypeIndex>, pool: &mut TypePool) {
        for (name, ty) in add_builtin_data_constructors()
            .into_iter()
            .chain(add_builtin_functions())
        {
            let idx = pool.intern(ty);
            defs.insert(name, idx);
        }
    }

    /// Existential placeholder for a user function, enabling mutual recursion.
    /// Skips names already defined (builtins, earlier overloads, struct/enum decls).
    fn register_function_placeholder(
        name: &str,
        defs: &mut HashMap<String, TypeIndex>,
        pool: &mut TypePool,
        worklist: &mut Worklist,
    ) {
        if defs.contains_key(name) {
            return;
        }
        let evar = worklist.fresh_evar();
        let idx = pool.intern(ErrandType::ETVar(evar));
        defs.insert(name.to_string(), idx);
    }

    /// Record an enum's variant table and type parameters, and register the
    /// enum name as a `Con` type for parameter annotations.  Variants are
    /// compiled away to integer literals during lowering, so only the type name
    /// is needed in `global_defs`.
    fn register_enum_def(
        ed: &EnumData,
        defs: &mut HashMap<String, TypeIndex>,
        enum_variants: &mut HashMap<String, Vec<EnumVariantInfo>>,
        enum_type_params: &mut HashMap<String, Vec<String>>,
        pool: &mut TypePool,
    ) {
        enum_type_params.insert(
            ed.name.clone(),
            ed.type_params.iter().map(|p| p.name.clone()).collect(),
        );
        enum_variants.insert(ed.name.clone(), ed.variants.clone());
        let idx = pool.intern(ErrandType::Con(ed.name.clone()));
        defs.insert(ed.name.clone(), idx);
    }

    /// Register a struct's constructor type:
    /// `∀T…. field1 -> field2 -> … -> App(Con(Name), [Var T…])`.
    fn register_struct_constructor(
        sd: &StructData,
        defs: &mut HashMap<String, TypeIndex>,
        pool: &mut TypePool,
    ) {
        let type_params: Vec<String> = sd.type_params.iter().map(|p| p.name.clone()).collect();
        let ret = Self::con_app_type(&sd.name, &type_params);
        let ctor_ty = Self::constructor_arrow_type(
            sd.fields.iter().map(|f| &f.field_type),
            ret,
            &type_params,
        );
        let ctor_ty = Self::wrap_foralls(&type_params, ctor_ty);
        let idx = pool.intern(ctor_ty);
        defs.insert(sd.name.clone(), idx);
    }

    // ── Core dispatch ─────────────────────────────────────────────────────────

    #[instrument(
        skip(self, instr),
        fields(instr_kind = fmt_instr(instr)),
        name = "analysis.analyze_instr_inner",
        target = "analysis",
        level = "trace"
    )]
    fn analyze_instr_inner(&mut self, instr: &Instr) -> TypeResult<TypeIndex> {
        match instr {
            Instr::Literal(lit) => self.infer_literal(lit),
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
            Instr::Typeof(operand) => self.analyze_typeof(*operand),
            Instr::ForLoop(_) => Err(ErrandTypeError::UnsupportedOperation("ForLoop".into())),
        }
    }

    // ── Per-instruction analysis ──────────────────────────────────────────────

    #[instrument(
        skip(self, lit),
        name = "analysis.infer_literal",
        target = "analysis",
        level = "trace"
    )]
    fn infer_literal(&self, lit: &LiteralPl) -> TypeResult<TypeIndex> {
        let ty = match lit {
            LiteralPl::Int(_) => self.pool.int,
            LiteralPl::Float(_) => self.pool.float,
            LiteralPl::Boolean(_) => self.pool.bool_,
            LiteralPl::String(_) => self.pool.string,
            LiteralPl::Symbol(_) => self.pool.string,
            LiteralPl::Unit => self.pool.unit,
        };
        Ok(ty)
    }

    #[instrument(
        skip(self, data),
        fields(name = %data.name),
        name = "analysis.analyze_var_ref",
        target = "analysis",
        level = "trace"
    )]
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
    #[instrument(
        skip(self, data),
        fields(enum_name = %data.enum_name, variant = %data.variant),
        name = "analysis.analyze_enum_variant_access",
        target = "analysis",
        level = "trace"
    )]
    fn analyze_enum_variant_access(&mut self, data: &EnumVariantData) -> TypeResult<TypeIndex> {
        self.lookup_enum_variant(&data.enum_name, &data.variant)?;
        let type_params = self
            .enum_type_params
            .get(&data.enum_name)
            .cloned()
            .unwrap_or_default();
        let ctor_ty = Self::wrap_foralls(
            &type_params,
            Self::con_app_type(&data.enum_name, &type_params),
        );
        let instantiated = self.instantiate_constructor_type(&ctor_ty)?;
        let ty = self.pool.intern(instantiated);
        Ok(ty)
    }

    /// Type a data-carrying enum variant construction, e.g. `Message::Move(1, 2)`.
    ///
    /// Validates enum name, variant name, and argument count.
    /// Returns `Con(enum_name)` — the enum's own type.
    #[instrument(
        skip(self, data),
        fields(
            enum_name = %data.enum_name,
            variant = %data.variant,
            arg_count = data.arg_indices.len()
        ),
        name = "analysis.analyze_enum_variant_construct",
        target = "analysis",
        level = "trace"
    )]
    fn analyze_enum_variant_construct(
        &mut self,
        data: &EnumVariantConstructData,
    ) -> TypeResult<TypeIndex> {
        let variant_info = self.lookup_enum_variant(&data.enum_name, &data.variant)?;

        if data.arg_indices.len() != variant_info.fields.len() {
            return Err(ErrandTypeError::UnboundVariable(format!(
                "enum variant `{}::{}` expects {} argument(s), got {}",
                data.enum_name,
                data.variant,
                variant_info.fields.len(),
                data.arg_indices.len()
            )));
        }

        let type_params = self
            .enum_type_params
            .get(&data.enum_name)
            .cloned()
            .unwrap_or_default();

        let ret_app = Self::con_app_type(&data.enum_name, &type_params);
        let arrow = Self::constructor_arrow_type(
            variant_info.fields.iter().map(|(_, ft)| ft),
            ret_app,
            &type_params,
        );
        let ctor_ty = Self::wrap_foralls(&type_params, arrow);

        let mut arg_types = Vec::new();
        for &idx in &data.arg_indices {
            arg_types.push(self.analyze_instr(idx)?);
        }

        let instantiated = self.instantiate_constructor_type(&ctor_ty)?;
        let remaining = self.apply_typed_arguments(&arg_types, instantiated)?;
        let ty = self.pool.intern(remaining);
        Ok(ty)
    }

    /// Type a `match` expression.
    ///
    /// For each arm that has binding variables, temporarily registers the binding
    /// names with the corresponding field types from `enum_variants` so that
    /// `VarRef` instructions in the arm body can resolve correctly.
    #[instrument(
        skip(self, data),
        fields(enum_name = %data.enum_name, arm_count = data.arms.len()),
        name = "analysis.analyze_match",
        target = "analysis",
        level = "trace"
    )]
    fn analyze_match(&mut self, data: &MatchData) -> TypeResult<TypeIndex> {
        let scrut_idx = self.analyze_instr(data.scrutinee)?;
        let scrut_ty = self.expanded_pool_type(scrut_idx);
        let enum_tparams = self
            .enum_type_params
            .get(&data.enum_name)
            .cloned()
            .unwrap_or_default();
        let scrut_subst = Self::scrutinee_subst(&scrut_ty, &data.enum_name, &enum_tparams);

        let variants = self
            .enum_variants
            .get(&data.enum_name)
            .cloned()
            .unwrap_or_default();

        let mut last_ty = self.pool.unit;
        for arm in &data.arms {
            // Register binding variables for this arm.
            if !arm.bindings.is_empty() {
                if let Some(tag) = arm.tag {
                    if let Some(variant_info) = variants.get(tag as usize) {
                        for (i, binding_name) in arm.bindings.iter().enumerate() {
                            if let Some((_, field_type)) = variant_info.fields.get(i) {
                                let mut ty =
                                    type_expr_to_errand_type_with_params(field_type, &enum_tparams);
                                ty = self.apply_substs_to_type(&ty, &scrut_subst);
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

    #[instrument(
        skip(self, data),
        name = "analysis.analyze_binop",
        target = "analysis",
        level = "trace"
    )]
    fn analyze_binop(&mut self, data: &BinOpPl) -> TypeResult<TypeIndex> {
        let left_ty = self.analyze_instr(data.left)?;
        let right_ty = self.analyze_instr(data.right)?;

        let (expected_left, expected_right, result) = self.binop_signature(&data.op);

        self.push_sub(left_ty, expected_left);
        self.push_sub(right_ty, expected_right);

        Ok(result)
    }

    #[instrument(
        skip(self, data),
        name = "analysis.analyze_unop",
        target = "analysis",
        level = "trace"
    )]
    fn analyze_unop(&mut self, data: &UnOpPl) -> TypeResult<TypeIndex> {
        let operand_ty = self.analyze_instr(data.operand)?;
        let (expected, result) = self.unop_signature(&data.op);

        self.push_sub(operand_ty, expected);

        Ok(result)
    }

    #[instrument(
        skip(self, data),
        fields(name = %data.name, arg_count = data.arguments.len()),
        name = "analysis.analyze_call",
        target = "analysis",
        level = "trace"
    )]
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

        let instantiated =
            self.instantiate_constructor_type(&self.pool.to_errand_type(func_ty_idx))?;
        let remaining = self.apply_typed_arguments(&arg_types, instantiated)?;
        let result_idx = self.pool.intern(remaining);
        Ok(result_idx)
    }

    #[instrument(
        skip(self, data),
        name = "analysis.analyze_if",
        target = "analysis",
        level = "trace"
    )]
    fn analyze_if(&mut self, data: &IfStatementData) -> TypeResult<TypeIndex> {
        let cond_ty = self.analyze_instr(data.condition)?;
        self.require_bool(cond_ty);

        let then_ty = self.analyze_instr(data.then_branch)?;

        if let Some(else_idx) = data.else_branch {
            let else_ty = self.analyze_instr(else_idx)?;
            self.push_sub(else_ty, then_ty);
        }

        Ok(then_ty)
    }

    #[instrument(
        skip(self, data),
        name = "analysis.analyze_while",
        target = "analysis",
        level = "trace"
    )]
    fn analyze_while(&mut self, data: &WhileLoopData) -> TypeResult<TypeIndex> {
        let cond_ty = self.analyze_instr(data.condition)?;
        self.require_bool(cond_ty);
        let _ = self.analyze_instr(data.body)?;
        Ok(self.pool.unit)
    }

    #[instrument(
        skip(self, data),
        name = "analysis.analyze_return",
        target = "analysis",
        level = "trace"
    )]
    fn analyze_return(&mut self, data: &ReturnData) -> TypeResult<TypeIndex> {
        match data.value {
            Some(idx) => self.analyze_instr(idx),
            None => Ok(self.pool.unit),
        }
    }

    #[instrument(
        skip(self, data),
        fields(
            instr_start = data.instr_start,
            instr_end = data.instr_end,
            return_loc = data.return_loc
        ),
        name = "analysis.analyze_region_instr",
        target = "analysis",
        level = "trace"
    )]
    fn analyze_region_instr(&mut self, data: &RegionData) -> TypeResult<TypeIndex> {
        self.analyze_region_range(data.instr_start, data.instr_end)
    }

    #[instrument(
        skip(self, data),
        fields(name = %data.name),
        name = "analysis.analyze_var_decl",
        target = "analysis",
        level = "trace"
    )]
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
            let value_et = self
                .worklist
                .expand_type(&self.pool.to_errand_type(value_ty));
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

    #[instrument(
        skip(self, data),
        fields(name = %data.name, is_foreign = data.is_foreign),
        name = "analysis.analyze_func_decl",
        target = "analysis",
        level = "trace"
    )]
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

    #[instrument(
        skip(self),
        fields(operand),
        name = "analysis.analyze_typeof",
        target = "analysis",
        level = "trace"
    )]
    fn analyze_typeof(&mut self, operand: instr_index) -> TypeResult<TypeIndex> {
        // `typeof(x)` evaluates to a string holding the type name of `x`.
        // Type-check the operand so its type is known by SIR emission, but the
        // result is always `String`.
        self.analyze_instr(operand)?;
        Ok(self.pool.string)
    }

    // ── Type construction helpers ─────────────────────────────────────────────

    /// The applied nominal type `Con(name)<Var p…>` used as the return type of
    /// both struct and enum-variant constructors.
    fn con_app_type(name: &str, type_params: &[String]) -> ErrandType {
        ErrandType::App(
            Box::new(ErrandType::Con(name.to_string())),
            type_params
                .iter()
                .map(|p| ErrandType::Var(p.clone()))
                .collect(),
        )
    }

    /// Wrap `inner` in one `∀` per type parameter (outermost = first parameter).
    fn wrap_foralls(type_params: &[String], inner: ErrandType) -> ErrandType {
        let mut ty = inner;
        for tp in type_params.iter().rev() {
            ty = ErrandType::Forall(tp.clone(), Box::new(ty));
        }
        ty
    }

    /// Build `field1 -> field2 -> … -> ret`, resolving each field type with
    /// `type_params` in scope.  An empty field iterator yields `ret` unchanged.
    fn constructor_arrow_type<'a>(
        field_types: impl DoubleEndedIterator<Item = &'a TypeExpression>,
        ret: ErrandType,
        type_params: &[String],
    ) -> ErrandType {
        field_types.rev().fold(ret, |acc, ft| {
            ErrandType::Arrow(
                Box::new(type_expr_to_errand_type_with_params(ft, type_params)),
                Box::new(acc),
            )
        })
    }

    /// Resolve and clone the descriptor for `enum_name::variant`, erroring if
    /// either the enum or the variant is unknown.
    fn lookup_enum_variant(&self, enum_name: &str, variant: &str) -> TypeResult<EnumVariantInfo> {
        let variants = self.enum_variants.get(enum_name).ok_or_else(|| {
            ErrandTypeError::UnboundVariable(format!("unknown enum `{}`", enum_name))
        })?;
        variants
            .iter()
            .find(|v| v.name == variant)
            .cloned()
            .ok_or_else(|| {
                ErrandTypeError::UnboundVariable(format!(
                    "unknown variant `{}` on enum `{}`",
                    variant, enum_name
                ))
            })
    }

    /// Map an enum's type parameters to the concrete type arguments of a
    /// scrutinee `App(Con(enum_name), args)`.  Empty unless the scrutinee is a
    /// matching application with the expected arity.
    fn scrutinee_subst(
        scrut_ty: &ErrandType,
        enum_name: &str,
        enum_tparams: &[String],
    ) -> HashMap<String, ErrandType> {
        let mut subst: HashMap<String, ErrandType> = HashMap::new();
        if let ErrandType::App(head, args) = scrut_ty {
            if let ErrandType::Con(en) = head.as_ref() {
                if en == enum_name && args.len() == enum_tparams.len() {
                    for (p, a) in enum_tparams.iter().zip(args.iter()) {
                        subst.insert(p.clone(), a.clone());
                    }
                }
            }
        }
        subst
    }

    // ── Region walk ───────────────────────────────────────────────────────────

    /// Analyze `instr_start..instr_end` in forward order, then drain the
    /// worklist.  Declarations are skipped, `VarDecl` is analyzed only for its
    /// `var_context` side-effect, and the last value-producing instruction's
    /// type is returned.
    #[instrument(
        skip(self),
        fields(instr_start, instr_end),
        name = "analysis.analyze_region_range",
        target = "analysis",
        level = "trace"
    )]
    fn analyze_region_range(
        &mut self,
        instr_start: instr_index,
        instr_end: instr_index,
    ) -> TypeResult<TypeIndex> {
        let mut last_ty = self.pool.unit;
        for i in instr_start..instr_end {
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

    // ── Constraint plumbing ───────────────────────────────────────────────────

    /// Push `left <: right`, both as pool handles.
    fn push_sub(&mut self, left: TypeIndex, right: TypeIndex) {
        let right = self.pool.to_errand_type(right);
        self.push_sub_type(left, right);
    }

    /// Push `left <: right` where `left` is a pool handle and `right` a type.
    fn push_sub_type(&mut self, left: TypeIndex, right: ErrandType) {
        self.worklist.push(WorklistEntry::Judgment(Judgment::Sub {
            left: self.pool.to_errand_type(left),
            right,
        }));
    }

    /// Constrain `cond` to be `Bool`.
    fn require_bool(&mut self, cond: TypeIndex) {
        let bool_ = self.pool.bool_;
        self.push_sub(cond, bool_);
    }

    /// Apply already-typed arguments to a (possibly existential) function type,
    /// pushing the per-argument subtyping constraints and returning the result
    /// type.  Shared by `analyze_call` and `analyze_enum_variant_construct`.
    fn apply_typed_arguments(
        &mut self,
        arg_types: &[TypeIndex],
        mut remaining: ErrandType,
    ) -> TypeResult<ErrandType> {
        for &arg_ty in arg_types {
            match remaining {
                ErrandType::Arrow(param_ty, result_ty) => {
                    self.push_sub_type(arg_ty, *param_ty);
                    remaining = *result_ty;
                }
                ErrandType::ETVar(ref evar_name) => {
                    let evar = evar_name.clone();
                    self.worklist
                        .push(WorklistEntry::TVar(evar.clone(), TyVarKind::Existential));
                    let result_evar = self.worklist.fresh_evar();
                    self.worklist.push(WorklistEntry::TVar(
                        result_evar.clone(),
                        TyVarKind::Existential,
                    ));
                    let expected_arrow = ErrandType::Arrow(
                        Box::new(self.pool.to_errand_type(arg_ty)),
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
        Ok(remaining)
    }

    // ── Type signatures for operators ─────────────────────────────────────────

    /// Returns `(expected_left, expected_right, result)` as `TypeIndex` triples.
    #[instrument(
        skip(self, op),
        name = "analysis.binop_signature",
        target = "analysis",
        level = "trace"
    )]
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
    #[instrument(
        skip(self, op),
        name = "analysis.unop_signature",
        target = "analysis",
        level = "trace"
    )]
    fn unop_signature(&mut self, op: &UnaryOperator) -> (TypeIndex, TypeIndex) {
        match op {
            UnaryOperator::Negate => (self.pool.int, self.pool.int),
            UnaryOperator::Not => (self.pool.bool_, self.pool.bool_),
        }
    }

    #[instrument(
        skip(self, params),
        fields(param_count = params.len(), return_idx = return_ty.0),
        name = "analysis.build_function_type",
        target = "analysis",
        level = "trace"
    )]
    fn build_function_type(&mut self, params: &[Parameter], return_ty: TypeIndex) -> ErrandType {
        params
            .iter()
            .rev()
            .fold(self.pool.to_errand_type(return_ty), |acc, param| {
                let param_ty = match &param.type_expr {
                    Some(te) => type_expr_to_errand_type(te),
                    None => ErrandType::ETVar(format!("param_{}", param.id.name)),
                };
                ErrandType::Arrow(Box::new(param_ty), Box::new(acc))
            })
    }

    // ── Type mangling utilities ───────────────────────────────────────────────

    #[instrument(
        skip(head, args),
        fields(head = %head, arg_count = args.len()),
        name = "analysis.mangle_type_app",
        target = "analysis",
        level = "trace"
    )]
    fn mangle_type_app(head: &str, args: &[ErrandType]) -> String {
        let mut s = head.to_string();
        for a in args {
            s.push_str("__");
            s.push_str(&Self::mangle_type_component(a));
        }
        s
    }

    #[instrument(
        skip(ty),
        name = "analysis.mangle_type_component",
        target = "analysis",
        level = "trace"
    )]
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

    #[instrument(
        skip(ty),
        name = "analysis.type_contains_etvar",
        target = "analysis",
        level = "trace"
    )]
    fn type_contains_etvar(ty: &ErrandType) -> bool {
        match ty {
            ErrandType::ETVar(_) => true,
            ErrandType::Arrow(a, b) => Self::type_contains_etvar(a) || Self::type_contains_etvar(b),
            ErrandType::App(h, a) => {
                Self::type_contains_etvar(h) || a.iter().any(Self::type_contains_etvar)
            }
            ErrandType::Forall(_, b) => Self::type_contains_etvar(b),
            ErrandType::Product(ts) => ts.iter().any(Self::type_contains_etvar),
            ErrandType::Var(_) | ErrandType::Con(_) => false,
        }
    }
}

// ─── Formatting helpers (private) ─────────────────────────────────────────────

/// Short, stable label for an instruction kind, used in tracing spans and the
/// human-readable inference trace.
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
