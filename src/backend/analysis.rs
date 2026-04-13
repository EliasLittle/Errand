use std::collections::HashMap;

use crate::backend::errand_builtins::{
    add_builtin_data_constructors, add_builtin_functions, type_expr_to_errand_type,
};
use crate::backend::preir::{
    BinOpPl, FnCallPl, FuncData, IfStatementData, Instr, LiteralPl, PreIR, RegionData,
    ReturnData, StructData, UnOpPl, VarDeclData, VarRefData, WhileLoopData, instr_index,
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
    global_defs: HashMap<String, TypeIndex>,
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

        Analyzer {
            pool,
            preir,
            var_context: HashMap::new(),
            module_context: HashMap::new(),
            analysis_cache: HashMap::new(),
            global_defs,
            worklist,
            trace: Vec::new(),
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
                    Instr::FuncDecl(_) | Instr::StructDecl(_) => continue,
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
        // Comptime fold: typeof(x) → the type of x as a Symbol literal.
        // This is handled structurally in `analyze_instr` (caller sees FnCall),
        // but the actual fold happens in SirGen; here we just return Con("Type").
        if data.name == "typeof" && data.arguments.len() == 1 {
            if let Some(&arg_idx) = data.arguments.first() {
                self.analyze_instr(arg_idx)?;
            }
            let type_ty = self.pool.intern(ErrandType::Con("Type".into()));
            return Ok(type_ty);
        }

        let func_ty_idx = self
            .global_defs
            .get(&data.name)
            .copied()
            .ok_or_else(|| ErrandTypeError::UnboundVariable(data.name.clone()))?;

        let mut arg_types = Vec::new();
        for &arg_idx in &data.arguments {
            arg_types.push(self.analyze_instr(arg_idx)?);
        }

        let mut remaining = self.pool.to_errand_type(func_ty_idx);
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
                    Instr::FuncDecl(_) | Instr::StructDecl(_) => continue,
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
        self.var_context.insert(data.name.clone(), value_ty);
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

    fn solve(&mut self) -> TypeResult<()> {
        while let Some(entry) = self.worklist.pop() {
            match entry {
                WorklistEntry::TVar(_, _) | WorklistEntry::Var(_, _) => continue,
                WorklistEntry::Judgment(j) => self.solve_judgment(j)?,
            }
        }
        Ok(())
    }

    fn solve_judgment(&mut self, j: Judgment) -> TypeResult<()> {
        match j {
            Judgment::Sub { left, right } => self.solve_subtype(left, right),
            Judgment::Inf { instr, ty } => self.solve_inference(instr, ty),
            Judgment::Chk { instr, ty } => self.solve_checking(instr, ty),
            Judgment::InstrSeq { .. } => Ok(()),
        }
    }

    fn solve_subtype(&mut self, left: ErrandType, right: ErrandType) -> TypeResult<()> {
        self.trace.push(format!("Sub {} <: {}", fmt_ty(&left), fmt_ty(&right)));

        if left == right {
            return Ok(());
        }

        match (&left, &right) {
            (ErrandType::Var(a), ErrandType::Var(b)) if a == b => Ok(()),
            (ErrandType::ETVar(a), ErrandType::ETVar(b)) if a == b => Ok(()),
            (ErrandType::Con(_), ErrandType::Var(_)) => Ok(()),
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
            Instr::IfStatement(_) | Instr::Region(_) | Instr::ForLoop(_) => Ok(()),
        }
    }

    fn solve_checking(&mut self, instr: Instr, ty: ErrandType) -> TypeResult<()> {
        self.trace.push(format!("Chk {} ⇐ {}", fmt_instr(&instr), fmt_ty(&ty)));
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
            ErrandType::ETVar(n) | ErrandType::Var(n) | ErrandType::Con(n) => n == var,
            ErrandType::Arrow(t1, t2) => self.occurs_check(var, t1) || self.occurs_check(var, t2),
            ErrandType::Forall(_, t) => self.occurs_check(var, t),
            ErrandType::Product(ts) | ErrandType::Sum(ts) => {
                ts.iter().any(|t| self.occurs_check(var, t))
            }
        }
    }

    fn is_monotype(&self, ty: &ErrandType) -> bool {
        match ty {
            ErrandType::Var(_) | ErrandType::ETVar(_) | ErrandType::Con(_) => true,
            ErrandType::Arrow(t1, t2) => self.is_monotype(t1) && self.is_monotype(t2),
            ErrandType::Product(ts) | ErrandType::Sum(ts) => {
                ts.iter().all(|t| self.is_monotype(t))
            }
            ErrandType::Forall(_, _) => false,
        }
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

        // Struct constructors: field1 -> field2 -> ... -> StructName
        for instr in &preir.instructions {
            if let Instr::StructDecl(StructData { name, fields }) = instr {
                let ctor_ty = fields.iter().rev().fold(
                    ErrandType::Con(name.clone()),
                    |acc, field| {
                        ErrandType::Arrow(
                            Box::new(type_expr_to_errand_type(&field.field_type)),
                            Box::new(acc),
                        )
                    },
                );
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
        ErrandType::Sum(ts) => ts.iter().map(fmt_ty).collect::<Vec<_>>().join(" + "),
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
    }
}
