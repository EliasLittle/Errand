//! Constraint solver for the [`Analyzer`](super::Analyzer).
//!
//! This is the worklist-driven half of analysis: draining pending judgments,
//! deciding subtyping, and instantiating existential / universal variables
//! (the DK worklist algorithm).  The demand-driven typing pass that *produces*
//! these judgments lives in the parent [`super`] module.

use std::collections::HashMap;

use crate::backend::fir::{
    InstrIndex, BinOpPl, FnCallPl, Instr, LiteralPl, ReturnData, UnOpPl, VarRefData,
};
use crate::backend::worklist::{
    ErrandType, ErrandTypeError, Judgment, TyVarKind, TypeResult, WorklistEntry,
};
use tracing::instrument;

use super::{fmt_instr, Analyzer};

impl Analyzer {
    // ── Worklist driver ───────────────────────────────────────────────────────

    #[instrument(
        skip(self),
        name = "analysis.solve",
        target = "analysis",
        level = "trace"
    )]
    pub(super) fn solve(&mut self) -> TypeResult<()> {
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
    #[instrument(
        skip(self),
        name = "analysis.drain_silently",
        target = "analysis",
        level = "trace"
    )]
    pub(super) fn drain_silently(&mut self) {
        while let Some(entry) = self.worklist.pop() {
            match entry {
                WorklistEntry::TVar(_, _) | WorklistEntry::Var(_, _) => continue,
                WorklistEntry::Judgment(j) => {
                    let _ = self.solve_judgment(j);
                }
            }
        }
    }

    #[instrument(
        skip(self, j),
        name = "analysis.solve_judgment",
        target = "analysis",
        level = "trace"
    )]
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

    // ── Judgment solvers ──────────────────────────────────────────────────────

    #[instrument(
        skip(self, func_ty, result_ty),
        fields(arg_idx),
        name = "analysis.solve_inf_app",
        target = "analysis",
        level = "trace"
    )]
    fn solve_inf_app(
        &mut self,
        func_ty: ErrandType,
        arg_idx: InstrIndex,
        result_ty: ErrandType,
    ) -> TypeResult<()> {
        let arg_instr = self
            .fir
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
                let subbed =
                    self.apply_substs_to_type(&body, &HashMap::from([(v, ErrandType::ETVar(e))]));
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

    #[instrument(
        skip(self, left, right),
        name = "analysis.solve_subtype",
        target = "analysis",
        level = "trace"
    )]
    pub(super) fn solve_subtype(&mut self, left: ErrandType, right: ErrandType) -> TypeResult<()> {
        // Substitute any already-solved existentials on both sides before
        // making subtyping decisions. Without this, a constraint like
        // `^α <: Int` for an existential that has already been solved to
        // `String` would silently no-op in `solve_evar` (TyVarKind::Solved
        // arm), letting bogus programs through. After expansion the same
        // constraint becomes `String <: Int`, which correctly fails.
        let left = self.worklist.expand_type(&left);
        let right = self.worklist.expand_type(&right);

        self.trace.push(format!("Sub {} <: {}", left, right));

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
                self.worklist
                    .push(WorklistEntry::TVar(fresh.clone(), TyVarKind::Universal));
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
                self.worklist
                    .push(WorklistEntry::TVar(fresh_e.clone(), TyVarKind::Marker));
                self.worklist
                    .push(WorklistEntry::TVar(fresh_e.clone(), TyVarKind::Existential));
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

    // ── Inference judgments (instr ⊢ ty) ──────────────────────────────────────

    #[instrument(
        skip(self, instr, ty),
        fields(instr_kind = fmt_instr(&instr)),
        name = "analysis.solve_inference",
        target = "analysis",
        level = "trace"
    )]
    fn solve_inference(&mut self, instr: Instr, ty: ErrandType) -> TypeResult<()> {
        self.trace
            .push(format!("Inf {} ⊢ {}", fmt_instr(&instr), ty));
        match instr {
            Instr::Literal(lit) => self.solve_inference_literal(lit, ty),
            Instr::VarRef(data) => self.solve_inference_var_ref(data, ty),
            Instr::BinOp(data) => self.solve_inference_binop(data, ty),
            Instr::UnOp(data) => self.solve_inference_unop(data, ty),
            Instr::WhileLoop(_) | Instr::VarDecl(_) | Instr::FuncDecl(_) | Instr::StructDecl(_) => {
                self.solve_inference_unit(ty)
            }
            Instr::FnCall(data) => self.solve_inference_call(data, ty),
            Instr::Return(data) => self.solve_inference_return(data, ty),
            Instr::IfStatement(_)
            | Instr::Region(_)
            | Instr::ForLoop(_)
            | Instr::EnumDecl(_)
            | Instr::EnumVariantAccess(_)
            | Instr::EnumVariantConstruct(_)
            | Instr::Match(_) => Ok(()),
            Instr::Typeof(_) => self.solve_inference_typeof(ty),
            Instr::New(_)
            | Instr::Printf(_)
            | Instr::MemLoad(_)
            | Instr::MemStore(_)
            | Instr::GetField(_)
            | Instr::Ffi(_)
            | Instr::AsPtr(_)
            | Instr::AsString(_) => {
                unreachable!("builtin operations are introduced during SIR generation, after analysis")
            }
        }
    }

    fn solve_inference_literal(&mut self, lit: LiteralPl, ty: ErrandType) -> TypeResult<()> {
        let lit_ty = self.infer_literal(&lit)?;
        self.push_sub_type(lit_ty, ty);
        Ok(())
    }

    fn solve_inference_var_ref(&mut self, data: VarRefData, ty: ErrandType) -> TypeResult<()> {
        if let Some(&var_ty) = self.var_context.get(&data.name) {
            self.push_sub_type(var_ty, ty);
            return Ok(());
        }
        if let Some(&var_ty) = self.global_defs.get(&data.name) {
            self.push_sub_type(var_ty, ty);
            return Ok(());
        }
        Err(ErrandTypeError::UnboundVariable(data.name))
    }

    fn solve_inference_binop(&mut self, data: BinOpPl, ty: ErrandType) -> TypeResult<()> {
        let (_, _, result) = self.binop_signature(&data.op);
        self.push_sub_type(result, ty);
        Ok(())
    }

    fn solve_inference_unop(&mut self, data: UnOpPl, ty: ErrandType) -> TypeResult<()> {
        let (_, result) = self.unop_signature(&data.op);
        self.push_sub_type(result, ty);
        Ok(())
    }

    fn solve_inference_unit(&mut self, ty: ErrandType) -> TypeResult<()> {
        let unit = self.pool.unit;
        self.push_sub_type(unit, ty);
        Ok(())
    }

    fn solve_inference_call(&mut self, data: FnCallPl, ty: ErrandType) -> TypeResult<()> {
        if let Some(&func_ty) = self.global_defs.get(&data.name) {
            self.push_sub_type(func_ty, ty);
            Ok(())
        } else {
            Err(ErrandTypeError::UnboundVariable(data.name))
        }
    }

    fn solve_inference_return(&mut self, data: ReturnData, ty: ErrandType) -> TypeResult<()> {
        if data.value.is_none() {
            let unit = self.pool.unit;
            self.push_sub_type(unit, ty);
        }
        Ok(())
    }

    fn solve_inference_typeof(&mut self, ty: ErrandType) -> TypeResult<()> {
        // `typeof` is always typed `String`.
        let string = self.pool.string;
        self.push_sub_type(string, ty);
        Ok(())
    }

    #[instrument(
        skip(self, instr, ty),
        fields(instr_kind = fmt_instr(&instr)),
        name = "analysis.solve_checking",
        target = "analysis",
        level = "trace"
    )]
    fn solve_checking(&mut self, instr: Instr, ty: ErrandType) -> TypeResult<()> {
        self.trace
            .push(format!("Chk {} ⇐ {}", fmt_instr(&instr), ty));
        if let ErrandType::Forall(var, body_ty) = &ty {
            let fresh = self.worklist.fresh_var();
            self.worklist
                .push(WorklistEntry::TVar(fresh.clone(), TyVarKind::Universal));
            let substituted = self.apply_substs_to_type(
                body_ty,
                &HashMap::from([(var.clone(), ErrandType::Var(fresh))]),
            );
            return self.solve_checking(instr, substituted);
        }
        let inferred_evar = self.worklist.fresh_evar();
        self.worklist.push(WorklistEntry::TVar(
            inferred_evar.clone(),
            TyVarKind::Existential,
        ));
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

    #[instrument(
        skip(self, ty),
        fields(var = %var),
        name = "analysis.instantiate_left",
        target = "analysis",
        level = "trace"
    )]
    fn instantiate_left(&mut self, var: &str, ty: &ErrandType) -> TypeResult<()> {
        match ty {
            ErrandType::ETVar(b) if self.worklist.before(var, b) => {
                self.worklist
                    .solve_evar(b, ErrandType::ETVar(var.to_string()))?;
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
                self.worklist
                    .push(WorklistEntry::TVar(a1.clone(), TyVarKind::Existential));
                self.worklist
                    .push(WorklistEntry::TVar(a2.clone(), TyVarKind::Existential));
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
                self.worklist
                    .push(WorklistEntry::TVar(fresh.clone(), TyVarKind::Universal));
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
                ty
            ))),
        }
    }

    #[instrument(
        skip(self, ty),
        fields(var = %var),
        name = "analysis.instantiate_right",
        target = "analysis",
        level = "trace"
    )]
    fn instantiate_right(&mut self, ty: &ErrandType, var: &str) -> TypeResult<()> {
        match ty {
            ErrandType::ETVar(a) if self.worklist.before(var, a) => {
                self.worklist
                    .solve_evar(a, ErrandType::ETVar(var.to_string()))?;
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
                self.worklist
                    .push(WorklistEntry::TVar(a1.clone(), TyVarKind::Existential));
                self.worklist
                    .push(WorklistEntry::TVar(a2.clone(), TyVarKind::Existential));
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
                self.worklist
                    .push(WorklistEntry::TVar(fresh_e.clone(), TyVarKind::Marker));
                self.worklist
                    .push(WorklistEntry::TVar(fresh_e.clone(), TyVarKind::Existential));
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
                ty
            ))),
        }
    }

    #[instrument(
        skip(self, ty),
        fields(var = %var),
        name = "analysis.occurs_check",
        target = "analysis",
        level = "trace"
    )]
    fn occurs_check(&self, var: &str, ty: &ErrandType) -> bool {
        match ty {
            ErrandType::ETVar(n) | ErrandType::Var(n) => n == var,
            ErrandType::Con(_) => false,
            ErrandType::Arrow(t1, t2) => self.occurs_check(var, t1) || self.occurs_check(var, t2),
            ErrandType::Forall(_, t) => self.occurs_check(var, t),
            ErrandType::Product(ts) => ts.iter().any(|t| self.occurs_check(var, t)),
            ErrandType::App(h, args) => {
                self.occurs_check(var, h) || args.iter().any(|t| self.occurs_check(var, t))
            }
        }
    }

    #[instrument(
        skip(self, ty),
        name = "analysis.is_monotype",
        target = "analysis",
        level = "trace"
    )]
    fn is_monotype(&self, ty: &ErrandType) -> bool {
        match ty {
            ErrandType::Var(_) | ErrandType::ETVar(_) | ErrandType::Con(_) => true,
            ErrandType::Arrow(t1, t2) => self.is_monotype(t1) && self.is_monotype(t2),
            ErrandType::Product(ts) => ts.iter().all(|t| self.is_monotype(t)),
            ErrandType::Forall(_, _) => false,
            ErrandType::App(h, args) => {
                self.is_monotype(h) && args.iter().all(|t| self.is_monotype(t))
            }
        }
    }

    /// Peel top-level `∀` into fresh existentials (DK instantiation for constructors).
    #[instrument(
        skip(self, ctor_ty),
        name = "analysis.instantiate_constructor_type",
        target = "analysis",
        level = "trace"
    )]
    pub(super) fn instantiate_constructor_type(
        &mut self,
        ctor_ty: &ErrandType,
    ) -> TypeResult<ErrandType> {
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
}
