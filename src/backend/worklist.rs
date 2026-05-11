use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};

use tracing::instrument;

use crate::backend::preir::{instr_index, Instr};

/// Type inference for Errand's PreIR using the DK Worklist Algorithm.
/// This module contains only the constraint-solver data types and the `Worklist`
/// itself.  All analysis logic lives in `analysis::Analyzer`.

pub type TyVar = String;
pub type TmVar = String;

/// The type language used throughout analysis and SIR.
#[derive(Debug, Clone, PartialEq)]
pub enum ErrandType {
    /// Function type: T1 -> T2
    Arrow(Box<ErrandType>, Box<ErrandType>),
    /// Universal type variable: α
    Var(String),
    /// Existential type variable: ^α
    ETVar(String),
    /// Type constant: Int, Bool, String, Unit, …
    Con(String),
    /// Polymorphic type: ∀α. T
    Forall(String, Box<ErrandType>),
    /// Product type: T1 × T2 × …
    Product(Vec<ErrandType>),
    /// Type application: `F<T1, T2, …>` (e.g. `Option` applied to `Int`).
    App(Box<ErrandType>, Vec<ErrandType>),
}

/// Human-readable type syntax for debug / SIR dumps. For mangling keys and
/// overload dispatch, use [`crate::backend::sir_gen::errand_type_name`] instead.
impl Display for ErrandType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ErrandType::Var(n) | ErrandType::ETVar(n) | ErrandType::Con(n) => write!(f, "{n}"),
            ErrandType::Arrow(a, b) => write!(f, "{a} -> {b}"),
            ErrandType::Forall(v, t) => write!(f, "∀{v}. {t}"),
            ErrandType::Product(ts) => {
                let parts: Vec<String> = ts.iter().map(|t| t.to_string()).collect();
                write!(f, "{}", parts.join(" × "))
            }
            ErrandType::App(h, args) => {
                let parts: Vec<String> = args.iter().map(|a| a.to_string()).collect();
                write!(f, "{}<{}>", h, parts.join(", "))
            }
        }
    }
}

/// An entry in the constraint-solving worklist.
#[derive(Debug, Clone, PartialEq)]
pub enum WorklistEntry {
    /// Type-variable binding: α (universal / existential / solved / marker)
    TVar(TyVar, TyVarKind),
    /// Term-variable binding: x : T
    Var(TmVar, ErrandType),
    /// A typing judgment to be solved
    Judgment(Judgment),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TyVarKind {
    Universal,
    Existential,
    Solved(ErrandType),
    Marker,
}

/// Typing judgments pushed onto the worklist.
#[derive(Debug, Clone, PartialEq)]
pub enum Judgment {
    /// Subtyping constraint: A <: B
    Sub { left: ErrandType, right: ErrandType },
    /// Inference mode: synthesise a type for `instr` and unify with `ty`
    Inf { instr: Instr, ty: ErrandType },
    /// Checking mode: verify `instr` has type `ty`
    Chk { instr: Instr, ty: ErrandType },
    /// Application inference: `func_ty • arg` yields `result_ty` (DK algorithm).
    InfApp {
        func_ty: ErrandType,
        arg_idx: instr_index,
        result_ty: ErrandType,
    },
}

// ─── Worklist ─────────────────────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub struct Worklist {
    entries: Vec<WorklistEntry>,
    next_var: usize,
    /// Latest solution for each existential (survives after the solve loop pops `TVar` entries).
    pub evar_bindings: HashMap<String, ErrandType>,
}

impl Default for Worklist {
    #[instrument(name = "worklist.default", target = "worklist", level = "trace")]
    fn default() -> Self {
        Self::new()
    }
}

impl Worklist {
    #[instrument(name = "worklist.new", target = "worklist", level = "trace")]
    pub fn new() -> Self {
        Worklist {
            entries: Vec::new(),
            next_var: 0,
            evar_bindings: HashMap::new(),
        }
    }

    /// Substitute solved existential variables (one level; call in a loop for fixpoint).
    #[instrument(
        skip(self, ty),
        name = "worklist.expand_type_shallow",
        target = "worklist",
        level = "trace"
    )]
    pub fn expand_type_shallow(&self, ty: &ErrandType) -> ErrandType {
        match ty {
            ErrandType::ETVar(n) => self
                .evar_bindings
                .get(n)
                .cloned()
                .unwrap_or_else(|| ty.clone()),
            ErrandType::Arrow(a, b) => ErrandType::Arrow(
                Box::new(self.expand_type_shallow(a)),
                Box::new(self.expand_type_shallow(b)),
            ),
            ErrandType::App(h, args) => ErrandType::App(
                Box::new(self.expand_type_shallow(h)),
                args.iter().map(|t| self.expand_type_shallow(t)).collect(),
            ),
            ErrandType::Forall(v, b) => {
                ErrandType::Forall(v.clone(), Box::new(self.expand_type_shallow(b)))
            }
            ErrandType::Product(ts) => {
                ErrandType::Product(ts.iter().map(|t| self.expand_type_shallow(t)).collect())
            }
            ErrandType::Var(_) | ErrandType::Con(_) => ty.clone(),
        }
    }

    /// Expand evar bindings until fixpoint.
    #[instrument(
        skip(self, ty),
        name = "worklist.expand_type",
        target = "worklist",
        level = "trace"
    )]
    pub fn expand_type(&self, ty: &ErrandType) -> ErrandType {
        let mut cur = ty.clone();
        for _ in 0..64 {
            let next = self.expand_type_shallow(&cur);
            if next == cur {
                return cur;
            }
            cur = next;
        }
        cur
    }

    #[instrument(
        skip(self),
        name = "worklist.fresh_var",
        target = "worklist",
        level = "trace"
    )]
    pub fn fresh_var(&mut self) -> TyVar {
        let v = format!("α{}", self.next_var);
        self.next_var += 1;
        v
    }

    #[instrument(
        skip(self),
        name = "worklist.fresh_evar",
        target = "worklist",
        level = "trace"
    )]
    pub fn fresh_evar(&mut self) -> TyVar {
        let v = format!("^α{}", self.next_var);
        self.next_var += 1;
        v
    }

    #[instrument(
        skip(self, entry),
        name = "worklist.push",
        target = "worklist",
        level = "trace"
    )]
    pub fn push(&mut self, entry: WorklistEntry) {
        self.entries.push(entry);
    }

    #[instrument(
        skip(self),
        name = "worklist.pop",
        target = "worklist",
        level = "trace"
    )]
    pub fn pop(&mut self) -> Option<WorklistEntry> {
        self.entries.pop()
    }

    /// Find the type of a term variable in the worklist (right-to-left search).
    #[instrument(skip(self), fields(name = %name), name = "worklist.find_var", target = "worklist", level = "trace")]
    pub fn find_var(&self, name: &str) -> Option<&ErrandType> {
        self.entries.iter().rev().find_map(|e| {
            if let WorklistEntry::Var(n, ty) = e {
                if n == name {
                    Some(ty)
                } else {
                    None
                }
            } else {
                None
            }
        })
    }

    /// Solve an existential variable by recording its solution in the worklist.
    #[instrument(skip(self, ty), fields(name = %name), name = "worklist.solve_evar", target = "worklist", level = "trace")]
    pub fn solve_evar(&mut self, name: &str, ty: ErrandType) -> Result<(), ErrandTypeError> {
        for entry in &mut self.entries {
            if let WorklistEntry::TVar(var_name, kind) = entry {
                if var_name == name {
                    match kind {
                        TyVarKind::Existential => {
                            self.evar_bindings.insert(var_name.clone(), ty.clone());
                            *kind = TyVarKind::Solved(ty);
                            return Ok(());
                        }
                        TyVarKind::Solved(_) => return Ok(()),
                        _ => continue,
                    }
                }
            }
        }
        Err(ErrandTypeError::UnboundVariable(name.to_string()))
    }

    /// Returns true if type variable `a` appears before `b` in the worklist
    /// (earlier index = declared earlier).
    #[instrument(skip(self), fields(a = %a, b = %b), name = "worklist.before", target = "worklist", level = "trace")]
    pub fn before(&self, a: &str, b: &str) -> bool {
        let mut pos_a = None;
        let mut pos_b = None;
        for (i, entry) in self.entries.iter().enumerate() {
            if let WorklistEntry::TVar(name, _) = entry {
                if name == a {
                    pos_a = Some(i);
                }
                if name == b {
                    pos_b = Some(i);
                }
            }
        }
        matches!((pos_a, pos_b), (Some(pa), Some(pb)) if pa < pb)
    }
}

// ─── Errors ───────────────────────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub enum ErrandTypeError {
    UnboundVariable(String),
    SubtypingError { left: ErrandType, right: ErrandType },
    NotAFunction(ErrandType),
    ArityMismatch { expected: usize, actual: usize },
    UnsupportedOperation(String),
    RegionTypingError(String),
}

pub type TypeResult<T> = Result<T, ErrandTypeError>;
