use crate::backend::preir::{Instr, instr_index};

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
    /// Sum type: T1 + T2 + …
    Sum(Vec<ErrandType>),
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
    /// Instruction-sequence typing (placeholder for future use)
    InstrSeq { instrs: Vec<instr_index>, ty: ErrandType },
}

// ─── Worklist ─────────────────────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub struct Worklist {
    entries: Vec<WorklistEntry>,
    next_var: usize,
}

impl Default for Worklist {
    fn default() -> Self {
        Self::new()
    }
}

impl Worklist {
    pub fn new() -> Self {
        Worklist { entries: Vec::new(), next_var: 0 }
    }

    pub fn fresh_var(&mut self) -> TyVar {
        let v = format!("α{}", self.next_var);
        self.next_var += 1;
        v
    }

    pub fn fresh_evar(&mut self) -> TyVar {
        let v = format!("^α{}", self.next_var);
        self.next_var += 1;
        v
    }

    pub fn push(&mut self, entry: WorklistEntry) {
        self.entries.push(entry);
    }

    pub fn pop(&mut self) -> Option<WorklistEntry> {
        self.entries.pop()
    }

    /// Find the type of a term variable in the worklist (right-to-left search).
    pub fn find_var(&self, name: &str) -> Option<&ErrandType> {
        self.entries.iter().rev().find_map(|e| {
            if let WorklistEntry::Var(n, ty) = e {
                if n == name { Some(ty) } else { None }
            } else {
                None
            }
        })
    }

    /// Solve an existential variable by recording its solution in the worklist.
    pub fn solve_evar(&mut self, name: &str, ty: ErrandType) -> Result<(), ErrandTypeError> {
        for entry in &mut self.entries {
            if let WorklistEntry::TVar(var_name, kind) = entry {
                if var_name == name {
                    match kind {
                        TyVarKind::Existential => {
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
    pub fn before(&self, a: &str, b: &str) -> bool {
        let mut pos_a = None;
        let mut pos_b = None;
        for (i, entry) in self.entries.iter().enumerate() {
            if let WorklistEntry::TVar(name, _) = entry {
                if name == a { pos_a = Some(i); }
                if name == b { pos_b = Some(i); }
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
