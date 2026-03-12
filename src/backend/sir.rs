use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result as FmtResult};

use crate::backend::preir::{Instr, instr_index};
use crate::backend::worklist::ErrandType;

/// One typed instruction in SIR. Operand indices are local to this SIR instance.
#[derive(Debug, Clone)]
pub struct SIRInstr {
    pub instr: Instr,
    pub ty: Option<ErrandType>,
}

/// Per-function (or main) typed IR. All instr_index references inside `instr`
/// fields are local indices into this SIR's `instructions` vec.
#[derive(Debug, Clone)]
pub struct SIR {
    pub instructions: Vec<SIRInstr>,
    /// Local index of the instruction whose value is this SIR's result.
    /// TODO: I think we might be able to remove this and just use the last instruction?
    pub return_loc: instr_index,
}

/// All SIR for a program: one for the top-level main body, one per function.
#[derive(Debug, Clone)]
pub struct SIRModule {
    pub main: SIR,
    pub functions: HashMap<String, SIR>,
}

// ─── Formatting ──────────────────────────────────────────────────────────────

// TODO: Remove this and just use the ErrandType's Display impl?
fn errand_type_to_string(ty: &ErrandType) -> String {
    match ty {
        ErrandType::Var(n) | ErrandType::ETVar(n) | ErrandType::Con(n) => n.clone(),
        ErrandType::Arrow(a, b) => format!("{} -> {}", errand_type_to_string(a), errand_type_to_string(b)),
        ErrandType::Forall(v, t) => format!("∀{}. {}", v, errand_type_to_string(t)),
        ErrandType::Product(ts) => ts.iter().map(errand_type_to_string).collect::<Vec<_>>().join(" × "),
        ErrandType::Sum(ts) => ts.iter().map(errand_type_to_string).collect::<Vec<_>>().join(" + "),
    }
}

impl SIR {
    /// Format all instructions as `%N : Ty = instr` (declarations without `%N =`).
    pub fn format_all(&self) -> String {
        let mut out = String::new();
        for (i, si) in self.instructions.iter().enumerate() {
            let ty_str = match &si.ty {
                Some(ty) => errand_type_to_string(ty),
                None => "?".to_string(),
            };
            let instr_str = format!("{}", si.instr);
            let line = match &si.instr {
                Instr::VarDecl(_) | Instr::FuncDecl(_) | Instr::StructDecl(_) => {
                    format!("    {} : {}\n", instr_str, ty_str)
                }
                _ => format!("%{} : {} = {}\n", i, ty_str, instr_str),
            };
            out.push_str(&line);
        }
        out.push_str(&format!("\nreturn_loc: %{}\n", self.return_loc));
        out
    }
}

impl SIRModule {
    pub fn format_all(&self) -> String {
        let mut out = String::new();
        out.push_str("=== SIR: main ===\n");
        out.push_str(&self.main.format_all());

        let mut names: Vec<&String> = self.functions.keys().collect();
        names.sort();
        for name in names {
            out.push_str(&format!("\n=== SIR: {} ===\n", name));
            out.push_str(&self.functions[name].format_all());
        }
        out
    }
}

impl Display for SIR {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{}", self.format_all())
    }
}

impl Display for SIRModule {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{}", self.format_all())
    }
}
