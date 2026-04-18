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

/// Metadata for one overload of a named function.
/// `body` is `None` for foreign/extern declarations that have no Errand body.
#[derive(Debug, Clone)]
pub struct SIRFunctionInfo {
    /// Parameter names paired with their resolved types.
    pub params: Vec<(String, ErrandType)>,
    pub return_type: ErrandType,
    pub is_foreign: bool,
    pub body: Option<SIR>,
}

/// A single field in a struct's memory layout.
#[derive(Debug, Clone)]
pub struct SIRStructField {
    pub name: String,
    pub ty: ErrandType,
    pub byte_offset: usize,
}

/// Full memory layout of a struct computed at SIR generation time.
#[derive(Debug, Clone)]
pub struct SIRStructLayout {
    pub fields: Vec<SIRStructField>,
    pub total_size: usize,
}

/// Layout of a single enum variant at the SIR level.
#[derive(Debug, Clone)]
pub struct SIREnumVariantLayout {
    pub name: String,
    /// Fields, with byte offsets relative to the START of the payload region
    /// (i.e. offset 8 from the base of the tagged-union allocation).
    pub fields: Vec<SIRStructField>,
    /// Total bytes consumed by this variant's payload.
    pub payload_size: usize,
}

/// Full tagged-union layout of an enum computed at SIR generation time.
///
/// `is_simple` is true when ALL variants are unit (no payload) — in that case
/// the enum is stored as a bare `i64` tag rather than a heap/stack allocation,
/// and `total_size` is 0 (irrelevant).
#[derive(Debug, Clone)]
pub struct SIREnumLayout {
    /// Variants in declaration order; `variants[i]` has integer tag `i`.
    pub variants: Vec<SIREnumVariantLayout>,
    /// `8 (tag) + max(payload_size across all variants)`.  0 when `is_simple`.
    pub total_size: usize,
    /// `true` iff every variant has zero fields (pure unit-tag enum).
    pub is_simple: bool,
}

/// All SIR for a program: one for the top-level main body, one per function.
///
/// `functions` uses a nested map: `plain_name → param_type_keys → SIRFunctionInfo`.
/// `Vec<String>` keys are the stringified `ErrandType` of each parameter (in order),
/// matching the output of `errand_type_name`. This avoids requiring `Hash` on `ErrandType`.
#[derive(Debug, Clone)]
pub struct SIRModule {
    pub main: SIR,
    pub functions: HashMap<String, HashMap<Vec<String>, SIRFunctionInfo>>,
    pub structs: HashMap<String, SIRStructLayout>,
    pub enums: HashMap<String, SIREnumLayout>,
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

        if !self.enums.is_empty() {
            let mut enum_names: Vec<&String> = self.enums.keys().collect();
            enum_names.sort();
            for name in enum_names {
                let layout = &self.enums[name];
                let variants_str: Vec<String> = layout.variants.iter().enumerate()
                    .map(|(i, v)| {
                        if v.fields.is_empty() {
                            format!("{} = {}", v.name, i)
                        } else {
                            let fields_str: Vec<String> = v.fields.iter().map(|f| format!("{}@{}", f.name, f.byte_offset)).collect();
                            format!("{} = {} ({})", v.name, i, fields_str.join(", "))
                        }
                    })
                    .collect();
                out.push_str(&format!("\n=== SIR: enum {} {{ {} }} ===\n", name, variants_str.join(", ")));
            }
        }

        let mut names: Vec<&String> = self.functions.keys().collect();
        names.sort();
        for name in names {
            let overloads = &self.functions[name];
            let mut keys: Vec<&Vec<String>> = overloads.keys().collect();
            keys.sort();
            for key in keys {
                let info = &overloads[key];
                let params_str = info
                    .params
                    .iter()
                    .map(|(n, t)| format!("{}: {}", n, errand_type_to_string(t)))
                    .collect::<Vec<_>>()
                    .join(", ");
                let foreign_tag = if info.is_foreign { " [foreign]" } else { "" };
                out.push_str(&format!(
                    "\n=== SIR: {}({}){}  -> {} ===\n",
                    name,
                    params_str,
                    foreign_tag,
                    errand_type_to_string(&info.return_type)
                ));
                if let Some(body) = &info.body {
                    out.push_str(&body.format_all());
                } else {
                    out.push_str("    (no body — extern declaration)\n");
                }
            }
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
