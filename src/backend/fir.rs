use crate::frontend::ast::{
    BinaryOperator, FieldDefinition, Id, Parameter, TypeExpression, UnaryOperator,
};
use std::fmt::{Display, Formatter, Result as FmtResult};

// ─── FIR container & pretty-printing ───────────────────────────────────────

#[derive(Debug, Clone)]
pub struct FIR {
    pub main: Instr,
    pub instructions: Vec<Instr>,
}

impl FIR {
    /// Parenthesized recursive formatting for an operand index, or `%idx` if missing.
    fn format_operand_idx(&self, idx: InstrIndex) -> String {
        match self.get_instruction(idx) {
            Some(instr) => format!("({})", self.format_instr_with_context(instr)),
            None => format!("%{}", idx),
        }
    }

    /// Creates a new `FIR` instance with an empty `Region` instruction as its main body.
    pub fn init() -> Self {
        FIR {
            main: Instr::Region(RegionData {
                instr_start: 0,
                instr_end: 0,
                return_loc: 0,
            }),
            instructions: Vec::new(),
        }
    }

    pub fn emit_instruction(&mut self, instr: Instr) -> InstrIndex {
        let index = self.instructions.len() as InstrIndex;
        self.instructions.push(instr);
        index
    }

    /// Get an instruction by index, returning None if out of bounds
    pub fn get_instruction(&self, index: InstrIndex) -> Option<&Instr> {
        if index >= 0 && (index as usize) < self.instructions.len() {
            Some(&self.instructions[index as usize])
        } else {
            None
        }
    }

    /// Format an instruction with context
    pub fn format_instruction(&self, index: InstrIndex) -> String {
        match self.get_instruction(index) {
            Some(instr) => {
                if matches!(
                    instr,
                    Instr::VarDecl(_)
                        | Instr::FuncDecl(_)
                        | Instr::StructDecl(_)
                        | Instr::EnumDecl(_)
                ) {
                    format!("    {}", self.format_instr_with_context(instr))
                } else {
                    format!("%{} = {}", index, self.format_instr_with_context(instr))
                }
            }
            None => format!("%{} = <invalid>", index),
        }
    }

    /// Format an instruction with contextual information (resolving indices)
    pub fn format_instr_with_context(&self, instr: &Instr) -> String {
        match instr {
            Instr::Literal(data) => format!("{}", data),
            Instr::VarRef(data) => format!("{}", data.name),
            Instr::UnOp(data) => {
                let operand = self.format_operand_idx(data.operand);
                format!("unop {:?} {}", data.op, operand)
            }
            Instr::BinOp(data) => {
                format!("binop {:?} %{} %{}", data.op, data.left, data.right)
            }
            Instr::FnCall(data) => {
                let args: Vec<String> = data
                    .arguments
                    .iter()
                    .map(|&arg_idx| self.format_operand_idx(arg_idx))
                    .collect();
                format!("call {}({})", data.name, args.join(", "))
            }
            Instr::IfStatement(data) => {
                let condition = self.format_operand_idx(data.condition);
                let then_branch = self.format_operand_idx(data.then_branch);
                if let Some(else_idx) = data.else_branch {
                    let else_branch = self.format_operand_idx(else_idx);
                    format!("if {} then {} else {}", condition, then_branch, else_branch)
                } else {
                    format!("if {} then {}", condition, then_branch)
                }
            }
            Instr::WhileLoop(data) => {
                let condition = self.format_operand_idx(data.condition);
                let body = self.format_operand_idx(data.body);
                format!("while {} do {}", condition, body)
            }
            Instr::ForLoop(data) => {
                let iterator = data.iterator.clone();
                let range = self.format_operand_idx(data.range);
                let body = self.format_operand_idx(data.body);
                format!("for {} in {} do {}", iterator, range, body)
            }
            Instr::Return(data) => {
                if let Some(value_idx) = data.value {
                    let value = self.format_operand_idx(value_idx);
                    format!("ret_node({})", value)
                } else {
                    format!("ret_node()")
                }
            }
            Instr::Region(data) => {
                let mut parts = Vec::new();

                // Show instructions on separate lines with indentation
                if data.instr_start != data.instr_end {
                    let instrs: Vec<String> = (data.instr_start..data.instr_end)
                        .map(|i| {
                            let formatted = self.format_instruction(i);
                            // Indent all lines of multi-line instructions
                            formatted
                                .lines()
                                .map(|line| format!("    {}", line))
                                .collect::<Vec<_>>()
                                .join("\n")
                        })
                        .collect();
                    parts.push(format!("    {}", instrs.join("\n")));
                }

                // Show return location with just the index
                parts.push(format!("    ret: %{}", data.return_loc));

                format!("region {{\n{}\n}}", parts.join("\n"))
            }
            Instr::StructDecl(data) => {
                format!("struct {} {{}}", data.name)
            }
            Instr::EnumDecl(data) => {
                let variants_str: Vec<String> = data
                    .variants
                    .iter()
                    .map(|v| {
                        if v.fields.is_empty() {
                            v.name.clone()
                        } else {
                            let fields_str: Vec<String> = v
                                .fields
                                .iter()
                                .map(|(n, t)| format!("{}::{}", n, t.name()))
                                .collect();
                            format!("{}({})", v.name, fields_str.join(", "))
                        }
                    })
                    .collect();
                format!("enum {} {{ {} }}", data.name, variants_str.join(", "))
            }
            Instr::EnumVariantAccess(data) => {
                format!("{}::{}", data.enum_name, data.variant)
            }
            Instr::EnumVariantConstruct(data) => {
                let args_str: Vec<String> =
                    data.arg_indices.iter().map(|i| format!("%{}", i)).collect();
                format!(
                    "{}::{}({})",
                    data.enum_name,
                    data.variant,
                    args_str.join(", ")
                )
            }
            Instr::Match(data) => {
                let arms_str: Vec<String> = data
                    .arms
                    .iter()
                    .map(|arm| {
                        let tag_s = arm.tag.map_or("_".to_string(), |t| t.to_string());
                        format!("{} [{}] => %{}", tag_s, arm.bindings.join(","), arm.body)
                    })
                    .collect();
                format!("match %{} {{ {} }}", data.scrutinee, arms_str.join("; "))
            }
            Instr::FuncDecl(data) => {
                let params: Vec<String> =
                    data.parameters.iter().map(|p| p.id.name.clone()).collect();
                let body = match self.get_instruction(data.body_index) {
                    Some(body_instr) => self.format_instr_with_context(body_instr),
                    None => format!("%{} <invalid>", data.body_index),
                };
                // Handle multi-line body formatting - indent all lines after the first
                let body_lines: Vec<&str> = body.lines().collect();
                let formatted_body = if body_lines.len() > 1 {
                    let first_line = body_lines[0];
                    let remaining_lines: Vec<String> = body_lines[1..]
                        .iter()
                        .map(|line| format!("    {}", line)) // Indent continuation lines
                        .collect();
                    format!("{}\n{}", first_line, remaining_lines.join("\n"))
                } else {
                    body
                };
                format!(
                    "func {} [{}] {}",
                    data.name,
                    params.join(", "),
                    formatted_body
                )
            }
            Instr::VarDecl(data) => {
                format!("var {} = %{}", data.name, data.value)
            }
            Instr::Typeof(operand) => {
                let operand_str = self.format_operand_idx(*operand);
                format!("typeof {}", operand_str)
            }
            Instr::New(_)
            | Instr::Printf(_)
            | Instr::MemLoad(_)
            | Instr::MemStore(_)
            | Instr::GetField(_)
            | Instr::Ffi(_)
            | Instr::AsPtr(_)
            | Instr::AsString(_) => {
                let (name, arguments) = instr.as_builtin().unwrap();
                let args: Vec<String> = arguments
                    .iter()
                    .map(|&arg_idx| self.format_operand_idx(arg_idx))
                    .collect();
                format!("builtin {}({})", name, args.join(", "))
            }
        }
    }

    /// Returns a formatted string representation of all instructions
    pub fn format_all(&self) -> String {
        let mut output = String::new();
        for (index, _instr) in self.instructions.iter().enumerate() {
            output.push_str(&format!(
                "{}\n",
                self.format_instruction(index as InstrIndex)
            ));
        }

        output.push_str("\nMain:\n");
        output.push_str("=====\n");
        output.push_str(&format!("{}\n", self.format_instr_with_context(&self.main)));

        output
    }

    pub fn format_main(&self) -> String {
        self.format_instr_with_context(&self.main)
    }
}

// ─── Instruction & operand data definitions ──────────────────────────────────

pub type InstrIndex = i64;
pub type DeclIndex = i64;

#[derive(Debug, Clone, PartialEq)]
pub enum Instr {
    Literal(LiteralPl),
    VarRef(VarRefData),
    UnOp(UnOpPl),
    BinOp(BinOpPl),
    FnCall(FnCallPl),
    IfStatement(IfStatementData),
    WhileLoop(WhileLoopData),
    ForLoop(ForLoopData),
    Return(ReturnData),
    Region(RegionData),
    StructDecl(StructData),
    EnumDecl(EnumData),
    EnumVariantAccess(EnumVariantData),
    EnumVariantConstruct(EnumVariantConstructData),
    Match(MatchData),
    FuncDecl(FuncData),
    VarDecl(VarDeclData),

    // ── Builtin operations ───────────────────────────────────────────────
    // Compiler intrinsics, each with a dedicated emitter in `sir_lowering` and
    // dispatched directly in the central instruction match like any other
    // variant (rather than via a separate enum). `Typeof` is the only one
    // recognized during FIR generation, because it must be analyzed; the
    // rest are introduced during SIR generation by rewriting the matching
    // `FnCall`s, so FIR and analysis only ever see the original `FnCall`.
    /// `typeof(operand)`: yields a `String` naming the operand's resolved type.
    /// The operand is type-checked during analysis; SIR generation rewrites
    /// this into a `Literal(Symbol(_))` once the type is known.
    Typeof(InstrIndex),
    /// `new(:Type, field0, field1, ...)` — allocate and initialize a struct.
    New(Vec<InstrIndex>),
    /// `printf(fmt, args...)` — variadic C `printf`.
    Printf(Vec<InstrIndex>),
    /// `_mem_load(ptr, offset)` — load an `i64` from `ptr + offset`.
    MemLoad(Vec<InstrIndex>),
    /// `_mem_store(ptr, offset, value)` — store `value` at `ptr + offset`.
    MemStore(Vec<InstrIndex>),
    /// `getfield(struct, :field, :Type)` — load a struct field by name.
    GetField(Vec<InstrIndex>),
    /// `ffi(:name, args...)` — call a foreign function by name.
    Ffi(Vec<InstrIndex>),
    /// `as_ptr(x)` — identity reinterpretation cast to a pointer.
    AsPtr(Vec<InstrIndex>),
    /// `as_string(x)` — identity reinterpretation cast to a string.
    AsString(Vec<InstrIndex>),
}

impl Instr {
    /// If `name` is a compiler intrinsic, build the corresponding builtin
    /// instruction from the call's `arguments`; otherwise `None` (a user or
    /// foreign call). This is the single place that classifies call names as
    /// builtins, used by SIR generation to rewrite `FnCall`s.
    pub fn builtin_from_call(name: &str, arguments: &[InstrIndex]) -> Option<Instr> {
        let args = || arguments.to_vec();
        Some(match name {
            "new" => Instr::New(args()),
            "printf" => Instr::Printf(args()),
            "_mem_load" => Instr::MemLoad(args()),
            "_mem_store" => Instr::MemStore(args()),
            "getfield" => Instr::GetField(args()),
            "ffi" => Instr::Ffi(args()),
            "as_ptr" => Instr::AsPtr(args()),
            "as_string" => Instr::AsString(args()),
            _ => return None,
        })
    }

    /// If this is a builtin operation (excluding `Typeof`), return its
    /// source-level name and argument operand indices.
    pub fn as_builtin(&self) -> Option<(&'static str, &[InstrIndex])> {
        match self {
            Instr::New(a) => Some(("new", a)),
            Instr::Printf(a) => Some(("printf", a)),
            Instr::MemLoad(a) => Some(("_mem_load", a)),
            Instr::MemStore(a) => Some(("_mem_store", a)),
            Instr::GetField(a) => Some(("getfield", a)),
            Instr::Ffi(a) => Some(("ffi", a)),
            Instr::AsPtr(a) => Some(("as_ptr", a)),
            Instr::AsString(a) => Some(("as_string", a)),
            _ => None,
        }
    }

    /// True if this instruction is (or, like the historical builtin `FnCall`s,
    /// is treated as) a function call for non-leaf detection.
    pub fn is_call_like(&self) -> bool {
        matches!(self, Instr::FnCall(_)) || self.as_builtin().is_some()
    }
}

//
// Declaration Values
//

#[derive(Debug, Clone, PartialEq)]
pub struct VarDeclData {
    pub name: String,
    pub value: InstrIndex,
    /// LHS type annotation from the source assignment, if any.
    pub declared_type: Option<crate::frontend::ast::TypeExpression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncData {
    pub name: String,
    /// Type parameter names declared on the function, e.g. `fn f<T>(...)`.
    /// Treated as type variables within the signature and body; used to detect
    /// generic functions for monomorphization.
    pub type_params: Vec<String>,
    pub parameters: Vec<Parameter>,
    /// Implicit-context bindings declared in `[...]` after the parameter list.
    /// Carried through the pipeline for later context-system lowering; not yet
    /// consumed by analysis or codegen.
    pub context_params: Vec<Parameter>,
    pub body_index: i64, // index into the instruction vector
    pub return_type: Option<TypeExpression>,
    pub is_foreign: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructData {
    pub name: String,
    pub fields: Vec<FieldDefinition>,
    pub type_params: Vec<Id>,
}

/// Metadata for a single enum variant, including optional associated field types.
///
/// NOTE: Tuple variants are lowered into ordinary fields auto-named `_0`, `_1`, …
/// because the language has no first-class tuple type yet.  When
/// `TypeExpression::Tuple` is added, consider whether this struct should instead
/// carry a dedicated tuple-type field rather than the current auto-name convention.
#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariantInfo {
    pub name: String,
    /// Associated fields in declaration order.  Empty for unit variants.
    /// Tuple variants use auto-names `_0`, `_1`, …; struct variants use user names.
    /// (Auto-names are a stand-in until first-class tuple types exist.)
    pub fields: Vec<(String, TypeExpression)>,
}

/// Metadata for an enum declaration.
/// Variants are ordered; their index is their integer tag value.
#[derive(Debug, Clone, PartialEq)]
pub struct EnumData {
    pub name: String,
    pub variants: Vec<EnumVariantInfo>,
    pub type_params: Vec<Id>,
}

/// A symbolic reference to a unit variant of a named enum.
/// The integer tag is not stored here — it is resolved at codegen time from
/// the `SIRModule.enums` layout so that type information survives through analysis.
#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariantData {
    pub enum_name: String,
    pub variant: String,
}

/// Construction of a data-carrying enum variant, e.g. `Message::Move(1, 2)`.
/// `arg_indices` are the FIR indices of the compiled argument values, in
/// declaration order.
#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariantConstructData {
    pub enum_name: String,
    pub variant: String,
    pub arg_indices: Vec<InstrIndex>,
}

/// A single arm of a `match` expression.
#[derive(Debug, Clone, PartialEq)]
pub struct MatchArmData {
    /// Integer tag for the variant, or `None` for a wildcard arm.
    pub tag: Option<i64>,
    /// Positional variable names bound to extracted fields; empty for unit variants.
    pub bindings: Vec<String>,
    /// FIR index of the compiled arm body.
    pub body: InstrIndex,
}

/// A `match` expression over an enum value.
#[derive(Debug, Clone, PartialEq)]
pub struct MatchData {
    pub scrutinee: InstrIndex,
    /// Name of the enum being matched, used for layout lookup at codegen time.
    pub enum_name: String,
    pub arms: Vec<MatchArmData>,
}

//
// Instruction Values
//

#[derive(Debug, Clone, PartialEq)]
pub struct VarRefData {
    pub name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralPl {
    Int(i64),
    Float(f64),
    Boolean(bool),
    String(String),
    Symbol(String),
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnOpPl {
    pub op: UnaryOperator,
    pub operand: InstrIndex,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinOpPl {
    pub op: BinaryOperator,
    pub left: InstrIndex,
    pub right: InstrIndex,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnCallPl {
    pub name: String,
    pub arguments: Vec<InstrIndex>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStatementData {
    pub condition: InstrIndex,
    pub then_branch: InstrIndex,
    pub else_branch: Option<InstrIndex>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileLoopData {
    pub condition: InstrIndex,
    pub body: InstrIndex,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForLoopData {
    pub iterator: String,
    pub range: InstrIndex,
    pub body: InstrIndex,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RegionData {
    // _start and _end define a slice into the instruction and declaration vectors
    pub instr_start: InstrIndex,
    pub instr_end: InstrIndex,
    pub return_loc: InstrIndex,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnData {
    pub value: Option<InstrIndex>,
}

// Display implementations for pretty printing

// ─── Display implementations ──────────────────────────────────────────────────

impl Display for LiteralPl {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            LiteralPl::Int(n) => write!(f, "{}", n),
            LiteralPl::Float(fl) => write!(f, "{}", fl),
            LiteralPl::Boolean(b) => write!(f, "{}", b),
            LiteralPl::String(s) => write!(f, "\"{}\"", s),
            LiteralPl::Symbol(s) => write!(f, ":{}", s),
            LiteralPl::Unit => write!(f, "()"),
        }
    }
}

impl Display for Instr {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match &self {
            Instr::Literal(data) => Display::fmt(&data, f),
            Instr::VarRef(data) => write!(f, "varref {}", data.name),
            Instr::UnOp(data) => write!(f, "unop {:?} {}", data.op, data.operand),
            Instr::BinOp(data) => write!(f, "binop {:?} {}, {}", data.op, data.left, data.right),
            Instr::FnCall(data) => {
                let args: Vec<String> = data.arguments.iter().map(|arg| arg.to_string()).collect();
                write!(f, "call {}({})", data.name, args.join(", "))
            }
            Instr::IfStatement(data) => {
                if let Some(else_branch) = &data.else_branch {
                    write!(
                        f,
                        "if {} then {} else {}",
                        data.condition, data.then_branch, else_branch
                    )
                } else {
                    write!(f, "if {} then {}", data.condition, data.then_branch)
                }
            }
            Instr::WhileLoop(data) => write!(f, "while {} do {}", data.condition, data.body),
            Instr::ForLoop(data) => write!(
                f,
                "for {} in {} do {}",
                data.iterator, data.range, data.body
            ),
            Instr::Return(data) => {
                if let Some(value) = &data.value {
                    write!(f, "ret_node({})", value)
                } else {
                    write!(f, "ret_node()")
                }
            }
            Instr::Region(data) => {
                write!(
                    f,
                    "region[{}..{}, ret:{}]",
                    data.instr_start, data.instr_end, data.return_loc
                )
            }
            Instr::StructDecl(data) => {
                let fields: Vec<String> = data
                    .fields
                    .iter()
                    .map(|field| field.id.name.clone())
                    .collect();
                write!(f, "struct {} {{ {} }}", data.name, fields.join(", "))
            }
            Instr::EnumDecl(data) => {
                let variants_str: Vec<String> =
                    data.variants.iter().map(|v| v.name.clone()).collect();
                write!(f, "enum {} {{ {} }}", data.name, variants_str.join(", "))
            }
            Instr::EnumVariantAccess(data) => {
                write!(f, "enum_variant {}::{}", data.enum_name, data.variant)
            }
            Instr::EnumVariantConstruct(data) => {
                let args_str: Vec<String> =
                    data.arg_indices.iter().map(|i| format!("%{}", i)).collect();
                write!(
                    f,
                    "enum_construct {}::{}({})",
                    data.enum_name,
                    data.variant,
                    args_str.join(", ")
                )
            }
            Instr::Match(data) => {
                write!(f, "match %{} [{}]", data.scrutinee, data.arms.len())
            }
            Instr::FuncDecl(data) => {
                let params: Vec<String> =
                    data.parameters.iter().map(|p| p.id.name.clone()).collect();
                write!(
                    f,
                    "func {} [{}] {}",
                    data.name,
                    params.join(", "),
                    data.body_index
                )
            }
            Instr::VarDecl(data) => {
                write!(f, "var {} = {}", data.name, data.value)
            }
            Instr::Typeof(operand) => write!(f, "typeof {}", operand),
            Instr::New(_)
            | Instr::Printf(_)
            | Instr::MemLoad(_)
            | Instr::MemStore(_)
            | Instr::GetField(_)
            | Instr::Ffi(_)
            | Instr::AsPtr(_)
            | Instr::AsString(_) => {
                let (name, arguments) = self.as_builtin().unwrap();
                let args: Vec<String> = arguments.iter().map(|arg| arg.to_string()).collect();
                write!(f, "builtin {}({})", name, args.join(", "))
            }
        }
    }
}
