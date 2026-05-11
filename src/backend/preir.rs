use crate::frontend::ast::{
    BinaryOperator, FieldDefinition, Id, Parameter, TypeExpression, UnaryOperator,
};
use std::fmt::{Display, Formatter, Result as FmtResult};

#[derive(Debug, Clone)]
pub struct PreIR {
    pub main: Instr,
    pub instructions: Vec<Instr>,
}

impl PreIR {
    /// Parenthesized recursive formatting for an operand index, or `%idx` if missing.
    fn format_operand_idx(&self, idx: instr_index) -> String {
        match self.get_instruction(idx) {
            Some(instr) => format!("({})", self.format_instr_with_context(instr)),
            None => format!("%{}", idx),
        }
    }

    /// Creates a new `PreIR` instance with an empty `Region` instruction as its main body.
    pub fn init() -> Self {
        PreIR {
            main: Instr::Region(RegionData {
                instr_start: 0,
                instr_end: 0,
                return_loc: 0,
            }),
            instructions: Vec::new(),
        }
    }

    pub fn emit_instruction(&mut self, instr: Instr) -> instr_index {
        let index = self.instructions.len() as instr_index;
        self.instructions.push(instr);
        index
    }

    /// Get an instruction by index, returning None if out of bounds
    pub fn get_instruction(&self, index: instr_index) -> Option<&Instr> {
        if index >= 0 && (index as usize) < self.instructions.len() {
            Some(&self.instructions[index as usize])
        } else {
            None
        }
    }

    /// Format an instruction with context
    pub fn format_instruction(&self, index: instr_index) -> String {
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
        }
    }

    /// Returns a formatted string representation of all instructions
    pub fn format_all(&self) -> String {
        let mut output = String::new();
        for (index, _instr) in self.instructions.iter().enumerate() {
            output.push_str(&format!(
                "{}\n",
                self.format_instruction(index as instr_index)
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

pub type instr_index = i64;
pub type decl_index = i64;

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
    /// Built-in `typeof(operand)` instruction. The operand is type-checked
    /// during analysis; the instruction itself yields a `String` whose value
    /// is the resolved (possibly mangled) type name of the operand.
    /// SIR generation rewrites this into a `Literal(Symbol(_))` once the
    /// operand's type is known.
    Typeof(instr_index),
}

//
// Declaration Values
//

#[derive(Debug, Clone, PartialEq)]
pub struct VarDeclData {
    pub name: String,
    pub value: instr_index,
    /// LHS type annotation from the source assignment, if any.
    pub declared_type: Option<crate::frontend::ast::TypeExpression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncData {
    pub name: String,
    pub parameters: Vec<Parameter>,
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
/// `arg_indices` are the PreIR indices of the compiled argument values, in
/// declaration order.
#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariantConstructData {
    pub enum_name: String,
    pub variant: String,
    pub arg_indices: Vec<instr_index>,
}

/// A single arm of a `match` expression.
#[derive(Debug, Clone, PartialEq)]
pub struct MatchArmData {
    /// Integer tag for the variant, or `None` for a wildcard arm.
    pub tag: Option<i64>,
    /// Positional variable names bound to extracted fields; empty for unit variants.
    pub bindings: Vec<String>,
    /// PreIR index of the compiled arm body.
    pub body: instr_index,
}

/// A `match` expression over an enum value.
#[derive(Debug, Clone, PartialEq)]
pub struct MatchData {
    pub scrutinee: instr_index,
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
    pub operand: instr_index,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinOpPl {
    pub op: BinaryOperator,
    pub left: instr_index,
    pub right: instr_index,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnCallPl {
    pub name: String,
    pub arguments: Vec<instr_index>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStatementData {
    pub condition: instr_index,
    pub then_branch: instr_index,
    pub else_branch: Option<instr_index>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileLoopData {
    pub condition: instr_index,
    pub body: instr_index,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForLoopData {
    pub iterator: String,
    pub range: instr_index,
    pub body: instr_index,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RegionData {
    // _start and _end define a slice into the instruction and declaration vectors
    pub instr_start: instr_index,
    pub instr_end: instr_index,
    pub return_loc: instr_index,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnData {
    pub value: Option<instr_index>,
}

// Display implementations for pretty printing

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
        }
    }
}
