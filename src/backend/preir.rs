


use crate::{backend::interpreter, frontend::ast::{BinaryOperator, FieldDefinition, Parameter, TypeExpression, UnaryOperator}};
use std::fmt::{Display, Formatter, Result as FmtResult};


#[derive(Debug, Clone)]
pub struct PreIR {
    pub main: Instr,
    pub instructions: Vec<Instr>,
}

impl PreIR {
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
                match instr {
                    Instr::VarDecl(data) => format!("    {}", self.format_instr_with_context(instr)),
                    Instr::FuncDecl(data) => format!("    {}", self.format_instr_with_context(instr)),
                    Instr::StructDecl(data) => format!("    {}", self.format_instr_with_context(instr)),
                    _ => format!("%{} = {}", index, self.format_instr_with_context(instr)),
                } 
            },
            None => format!("%{} = <invalid>", index),
        }
    }


    /// Format an instruction with contextual information (resolving indices)
    pub fn format_instr_with_context(&self, instr: &Instr) -> String {
        match instr {
            Instr::Literal(data) => format!("{}", data),
            Instr::VarRef(data) => format!("{}", data.name),
            Instr::UnOp(data) => {
                let operand = match self.get_instruction(data.operand) {
                    Some(op_instr) => format!("({})", self.format_instr_with_context(op_instr)),
                    None => format!("%{}", data.operand),
                };
                format!("unop {:?} {}", data.op, operand)
            },
            Instr::BinOp(data) => {
                format!("binop {:?} %{} %{}", data.op, data.left, data.right)
            },
            Instr::FnCall(data) => {
                let args: Vec<String> = data.arguments.iter().map(|&arg_idx| {
                    match self.get_instruction(arg_idx) {
                        Some(arg_instr) => format!("({})", self.format_instr_with_context(arg_instr)),
                        None => format!("%{}", arg_idx),
                    }
                }).collect();
                format!("call {}({})", data.name, args.join(", "))
            },
            Instr::IfStatement(data) => {
                let condition = match self.get_instruction(data.condition) {
                    Some(cond_instr) => format!("({})", self.format_instr_with_context(cond_instr)),
                    None => format!("%{}", data.condition),
                };
                let then_branch = match self.get_instruction(data.then_branch) {
                    Some(then_instr) => format!("({})", self.format_instr_with_context(then_instr)),
                    None => format!("%{}", data.then_branch),
                };
                if let Some(else_idx) = data.else_branch {
                    let else_branch = match self.get_instruction(else_idx) {
                        Some(else_instr) => format!("({})", self.format_instr_with_context(else_instr)),
                        None => format!("%{}", else_idx),
                    };
                    format!("if {} then {} else {}", condition, then_branch, else_branch)
                } else {
                    format!("if {} then {}", condition, then_branch)
                }
            },
            Instr::WhileLoop(data) => {
                let condition = match self.get_instruction(data.condition) {
                    Some(cond_instr) => format!("({})", self.format_instr_with_context(cond_instr)),
                    None => format!("%{}", data.condition),
                };
                let body = match self.get_instruction(data.body) {
                    Some(body_instr) => format!("({})", self.format_instr_with_context(body_instr)),
                    None => format!("%{}", data.body),
                };
                format!("while {} do {}", condition, body)
            },
            Instr::ForLoop(data) => {
                let iterator = data.iterator.clone();
                let range = match self.get_instruction(data.range) {
                    Some(range_instr) => format!("({})", self.format_instr_with_context(range_instr)),
                    None => format!("%{}", data.range),
                };
                let body = match self.get_instruction(data.body) {
                    Some(body_instr) => format!("({})", self.format_instr_with_context(body_instr)),
                    None => format!("%{}", data.body),
                };
                format!("for {} in {} do {}", iterator, range, body)
            },
            Instr::Return(data) => {
                if let Some(value_idx) = data.value {
                    let value = match self.get_instruction(value_idx) {
                        Some(val_instr) => format!("({})", self.format_instr_with_context(val_instr)),
                        None => format!("%{}", value_idx),
                    };
                    format!("ret_node({})", value)
                } else {
                    format!("ret_node()")
                }
            },
            Instr::Region(data) => {
                let mut parts = Vec::new();
                
                // Show instructions on separate lines with indentation
                if data.instr_start != data.instr_end {
                    let instrs: Vec<String> = (data.instr_start..data.instr_end)
                        .map(|i| {
                            let formatted = self.format_instruction(i);
                            // Indent all lines of multi-line instructions
                            formatted.lines()
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
            },
            Instr::StructDecl(data) => {
                format!("struct {} {{}}", data.name)
            },
            Instr::FuncDecl(data) => {
                let params: Vec<String> = data.parameters.iter().map(|p| p.id.name.clone()).collect();
                let body = self.format_instr_with_context(&self.get_instruction(data.body_index).unwrap());
                // Handle multi-line body formatting - indent all lines after the first
                let body_lines: Vec<&str> = body.lines().collect();
                let formatted_body = if body_lines.len() > 1 {
                    let first_line = body_lines[0];
                    let remaining_lines: Vec<String> = body_lines[1..]
                        .iter()
                        .map(|line| format!("    {}", line))  // Indent continuation lines
                        .collect();
                    format!("{}\n{}", first_line, remaining_lines.join("\n"))
                } else {
                    body
                };
                format!("func {} [{}] {}", data.name, params.join(", "), formatted_body)
            },
            Instr::VarDecl(data) => {
                format!("var {} = %{}", data.name, data.value)
            },
        }
    }


    /// Returns a formatted string representation of all instructions
    pub fn format_all(&self) -> String {
        let mut output = String::new();
        for (index, _instr) in self.instructions.iter().enumerate() {
            output.push_str(&format!("{}\n", self.format_instruction(index as instr_index)));
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

#[derive(Debug, Clone)]
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
    FuncDecl(FuncData),
    VarDecl(VarDeclData),
}


//
// Declaration Values
//

#[derive(Debug, Clone)]
pub struct VarDeclData {
    pub name: String,
    pub value: instr_index,
}

#[derive(Debug, Clone)]
pub struct FuncData {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub body_index: i64, // index into the instruction vector
}

#[derive(Debug, Clone)]
pub struct StructData {
    pub name: String,
    pub fields: Vec<FieldDefinition>,
}

//
// Instruction Values
//

#[derive(Debug, Clone)]
pub struct VarRefData {
    pub name: String,
}

#[derive(Debug, Clone)]
pub enum LiteralPl {
    Int(i64),
    Float(f64),
    Boolean(bool),
    String(String),
    Symbol(String),
    Unit,
}

#[derive(Debug, Clone)]
pub struct UnOpPl {
    pub op: UnaryOperator,
    pub operand: instr_index,
}

#[derive(Debug, Clone)]
pub struct BinOpPl {
    pub op: BinaryOperator,
    pub left: instr_index, 
    pub right: instr_index,
}

#[derive(Debug, Clone)]
pub struct FnCallPl {
    pub name: String,
    pub arguments: Vec<instr_index>,
}

#[derive(Debug, Clone)]
pub struct IfStatementData {
    pub condition: instr_index,
    pub then_branch: instr_index,
    pub else_branch: Option<instr_index>,
}

#[derive(Debug, Clone)]
pub struct WhileLoopData {
    pub condition: instr_index,
    pub body: instr_index,
}

#[derive(Debug, Clone)]
pub struct ForLoopData {
    pub iterator: String,
    pub range: instr_index,
    pub body: instr_index,
}

#[derive(Debug, Clone)]
pub struct RegionData {
    // _start and _end define a slice into the instruction and declaration vectors
    pub instr_start: instr_index,
    pub instr_end: instr_index,
    pub return_loc: instr_index,
}

#[derive(Debug, Clone)]
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
            },
            Instr::IfStatement(data) => {
                if let Some(else_branch) = &data.else_branch {
                    write!(f, "if {} then {} else {}", data.condition, data.then_branch, else_branch)
                } else {
                    write!(f, "if {} then {}", data.condition, data.then_branch)
                }
            },
            Instr::WhileLoop(data) => write!(f, "while {} do {}", data.condition, data.body),
            Instr::ForLoop(data) => write!(f, "for {} in {} do {}", data.iterator, data.range, data.body),
            Instr::Return(data) => {
                if let Some(value) = &data.value {
                    write!(f, "ret_node({})", value)
                } else {
                    write!(f, "ret_node()")
                }
            },
            Instr::Region(data) => {
                write!(f, "region[{}..{}, ret:{}]", 
                       data.instr_start, data.instr_end, 
                       data.return_loc)
            },
            Instr::StructDecl(data) => {
                let fields: Vec<String> = data.fields.iter().map(|field| field.id.name.clone()).collect();
                write!(f, "struct {} {{ {} }}", data.name, fields.join(", "))
            },
            Instr::FuncDecl(data) => {
                let params: Vec<String> = data.parameters.iter().map(|p| p.id.name.clone()).collect();
                write!(f, "func {} [{}] {}", data.name, params.join(", "), data.body_index)
            },
            Instr::VarDecl(data) => {
                write!(f, "var {} = {}", data.name, data.value)
            },
        }
    }
}