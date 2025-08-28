//use crate::frontend::lexer::TokenType;

#[derive(Debug, Clone)]
pub enum Expression {
    Int(i64),
    Float(f64),
    Boolean(bool),
    String(String),
    Symbol(String),
    Identifier {
        id: Id,
        type_expr: Option<TypeExpression>,
    },
    UnaryOp {
        operator: UnaryOperator,
        operand: Box<Expression>,
    },
    BinaryOp {
        operator: BinaryOperator,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    FunctionCall {
        // TODO: Change to expression to allow functions to return functions
        // e.g. should be able to call `my_fn()()` which currently fails
        id: Id, 
        arguments: Vec<Expression>,
    },
    FunctionDefinition {
        id: Id,
        parameters: Vec<Parameter>,
        body: Box<Expression>,
        return_type_expr: Option<TypeExpression>,
    },
    StructDefinition {
        id: Id,
        fields: Vec<FieldDefinition>,
    },
    If {
        condition: Box<Expression>,
        then_branch: Box<Expression>,
        else_branch: Option<Box<Expression>>,
        //elseif_branches: Box<If>
    },
    While {
        condition: Box<Expression>,
        body: Box<Expression>,
    },
    For {
        iterator: Id, // TODO?: Accept Vec<Id> for unpacking
        range: Box<Expression>,
        body: Box<Expression>,
    },
    Block(Vec<Box<Expression>>),
    Return(Option<Box<Expression>>),
    Print(Box<Expression>),
    /*Match {
        value: Box<Expression>,
        cases: Vec<MatchCase>,
    },*/
    /* This should just be a binary operation
    VariableAssignment {
        id: Id,
        value: Box<Expression>,
    },
    */
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Id {
    pub name: String,
}

/*pub struct Block {
    pub expressions: Vec<Expression>,
}*/

#[derive(Debug, Clone)]
pub struct Program {
    pub expressions: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Not,
    Negate,
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Power,
    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    And,
    Ampersand,
    Or,
    Pipe,
    Dot,
    Assignment,
}

// Parameters can optionally have a type
#[derive(Debug, Clone)]
pub struct Parameter {
    pub id: Id,
    pub type_expr: Option<TypeExpression>,
}

// Fields must have a type
#[derive(Debug, Clone)]
pub struct FieldDefinition {
    pub id: Id,
    pub field_type: TypeExpression,
}

/*
#[derive(Debug, Clone)]
pub struct MatchCase {
    pub pattern: Box<Expression>,
    pub body: Box<Expression>,
}*/


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeExpression {
    Int,
    Float,
    Bool,
    String,
    Void,
    Struct(Id, Option<Vec<TypeExpression>>),
}

impl TypeExpression {
    pub fn name(&self) -> String {
        match self {
            TypeExpression::Int => "Int".to_string(),
            TypeExpression::Float => "Float".to_string(),
            TypeExpression::Bool => "Bool".to_string(),
            TypeExpression::String => "String".to_string(),
            TypeExpression::Void => "Void".to_string(),
            TypeExpression::Struct(id, _) => id.name.clone().to_string(),
        }
    }
}


use std::fmt;

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Int(n) => write!(f, "{}::Int", n),
            Expression::Float(n) => write!(f, "{}::Float", n),
            Expression::Boolean(b) => write!(f, "{}::Bool", b),
            Expression::String(s) => write!(f, "\"{}\"::String", s),
            Expression::Symbol(s) => write!(f, ":{}", s),
            Expression::Identifier { id, type_expr } => match type_expr {
                Some(type_expr) => write!(f, "{}::{}", id.name, type_expr.name()),
                None => write!(f, "{}::Any", id.name),
            },
            Expression::UnaryOp { operator, operand } => write!(f, "({:?} {})", operator, operand),
            Expression::BinaryOp { operator, left, right } => write!(f, "({} {:?} {})", left, operator, right),
            Expression::FunctionCall { id, arguments } => {
                let args: Vec<String> = arguments.iter().map(|arg| format!("{}", arg)).collect();
                write!(f, "{}({})", id.name, args.join(", "))
            },
            Expression::FunctionDefinition { id, parameters, body, return_type_expr } => {
                let params: Vec<String> = parameters.iter().map(|param| format!("{}", param)).collect();
                let body_str = format!("{}", *body);
                match return_type_expr {
                    Some(return_type_expr) => write!(f, "fn {}({}) -> {} {{ {} }}", id.name, params.join(", "), return_type_expr.name(), body_str),
                    None => write!(f, "fn {}({}) {{ {} }}", id.name, params.join(", "), body_str),
                }
            },
            Expression::StructDefinition { id, fields } => {
                let fields_str: Vec<String> = fields.iter().map(|field| format!("{}", field.id.name)).collect();
                write!(f, "struct {} {{ {} }}", id.name, fields_str.join(", "))
            },
            Expression::If { condition, then_branch, else_branch } => {
                let else_str = if let Some(else_branch) = else_branch {
                    format!(" else {}", else_branch)
                } else {
                    String::new()
                };
                write!(f, "if {} {{ {} }}{}", condition, then_branch, else_str)?;
                Ok(())
            },
            Expression::While { condition, body } => write!(f, "while {} {{ {} }}", condition, body),
            Expression::For { iterator, range, body } => write!(f, "for {} in {} {{ {} }}", iterator.name, range, body),
            Expression::Block(expressions) => {
                let block_str: Vec<String> = expressions.iter().map(|expr| format!("{}", expr)).collect();
                write!(f, "{{ {} }}", block_str.join("; "))
            },
            Expression::Return(expr) => write!(f, "return {}", expr.as_ref().map_or("".to_string(), |e| format!("{}", e))),
            Expression::Print(expr) => write!(f, "print({})", expr),
            /*Expression::Match { value, cases } => {
                let cases_str: Vec<String> = cases.iter().map(|case| format!("{}", case)).collect();
                write!(f, "match {} {{ {} }}", value, cases_str.join(", "))
            },*/
            //Expression::VariableAssignment { id, value } => write!(f, "{} = {}", id.name, value),
        }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let expressions_str: Vec<String> = self.expressions.iter().map(|expr| format!("{}", expr)).collect();
        write!(f, "{{ {} }}", expressions_str.join("; "))
    }
}

impl fmt::Display for Parameter {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.type_expr {
            Some(type_expr) => write!(f, "{}::{}", self.id.name, type_expr.name()),
            None => write!(f, "{}::Any", self.id.name),
        }
    }
}

impl fmt::Display for FieldDefinition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}::{}", self.id.name, self.field_type.name())
        //write!(f, "{}: {:?}", self.id.name, self.type_expr)
    }
}

/*impl fmt::Display for MatchCase {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} => {}", self.pattern, self.body)
    }
}*/

impl fmt::Display for TypeExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeExpression::Int => write!(f, "Int"),
            TypeExpression::Float => write!(f, "Float"),
            TypeExpression::Bool => write!(f, "Bool"),
            TypeExpression::String => write!(f, "String"),
            TypeExpression::Void => write!(f, "Void"),
            TypeExpression::Struct(id, _) => write!(f, "{}", id.name),
        }
    }
} 