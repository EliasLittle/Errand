//use crate::frontend::lexer::TokenType;

#[derive(Debug, Clone)]
pub enum Expression {
    Number(f64),
    Boolean(bool),
    String(String),
    Identifier(Id),
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
        id: Id,
        arguments: Vec<Expression>,
    },
    FunctionDefinition {
        id: Id,
        parameters: Vec<Parameter>,
        body: Box<Expression>,
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
    VariableAssignment {
        id: Id,
        value: Box<Expression>,
    },
}

#[derive(Debug, Clone)]
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
    Assignment,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub id: Id,
    //pub type_expr: Option<TypeExpression>,
}

#[derive(Debug, Clone)]
pub struct FieldDefinition {
    pub id: Id,
    //pub type_expr: TypeExpression,
}

/*
#[derive(Debug, Clone)]
pub struct MatchCase {
    pub pattern: Box<Expression>,
    pub body: Box<Expression>,
}*/

/*
#[derive(Debug)]
pub enum TypeExpression {
    Int,
    Bool,
    Void,
    //Struct(Identifier, Option<Box<TypeExpression>>),
}
*/

use std::fmt;

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Number(n) => write!(f, "{}", n),
            Expression::Boolean(b) => write!(f, "{}", b),
            Expression::String(s) => write!(f, "\"{}\"", s),
            Expression::Identifier(id) => write!(f, "{}", id.name),
            Expression::UnaryOp { operator, operand } => write!(f, "({:?} {})", operator, operand),
            Expression::BinaryOp { operator, left, right } => write!(f, "({} {:?} {})", left, operator, right),
            Expression::FunctionCall { id, arguments } => {
                let args: Vec<String> = arguments.iter().map(|arg| format!("{}", arg)).collect();
                write!(f, "{}({})", id.name, args.join(", "))
            },
            Expression::FunctionDefinition { id, parameters, body } => {
                let params: Vec<String> = parameters.iter().map(|param| format!("{}", param.id.name)).collect();
                let body_str = format!("{}", *body);
                write!(f, "fn {}({}) {{ {} }}", id.name, params.join(", "), body_str)
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
            Expression::VariableAssignment { id, value } => write!(f, "{} = {}", id.name, value),
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
        write!(f, "{}", self.id.name)
        //write!(f, "{}: {:?}", self.id.name, self.type_expr)
    }
}

impl fmt::Display for FieldDefinition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.id.name)
        //write!(f, "{}: {:?}", self.id.name, self.type_expr)
    }
}

/*impl fmt::Display for MatchCase {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} => {}", self.pattern, self.body)
    }
}*/

/*
impl fmt::Display for TypeExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeExpression::Int => write!(f, "Int"),
            TypeExpression::Bool => write!(f, "Bool"),
            TypeExpression::Void => write!(f, "Void"),
            TypeExpression::Struct(name, _) => write!(f, "Struct({})", name),
        }
    }
} 
*/