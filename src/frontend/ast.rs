#[derive(Debug)]
pub enum Expression {
    Number(f64),
    Boolean(bool),
    String(String),
    Identifier(String),
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
        name: Identifier,
        arguments: Vec<Expression>,
    },
    FunctionDefinition {
        name: Identifier,
        parameters: Vec<Parameter>,
        body: Vec<Expression>,
    },
    StructDefinition {
        name: Identifier,
        fields: Vec<FieldDefinition>,
    },
    If {
        condition: Box<Expression>,
        then_branch: Box<Expression>,
        else_branch: Option<Box<Expression>>,
        //elseif_branches: Vec<(Box<Expression>, Box<Expression>)>,
    },
    For {
        iterator: Identifier,
        range: Box<Expression>,
        body: Box<Expression>,
    },
    Block(Vec<Expression>),
    Return(Option<Box<Expression>>),
    Print(Box<Expression>),
    /*Match {
        value: Box<Expression>,
        cases: Vec<MatchCase>,
    },*/
    VariableAssignment {
        name: Identifier,
        value: Box<Expression>,
    },
}

#[derive(Debug)]
pub struct Program {
    pub expressions: Vec<Expression>,
}

#[derive(Debug)]
pub enum UnaryOperator {
    Not,
    Negate,
}

#[derive(Debug)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Equal,
    NotEqual,
    And,
    Or,
}

#[derive(Debug)]
pub struct Parameter {
    pub name: Identifier,
    pub type_expr: Option<TypeExpression>,
}

#[derive(Debug)]
pub struct FieldDefinition {
    pub name: Identifier,
    pub type_expr: TypeExpression,
}

#[derive(Debug)]
pub struct MatchCase {
    pub pattern: Box<Expression>,
    pub body: Box<Expression>,
}

#[derive(Debug)]
pub enum TypeExpression {
    Int,
    Bool,
    Void,
    Struct(Identifier, Option<Box<TypeExpression>>),
}

use std::fmt;

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Number(n) => write!(f, "{}", n),
            Expression::Boolean(b) => write!(f, "{}", b),
            Expression::String(s) => write!(f, "\"{}\"", s),
            Expression::Identifier(id) => write!(f, "{}", id),
            Expression::UnaryOp { operator, operand } => write!(f, "({:?} {})", operator, operand),
            Expression::BinaryOp { operator, left, right } => write!(f, "({} {:?} {})", left, operator, right),
            Expression::FunctionCall { name, arguments } => {
                let args: Vec<String> = arguments.iter().map(|arg| format!("{}", arg)).collect();
                write!(f, "{}({})", name, args.join(", "))
            },
            Expression::FunctionDefinition { name, parameters, body } => {
                let params: Vec<String> = parameters.iter().map(|param| format!("{}", param.name)).collect();
                let body_str: Vec<String> = body.iter().map(|expr| format!("{}", expr)).collect();
                write!(f, "fn {}({}) {{ {} }}", name, params.join(", "), body_str.join("; "))
            },
            Expression::StructDefinition { name, fields } => {
                let fields_str: Vec<String> = fields.iter().map(|field| format!("{}", field.name)).collect();
                write!(f, "struct {} {{ {} }}", name, fields_str.join(", "))
            },
            Expression::If { condition, then_branch, else_branch, elseif_branches } => {
                let else_str = if let Some(else_branch) = else_branch {
                    format!(" else {}", else_branch)
                } else {
                    String::new()
                };
                let elseif_str: Vec<String> = elseif_branches.iter().map(|(cond, branch)| format!(" elseif {} {{ {} }}", cond, branch)).collect();
                write!(f, "if {} {{ {} }}{}", condition, then_branch, else_str)?;
                for elseif in elseif_str {
                    write!(f, "{}", elseif)?;
                }
                Ok(())
            },
            Expression::For { iterator, range, body } => write!(f, "for {} in {} {{ {} }}", iterator, range, body),
            Expression::Block(expressions) => {
                let block_str: Vec<String> = expressions.iter().map(|expr| format!("{}", expr)).collect();
                write!(f, "{{ {} }}", block_str.join("; "))
            },
            Expression::Return(expr) => write!(f, "return {}", expr.as_ref().map_or("".to_string(), |e| format!("{}", e))),
            Expression::Print(expr) => write!(f, "print({})", expr),
            Expression::Match { value, cases } => {
                let cases_str: Vec<String> = cases.iter().map(|case| format!("{}", case)).collect();
                write!(f, "match {} {{ {} }}", value, cases_str.join(", "))
            },
            Expression::VariableAssignment { name, value } => write!(f, "{} = {}", name, value),
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
        write!(f, "{}: {:?}", self.name, self.type_expr)
    }
}

impl fmt::Display for FieldDefinition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {:?}", self.name, self.type_expr)
    }
}

impl fmt::Display for MatchCase {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} => {}", self.pattern, self.body)
    }
}

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