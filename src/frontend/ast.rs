#[derive(Debug)]
pub enum Expression {
    Number(f64),
    Print(Box<Expression>),
    // Add more expression types as needed
}

#[derive(Debug)]
pub struct Program {
    pub expressions: Vec<Expression>,
} 