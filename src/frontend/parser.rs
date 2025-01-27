use super::lexer::Token;
use super::ast::{Expression, Program};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            current: 0,
        }
    }

    pub fn parse(&mut self) -> Result<Program, String> {
        // Implement parsing logic here
        // This is where you'll convert tokens into an AST
        Ok(Program {
            expressions: Vec::new(),
        })
    }
} 