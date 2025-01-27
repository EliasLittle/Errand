use crate::frontend::ast::{Expression, Program};

pub struct Interpreter;

impl Interpreter {
    pub fn new() -> Self {
        Interpreter
    }

    pub fn interpret(&self, program: &Program) -> Result<(), String> {
        // Implement interpretation logic here
        // This is where you'll execute the AST directly
        Ok(())
    }
} 