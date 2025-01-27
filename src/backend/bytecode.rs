#[derive(Debug)]
pub enum OpCode {
    LoadConstant(f64),
    Print,
    // Add more opcodes as needed
}

pub struct Bytecode {
    pub instructions: Vec<OpCode>,
}

impl Bytecode {
    pub fn new() -> Self {
        Bytecode {
            instructions: Vec::new(),
        }
    }
} 