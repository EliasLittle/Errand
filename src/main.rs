mod frontend;
mod backend;

use frontend::lexer::Lexer;
use frontend::parser::Parser;
use backend::{interpreter::Interpreter, compiler::Compiler};
use std::env;

fn main() {
    // Get the file path from command line arguments
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <file_path>", args[0]);
        return;
    }
    
    let file_path = &args[1];
    println!("Received file path: {}", file_path); // Debug print statement
    let mut lexer = Lexer::new(""); // Initialize with an empty string

    match lexer.test_lexer(file_path) {
        Ok(tokens) => {
            println!("Lexer processed tokens successfully."); // Debug print statement
            //lexer.print_tokens(&tokens); // Print the tokens
        }
        Err(e) => {
            eprintln!("Error: {}", e);
        }
    }

    // Parse tokens into AST
    /* 
    let mut parser = Parser::new(tokens);
    let ast = parser.parse().expect("Parsing failed");

    // You can either interpret or compile
    let interpreter = Interpreter::new();
    interpreter.interpret(&ast).expect("Interpretation failed");

    // Or compile to bytecode
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast).expect("Compilation failed");
    */
} 