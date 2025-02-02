mod frontend;
mod backend;

use frontend::lexer::Lexer;
use frontend::parser::Parser;
use backend::{interpreter::Interpreter, compiler::Compiler};
use std::env;

fn main() {

    // Get the file path or string from command line arguments
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <string> or {} --file <file_path>", args[0], args[0]);
        return;
    }
    
    let mut file_path = "";
    let source = if args[1] == "--file" {
        if args.len() < 3 {
            eprintln!("Usage: {} <string> or {} --file <file_path>", args[0], args[0]);
            return;
        }
        file_path = &args[2];
        println!("Received file path: {}", file_path); // Debug print statement
        std::fs::read_to_string(file_path).expect("Failed to read file")
    } else {
        args[1].clone() // Use the string directly
    };
    
    let mut lexer = Lexer::new(&source); // Initialize with the source

    let tokens = lexer.lex(file_path).expect("Lexing failed");
    println!("Lexer processed tokens successfully."); // Debug print statement

    // Parse tokens into AST
    let mut parser = Parser::new(tokens);
    let ast = parser.parse().expect("Parsing failed");

    println!("AST: {}", ast);

    // Write the AST to a new file with '.ast' extension
    if !file_path.is_empty() {
        let ast_file_path = format!("{}.ast", file_path);
        std::fs::write(&ast_file_path, format!("{}", ast)).expect("Failed to write AST to file");
        println!("AST written to: {}", ast_file_path); // Debug print statement
    }

    /*
    // You can either interpret or compile
    let interpreter = Interpreter::new();
    interpreter.interpret(&ast).expect("Interpretation failed");

    // Or compile to bytecode
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast).expect("Compilation failed");
    */
} 