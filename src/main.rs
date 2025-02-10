mod frontend;
use frontend::lexer::Lexer;
use frontend::ast::Program;
use frontend::parser::Parser;
use frontend::resolver::Resolver;

mod backend;
use crate::backend::interpreter::Interpreter;

use std::env;

fn print_ast(path: &str, extension: &str, ast: &Program) {
    println!("AST: {}", ast);

    // Write the AST to a new file with '.ast' extension
    if !path.is_empty() {
        let ast_file_path = format!("{}.{}", path, extension);
        std::fs::write(&ast_file_path, format!("{}", ast)).expect("Failed to write AST to file");
        println!("AST written to: {}", ast_file_path); // Debug print statement
    }
}

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
    print_ast(file_path, "ast", &ast);
    let lowered = ast.lower();
    print_ast(file_path, "last", &lowered);

    let mut resolver = Resolver::new();
    let locals = resolver.resolve(&lowered).expect("Resolution failed");
    println!("Locals: {:?}", locals);

    let mut interpreter = Interpreter::new();
    interpreter.update_local_scope(locals);
    let result = interpreter.interpret(&lowered).expect("Interpretation failed");
    println!("\n-------------------------\n\nResult: {:?}", result);

    /*
    // You can either interpret or compile
    let interpreter = Interpreter::new();
    interpreter.interpret(&ast).expect("Interpretation failed");

    // Or compile to bytecode
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast).expect("Compilation failed");
    */
} 