mod frontend;
use frontend::lexer::Lexer;
use frontend::ast::{Program, TypeExpression};
use frontend::parser::Parser;
use frontend::resolver::Resolver;
use frontend::type_inference::TypeInferencer;

mod backend;
use crate::backend::interpreter::Interpreter;
use crate::backend::ir_lowering::IRLoweringPass;

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
        eprintln!("Usage: {} <string> or {} --file <file_path> [--clif]", args[0], args[0]);
        return;
    }
    
    // Check if --clif flag is present
    let clif_mode = args.contains(&"--clif".to_string());
    
    let mut file_path = "";
    let source = if args[1] == "--file" {
        if args.len() < 3 {
            eprintln!("Usage: {} <string> or {} --file <file_path> [--clif]", args[0], args[0]);
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
    
    // Lower the AST
    let lowered = ast.lower();
    print_ast(file_path, "last", &lowered);

    // Type inference
    let mut type_inferencer = TypeInferencer::new();
    let typed_program = type_inferencer.infer_program(&lowered).expect("Type inference failed");
    print_ast(file_path, "tast", &typed_program); // Print inferred program

    /*

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
    */

    // After type inference, add compilation
    let ir_lowering = IRLoweringPass::new();
    
    // Check if we're in CLIF mode
    if clif_mode {
        println!("Generating CLIF IR");
        // Just generate CLIF IR and write to file
        match ir_lowering.generate_clif(&typed_program) {
            Ok(clif_ir) => {
                println!("Successfully generated CLIF IR");
                
                // Write the CLIF IR to a file
                if !file_path.is_empty() {
                    let clif_file_path = format!("{}.clif", file_path);
                    std::fs::write(&clif_file_path, clif_ir)
                        .expect("Failed to write CLIF IR to file");
                    println!("CLIF IR written to: {}", clif_file_path);
                }
            }
            Err(e) => {
                eprintln!("CLIF generation failed: {}", e);
                eprintln!("CLIF IR: {}", e.to_string());
            }
        }
    } else {
        // Compile the entire program (including function definitions)
        match ir_lowering.lower_to_cranelift(&typed_program) {
            Ok(compiled_code) => {
                println!("Successfully compiled to machine code ({} bytes)", compiled_code.len());
                
                // Write the compiled code to a file
                if !file_path.is_empty() {
                    let bin_file_path = format!("{}.bin", file_path);
                    std::fs::write(&bin_file_path, &compiled_code)
                        .expect("Failed to write compiled code to file");
                    println!("Machine code written to: {}", bin_file_path);
                }
            }
            Err(e) => {
                eprintln!("Compilation failed: {}", e);
            }
        }
    }
} 