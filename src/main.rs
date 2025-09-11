use Errand::frontend::lexer::Lexer;
use Errand::frontend::parser::Parser as ErrandParser;
use Errand::frontend::resolver::Resolver;
use Errand::frontend::type_inference::TypeInferencer;
use Errand::frontend::ast::Program;
use Errand::frontend::typeof_eval::TypeofEvaluator;
use Errand::backend::interpreter::Interpreter;
use Errand::backend::ir_lowering::IRLoweringPass;
use Errand::logging::{init_logger, CompilerLogLevel};
use Errand::{compiler_info, compiler_debug, compiler_error};

use clap::{Parser, ValueEnum};
use std::process::Command;
use std::path::Path;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// Input .err file to compile
    file: String,
    
    /// Output file path
    #[arg(short, long)]
    output: Option<String>,
    
    /// What to emit: 'obj' for object file, 'exe' for executable (default)
    #[arg(long, value_enum, default_value = "exe")]
    emit: EmitType,
    
    /// Target architecture
    #[arg(long, value_enum)]
    arch: Option<Arch>,
    
    /// Generate CLIF IR instead of machine code
    #[arg(long)]
    clif: bool,
    
    /// Log level for compiler output
    #[arg(long, value_enum, default_value = "info")]
    log_level: LogLevel,
    
    /// Enable specific logging modules (comma-separated)
    #[arg(long, value_enum)]
    log_modules: Vec<LogModule>,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum EmitType {
    /// Emit object file (.bin)
    Obj,
    /// Emit executable
    Exe,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum Arch {
    Arm,
    X86,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum LogLevel {
    Error,
    Warn,
    Info,
    Debug,
    Trace,
    Lexer,
    Parser,
    TypeInference,
    CodeGen,
    Lowering,
    Cranelift,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum LogModule {
    /// Lexer-specific debug information
    Lexer,
    /// Parser-specific debug information
    Parser,
    /// Type inference debug information
    TypeInference,
    /// Code generation debug information
    CodeGen,
    /// Lowering/desugaring debug information
    Lowering,
    /// Cranelift compilation debug information
    Cranelift,
    /// General compiler debug information
    Compiler,
}

impl From<LogLevel> for CompilerLogLevel {
    fn from(level: LogLevel) -> Self {
        match level {
            LogLevel::Error => CompilerLogLevel::Error,
            LogLevel::Warn => CompilerLogLevel::Warn,
            LogLevel::Info => CompilerLogLevel::Info,
            LogLevel::Debug => CompilerLogLevel::Debug,
            LogLevel::Trace => CompilerLogLevel::Trace,
            LogLevel::Lexer => CompilerLogLevel::Lexer,
            LogLevel::Parser => CompilerLogLevel::Parser,
            LogLevel::TypeInference => CompilerLogLevel::TypeInference,
            LogLevel::CodeGen => CompilerLogLevel::CodeGen,
            LogLevel::Lowering => CompilerLogLevel::Lowering,
            LogLevel::Cranelift => CompilerLogLevel::Cranelift,
        }
    }
}

impl From<LogModule> for String {
    fn from(module: LogModule) -> Self {
        match module {
            LogModule::Lexer => "lexer".to_string(),
            LogModule::Parser => "parser".to_string(),
            LogModule::TypeInference => "type_inference".to_string(),
            LogModule::CodeGen => "codegen".to_string(),
            LogModule::Lowering => "lowering".to_string(),
            LogModule::Cranelift => "cranelift".to_string(),
            LogModule::Compiler => "compiler".to_string(),
        }
    }
}

fn print_ast(path: &str, extension: &str, ast: &Program) {
    compiler_info!("AST: {}", ast);

    // Write the AST to a new file with the correct extension
    if !path.is_empty() {
        let ast_file_path = if let Some(stripped) = path.strip_suffix(".err") {
            format!("{}.{}", stripped, extension)
        } else {
            format!("{}.{}", path, extension)
        };
        std::fs::write(&ast_file_path, format!("{}", ast)).expect("Failed to write AST to file");
        compiler_info!("AST written to: {}", ast_file_path);
    }
}

fn link_object_file(obj_file: &str, output_file: &str, arch: Option<Arch>) -> Result<(), String> {
    compiler_info!("Linking {}...", obj_file);
    
    if !Path::new(obj_file).exists() {
        return Err(format!("Object file '{}' not found", obj_file));
    }
    
    let mut gcc_cmd = Command::new("gcc");
    match arch {
        Some(Arch::Arm) => {
            gcc_cmd.arg("-arch").arg("arm64");
        }
        Some(Arch::X86) => {
            gcc_cmd.arg("-arch").arg("x86_64");
        }
        None => {}
    }
    gcc_cmd
        .arg("-o")
        .arg(output_file)
        .arg(obj_file);
    
    // Print the full gcc command for debugging
    compiler_debug!("Running gcc command: gcc{}", 
        gcc_cmd.get_args()
            .map(|arg| format!(" {}", arg.to_string_lossy()))
            .collect::<String>()
    );
    
    let status = gcc_cmd
        .status()
        .map_err(|e| format!("Failed to run gcc: {}", e))?;
    if status.success() {
        compiler_info!("Success! Executable created: {}", output_file);
        Ok(())
    } else {
        Err("Linking failed".to_string())
    }
}

fn main() {
    let cli = Cli::parse();
    
    // Initialize logging
    let log_level = CompilerLogLevel::from(cli.log_level);
    let log_modules: Vec<String> = cli.log_modules
        .into_iter()
        .map(|m| m.into())
        .collect();
    
    if let Err(e) = init_logger(log_level, if log_modules.is_empty() { None } else { Some(log_modules) }) {
        eprintln!("Failed to initialize logger: {}", e);
        std::process::exit(1);
    }
    
    let file_path = &cli.file;
    if !Path::new(file_path).exists() {
        compiler_error!("File '{}' not found", file_path);
        std::process::exit(1);
    }
    
    let source = std::fs::read_to_string(file_path).expect("Failed to read file");
    compiler_info!("Compiling {}...", file_path);
    
    let mut lexer = Lexer::new(&source);
    let tokens = lexer.lex(file_path).expect("Lexing failed");
    compiler_info!("Lexer processed tokens successfully.");

    // Parse tokens into AST
    let mut parser = ErrandParser::new(tokens);
    let ast = parser.parse().expect("Parsing failed");
    print_ast(file_path, "ast", &ast);
    
    // Lower the AST
    let lowered = ast.lower();
    print_ast(file_path, "last", &lowered);

    // Type inference
    let mut type_inferencer = TypeInferencer::new();
    let typed_program = type_inferencer.infer_program(&lowered).expect("Type inference failed");
    print_ast(file_path, "tast", &typed_program);

    // Evaluate typeof calls
    let typeof_evaluator = TypeofEvaluator;
    let typeof_evaluated_program = typeof_evaluator.eval_program(&typed_program);
    print_ast(file_path, "typeof", &typeof_evaluated_program);

    // After type inference, add compilation
    let ir_lowering = IRLoweringPass::new();
    
    // Check if we're in CLIF mode
    if cli.clif {
        compiler_info!("Generating CLIF IR");
        match ir_lowering.generate_clif(&typeof_evaluated_program) {
            Ok(clif_ir) => {
                compiler_info!("Successfully generated CLIF IR");
                
                // Determine output file path
                let output_path = cli.output.unwrap_or_else(|| {
                    if let Some(stripped) = file_path.strip_suffix(".err") {
                        format!("{}.clif", stripped)
                    } else {
                        format!("{}.clif", file_path)
                    }
                });
                
                std::fs::write(&output_path, clif_ir)
                    .expect("Failed to write CLIF IR to file");
                compiler_info!("CLIF IR written to: {}", output_path);
            }
            Err(e) => {
                compiler_error!("CLIF generation failed: {}", e);
                std::process::exit(1);
            }
        }
    } else {
        // Compile the entire program (including function definitions)
        match ir_lowering.lower_to_cranelift(&typeof_evaluated_program) {
            Ok(compiled_code) => {
                compiler_info!("Successfully compiled to machine code ({} bytes)", compiled_code.len());
                
                // Determine object file path
                let obj_file_path = if let Some(stripped) = file_path.strip_suffix(".err") {
                    format!("{}.bin", stripped)
                } else {
                    format!("{}.bin", file_path)
                };
                
                // Write the compiled code to object file
                std::fs::write(&obj_file_path, &compiled_code)
                    .expect("Failed to write compiled code to file");
                compiler_info!("Machine code written to: {}", obj_file_path);
                
                // Handle linking based on emit type
                match cli.emit {
                    EmitType::Obj => {
                        // Just output the object file, no linking needed
                        if let Some(output) = cli.output {
                            std::fs::copy(&obj_file_path, &output)
                                .expect("Failed to copy object file");
                            compiler_info!("Object file copied to: {}", output);
                        }
                    }
                    EmitType::Exe => {
                        // Link the object file to create executable
                        let output_path = cli.output.unwrap_or_else(|| {
                            Path::new(file_path).file_stem()
                                .unwrap()
                                .to_string_lossy()
                                .to_string()
                        });
                        
                        if let Err(e) = link_object_file(&obj_file_path, &output_path, cli.arch) {
                            compiler_error!("Error: {}", e);
                            std::process::exit(1);
                        }
                    }
                }
            }
            Err(e) => {
                compiler_error!("Compilation failed: {}", e);
                std::process::exit(1);
            }
        }
    }
}
