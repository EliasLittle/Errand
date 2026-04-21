use Errand::frontend::lexer::Lexer;
use Errand::frontend::parser::Parser as ErrandParser;
use Errand::frontend::ast::Program;
use Errand::backend::ir_lowering::IRLoweringPass;
use Errand::backend::preir_gen::compile_preir;
use Errand::backend::sir_gen::SirGen;
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
    
    /// Dump IR instructions to a file
    #[arg(long)]
    dump_ir: bool,
    
    /// Log level for compiler output
    #[arg(long, value_enum, default_value = "info")]
    log_level: LogLevel,
    
    /// Enable specific logging modules (comma-separated)
    #[arg(long, value_enum)]
    log_modules: Vec<LogModule>,

    /// Generate SIR (typed IR) from PreIR and dump to file
    #[arg(long)]
    dump_sir: bool,
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
    Worklist,
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
    /// Worklist type inference debug information
    Worklist,
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
            LogLevel::Worklist => CompilerLogLevel::Debug, // Map to debug for now
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
            LogModule::Worklist => "worklist".to_string(),
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
    compiler_info!("Log level: {:?}", log_level);
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

    // Generate PreIR directly from the lowered AST. All type inference and
    // checking happens inside SirGen via the worklist-based `Analyzer`
    // (src/backend/analysis.rs).
    compiler_info!("Generating PreIR instructions");
    let preir = match compile_preir(&lowered) {
        Ok(preir) => preir,
        Err(e) => {
            compiler_error!("PreIR generation failed: {}", e);
            std::process::exit(1);
        }
    };

    // Generate SIR. This is the single type-inference + checking + emission
    // pass: SirGen's internal Analyzer types every PreIR instruction as SIR
    // is emitted, so any type error surfaces here.
    compiler_info!("Generating SIR...");
    let sir_module = match SirGen::emit_sir_module(preir.clone(), &lowered) {
        Ok(sir_module) => {
            compiler_info!("SIR generation successful");
            sir_module
        }
        Err(e) => {
            compiler_error!("SIR generation failed: {}", e);
            std::process::exit(1);
        }
    };

    // Dump SIR to file if requested.
    if cli.dump_sir {
        let sir_output = sir_module.format_all();
        let sir_file_path = if let Some(stripped) = file_path.strip_suffix(".err") {
            format!("{}.sir", stripped)
        } else {
            format!("{}.sir", file_path)
        };
        std::fs::write(&sir_file_path, sir_output)
            .expect("Failed to write SIR to file");
        compiler_info!("SIR written to: {}", sir_file_path);
    }

    // Dump IR if requested
    if cli.dump_ir {
        let verbir = preir.format_all();
        let main_ir = preir.format_main();
        
        // Determine IR file path
        let verbir_file_path = if let Some(stripped) = file_path.strip_suffix(".err") {
            format!("{}.verbir", stripped)
        } else {
            format!("{}.verbir", file_path)
        };

        let main_ir_file_path = if let Some(stripped) = file_path.strip_suffix(".err") {
            format!("{}.ir", stripped)
        } else {
            format!("{}.ir", file_path)
        };
        
        std::fs::write(&verbir_file_path, verbir)
            .expect("Failed to write Verbose IR to file");
        compiler_info!("IR instructions written to: {}", verbir_file_path);

        std::fs::write(&main_ir_file_path, main_ir)
            .expect("Failed to write main IR to file");
        compiler_info!("IR instructions written to: {}", main_ir_file_path);
    }

    // Compile via the SIR-based Cranelift backend.
    let ir_lowering = IRLoweringPass::new();
    match ir_lowering.lower_sir_to_cranelift(&sir_module) {
        Ok(compiled_code) => {
            compiler_info!("Successfully compiled SIR to machine code ({} bytes)", compiled_code.len());

            let obj_file_path = if let Some(stripped) = file_path.strip_suffix(".err") {
                format!("{}.bin", stripped)
            } else {
                format!("{}.bin", file_path)
            };

            std::fs::write(&obj_file_path, &compiled_code)
                .expect("Failed to write compiled code to file");
            compiler_info!("Machine code written to: {}", obj_file_path);

            match cli.emit {
                EmitType::Obj => {
                    if let Some(output) = cli.output {
                        std::fs::copy(&obj_file_path, &output)
                            .expect("Failed to copy object file");
                        compiler_info!("Object file copied to: {}", output);
                    }
                }
                EmitType::Exe => {
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
