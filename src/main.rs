use Errand::backend::ir_lowering::IRLoweringPass;
use Errand::backend::preir::PreIR;
use Errand::backend::preir_gen::compile_preir;
use Errand::backend::sir::SIRModule;
use Errand::backend::sir_gen::SirGen;
use Errand::frontend::ast::Program;
use Errand::frontend::lexer::Lexer;
use Errand::frontend::parser::Parser as ErrandParser;
// TODO: Replace these with tracing macros
use Errand::{compiler_debug, compiler_error, compiler_info};

use clap::{Parser, ValueEnum};
use std::path::Path;
use std::process::Command;

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

    /// Generate SIR (typed IR) from PreIR and dump to file
    #[arg(long)]
    verbose: bool,
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

fn derived_output_path(input_path: &str, ext: &str) -> String {
    if let Some(stem) = input_path.strip_suffix(".err") {
        format!("{}.{}", stem, ext)
    } else {
        format!("{}.{}", input_path, ext)
    }
}

fn print_ast(path: &str, extension: &str, ast: &Program) {
    compiler_info!("AST: {}", ast);

    if path.is_empty() {
        return;
    }

    let ast_file_path = derived_output_path(path, extension);
    std::fs::write(&ast_file_path, format!("{}", ast)).expect("Failed to write AST to file");
    compiler_info!("AST written to: {}", ast_file_path);
}

fn dump_verbose_preir(file_path: &str, preir: &PreIR) {
    let verbir = preir.format_all();
    let main_ir = preir.format_main();

    let verbir_path = derived_output_path(file_path, "verbir");
    let main_ir_path = derived_output_path(file_path, "ir");

    std::fs::write(&verbir_path, verbir).expect("Failed to write Verbose IR to file");
    compiler_info!("IR instructions written to: {}", verbir_path);

    std::fs::write(&main_ir_path, main_ir).expect("Failed to write main IR to file");
    compiler_info!("IR instructions written to: {}", main_ir_path);
}

fn dump_verbose_sir(file_path: &str, sir: &SIRModule) {
    let sir_path = derived_output_path(file_path, "sir");
    std::fs::write(&sir_path, sir.format_all()).expect("Failed to write SIR to file");
    compiler_info!("SIR written to: {}", sir_path);
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
    gcc_cmd.arg("-o").arg(output_file).arg(obj_file);

    compiler_debug!(
        "Running gcc command: gcc{}",
        gcc_cmd
            .get_args()
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

fn run_compile(cli: &Cli) -> Result<(), String> {
    let file_path = &cli.file;
    if !Path::new(file_path).exists() {
        return Err(format!("File '{}' not found", file_path));
    }

    let source = std::fs::read_to_string(file_path).expect("Failed to read file");
    compiler_info!("Compiling {}...", file_path);

    let mut lexer = Lexer::new(source);
    let tokens = lexer.lex(file_path).expect("Lexing failed");
    compiler_info!("Lexer processed tokens successfully.");

    let mut parser = ErrandParser::new(tokens);
    let ast = parser.parse().expect("Parsing failed");
    if cli.verbose {
        print_ast(file_path, "ast", &ast);
    }

    let lowered = ast.lower();
    if cli.verbose {
        print_ast(file_path, "last", &lowered);
    }

    compiler_info!("Generating PreIR instructions");
    let preir = compile_preir(&lowered).map_err(|e| format!("PreIR generation failed: {}", e))?;

    if cli.verbose {
        dump_verbose_preir(file_path, &preir);
    }

    compiler_info!("Generating SIR...");
    let sir_module = SirGen::emit_sir_module(preir.clone(), &lowered)
        .map_err(|e| format!("SIR generation failed: {}", e))?;
    compiler_info!("SIR generation successful");

    if cli.verbose {
        dump_verbose_sir(file_path, &sir_module);
    }

    let ir_lowering = IRLoweringPass::new();
    let compiled_code = ir_lowering
        .lower_sir_to_cranelift(&sir_module)
        .map_err(|e| format!("Compilation failed: {}", e))?;
    compiler_info!(
        "Successfully compiled SIR to machine code ({} bytes)",
        compiled_code.len()
    );

    let obj_file_path = derived_output_path(file_path, "o");
    std::fs::write(&obj_file_path, &compiled_code).expect("Failed to write compiled code to file");
    compiler_info!("Machine code written to: {}", obj_file_path);

    match cli.emit {
        EmitType::Obj => {
            if let Some(output) = &cli.output {
                std::fs::copy(&obj_file_path, output).expect("Failed to copy object file");
                compiler_info!("Object file copied to: {}", output);
            }
        }
        EmitType::Exe => {
            let output_path = cli.output.clone().unwrap_or_else(|| {
                Path::new(file_path)
                    .file_stem()
                    .unwrap()
                    .to_string_lossy()
                    .into_owned()
            });

            link_object_file(&obj_file_path, &output_path, cli.arch)
                .map_err(|e| format!("Error: {}", e))?;
        }
    }

    Ok(())
}

fn main() {
    let cli = Cli::parse();
    if let Err(msg) = run_compile(&cli) {
        compiler_error!("{}", msg);
        std::process::exit(1);
    }
}
