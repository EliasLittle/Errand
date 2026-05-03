use Errand::backend::ir_lowering::IRLoweringPass;
use Errand::backend::preir::PreIR;
use Errand::backend::preir_gen::compile_preir;
use Errand::backend::sir::SIRModule;
use Errand::backend::sir_gen::SirGen;
use Errand::frontend::ast::Program;
use Errand::frontend::lexer::Lexer;
use Errand::frontend::parser::Parser as ErrandParser;

use clap::{Parser, ValueEnum};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::Mutex;
use tracing::{debug, error, info};
use tracing_subscriber::fmt::format::FmtSpan;
use tracing_subscriber::fmt::time::SystemTime;
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::util::SubscriberInitExt;
use tracing_subscriber::{fmt, EnvFilter, Registry};

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

    /// Write structured tracing output (newline-delimited JSON) to this file
    #[arg(long, default_value = "errand-trace.jsonl")]
    trace_json: PathBuf,
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
    info!(ast = %ast, "AST");

    if path.is_empty() {
        return;
    }

    let ast_file_path = derived_output_path(path, extension);
    std::fs::write(&ast_file_path, format!("{}", ast)).expect("Failed to write AST to file");
    info!(path = %ast_file_path, "AST written to file");
}

fn dump_verbose_preir(file_path: &str, preir: &PreIR) {
    let verbir = preir.format_all();
    let main_ir = preir.format_main();

    let verbir_path = derived_output_path(file_path, "verbir");
    let main_ir_path = derived_output_path(file_path, "ir");

    std::fs::write(&verbir_path, verbir).expect("Failed to write Verbose IR to file");
    info!(path = %verbir_path, "verbose IR written");

    std::fs::write(&main_ir_path, main_ir).expect("Failed to write main IR to file");
    info!(path = %main_ir_path, "main IR written");
}

fn dump_verbose_sir(file_path: &str, sir: &SIRModule) {
    let sir_path = derived_output_path(file_path, "sir");
    std::fs::write(&sir_path, sir.format_all()).expect("Failed to write SIR to file");
    info!(path = %sir_path, "SIR written");
}

fn link_object_file(obj_file: &str, output_file: &str, arch: Option<Arch>) -> Result<(), String> {
    info!(obj = %obj_file, "linking object file");

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

    debug!(
        cmd = %format!(
            "gcc{}",
            gcc_cmd
                .get_args()
                .map(|arg| format!(" {}", arg.to_string_lossy()))
                .collect::<String>()
        ),
        "invoking gcc"
    );

    let status = gcc_cmd
        .status()
        .map_err(|e| format!("Failed to run gcc: {}", e))?;
    if status.success() {
        info!(path = %output_file, "executable created");
        Ok(())
    } else {
        Err("Linking failed".to_string())
    }
}

fn run_compile(cli: &Cli) -> Result<(), String> {
    let file_path = &cli.file;
    let _compile = tracing::info_span!("compile", path = %file_path).entered();

    if !Path::new(file_path).exists() {
        return Err(format!("File '{}' not found", file_path));
    }

    let source = {
        let _read = tracing::debug_span!("compile.read_source").entered();
        std::fs::read_to_string(file_path).expect("Failed to read file")
    };
    info!(path = %file_path, "compiling");

    let tokens = {
        let _lex = tracing::info_span!("compile.lex").entered();
        let mut lexer = Lexer::new(source);
        lexer.lex(file_path).expect("Lexing failed")
    };
    info!("lexing complete");

    let ast = {
        let _parse = tracing::info_span!("compile.parse").entered();
        let mut parser = ErrandParser::new(tokens);
        parser.parse().expect("Parsing failed")
    };
    if cli.verbose {
        print_ast(file_path, "ast", &ast);
    }

    let lowered = {
        let _lower = tracing::info_span!("compile.lower").entered();
        ast.lower()
    };
    if cli.verbose {
        print_ast(file_path, "last", &lowered);
    }

    info!("generating PreIR");
    let preir = {
        let _preir = tracing::info_span!("compile.preir").entered();
        compile_preir(&lowered).map_err(|e| format!("PreIR generation failed: {}", e))?
    };

    if cli.verbose {
        dump_verbose_preir(file_path, &preir);
    }

    info!("generating SIR");
    let sir_module = {
        let _sir = tracing::info_span!("compile.sir").entered();
        SirGen::emit_sir_module(preir.clone(), &lowered)
            .map_err(|e| format!("SIR generation failed: {}", e))?
    };
    info!("SIR generation complete");

    if cli.verbose {
        dump_verbose_sir(file_path, &sir_module);
    }

    let compiled_code = {
        let _cranelift = tracing::info_span!("compile.cranelift").entered();
        let ir_lowering = IRLoweringPass::new();
        ir_lowering
            .lower_sir_to_cranelift(&sir_module)
            .map_err(|e| format!("Compilation failed: {}", e))?
    };
    info!(bytes = compiled_code.len(), "compiled SIR to machine code");

    let obj_file_path = derived_output_path(file_path, "o");
    {
        let _write = tracing::debug_span!("compile.write_object", path = %obj_file_path).entered();
        std::fs::write(&obj_file_path, &compiled_code)
            .expect("Failed to write compiled code to file");
    }
    info!(path = %obj_file_path, "machine code written");

    match cli.emit {
        EmitType::Obj => {
            if let Some(output) = &cli.output {
                let _copy = tracing::debug_span!("compile.copy_object", dest = %output).entered();
                std::fs::copy(&obj_file_path, output).expect("Failed to copy object file");
                info!(path = %output, "object file copied");
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

            let _link = tracing::info_span!("compile.link", output = %output_path).entered();
            link_object_file(&obj_file_path, &output_path, cli.arch)
                .map_err(|e| format!("Error: {}", e))?;
        }
    }

    Ok(())
}

fn init_tracing_json(path: &Path) {
    let file = std::fs::OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(path)
        .unwrap_or_else(|e| panic!("failed to open trace file {}: {e}", path.display()));

    let writer = Mutex::new(file);

    let filter = EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("trace"));

    Registry::default()
        .with(filter)
        .with(
            fmt::layer()
                .json()
                .with_writer(writer)
                .with_current_span(true)
                .with_span_list(true)
                .with_span_events(FmtSpan::CLOSE)
                .with_timer(SystemTime),
        )
        .init();
}

fn main() {
    let cli = Cli::parse();
    init_tracing_json(&cli.trace_json);
    info!(path = %cli.trace_json.display(), "JSON trace output");

    if let Err(msg) = run_compile(&cli) {
        error!(%msg, "compilation failed");
        std::process::exit(1);
    }
}
