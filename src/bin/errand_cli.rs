use clap::{Parser, Subcommand, Arg, ValueEnum};
use std::process::Command;
use std::path::Path;
use std::fs;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Build the Errand compiler
    Build {
        /// Target architecture (optional)
        #[arg(long, value_enum)]
        arch: Option<Arch>,
        /// Build in release mode
        #[arg(long)]
        release: bool,
    },
    /// Compile the .err file to an executable
    Compile {
        /// The .err file to compile
        file: String,
        /// Target architecture (optional)
        #[arg(long, value_enum)]
        arch: Option<Arch>,
        /// Build in release mode
        #[arg(long)]
        release: bool,
    },
    /// Compile and run the .err file
    Run {
        /// The .err file to run
        file: String,
        /// Target architecture (optional)
        #[arg(long, value_enum)]
        arch: Option<Arch>,
        /// Build in release mode
        #[arg(long)]
        release: bool,
    },
    /// Parse the .err file to an AST
    Parse {
        /// The .err file to parse
        file: String,
    },
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum Arch {
    Arm,
    X86,
}

fn main() {
    let cli = Cli::parse();
    match &cli.command {
        Commands::Build { arch, release } => {
            if let Err(e) = build_compiler(arch, *release) {
                eprintln!("Error: {}", e);
                std::process::exit(1);
            }
        }
        Commands::Compile { file, arch, release } => {
            if let Err(e) = build_compiler(arch, *release) {
                eprintln!("Error: {}", e);
                std::process::exit(1);
            }
            if let Err(e) = compile_file(file, arch, *release) {
                eprintln!("Error: {}", e);
                std::process::exit(1);
            }
        }
        Commands::Run { file, arch, release } => {
            if let Err(e) = build_compiler(arch, *release) {
                eprintln!("Error: {}", e);
                std::process::exit(1);
            }
            if let Err(e) = compile_file(file, arch, *release) {
                eprintln!("Error: {}", e);
                std::process::exit(1);
            }
            let base_name = Path::new(file).file_stem().unwrap().to_string_lossy();
            println!("Running {}...", base_name);
            println!("----------------------------------------");
            let status = Command::new(format!("./{}", base_name))
                .status()
                .expect("Failed to run executable");
            println!("----------------------------------------");
            std::process::exit(status.code().unwrap_or(1));
        }
        Commands::Parse { file } => {
            if let Err(e) = parse_to_ast(file) {
                eprintln!("Error: {}", e);
                std::process::exit(1);
            }
        }
    }
}

fn build_compiler(arch: &Option<Arch>, release: bool) -> Result<(), String> {
    println!("Building Errand compiler...");
    let mut build_args = vec!["build".to_string()];
    
    if release {
        build_args.push("--release".to_string());
    }
    
    match arch {
        Some(Arch::Arm) => {
            build_args.push("--target".to_string());
            build_args.push("aarch64-apple-darwin".to_string());
        }
        Some(Arch::X86) => {
            build_args.push("--target".to_string());
            build_args.push("x86_64-apple-darwin".to_string());
        }
        None => {}
    }
    
    println!("Running cargo build with args: {:?}", build_args);
    let status = Command::new("cargo")
        .args(&build_args)
        .status()
        .map_err(|e| format!("Failed to run cargo build: {}", e))?;
    if !status.success() {
        return Err("Failed to build Errand compiler".to_string());
    }
    
    println!("Errand compiler built successfully!");
    Ok(())
}

fn compile_file(file: &str, arch: &Option<Arch>, release: bool) -> Result<(), String> {
    if !Path::new(file).exists() {
        return Err(format!("File '{}' not found", file));
    }
    let base_name = Path::new(file).file_stem().unwrap().to_string_lossy();
    
    // Use the compiled Errand binary to compile the source file
    println!("Compiling {}...", file);
    let errand_binary = match (arch, release) {
        (Some(Arch::Arm), true) => "target/aarch64-apple-darwin/release/Errand",
        (Some(Arch::Arm), false) => "target/aarch64-apple-darwin/debug/Errand",
        (Some(Arch::X86), true) => "target/x86_64-apple-darwin/release/Errand",
        (Some(Arch::X86), false) => "target/x86_64-apple-darwin/debug/Errand",
        (None, true) => "target/release/Errand",
        (None, false) => "target/debug/Errand",
    };
    
    let status = Command::new(errand_binary)
        .arg("--file")
        .arg(file)
        .status()
        .map_err(|e| format!("Failed to run Errand compiler: {}", e))?;
    if !status.success() {
        return Err("Compilation failed".to_string());
    }
    
    let bin_file = format!("{}.bin", file.strip_suffix(".err").unwrap_or(file));
    println!("Linking {}...", bin_file);
    if !Path::new(&bin_file).exists() {
        return Err(format!("Expected binary file '{}' not found", bin_file));
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
        .arg(&*base_name)
        .arg(&bin_file);
    
    // Print the full gcc command for debugging
    println!("Running gcc command: gcc{}", 
        gcc_cmd.get_args()
            .map(|arg| format!(" {}", arg.to_string_lossy()))
            .collect::<String>()
    );
    
    let status = gcc_cmd
        .status()
        .map_err(|e| format!("Failed to run gcc: {}", e))?;
    if status.success() {
        println!("Success! Executable created: {}", base_name);
        Ok(())
    } else {
        Err("Linking failed".to_string())
    }
}

fn parse_to_ast(file: &str) -> Result<(), String> {
    use Errand::frontend::lexer::Lexer;
    use Errand::frontend::parser::Parser;

    if !Path::new(file).exists() {
        return Err(format!("File '{}' not found", file));
    }
    let source = fs::read_to_string(file).map_err(|e| format!("Failed to read file: {}", e))?;
    let mut lexer = Lexer::new(&source);
    let tokens = lexer.lex(file).map_err(|e| format!("Lexing failed: {}", e))?;
    let mut parser = Parser::new(tokens);
    let ast = parser.parse().map_err(|errs| format!("Parsing failed: {:?}", errs))?;
    let ast_file = format!("{}.ast", file);
    fs::write(&ast_file, format!("{}", ast)).map_err(|e| format!("Failed to write AST: {}", e))?;
    println!("AST written to: {}", ast_file);
    Ok(())
} 