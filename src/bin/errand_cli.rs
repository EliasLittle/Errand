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
    /// Compile the .err file to an executable
    Compile {
        /// The .err file to compile
        file: String,
        /// Target architecture (optional)
        #[arg(long, value_enum)]
        arch: Option<Arch>,
    },
    /// Compile and run the .err file
    Run {
        /// The .err file to run
        file: String,
        /// Target architecture (optional)
        #[arg(long, value_enum)]
        arch: Option<Arch>,
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
        Commands::Compile { file, arch } => {
            if let Err(e) = compile(file, arch) {
                eprintln!("Error: {}", e);
                std::process::exit(1);
            }
        }
        Commands::Run { file, arch } => {
            if let Err(e) = compile(file, arch) {
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

fn compile(file: &str, arch: &Option<Arch>) -> Result<(), String> {
    if !Path::new(file).exists() {
        return Err(format!("File '{}' not found", file));
    }
    let base_name = Path::new(file).file_stem().unwrap().to_string_lossy();
    println!("Compiling {}...", file);
    let mut cargo_args = vec!["run".to_string(), "--".to_string(), "--file".to_string(), file.to_string()];
    match arch {
        Some(Arch::Arm) => {
            cargo_args.insert(1, "--target".to_string());
            cargo_args.insert(2, "aarch64-apple-darwin".to_string());
        }
        Some(Arch::X86) => {
            cargo_args.insert(1, "--target".to_string());
            cargo_args.insert(2, "x86_64-apple-darwin".to_string());
        }
        None => {}
    }
    let status = Command::new("cargo")
        .args(&cargo_args)
        .status()
        .map_err(|e| format!("Failed to run cargo: {}", e))?;
    if !status.success() {
        return Err("Compilation failed".to_string());
    }
    println!("Linking {}.bin...", file);
    let bin_file = format!("{}.bin", file);
    if !Path::new(&bin_file).exists() {
        return Err(format!("Expected binary file '{}' not found", bin_file));
    }
    let status = Command::new("gcc")
        .arg("-o")
        .arg(&*base_name)
        .arg(&bin_file)
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