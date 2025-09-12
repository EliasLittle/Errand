use log::{Level, LevelFilter, Metadata, Record};
use colored::*;
use std::sync::atomic::{AtomicUsize, Ordering};

/// Custom log levels for the Errand compiler
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum CompilerLogLevel {
    /// Critical errors that prevent compilation
    Error = 0,
    /// Warnings that don't prevent compilation but indicate potential issues
    Warn = 1,
    /// General information about compilation progress
    Info = 2,
    /// Detailed information about compilation phases
    Debug = 3,
    /// Very detailed tracing information
    Trace = 4,
    /// Lexer-specific debug information
    Lexer = 5,
    /// Parser-specific debug information
    Parser = 6,
    /// Type inference debug information
    TypeInference = 7,
    /// Code generation debug information
    CodeGen = 8,
    /// Lowering/desugaring debug information
    Lowering = 9,
    /// Cranelift compiler debug information
    Cranelift = 10,
}

impl CompilerLogLevel {
    pub fn as_str(&self) -> &'static str {
        match self {
            CompilerLogLevel::Error => "ERROR",
            CompilerLogLevel::Warn => "WARN",
            CompilerLogLevel::Info => "INFO",
            CompilerLogLevel::Debug => "DEBUG",
            CompilerLogLevel::Trace => "TRACE",
            CompilerLogLevel::Lexer => "LEXER",
            CompilerLogLevel::Parser => "PARSER",
            CompilerLogLevel::TypeInference => "TYPE",
            CompilerLogLevel::CodeGen => "CODEGEN",
            CompilerLogLevel::Lowering => "LOWER",
            CompilerLogLevel::Cranelift => "CRANELIFT",
        }
    }

    pub fn color(&self) -> Color {
        match self {
            CompilerLogLevel::Error => Color::Red,
            CompilerLogLevel::Warn => Color::Yellow,
            CompilerLogLevel::Info => Color::Green,
            CompilerLogLevel::Debug => Color::Blue,
            CompilerLogLevel::Trace => Color::Magenta,
            CompilerLogLevel::Lexer => Color::Cyan,
            CompilerLogLevel::Parser => Color::BrightCyan,
            CompilerLogLevel::TypeInference => Color::BrightBlue,
            CompilerLogLevel::CodeGen => Color::BrightGreen,
            CompilerLogLevel::Lowering => Color::BrightMagenta,
            CompilerLogLevel::Cranelift => Color::BrightYellow,
        }
    }
}

/// Global log level filter
static LOG_LEVEL: AtomicUsize = AtomicUsize::new(CompilerLogLevel::Info as usize);

/// Custom logger for the Errand compiler
pub struct CompilerLogger {
    enabled_modules: Vec<String>,
}

impl CompilerLogger {
    pub fn new() -> Self {
        Self {
            enabled_modules: Vec::new(),
        }
    }

    pub fn with_modules(mut self, modules: Vec<String>) -> Self {
        self.enabled_modules = modules;
        self
    }

    pub fn set_level(level: CompilerLogLevel) {
        LOG_LEVEL.store(level as usize, Ordering::Relaxed);
    }

    pub fn get_level() -> CompilerLogLevel {
        let level = LOG_LEVEL.load(Ordering::Relaxed);
        match level {
            0 => CompilerLogLevel::Error,
            1 => CompilerLogLevel::Warn,
            2 => CompilerLogLevel::Info,
            3 => CompilerLogLevel::Debug,
            4 => CompilerLogLevel::Trace,
            5 => CompilerLogLevel::Lexer,
            6 => CompilerLogLevel::Parser,
            7 => CompilerLogLevel::TypeInference,
            8 => CompilerLogLevel::CodeGen,
            9 => CompilerLogLevel::Lowering,
            10 => CompilerLogLevel::Cranelift,
            _ => CompilerLogLevel::Info,
        }
    }

    fn should_log(&self, record: &Record) -> bool {
        let current_level = Self::get_level();
        let record_level = self.parse_level_from_target(record.target());
        
        if record_level > current_level {
            return false;
        }

        // If no specific modules are enabled, log everything at the current level
        if self.enabled_modules.is_empty() {
            return true;
        }

        // Check if the module is in the enabled list
        self.enabled_modules.iter().any(|module| record.target().starts_with(module))
    }

    fn parse_level_from_target(&self, target: &str) -> CompilerLogLevel {
        if target.starts_with("lexer") {
            CompilerLogLevel::Lexer
        } else if target.starts_with("parser") {
            CompilerLogLevel::Parser
        } else if target.starts_with("type_inference") {
            CompilerLogLevel::TypeInference
        } else if target.starts_with("codegen") || target.starts_with("cranelift") {
            CompilerLogLevel::CodeGen
        } else if target.starts_with("lowering") {
            CompilerLogLevel::Lowering
        } else {
            // Default to the standard log level
            match target {
                "error" => CompilerLogLevel::Error,
                "warn" => CompilerLogLevel::Warn,
                "info" => CompilerLogLevel::Info,
                "debug" => CompilerLogLevel::Debug,
                "trace" => CompilerLogLevel::Trace,
                _ => CompilerLogLevel::Info,
            }
        }
    }
}

impl log::Log for CompilerLogger {
    fn enabled(&self, metadata: &Metadata) -> bool {
        // Create a dummy record to check if we should log
        let record = Record::builder()
            .level(Level::Info)
            .target(metadata.target())
            .build();
        self.should_log(&record)
    }

    fn log(&self, record: &Record) {
        if !self.should_log(record) {
            return;
        }

        let level = self.parse_level_from_target(record.target());
        let level_str = level.as_str().color(level.color()).bold();
        let target = record.target().bright_black();
        let message = record.args().to_string();

        // Format: [LEVEL] target: message
        println!("[{}] {}: {}", level_str, target, message);
    }

    fn flush(&self) {}
}

/// Initialize the compiler logger
pub fn init_logger(level: CompilerLogLevel, modules: Option<Vec<String>>) -> Result<(), log::SetLoggerError> {
    let logger = CompilerLogger::new().with_modules(modules.unwrap_or_default());
    CompilerLogger::set_level(level);
    log::set_boxed_logger(Box::new(logger))?;
    log::set_max_level(LevelFilter::Trace);
    Ok(())
}

/// Convenience macros for logging
#[macro_export]
macro_rules! compiler_error {
    ($($arg:tt)*) => {
        log::log!(target: "error", log::Level::Error, $($arg)*);
    };
}

#[macro_export]
macro_rules! compiler_warn {
    ($($arg:tt)*) => {
        log::log!(target: "warn", log::Level::Warn, $($arg)*);
    };
}

#[macro_export]
macro_rules! compiler_info {
    ($($arg:tt)*) => {
        log::log!(target: "info", log::Level::Info, $($arg)*);
    };
}

#[macro_export]
macro_rules! compiler_debug {
    ($($arg:tt)*) => {
        log::log!(target: "debug", log::Level::Debug, $($arg)*);
    };
}

#[macro_export]
macro_rules! compiler_trace {
    ($($arg:tt)*) => {
        log::log!(target: "trace", log::Level::Trace, $($arg)*);
    };
}

#[macro_export]
macro_rules! lexer_log {
    ($($arg:tt)*) => {
        log::log!(target: "lexer", log::Level::Debug, $($arg)*);
    };
}

#[macro_export]
macro_rules! parser_log {
    ($($arg:tt)*) => {
        log::log!(target: "parser", log::Level::Debug, $($arg)*);
    };
}

#[macro_export]
macro_rules! type_inference_log {
    ($($arg:tt)*) => {
        log::log!(target: "type_inference", log::Level::Debug, $($arg)*);
    };
}

#[macro_export]
macro_rules! codegen_log {
    ($($arg:tt)*) => {
        log::log!(target: "codegen", log::Level::Debug, $($arg)*);
    };
}

#[macro_export]
macro_rules! lowering_log {
    ($($arg:tt)*) => {
        log::log!(target: "lowering", log::Level::Debug, $($arg)*);
    };
}

#[macro_export]
macro_rules! cranelift_log {
    ($($arg:tt)*) => {
        log::log!(target: "cranelift", log::Level::Debug, $($arg)*);
    };
}
