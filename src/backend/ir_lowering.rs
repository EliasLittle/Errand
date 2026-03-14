use crate::frontend::ast::Program;
use crate::backend::cranelift_compiler::CraneliftCompiler;
use crate::backend::sir_lowering::SIRLoweringPass;
use crate::backend::sir::SIRModule;
use cranelift_codegen::ir::Function;

pub struct IRLoweringPass;

impl IRLoweringPass {
    pub fn new() -> Self {
        IRLoweringPass
    }

    pub fn lower_to_cranelift(&self, program: &Program) -> Result<Vec<u8>, String> {
        let mut compiler = CraneliftCompiler::new();
        compiler.compile_program(program)
    }

    pub fn lower_to_clif(&self, program: &Program) -> Result<Function, String> {
        let mut compiler = CraneliftCompiler::new();
        compiler.lower_to_clif(program)
    }

    pub fn generate_clif(&self, program: &Program) -> Result<String, String> {
        let mut compiler = CraneliftCompiler::new();
        compiler.generate_clif(program)
    }

    pub fn compile_clif_to_machine_code(&self, func: Function) -> Result<Vec<u8>, String> {
        // This method is now deprecated since we handle compilation in compile_program
        // For backward compatibility, we'll create a simple wrapper
        let mut compiler = CraneliftCompiler::new();
        
        // Create a simple program with just the main function
        let program = Program {
            expressions: vec![], // Empty since we're just compiling the main function
        };
        
        // Use the new compilation approach
        compiler.compile_program(&program)
    }

    /// Lower a typed `SIRModule` to native machine code via the new SIR-based
    /// Cranelift backend.  This is the preferred compilation path once SIR
    /// generation is complete.
    pub fn lower_sir_to_cranelift(&self, sir_module: &SIRModule) -> Result<Vec<u8>, String> {
        SIRLoweringPass::compile_sir_module(sir_module)
    }

    // You can add more IR lowering passes here for different targets
    pub fn lower_to_llvm(&self, program: &Program) -> Result<String, String> {
        // Future LLVM IR lowering
        Err("LLVM IR lowering not yet implemented".to_string())
    }
} 