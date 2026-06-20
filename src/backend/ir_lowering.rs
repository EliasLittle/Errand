use crate::backend::sir::SIRModule;
use crate::backend::sir_lowering::SIRLoweringPass;
use tracing::instrument;

// This is a wrapper for lowering IR to machine code.
// The only up-to-date path is `lower_sir_to_cranelift` (SIR -> native code).
pub struct IRLoweringPass;

impl IRLoweringPass {
    #[instrument(name = "ir_lowering.new", target = "backend", level = "trace")]
    pub fn new() -> Self {
        IRLoweringPass
    }

    /// Lower a typed `SIRModule` to native machine code via the SIR-based
    /// Cranelift backend.
    #[instrument(
        skip(self, sir_module),
        fields(
            function_names = sir_module.functions.len(),
            struct_names = sir_module.structs.len(),
            enum_names = sir_module.enums.len(),
            main_instrs = sir_module.main.instructions.len()
        ),
        name = "ir_lowering.lower_sir_to_cranelift",
        target = "backend",
        level = "debug"
    )]
    pub fn lower_sir_to_cranelift(&self, sir_module: &SIRModule) -> Result<Vec<u8>, String> {
        SIRLoweringPass::compile_sir_module(sir_module)
    }
}
