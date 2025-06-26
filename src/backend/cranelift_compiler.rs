use cranelift::prelude::*;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_native;
use std::collections::HashMap;
use crate::frontend::ast::{Program, Expression, BinaryOperator, UnaryOperator, Id, TypeExpression};

pub struct CraneliftCompiler {
    builder_ctx: FunctionBuilderContext,
    variables: HashMap<String, Variable>,
    functions: HashMap<String, FuncRef>,
}

impl CraneliftCompiler {
    pub fn new() -> Self {
        CraneliftCompiler {
            builder_ctx: FunctionBuilderContext::new(),
            variables: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    pub fn compile_program(&mut self, program: &Program) -> Result<Vec<u8>, String> {
        let clif_function = self.lower_to_clif(program)?;
        self.compile_clif_to_machine_code(clif_function)
    }

    pub fn lower_to_clif(&mut self, program: &Program) -> Result<Function, String> {
        // Create a new function
        let mut sig = cranelift_codegen::ir::Signature::new(cranelift_codegen::isa::CallConv::SystemV);
        sig.params.push(AbiParam::new(types::I32));
        sig.returns.push(AbiParam::new(types::I32));
        
        let mut func = Function::with_name_signature(ExternalName::user(0, 0), sig);
        let mut builder = FunctionBuilder::new(&mut func, &mut self.builder_ctx);
        
        let entry_block = builder.create_block();
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);
        
        // Compile each expression
        for expr in &program.expressions {
            self.compile_expression(&mut builder, expr)?;
        }
        
        // Return 0
        let return_value = builder.ins().iconst(types::I32, 0);
        builder.ins().return_(&[return_value]);
        
        builder.finalize();
        
        Ok(func)
    }

    pub fn compile_clif_to_machine_code(&self, func: Function) -> Result<Vec<u8>, String> {
        // Set up the target
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();
        let isa_builder = cranelift_native::builder().unwrap();
        let isa = isa_builder.finish(settings::Flags::new(flag_builder)).unwrap();

        // Create module and compile function
        let mut module = cranelift_module::Module::new(isa);
        
        // Create function signature
        let mut sig = module.make_signature();
        sig.params.push(AbiParam::new(types::I32));
        sig.returns.push(AbiParam::new(types::I32));
        
        let func_id = module.declare_function("main", cranelift_module::Linkage::Export, &sig)
            .map_err(|e| format!("Failed to declare function: {}", e))?;
        
        // Compile the function
        let code_size = module.define_function(func_id, &mut func.clone())
            .map_err(|e| format!("Failed to define function: {}", e))?;
        
        // Finalize the module
        module.finalize_definitions();
        
        // Get the compiled code
        let code = module.get_finalized_function(func_id);
        Ok(code.to_vec())
    }

    fn compile_expression(&mut self, builder: &mut FunctionBuilder, expr: &Expression) -> Result<Value, String> {
        match expr {
            Expression::Int(n) => {
                Ok(builder.ins().iconst(types::I64, *n as i64))
            }
            Expression::Float(f) => {
                Ok(builder.ins().f64const(*f))
            }
            Expression::Boolean(b) => {
                Ok(builder.ins().iconst(types::I8, *b as i64))
            }
            Expression::String(s) => {
                // For strings, we'll need to handle them differently
                // This is a simplified version
                Err("String literals not yet implemented".to_string())
            }
            Expression::Identifier { id, type_expr } => {
                if let Some(var) = self.variables.get(&id.name) {
                    Ok(builder.use_var(*var))
                } else {
                    Err(format!("Undefined variable: {}", id.name))
                }
            }
            Expression::BinaryOp { operator, left, right } => {
                let lhs = self.compile_expression(builder, left)?;
                let rhs = self.compile_expression(builder, right)?;
                
                match operator {
                    BinaryOperator::Add => {
                        if lhs.ty() == types::I64 && rhs.ty() == types::I64 {
                            Ok(builder.ins().iadd(lhs, rhs))
                        } else if lhs.ty() == types::F64 && rhs.ty() == types::F64 {
                            Ok(builder.ins().fadd(lhs, rhs))
                        } else {
                            Err("Type mismatch in addition".to_string())
                        }
                    }
                    BinaryOperator::Subtract => {
                        if lhs.ty() == types::I64 && rhs.ty() == types::I64 {
                            Ok(builder.ins().isub(lhs, rhs))
                        } else if lhs.ty() == types::F64 && rhs.ty() == types::F64 {
                            Ok(builder.ins().fsub(lhs, rhs))
                        } else {
                            Err("Type mismatch in subtraction".to_string())
                        }
                    }
                    BinaryOperator::Multiply => {
                        if lhs.ty() == types::I64 && rhs.ty() == types::I64 {
                            Ok(builder.ins().imul(lhs, rhs))
                        } else if lhs.ty() == types::F64 && rhs.ty() == types::F64 {
                            Ok(builder.ins().fmul(lhs, rhs))
                        } else {
                            Err("Type mismatch in multiplication".to_string())
                        }
                    }
                    BinaryOperator::Divide => {
                        if lhs.ty() == types::I64 && rhs.ty() == types::I64 {
                            Ok(builder.ins().sdiv(lhs, rhs))
                        } else if lhs.ty() == types::F64 && rhs.ty() == types::F64 {
                            Ok(builder.ins().fdiv(lhs, rhs))
                        } else {
                            Err("Type mismatch in division".to_string())
                        }
                    }
                    BinaryOperator::Assignment => {
                        // Handle variable assignment
                        if let Expression::Identifier { id, .. } = left.as_ref() {
                            let var = self.variables.entry(id.name.clone()).or_insert_with(|| {
                                builder.declare_var(id.name.clone(), types::I64)
                            });
                            builder.def_var(*var, rhs);
                            Ok(rhs)
                        } else {
                            Err("Left side of assignment must be an identifier".to_string())
                        }
                    }
                    _ => Err(format!("Unsupported binary operator: {:?}", operator))
                }
            }
            Expression::UnaryOp { operator, operand } => {
                let operand_val = self.compile_expression(builder, operand)?;
                
                match operator {
                    UnaryOperator::Negate => {
                        if operand_val.ty() == types::I64 {
                            Ok(builder.ins().ineg(operand_val))
                        } else if operand_val.ty() == types::F64 {
                            Ok(builder.ins().fneg(operand_val))
                        } else {
                            Err("Cannot negate non-numeric value".to_string())
                        }
                    }
                    UnaryOperator::Not => {
                        if operand_val.ty() == types::I8 {
                            Ok(builder.ins().bnot(operand_val))
                        } else {
                            Err("Cannot apply not to non-boolean value".to_string())
                        }
                    }
                }
            }
            Expression::FunctionCall { id, arguments } => {
                // Compile arguments
                let mut compiled_args = Vec::new();
                for arg in arguments {
                    compiled_args.push(self.compile_expression(builder, arg)?);
                }
                
                // Call the function
                if let Some(func_ref) = self.functions.get(&id.name) {
                    Ok(builder.ins().call(*func_ref, &compiled_args))
                } else {
                    Err(format!("Undefined function: {}", id.name))
                }
            }
            Expression::If { condition, then_branch, else_branch } => {
                let cond_val = self.compile_expression(builder, condition)?;
                
                // Create blocks for then, else, and merge
                let then_block = builder.create_block();
                let else_block = builder.create_block();
                let merge_block = builder.create_block();
                
                // Branch based on condition
                builder.ins().brif(cond_val, then_block, &[], else_block, &[]);
                
                // Compile then branch
                builder.switch_to_block(then_block);
                let then_val = self.compile_expression(builder, then_branch)?;
                builder.ins().jump(merge_block, &[then_val]);
                builder.seal_block(then_block);
                
                // Compile else branch
                builder.switch_to_block(else_block);
                let else_val = if let Some(else_expr) = else_branch {
                    self.compile_expression(builder, else_expr)?
                } else {
                    builder.ins().iconst(types::I64, 0)
                };
                builder.ins().jump(merge_block, &[else_val]);
                builder.seal_block(else_block);
                
                // Merge block
                builder.switch_to_block(merge_block);
                let phi = builder.ins().phi(types::I64, &[(then_val, then_block), (else_val, else_block)]);
                builder.seal_block(merge_block);
                
                Ok(phi)
            }
            Expression::While { condition, body } => {
                let loop_block = builder.create_block();
                let body_block = builder.create_block();
                let exit_block = builder.create_block();
                
                // Jump to loop condition
                builder.ins().jump(loop_block, &[]);
                
                // Loop condition
                builder.switch_to_block(loop_block);
                let cond_val = self.compile_expression(builder, condition)?;
                builder.ins().brif(cond_val, body_block, &[], exit_block, &[]);
                builder.seal_block(loop_block);
                
                // Loop body
                builder.switch_to_block(body_block);
                self.compile_expression(builder, body)?;
                builder.ins().jump(loop_block, &[]);
                builder.seal_block(body_block);
                
                // Exit block
                builder.switch_to_block(exit_block);
                builder.seal_block(exit_block);
                
                Ok(builder.ins().iconst(types::I64, 0))
            }
            Expression::Block(expressions) => {
                let mut result = builder.ins().iconst(types::I64, 0);
                for expr in expressions {
                    result = self.compile_expression(builder, expr)?;
                }
                Ok(result)
            }
            Expression::Print(expr) => {
                let value = self.compile_expression(builder, expr)?;
                // For now, we'll just return the value
                // In a real implementation, you'd call a print function
                Ok(value)
            }
            _ => Err(format!("Unsupported expression type: {:?}", expr))
        }
    }
} 