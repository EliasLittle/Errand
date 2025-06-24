use crate::frontend::ast::{Program, Expression, BinaryOperator, UnaryOperator, Id};
use inkwell::context::Context;
use inkwell::builder::Builder;
use inkwell::module::Module;
use inkwell::values::{FloatValue, FunctionValue, PointerValue, BasicValueEnum};
use inkwell::types::BasicTypeEnum;
use inkwell::FloatPredicate;
use std::collections::HashMap;

pub struct LLVMCompiler<'a, 'ctx> {
    context: &'ctx Context,
    builder: &'a Builder<'ctx>,
    module: &'a Module<'ctx>,
    
    // Track current function being compiled
    current_function: Option<FunctionValue<'ctx>>,
    
    // Symbol table for variables
    variables: HashMap<String, PointerValue<'ctx>>,
    
    // Symbol table for functions
    functions: HashMap<String, FunctionValue<'ctx>>,
}

impl<'a, 'ctx> LLVMCompiler<'a, 'ctx> {
    pub fn new(
        context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        module: &'a Module<'ctx>,
    ) -> Self {
        LLVMCompiler {
            context,
            builder,
            module,
            current_function: None,
            variables: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    /// Create an alloca instruction in the entry block of the function
    fn create_entry_block_alloca(&self, name: &str) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();
        
        if let Some(function) = self.current_function {
            let entry = function.get_first_basic_block().unwrap();
            match entry.get_first_instruction() {
                Some(first_instr) => builder.position_before(&first_instr),
                None => builder.position_at_end(entry),
            }
            
            builder.build_alloca(self.context.f64_type(), name).unwrap()
        } else {
            panic!("No current function");
        }
    }

    /// Compile a full program
    pub fn compile_program(&mut self, program: &Program) -> Result<(), String> {
        // Create main function
        let main_type = self.context.i32_type().fn_type(&[], false);
        let main_fn = self.module.add_function("main", main_type, None);
        let basic_block = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(basic_block);
        
        self.current_function = Some(main_fn);
        
        // Compile each expression in the program
        for expr in &program.expressions {
            self.compile_expression(expr)?;
        }
        
        // Return 0 from main
        self.builder.build_return(Some(&self.context.i32_type().const_int(0, false))).unwrap();
        
        Ok(())
    }

    /// Compile a single expression
    fn compile_expression(&mut self, expr: &Expression) -> Result<BasicValueEnum<'ctx>, String> {
        match expr {
            Expression::Number(n) => {
                Ok(self.context.f64_type().const_float(*n).into())
            },
            
            Expression::Boolean(b) => {
                Ok(self.context.bool_type().const_int(*b as u64, false).into())
            },
            
            Expression::String(s) => {
                let string_const = self.builder.build_global_string_ptr(s, "str").unwrap();
                Ok(string_const.as_pointer_value().into())
            },
            
            Expression::Identifier(id) => {
                if let Some(var) = self.variables.get(&id.name) {
                    Ok(self.builder.build_load(self.context.f64_type(), *var, &id.name).unwrap())
                } else {
                    Err(format!("Undefined variable: {}", id.name))
                }
            },
            
            Expression::UnaryOp { operator, operand } => {
                let operand_val = self.compile_expression(operand)?;
                match operator {
                    UnaryOperator::Negate => {
                        if let Some(float_val) = operand_val.as_float_value() {
                            Ok(self.builder.build_float_neg(float_val, "neg").unwrap().into())
                        } else {
                            Err("Expected float value for negation".to_string())
                        }
                    },
                    UnaryOperator::Not => {
                        if let Some(int_val) = operand_val.as_int_value() {
                            Ok(self.builder.build_not(int_val, "not").unwrap().into())
                        } else {
                            Err("Expected boolean value for not operation".to_string())
                        }
                    }
                }
            },
            
            Expression::BinaryOp { operator, left, right } => {
                let lhs = self.compile_expression(left)?;
                let rhs = self.compile_expression(right)?;
                
                match operator {
                    BinaryOperator::Add => {
                        if let (Some(l), Some(r)) = (lhs.as_float_value(), rhs.as_float_value()) {
                            Ok(self.builder.build_float_add(l, r, "add").unwrap().into())
                        } else {
                            Err("Expected float values for addition".to_string())
                        }
                    },
                    BinaryOperator::Subtract => {
                        if let (Some(l), Some(r)) = (lhs.as_float_value(), rhs.as_float_value()) {
                            Ok(self.builder.build_float_sub(l, r, "sub").unwrap().into())
                        } else {
                            Err("Expected float values for subtraction".to_string())
                        }
                    },
                    BinaryOperator::Multiply => {
                        if let (Some(l), Some(r)) = (lhs.as_float_value(), rhs.as_float_value()) {
                            Ok(self.builder.build_float_mul(l, r, "mul").unwrap().into())
                        } else {
                            Err("Expected float values for multiplication".to_string())
                        }
                    },
                    BinaryOperator::Divide => {
                        if let (Some(l), Some(r)) = (lhs.as_float_value(), rhs.as_float_value()) {
                            Ok(self.builder.build_float_div(l, r, "div").unwrap().into())
                        } else {
                            Err("Expected float values for division".to_string())
                        }
                    },
                    _ => Err(format!("Unsupported binary operator: {:?}", operator))
                }
            },
            
            Expression::FunctionCall { id, arguments } => {
                if let Some(function) = self.functions.get(&id.name) {
                    let mut compiled_args = Vec::new();
                    for arg in arguments {
                        compiled_args.push(self.compile_expression(arg)?);
                    }
                    
                    let args: Vec<_> = compiled_args.iter().map(|val| val.into()).collect();
                    match self.builder.build_call(*function, &args, "call").unwrap().try_as_basic_value().left() {
                        Some(value) => Ok(value),
                        None => Err("Invalid function call result".to_string())
                    }
                } else {
                    Err(format!("Undefined function: {}", id.name))
                }
            },
            
            Expression::FunctionDefinition { id, parameters, body } => {
                // Create function type
                let return_type = self.context.f64_type();
                let param_types: Vec<BasicTypeEnum> = parameters.iter()
                    .map(|_| self.context.f64_type().into())
                    .collect();
                let fn_type = return_type.fn_type(&param_types, false);
                
                // Create function
                let function = self.module.add_function(&id.name, fn_type, None);
                self.functions.insert(id.name.clone(), function);
                
                // Create basic block
                let basic_block = self.context.append_basic_block(function, "entry");
                self.builder.position_at_end(basic_block);
                
                // Store current function
                let prev_function = self.current_function;
                self.current_function = Some(function);
                
                // Add parameters to variables
                let mut old_bindings = Vec::new();
                for (i, param) in parameters.iter().enumerate() {
                    let alloca = self.create_entry_block_alloca(&param.id.name);
                    self.builder.build_store(alloca, function.get_nth_param(i as u32).unwrap()).unwrap();
                    
                    if let Some(old_binding) = self.variables.insert(param.id.name.clone(), alloca) {
                        old_bindings.push((param.id.name.clone(), old_binding));
                    }
                }
                
                // Compile body
                let body_val = self.compile_expression(body)?;
                self.builder.build_return(Some(&body_val)).unwrap();
                
                // Restore previous state
                for (name, old_binding) in old_bindings {
                    self.variables.insert(name, old_binding);
                }
                self.current_function = prev_function;
                
                Ok(function.as_global_value().as_pointer_value().into())
            },
            
            Expression::If { condition, then_branch, else_branch } => {
                let cond_val = self.compile_expression(condition)?;
                let parent = self.current_function.unwrap();
                
                let then_bb = self.context.append_basic_block(parent, "then");
                let else_bb = self.context.append_basic_block(parent, "else");
                let merge_bb = self.context.append_basic_block(parent, "merge");
                
                // Convert condition to boolean
                let cond_val = if let Some(float_val) = cond_val.as_float_value() {
                    self.builder.build_float_compare(
                        FloatPredicate::ONE,
                        float_val,
                        self.context.f64_type().const_float(0.0),
                        "ifcond"
                    ).unwrap()
                } else {
                    return Err("Expected numeric condition".to_string());
                };
                
                self.builder.build_conditional_branch(cond_val, then_bb, else_bb).unwrap();
                
                // Compile then branch
                self.builder.position_at_end(then_bb);
                let then_val = self.compile_expression(then_branch)?;
                self.builder.build_unconditional_branch(merge_bb).unwrap();
                let then_bb = self.builder.get_insert_block().unwrap();
                
                // Compile else branch
                self.builder.position_at_end(else_bb);
                let else_val = if let Some(else_expr) = else_branch {
                    self.compile_expression(else_expr)?
                } else {
                    self.context.f64_type().const_float(0.0).into()
                };
                self.builder.build_unconditional_branch(merge_bb).unwrap();
                let else_bb = self.builder.get_insert_block().unwrap();
                
                // Emit merge block
                self.builder.position_at_end(merge_bb);
                let phi = self.builder.build_phi(self.context.f64_type(), "iftmp").unwrap();
                phi.add_incoming(&[
                    (&then_val, then_bb),
                    (&else_val, else_bb)
                ]);
                
                Ok(phi.as_basic_value())
            },
            
            Expression::While { condition, body } => {
                let parent = self.current_function.unwrap();
                
                let cond_bb = self.context.append_basic_block(parent, "while.cond");
                let body_bb = self.context.append_basic_block(parent, "while.body");
                let end_bb = self.context.append_basic_block(parent, "while.end");
                
                // Jump to condition
                self.builder.build_unconditional_branch(cond_bb).unwrap();
                
                // Emit condition
                self.builder.position_at_end(cond_bb);
                let cond_val = self.compile_expression(condition)?;
                let cond_val = if let Some(float_val) = cond_val.as_float_value() {
                    self.builder.build_float_compare(
                        FloatPredicate::ONE,
                        float_val,
                        self.context.f64_type().const_float(0.0),
                        "whilecond"
                    ).unwrap()
                } else {
                    return Err("Expected numeric condition".to_string());
                };
                self.builder.build_conditional_branch(cond_val, body_bb, end_bb).unwrap();
                
                // Emit body
                self.builder.position_at_end(body_bb);
                self.compile_expression(body)?;
                self.builder.build_unconditional_branch(cond_bb).unwrap();
                
                // Continue at end block
                self.builder.position_at_end(end_bb);
                
                Ok(self.context.f64_type().const_float(0.0).into())
            },
            
            Expression::Print(expr) => {
                // Get printf function
                let printf_type = self.context.i32_type().fn_type(&[self.context.i8_type().ptr_type(inkwell::AddressSpace::Generic).into()], true);
                let printf = self.module.add_function("printf", printf_type, None);
                
                // Compile expression to print
                let value = self.compile_expression(expr)?;
                
                // Create format string based on value type
                let format_str = if value.is_float_value() {
                    "%f\n\0"
                } else if value.is_int_value() {
                    "%d\n\0"
                } else {
                    return Err("Unsupported type for printing".to_string());
                };
                
                let format_str_ptr = self.builder.build_global_string_ptr(format_str, "printf.str").unwrap();
                
                // Call printf
                self.builder.build_call(
                    printf,
                    &[format_str_ptr.as_pointer_value().into(), value.into()],
                    "printf"
                ).unwrap();
                
                Ok(self.context.f64_type().const_float(0.0).into())
            },
            
            _ => Err(format!("Unsupported expression type: {:?}", expr))
        }
    }
} 