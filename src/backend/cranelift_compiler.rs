use cranelift::prelude::*;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_codegen::ir::{Function, InstBuilder, types, UserFuncName, BlockArg};
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::settings::{self, Configurable};
use cranelift_module::{Module, Linkage, ModuleError};
use cranelift_codegen::ir::{AbiParam, ExternalName};
use cranelift_codegen::ir::{Value, FuncRef};
use cranelift_codegen::print_errors::pretty_error;
use cranelift_module::FuncId;
use cranelift_object::ObjectModule;
use std::collections::HashMap;
use std::error::Error;
use crate::frontend::ast::{Program, Expression, BinaryOperator, UnaryOperator, Id, TypeExpression, Parameter};

pub struct CraneliftCompiler {
    builder_ctx: FunctionBuilderContext,
    variables: HashMap<String, Variable>,
    functions: HashMap<String, FuncId>,
    module: Option<ObjectModule>,
    function_signatures: HashMap<String, cranelift_codegen::ir::Signature>,
    next_function_id: u32,  // Add this field
    string_counter: u32, // Counter for unique string names
    isa: Option<std::sync::Arc<dyn cranelift_codegen::isa::TargetIsa>>, // Store the ISA as Arc
    printf_variants: HashMap<String, FuncId>, // Cache for printf FFI variants
}

impl CraneliftCompiler {
    pub fn new() -> Self {
        CraneliftCompiler {
            builder_ctx: FunctionBuilderContext::new(),
            variables: HashMap::new(),
            functions: HashMap::new(),
            module: None,
            function_signatures: HashMap::new(),
            next_function_id: 0,
            string_counter: 0,
            isa: None,
            printf_variants: HashMap::new(), // will not be used anymore
        }
    }

    pub fn compile_program(&mut self, program: &Program) -> Result<Vec<u8>, String> {
        // First pass: collect function definitions and create signatures
        self.collect_function_definitions(program)?;
        
        // Initialize the module
        self.initialize_module()?;
        
        // Second pass: compile all functions
        self.compile_all_functions(program)?;
        
        // Third pass: compile the main function
        let mut main_function = self.lower_to_clif(program)?;
        
        // Add main function to module and compile everything
        let _main_func_ref = self.add_function_to_module("main", &mut main_function)?;
        self.finalize_module()
    }

    pub fn generate_clif(&mut self, program: &Program) -> Result<String, String> {
        // First pass: collect function definitions and create signatures
        self.collect_function_definitions(program)?;
        println!("Collected function definitions");
        
        // Initialize the module
        self.initialize_module()?;
        println!("Initialized module");
        
        // Second pass: compile all functions
        self.compile_all_functions(program)?;
        println!("Compiled all functions");

        // Third pass: compile the main function
        let mut main_function = self.lower_to_clif(program)?;
        println!("Compiled main function");

        // Add main function to module (but don't compile to machine code)
        let _main_func_ref = match self.add_function_to_module("main", &mut main_function) {
            Ok(func_ref) => {
                println!("Added main function to module");
                func_ref
            }
            Err(e) => {
                println!("=== ERROR ADDING MAIN FUNCTION TO MODULE (generate_clif) ===");
                println!("Error: {}", e);
                println!("Main function IR:");
                println!("{}", main_function);
                println!("Function name: {:?}", main_function.name);
                println!("Function signature: {:?}", main_function.signature);
                println!("=== END ERROR DETAILS ===");
                return Err(e);
            }
        };

        // Return the CLIF IR as a string
        Ok(format!("{}", main_function))
    }

    fn initialize_module(&mut self) -> Result<(), String> {
        // Set up the target
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "true").unwrap();
        flag_builder.set("enable_verifier", "true").unwrap();
        flag_builder.set("enable_llvm_abi_extensions", "true").unwrap();
        let isa = cranelift_native::builder().unwrap()
            .finish(settings::Flags::new(flag_builder))
            .unwrap();

        // Store the ISA for later use
        self.isa = Some(isa.clone());

        // Create module
        let object_builder = cranelift_object::ObjectBuilder::new(isa, "main", Box::new(|libcall| format!("{}", libcall)))
            .map_err(|e| format!("Failed to create object builder: {}", e))?;
        self.module = Some(cranelift_object::ObjectModule::new(object_builder));
        
        // Add built-in FFI functions to the symbol table
        self.add_builtin_ffi_functions()?;
        
        Ok(())
    }

    fn compile_all_functions(&mut self, program: &Program) -> Result<(), String> {
        // Phase 1: Declare all functions in the module (but don't define them yet)
        for expr in &program.expressions {
            if let Expression::FunctionDefinition { id, parameters, body: _, return_type_expr } = expr {
                println!("Declaring function: {:?}", id);
                self.declare_function_in_module(id, parameters, return_type_expr)?;
            }
        }
        
        // Phase 2: Compile all function bodies (now they can call each other recursively)
        for expr in &program.expressions {
            if let Expression::FunctionDefinition { id, parameters, body, return_type_expr: _ } = expr {
                println!("Compiling function body: {:?}", id);
                self.compile_function_definition(id, parameters, body)?;
            }
        }
        println!("All functions defined: {:?}", self.functions);
        Ok(())
    }

    fn declare_function_in_module(&mut self, id: &Id, parameters: &[Parameter], return_type_expr: &Option<TypeExpression>) -> Result<(), String> {
        let module = self.module.as_mut()
            .ok_or("Module not initialized")?;
        
        // Get the signature for this function
        let signature = self.function_signatures.get(&id.name)
            .ok_or_else(|| format!("No signature found for function: {}", id.name))?
            .clone();
        
        // Declare the function in the module
        let func_id = module.declare_function(&id.name, cranelift_module::Linkage::Export, &signature)
            .map_err(|e| format!("Failed to declare function: {}", e))?;
        
        // Store the function ID for later use
        self.functions.insert(id.name.clone(), func_id);
        
        println!("Declared function '{}' with func_id: {:?}", id.name, func_id);
        Ok(())
    }

    fn define_function_in_module(&mut self, name: &str, func: &mut Function) -> Result<(), String> {
        let module = self.module.as_mut()
            .ok_or("Module not initialized")?;
        
        // Get the function ID that was already assigned during declaration
        let func_id = self.functions.get(name)
            .ok_or_else(|| format!("Function '{}' was not declared", name))?;
        
        // Create a context for compilation
        let mut ctx = cranelift_codegen::Context::for_function(func.clone());
        
        // Enable verbose error reporting
        ctx.set_disasm(true);
        
        println!("About to define function name: '{}' with func_id: {:?}", name, func_id);
        
        // Define the function in the module
        module.define_function(*func_id, &mut ctx)
            .map_err(|e| {
                println!("!!Error: {:?}", e.source());
                format!("Failed to define function '{}': {}", name, e)
            })?;

        println!("Function defined: {}", name);
        Ok(())
    }

    fn add_function_to_module(&mut self, name: &str, func: &mut Function) -> Result<FuncId, String> {
        println!("=== ADDING FUNCTION TO MODULE ===");
        println!("Function name: '{}'", name);
        println!("Function IR name: {:?}", func.name);
        
        let module = self.module.as_mut()
            .ok_or("Module not initialized")?;
        
        // Get the signature for this function
        let signature = if name == "main" {
            // Create function signature for main
            let mut sig = module.make_signature();
            sig.params.push(AbiParam::new(types::I64)); // Use I64 to match other functions
            sig.returns.push(AbiParam::new(types::I64)); // Use I64 to match other functions
            // println!("Created main signature: {:?}", sig);
            sig
        } else {
            // Get the signature from our stored signatures
            let sig = self.function_signatures.get(name)
                .ok_or_else(|| format!("No signature found for function: {}", name))?
                .clone();
            // println!("Retrieved signature for '{}': {:?}", name, sig);
            sig
        };
        
        // println!("About to declare function '{}' in module", name);
        let func_id = module.declare_function(name, cranelift_module::Linkage::Export, &signature)
            .map_err(|e| format!("Failed to declare function: {}", e))?;
        // println!("Module returned func_id: {:?}", func_id);
        
        // Create a context for compilation
        let mut ctx = cranelift_codegen::Context::for_function(func.clone());
        
        // Enable verbose error reporting
        ctx.set_disasm(true);
        
        // println!("About to define function name: '{}' with func_id: {:?}, func: {:?}", name, func_id, func);
        // println!("In context: {:?}", ctx.func);
        // Compile the function
        let define_result = module.define_function(func_id, &mut ctx)
            .map_err(|e| {
                println!("!!Error: {:?}", e.source());
                // Try to get more detailed error information
                format!("Failed to define function '{}': {}", name, e)
            });
        if let Err(e) = define_result {
            return Err(e);
        }

        println!("Function defined: {}", name);
        
        // Return the FuncId - we'll create FuncRefs when needed in calling contexts
        Ok(func_id)
    }

    fn add_builtin_ffi_functions(&mut self) -> Result<(), String> {
        let module = self.module.as_mut()
            .ok_or("Module not initialized")?;
        
        // Determine the target architecture and OS
        let (arch, os) = if let Some(ref isa) = self.isa {
            let triple = isa.triple();
            (triple.architecture, triple.operating_system)
        } else {
            (cranelift_codegen::isa::TargetIsa::triple(&*cranelift_native::builder().unwrap().finish(settings::Flags::new(settings::builder())).unwrap()).architecture, cranelift_codegen::isa::TargetIsa::triple(&*cranelift_native::builder().unwrap().finish(settings::Flags::new(settings::builder())).unwrap()).operating_system)
        };

        let ffi_functions = if format!("{}", arch) == "aarch64" && format!("{}", os) == "darwin" {
            // On aarch64-apple-darwin, always use the 8x i64 signature for printf
            vec![
                ("printf", vec![types::I64; 9], types::I32, cranelift_module::Linkage::Import),
                ("malloc", vec![types::I64], types::I64, cranelift_module::Linkage::Import),
                ("free", vec![types::I64], types::I64, cranelift_module::Linkage::Import),
                ("strlen", vec![types::I64], types::I64, cranelift_module::Linkage::Import),
                ("strcpy", vec![types::I64, types::I64], types::I64, cranelift_module::Linkage::Import),
                ("strcmp", vec![types::I64, types::I64], types::I32, cranelift_module::Linkage::Import),
            ]
        } else {
            vec![
                ("printf", vec![types::I64, types::I32], types::I32, cranelift_module::Linkage::Import),
                ("malloc", vec![types::I64], types::I64, cranelift_module::Linkage::Import),
                ("free", vec![types::I64], types::I64, cranelift_module::Linkage::Import),
                ("strlen", vec![types::I64], types::I64, cranelift_module::Linkage::Import),
                ("strcpy", vec![types::I64, types::I64], types::I64, cranelift_module::Linkage::Import),
                ("strcmp", vec![types::I64, types::I64], types::I32, cranelift_module::Linkage::Import),
            ]
        };
        
        for (name, param_types, return_type, linkage) in ffi_functions {
            let mut sig = cranelift_codegen::ir::Signature::new(cranelift_codegen::isa::CallConv::SystemV);
            for param_type in &param_types {
                sig.params.push(AbiParam::new(*param_type));
            }
            sig.returns.push(AbiParam::new(return_type));
            let func_id = module.declare_function(name, linkage, &sig)
                .map_err(|e| format!("Failed to declare FFI function '{}': {}", name, e))?;
            self.functions.insert(name.to_string(), func_id);
            self.function_signatures.insert(name.to_string(), sig);
            println!("Added FFI function '{}' to symbol table with ID: {:?}", name, func_id);
        }
        Ok(())
    }

    fn finalize_module(&mut self) -> Result<Vec<u8>, String> {
        let module = self.module.take()
            .ok_or("Module not initialized")?;
        
        // Finalize the module
        let object_product = module.finish();
        
        // Get the compiled code
        let object_bytes = object_product.emit().map_err(|e| format!("Failed to emit object: {}", e))?;
        Ok(object_bytes.to_vec())
    }

    fn collect_function_definitions(&mut self, program: &Program) -> Result<(), String> {
        for expr in &program.expressions {
            if let Expression::FunctionDefinition { id, parameters, body: _, return_type_expr } = expr {
                let signature = self.create_function_signature(parameters, return_type_expr)?;
                self.function_signatures.insert(id.name.clone(), signature);
            }
        }
        Ok(())
    }

    fn create_function_signature(&self, parameters: &[Parameter], return_type_expr: &Option<TypeExpression>) -> Result<cranelift_codegen::ir::Signature, String> {
        let mut sig = cranelift_codegen::ir::Signature::new(cranelift_codegen::isa::CallConv::SystemV);
        
        // Add parameters
        for param in parameters {
            let param_type = self.type_expression_to_cranelift_type(param.type_expr.as_ref())?;
            sig.params.push(AbiParam::new(param_type));
        }
        
        // Add return type
        let return_type = if let Some(return_type_expr) = return_type_expr {
            self.type_expression_to_cranelift_type(Some(return_type_expr))?
        } else {
            types::I64 // Default return type
        };
        sig.returns.push(AbiParam::new(return_type));
        
        Ok(sig)
    }

    fn type_expression_to_cranelift_type(&self, type_expr: Option<&TypeExpression>) -> Result<types::Type, String> {
        match type_expr {
            Some(TypeExpression::Int) => Ok(types::I64),
            Some(TypeExpression::Float) => Ok(types::F64),
            Some(TypeExpression::Bool) => Ok(types::I8),
            Some(TypeExpression::String) => Ok(types::I64), // Simplified: string as pointer
            Some(TypeExpression::Void) => Ok(types::I64), // Void as I64 for now
            Some(TypeExpression::Struct(_, _)) => Ok(types::I64), // Simplified: struct as pointer
            None => Ok(types::I64), // Default type
        }
    }

    pub fn lower_to_clif(&mut self, program: &Program) -> Result<Function, String> {
        println!("=== CREATING MAIN FUNCTION ===");
        // println!("Current next_function_id: {}", self.next_function_id);
        println!("Available functions: {:?}", self.functions);
        
        // Create a new function
        let mut sig = cranelift_codegen::ir::Signature::new(cranelift_codegen::isa::CallConv::SystemV);
        sig.params.push(AbiParam::new(types::I64)); // Use I64 to match other functions
        sig.returns.push(AbiParam::new(types::I64)); // Use I64 to match other functions
        
        let mut func = Function::with_name_signature(UserFuncName::user(0, self.next_function_id), sig);
        self.next_function_id += 1;
        println!("Created main function with name: {:?}", func.name);
        println!("=== END MAIN FUNCTION CREATION ===\n");
        
        // Create a new FunctionBuilderContext for this compilation
        let mut local_builder_ctx = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut func, &mut local_builder_ctx);
        
        let entry_block = builder.create_block();
        builder.switch_to_block(entry_block);
        // Add the parameter to the entry block to match the function signature
        builder.append_block_param(entry_block, types::I64);
        builder.seal_block(entry_block);
        
        // Compile each expression (skip function definitions as they're already compiled)
        let mut last_result = None;
        for expr in &program.expressions {
            match expr {
                Expression::FunctionDefinition { .. } => continue,
                _ => {
                    let result = self.compile_expression(&mut builder, expr)?;
                    last_result = Some(result);
                },
            };
        }
        
        // Return the last computed value, or 0 if no expressions were compiled
        let return_value = last_result.unwrap_or_else(|| builder.ins().iconst(types::I64, 0));
        builder.ins().return_(&[return_value]);
        
        builder.finalize();
        
        Ok(func)
    }

    fn compile_function_definition(&mut self, id: &Id, parameters: &[Parameter], body: &Expression) -> Result<(), String> {
        // Get the signature for this function
        let signature = self.function_signatures.get(&id.name)
            .ok_or_else(|| format!("No signature found for function: {}", id.name))?
            .clone();
        
        // Get the function ID that was already assigned during declaration
        let function_id = self.functions.get(&id.name)
            .ok_or_else(|| format!("Function '{}' was not declared", id.name))?;

        println!("=== COMPILING FUNCTION DEFINITION ===");
        println!("Function name: '{}'", id.name);
        println!("Using existing function_id: {:?}", function_id);

        // Create a new function
        let mut func = Function::with_name_signature(
            UserFuncName::user(0, function_id.index() as u32),
            signature.clone()
        );
        
        // Create a new FunctionBuilderContext for this function
        let mut local_builder_ctx = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut func, &mut local_builder_ctx);
        
        let entry_block = builder.create_block();
        builder.switch_to_block(entry_block);
        
        // Add block parameters for function parameters first
        for param in parameters {
            let param_type = self.type_expression_to_cranelift_type(param.type_expr.as_ref())?;
            builder.append_block_param(entry_block, param_type);
        }
        
        // Now seal the block after adding all parameters
        builder.seal_block(entry_block);
        
        // Declare parameters as variables
        let mut param_vars = HashMap::new();
        for (i, param) in parameters.iter().enumerate() {
            let param_type = self.type_expression_to_cranelift_type(param.type_expr.as_ref())?;
            let var = Variable::new(i);
            builder.declare_var(var, param_type);
            
            // Get the parameter value from the function parameters
            let param_value = builder.block_params(entry_block)[i];
            builder.def_var(var, param_value);
            
            param_vars.insert(param.id.name.clone(), var);
            // println!("Function parameter '{}' mapped to variable {:?} with value {:?}", param.id.name, var, param_value);
        }
        
        // Temporarily replace the global variables with function parameters
        // println!("Before replacing variables: {:?}", self.variables.keys().collect::<Vec<_>>());
        let original_variables = std::mem::replace(&mut self.variables, param_vars);
        // println!("After replacing variables: {:?}", self.variables.keys().collect::<Vec<_>>());
        
        println!("Compiling body: {:?}", body);
        // Compile the function body
        let result = self.compile_expression(&mut builder, body);
        
        // Restore the original variables
        self.variables = original_variables;
        
        // Handle the result - the desugarer ensures every function has a return statement
        match result {
            Ok(_) => {
                // The function body should already have a return statement
                // No need to add an implicit return
            }
            Err(e) => return Err(e),
        }
        
        // Ensure all blocks are sealed before finalizing
        builder.finalize();
        
        // Debug: Print the function IR before adding to module
        println!("Function '{}' IR before module addition:", id.name);
        println!("{}", func);
        

        
        // Define the function in the module (it's already declared)
        self.define_function_in_module(&id.name, &mut func)?;
        
        println!("=== FUNCTION MODULE REGISTRATION ===");
        println!("Function name: '{}'", id.name);
        println!("Function signature: {:?}", signature);
        println!("Function defined in module");
        println!("Current functions map: {:?}", self.functions);
        println!("=== END FUNCTION DEFINITION ===\n");

        
        Ok(())
    }



    pub fn write_object_to_file(&self, object_bytes: &[u8], filename: &str) -> Result<(), String> {
        std::fs::write(filename, object_bytes)
            .map_err(|e| format!("Failed to write object to file: {}", e))?;
        Ok(())
    }

    fn compile_expression(&mut self, builder: &mut FunctionBuilder, expr: &Expression) -> Result<Value, String> {
        match expr {
            Expression::Int(n) => {
                // println!("Compiling int: {:?}", n);
                let result = builder.ins().iconst(types::I64, *n as i64);
                // println!("result: {:?}", result);
                Ok(result)
            }
            Expression::Float(f) => {
                let result = builder.ins().f64const(*f);
                // println!("result: {:?}", result);
                Ok(result)
            }
            Expression::Boolean(b) => {
                Ok(builder.ins().iconst(types::I8, *b as i64))
            }
            Expression::String(s) => {
                println!("Compiling string literal: '{}'", s);
                
                // For now, let's use a simple approach - create a static string
                // This is a temporary workaround until we fix the data object approach

                let static_string = format!("{}{}", s.as_str(), '\0');
                let static_string = static_string.as_str();
                println!("static_string: {:?} : {:?}", static_string, std::any::type_name_of_val(static_string));

                let string_literal = static_string;
                
                // Create a data object for the string
                let module = self.module.as_mut()
                    .ok_or("Module not initialized")?;
                
                // Create a unique name for this string using a counter
                self.string_counter += 1;
                let string_name = format!("static_str_{}", self.string_counter);
                
                // Create the string data
                let string_data = string_literal.as_bytes().to_vec();
                
                // Declare the data object in the module
                let data_id = module.declare_data(&string_name, cranelift_module::Linkage::Local, false, false)
                    .map_err(|e| format!("Failed to declare string data: {}", e))?;
                
                // Define the data object
                let mut data_desc = cranelift_module::DataDescription::new();
                data_desc.define(string_data.into_boxed_slice());
                module.define_data(data_id, &data_desc)
                    .map_err(|e| format!("Failed to define string data: {}", e))?;
                
                // Create a global symbol reference
                let symbol = module.declare_data_in_func(data_id, &mut builder.func);
                
                // Load the address of the string using global_value
                let addr = builder.ins().global_value(types::I64, symbol);
                
                Ok(addr)
            }
            Expression::Identifier { id, type_expr } => {
                // println!("Looking up identifier: {}", id.name);
                // println!("Available variables: {:?}", self.variables.keys().collect::<Vec<_>>());
                if let Some(var) = self.variables.get(&id.name) {
                    // println!("Found variable: {:?}", var);
                    Ok(builder.use_var(*var))
                } else {
                    Err(format!("Undefined variable: {}", id.name))
                }
            }
            Expression::BinaryOp { operator, left, right } => {
                match operator {
                    BinaryOperator::Assignment => {
                        // Handle variable assignment - don't compile left side as expression
                        if let Expression::Identifier { id, .. } = left.as_ref() {
                            let rhs = self.compile_expression(builder, right)?;
                            
                            // Get the variable count before borrowing
                            let var_count = self.variables.len();
                            
                            let var = self.variables.entry(id.name.clone()).or_insert_with(|| {
                                let var = Variable::new(var_count);
                                builder.declare_var(var, types::I64);
                                var
                            });
                            
                            // Convert rhs to I64 if needed
                            let rhs_i64 = if builder.func.dfg.value_type(rhs) == types::I32 {
                                builder.ins().uextend(types::I64, rhs)
                            } else {
                                rhs
                            };
                            
                            builder.def_var(*var, rhs_i64);
                            Ok(rhs_i64)
                        } else {
                            Err("Left side of assignment must be an identifier".to_string())
                        }
                    }
                    _ => {
                        // For non-assignment operators, compile both sides
                        let lhs = self.compile_expression(builder, left)?;
                        let rhs = self.compile_expression(builder, right)?;
                        
                        match operator {
                            BinaryOperator::Add => {
                                // println!("Compiling add: {:?}, {:?}", lhs, rhs);
                                // Assume I64 for now, we can add type checking later
                                Ok(builder.ins().iadd(lhs, rhs))
                            }
                            BinaryOperator::Subtract => {
                                Ok(builder.ins().isub(lhs, rhs))
                            }
                            BinaryOperator::Multiply => {
                                Ok(builder.ins().imul(lhs, rhs))
                            }
                            BinaryOperator::Divide => {
                                Ok(builder.ins().sdiv(lhs, rhs))
                            }
                            BinaryOperator::LessThanEqual => {
                                Ok(builder.ins().icmp(IntCC::UnsignedLessThanOrEqual, lhs, rhs))
                            }
                            BinaryOperator::GreaterThanEqual => {
                                Ok(builder.ins().icmp(IntCC::UnsignedGreaterThanOrEqual, lhs, rhs))
                            }
                            BinaryOperator::GreaterThan => {
                                Ok(builder.ins().icmp(IntCC::UnsignedGreaterThan, lhs, rhs))
                            }
                            BinaryOperator::LessThan => {
                                Ok(builder.ins().icmp(IntCC::UnsignedLessThan, lhs, rhs))
                            }
                            BinaryOperator::Equal => {
                                Ok(builder.ins().icmp(IntCC::Equal, lhs, rhs))
                            }
                            BinaryOperator::NotEqual => {
                                Ok(builder.ins().icmp(IntCC::NotEqual, lhs, rhs))
                            }
                            _ => Err(format!("Unsupported binary operator: {:?}", operator))
                        }
                    }
                }
            }
            Expression::UnaryOp { operator, operand } => {
                let operand_val = self.compile_expression(builder, operand)?;
                
                match operator {
                    UnaryOperator::Negate => {
                        Ok(builder.ins().ineg(operand_val))
                    }
                    UnaryOperator::Not => {
                        Ok(builder.ins().bnot(operand_val))
                    }
                }
            }
            Expression::FunctionCall { id, arguments } => {
                println!("=== COMPILING FUNCTION CALL ===");
                // Compile arguments
                let mut compiled_args = Vec::new();
                for (i, arg) in arguments.iter().enumerate() {
                    let arg_val = self.compile_expression(builder, arg)?;
                    println!("Argument {}:{} compiled to: {:?}", i, arg, arg_val);
                    compiled_args.push(arg_val);
                }

                if id.name == "printf" {
                    // Expect exactly two arguments: format string and value
                    // if compiled_args.len() != 2 {
                    //     return Err(format!("printf expects exactly 2 arguments (format string, value), got {}", compiled_args.len()));
                    // }
                    let mut final_args = Vec::new();
                    let is_aarch64_apple = if let Some(ref isa) = self.isa {
                        let triple = isa.triple();
                        format!("{}", triple.architecture) == "aarch64" && format!("{}", triple.operating_system) == "darwin"
                    } else {
                        false
                    };
                    if is_aarch64_apple {
                        // For aarch64-apple-darwin, fixed arg (format string) first, pad to 8, then variadic args
                        final_args.push(compiled_args[0]); // format string (i64)

                        // Pad with zeros up to 8 arguments (including the format string)
                        while final_args.len() < 8 {
                            final_args.push(builder.ins().iconst(types::I64, 0));
                        }

                        // Push all variadic arguments (starting from compiled_args[1])
                        for arg in &compiled_args[1..] {
                            final_args.push(*arg);
                        }

                        // Dynamically adjust the signature to match the number of arguments
                        let func_id = self.functions.get("printf").unwrap();
                        let func_ref = self.module.as_mut().unwrap().declare_func_in_func(*func_id, &mut builder.func);
                        let sig_ref = builder.func.dfg.ext_funcs[func_ref].signature;
                        // Mutate the signature params to match the call site
                        let params = &mut builder.func.dfg.signatures[sig_ref].params;
                        params.clear();
                        for _ in 0..final_args.len() {
                            params.push(AbiParam::new(types::I64));
                        }
                        builder.func.dfg.signatures[sig_ref].returns.clear();
                        builder.func.dfg.signatures[sig_ref].returns.push(AbiParam::new(types::I32));
                        let call_inst = builder.ins().call(func_ref, &final_args);
                        let results = builder.inst_results(call_inst);
                        if let Some(&return_value) = results.first() {
                            Ok(return_value)
                        } else {
                            Ok(builder.ins().iconst(types::I32, 0))
                        }
                    } else {
                        // Non-arm64: use the global signature
                        let func_id = self.functions.get(&id.name)
                            .ok_or_else(|| format!("Function not found: {}", id.name))?;
                        let func_ref = self.module.as_mut().unwrap().declare_func_in_func(*func_id, &mut builder.func);
                        let sig_ref = builder.func.dfg.ext_funcs[func_ref].signature;
                        let params = &mut builder.func.dfg.signatures[sig_ref].params;
                        params.clear();
                        for _ in 0..compiled_args.len() {
                            params.push(AbiParam::new(types::I64));
                        }
                        let call_inst = builder.ins().call(func_ref, &compiled_args);
                        let results = builder.inst_results(call_inst);
                        if let Some(&return_value) = results.first() {
                            Ok(return_value)
                        } else {
                            Ok(builder.ins().iconst(types::I32, 0))
                        }
                    }
                } else {
                    // Regular function call (non-variadic)
                    let signature = self.function_signatures.get(&id.name)
                        .ok_or_else(|| format!("No signature found for function: {}", id.name))?
                        .clone();
                    let func_id = self.functions.get(&id.name)
                        .ok_or_else(|| format!("Function not found: {}", id.name))?;
                    let func_ref = self.module.as_mut().unwrap().declare_func_in_func(*func_id, &mut builder.func);
                    let call_inst = builder.ins().call(func_ref, &compiled_args);
                    let results = builder.inst_results(call_inst);
                    if let Some(&return_value) = results.first() {
                        Ok(return_value)
                    } else {
                        Ok(builder.ins().iconst(types::I64, 0))
                    }
                }
            }
            Expression::If { condition, then_branch, else_branch } => {
                let cond_val = self.compile_expression(builder, condition)?;
                
                // Create blocks for then, else, and merge
                let then_block = builder.create_block();
                let else_block = builder.create_block();
                let merge_block = builder.create_block();
                builder.append_block_param(merge_block, types::I64);

                // Branch based on condition
                builder.ins().brif(cond_val, then_block, &[], else_block, &[]);

                // Compile then branch
                builder.switch_to_block(then_block);
                // println!("Compiling then branch");
                let then_val = self.compile_expression(builder, then_branch)?;
                // println!("Then branch compiled to: {:?}", then_val);
                
                // Check if the last instruction is a return - if so, don't add a jump
                let should_add_jump = if let Some(last_inst) = builder.func.layout.last_inst(then_block) {
                    builder.func.dfg.insts[last_inst].opcode() != cranelift_codegen::ir::Opcode::Return
                } else {
                    true
                };
                
                if should_add_jump {
                    builder.ins().jump(merge_block, &[BlockArg::Value(then_val)]);
                    // println!("Jumped to merge block");
                } else {
                    // println!("Skipping jump due to return instruction");
                }
                builder.seal_block(then_block);

                // Compile else branch
                builder.switch_to_block(else_block);
                // println!("Compiling else branch");
                let else_val = if let Some(else_expr) = else_branch {
                    self.compile_expression(builder, else_expr)?
                } else {
                    builder.ins().iconst(types::I64, 0)
                };
                // println!("Else branch compiled to: {:?}", else_val);
                
                // Check if the last instruction is a return - if so, don't add a jump
                let should_add_jump = if let Some(last_inst) = builder.func.layout.last_inst(else_block) {
                    builder.func.dfg.insts[last_inst].opcode() != cranelift_codegen::ir::Opcode::Return
                } else {
                    true
                };
                
                if should_add_jump {
                    builder.ins().jump(merge_block, &[BlockArg::Value(else_val)]);
                } else {
                    // println!("Skipping jump due to return instruction");
                }
                builder.seal_block(else_block);

                // Merge block
                builder.switch_to_block(merge_block);
                builder.seal_block(merge_block);
                
                // For now, return the then value (this is a simplified approach)
                // TODO: In a proper implementation, you'd need to handle the merge differently
                Ok(builder.block_params(merge_block)[0])
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
                    
                    // Check if the last instruction in the current block is a return
                    // If so, we should stop processing further expressions
                    if let Some(last_inst) = builder.func.layout.last_inst(builder.current_block().unwrap()) {
                        if builder.func.dfg.insts[last_inst].opcode() == cranelift_codegen::ir::Opcode::Return {
                            // We've hit a return statement, so we're done with this block
                            break;
                        }
                    }
                }
                Ok(result)
            }
            Expression::Print(expr) => {
                let value = self.compile_expression(builder, expr)?;
                // For now, we'll just return the value
                // In a real implementation, you'd call a print function
                Ok(value)
            }
            Expression::FunctionDefinition { id, parameters, body, return_type_expr: _ } => {
                // Function definitions should be compiled during the "compile all functions" phase
                // When we encounter them during main expression compilation, we should skip them
                // or return a unit value. For now, we'll return a unit value (0).
                // This should not happen in normal flow since we skip function definitions in lower_to_clif
                // println!("Warning: Function definition '{}' encountered during main expression compilation", id.name);
                Ok(builder.ins().iconst(types::I64, 0))
            }
            Expression::Return(expr) => {
                // println!("Compiling return expression: {:?}", expr);
                let value = self.compile_expression(builder, expr.as_ref().unwrap())?;
                builder.ins().return_(&[value]);
                Ok(value)
            }
            Expression::StructDefinition { id, fields } => {
                // TODO: Implement struct definition
                Ok(builder.ins().iconst(types::I64, 0))
            }
            _ => Err(format!("Unsupported expression type: {:?}", expr))
        }
    }
} 
