use std::collections::HashMap;
use std::collections::VecDeque;
use crate::frontend::ast::{Expression, Program, Id, TypeExpression};

#[derive(Debug)]
pub struct Resolver {
    scopes: VecDeque<HashMap<String, bool>>,
    errors: Vec<String>,
    locals: HashMap<String, usize>,
}

impl Resolver {
    pub fn new() -> Self {
        Resolver {
            scopes: VecDeque::new(),
            errors: Vec::new(),
            locals: HashMap::new(),
        }
    }

    pub fn resolve(&mut self, program: &Program) -> Result<HashMap<String, usize>, Vec<String>> {
        println!("------ Resolver start ------");
        self.begin_scope(); // Global scope
        println!("Res.resolve| Global scope: {:?}, locals: {:?}", self.scopes, self.locals);
        
        for expr in &program.expressions {
            self.resolve_expr(expr);
        }
        
        self.end_scope();
        
        println!("------ Resolver end ------");

        if self.errors.is_empty() {
            Ok(self.locals.clone())
        } else {
            Err(self.errors.clone())
        }
    }

    fn resolve_expr(&mut self, expr: &Expression) -> Result<(), String> {
        match expr {
            Expression::Int(_) | Expression::Float(_) | Expression::Boolean(_) | Expression::String(_) | Expression::Symbol(_) => Ok(()),
            Expression::Identifier { id, type_expr: _ } => self.resolve_identifier(id),
            Expression::UnaryOp { operand, .. } => self.resolve_expr(operand),
            Expression::BinaryOp { left, right, .. } => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)
            },
            Expression::FunctionCall { id, arguments } => {
                self.resolve_identifier(id)?;
                for arg in arguments {
                    self.resolve_expr(arg)?;
                }
                Ok(())
            },
            Expression::FunctionDefinition { id, parameters, body, return_type_expr, foreign: _ } => {
                // Declare the function name in the current scope
                self.declare(id)?;
                self.define(id);
                
                // Create new scope for function body
                self.begin_scope();
                println!("Res.resolve_expr.fn_def| Function scope: {:?}, locals: {:?}", self.scopes, self.locals);
                
                // Declare and define parameters
                for param in parameters {
                    self.declare(&param.id)?;
                    self.define(&param.id);
                }
                
                // Resolve the function body
                match body.as_ref() {
                    Expression::Block(expressions) => {
                        for expr in expressions {
                            println!("Res.resolve_expr.fn_def| Resolving expr: {:?}", expr);
                            self.resolve_expr(&expr)?;
                        }
                    }
                    _ => return Err(format!("Invalid function body: {:?}", body))
                }
                //self.resolve_expr(body)?;
                
                self.end_scope();
                Ok(())
            },
            Expression::StructDefinition { id, fields } => {
                self.declare(id)?;
                self.define(id);
                
                // Create scope for struct fields
                self.begin_scope();
                println!("Res.resolve_expr.struct_def| Struct scope: {:?}, locals: {:?}", self.scopes, self.locals);

                for field in fields {
                    self.declare(&field.id)?;
                    self.define(&field.id);
                }
                
                self.end_scope();
                Ok(())
            },
            Expression::If { condition, then_branch, else_branch } => {
                self.resolve_expr(condition)?;
                self.resolve_expr(then_branch)?;
                if let Some(else_branch) = else_branch {
                    self.resolve_expr(else_branch)?;
                }
                Ok(())
            },
            Expression::While { condition, body } => {
                self.resolve_expr(condition)?;
                self.resolve_expr(body)
            },
            Expression::For { iterator, range, body } => {
                self.resolve_expr(range)?;
                
                // Create new scope for iterator variable
                self.begin_scope();
                println!("Res.resolve_expr.for| For scope: {:?}, locals: {:?}", self.scopes, self.locals);

                self.declare(iterator)?;
                self.define(iterator);
                
                self.resolve_expr(body)?;
                
                self.end_scope();
                Ok(())
            },
            Expression::Block(expressions) => {
                self.begin_scope();
                println!("Res.resolve_expr.block| Block scope: {:?}, locals: {:?}", self.scopes, self.locals);

                for expr in expressions {
                    self.resolve_expr(expr)?;
                }
                self.end_scope();
                Ok(())
            },
            Expression::Return(expr) => {
                if let Some(expr) = expr {
                    self.resolve_expr(expr)?;
                }
                Ok(())
            },
            Expression::Print(expr) => self.resolve_expr(expr),
            // Expression::VariableAssignment { id, value } => {
            //     println!("Res.resolve_expr.var_assign| Variable assignment: {:?}, value: {:?}", id.name, value);
            //     // Check if the variable is already declared
            //     if self.locals.contains_key(&id.name) {
            //         println!("Res.resolve_expr.var_assign| Variable already declared: {:?}", id.name);
            //         // If declared, resolve the value for assignment
            //         self.resolve_expr(value)?;
            //         self.resolve_local(id)?;
            //     } else {
            //         println!("Res.resolve_expr.var_assign| Variable not declared: {:?}", id.name);
            //         // If not declared, declare and initialize
            //         self.declare(id)?;
            //         self.resolve_expr(value)?;
            //         self.define(id);
            //         self.resolve_local(id)?;
            //     }
            //     Ok(())
            // },
        }
    }

    fn resolve_identifier(&mut self, id: &Id) -> Result<(), String> {
        if let Some(scope) = self.scopes.front() {
            if let Some(&initialized) = scope.get(&id.name) {
                if !initialized {
                    return Err(format!("Cannot read local variable '{}' in its own initializer", id.name));
                }
            }
        }
        
        self.resolve_local(id)?;
        return Ok(());
    }

    fn begin_scope(&mut self) {
        self.scopes.push_front(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop_front();
    }

    fn declare(&mut self, id: &Id) -> Result<(), String> {
        if let Some(scope) = self.scopes.front_mut() {
            if scope.contains_key(&id.name) {
                return Err(format!("Variable '{}' already declared in this scope", id.name));
            }
            scope.insert(id.name.clone(), false);
        }
        Ok(())
    }

    fn define(&mut self, id: &Id) {
        if let Some(scope) = self.scopes.front_mut() {
            scope.insert(id.name.clone(), true);
        }
    }

    fn resolve_local(&mut self, id: &Id) -> Result<(), String> {
        // New implementation
        for i in (0..self.scopes.len()).rev() {
            if self.scopes[i].contains_key(&id.name) {
                let depth = self.scopes.len() - 1 - i;
                self.locals.insert(id.name.clone(), depth);
                return Ok(());
            }
        }
        Err(format!("Undefined variable '{}'", id.name))
    }
}
