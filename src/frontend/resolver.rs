use std::collections::HashMap;
use std::collections::VecDeque;
use crate::frontend::ast::{Expression, Program, Id, Parameter};

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
        self.begin_scope(); // Global scope
        
        for expr in &program.expressions {
            self.resolve_expr(expr)?;
        }
        
        self.end_scope();
        
        if self.errors.is_empty() {
            Ok(self.locals.clone())
        } else {
            Err(self.errors.clone())
        }
    }

    fn resolve_expr(&mut self, expr: &Expression) -> Result<(), String> {
        match expr {
            Expression::Number(_) | Expression::Boolean(_) | Expression::String(_) => Ok(()),
            Expression::Identifier(id) => self.resolve_identifier(id),
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
            Expression::FunctionDefinition { id, parameters, body } => {
                // Declare the function name in the current scope
                self.declare(id)?;
                self.define(id);
                
                // Create new scope for function body
                self.begin_scope();
                
                // Declare and define parameters
                for param in parameters {
                    self.declare(&param.id)?;
                    self.define(&param.id);
                }
                
                // Resolve the function body
                self.resolve_expr(body)?;
                
                self.end_scope();
                Ok(())
            },
            Expression::StructDefinition { id, fields } => {
                self.declare(id)?;
                self.define(id);
                
                // Create scope for struct fields
                self.begin_scope();
                
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
                self.declare(iterator)?;
                self.define(iterator);
                
                self.resolve_expr(body)?;
                
                self.end_scope();
                Ok(())
            },
            Expression::Block(expressions) => {
                self.begin_scope();
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
            Expression::VariableAssignment { id, value } => {
                self.resolve_expr(value)?;
                self.declare(id)?;
                self.define(id);
                self.resolve_local(id)?;
                Ok(())
            },
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
        for (i, scope) in self.scopes.iter().enumerate().rev() {
            if scope.contains_key(&id.name) {
                let depth = i;
                self.locals.insert(id.name.clone(), depth);
                return Ok(());
            }
        }
        Err(format!("Undefined variable '{}'", id.name))
    }
}
