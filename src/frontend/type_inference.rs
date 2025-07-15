use std::collections::HashMap;
use super::ast::{Expression, Program, BinaryOperator, TypeExpression, Id, Parameter};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    Float,
    Bool,
    String,
    Unit, // For void/unit type
    Function {
        parameters: Vec<Type>,
        return_type: Box<Type>,
    },
    Struct {
        name: String,
        fields: HashMap<String, Type>,
    },
    Union(Vec<Type>),
    Unknown(usize), // Type variable for inference
    Any, // Top type
    None, // Bottom type
}

#[derive(Debug, Clone)]
enum TypeConstraint {
    Equal(Type, Type),
    Subset(Type, Type),
}

impl From<TypeExpression> for Type {
    fn from(ty: TypeExpression) -> Self {
        match ty {
            TypeExpression::Int => Type::Int,
            TypeExpression::Float => Type::Float,
            TypeExpression::Bool => Type::Bool,
            TypeExpression::String => Type::String,
            TypeExpression::Void => Type::Unit,
            TypeExpression::Struct(id, fields) => match fields {
                Some(fields) => Type::Struct {
                    name: id.name.clone(),
                    fields: fields.iter().map(|field| (field.name().to_string(), Type::from(field.clone()))).collect(),
                },
                None => Type::Struct {
                    name: id.name.clone(),
                    fields: HashMap::new(),
                },
            },
        }
    }
}

impl From<Type> for TypeExpression {
    fn from(ty: Type) -> Self {
        match ty {
            Type::Int => TypeExpression::Int,
            Type::Float => TypeExpression::Float,
            Type::Bool => TypeExpression::Bool,
            Type::String => TypeExpression::String,
            Type::Unit => TypeExpression::Void,
            Type::Function { .. } => TypeExpression::Void, // TODO: Handle function types
            Type::Struct { name, .. } => TypeExpression::Struct(Id { name }, None),
            Type::Union(_) => TypeExpression::Int, // TODO: Handle union types
            Type::Unknown(_) => TypeExpression::Int, // TODO: Handle type variables
            Type::Any => TypeExpression::Int, // TODO: Handle any type
            Type::None => TypeExpression::Void, // TODO: Handle bottom type
        }
    }
}

#[derive(Debug)]
pub struct TypeEnvironment {
    types: HashMap<String, Type>,
    constraints: Vec<TypeConstraint>,
    next_type_var: usize,
}

impl TypeEnvironment {
    pub fn new() -> Self {
        TypeEnvironment {
            types: HashMap::new(),
            constraints: Vec::new(),
            next_type_var: 0,
        }
    }

    fn fresh_type_var(&mut self) -> Type {
        let var = Type::Unknown(self.next_type_var);
        self.next_type_var += 1;
        var
    }

    fn add_constraint(&mut self, constraint: TypeConstraint) {
        self.constraints.push(constraint);
    }

    fn get_type(&self, name: &str) -> Option<&Type> {
        self.types.get(name)
    }

    // Insert a type into the environment
    fn set_type(&mut self, name: String, ty: Type) {
        self.types.insert(name, ty);
    }

    fn type_from_expr(&self, expr: Expression) -> Type {
        match expr {
            Expression::Int(_) => Type::Int,
            Expression::Float(_) => Type::Float,
            Expression::Boolean(_) => Type::Bool,
            Expression::String(_) => Type::String,
            Expression::Identifier { id, type_expr } => Type::from(type_expr.unwrap()),
            //Expression::VariableAssignment { id, value } => self.type_from_expr(*value.clone()),
            Expression::FunctionCall { id, arguments } => {
                match self.get_type(&id.name) {
                    Some(ty) => ty.clone(),
                    None => Type::Any,
                }
            }
            _ => Type::Any,
        }
    }
}

pub struct TypeInferencer {
    env: TypeEnvironment,
}

impl TypeInferencer {
    pub fn new() -> Self {
        TypeInferencer {
            env: TypeEnvironment::new(),
        }
    }

    pub fn infer_program(&mut self, program: &Program) -> Result<Program, String> {
        println!("------ Typing program ------");
        // First pass: collect all function and struct declarations
        // TODO: Repeat while there are changes in the environment to account for recursive types
        self.collect_declarations(program)?;

        println!("------ Collected declarations ------");
        println!("------ Environment: {:?}", self.env);
        
        // Second pass: infer types for all expressions
        let typed_expressions = program.expressions.iter()
            .map(|expr| self.infer_expression(expr))
            .collect::<Result<Vec<_>, _>>()?;

        println!("------ Typed expressions ------");
        println!("------ Environment: {:?}", self.env);

        // Third pass: solve constraints
        self.solve_constraints()?;

        println!("------ Solved constraints ------");
        println!("------ Environment: {:?}", self.env);

        Ok(Program { expressions: typed_expressions })
    }

    fn collect_declarations(&mut self, program: &Program) -> Result<(), String> {
        for expr in &program.expressions {
            match expr {
                Expression::FunctionDefinition { id, parameters, body, return_type_expr } => {
                    // Create a fresh type variable for each parameter and the return type
                    let param_types: Vec<Type> = parameters.iter()
                        .map(|_| self.env.fresh_type_var())
                        .collect();
                    let return_type = match return_type_expr {
                        Some(ty) => Type::from(ty.clone()),
                        None => self.env.fresh_type_var(),
                    };
                    
                    self.env.set_type(
                        id.name.clone(),
                        Type::Function {
                            parameters: param_types,
                            return_type: Box::new(return_type),
                        }
                    );
                }
                Expression::StructDefinition { id, fields } => {
                    let mut field_types = HashMap::new();
                    for field in fields {
                        let field_type = Type::from(field.field_type.clone());
                        field_types.insert(
                            field.id.name.clone(),
                            field_type
                        );
                    }
                    
                    self.env.set_type(
                        id.name.clone(),
                        Type::Struct {
                            name: id.name.clone(),
                            fields: field_types,
                        }
                    );
                }
                Expression::Identifier { id, type_expr } => {
                    if let Some(ty) = type_expr {
                        self.env.set_type(
                            id.name.clone(), 
                            Type::from(ty.clone())
                        );
                    }
                }
                /* // This is not needed, because in this pass we are only adding types to the environment not assigning them to expressions
                Expression::VariableAssignment { id, value } => {
                    let value_type = self.env.type_from_expr(*value.clone());
                    self.env.set_type(
                        id.name.clone(),
                        value_type
                    );
                }
                */
                _ => {}
            }
        }
        Ok(())
    }

    fn infer_expression(&mut self, expr: &Expression) -> Result<Expression, String> {
        match expr {
            Expression::Int(_) => Ok(expr.clone()),
            Expression::Float(_) => Ok(expr.clone()),
            Expression::Boolean(_) => Ok(expr.clone()),
            Expression::String(_) => Ok(expr.clone()),
            Expression::Identifier { id, type_expr } => {
                let env_type = self.env.get_type(&id.name);

                if let Some(id_type_expr) = type_expr {
                    match env_type {
                        Some(ty) => {
                            if ty != &Type::from(id_type_expr.clone()) {
                                return Err(format!("Type mismatch for identifier: {:?}", id.name));
                            } else {
                                Ok(expr.clone())
                            }
                        }
                        None => {
                            println!("Typing | Missed type for identifier: {:?} in initial pass", id.name);
                            self.env.set_type(id.name.clone(), Type::from(id_type_expr.clone()));
                            Ok(expr.clone())
                        }
                    }
                } else {
                    match env_type {
                        Some(ty) => {
                            Ok(Expression::Identifier { id: id.clone(), type_expr: Some(TypeExpression::from(ty.clone())) })
                        }
                        None => {
                            let ty = self.env.fresh_type_var();
                            self.env.set_type(id.name.clone(), ty.clone());
                            Ok(Expression::Identifier { id: id.clone(), type_expr: Some(TypeExpression::from(ty.clone())) })
                        }
                    }
                }
            }
            Expression::BinaryOp { operator, left, right } => {
                let left_expr = self.infer_expression(left)?;
                let right_expr = self.infer_expression(right)?;
                
                // Add constraints based on operator
                match operator {
                    BinaryOperator::Assignment => {
                        // Variable assignment - set the type of the identifier
                        if let Expression::Identifier { id, .. } = &left_expr {
                            let right_type = self.env.type_from_expr(right_expr.clone());
                            self.env.set_type(id.name.clone(), right_type);
                        }
                    }
                    BinaryOperator::Add | BinaryOperator::Subtract 
                    | BinaryOperator::Multiply | BinaryOperator::Divide => {
                        // Numeric operations - both operands must be numeric
                        let numeric_types: Vec<Type> = 
                            vec![Type::Int, Type::Float];
                        
                        if let Some(left_type) = self.env.get_type(&format!("{:?}", left)) {
                            self.env.add_constraint(TypeConstraint::Subset(
                                left_type.clone(),
                                Type::Union(numeric_types.clone())
                            ));
                        }
                        
                        if let Some(right_type) = self.env.get_type(&format!("{:?}", right)) {
                            self.env.add_constraint(TypeConstraint::Subset(
                                right_type.clone(),
                                Type::Union(numeric_types)
                            ));
                        }
                    }
                    BinaryOperator::Equal | BinaryOperator::NotEqual => {
                        // Equality operations - operands must be of the same type
                        if let (Some(left_type), Some(right_type)) = (
                            self.env.get_type(&format!("{:?}", left)),
                            self.env.get_type(&format!("{:?}", right))
                        ) {
                            self.env.add_constraint(TypeConstraint::Equal(
                                left_type.clone(),
                                right_type.clone()
                            ));
                        }
                    }
                    _ => {}
                }
                
                Ok(Expression::BinaryOp {
                    operator: operator.clone(),
                    left: Box::new(left_expr),
                    right: Box::new(right_expr),
                })
            }
            Expression::FunctionCall { id, arguments } => {
                // Infer types for arguments
                let typed_args = arguments.iter()
                    .map(|arg| self.infer_expression(arg))
                    .collect::<Result<Vec<_>, _>>()?;
                
                // For multiple dispatch, we don't constrain based on function type
                // Instead, we track possible function types that could match
                
                Ok(Expression::FunctionCall {
                    id: id.clone(),
                    arguments: typed_args,
                })
            }
            Expression::FunctionDefinition { id, parameters, body, return_type_expr } => {
                let mut typed_params = Vec::new();
                for param in parameters {
                    let param_type = match &param.type_expr {
                        Some(ty) => Type::from(ty.clone()),
                        None => self.env.fresh_type_var(),
                    };
                    typed_params.push(Parameter {
                        id: param.id.clone(),
                        type_expr: Some(TypeExpression::from(param_type)),
                    });
                }
                let return_type = match return_type_expr {
                    Some(ty) => Type::from(ty.clone()), 
                    None => self.env.fresh_type_var(), // TODO: Try to infer from body
                };

                /* TODO: This is not neede here, right?
                self.env.set_type(
                    id.name.clone(),
                    Type::Function { parameters: param_types, return_type: Box::new(return_type) }
                );  
                */
                Ok(Expression::FunctionDefinition { id: id.clone(), parameters: typed_params, body: body.clone(), return_type_expr: Some(TypeExpression::from(return_type)) })
            }
            // TODO: This should set the type of the identifier
            /*Expression::VariableAssignment { id, value } => {
                let value_type = self.infer_expression(value)?;
                //self.env.set_type(id.name.clone(), value_type);
                Ok(Expression::VariableAssignment { id: id.clone(), value: Box::new(value_type) })
            }*/
            // Handle other expression types...
            _ => Ok(expr.clone()),
        }
    }

    fn solve_constraints(&mut self) -> Result<(), String> {
        // Simple constraint solver that handles basic type equality and subset constraints
        // In a real implementation, this would be more sophisticated
        
        let mut changed = true;
        while changed {
            changed = false;
            
            // Take ownership of constraints temporarily
            let constraints = std::mem::take(&mut self.env.constraints);
            for constraint in constraints {
                let mut changed = false;
                match constraint.clone() {
                    TypeConstraint::Equal(t1, t2) => {
                        if let Err(err) = self.unify(&t1, &t2) {
                            return Err(err);
                        }
                        changed = true;
                    }
                    TypeConstraint::Subset(t1, t2) => {
                        if let Err(err) = self.subset(&t1, &t2) {
                            return Err(err);
                        }
                        changed = true;
                    }
                };
                // Add back any constraints that still need processing
                if !changed {
                    self.env.constraints.push(constraint);
                }
            }
        }
        
        Ok(())
    }

    fn unify(&mut self, t1: &Type, t2: &Type) -> Result<(), String> {
        match (t1, t2) {
            (Type::Unknown(var1), Type::Unknown(var2)) if var1 != var2 => {
                // Both are type variables, unify them
                // TODO: Here you would typically update the environment to reflect this
                Ok(())
            }
            (Type::Unknown(var), _) => {
                // Unify type variable with a concrete type
                // TODO: 
                // Check for circular references
                // Update the environment to replace the variable with the concrete type
                Ok(())
            }
            (_, Type::Unknown(var)) => {
                // Unify concrete type with a type variable
                // TODO: 
                // Check for circular references
                // Update the environment to replace the variable with the concrete type
                Ok(())
            }
            (Type::Int, Type::Int) | (Type::Float, Type::Float) | (Type::Bool, Type::Bool) | (Type::String, Type::String) => {
                // Trivial unification for primitive types
                Ok(())
            }
            (Type::Function { parameters: params1, return_type: ret1 }, Type::Function { parameters: params2, return_type: ret2 }) => {
                // Unify function types
                if params1.len() != params2.len() {
                    return Err("Function parameter lengths do not match".to_string());
                }
                for (p1, p2) in params1.iter().zip(params2.iter()) {
                    self.unify(p1, p2)?;
                }
                self.unify(ret1, ret2)
            }
            (Type::Struct { name: name1, fields: fields1 }, Type::Struct { name: name2, fields: fields2 }) if name1 == name2 => {
                // Unify struct types
                for (key, field1) in fields1 {
                    if let Some(field2) = fields2.get(key) {
                        self.unify(field1, field2)?;
                    } else {
                        return Err(format!("Field {} not found in struct {}", key, name2));
                    }
                }
                Ok(())
            }
            (Type::Union(set1), Type::Union(set2)) => {
                // Unify union types
                // TODO: Can this be improved to check if there's a common type in the union?
                for t1 in set1 {
                    for t2 in set2 {
                        self.unify(t1, t2)?;
                    }
                }
                Ok(())
            }
            _ => {
                // Types cannot be unified
                Err(format!("Cannot unify types: {:?} and {:?}", t1, t2))
            }
        }
    }

    fn subset(&mut self, s1: &Type, s2: &Type) -> Result<(), String> {
        match (s1, s2) {
            (Type::Union(s1), Type::Union(s2)) => {
                if !s1.iter().all(|t1| s2.iter().any(|t2| t1.is_compatible_with(t2))) {
                    return Err(format!("Type {:?} is not a subset of {:?}", s1, s2));
                }
                Ok(())
            }
            (t1, Type::Union(s2)) => {
                if !s2.iter().any(|t2| t1.is_compatible_with(t2)) {
                    return Err(format!("Type {:?} is not a subset of {:?}", t1, s2));
                }
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn is_subset(&self, s1: &[Type], s2: &[Type]) -> bool {
        s1.iter().all(|t1| s2.iter().any(|t2| t1.is_compatible_with(t2)))
    }
}

// Helper functions for type operations
impl Type {
    fn is_numeric(&self) -> bool {
        matches!(self, Type::Int | Type::Float)
    }
    
    fn is_compatible_with(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Union(s1), Type::Union(s2)) => {
                s1.iter().any(|t1| s2.iter().any(|t2| t1.is_compatible_with(t2)))
            }
            (Type::Union(s), t) | (t, Type::Union(s)) => s.iter().any(|st| st.is_compatible_with(t)),
            (t1, t2) => t1 == t2,
        }
    }

    fn is_subset(&self, other: &[Type]) -> bool {
        match self {
            Type::Union(types) => types.iter().all(|t| other.iter().any(|o| t.is_compatible_with(o))),
            _ => other.iter().any(|t| self.is_compatible_with(t))
        }
    }

    fn is_disjoint(&self, other: &[Type]) -> bool {
        match self {
            Type::Union(types) => !types.iter().any(|t| other.iter().any(|o| t.is_compatible_with(o))),
            _ => !other.iter().any(|t| self.is_compatible_with(t))
        }
    }
}
