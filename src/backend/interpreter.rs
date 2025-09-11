use crate::{compiler_debug};
use crate::frontend::ast::{Expression, Program, BinaryOperator, UnaryOperator, Id, Parameter, FieldDefinition, TypeExpression};
use crate::backend::environment::Environment;
use std::collections::HashMap;
use std::any::Any;
use std::error::Error;
use std::fmt;
use std::rc::Rc;
use std::cell::RefCell;


#[derive(Debug)]
pub enum ControlFlow<R, E> {
    Return(R),
    Err(E),
}

impl<R, E: Error> From<E> for ControlFlow<R, E> {
    fn from(err: E) -> Self {
        ControlFlow::Err(err)
    }
}

trait Callable {
    fn arity(&self) -> usize;

    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<Value>) -> Result<Value, String>;

    fn as_any(&self) -> &dyn Any;
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub parameters: Vec<String>,
    pub body: Expression,
    pub return_type_expr: Option<TypeExpression>,
    pub environment: Rc<RefCell<Environment>>,
}

impl Callable for Function {
    fn arity(&self) -> usize {
        self.parameters.len()
    }

    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<Value>) -> Result<Value, String> {
        compiler_debug!("Int.call| Function: {} | {:?}", self.name, self.parameters);
        if arguments.len() != self.arity() {
            return Err(format!("Incorrect number of arguments: expected {}, got {}", self.arity(), arguments.len()));
        }

        interpreter.in_function = true;
        let env = Environment::new(Rc::clone(&self.environment));
        compiler_debug!("Func.call| New level: {}", env.borrow());

        // Set the arguments in the function environment
        for (param, arg) in self.parameters.iter().zip(arguments) {
            env.borrow_mut().define(param.clone(), arg);
        }
        compiler_debug!("Int.call| Environment after defining arguments: [{}]", env.borrow());

        let result = match self.body.clone() {
            Expression::Block(expressions) => {
                interpreter.eval_block(&expressions, env)
            },
            // Parser should ensure that the body is a block
            _ => return Err(format!("Function body is not a block: {:?}", self.body)),
        };

        //let result = interpreter.eval_block(&[Box::new(self.body.clone())], env);
        compiler_debug!("Int.call| Result: {:?}", result);

        match result {
            Ok(value) => Ok(value),
            Err(e) => match e {
                ControlFlow::Return(val) => Ok(val),
                ControlFlow::Err(err) => Err(err),
            },
        }
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug, Clone)]
pub struct Type {
    pub name: String,
    pub fields: HashMap<String, Box<Type>>,
}

impl Callable for Type {
    fn arity(&self) -> usize {
        self.fields.len()
    }

    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<Value>) -> Result<Value, String> {
        if arguments.len() != self.arity() {
            return Err(format!("Incorrect number of arguments: expected {}, got {}", self.arity(), arguments.len()));
        }

        let fields = self.fields.iter()
            .enumerate()
            .map(|(index, (key, _))| {
                let value = arguments[index].clone();
                (key.clone(), value)
            })
            .collect::<HashMap<String, Value>>();

        let instance = Instance {
            of_type: self.clone(),
            values: fields,
        };
        Ok(Value::Instance(instance))
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug, Clone)]
pub struct Instance {
    pub of_type: Type,
    pub values: HashMap<String, Value>,
}

pub enum Value {
    Int(i64),
    Float(f64),
    Boolean(bool),
    String(String),
    Function(Function),
    Type(Type),
    //Callable(Box<dyn Callable>),
    Instance(Instance),
    Unit,
}

impl Value {
    pub fn is_truthy(&self) -> Result<bool, ControlFlow<Value, String>> {
        match self {
            Value::Boolean(b) => Ok(*b),
            _ => Err(ControlFlow::Err("Value is not a boolean".to_string())),
        }
    }

    pub fn to_type_expr(&self) -> TypeExpression {
        match self {
            Value::Int(_) => TypeExpression::Int,
            Value::Float(_) => TypeExpression::Float,
            Value::Boolean(_) => TypeExpression::Bool,
            Value::String(_) => TypeExpression::String,
            Value::Function(f) =>  f.return_type_expr.clone().unwrap_or(TypeExpression::Void),
            Value::Type(t) => TypeExpression::Struct(
                Id { name: t.name.clone() }, 
                Some(t.fields.iter().map(|(k, v)| Value::Type(*v.clone()).to_type_expr()).collect())
            ),
            Value::Instance(i) => Value::Type(i.of_type.clone()).to_type_expr(),
            Value::Unit => TypeExpression::Void,
        }
    }
}

// Implement Clone for Value manually
impl Clone for Value {
    fn clone(&self) -> Self {
        match self {
            Value::Int(n) => Value::Int(*n),
            Value::Float(n) => Value::Float(*n),
            Value::Boolean(b) => Value::Boolean(*b),
            Value::String(s) => Value::String(s.clone()),
            //Value::Callable(c) => Value::Callable(*c.clone()), // Clone the Box<dyn Callable>
            Value::Function(f) => Value::Function(f.clone()),
            Value::Type(t) => Value::Type(t.clone()),
            Value::Instance(i) => Value::Instance(i.clone()),
            Value::Unit => Value::Unit,
        }
    }
}

// Implement Debug for Value manually
impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::Int(n) => write!(f, "Int({})", n),
            Value::Float(n) => write!(f, "Float({})", n),
            Value::Boolean(b) => write!(f, "Boolean({})", b),
            Value::String(s) => write!(f, "String({})", s),
            //Value::Callable(_) => write!(f, "Callable(<function>)"), // Indicate it's a callable
            Value::Function(func) => write!(f, "Function({:?})", func),
            Value::Type(t) => write!(f, "Type({:?})", t),
            Value::Instance(i) => write!(f, "Instance({:?})", i),
            Value::Unit => write!(f, "Unit"),
        }
    }
}

impl From<Value> for Expression {
    fn from(value: Value) -> Self {
        match value {
            Value::Int(n) => Expression::Int(n),
            Value::Float(n) => Expression::Float(n),
            Value::Boolean(b) => Expression::Boolean(b),
            Value::String(s) => Expression::String(s),
            Value::Function(f) => Expression::FunctionDefinition {
                id: Id { name: f.name.clone() },
                parameters: f.parameters.iter().map(|p| Parameter { id: Id { name: p.clone() }, type_expr: None }).collect(),
                body: Box::new(f.body.clone()),
                return_type_expr: None,
                foreign: false,
            },
            Value::Type(t) => Expression::StructDefinition {
                id: Id { name: t.name },
                fields: t.fields.iter().map(|(k, v)| FieldDefinition { 
                    id: Id { name: k.clone() }, 
                    field_type: Value::Type(*v.clone()).to_type_expr() // Recursively convert Value to Expression
                }).collect(),
            },
            Value::Instance(i) => Expression::Identifier{ id: Id { name: "_".to_string() }, type_expr: Some(Value::Type(i.of_type).to_type_expr()) }, // TODO: Fix this, it is not an identifier
            Value::Unit => Expression::Block(vec![]), // Represent Unit as empty block
        }
    }
}

pub struct Interpreter {
    environment: Rc<RefCell<Environment>>,
    local_scope: HashMap<String, usize>,
    in_function: bool,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            environment: Environment::new_global(),
            local_scope: HashMap::new(),
            in_function: false,
        }
    }

    pub fn update_local_scope(&mut self, local_scope: HashMap<String, usize>) {
        self.local_scope = local_scope;
    }

    pub fn interpret(&mut self, program: &Program) -> Result<Value, String> {
        let mut result = Value::Unit;
        for expr in &program.expressions {
            match self.eval_expression(expr) {
                Ok(value) => result = value,
                Err(flow) => match flow {
                    ControlFlow::Return(val) => unreachable!(),
                    ControlFlow::Err(err) => return Err(err),
                },
            }
        }
        Ok(result)
    }

    fn eval_expression(&mut self, expr: &Expression) -> Result<Value, ControlFlow<Value, String>> {
        let result = match expr {
            Expression::Int(_) | Expression::Float(_) | Expression::Boolean(_) | Expression::String(_) | Expression::Symbol(_) => self.eval_literal(expr),
            Expression::Identifier { id, type_expr } => self.eval_identifier(id, type_expr),
            Expression::UnaryOp { operator, operand } => self.eval_unary_op(operator, operand),
            Expression::BinaryOp { operator, left, right } => self.eval_binary_op(operator, left, right),
            Expression::FunctionCall { id, arguments } => self.eval_call(id, arguments),
            Expression::FunctionDefinition { id, parameters, body, return_type_expr, foreign: _ } => self.eval_function_definition(id, parameters, body, return_type_expr),
            Expression::StructDefinition { id, fields } => self.eval_struct_definition(id, fields),
            Expression::If { condition, then_branch, else_branch } => self.eval_if(condition, then_branch, else_branch),
            Expression::While { condition, body } => self.eval_while(condition, body),
            Expression::Block(expressions) => {
                let env = Environment::new(Rc::clone(&self.environment));
                compiler_debug!("Int.eval.expr.block| New level: {}", env.borrow());
                self.eval_block(expressions, env)
            },
            Expression::Return(expr) => self.eval_return(expr),
            Expression::Print(expr) => self.eval_print(expr),
            //Expression::VariableAssignment { id, value } => self.eval_variable_assignment(id, value),
            Expression::For { iterator, range, body } => Err(ControlFlow::Err("For loops not implemented in interpreter, please desugar".to_string())),
        };
        compiler_debug!("Int.eval.expr| Result: {:?}", result);
        result
    }

    fn eval_identifier(&self, id: &Id, type_expr: &Option<TypeExpression>) -> Result<Value, ControlFlow<Value, String>> {
        // Print the current environment for debugging
        compiler_debug!("Int.eval.identifier| Identifier: {}", id.name);
        //compiler_debug!("Int.eval| Current environment: {}", self.environment);
        
        self.environment.borrow()
            .get(&id.name)
            .ok_or_else(|| ControlFlow::Err(format!("Undefined variable: {}", id.name)))
    }

    fn eval_literal(&self, expr: &Expression) -> Result<Value, ControlFlow<Value, String>> {
        match expr {
            Expression::Int(n) => Ok(Value::Int(*n)),
            Expression::Float(n) => Ok(Value::Float(*n)),
            Expression::Boolean(b) => Ok(Value::Boolean(*b)),
            Expression::String(s) => Ok(Value::String(s.clone())),
            Expression::Symbol(s) => Ok(Value::String(s.clone())),
            _ => Err(ControlFlow::Err(format!("Cannot convert expression to value: {:?}", expr)))
        }
    }

    fn eval_unary_op(&mut self, operator: &UnaryOperator, operand: &Expression) -> Result<Value, ControlFlow<Value, String>> {
        let operand_val = self.eval_expression(operand)?;
        match operator {
            UnaryOperator::Not => {
                if let Value::Boolean(b) = operand_val {
                    Ok(Value::Boolean(!b))
                } else {
                    Err(ControlFlow::Err("Cannot apply 'not' to non-boolean value".to_string()))
                }
            }
            UnaryOperator::Negate => {
                if let Value::Int(n) = operand_val {
                    Ok(Value::Int(-n))
                } else if let Value::Float(n) = operand_val {
                    Ok(Value::Float(-n))
                } else {
                    Err(ControlFlow::Err("Cannot negate non-numeric value".to_string()))
                }
            }
        }
    }

    fn handle_div_by_zero(&self, operator: &BinaryOperator, left: &Value, right: &Value) -> Result<Value, ControlFlow<Value, String>> {
        match (left, right) {
            (Value::Int(l), Value::Int(r)) => {
                if *r == 0 {
                    Err(ControlFlow::Err("Division by zero".to_string()))
                } else {
                    Ok(Value::Int(l / r))
                }
            }
            (Value::Float(l), Value::Float(r)) => {
                if *r == 0.0 {
                    Err(ControlFlow::Err("Division by zero".to_string()))
                } else {
                    Ok(Value::Float(l / r))
                }
            }
            (Value::Int(l), Value::Float(r)) => {
                if *r == 0.0 {
                    Err(ControlFlow::Err("Division by zero".to_string()))
                } else {
                    Ok(Value::Float(*l as f64 / r))
                }
            }
            (Value::Float(l), Value::Int(r)) => {
                if *r == 0 {
                    Err(ControlFlow::Err("Division by zero".to_string()))
                } else {
                    Ok(Value::Float(l / *r as f64))
                }
            }
            _ => Err(ControlFlow::Err("Invalid operands for division".to_string()))
        }
    }

    fn eval_binary_op(&mut self, operator: &BinaryOperator, left: &Expression, right: &Expression) -> Result<Value, ControlFlow<Value, String>> {
        let left_val = self.eval_expression(left)?;
        let right_val = self.eval_expression(right)?;

        match (operator, &left_val, &right_val) {
            (BinaryOperator::Assignment, left_val, right_val) => {
                if let Expression::Identifier { id, type_expr } = left {
                    let distance = self.local_scope.get(&id.name).unwrap();
                    compiler_debug!("Int.eval.bin_op| Assigning {:?} = {:?} at {:?} distance to [{}]", id.name, right_val, distance, self.environment.borrow());
                    self.environment.borrow_mut().assign(*distance, id.name.clone(), right_val.clone());
                    Ok(right_val.clone())
                } else {
                    Err(ControlFlow::Err("Left operand is not an identifier".to_string()))
                }
            }
            // TODO: Refactor to better handle mix of int and float
            (BinaryOperator::Add, Value::Int(l), Value::Int(r)) => Ok(Value::Int(l + r)),
            (BinaryOperator::Add, Value::Float(l), Value::Float(r)) => Ok(Value::Float(l + r)),
            (BinaryOperator::Add, Value::Int(l), Value::Float(r)) => Ok(Value::Float(*l as f64 + r)),
            (BinaryOperator::Add, Value::Float(l), Value::Int(r)) => Ok(Value::Float(l + *r as f64)),
            (BinaryOperator::Add, Value::String(l), Value::String(r)) => Ok(Value::String(l.to_owned() + r)),
            (BinaryOperator::Subtract, Value::Int(l), Value::Int(r)) => Ok(Value::Int(l - r)),
            (BinaryOperator::Subtract, Value::Float(l), Value::Float(r)) => Ok(Value::Float(l - r)),
            (BinaryOperator::Subtract, Value::Int(l), Value::Float(r)) => Ok(Value::Float(*l as f64 - r)),
            (BinaryOperator::Subtract, Value::Float(l), Value::Int(r)) => Ok(Value::Float(l - *r as f64)),
            (BinaryOperator::Multiply, Value::Int(l), Value::Int(r)) => Ok(Value::Int(l * r)),
            (BinaryOperator::Multiply, Value::Float(l), Value::Float(r)) => Ok(Value::Float(l * r)),
            (BinaryOperator::Multiply, Value::Int(l), Value::Float(r)) => Ok(Value::Float(*l as f64 * r)),
            (BinaryOperator::Multiply, Value::Float(l), Value::Int(r)) => Ok(Value::Float(l * *r as f64)),
            (BinaryOperator::Divide, Value::Int(l), Value::Int(r)) => self.handle_div_by_zero(operator, &left_val, &right_val),
            (BinaryOperator::Divide, Value::Float(l), Value::Float(r)) => self.handle_div_by_zero(operator, &left_val, &right_val),
            (BinaryOperator::Divide, Value::Int(l), Value::Float(r)) => self.handle_div_by_zero(operator, &left_val, &right_val),
            (BinaryOperator::Divide, Value::Float(l), Value::Int(r)) => self.handle_div_by_zero(operator, &left_val, &right_val),
            // TODO: Implement eval_equality to get better comparison results
            // Arguments can be of different types, so we need to implement a better comparison
            (BinaryOperator::Equal, l, r) => Ok(Value::Boolean(format!("{:?}", l) == format!("{:?}", r))),
            (BinaryOperator::NotEqual, l, r) => Ok(Value::Boolean(format!("{:?}", l) != format!("{:?}", r))),
            (BinaryOperator::LessThan, Value::Int(l), Value::Int(r)) => Ok(Value::Boolean(l < r)),
            (BinaryOperator::LessThan, Value::Float(l), Value::Float(r)) => Ok(Value::Boolean(l < r)),
            (BinaryOperator::LessThan, Value::Int(l), Value::Float(r)) => Ok(Value::Boolean((*l as f64) < *r)),
            (BinaryOperator::LessThan, Value::Float(l), Value::Int(r)) => Ok(Value::Boolean(*l < (*r as f64))),
            (BinaryOperator::LessThanEqual, Value::Int(l), Value::Int(r)) => Ok(Value::Boolean(l <= r)),
            (BinaryOperator::LessThanEqual, Value::Float(l), Value::Float(r)) => Ok(Value::Boolean(l <= r)),
            (BinaryOperator::LessThanEqual, Value::Int(l), Value::Float(r)) => Ok(Value::Boolean((*l as f64) <= *r)),
            (BinaryOperator::LessThanEqual, Value::Float(l), Value::Int(r)) => Ok(Value::Boolean(*l <= (*r as f64))),
            (BinaryOperator::GreaterThan, Value::Int(l), Value::Int(r)) => Ok(Value::Boolean(l > r)),
            (BinaryOperator::GreaterThan, Value::Float(l), Value::Float(r)) => Ok(Value::Boolean(l > r)),
            (BinaryOperator::GreaterThan, Value::Int(l), Value::Float(r)) => Ok(Value::Boolean((*l as f64) > *r)),
            (BinaryOperator::GreaterThan, Value::Float(l), Value::Int(r)) => Ok(Value::Boolean(*l > (*r as f64))),
            (BinaryOperator::GreaterThanEqual, Value::Int(l), Value::Int(r)) => Ok(Value::Boolean(l >= r)),
            (BinaryOperator::GreaterThanEqual, Value::Float(l), Value::Float(r)) => Ok(Value::Boolean(l >= r)),
            (BinaryOperator::GreaterThanEqual, Value::Int(l), Value::Float(r)) => Ok(Value::Boolean((*l as f64) >= *r)),
            (BinaryOperator::GreaterThanEqual, Value::Float(l), Value::Int(r)) => Ok(Value::Boolean(*l >= (*r as f64))),
            // TODO: Move these, to take advantage of short-circuiting
            (BinaryOperator::And, Value::Boolean(l), Value::Boolean(r)) => Ok(Value::Boolean(*l && *r)),
            (BinaryOperator::Or, Value::Boolean(l), Value::Boolean(r)) => Ok(Value::Boolean(*l || *r)),
            _ => Err(ControlFlow::Err(format!("Invalid operation {:?} between {:?} and {:?}", operator, left_val, right_val)))
        }
    }

    fn eval_call(&mut self, id: &Id, arguments: &[Expression]) -> Result<Value, ControlFlow<Value, String>> {
        compiler_debug!("Int.eval.call| Call: {}", id.name);
        let ident = self.environment.borrow()
            .get(&id.name)
            .ok_or_else(|| ControlFlow::Err(format!("Undefined function: {}", id.name)))?;

        let args = arguments.iter()
            .map(|arg| self.eval_expression(arg))
            .collect::<Result<Vec<_>, _>>()?;

        compiler_debug!("Int.eval.call| args: {:?}", args);
        match ident {
            Value::Function(f) => f.call(self, args).map_err(ControlFlow::Err),
            Value::Type(t) => t.call(self, args).map_err(ControlFlow::Err),
            _ => Err(ControlFlow::Err(format!("Value is not callable: {:?}", ident))),
        }
    }

    fn eval_function_definition(&mut self, id: &Id, parameters: &[Parameter], body: &Expression, return_type_expr: &Option<TypeExpression>) -> Result<Value, ControlFlow<Value, String>> {
        compiler_debug!("Int.eval.fn_def| Function definition: {}", id.name);
        let function = Function {
            name: id.name.clone(),
            parameters: parameters.iter().map(|p| p.id.name.clone()).collect(),
            body: body.clone(),
            return_type_expr: return_type_expr.clone(),
            environment: Rc::clone(&self.environment),
        };

        let callable_function = Value::Function(function);
        self.environment.borrow_mut().define(id.name.clone(), callable_function);
        compiler_debug!("Int.eval.fn_def| Environment after function definition: {}", self.environment.borrow());
        Ok(Value::Unit)
    }

    fn eval_struct_definition(&mut self, id: &Id, fields: &[FieldDefinition]) -> Result<Value, ControlFlow<Value, String>> {
        let type_name = id.name.clone();
        let mut field_map = HashMap::new();

        for field in fields {
            let field_type = self.environment.borrow()
                .get(&field.field_type.name())
                .ok_or_else(|| ControlFlow::Err(format!("Undefined type: {}", field.field_type.name())))?;

            match field_type {
                Value::Type(t) => {
                    field_map.insert(field.id.name.clone(), Box::new(t.clone()));
                }
                _ => return Err(ControlFlow::Err(format!("{} is not a valid Type", field.field_type.name()))),
            }
        }

        let type_def = Type {
            name: type_name,
            fields: field_map,
        };

        let callable_type = Value::Type(type_def);
        self.environment.borrow_mut().define(id.name.clone(), callable_type);
        Ok(Value::Unit)
    }

    fn eval_if(&mut self, condition: &Expression, then_branch: &Expression, else_branch: &Option<Box<Expression>>) -> Result<Value, ControlFlow<Value, String>> {
        let cond_val = self.eval_expression(condition)?;
        match cond_val {
            Value::Boolean(true) => self.eval_expression(then_branch),
            Value::Boolean(false) => {
                if let Some(else_expr) = else_branch {
                    self.eval_expression(else_expr)
                } else {
                    Ok(Value::Unit)
                }
            }
            // Assume values are truthy
            Value::Int(_) | Value::Float(_) | Value::String(_) | Value::Function(_) | Value::Type(_) | Value::Instance(_) => self.eval_expression(then_branch),
            Value::Unit => Err(ControlFlow::Err("Condition must be a boolean".to_string())),
        }
    }

    fn eval_while(&mut self, condition: &Expression, body: &Expression) -> Result<Value, ControlFlow<Value, String>> {
        compiler_debug!("Int.eval.while| Entering while loop");
        //let while_env = Environment::new(Some(Box::new(self.environment.clone())));
        //let old_env = std::mem::replace(&mut self.environment, while_env);

        let mut result = Ok(Value::Unit);
        while self.eval_expression(condition)?.is_truthy()? {
            compiler_debug!("Int.eval.while| Entering body of while loop");
            result = self.eval_expression(&body);
        }

        //self.environment = old_env;
        result
    }

    fn eval_block(&mut self, expressions: &[Box<Expression>], env: Rc<RefCell<Environment>>) -> Result<Value, ControlFlow<Value, String>> {
        let old_env = std::mem::replace(&mut self.environment, env);

        compiler_debug!("Int.eval.block| Env: {}", self.environment.borrow());
        compiler_debug!("Int.eval.block| expressions: {:?}", expressions);

        let result = self.eval_inner_block(expressions);
        compiler_debug!("Int.eval.block| Result: {:?}", result);

        self.environment = old_env;
        result
    }

    fn eval_inner_block(&mut self, expressions: &[Box<Expression>]) -> Result<Value, ControlFlow<Value, String>> {
        let mut result = Value::Unit;
        for expr in expressions {
            result = self.eval_expression(expr)?;
        }
        Ok(result)
    }

    fn eval_return(&mut self, expr: &Option<Box<Expression>>) -> Result<Value, ControlFlow<Value, String>> {
        if !self.in_function {
            return Err(ControlFlow::Err("Return statement outside of function".to_string()));
        }

        match expr {
            Some(e) => {
                let value = self.eval_expression(e)?;
                Err(ControlFlow::Return(value))
            },
            None => {
                Err(ControlFlow::Return(Value::Unit))
            }
        }
    }

    // TODO: Remove Print statement, move to built in function instead
    // print <expr>
    fn eval_print(&mut self, expr: &Expression) -> Result<Value, ControlFlow<Value, String>> {
        let value = self.eval_expression(expr)?;
        compiler_debug!("{:?}", value);
        Ok(Value::Unit)
    }

    /* TODO: Remove this, as it is not needed
    fn eval_variable_assignment(&mut self, id: &Id, value: &Expression) -> Result<Value, ControlFlow<Value, String>> {
        let evaluated_value = self.eval_expression(value)?;
        let distance = self.local_scope.get(&id.name).unwrap();
        compiler_debug!("Int.eval.var_assign| Assigning {:?} = {:?} at {:?} distance to [{}]", id.name, evaluated_value, distance, self.environment.borrow());
        self.environment.borrow_mut().assign(*distance, id.name.clone(), evaluated_value.clone());
        compiler_debug!("Int.eval.var_assign| Environment after assignment: [{}]", self.environment.borrow());
        Ok(evaluated_value)
    }
    */
}