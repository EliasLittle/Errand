use crate::frontend::ast::{Expression, Program, BinaryOperator, UnaryOperator, Id, Parameter, FieldDefinition};
use crate::backend::tree_walk_interpreter::environment::Environment;

trait Callable {
    pub fn arity(&self) -> usize;

    pub fn call(&self, env: &mut Environment, arguments: Vec<Expression>) -> Result<Expression, String>;
}

pub struct Function {
    pub name: String,
    pub parameters: Vec<String>,
    pub body: Expression,
    pub environment: Environment,
}

impl Callable for Function {
    fn arity(&self) -> usize {
        self.parameters.len()
    }

    fn call(&self, env: &mut Environment, arguments: Vec<Expression>) -> Result<Expression, String> {
        if arguments.len() != self.arity() {
            return Err(format!("Incorrect number of arguments: expected {}, got {}", self.arity(), arguments.len()));
        }

        let mut fn_env = Environment::new(Some(env));
        let old_env = std::mem::replace(&mut self.environment, fn_env);
        
        let mut result = Value::Unit;
        result = self.eval_expression(&self.body)?;
        
        // Restore the old environment
        self.environment = old_env; 
    }
}

pub struct Type {
    pub name: String,
    pub fields: HashMap<String, Type>,
}

impl Callable for Type {
    fn arity(&self) -> usize {
        self.fields.len()
    }

    fn call(&self, env: &mut Environment, arguments: Vec<Expression>) -> Result<Expression, String> {
        if arguments.len() != self.arity() {
            return Err(format!("Incorrect number of arguments: expected {}, got {}", self.arity(), arguments.len()));
        }

        let values = arguments.iter().map(|arg| self.eval_expression(arg)).collect::<Result<Vec<_>, String>>()?;

        let fields = self.fields.iter()
            .enumerate()
            .map(|(index, (key, _))| {
                let value = values[index].clone();
                (key.clone(), value)
            })
            .collect::<HashMap<String, Value>>();

        let instance = Instance {
            type_name: self.name.clone(),
            fields,
        };
        Ok(Expression::Instance(instance))
    }
}

pub struct Instance {
    pub type_name: String,
    pub fields: HashMap<String, Value>,
}

#[derive(Clone, Debug)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    String(String),
    Callable(Box<dyn Callable>),
    Instance(Instance),
    Unit,
}

impl From<Value> for Expression {
    fn from(value: Value) -> Self {
        match value {
            Value::Number(n) => Expression::Number(n),
            Value::Boolean(b) => Expression::Boolean(b),
            Value::String(s) => Expression::String(s),
            Value::Function(f) => Expression::FunctionDefinition {
                id: Id { name: f.name },
                parameters: f.parameters.iter().map(|p| Parameter { id: Id { name: p.clone() } }).collect(),
                body: f.body,
            },
            Value::Unit => Expression::Block(vec![]), // Represent Unit as empty block
        }
    }
}

pub struct Return(Value); // Define Return type

impl std::fmt::Debug for Return {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Return({:?})", self.0)
    }
}

pub struct Interpreter {
    environment: Environment,
    local_scope: HashMap<String, usize>,
    in_function: bool,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            environment: Environment::new(None),
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
            result = self.eval_expression(expr)?;
        }
        Ok(result)
    }

    fn eval_expression(&mut self, expr: &Expression) -> Result<Value, String> {
        match expr {
            Expression::Number(_) | Expression::Boolean(b) | Expression::String(s) => self.eval_literal(expr),
            Expression::Identifier(id) => self.eval_identifier(id),
            Expression::UnaryOp { operator, operand } => self.eval_unary_op(operator, operand),
            Expression::BinaryOp { operator, left, right } => self.eval_binary_op(operator, left, right),
            Expression::FunctionCall { id, arguments } => self.eval_function_call(id, arguments),
            Expression::FunctionDefinition { id, parameters, body } => self.eval_function_definition(id, parameters, body),
            Expression::StructDefinition { id, fields } => self.eval_struct_definition(id, fields),
            Expression::If { condition, then_branch, else_branch } => self.eval_if(condition, then_branch, else_branch),
            Expression::While { condition, body } => self.eval_while(condition, body),
            Expression::Block(expressions) => self.eval_block(expressions),
            Expression::Return(expr) => self.eval_return(expr),
            Expression::Print(expr) => self.eval_print(expr),
            Expression::VariableAssignment { id, value } => self.eval_variable_assignment(id, value),
            Expression::For { iterator, range, body } => Err("For loops not implemented in interpreter, please desugar".to_string()),
        }
    }

    fn eval_identifier(&self, id: &Id) -> Result<Value, String> {
        self.environment
            .get(&id.name)
            .ok_or_else(|| format!("Undefined variable: {}", id.name))
            .and_then(|expr| self.eval_literal(expr))
    }

    fn eval_literal(&self, expr: &Expression) -> Result<Value, String> {
        match expr {
            Expression::Number(n) => Ok(Value::Number(*n)),
            Expression::Boolean(b) => Ok(Value::Boolean(*b)),
            Expression::String(s) => Ok(Value::String(s.clone())),
            _ => Err(format!("Cannot convert expression to value: {:?}", expr))
        }
    }

    fn eval_unary_op(&mut self, operator: &UnaryOperator, operand: &Expression) -> Result<Value, String> {
        let operand_val = self.eval_expression(operand)?;
        match operator {
            UnaryOperator::Not => {
                if let Value::Boolean(b) = operand_val {
                    Ok(Value::Boolean(!b))
                } else {
                    Err("Cannot apply 'not' to non-boolean value".to_string())
                }
            }
            UnaryOperator::Negate => {
                if let Value::Number(n) = operand_val {
                    Ok(Value::Number(-n))
                } else {
                    Err("Cannot negate non-numeric value".to_string())
                }
            }
        }
    }

    fn eval_binary_op(&mut self, operator: &BinaryOperator, left: &Expression, right: &Expression) -> Result<Value, String> {
        let left_val = self.eval_expression(left)?;
        let right_val = self.eval_expression(right)?;

        match (operator, &left_val, &right_val) {
            (BinaryOperator::Add, Value::Number(l), Value::Number(r)) => Ok(Value::Number(l + r)),
            (BinaryOperator::Subtract, Value::Number(l), Value::Number(r)) => Ok(Value::Number(l - r)),
            (BinaryOperator::Multiply, Value::Number(l), Value::Number(r)) => Ok(Value::Number(l * r)),
            (BinaryOperator::Divide, Value::Number(l), Value::Number(r)) => {
                if *r == 0.0 {
                    Err("Division by zero".to_string())
                } else {
                    Ok(Value::Number(l / r))
                }
            }
            // TODO: Implement eval_equality to get better comparison results
            // Arguments can be of different types, so we need to implement a better comparison
            (BinaryOperator::Equal, l, r) => Ok(Value::Boolean(format!("{:?}", l) == format!("{:?}", r))),
            (BinaryOperator::NotEqual, l, r) => Ok(Value::Boolean(format!("{:?}", l) != format!("{:?}", r))),
            (BinaryOperator::LessThan, Value::Number(l), Value::Number(r)) => Ok(Value::Boolean(l < r)),
            (BinaryOperator::LessThanEqual, Value::Number(l), Value::Number(r)) => Ok(Value::Boolean(l <= r)),
            (BinaryOperator::GreaterThan, Value::Number(l), Value::Number(r)) => Ok(Value::Boolean(l > r)),
            (BinaryOperator::GreaterThanEqual, Value::Number(l), Value::Number(r)) => Ok(Value::Boolean(l >= r)),
            // TODO: Move these, to take advantage of short-circuiting
            (BinaryOperator::And, Value::Boolean(l), Value::Boolean(r)) => Ok(Value::Boolean(*l && *r)),
            (BinaryOperator::Or, Value::Boolean(l), Value::Boolean(r)) => Ok(Value::Boolean(*l || *r)),
            _ => Err(format!("Invalid operation {:?} between {:?} and {:?}", operator, left_val, right_val))
        }
    }

    fn eval_function_call(&mut self, id: &Id, arguments: &[Expression]) -> Result<Value, String> {
        let callee = self.environment.get(&id.name).ok_or_else(|| format!("Undefined function: {}", id.name))?;

        let args = arguments.iter().map(|arg| self.eval_expression(arg)).collect::<Result<Vec<_>, String>>()?;

        if callee.parameters.len() != arguments.len() {
            return Err(format!("Incorrect number of arguments: expected {}, got {}", callee.parameters.len(), arguments.len()));
        }
        
        let mut fn_env = Environment::new(Some(callee.environment));
        let old_env = std::mem::replace(&mut self.environment, fn_env);

        // Set the arguments in the function environment
        for (param, arg) in callee.parameters.iter().zip(args) {
            fn_env.define(param.id.name.clone(), arg.clone());
        }
        
        // Run the block and handle Return
        let result = std::panic::catch_unwind(|| self.eval_inner_block(callee.body));
        
        // Restore the old environment
        self.environment = old_env; 

        match result {
            Ok(value) => Ok(value),
            Err(err) => {
                if let Some(return_value) = err.downcast_ref::<Return>() {
                    Ok(return_value.0.clone()) // Return the value from Return
                } else {
                    Err(format!("Error during function call: {:?}", err))
                }
            }
        }
    }

    fn eval_function_definition(&mut self, id: &Id, parameters: &[Parameter], body: &Expression) -> Result<Value, String> {
        let function = Function {
            name: id.name.clone(),
            parameters: parameters.iter().map(|p| p.id.name.clone()).collect(),
            body: body.clone(),
            environment: self.environment.clone(),
        };

        self.environment.define(id.name.clone(), Value::Function(function));
        Ok(Value::Unit)
    }

    fn eval_struct_definition(&mut self, id: &Id, fields: &[FieldDefinition]) -> Result<Value, String> {
        let type_name = id.name.clone();
        let fields = fields.iter().map(|field| {
            let field_type = self.environment.get(&field.field_type.name)
                .ok_or_else(|| format!("Undefined type: {}", field.field_type.name))?;
            (field.id.name.clone(), field_type.clone())
        }).collect::<Result<HashMap<String, Type>, String>>()?;
        let type_def = Type {
            name: type_name,
            fields,
        };
        self.environment.define(id.name.clone(), Value::Callable(Box::new(type_def)));
        Ok(Value::Unit)
    }

    fn eval_if(&mut self, condition: &Expression, then_branch: &Expression, else_branch: &Option<Box<Expression>>) -> Result<Value, String> {
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
            Value::Number(_) | Value::String(_) => self.eval_expression(then_branch),
            Value::Unit => Err("Condition must be a boolean".to_string())
        }
    }

    fn eval_while(&mut self, condition: &Expression, body: &Expression) -> Result<Value, String> {
        let mut while_env = Environment::new(Some(self.environment));
        let old_env = std::mem::replace(&mut self.environment, while_env);

        let result = self.eval_inner_block(body);

        // Restore the old environment
        self.environment = old_env;
        Ok(result)
    }

    fn eval_block(&mut self, expressions: &[Box<Expression>]) -> Result<Value, String> {
        // Create a new environment for this block with the current environment as parent
        let mut block_env = Environment::new(Some(self.environment));
        let old_env = std::mem::replace(&mut self.environment, block_env);
        
        let result = self.eval_inner_block(expressions);
        
        // Restore the old environment
        self.environment = old_env;
        Ok(result)
    }

    fn eval_inner_block(&mut self, expressions: &[Box<Expression>]) -> Value {
        let mut result = Value::Unit;
        for expr in expressions {
            result = self.eval_expression(expr)?;
        }
        result
    }

    fn eval_return(&mut self, expr: &Option<Box<Expression>>) -> Result<Value, String> {
        if !self.in_function {
            return Err("Return statement outside of function".to_string());
        }

        match expr {
            Some(e) => {
                let value = self.eval_expression(e)?;
                // Raise a Return with the evaluated value
                panic!(Return(value));
            },
            None => {
                // Raise a Return with Unit value
                panic!(Return(Value::Unit));
            }
        }
    }

    fn eval_print(&mut self, expr: &Expression) -> Result<Value, String> {
        let value = self.eval_expression(expr)?;
        println!("{:?}", value);
        Ok(Value::Unit)
    }

    fn eval_variable_assignment(&mut self, id: &Id, value: &Expression) -> Result<Value, String> {
        let evaluated_value = self.eval_expression(value)?;
        let distance = self.local_scope.get(&id.name).unwrap();
        self.environment.assign(distance, id.name.clone(), evaluated_value.clone().into());
        Ok(evaluated_value)
    }
} 