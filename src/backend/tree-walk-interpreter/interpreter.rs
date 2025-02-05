use crate::frontend::ast::{Expression, Program, BinaryOperator, UnaryOperator, Id, Parameter, FieldDefinition};
use crate::backend::tree_walk_interpreter::environment::Environment;

pub struct Interpreter {
    environment: Environment,
}

#[derive(Clone, Debug)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    String(String),
    Unit,
}

impl From<Value> for Expression {
    fn from(value: Value) -> Self {
        match value {
            Value::Number(n) => Expression::Number(n),
            Value::Boolean(b) => Expression::Boolean(b),
            Value::String(s) => Expression::String(s),
            Value::Unit => Expression::Block(vec![]), // Represent Unit as empty block
        }
    }
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            environment: Environment::new(None, None),
        }
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
            Expression::For { iterator, range, body } => self.eval_for(iterator, range, body),
            Expression::Block(expressions) => self.eval_block(expressions),
            Expression::Return(expr) => self.eval_return(expr),
            Expression::Print(expr) => self.eval_print(expr),
            Expression::VariableAssignment { id, value } => self.eval_variable_assignment(id, value),
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
        // For now, just handle built-in functions
        // TODO: Implement user-defined function calls
        Err("Function calls not yet implemented".to_string())
    }

    fn eval_function_definition(&mut self, id: &Id, parameters: &[Parameter], body: &Expression) -> Result<Value, String> {
        // TODO: Implement function definition
        Err("Function definition not yet implemented".to_string())
    }

    fn eval_struct_definition(&mut self, id: &Id, fields: &[FieldDefinition]) -> Result<Value, String> {
        // TODO: Implement struct definition
        Err("Struct definition not yet implemented".to_string())
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

    fn eval_for(&mut self, iterator: &Id, range: &Expression, body: &Expression) -> Result<Value, String> {
        // TODO: Implement for loop
        Err("For loops not yet implemented".to_string())
    }

    fn eval_block(&mut self, expressions: &[Box<Expression>]) -> Result<Value, String> {
        // Create a new environment for this block with the current environment as parent
        let mut block_env = Environment::new(Some(self.environment));
        let old_env = std::mem::replace(&mut self.environment, block_env);
        
        let mut result = Value::Unit;
        for expr in expressions {
            result = self.eval_expression(expr)?;
        }
        
        // Restore the old environment
        self.environment = old_env;
        Ok(result)
    }

    fn eval_return(&mut self, expr: &Option<Box<Expression>>) -> Result<Value, String> {
        match expr {
            Some(e) => self.eval_expression(e),
            None => Ok(Value::Unit)
        }
    }

    fn eval_print(&mut self, expr: &Expression) -> Result<Value, String> {
        let value = self.eval_expression(expr)?;
        println!("{:?}", value);
        Ok(Value::Unit)
    }

    fn eval_variable_assignment(&mut self, id: &Id, value: &Expression) -> Result<Value, String> {
        let evaluated_value = self.eval_expression(value)?;
        self.environment.define(id.name.clone(), evaluated_value.clone().into());
        Ok(evaluated_value)
    }
} 