use crate::frontend::ast::*;

pub struct TypeofEvaluator;

impl TypeofEvaluator {
    pub fn eval_program(&self, program: &Program) -> Program {
        Program {
            expressions: program.expressions.iter().map(|e| self.eval_expression(e)).collect(),
        }
    }

    fn eval_expression(&self, expr: &Expression) -> Expression {
        match expr {
            Expression::FunctionCall { id, arguments } if id.name == "typeof" => {
                if arguments.len() != 1 {
                    panic!("typeof expects one argument");
                }
                let arg = &arguments[0];
                match arg {
                    Expression::Identifier { type_expr: Some(TypeExpression::Struct(id, _)), .. } => Expression::Symbol(id.name.clone()),
                    Expression::Identifier { type_expr: Some(TypeExpression::Int), .. } => Expression::Symbol("Int".to_string()),
                    Expression::Identifier { type_expr: Some(TypeExpression::Float), .. } => Expression::Symbol("Float".to_string()),
                    Expression::Identifier { type_expr: Some(TypeExpression::Bool), .. } => Expression::Symbol("Bool".to_string()),
                    Expression::Identifier { type_expr: Some(TypeExpression::String), .. } => Expression::Symbol("String".to_string()),
                    Expression::Identifier { type_expr: Some(TypeExpression::Void), .. } => Expression::Symbol("Void".to_string()),
                    Expression::Identifier { type_expr: None, .. } => panic!("Cannot determine type for typeof: identifier has no type annotation"),
                    _ => panic!("typeof only supported for identifiers with type annotations for now"),
                }
            }
            Expression::FunctionCall { id, arguments } => Expression::FunctionCall {
                id: id.clone(),
                arguments: arguments.iter().map(|a| self.eval_expression(a)).collect(),
            },
            Expression::BinaryOp { operator, left, right } => Expression::BinaryOp {
                operator: operator.clone(),
                left: Box::new(self.eval_expression(left)),
                right: Box::new(self.eval_expression(right)),
            },
            Expression::Block(exprs) => Expression::Block(exprs.iter().map(|e| Box::new(self.eval_expression(e))).collect()),
            Expression::If { condition, then_branch, else_branch } => Expression::If {
                condition: Box::new(self.eval_expression(condition)),
                then_branch: Box::new(self.eval_expression(then_branch)),
                else_branch: else_branch.as_ref().map(|e| Box::new(self.eval_expression(e))),
            },
            Expression::While { condition, body } => Expression::While {
                condition: Box::new(self.eval_expression(condition)),
                body: Box::new(self.eval_expression(body)),
            },
            Expression::Return(expr) => Expression::Return(expr.as_ref().map(|e| Box::new(self.eval_expression(e)))),
            Expression::Print(expr) => Expression::Print(Box::new(self.eval_expression(expr))),
            Expression::FunctionDefinition { id, parameters, body, return_type_expr, foreign } => Expression::FunctionDefinition {
                id: id.clone(),
                parameters: parameters.clone(),
                body: Box::new(self.eval_expression(body)),
                return_type_expr: return_type_expr.clone(),
                foreign: *foreign,
            },
            _ => expr.clone(),
        }
    }
} 