use crate::frontend::ast::*;

// TODO: Rename to desugar

impl Program {
    pub fn lower(&self) -> Program {
        println!("------ Lowering program ------");
        let lowered = Program {
            expressions: self.expressions.iter().map(|expr| self.lower_expression(expr.clone())).collect(),
        };
        println!("------ Program lowered ------");
        lowered
    }

    fn lower_expression(&self, expr: Expression) -> Expression {
        match expr.clone() {
            Expression::For { iterator, range, body } => lower_for_loop(iterator, range, body),
            Expression::BinaryOp { operator, left, right } => {
                match operator {
                    BinaryOperator::Dot => lower_field_access(left, right),
                    _ => expr,
                }
            },
            Expression::FunctionDefinition { id, parameters, body, return_type_expr } => {
                lower_function_definition(id, parameters, body, return_type_expr)
            },
            _ => expr,
        }
    }
}

fn lower_field_access(left: Box<Expression>, right: Box<Expression>) -> Expression {
    println!("------ Lowering field access ------");
    Expression::FunctionCall { id: Id { name: "getfield".to_string() }, arguments: vec![*left, *right] }
}

fn lower_function_definition(id: Id, parameters: Vec<Parameter>, body: Box<Expression>, return_type_expr: Option<TypeExpression>) -> Expression {
    println!("------ Lowering function definition ------");
    
    // Check if the function body already has a return statement
    let has_return = has_return_statement(&body);
    
    if has_return {
        // If it already has a return, just return the function as-is
        Expression::FunctionDefinition { id, parameters, body, return_type_expr }
    } else {
        // If no return statement, add an implicit return at the end
        let implicit_return = match &return_type_expr {
            Some(TypeExpression::Void) => Expression::Return(None),
            _ => Expression::Return(Some(Box::new(Expression::Int(0)))), // Default return value
        };
        // TODO: Change the return type and value to that of the last expression in the body
        
        // Wrap the body in a block with the implicit return
        let new_body = match *body {
            Expression::Block(mut expressions) => {
                expressions.push(Box::new(implicit_return));
                Expression::Block(expressions)
            },
            _ => {
                Expression::Block(vec![
                    body,
                    Box::new(implicit_return),
                ])
            },
        };
        
        Expression::FunctionDefinition { 
            id, 
            parameters, 
            body: Box::new(new_body), 
            return_type_expr 
        }
    }
}

fn has_return_statement(expr: &Expression) -> bool {
    match expr {
        Expression::Return(_) => true,
        Expression::Block(expressions) => {
            expressions.iter().any(|e| has_return_statement(e))
        },
        Expression::If { then_branch, else_branch, .. } => {
            has_return_statement(then_branch) || 
            else_branch.as_ref().map_or(false, |e| has_return_statement(e))
        },
        Expression::While { body, .. } => {
            has_return_statement(body)
        },
        _ => false,
    }
}

fn lower_for_loop(iterator: Id, range: Box<Expression>, body: Box<Expression>) -> Expression {
    println!("------ Lowering for loop ------");
      // Create a dummy variable for the index
      let index_var = Expression::Identifier { id: Id { name: "%index".to_string() }, type_expr: Some(TypeExpression::Int) };
      let index_assignment = Expression::BinaryOp {
          operator: BinaryOperator::Assignment,
          left: Box::new(index_var.clone()),
          right: Box::new(Expression::Int(0)), // Initialize index to 0
      };

      let range_expr = *range;  // Unbox once
      
      // Set the iterator to the value at the current index
      let iterator_assignment = Expression::BinaryOp {
          operator: BinaryOperator::Assignment,
          left: Box::new(Expression::Identifier { id: iterator.clone(), type_expr: None }),
          right: Box::new(Expression::FunctionCall {
              id: Id { name: "get".to_string() },
              arguments: [range_expr.clone(), index_var.clone()].to_vec(),
          }),
      };

      // Increment the index at the end of the body
      let increment_index = Expression::BinaryOp {
          operator: BinaryOperator::Assignment,
          left: Box::new(index_var.clone()),
          right: Box::new(Expression::BinaryOp {
              operator: BinaryOperator::Add,
              left: Box::new(index_var.clone()),
              right: Box::new(Expression::Int(1)),
          }),
      };

      let while_body = Expression::Block([
          Box::new(iterator_assignment),
          body,
          Box::new(increment_index),
      ].to_vec());

      Expression::Block([
          Box::new(index_assignment),
          Box::new(Expression::While {
              condition: Box::new(Expression::BinaryOp {
                  operator: BinaryOperator::LessThan,
                  left: Box::new(index_var.clone()),
                  right: Box::new(Expression::FunctionCall {
                      id: Id { name: "length".to_string() },
                      arguments: [range_expr].to_vec(),
                  }),
              }),
              body: Box::new(while_body),
          }),
      ].to_vec())
}
