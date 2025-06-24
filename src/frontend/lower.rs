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
            _ => expr,
        }
    }
}

fn lower_field_access(left: Box<Expression>, right: Box<Expression>) -> Expression {
    println!("------ Lowering field access ------");
    Expression::FunctionCall { id: Id { name: "getfield".to_string() }, arguments: vec![*left, *right] }
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
          left: Box::new(iterator.clone()),
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
