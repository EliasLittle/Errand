use crate::frontend::ast::*;

impl Program {
    pub fn lower(self) -> Program {
        Program {
            expressions: self.expressions.into_iter().map(|expr| self.lower_expression(expr)).collect(),
        }
    }

    fn lower_expression(&self, expr: Expression) -> Expression {
        match expr {
            Expression::For { iterator, range, body } => {
                // Create a dummy variable for the index
                let index_var = Id { name: "%index".to_string() };
                let index_assignment = Expression::VariableAssignment {
                    id: index_var.clone(),
                    value: Box::new(Expression::Number(0)), // Initialize index to 0
                };

                let range_expr = *range;  // Unbox once
                
                // Set the iterator to the value at the current index
                let iterator_assignment = Expression::VariableAssignment {
                    id: iterator,
                    value: Box::new(Expression::FunctionCall {
                        id: Id { name: "get".to_string() },
                        arguments: [range_expr.clone(), Expression::Identifier(index_var.clone())],
                    }),
                };

                // Increment the index at the end of the body
                let increment_index = Expression::VariableAssignment {
                    id: index_var.clone(),
                    value: Box::new(Expression::BinaryOp {
                        operator: BinaryOperator::Add,
                        left: Box::new(Expression::Identifier(index_var.clone())),
                        right: Box::new(Expression::Number(1)),
                    }),
                };

                let while_body = Expression::Block([
                    Box::new(iterator_assignment),
                    body,
                    Box::new(increment_index),
                ]);

                Expression::Block([
                    Box::new(index_assignment),
                    Box::new(Expression::While {
                        condition: Box::new(Expression::BinaryOp {
                            operator: BinaryOperator::LessThan,
                            left: Box::new(Expression::Identifier(index_var.clone())),
                            right: Box::new(Expression::FunctionCall {
                                id: Id { name: "length".to_string() },
                                arguments: range,
                            }),
                        }),
                        body: Box::new(while_body),
                    }),
                ])
            },
            _ => expr,
        }
    }
}
