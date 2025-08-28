use crate::frontend::ast::*;

// TODO: Rename to desugar

impl Program {
    pub fn lower(&self) -> Program {
        println!("------ Lowering program ------");
        // 1. Find all struct definitions and generate constructor functions
        let mut constructor_functions = Vec::new();
        for expr in &self.expressions {
            if let Expression::StructDefinition { id, fields } = expr {
                // Build parameter list from fields
                let parameters: Vec<Parameter> = fields.iter().map(|f| Parameter {
                    id: f.id.clone(),
                    type_expr: Some(f.field_type.clone()),
                }).collect();
                // Build arguments for new(:StructName, ...fields...)
                let mut new_args = vec![Expression::Symbol(id.name.clone())];
                new_args.extend(fields.iter().map(|f| Expression::Identifier {
                    id: f.id.clone(),
                    type_expr: Some(f.field_type.clone()),
                }));
                let body = Box::new(Expression::Return(Some(Box::new(Expression::FunctionCall {
                    id: Id { name: "new".to_string() },
                    arguments: new_args,
                }))));
                let constructor = Expression::FunctionDefinition {
                    id: id.clone(),
                    parameters,
                    body,
                    return_type_expr: Some(TypeExpression::Struct(id.clone(), None)),
                };
                constructor_functions.push(constructor);
            }
        }
        // 2. Lower the rest of the program
        let lowered = Program {
            expressions: constructor_functions.into_iter()
                .chain(self.expressions.iter().map(|expr| self.lower_expression(expr.clone())))
                .collect(),
        };
        println!("------ Program lowered ------");
        lowered
    }

    fn lower_expression(&self, expr: Expression) -> Expression {
        match expr.clone() {
            Expression::For { iterator, range, body } => lower_for_loop(iterator, range, body),
            Expression::BinaryOp { operator, left, right } => {
                let lowered_left = self.lower_expression(*left);
                let lowered_right = self.lower_expression(*right);
                match operator {
                    BinaryOperator::Dot => lower_field_access(Box::new(lowered_left), Box::new(lowered_right)),
                    _ => Expression::BinaryOp {
                        operator,
                        left: Box::new(lowered_left),
                        right: Box::new(lowered_right),
                    },
                }
            },
            Expression::FunctionDefinition { id, parameters, body, return_type_expr } => {
                let lowered_body = Box::new(self.lower_expression(*body));
                lower_function_definition(id, parameters, lowered_body, return_type_expr)
            },
            Expression::FunctionCall { id, arguments } if id.name == "printf" => {
                // Desugar string arguments to printf (as before), but lower arguments first
                let mut new_block: Vec<Box<Expression>> = Vec::new();
                let mut new_args: Vec<Expression> = Vec::new();
                let mut tmp_counter = 0;
                for arg in arguments.into_iter().map(|a| self.lower_expression(a)) {
                    if let Expression::String(s) = arg {
                        let tmp_name = format!("_tmp_str_{}", tmp_counter);
                        tmp_counter += 1;
                        let tmp_id = Id { name: tmp_name.clone() };
                        let malloc_call = Expression::BinaryOp {
                            operator: BinaryOperator::Assignment,
                            left: Box::new(Expression::Identifier { id: tmp_id.clone(), type_expr: Some(TypeExpression::String) }),
                            right: Box::new(Expression::FunctionCall {
                                id: Id { name: "malloc".to_string() },
                                arguments: vec![
                                    Expression::BinaryOp {
                                        operator: BinaryOperator::Add,
                                        left: Box::new(Expression::FunctionCall {
                                            id: Id { name: "strlen".to_string() },
                                            arguments: vec![Expression::String(s.clone())],
                                        }),
                                        right: Box::new(Expression::Int(1)),
                                    }
                                ],
                            }),
                        };
                        let strcpy_call = Expression::FunctionCall {
                            id: Id { name: "strcpy".to_string() },
                            arguments: vec![
                                Expression::Identifier { id: tmp_id.clone(), type_expr: Some(TypeExpression::String) },
                                Expression::String(s.clone()),
                            ],
                        };
                        new_block.push(Box::new(malloc_call));
                        new_block.push(Box::new(strcpy_call));
                        new_args.push(Expression::Identifier { id: tmp_id.clone(), type_expr: Some(TypeExpression::String) });
                    } else {
                        new_args.push(arg);
                    }
                }
                let printf_call = Expression::FunctionCall {
                    id: id.clone(),
                    arguments: new_args,
                };
                new_block.push(Box::new(printf_call));
                for i in 0..tmp_counter {
                    let tmp_name = format!("_tmp_str_{}", i);
                    let tmp_id = Id { name: tmp_name };
                    let free_call = Expression::FunctionCall {
                        id: Id { name: "free".to_string() },
                        arguments: vec![Expression::Identifier { id: tmp_id, type_expr: Some(TypeExpression::String) }],
                    };
                    new_block.push(Box::new(free_call));
                }
                Expression::Block(new_block)
            },
            Expression::FunctionCall { id, arguments } => {
                let lowered_args = arguments.into_iter().map(|a| self.lower_expression(a)).collect();
                Expression::FunctionCall { id, arguments: lowered_args }
            },
            Expression::Block(exprs) => {
                let lowered_exprs = exprs.into_iter().map(|e| Box::new(self.lower_expression(*e))).collect();
                Expression::Block(lowered_exprs)
            },
            Expression::If { condition, then_branch, else_branch } => {
                let lowered_condition = Box::new(self.lower_expression(*condition));
                let lowered_then = Box::new(self.lower_expression(*then_branch));
                let lowered_else = else_branch.map(|e| Box::new(self.lower_expression(*e)));
                Expression::If {
                    condition: lowered_condition,
                    then_branch: lowered_then,
                    else_branch: lowered_else,
                }
            },
            Expression::While { condition, body } => {
                let lowered_condition = Box::new(self.lower_expression(*condition));
                let lowered_body = Box::new(self.lower_expression(*body));
                Expression::While {
                    condition: lowered_condition,
                    body: lowered_body,
                }
            },
            Expression::Return(expr) => {
                let lowered_expr = expr.map(|e| Box::new(self.lower_expression(*e)));
                Expression::Return(lowered_expr)
            },
            Expression::Print(expr) => {
                let lowered_expr = Box::new(self.lower_expression(*expr));
                Expression::Print(lowered_expr)
            },
            _ => expr,
        }
    }
}

fn lower_field_access(left: Box<Expression>, right: Box<Expression>) -> Expression {
    println!("------ Lowering field access ------");
    let field_symbol = match *right {
        Expression::Identifier { id, .. } => Expression::Symbol(id.name),
        other => panic!("Dot field access expects an identifier as the field name, got: {:?}", other),
    };
    // Instead of passing the struct type directly, pass typeof(left) as the third argument
    let typeof_call = Expression::FunctionCall {
        id: Id { name: "typeof".to_string() },
        arguments: vec![(*left).clone()],
    };
    Expression::FunctionCall {
        id: Id { name: "getfield".to_string() },
        arguments: vec![*left, field_symbol, typeof_call],
    }
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
