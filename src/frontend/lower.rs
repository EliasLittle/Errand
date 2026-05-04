use crate::frontend::ast::*;

// Desugaring that still lives in the frontend until a later phase owns it:
// - `printf` string literal arguments (C ABI / heap temporaries)
// - implicit `return` on user functions without an explicit return
//
// For-loops, field access (`getfield`), enum variant recognition, and struct
// constructor synthesis are handled in `backend/preir_gen.rs`.

// ----------------------------------------------------------------------------
// Core lowering "loop" (mutual recursion with `lower_expression`)
// ----------------------------------------------------------------------------

impl Program {
    pub fn lower(&self) -> Program {
        let _span = tracing::debug_span!(
            target: "lowering",
            "lower.program",
            expr_count = self.expressions.len()
        )
        .entered();
        tracing::debug!(target: "lowering", "start lowering program");
        let expressions: Vec<Expression> = self
            .expressions
            .iter()
            .map(|expr| self.lower_expression(expr.clone()))
            .collect();
        tracing::debug!(
            target: "lowering",
            lowered = expressions.len(),
            "program lowered"
        );
        Program { expressions }
    }

    fn lower_expression(&self, expr: Expression) -> Expression {
        match expr {
            Expression::UnaryOp { operator, operand } => Expression::UnaryOp {
                operator,
                operand: Box::new(self.lower_expression(*operand)),
            },
            Expression::BinaryOp {
                operator,
                left,
                right,
            } => {
                let lowered_left = self.lower_expression(*left);
                let lowered_right = self.lower_expression(*right);
                Expression::BinaryOp {
                    operator,
                    left: Box::new(lowered_left),
                    right: Box::new(lowered_right),
                }
            }
            Expression::FunctionDefinition {
                id,
                parameters,
                body,
                return_type_expr,
                foreign,
            } => {
                let _fn_span = tracing::trace_span!(
                    target: "lowering",
                    "lower.function_definition",
                    name = %id.name,
                    foreign
                )
                .entered();
                tracing::trace!(
                    target: "lowering",
                    name = %id.name,
                    foreign,
                    "visit function definition"
                );
                let lowered_body = Box::new(self.lower_expression(*body));
                if foreign {
                    // For foreign functions, preserve the flag and do not call lower_function_definition
                    Expression::FunctionDefinition {
                        id,
                        parameters,
                        body: lowered_body,
                        return_type_expr,
                        foreign: true,
                    }
                } else {
                    lower_function_definition(id, parameters, lowered_body, return_type_expr)
                }
            }
            Expression::FunctionCall { id, arguments } if id.name == "printf" => {
                lower_printf(self, id, arguments)
            }
            Expression::FunctionCall { id, arguments } => Expression::FunctionCall {
                id,
                arguments: arguments
                    .into_iter()
                    .map(|a| self.lower_expression(a))
                    .collect(),
            },
            Expression::Block(exprs) => Expression::Block(
                exprs
                    .into_iter()
                    .map(|e| Box::new(self.lower_expression(*e)))
                    .collect(),
            ),
            Expression::If {
                condition,
                then_branch,
                else_branch,
            } => Expression::If {
                condition: Box::new(self.lower_expression(*condition)),
                then_branch: Box::new(self.lower_expression(*then_branch)),
                else_branch: else_branch.map(|e| Box::new(self.lower_expression(*e))),
            },
            Expression::While { condition, body } => Expression::While {
                condition: Box::new(self.lower_expression(*condition)),
                body: Box::new(self.lower_expression(*body)),
            },
            Expression::Return(expr) => {
                Expression::Return(expr.map(|e| Box::new(self.lower_expression(*e))))
            }
            Expression::Print(expr) => Expression::Print(Box::new(self.lower_expression(*expr))),
            Expression::EnumVariantConstruct {
                enum_name,
                variant,
                args,
            } => Expression::EnumVariantConstruct {
                enum_name,
                variant,
                args: args.into_iter().map(|a| self.lower_expression(a)).collect(),
            },
            Expression::Match { value, cases } => Expression::Match {
                value: Box::new(self.lower_expression(*value)),
                cases: cases
                    .into_iter()
                    .map(|c| MatchCase {
                        pattern: c.pattern,
                        body: Box::new(self.lower_expression(*c.body)),
                    })
                    .collect(),
            },
            other => other,
        }
    }
}

// ----------------------------------------------------------------------------
// Helper functions for specific lowering logic
// ----------------------------------------------------------------------------

fn lower_printf(program: &Program, id: Id, arguments: Vec<Expression>) -> Expression {
    let mut block: Vec<Box<Expression>> = Vec::new();
    let mut new_args: Vec<Expression> = Vec::new();
    let mut tmp_counter = 0;

    for arg in arguments.into_iter().map(|a| program.lower_expression(a)) {
        if let Expression::String(s) = arg {
            push_string_temp_for_printf(&mut block, &mut new_args, &mut tmp_counter, s);
        } else {
            new_args.push(arg);
        }
    }

    block.push(Box::new(Expression::FunctionCall {
        id: id.clone(),
        arguments: new_args,
    }));

    for i in 0..tmp_counter {
        block.push(Box::new(free_as_ptr_tmp(i)));
    }

    Expression::Block(block)
}

fn push_string_temp_for_printf(
    block: &mut Vec<Box<Expression>>,
    new_args: &mut Vec<Expression>,
    tmp_counter: &mut usize,
    s: String,
) {
    let tmp_name = format!("_tmp_str_{}", *tmp_counter);
    *tmp_counter += 1;
    let tmp_id = Id {
        name: tmp_name.clone(),
    };

    let malloc_call = Expression::BinaryOp {
        operator: BinaryOperator::Assignment,
        left: Box::new(Expression::Identifier {
            id: tmp_id.clone(),
            type_expr: Some(TypeExpression::String),
        }),
        right: Box::new(Expression::FunctionCall {
            id: Id {
                name: "as_string".to_string(),
            },
            arguments: vec![Expression::FunctionCall {
                id: Id {
                    name: "malloc".to_string(),
                },
                arguments: vec![Expression::BinaryOp {
                    operator: BinaryOperator::Add,
                    left: Box::new(Expression::FunctionCall {
                        id: Id {
                            name: "strlen".to_string(),
                        },
                        arguments: vec![Expression::String(s.clone())],
                    }),
                    right: Box::new(Expression::Int(1)),
                }],
            }],
        }),
    };
    let strcpy_call = Expression::FunctionCall {
        id: Id {
            name: "strcpy".to_string(),
        },
        arguments: vec![
            Expression::Identifier {
                id: tmp_id.clone(),
                type_expr: Some(TypeExpression::String),
            },
            Expression::String(s.clone()),
        ],
    };
    block.push(Box::new(malloc_call));
    block.push(Box::new(strcpy_call));
    new_args.push(Expression::Identifier {
        id: tmp_id,
        type_expr: Some(TypeExpression::String),
    });
}

fn free_as_ptr_tmp(i: usize) -> Expression {
    let tmp_id = Id {
        name: format!("_tmp_str_{}", i),
    };
    Expression::FunctionCall {
        id: Id {
            name: "free".to_string(),
        },
        arguments: vec![Expression::FunctionCall {
            id: Id {
                name: "as_ptr".to_string(),
            },
            arguments: vec![Expression::Identifier {
                id: tmp_id,
                type_expr: Some(TypeExpression::String),
            }],
        }],
    }
}

fn lower_function_definition(
    id: Id,
    parameters: Vec<Parameter>,
    body: Box<Expression>,
    return_type_expr: Option<TypeExpression>,
) -> Expression {
    let _span = tracing::trace_span!(
        target: "lowering",
        "lower.implicit_return",
        name = %id.name
    )
    .entered();
    tracing::trace!(
        target: "lowering",
        name = %id.name,
        "check implicit return / append tail"
    );

    if has_return_statement(&body) {
        tracing::trace!(
            target: "lowering",
            name = %id.name,
            "explicit return present; body unchanged"
        );
        return Expression::FunctionDefinition {
            id,
            parameters,
            body,
            return_type_expr,
            foreign: false,
        };
    }

    tracing::trace!(
        target: "lowering",
        name = %id.name,
        "appending implicit return"
    );

    let implicit_return = match &return_type_expr {
        Some(TypeExpression::Void) => Expression::Return(None),
        _ => Expression::Return(Some(Box::new(Expression::Int(0)))),
    };
    // TODO: Change the return type and value to that of the last expression in the body

    let new_body = match *body {
        Expression::Block(mut expressions) => {
            expressions.push(Box::new(implicit_return));
            Expression::Block(expressions)
        }
        _ => Expression::Block(vec![body, Box::new(implicit_return)]),
    };

    Expression::FunctionDefinition {
        id,
        parameters,
        body: Box::new(new_body),
        return_type_expr,
        foreign: false,
    }
}

fn has_return_statement(expr: &Expression) -> bool {
    match expr {
        Expression::Return(_) => true,
        Expression::Block(expressions) => expressions.iter().any(|e| has_return_statement(e)),
        Expression::If {
            then_branch,
            else_branch,
            ..
        } => {
            has_return_statement(then_branch)
                || else_branch
                    .as_ref()
                    .map_or(false, |e| has_return_statement(e))
        }
        Expression::While { body, .. } => has_return_statement(body),
        Expression::For { body, .. } => has_return_statement(body),
        // Only walk syntactic "statement" positions — not operands, call arguments, etc.
        Expression::Match { cases, .. } => cases.iter().any(|c| has_return_statement(&c.body)),
        _ => false,
    }
}
