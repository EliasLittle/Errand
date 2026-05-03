use crate::backend::errand_builtins::{
    add_builtin_data_constructors, add_builtin_functions, type_expr_to_errand_type,
};
use crate::backend::preir::{
    instr_index, BinOpPl, EnumData, EnumVariantConstructData, EnumVariantData, EnumVariantInfo,
    FnCallPl, ForLoopData, FuncData, IfStatementData, Instr, LiteralPl, MatchArmData, MatchData,
    PreIR, RegionData, ReturnData, StructData, UnOpPl, VarDeclData, VarRefData, WhileLoopData,
};
use crate::backend::worklist::ErrandType;
use crate::frontend::ast::MatchPattern;
use crate::frontend::ast::{BinaryOperator, Expression, Program};
use log::info;
use std::collections::HashMap;

pub fn compile_preir(program: &Program) -> Result<PreIR, String> {
    let mut ctx = PreIR::init();

    // First pass: collect function and struct definitions
    for expr in &program.expressions {
        match expr {
            Expression::FunctionDefinition {
                id,
                parameters,
                body,
                return_type_expr,
                foreign,
            } => {
                // Reserve space for function declaration
                let func = FuncData {
                    name: id.name.clone(),
                    parameters: parameters.clone(),
                    body_index: compile_expression(&mut ctx, body)?,
                    return_type: return_type_expr.clone(),
                    is_foreign: *foreign,
                };
                ctx.emit_instruction(Instr::FuncDecl(func));
            }
            Expression::StructDefinition {
                id,
                type_params,
                fields,
                ..
            } => {
                let strct = StructData {
                    name: id.name.clone(),
                    type_params: type_params.iter().map(|p| p.clone()).collect(),
                    fields: fields.clone(),
                };
                ctx.emit_instruction(Instr::StructDecl(strct));
            }
            Expression::EnumDefinition {
                id,
                type_params,
                variants,
            } => {
                let enum_data = EnumData {
                    name: id.name.clone(),
                    type_params: type_params.iter().map(|p| p.clone()).collect(),
                    variants: variants
                        .iter()
                        .map(|v| EnumVariantInfo {
                            name: v.name.clone(),
                            fields: v
                                .fields
                                .iter()
                                .map(|f| (f.id.name.clone(), f.field_type.clone()))
                                .collect(),
                        })
                        .collect(),
                };
                ctx.emit_instruction(Instr::EnumDecl(enum_data));
            }
            _ => {}
        }
    }

    // Second pass: compile all expressions and build main region
    let mut main_instructions = Vec::new();

    let instr_start = ctx.instructions.len() as instr_index;
    for expr in &program.expressions {
        match expr {
            Expression::FunctionDefinition { .. } => continue,
            Expression::StructDefinition { .. } => continue,
            Expression::EnumDefinition { .. } => continue,
            _ => {
                let instr_idx = compile_expression(&mut ctx, expr)?;
                main_instructions.push(instr_idx);
            }
        }
    }
    let instr_end = ctx.instructions.len() as instr_index;

    // The return location is the last instruction, or 0 if no instructions
    let return_loc = if let Some(&last_instr) = main_instructions.last() {
        last_instr
    } else {
        // Create a unit literal as default return
        ctx.emit_instruction(Instr::Literal(LiteralPl::Unit))
    };

    let main = Instr::Region(RegionData {
        instr_start,
        instr_end,
        return_loc,
    });

    Ok(PreIR {
        main,
        instructions: ctx.instructions,
    })
}

/// Extract function signatures from a Program to build typing context
/// Returns (data_constructors, function_types) for type inference
pub fn extract_typing_context(
    program: &Program,
) -> (HashMap<String, ErrandType>, HashMap<String, ErrandType>) {
    let data_constructors = add_builtin_data_constructors();
    let mut function_types = add_builtin_functions();

    // Extract user-defined function signatures and variable assignments
    for expr in &program.expressions {
        info!("Extracting typing context for expression: {:?}", expr);
        // Extract function definitions
        if let Expression::FunctionDefinition {
            id,
            parameters,
            return_type_expr,
            ..
        } = expr
        {
            let function_type = build_function_type(parameters, return_type_expr);
            function_types.insert(id.name.clone(), function_type);
        }

        // Extract variable assignments (zero-parameter functions)
        if let Expression::BinaryOp {
            operator: BinaryOperator::Assignment,
            left,
            right: _,
        } = expr
        {
            if let Expression::Identifier { id, type_expr } = left.as_ref() {
                let var_type = match type_expr {
                    Some(ty) => type_expr_to_errand_type(ty),
                    None => ErrandType::ETVar(format!("var_{}", id.name)), // Create type variable for inference
                };
                function_types.insert(id.name.clone(), var_type);
            }
        }
    }

    (data_constructors, function_types)
}

/// Build a function type from parameters and return type
fn build_function_type(
    parameters: &[crate::frontend::ast::Parameter],
    return_type_expr: &Option<crate::frontend::ast::TypeExpression>,
) -> ErrandType {
    let return_type = match return_type_expr {
        Some(type_expr) => type_expr_to_errand_type(type_expr),
        None => ErrandType::Con("Unit".to_string()), // Default to Unit if no return type specified
    };

    // Build function type by folding parameter types: T1 -> T2 -> ... -> ReturnType
    parameters.iter().rev().fold(return_type, |acc, param| {
        let param_type = match &param.type_expr {
            Some(type_expr) => type_expr_to_errand_type(type_expr),
            None => ErrandType::ETVar(format!("param_{}", param.id.name)), // Create type variable for untyped params
        };
        ErrandType::Arrow(Box::new(param_type), Box::new(acc))
    })
}

fn compile_expression(ctx: &mut PreIR, expr: &Expression) -> Result<instr_index, String> {
    match expr {
        Expression::Int(n) => {
            let instr = Instr::Literal(LiteralPl::Int(*n));
            Ok(ctx.emit_instruction(instr))
        }
        Expression::Float(f) => {
            let instr = Instr::Literal(LiteralPl::Float(*f));
            Ok(ctx.emit_instruction(instr))
        }
        Expression::Boolean(b) => {
            let instr = Instr::Literal(LiteralPl::Boolean(*b));
            Ok(ctx.emit_instruction(instr))
        }
        Expression::String(s) => {
            let instr = Instr::Literal(LiteralPl::String(s.clone()));
            Ok(ctx.emit_instruction(instr))
        }
        Expression::Symbol(s) => {
            let instr = Instr::Literal(LiteralPl::Symbol(s.clone()));
            Ok(ctx.emit_instruction(instr))
        }
        Expression::Identifier { id, type_expr: _ } => {
            // Look up variable in declaration table
            let instr = Instr::VarRef(VarRefData {
                name: id.name.clone(),
            });
            Ok(ctx.emit_instruction(instr))
        }
        Expression::UnaryOp { operator, operand } => {
            let operand_idx = compile_expression(ctx, operand)?;
            let instr = Instr::UnOp(UnOpPl {
                op: operator.clone(),
                operand: operand_idx,
            });
            Ok(ctx.emit_instruction(instr))
        }
        Expression::BinaryOp {
            operator,
            left,
            right,
        } => {
            match operator {
                BinaryOperator::Assignment => {
                    // Handle variable assignment
                    if let Expression::Identifier { id, type_expr } = left.as_ref() {
                        let value_idx = compile_expression(ctx, right)?;

                        // Create or update variable declaration
                        let instr = Instr::VarDecl(VarDeclData {
                            name: id.name.clone(),
                            value: value_idx,
                            declared_type: type_expr.clone(),
                        });

                        // Return the value instruction index (assignments evaluate to their value)
                        Ok(ctx.emit_instruction(instr))
                    } else {
                        Err("Left side of assignment must be an identifier".to_string())
                    }
                }
                _ => {
                    let left_idx = compile_expression(ctx, left)?;
                    let right_idx = compile_expression(ctx, right)?;
                    let instr = Instr::BinOp(BinOpPl {
                        op: operator.clone(),
                        left: left_idx,
                        right: right_idx,
                    });
                    Ok(ctx.emit_instruction(instr))
                }
            }
        }
        Expression::FunctionCall { id, arguments } => {
            // `typeof(arg)` is a built-in instruction, not a regular function
            // call. Lower it to `Instr::Typeof` so it bypasses overload
            // dispatch entirely; analysis and SIR generation handle it.
            if id.name == "typeof" {
                if arguments.len() != 1 {
                    return Err(format!(
                        "typeof expects exactly one argument, got {}",
                        arguments.len()
                    ));
                }
                let operand_idx = compile_expression(ctx, &arguments[0])?;
                return Ok(ctx.emit_instruction(Instr::Typeof(operand_idx)));
            }

            let mut arg_indices = Vec::new();
            for arg in arguments {
                arg_indices.push(compile_expression(ctx, arg)?);
            }

            let instr = Instr::FnCall(FnCallPl {
                name: id.name.clone(),
                arguments: arg_indices,
            });
            Ok(ctx.emit_instruction(instr))
        }
        Expression::FunctionDefinition {
            id,
            parameters,
            body,
            return_type_expr,
            foreign,
        } => {
            let body_idx = compile_expression(ctx, body)?;

            let func = Instr::FuncDecl(FuncData {
                name: id.name.clone(),
                parameters: parameters.clone(),
                body_index: body_idx,
                return_type: return_type_expr.clone(),
                is_foreign: *foreign,
            });

            Ok(ctx.emit_instruction(func))
        }
        Expression::StructDefinition {
            id,
            type_params,
            fields,
        } => {
            let struct_data = Instr::StructDecl(StructData {
                name: id.name.clone(),
                type_params: type_params.iter().map(|p| p.clone()).collect(),
                fields: fields.clone(),
            });
            Ok(ctx.emit_instruction(struct_data))
        }
        Expression::EnumDefinition {
            id,
            type_params,
            variants,
        } => {
            let enum_data = Instr::EnumDecl(EnumData {
                name: id.name.clone(),
                type_params: type_params.iter().map(|p| p.clone()).collect(),
                variants: variants
                    .iter()
                    .map(|v| EnumVariantInfo {
                        name: v.name.clone(),
                        fields: v
                            .fields
                            .iter()
                            .map(|f| (f.id.name.clone(), f.field_type.clone()))
                            .collect(),
                    })
                    .collect(),
            });
            Ok(ctx.emit_instruction(enum_data))
        }
        Expression::EnumVariantAccess { enum_name, variant } => Ok(ctx.emit_instruction(
            Instr::EnumVariantAccess(EnumVariantData {
                enum_name: enum_name.clone(),
                variant: variant.clone(),
            }),
        )),
        Expression::EnumVariantConstruct {
            enum_name,
            variant,
            args,
        } => {
            let arg_indices: Vec<instr_index> = args
                .iter()
                .map(|a| compile_expression(ctx, a))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(
                ctx.emit_instruction(Instr::EnumVariantConstruct(EnumVariantConstructData {
                    enum_name: enum_name.clone(),
                    variant: variant.clone(),
                    arg_indices,
                })),
            )
        }
        Expression::Match { value, cases } => {
            let scrutinee_idx = compile_expression(ctx, value)?;

            // Resolve enum_name from the first non-wildcard pattern.
            let enum_name = cases
                .iter()
                .find_map(|c| {
                    if let MatchPattern::EnumVariant { enum_name, .. } = &c.pattern {
                        Some(enum_name.clone())
                    } else {
                        None
                    }
                })
                .ok_or_else(|| {
                    "match expression has only wildcard arms — cannot determine enum type"
                        .to_string()
                })?;

            // Build tag lookup: variant_name → tag index.
            let tag_map: std::collections::HashMap<String, i64> = ctx
                .instructions
                .iter()
                .find_map(|instr| {
                    if let Instr::EnumDecl(d) = instr {
                        if d.name == enum_name {
                            Some(
                                d.variants
                                    .iter()
                                    .enumerate()
                                    .map(|(i, v)| (v.name.clone(), i as i64))
                                    .collect(),
                            )
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                })
                .ok_or_else(|| format!("match: unknown enum `{}`", enum_name))?;

            // Compile each arm body and resolve tag.
            let arms: Vec<MatchArmData> = cases
                .iter()
                .map(|c| {
                    let (tag, bindings) = match &c.pattern {
                        MatchPattern::Wildcard => (None, vec![]),
                        MatchPattern::EnumVariant {
                            variant, bindings, ..
                        } => {
                            let tag = tag_map.get(variant).copied();
                            (tag, bindings.clone())
                        }
                    };
                    let body = compile_expression(ctx, &c.body)?;
                    Ok(MatchArmData {
                        tag,
                        bindings,
                        body,
                    })
                })
                .collect::<Result<Vec<_>, String>>()?;

            Ok(ctx.emit_instruction(Instr::Match(MatchData {
                scrutinee: scrutinee_idx,
                enum_name,
                arms,
            })))
        }
        Expression::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let condition_idx = compile_expression(ctx, condition)?;
            let then_idx = compile_expression(ctx, then_branch)?;
            let else_idx = if let Some(else_expr) = else_branch {
                Some(compile_expression(ctx, else_expr)?)
            } else {
                None
            };

            let instr = Instr::IfStatement(IfStatementData {
                condition: condition_idx,
                then_branch: then_idx,
                else_branch: else_idx,
            });
            Ok(ctx.emit_instruction(instr))
        }
        Expression::While { condition, body } => {
            let condition_idx = compile_expression(ctx, condition)?;
            let body_idx = compile_expression(ctx, body)?;

            let instr = Instr::WhileLoop(WhileLoopData {
                condition: condition_idx,
                body: body_idx,
            });
            Ok(ctx.emit_instruction(instr))
        }
        Expression::For {
            iterator,
            range,
            body,
        } => {
            // For now, treat For loops similar to While loops
            // In a complete implementation, we'd desugar this to a while loop
            let iterator = iterator.name.clone();
            let range_idx = compile_expression(ctx, range)?;
            let body_idx = compile_expression(ctx, body)?;
            let instr = Instr::ForLoop(ForLoopData {
                iterator: iterator,
                range: range_idx,
                body: body_idx,
            });
            Ok(ctx.emit_instruction(instr))
        }
        Expression::Block(expressions) => {
            let instr_start = ctx.instructions.len() as instr_index;

            let mut last_instr_idx = None;
            for expr in expressions {
                last_instr_idx = Some(compile_expression(ctx, expr)?);
            }

            let instr_end = ctx.instructions.len() as instr_index;

            let return_loc = last_instr_idx.unwrap_or_else(|| {
                // Empty block returns unit
                ctx.emit_instruction(Instr::Literal(LiteralPl::Unit))
            });

            let instr = Instr::Region(RegionData {
                instr_start,
                instr_end,
                return_loc,
            });
            Ok(ctx.emit_instruction(instr))
        }
        Expression::Return(expr) => {
            let value_idx = if let Some(return_expr) = expr {
                Some(compile_expression(ctx, return_expr)?)
            } else {
                None
            };

            let instr = Instr::Return(ReturnData { value: value_idx });
            Ok(ctx.emit_instruction(instr))
        }
        Expression::Print(expr) => {
            let _value_idx = compile_expression(ctx, expr)?;
            // For now, we don't have a Print instruction in the new IR
            // We could add it or handle it differently
            // For now, let's just return the value (this might need refinement)
            Err("Print expressions not yet implemented in new IR".to_string())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::ast::{
        BinaryOperator, Expression, Id, Parameter, Program, TypeExpression,
    };

    #[test]
    fn test_basic_ir_generation() {
        let program = Program {
            expressions: vec![
                // x = 42
                Expression::BinaryOp {
                    operator: BinaryOperator::Assignment,
                    left: Box::new(Expression::Identifier {
                        id: Id {
                            name: "x".to_string(),
                        },
                        type_expr: None,
                    }),
                    right: Box::new(Expression::Int(42)),
                },
            ],
        };

        let preir = compile_preir(&program).expect("Compilation should succeed");

        // Should have at least 2 instructions: literal 42, and the assignment creates a unit
        assert!(preir.instructions.len() >= 1);

        // Check that we have a literal instruction
        assert!(matches!(
            preir.instructions[0],
            Instr::Literal(LiteralPl::Int(42))
        ));
    }

    #[test]
    fn test_function_definition() {
        let program = Program {
            expressions: vec![Expression::FunctionDefinition {
                id: Id {
                    name: "add".to_string(),
                },
                parameters: vec![
                    Parameter {
                        id: Id {
                            name: "a".to_string(),
                        },
                        type_expr: Some(TypeExpression::Int),
                    },
                    Parameter {
                        id: Id {
                            name: "b".to_string(),
                        },
                        type_expr: Some(TypeExpression::Int),
                    },
                ],
                body: Box::new(Expression::Return(Some(Box::new(Expression::BinaryOp {
                    operator: BinaryOperator::Add,
                    left: Box::new(Expression::Identifier {
                        id: Id {
                            name: "a".to_string(),
                        },
                        type_expr: None,
                    }),
                    right: Box::new(Expression::Identifier {
                        id: Id {
                            name: "b".to_string(),
                        },
                        type_expr: None,
                    }),
                })))),
                return_type_expr: Some(TypeExpression::Int),
                foreign: false,
            }],
        };

        let preir = compile_preir(&program).expect("Compilation should succeed");

        // Should have instructions for the function body and a unit return
        assert!(preir.instructions.len() >= 1);
    }

    #[test]
    fn test_instruction_formatting() {
        let program = Program {
            expressions: vec![
                // x = 42
                Expression::BinaryOp {
                    operator: BinaryOperator::Assignment,
                    left: Box::new(Expression::Identifier {
                        id: Id {
                            name: "x".to_string(),
                        },
                        type_expr: None,
                    }),
                    right: Box::new(Expression::Int(42)),
                },
                // y = 10
                Expression::BinaryOp {
                    operator: BinaryOperator::Assignment,
                    left: Box::new(Expression::Identifier {
                        id: Id {
                            name: "y".to_string(),
                        },
                        type_expr: None,
                    }),
                    right: Box::new(Expression::Int(10)),
                },
            ],
        };

        let preir = compile_preir(&program).expect("Compilation should succeed");

        // Get the formatted output to verify structure
        let formatted_output = preir.format_all();

        // Verify the output contains expected elements (see `PreIR::format_all`).
        assert!(formatted_output.contains("%0"));
        assert!(formatted_output.contains("Main:"));

        // Check that we have variable declarations (`format_instruction` for VarDecl).
        assert!(formatted_output.contains("var x ="));
        assert!(formatted_output.contains("var y ="));

        // Print for manual verification
        println!("\n=== PreIR Formatting Test Output ===");
        println!("{}", formatted_output);
        println!("=== End PreIR Formatting Test Output ===\n");
    }
}
