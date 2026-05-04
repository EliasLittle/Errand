use crate::backend::errand_builtins::{
    add_builtin_data_constructors, add_builtin_functions, type_expr_to_errand_type,
};
use crate::backend::preir::{
    instr_index, BinOpPl, EnumData, EnumVariantConstructData, EnumVariantData, EnumVariantInfo,
    FnCallPl, FuncData, IfStatementData, Instr, LiteralPl, MatchArmData, MatchData, PreIR,
    RegionData, ReturnData, StructData, UnOpPl, VarDeclData, VarRefData, WhileLoopData,
};
use crate::backend::worklist::ErrandType;
use crate::frontend::ast::MatchPattern;
use crate::frontend::ast::{
    BinaryOperator, Expression, FieldDefinition, GenericArg, Id, Program, TypeExpression,
};
use std::collections::HashMap;

/// Enum name → ordered variant names (for recognising `Enum::Variant` in the AST).
fn collect_enum_names(program: &Program) -> HashMap<String, Vec<String>> {
    let mut enum_names = HashMap::new();
    for expr in &program.expressions {
        if let Expression::EnumDefinition { id, variants, .. } = expr {
            let variant_list: Vec<String> = variants.iter().map(|v| v.name.clone()).collect();
            enum_names.insert(id.name.clone(), variant_list);
        }
    }
    enum_names
}

/// Same shape as `frontend/lower.rs` used to synthesise: `fn StructName(...) = return new(:StructName, ...)`.
fn build_struct_constructor_expression(
    id: &Id,
    fields: &[FieldDefinition],
    type_params: &[Id],
) -> Expression {
    let parameters: Vec<crate::frontend::ast::Parameter> = fields
        .iter()
        .map(|f| crate::frontend::ast::Parameter {
            id: f.id.clone(),
            type_expr: Some(f.field_type.clone()),
        })
        .collect();
    let mut new_args = vec![Expression::Symbol(id.name.clone())];
    new_args.extend(fields.iter().map(|f| Expression::Identifier {
        id: f.id.clone(),
        type_expr: Some(f.field_type.clone()),
    }));
    let body = Box::new(Expression::Return(Some(Box::new(
        Expression::FunctionCall {
            id: Id {
                name: "new".to_string(),
            },
            arguments: new_args,
        },
    ))));
    let return_type_expr = if type_params.is_empty() {
        Some(TypeExpression::Struct(id.clone(), None, None))
    } else {
        let gen_args: Vec<GenericArg> = type_params
            .iter()
            .map(|t| GenericArg::Type(TypeExpression::Struct(t.clone(), None, None)))
            .collect();
        Some(TypeExpression::Struct(id.clone(), None, Some(gen_args)))
    };
    Expression::FunctionDefinition {
        id: id.clone(),
        parameters,
        body,
        return_type_expr,
        foreign: false,
    }
}

pub fn compile_preir(program: &Program) -> Result<PreIR, String> {
    let _span = tracing::debug_span!(
        target: "preir",
        "preir.compile",
        top_level_exprs = program.expressions.len()
    )
    .entered();
    tracing::debug!(target: "preir", "compile PreIR: metadata pass");

    let enum_names = collect_enum_names(program);
    let mut ctx = PreIR::init();

    // Pass 1: struct and enum declarations (metadata only; no function bodies).
    for expr in &program.expressions {
        match expr {
            Expression::StructDefinition {
                id,
                type_params,
                fields,
                ..
            } => {
                let struct_data = StructData {
                    name: id.name.clone(),
                    type_params: type_params.iter().map(|p| p.clone()).collect(),
                    fields: fields.clone(),
                };
                ctx.emit_instruction(Instr::StructDecl(struct_data));
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

    tracing::debug!(target: "preir", "compile PreIR: struct constructors");

    // Pass 2: synthetic struct constructors before any user function bodies
    for expr in &program.expressions {
        if let Expression::StructDefinition {
            id,
            type_params,
            fields,
            ..
        } = expr
        {
            let struct_constructor_expression =
                build_struct_constructor_expression(id, fields, type_params);
            if let Expression::FunctionDefinition {
                id: constructor_function_id,
                parameters,
                body,
                return_type_expr,
                foreign,
            } = struct_constructor_expression
            {
                let body_idx = compile_expression(&mut ctx, &enum_names, body.as_ref())?;
                let func = FuncData {
                    name: constructor_function_id.name.clone(),
                    parameters,
                    body_index: body_idx,
                    return_type: return_type_expr,
                    is_foreign: foreign,
                };
                ctx.emit_instruction(Instr::FuncDecl(func));
            }
        }
    }

    tracing::debug!(target: "preir", "compile PreIR: user and foreign functions");

    // Pass 3: user-defined (and foreign) functions.
    for expr in &program.expressions {
        if let Expression::FunctionDefinition {
            id,
            parameters,
            body,
            return_type_expr,
            foreign,
        } = expr
        {
            let func = FuncData {
                name: id.name.clone(),
                parameters: parameters.clone(),
                body_index: compile_expression(&mut ctx, &enum_names, body)?,
                return_type: return_type_expr.clone(),
                is_foreign: *foreign,
            };
            ctx.emit_instruction(Instr::FuncDecl(func));
        }
    }

    tracing::debug!(target: "preir", "compile PreIR: main region");

    // Second pass: compile all expressions and build main region
    let mut main_instructions = Vec::new();

    let instr_start = ctx.instructions.len() as instr_index;
    for expr in &program.expressions {
        match expr {
            Expression::FunctionDefinition { .. } => continue,
            Expression::StructDefinition { .. } => continue,
            Expression::EnumDefinition { .. } => continue,
            _ => {
                let instr_idx = compile_expression(&mut ctx, &enum_names, expr)?;
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

    tracing::debug!(
        target: "preir",
        instructions = ctx.instructions.len(),
        "compile PreIR complete"
    );

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
    let _span = tracing::trace_span!(
        target: "preir",
        "preir.extract_typing_context",
        top_level = program.expressions.len()
    )
    .entered();

    let data_constructors = add_builtin_data_constructors();
    let mut function_types = add_builtin_functions();

    // Extract user-defined function signatures and variable assignments
    for expr in &program.expressions {
        tracing::trace!(target: "preir", expr = ?expr, "typing context scan");
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

fn compile_expression(
    ctx: &mut PreIR,
    enum_names: &HashMap<String, Vec<String>>,
    expr: &Expression,
) -> Result<instr_index, String> {
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
        Expression::Identifier { id, type_expr } => {
            if let Some(TypeExpression::Struct(variant_id, None, None)) = type_expr {
                if let Some(variants) = enum_names.get(&id.name) {
                    if variants.iter().any(|v| v == &variant_id.name) {
                        return Ok(ctx.emit_instruction(Instr::EnumVariantAccess(
                            EnumVariantData {
                                enum_name: id.name.clone(),
                                variant: variant_id.name.clone(),
                            },
                        )));
                    }
                }
            }
            let instr = Instr::VarRef(VarRefData {
                name: id.name.clone(),
            });
            Ok(ctx.emit_instruction(instr))
        }
        Expression::UnaryOp { operator, operand } => {
            let operand_idx = compile_expression(ctx, enum_names, operand)?;
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
                        let value_idx = compile_expression(ctx, enum_names, right)?;

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
                    if matches!(operator, BinaryOperator::Dot) {
                        return compile_field_access(ctx, enum_names, left, right);
                    }
                    let left_idx = compile_expression(ctx, enum_names, left)?;
                    let right_idx = compile_expression(ctx, enum_names, right)?;
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
                let operand_idx = compile_expression(ctx, enum_names, &arguments[0])?;
                return Ok(ctx.emit_instruction(Instr::Typeof(operand_idx)));
            }

            let mut arg_indices = Vec::new();
            for arg in arguments {
                arg_indices.push(compile_expression(ctx, enum_names, arg)?);
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
            let body_idx = compile_expression(ctx, enum_names, body)?;

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
                .map(|a| compile_expression(ctx, enum_names, a))
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
            let scrutinee_idx = compile_expression(ctx, enum_names, value)?;

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
                    let body = compile_expression(ctx, enum_names, &c.body)?;
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
            let condition_idx = compile_expression(ctx, enum_names, condition)?;
            let then_idx = compile_expression(ctx, enum_names, then_branch)?;
            let else_idx = if let Some(else_expr) = else_branch {
                Some(compile_expression(ctx, enum_names, else_expr)?)
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
            let condition_idx = compile_expression(ctx, enum_names, condition)?;
            let body_idx = compile_expression(ctx, enum_names, body)?;

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
        } => compile_for_loop(ctx, enum_names, iterator, range, body),
        Expression::Block(expressions) => {
            let instr_start = ctx.instructions.len() as instr_index;

            let mut last_instr_idx = None;
            for expr in expressions {
                last_instr_idx = Some(compile_expression(ctx, enum_names, expr)?);
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
                Some(compile_expression(ctx, enum_names, return_expr)?)
            } else {
                None
            };

            let instr = Instr::Return(ReturnData { value: value_idx });
            Ok(ctx.emit_instruction(instr))
        }
        Expression::Print(expr) => {
            let _value_idx = compile_expression(ctx, enum_names, expr)?;
            // For now, we don't have a Print instruction in the new IR
            // We could add it or handle it differently
            // For now, let's just return the value (this might need refinement)
            Err("Print expressions not yet implemented in new IR".to_string())
        }
    }
}

fn compile_field_access(
    ctx: &mut PreIR,
    enum_names: &HashMap<String, Vec<String>>,
    left: &Expression,
    right: &Expression,
) -> Result<instr_index, String> {
    let field_symbol = match right {
        Expression::Identifier { id, .. } => id.name.clone(),
        other => {
            return Err(format!(
                "Dot field access expects an identifier as the field name, got: {:?}",
                other
            ))
        }
    };
    let left_idx = compile_expression(ctx, enum_names, left)?;
    let sym_idx = ctx.emit_instruction(Instr::Literal(LiteralPl::Symbol(field_symbol)));
    let typeof_idx = ctx.emit_instruction(Instr::Typeof(left_idx));
    Ok(ctx.emit_instruction(Instr::FnCall(FnCallPl {
        name: "getfield".to_string(),
        arguments: vec![left_idx, sym_idx, typeof_idx],
    })))
}

/// Desugar `for iter in range ...` to index, `while`, and `get` / `length` (mirrors former `lower.rs`).
fn compile_for_loop(
    ctx: &mut PreIR,
    enum_names: &HashMap<String, Vec<String>>,
    iterator: &Id,
    range: &Expression,
    body: &Expression,
) -> Result<instr_index, String> {
    let instr_start = ctx.instructions.len() as instr_index;

    let zero_idx = ctx.emit_instruction(Instr::Literal(LiteralPl::Int(0)));
    let _index_init = ctx.emit_instruction(Instr::VarDecl(VarDeclData {
        name: "%index".to_string(),
        value: zero_idx,
        declared_type: Some(TypeExpression::Int),
    }));

    let range_idx = compile_expression(ctx, enum_names, range)?;

    let index_ref_cond = ctx.emit_instruction(Instr::VarRef(VarRefData {
        name: "%index".to_string(),
    }));
    let len_call = ctx.emit_instruction(Instr::FnCall(FnCallPl {
        name: "length".to_string(),
        arguments: vec![range_idx],
    }));
    let cond_idx = ctx.emit_instruction(Instr::BinOp(BinOpPl {
        op: BinaryOperator::LessThan,
        left: index_ref_cond,
        right: len_call,
    }));

    let body_region_start = ctx.instructions.len() as instr_index;

    let index_ref_get = ctx.emit_instruction(Instr::VarRef(VarRefData {
        name: "%index".to_string(),
    }));
    let get_call = ctx.emit_instruction(Instr::FnCall(FnCallPl {
        name: "get".to_string(),
        arguments: vec![range_idx, index_ref_get],
    }));
    let _iter_assign = ctx.emit_instruction(Instr::VarDecl(VarDeclData {
        name: iterator.name.clone(),
        value: get_call,
        declared_type: None,
    }));

    let _body = compile_expression(ctx, enum_names, body)?;

    let index_ref_add_left = ctx.emit_instruction(Instr::VarRef(VarRefData {
        name: "%index".to_string(),
    }));
    let one = ctx.emit_instruction(Instr::Literal(LiteralPl::Int(1)));
    let add_idx = ctx.emit_instruction(Instr::BinOp(BinOpPl {
        op: BinaryOperator::Add,
        left: index_ref_add_left,
        right: one,
    }));
    let incr_idx = ctx.emit_instruction(Instr::VarDecl(VarDeclData {
        name: "%index".to_string(),
        value: add_idx,
        declared_type: Some(TypeExpression::Int),
    }));

    let body_region_end = ctx.instructions.len() as instr_index;
    let while_body = ctx.emit_instruction(Instr::Region(RegionData {
        instr_start: body_region_start,
        instr_end: body_region_end,
        return_loc: incr_idx,
    }));

    let while_idx = ctx.emit_instruction(Instr::WhileLoop(WhileLoopData {
        condition: cond_idx,
        body: while_body,
    }));

    let instr_end = ctx.instructions.len() as instr_index;
    Ok(ctx.emit_instruction(Instr::Region(RegionData {
        instr_start,
        instr_end,
        return_loc: while_idx,
    })))
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
