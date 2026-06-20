use crate::backend::errand_builtins::{
    add_builtin_data_constructors, add_builtin_functions, type_expr_to_errand_type,
};
use crate::backend::fir::{
    InstrIndex, BinOpPl, EnumData, EnumVariantConstructData, EnumVariantData, EnumVariantInfo,
    FnCallPl, FuncData, IfStatementData, Instr, LiteralPl, MatchArmData, MatchData, FIR,
    RegionData, ReturnData, StructData, UnOpPl, VarDeclData, VarRefData, WhileLoopData,
};
use crate::backend::worklist::ErrandType;
use crate::frontend::ast::MatchPattern;
use crate::frontend::ast::{
    BinaryOperator, EnumVariant, Expression, FieldDefinition, GenericArg, Id, MatchCase, Parameter,
    Program, TypeExpression,
};
use std::collections::HashMap;

// ---------------------------------------------------------------------------
// Public API — program-level lowering and typing-context extraction
// ---------------------------------------------------------------------------

/// Build flat FIR from a program: metadata passes, function bodies, then main region.
pub fn compile_fir(program: &Program) -> Result<FIR, String> {
    let _span = tracing::debug_span!(
        target: "fir",
        "fir.compile",
        top_level_exprs = program.expressions.len()
    )
    .entered();
    tracing::debug!(target: "fir", "compile FIR: metadata pass");

    let enum_names = collect_enum_names(program);
    let mut ctx = FIR::init();
    let mut gen = FirGen {
        ir: &mut ctx,
        enum_names: &enum_names,
    };

    // Pass 1: struct and enum declarations (metadata only; no function bodies).
    for expr in &program.expressions {
        match expr {
            Expression::StructDefinition {
                id,
                type_params,
                fields,
                ..
            } => {
                let struct_data = struct_data_from_ast(id, type_params, fields);
                gen.emit(Instr::StructDecl(struct_data));
            }
            Expression::EnumDefinition {
                id,
                type_params,
                variants,
            } => {
                let enum_data = enum_data_from_ast(id, type_params, variants);
                gen.emit(Instr::EnumDecl(enum_data));
            }
            _ => {}
        }
    }

    tracing::debug!(target: "fir", "compile FIR: struct constructors");

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
                context_params,
                body,
                return_type_expr,
                foreign,
            } = struct_constructor_expression
            {
                let body_idx = gen.compile_expression(body.as_ref())?;
                let func = func_data_from_ast(
                    &constructor_function_id,
                    parameters.as_slice(),
                    context_params.as_slice(),
                    body_idx,
                    &return_type_expr,
                    foreign,
                );
                gen.emit(Instr::FuncDecl(func));
            }
        }
    }

    tracing::debug!(target: "fir", "compile FIR: user and foreign functions");

    // Pass 3: user-defined (and foreign) functions.
    for expr in &program.expressions {
        if let Expression::FunctionDefinition {
            id,
            parameters,
            context_params,
            body,
            return_type_expr,
            foreign,
        } = expr
        {
            let body = prepare_function_body(body, return_type_expr, *foreign);
            let body_idx = gen.compile_expression(&body)?;
            let func = func_data_from_ast(
                id,
                parameters,
                context_params,
                body_idx,
                return_type_expr,
                *foreign,
            );
            gen.emit(Instr::FuncDecl(func));
        }
    }

    tracing::debug!(target: "fir", "compile FIR: main region");

    // Pass 4: compile top-level statements into the main region
    let mut main_instructions = Vec::new();

    let instr_start = gen.ir.instructions.len() as InstrIndex;
    for expr in &program.expressions {
        match expr {
            Expression::FunctionDefinition { .. } => continue,
            Expression::StructDefinition { .. } => continue,
            Expression::EnumDefinition { .. } => continue,
            _ => {
                let instr_idx = gen.compile_expression(expr)?;
                main_instructions.push(instr_idx);
            }
        }
    }
    let instr_end = gen.ir.instructions.len() as InstrIndex;

    let return_loc = if let Some(&last_instr) = main_instructions.last() {
        last_instr
    } else {
        gen.emit(Instr::Literal(LiteralPl::Unit))
    };

    let main = Instr::Region(RegionData {
        instr_start,
        instr_end,
        return_loc,
    });

    tracing::debug!(
        target: "fir",
        instructions = gen.ir.instructions.len(),
        "compile FIR complete"
    );

    Ok(FIR {
        main,
        instructions: std::mem::take(&mut gen.ir.instructions),
    })
}

/// Extract function signatures from a program for type inference (worklist / builtins).
pub fn extract_typing_context(
    program: &Program,
) -> (HashMap<String, ErrandType>, HashMap<String, ErrandType>) {
    let _span = tracing::trace_span!(
        target: "fir",
        "fir.extract_typing_context",
        top_level = program.expressions.len()
    )
    .entered();

    let data_constructors = add_builtin_data_constructors();
    let mut function_types = add_builtin_functions();

    for expr in &program.expressions {
        tracing::trace!(target: "fir", expr = ?expr, "typing context scan");
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

        if let Expression::BinaryOp {
            operator: BinaryOperator::Assignment,
            left,
            right: _,
        } = expr
        {
            if let Expression::Identifier { id, type_expr } = left.as_ref() {
                let var_type = match type_expr {
                    Some(ty) => type_expr_to_errand_type(ty),
                    None => ErrandType::ETVar(format!("var_{}", id.name)),
                };
                function_types.insert(id.name.clone(), var_type);
            }
        }
    }

    (data_constructors, function_types)
}

fn build_function_type(
    parameters: &[crate::frontend::ast::Parameter],
    return_type_expr: &Option<crate::frontend::ast::TypeExpression>,
) -> ErrandType {
    let return_type = match return_type_expr {
        Some(type_expr) => type_expr_to_errand_type(type_expr),
        None => ErrandType::Con("Unit".to_string()),
    };

    parameters.iter().rev().fold(return_type, |acc, param| {
        let param_type = match &param.type_expr {
            Some(type_expr) => type_expr_to_errand_type(type_expr),
            None => ErrandType::ETVar(format!("param_{}", param.id.name)),
        };
        ErrandType::Arrow(Box::new(param_type), Box::new(acc))
    })
}

// ---------------------------------------------------------------------------
// FirGen — recursive AST → instruction lowering (one dispatcher + per-form)
// ---------------------------------------------------------------------------

/// Mutable FIR builder with shared enum metadata (lexer-style context object).
struct FirGen<'a> {
    ir: &'a mut FIR,
    enum_names: &'a HashMap<String, Vec<String>>,
}

impl<'a> FirGen<'a> {
    fn emit(&mut self, instr: Instr) -> InstrIndex {
        self.ir.emit_instruction(instr)
    }

    fn compile_expression(&mut self, expr: &Expression) -> Result<InstrIndex, String> {
        match expr {
            Expression::Int(n) => compile_literal(self, LiteralPl::Int(*n)),
            Expression::Float(f) => compile_literal(self, LiteralPl::Float(*f)),
            Expression::Boolean(b) => compile_literal(self, LiteralPl::Boolean(*b)),
            Expression::String(s) => compile_literal(self, LiteralPl::String(s.clone())),
            Expression::Symbol(s) => compile_literal(self, LiteralPl::Symbol(s.clone())),
            Expression::Identifier { id, type_expr } => compile_identifier(self, id, type_expr),
            Expression::UnaryOp { operator, operand } => {
                compile_unary_op(self, operator, operand.as_ref())
            }
            Expression::BinaryOp {
                operator,
                left,
                right,
            } => compile_binary_op(self, operator, left.as_ref(), right.as_ref()),
            Expression::FunctionCall { id, arguments } => {
                compile_function_call(self, id, arguments)
            }
            Expression::FunctionDefinition {
                id,
                parameters,
                context_params,
                body,
                return_type_expr,
                foreign,
            } => compile_function_definition(
                self,
                id,
                parameters,
                context_params,
                body.as_ref(),
                return_type_expr,
                *foreign,
            ),
            Expression::StructDefinition {
                id,
                type_params,
                fields,
            } => compile_struct_definition(self, id, type_params, fields),
            Expression::EnumDefinition {
                id,
                type_params,
                variants,
            } => compile_enum_definition(self, id, type_params, variants),
            Expression::EnumVariantAccess { enum_name, variant } => {
                compile_enum_variant_access(self, enum_name, variant)
            }
            Expression::EnumVariantConstruct {
                enum_name,
                variant,
                args,
            } => compile_enum_variant_construct(self, enum_name, variant, args),
            Expression::Match { value, cases } => compile_match(self, value.as_ref(), cases),
            Expression::If {
                condition,
                then_branch,
                else_branch,
            } => compile_if(
                self,
                condition.as_ref(),
                then_branch.as_ref(),
                else_branch.as_ref().map(|b| b.as_ref()),
            ),
            Expression::While { condition, body } => {
                compile_while(self, condition.as_ref(), body.as_ref())
            }
            Expression::For {
                iterator,
                range,
                body,
            } => compile_for_loop(self, iterator, range.as_ref(), body.as_ref()),
            Expression::Block(expressions) => compile_block(self, expressions.as_slice()),
            Expression::Return(expr) => compile_return(self, expr.as_ref().map(|b| b.as_ref())),
            Expression::Print(expr) => compile_print(self, expr.as_ref()),
            // TODO(context-system): the implicit-context override is not yet
            // lowered. For now the body is compiled directly so existing
            // programs keep working; the `overrides` are intentionally ignored
            // until context threading lands in a later phase.
            Expression::With { body, .. } => self.compile_expression(body.as_ref()),
        }
    }
}

fn compile_literal(gen: &mut FirGen<'_>, lit: LiteralPl) -> Result<InstrIndex, String> {
    Ok(gen.emit(Instr::Literal(lit)))
}

fn compile_identifier(
    gen: &mut FirGen<'_>,
    id: &Id,
    type_expr: &Option<TypeExpression>,
) -> Result<InstrIndex, String> {
    if let Some(TypeExpression::Struct(variant_id, None, None)) = type_expr {
        if let Some(variants) = gen.enum_names.get(&id.name) {
            if variants.iter().any(|v| v == &variant_id.name) {
                return Ok(gen.emit(Instr::EnumVariantAccess(EnumVariantData {
                    enum_name: id.name.clone(),
                    variant: variant_id.name.clone(),
                })));
            }
        }
    }
    Ok(gen.emit(Instr::VarRef(VarRefData {
        name: id.name.clone(),
    })))
}

fn compile_unary_op(
    gen: &mut FirGen<'_>,
    operator: &crate::frontend::ast::UnaryOperator,
    operand: &Expression,
) -> Result<InstrIndex, String> {
    let operand_idx = gen.compile_expression(operand)?;
    Ok(gen.emit(Instr::UnOp(UnOpPl {
        op: operator.clone(),
        operand: operand_idx,
    })))
}

fn compile_binary_op(
    gen: &mut FirGen<'_>,
    operator: &BinaryOperator,
    left: &Expression,
    right: &Expression,
) -> Result<InstrIndex, String> {
    match operator {
        BinaryOperator::Assignment => {
            if let Expression::Identifier { id, type_expr } = left {
                let value_idx = gen.compile_expression(right)?;
                Ok(gen.emit(Instr::VarDecl(VarDeclData {
                    name: id.name.clone(),
                    value: value_idx,
                    declared_type: type_expr.clone(),
                })))
            } else {
                Err("Left side of assignment must be an identifier".to_string())
            }
        }
        BinaryOperator::Dot => compile_field_access(gen, left, right),
        _ => {
            let left_idx = gen.compile_expression(left)?;
            let right_idx = gen.compile_expression(right)?;
            Ok(gen.emit(Instr::BinOp(BinOpPl {
                op: operator.clone(),
                left: left_idx,
                right: right_idx,
            })))
        }
    }
}

fn compile_field_access(
    gen: &mut FirGen<'_>,
    left: &Expression,
    right: &Expression,
) -> Result<InstrIndex, String> {
    let field_symbol = match right {
        Expression::Identifier { id, .. } => id.name.clone(),
        other => {
            return Err(format!(
                "Dot field access expects an identifier as the field name, got: {:?}",
                other
            ))
        }
    };
    let left_idx = gen.compile_expression(left)?;
    let sym_idx = gen.emit(Instr::Literal(LiteralPl::Symbol(field_symbol)));
    let typeof_idx = gen.emit(Instr::Typeof(left_idx));
    Ok(gen.emit(Instr::FnCall(FnCallPl {
        name: "getfield".to_string(),
        arguments: vec![left_idx, sym_idx, typeof_idx],
    })))
}

/// Built-in calls that lower to dedicated `Instr` variants (not overload dispatch).
fn try_emit_builtin_call(
    gen: &mut FirGen<'_>,
    id: &Id,
    arguments: &[Expression],
) -> Option<Result<InstrIndex, String>> {
    match id.name.as_str() {
        "typeof" => Some(compile_typeof_call(gen, arguments)),
        _ => None,
    }
}

fn compile_typeof_call(
    gen: &mut FirGen<'_>,
    arguments: &[Expression],
) -> Result<InstrIndex, String> {
    if arguments.len() != 1 {
        return Err(format!(
            "typeof expects exactly one argument, got {}",
            arguments.len()
        ));
    }
    let operand_idx = gen.compile_expression(&arguments[0])?;
    Ok(gen.emit(Instr::Typeof(operand_idx)))
}

fn compile_function_call(
    gen: &mut FirGen<'_>,
    id: &Id,
    arguments: &[Expression],
) -> Result<InstrIndex, String> {
    if let Some(r) = try_emit_builtin_call(gen, id, arguments) {
        return r;
    }
    // `printf` string literals are C ABI / heap temporaries: desugar into a
    // block that mallocs+copies each string, calls `printf`, then frees. The
    // desugared call carries temp identifiers (not `String`s), so it does not
    // re-enter this branch.
    if id.name == "printf" && arguments.iter().any(|a| matches!(a, Expression::String(_))) {
        let desugared = desugar_printf(id, arguments);
        return gen.compile_expression(&desugared);
    }
    let mut arg_indices = Vec::new();
    for arg in arguments {
        arg_indices.push(gen.compile_expression(arg)?);
    }
    Ok(gen.emit(Instr::FnCall(FnCallPl {
        name: id.name.clone(),
        arguments: arg_indices,
    })))
}

fn compile_function_definition(
    gen: &mut FirGen<'_>,
    id: &Id,
    parameters: &[Parameter],
    context_params: &[Parameter],
    body: &Expression,
    return_type_expr: &Option<TypeExpression>,
    foreign: bool,
) -> Result<InstrIndex, String> {
    let body = prepare_function_body(body, return_type_expr, foreign);
    let body_idx = gen.compile_expression(&body)?;
    let func = func_data_from_ast(
        id,
        parameters,
        context_params,
        body_idx,
        return_type_expr,
        foreign,
    );
    Ok(gen.emit(Instr::FuncDecl(func)))
}

fn compile_struct_definition(
    gen: &mut FirGen<'_>,
    id: &Id,
    type_params: &[Id],
    fields: &[FieldDefinition],
) -> Result<InstrIndex, String> {
    let struct_data = struct_data_from_ast(id, type_params, fields);
    Ok(gen.emit(Instr::StructDecl(struct_data)))
}

fn compile_enum_definition(
    gen: &mut FirGen<'_>,
    id: &Id,
    type_params: &[Id],
    variants: &[EnumVariant],
) -> Result<InstrIndex, String> {
    let enum_data = enum_data_from_ast(id, type_params, variants);
    Ok(gen.emit(Instr::EnumDecl(enum_data)))
}

fn compile_enum_variant_access(
    gen: &mut FirGen<'_>,
    enum_name: &str,
    variant: &str,
) -> Result<InstrIndex, String> {
    Ok(gen.emit(Instr::EnumVariantAccess(EnumVariantData {
        enum_name: enum_name.to_string(),
        variant: variant.to_string(),
    })))
}

fn compile_enum_variant_construct(
    gen: &mut FirGen<'_>,
    enum_name: &str,
    variant: &str,
    args: &[Expression],
) -> Result<InstrIndex, String> {
    let arg_indices: Vec<InstrIndex> = args
        .iter()
        .map(|a| gen.compile_expression(a))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(
        gen.emit(Instr::EnumVariantConstruct(EnumVariantConstructData {
            enum_name: enum_name.to_string(),
            variant: variant.to_string(),
            arg_indices,
        })),
    )
}

fn compile_match(
    gen: &mut FirGen<'_>,
    value: &Expression,
    cases: &[MatchCase],
) -> Result<InstrIndex, String> {
    let scrutinee_idx = gen.compile_expression(value)?;

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
            "match expression has only wildcard arms — cannot determine enum type".to_string()
        })?;

    let tag_map = enum_variant_tag_map(gen.enum_names, &enum_name)?;

    let arms: Vec<MatchArmData> = cases
        .iter()
        .map(|c| {
            let (tag, bindings) = match &c.pattern {
                MatchPattern::Wildcard => (None, vec![]),
                MatchPattern::EnumVariant {
                    variant, bindings, ..
                } => {
                    let tag = tag_map.get(variant.as_str()).copied();
                    (tag, bindings.clone())
                }
            };
            let body = gen.compile_expression(&c.body)?;
            Ok(MatchArmData {
                tag,
                bindings,
                body,
            })
        })
        .collect::<Result<Vec<_>, String>>()?;

    Ok(gen.emit(Instr::Match(MatchData {
        scrutinee: scrutinee_idx,
        enum_name,
        arms,
    })))
}

fn compile_if(
    gen: &mut FirGen<'_>,
    condition: &Expression,
    then_branch: &Expression,
    else_branch: Option<&Expression>,
) -> Result<InstrIndex, String> {
    let condition_idx = gen.compile_expression(condition)?;
    let then_idx = gen.compile_expression(then_branch)?;
    let else_idx = if let Some(else_expr) = else_branch {
        Some(gen.compile_expression(else_expr)?)
    } else {
        None
    };
    Ok(gen.emit(Instr::IfStatement(IfStatementData {
        condition: condition_idx,
        then_branch: then_idx,
        else_branch: else_idx,
    })))
}

fn compile_while(
    gen: &mut FirGen<'_>,
    condition: &Expression,
    body: &Expression,
) -> Result<InstrIndex, String> {
    let condition_idx = gen.compile_expression(condition)?;
    let body_idx = gen.compile_expression(body)?;
    Ok(gen.emit(Instr::WhileLoop(WhileLoopData {
        condition: condition_idx,
        body: body_idx,
    })))
}

/// Desugar `for iter in range ...` to index, `while`, and `get` / `length` (mirrors former `lower.rs`).
fn compile_for_loop(
    gen: &mut FirGen<'_>,
    iterator: &Id,
    range: &Expression,
    body: &Expression,
) -> Result<InstrIndex, String> {
    let instr_start = gen.ir.instructions.len() as InstrIndex;

    let zero_idx = gen.emit(Instr::Literal(LiteralPl::Int(0)));
    let _index_init = gen.emit(Instr::VarDecl(VarDeclData {
        name: "%index".to_string(),
        value: zero_idx,
        declared_type: Some(TypeExpression::Int),
    }));

    let range_idx = gen.compile_expression(range)?;

    let index_ref_cond = gen.emit(Instr::VarRef(VarRefData {
        name: "%index".to_string(),
    }));
    let len_call = gen.emit(Instr::FnCall(FnCallPl {
        name: "length".to_string(),
        arguments: vec![range_idx],
    }));
    let cond_idx = gen.emit(Instr::BinOp(BinOpPl {
        op: BinaryOperator::LessThan,
        left: index_ref_cond,
        right: len_call,
    }));

    let body_region_start = gen.ir.instructions.len() as InstrIndex;

    let index_ref_get = gen.emit(Instr::VarRef(VarRefData {
        name: "%index".to_string(),
    }));
    let get_call = gen.emit(Instr::FnCall(FnCallPl {
        name: "get".to_string(),
        arguments: vec![range_idx, index_ref_get],
    }));
    let _iter_assign = gen.emit(Instr::VarDecl(VarDeclData {
        name: iterator.name.clone(),
        value: get_call,
        declared_type: None,
    }));

    let _body = gen.compile_expression(body)?;

    let index_ref_add_left = gen.emit(Instr::VarRef(VarRefData {
        name: "%index".to_string(),
    }));
    let one = gen.emit(Instr::Literal(LiteralPl::Int(1)));
    let add_idx = gen.emit(Instr::BinOp(BinOpPl {
        op: BinaryOperator::Add,
        left: index_ref_add_left,
        right: one,
    }));
    let incr_idx = gen.emit(Instr::VarDecl(VarDeclData {
        name: "%index".to_string(),
        value: add_idx,
        declared_type: Some(TypeExpression::Int),
    }));

    let body_region_end = gen.ir.instructions.len() as InstrIndex;
    let while_body = gen.emit(Instr::Region(RegionData {
        instr_start: body_region_start,
        instr_end: body_region_end,
        return_loc: incr_idx,
    }));

    let while_idx = gen.emit(Instr::WhileLoop(WhileLoopData {
        condition: cond_idx,
        body: while_body,
    }));

    let instr_end = gen.ir.instructions.len() as InstrIndex;
    Ok(gen.emit(Instr::Region(RegionData {
        instr_start,
        instr_end,
        return_loc: while_idx,
    })))
}

fn compile_block(
    gen: &mut FirGen<'_>,
    expressions: &[Box<Expression>],
) -> Result<InstrIndex, String> {
    let instr_start = gen.ir.instructions.len() as InstrIndex;
    let mut last_instr_idx = None;
    for expr in expressions {
        last_instr_idx = Some(gen.compile_expression(expr.as_ref())?);
    }
    let instr_end = gen.ir.instructions.len() as InstrIndex;
    let return_loc = last_instr_idx.unwrap_or_else(|| gen.emit(Instr::Literal(LiteralPl::Unit)));
    Ok(gen.emit(Instr::Region(RegionData {
        instr_start,
        instr_end,
        return_loc,
    })))
}

fn compile_return(
    gen: &mut FirGen<'_>,
    expr: Option<&Expression>,
) -> Result<InstrIndex, String> {
    let value_idx = if let Some(return_expr) = expr {
        Some(gen.compile_expression(return_expr)?)
    } else {
        None
    };
    Ok(gen.emit(Instr::Return(ReturnData { value: value_idx })))
}

fn compile_print(gen: &mut FirGen<'_>, expr: &Expression) -> Result<InstrIndex, String> {
    let _value_idx = gen.compile_expression(expr)?;
    Err("Print expressions not yet implemented in new IR".to_string())
}

// ---------------------------------------------------------------------------
// Shared helpers — decl shapes, enum metadata, struct constructor desugar
// ---------------------------------------------------------------------------

/// Enum name → ordered variant names (for `Enum::Variant` in the AST and match tags).
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

fn struct_data_from_ast(id: &Id, type_params: &[Id], fields: &[FieldDefinition]) -> StructData {
    StructData {
        name: id.name.clone(),
        type_params: type_params.to_vec(),
        fields: fields.to_vec(),
    }
}

fn enum_data_from_ast(id: &Id, type_params: &[Id], variants: &[EnumVariant]) -> EnumData {
    EnumData {
        name: id.name.clone(),
        type_params: type_params.to_vec(),
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
    }
}

fn func_data_from_ast(
    id: &Id,
    parameters: &[Parameter],
    context_params: &[Parameter],
    body_index: InstrIndex,
    return_type: &Option<TypeExpression>,
    is_foreign: bool,
) -> FuncData {
    FuncData {
        name: id.name.clone(),
        parameters: parameters.to_vec(),
        context_params: context_params.to_vec(),
        body_index,
        return_type: return_type.clone(),
        is_foreign,
    }
}

/// Variant name → tag index, consistent with [`EnumData`] declaration order.
fn enum_variant_tag_map(
    enum_names: &HashMap<String, Vec<String>>,
    enum_name: &str,
) -> Result<HashMap<String, i64>, String> {
    let variants = enum_names
        .get(enum_name)
        .ok_or_else(|| format!("match: unknown enum `{}`", enum_name))?;
    Ok(variants
        .iter()
        .enumerate()
        .map(|(i, v)| (v.clone(), i as i64))
        .collect())
}

/// Same shape as `frontend/lower.rs` used to synthesise: `fn StructName(...) = return new(:StructName, ...)`.
fn build_struct_constructor_expression(
    id: &Id,
    fields: &[FieldDefinition],
    type_params: &[Id],
) -> Expression {
    let parameters: Vec<Parameter> = fields
        .iter()
        .map(|f| Parameter {
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
        context_params: vec![],
        body,
        return_type_expr,
        foreign: false,
    }
}

// ---------------------------------------------------------------------------
// Frontend desugaring formerly performed in `frontend/lower.rs`
// ---------------------------------------------------------------------------

/// Apply body-level desugaring before lowering a function definition.
/// Non-foreign functions gain an implicit `return` when they lack one; foreign
/// declarations are left untouched (they have no real body to lower).
fn prepare_function_body(
    body: &Expression,
    return_type_expr: &Option<TypeExpression>,
    foreign: bool,
) -> Expression {
    if foreign {
        body.clone()
    } else {
        ensure_implicit_return(body.clone(), return_type_expr)
    }
}

/// Append an implicit `return` to a function body that has no explicit return.
/// `Void` functions return unit; everything else returns `0` for now.
fn ensure_implicit_return(
    body: Expression,
    return_type_expr: &Option<TypeExpression>,
) -> Expression {
    if has_return_statement(&body) {
        return body;
    }

    let implicit_return = match return_type_expr {
        Some(TypeExpression::Void) => Expression::Return(None),
        _ => Expression::Return(Some(Box::new(Expression::Int(0)))),
    };
    // TODO: Change the return type and value to that of the last expression in the body

    match body {
        Expression::Block(mut expressions) => {
            expressions.push(Box::new(implicit_return));
            Expression::Block(expressions)
        }
        other => Expression::Block(vec![Box::new(other), Box::new(implicit_return)]),
    }
}

/// Whether every syntactic statement position reaches an explicit `return`.
/// Only walks statement positions — not operands, call arguments, etc.
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
        Expression::Match { cases, .. } => cases.iter().any(|c| has_return_statement(&c.body)),
        _ => false,
    }
}

/// Desugar a `printf` call containing string literals into a block of heap
/// temporaries: each `String` arg is `malloc`'d, copied, passed by pointer, and
/// freed after the call.
fn desugar_printf(id: &Id, arguments: &[Expression]) -> Expression {
    let mut block: Vec<Box<Expression>> = Vec::new();
    let mut new_args: Vec<Expression> = Vec::new();
    let mut tmp_counter = 0;

    for arg in arguments {
        if let Expression::String(s) = arg {
            push_string_temp_for_printf(&mut block, &mut new_args, &mut tmp_counter, s.clone());
        } else {
            new_args.push(arg.clone());
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

        let fir = compile_fir(&program).expect("Compilation should succeed");

        // Should have at least 2 instructions: literal 42, and the assignment creates a unit
        assert!(fir.instructions.len() >= 1);

        // Check that we have a literal instruction
        assert!(matches!(
            fir.instructions[0],
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
                context_params: vec![],
            }],
        };

        let fir = compile_fir(&program).expect("Compilation should succeed");

        // Should have instructions for the function body and a unit return
        assert!(fir.instructions.len() >= 1);
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

        let fir = compile_fir(&program).expect("Compilation should succeed");

        // Get the formatted output to verify structure
        let formatted_output = fir.format_all();

        // Verify the output contains expected elements (see `FIR::format_all`).
        assert!(formatted_output.contains("%0"));
        assert!(formatted_output.contains("Main:"));

        // Check that we have variable declarations (`format_instruction` for VarDecl).
        assert!(formatted_output.contains("var x ="));
        assert!(formatted_output.contains("var y ="));

        // Print for manual verification
        println!("\n=== FIR Formatting Test Output ===");
        println!("{}", formatted_output);
        println!("=== End FIR Formatting Test Output ===\n");
    }

    #[test]
    fn format_func_decl_invalid_body_index_does_not_panic() {
        let fir = FIR {
            main: Instr::Region(RegionData {
                instr_start: 0,
                instr_end: 0,
                return_loc: 0,
            }),
            instructions: vec![Instr::FuncDecl(FuncData {
                name: "f".to_string(),
                parameters: vec![],
                context_params: vec![],
                body_index: 9999,
                return_type: None,
                is_foreign: false,
            })],
        };
        let s = fir.format_instr_with_context(&fir.instructions[0]);
        assert!(s.contains("f"));
        assert!(s.contains("<invalid>"));
    }
}
