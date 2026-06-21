//! Desugaring of the implicit context system into ordinary functions and calls.
//!
//! The surface language threads an implicit `context` through every call. Rather
//! than teaching the whole backend about contexts, this pass lowers the feature
//! away at the FIR-generation boundary:
//!
//!   - A function that consumes a context field (via a `[field]` binding) or
//!     forwards one to a callee gains a hidden trailing parameter `__ctx_<field>`
//!     for each context field it (transitively) needs.
//!   - `[field]` bindings become read-only aliases `field = __ctx_<field>` at the
//!     top of the body.
//!   - Calls to context-using functions get the relevant ambient context values
//!     appended as trailing arguments.
//!   - `with` blocks and per-call `[field = expr]` overrides (both parsed as
//!     [`Expression::With`]) install new ambient values for their body and then
//!     disappear.
//!
//! The hidden parameters are intentionally left untyped so the existing generic
//! (monomorphizing) function machinery specialises each context-using function
//! to the concrete field types at every call site — which is what makes the
//! multiple-dispatch `alloc(allocator, n)` resolve statically.
//!
//! ## v1 limitations
//!
//! - There is no default/root context: a context field must be supplied by some
//!   enclosing `with`/override before control reaches a consumer, otherwise this
//!   pass reports an error.
//! - Context-using analysis is keyed by function *name*, so a context-using
//!   function must not share its name with a foreign/builtin function or with a
//!   non-context overload. (The current fixtures honour this.)

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

use tracing::{debug, instrument, trace};

use crate::frontend::ast::{Expression, Id, Parameter, Program};

/// Prefix for the synthesised hidden context parameters.
fn hidden_param_name(field: &str) -> String {
    format!("__ctx_{}", field)
}

/// Lower every implicit-context construct in `program` into plain functions and
/// calls. Returns a new [`Program`]; the input is left untouched.
#[instrument(
    skip(program),
    fields(top_level_exprs = program.expressions.len()),
    name = "context_desugar.desugar",
    target = "context_desugar",
    level = "debug"
)]
pub fn desugar(program: &Program) -> Result<Program, String> {
    let needs = compute_needs(program);
    debug!(
        target: "context_desugar",
        needs = ?needs_summary(&needs),
        "computed context-field requirements per function"
    );

    let mut expressions = Vec::with_capacity(program.expressions.len());
    // Top-level statements run with no ambient context (no field is in scope
    // until a `with`/override introduces one).
    let empty_ambient: HashMap<String, Expression> = HashMap::new();
    for expr in &program.expressions {
        expressions.push(rewrite_top_level(expr, &needs, &empty_ambient)?);
    }
    Ok(Program { expressions })
}

// ─── Pass 1: which fields does each function (transitively) need? ────────────

/// Compute, for every named function, the set of context fields it requires —
/// either because it binds them via `[field]` or because it forwards them to a
/// context-using callee that is not covered by an enclosing override.
#[instrument(
    skip(program),
    name = "context_desugar.compute_needs",
    target = "context_desugar",
    level = "debug"
)]
fn compute_needs(program: &Program) -> HashMap<String, HashSet<String>> {
    // Seed each non-foreign function with the fields it declares directly.
    let mut needs: HashMap<String, HashSet<String>> = HashMap::new();
    for expr in &program.expressions {
        if let Expression::FunctionDefinition {
            id,
            context_params,
            foreign,
            ..
        } = expr
        {
            if *foreign {
                continue;
            }
            let entry = needs.entry(id.name.clone()).or_default();
            for param in context_params {
                entry.insert(param.id.name.clone());
            }
        }
    }

    trace!(target: "context_desugar", seeded = ?needs_summary(&needs), "seeded direct `[field]` requirements");

    // Propagate to a fixed point: a function needs every field that an unguarded
    // call to a context-using function requires.
    let mut iteration = 0usize;
    loop {
        iteration += 1;
        let mut changed = false;
        for expr in &program.expressions {
            if let Expression::FunctionDefinition {
                id, body, foreign, ..
            } = expr
            {
                if *foreign {
                    continue;
                }
                let mut acc = needs.get(&id.name).cloned().unwrap_or_default();
                let before = acc.len();
                collect_call_needs(body, &needs, &HashSet::new(), &mut acc);
                if acc.len() != before {
                    trace!(
                        target: "context_desugar",
                        iteration,
                        function = %id.name,
                        fields = ?sorted_set(&acc),
                        "function gained forwarded context fields"
                    );
                    needs.insert(id.name.clone(), acc);
                    changed = true;
                }
            }
        }
        if !changed {
            break;
        }
    }

    debug!(target: "context_desugar", iterations = iteration, "needs fixpoint reached");
    needs
}

/// Accumulate into `acc` every context field required by an unguarded call in
/// `expr`. `overridden` is the set of fields supplied by enclosing `with`/
/// override scopes (those do not propagate to the current function).
fn collect_call_needs(
    expr: &Expression,
    needs: &HashMap<String, HashSet<String>>,
    overridden: &HashSet<String>,
    acc: &mut HashSet<String>,
) {
    match expr {
        Expression::FunctionCall { id, arguments } => {
            if let Some(callee_needs) = needs.get(&id.name) {
                for field in callee_needs {
                    if !overridden.contains(field) {
                        acc.insert(field.clone());
                    }
                }
            }
            for arg in arguments {
                collect_call_needs(arg, needs, overridden, acc);
            }
        }
        Expression::With { overrides, body } => {
            // Override value expressions are evaluated in the outer scope.
            for ov in overrides {
                collect_call_needs(&ov.value, needs, overridden, acc);
            }
            let mut inner = overridden.clone();
            for ov in overrides {
                inner.insert(ov.field.name.clone());
            }
            collect_call_needs(body, needs, &inner, acc);
        }
        _ => {
            for child in child_exprs(expr) {
                collect_call_needs(child, needs, overridden, acc);
            }
        }
    }
}

// ─── Pass 2: rewrite the AST ─────────────────────────────────────────────────

/// Rewrite a top-level statement. Function definitions become context-threaded
/// functions; everything else is an ordinary statement evaluated with an empty
/// ambient context.
fn rewrite_top_level(
    expr: &Expression,
    needs: &HashMap<String, HashSet<String>>,
    ambient: &HashMap<String, Expression>,
) -> Result<Expression, String> {
    match expr {
        Expression::FunctionDefinition {
            id,
            type_params,
            parameters,
            context_params,
            body,
            return_type_expr,
            foreign,
        } => {
            if *foreign {
                return Ok(expr.clone());
            }
            let fields = sorted_fields(needs.get(&id.name));

            if !fields.is_empty() || !context_params.is_empty() {
                debug!(
                    target: "context_desugar",
                    function = %id.name,
                    hidden_params = ?fields,
                    bound_aliases = ?context_params.iter().map(|p| p.id.name.as_str()).collect::<Vec<_>>(),
                    "threading context through function definition"
                );
            }

            // Inside the body, each needed field is available as its hidden
            // parameter; `[field]` additionally aliases it to the bare name.
            let body_ambient: HashMap<String, Expression> = fields
                .iter()
                .map(|f| (f.clone(), ident(&hidden_param_name(f))))
                .collect();
            let rewritten_body = rewrite_expr(body, needs, &body_ambient)?;
            let body_with_bindings = prepend_context_bindings(rewritten_body, context_params);

            let mut new_params = parameters.clone();
            for field in &fields {
                new_params.push(Parameter {
                    id: Id {
                        name: hidden_param_name(field),
                    },
                    type_expr: None,
                });
            }

            Ok(Expression::FunctionDefinition {
                id: id.clone(),
                type_params: type_params.clone(),
                parameters: new_params,
                context_params: Vec::new(),
                body: Box::new(body_with_bindings),
                return_type_expr: return_type_expr.clone(),
                foreign: *foreign,
            })
        }
        _ => rewrite_expr(expr, needs, ambient),
    }
}

/// Rewrite an expression, threading `ambient` context values into context-using
/// calls and lowering `with`/override scopes.
fn rewrite_expr(
    expr: &Expression,
    needs: &HashMap<String, HashSet<String>>,
    ambient: &HashMap<String, Expression>,
) -> Result<Expression, String> {
    match expr {
        Expression::FunctionCall { id, arguments } => {
            let mut new_args = Vec::with_capacity(arguments.len());
            for arg in arguments {
                new_args.push(rewrite_expr(arg, needs, ambient)?);
            }
            let callee_fields = sorted_fields(needs.get(&id.name));
            if !callee_fields.is_empty() {
                trace!(
                    target: "context_desugar",
                    callee = %id.name,
                    fields = ?callee_fields,
                    "appending context arguments to call"
                );
            }
            for field in callee_fields {
                match ambient.get(&field) {
                    Some(value) => new_args.push(value.clone()),
                    None => {
                        return Err(format!(
                            "call to `{}` needs context field `{}`, but it is not available here \
                             (install it with `with context.{} = ...` or a `[{} = ...]` override)",
                            id.name, field, field, field
                        ))
                    }
                }
            }
            Ok(Expression::FunctionCall {
                id: id.clone(),
                arguments: new_args,
            })
        }
        Expression::With { overrides, body } => {
            trace!(
                target: "context_desugar",
                fields = ?overrides.iter().map(|o| o.field.name.as_str()).collect::<Vec<_>>(),
                "lowering `with`/per-call override (node dissolves into its body)"
            );
            // Override values are evaluated in the current (outer) ambient.
            let mut new_ambient = ambient.clone();
            for ov in overrides {
                let value = rewrite_expr(&ov.value, needs, ambient)?;
                new_ambient.insert(ov.field.name.clone(), value);
            }
            // The `with` node itself disappears: its body takes its place.
            rewrite_expr(body, needs, &new_ambient)
        }
        // Structural recursion for everything else.
        Expression::UnaryOp { operator, operand } => Ok(Expression::UnaryOp {
            operator: operator.clone(),
            operand: Box::new(rewrite_expr(operand, needs, ambient)?),
        }),
        Expression::BinaryOp {
            operator,
            left,
            right,
        } => Ok(Expression::BinaryOp {
            operator: operator.clone(),
            left: Box::new(rewrite_expr(left, needs, ambient)?),
            right: Box::new(rewrite_expr(right, needs, ambient)?),
        }),
        Expression::EnumVariantConstruct {
            enum_name,
            variant,
            args,
        } => {
            let mut new_args = Vec::with_capacity(args.len());
            for arg in args {
                new_args.push(rewrite_expr(arg, needs, ambient)?);
            }
            Ok(Expression::EnumVariantConstruct {
                enum_name: enum_name.clone(),
                variant: variant.clone(),
                args: new_args,
            })
        }
        Expression::If {
            condition,
            then_branch,
            else_branch,
        } => Ok(Expression::If {
            condition: Box::new(rewrite_expr(condition, needs, ambient)?),
            then_branch: Box::new(rewrite_expr(then_branch, needs, ambient)?),
            else_branch: match else_branch {
                Some(e) => Some(Box::new(rewrite_expr(e, needs, ambient)?)),
                None => None,
            },
        }),
        Expression::While { condition, body } => Ok(Expression::While {
            condition: Box::new(rewrite_expr(condition, needs, ambient)?),
            body: Box::new(rewrite_expr(body, needs, ambient)?),
        }),
        Expression::For {
            iterator,
            range,
            body,
        } => Ok(Expression::For {
            iterator: iterator.clone(),
            range: Box::new(rewrite_expr(range, needs, ambient)?),
            body: Box::new(rewrite_expr(body, needs, ambient)?),
        }),
        Expression::Block(exprs) => {
            let mut new_exprs = Vec::with_capacity(exprs.len());
            for e in exprs {
                new_exprs.push(Box::new(rewrite_expr(e, needs, ambient)?));
            }
            Ok(Expression::Block(new_exprs))
        }
        Expression::Return(value) => Ok(Expression::Return(match value {
            Some(v) => Some(Box::new(rewrite_expr(v, needs, ambient)?)),
            None => None,
        })),
        Expression::Print(value) => Ok(Expression::Print(Box::new(rewrite_expr(
            value, needs, ambient,
        )?))),
        Expression::Match { value, cases } => {
            let mut new_cases = Vec::with_capacity(cases.len());
            for case in cases {
                new_cases.push(crate::frontend::ast::MatchCase {
                    pattern: case.pattern.clone(),
                    body: Box::new(rewrite_expr(&case.body, needs, ambient)?),
                });
            }
            Ok(Expression::Match {
                value: Box::new(rewrite_expr(value, needs, ambient)?),
                cases: new_cases,
            })
        }
        // A nested function definition starts a fresh scope; in v1 it receives no
        // context (nested context-carrying functions are not supported yet).
        Expression::FunctionDefinition { .. } => {
            let empty: HashMap<String, Expression> = HashMap::new();
            rewrite_top_level(expr, needs, &empty)
        }
        // Leaves: literals, identifiers, type/enum declarations, variant access.
        _ => Ok(expr.clone()),
    }
}

/// Prepend `field = __ctx_<field>` read-only aliases for each declared
/// `[field]` binding to the front of the (already rewritten) body.
fn prepend_context_bindings(body: Expression, context_params: &[Parameter]) -> Expression {
    if context_params.is_empty() {
        return body;
    }
    let mut stmts: Vec<Box<Expression>> = context_params
        .iter()
        .map(|param| {
            let field = &param.id.name;
            Box::new(Expression::BinaryOp {
                operator: crate::frontend::ast::BinaryOperator::Assignment,
                left: Box::new(ident(field)),
                right: Box::new(ident(&hidden_param_name(field))),
            })
        })
        .collect();

    match body {
        Expression::Block(existing) => {
            stmts.extend(existing);
            Expression::Block(stmts)
        }
        other => {
            stmts.push(Box::new(other));
            Expression::Block(stmts)
        }
    }
}

// ─── Helpers ─────────────────────────────────────────────────────────────────

fn ident(name: &str) -> Expression {
    Expression::Identifier {
        id: Id {
            name: name.to_string(),
        },
        type_expr: None,
    }
}

/// A deterministic, log-friendly view of a single field set.
fn sorted_set(set: &HashSet<String>) -> Vec<String> {
    set.iter()
        .cloned()
        .collect::<BTreeSet<_>>()
        .into_iter()
        .collect()
}

/// A deterministic, log-friendly view of the whole needs map.
fn needs_summary(needs: &HashMap<String, HashSet<String>>) -> BTreeMap<String, Vec<String>> {
    needs
        .iter()
        .filter(|(_, fields)| !fields.is_empty())
        .map(|(name, fields)| (name.clone(), sorted_set(fields)))
        .collect()
}

/// Deterministic ordering for a function's context fields so that the parameters
/// appended at a definition match the arguments appended at every call site.
fn sorted_fields(set: Option<&HashSet<String>>) -> Vec<String> {
    match set {
        Some(fields) if !fields.is_empty() => fields
            .iter()
            .cloned()
            .collect::<BTreeSet<_>>()
            .into_iter()
            .collect(),
        _ => Vec::new(),
    }
}

/// Immediate sub-expressions of `expr`, for analysis traversal. Mirrors the
/// recursion in [`rewrite_expr`] but read-only and untyped.
fn child_exprs(expr: &Expression) -> Vec<&Expression> {
    match expr {
        Expression::UnaryOp { operand, .. } => vec![operand],
        Expression::BinaryOp { left, right, .. } => vec![left, right],
        Expression::FunctionCall { arguments, .. } => arguments.iter().collect(),
        Expression::EnumVariantConstruct { args, .. } => args.iter().collect(),
        Expression::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let mut v = vec![condition.as_ref(), then_branch.as_ref()];
            if let Some(e) = else_branch {
                v.push(e.as_ref());
            }
            v
        }
        Expression::While { condition, body } => vec![condition, body],
        Expression::For { range, body, .. } => vec![range, body],
        Expression::Block(exprs) => exprs.iter().map(|e| e.as_ref()).collect(),
        Expression::Return(Some(v)) => vec![v.as_ref()],
        Expression::Print(v) => vec![v.as_ref()],
        Expression::With { overrides, body } => {
            let mut v: Vec<&Expression> = overrides.iter().map(|o| o.value.as_ref()).collect();
            v.push(body.as_ref());
            v
        }
        Expression::Match { value, cases } => {
            let mut v = vec![value.as_ref()];
            v.extend(cases.iter().map(|c| c.body.as_ref()));
            v
        }
        Expression::FunctionDefinition { body, .. } => vec![body.as_ref()],
        _ => Vec::new(),
    }
}

#[cfg(test)]
mod tests {
    use super::desugar;
    use crate::frontend::lexer::Lexer;
    use crate::frontend::parser::Parser;

    /// Parse + desugar `src`, returning the rendered (desugared) program.
    fn desugar_display(src: &str) -> String {
        let tokens = Lexer::new(src.to_string())
            .lex("test")
            .expect("lexing should succeed");
        let program = Parser::new(tokens).parse().expect("parsing should succeed");
        let desugared = desugar(&program).expect("desugaring should succeed");
        format!("{}", desugared)
    }

    fn desugar_err(src: &str) -> String {
        let tokens = Lexer::new(src.to_string())
            .lex("test")
            .expect("lexing should succeed");
        let program = Parser::new(tokens).parse().expect("parsing should succeed");
        desugar(&program).expect_err("desugaring should fail")
    }

    #[test]
    fn context_using_function_gets_hidden_param_and_binding() {
        let out = desugar_display("fn b(n::Int)[allocator]\n    return alloc(allocator, n)\nend\n");
        // Hidden parameter appended and `[allocator]` aliased at the top of body.
        assert!(out.contains("__ctx_allocator"), "out: {out}");
        assert!(
            out.contains("allocator") && out.contains("Assignment __ctx_allocator"),
            "expected `allocator = __ctx_allocator` binding, out: {out}"
        );
        // No `with`/override remains.
        assert!(!out.contains("with "), "out: {out}");
    }

    #[test]
    fn forwarding_function_gets_field_but_no_binding() {
        let out = desugar_display(
            "fn b(n::Int)[allocator]\n    return n\nend\n\
             fn a(n::Int)\n    return b(n)\nend\n",
        );
        // `a` forwards the context: it gains the hidden param and threads it.
        assert!(out.contains("__ctx_allocator"), "out: {out}");
        // The call inside `a` threads the forwarded context parameter.
        assert!(
            out.contains("b(n::Any, __ctx_allocator::Any)"),
            "expected threaded call, out: {out}"
        );
    }

    #[test]
    fn with_override_is_removed_and_threads_value() {
        let out = desugar_display(
            "fn b(n::Int)[allocator]\n    return n\nend\n\
             with context.allocator = 5\n    b(3)\nend\n",
        );
        // The override value is threaded into the call and the `with` disappears.
        assert!(out.contains("b(3::Int, 5::Int)"), "out: {out}");
        assert!(!out.contains("with "), "out: {out}");
    }

    #[test]
    fn per_call_override_threads_value() {
        let out = desugar_display(
            "fn b(n::Int)[allocator]\n    return n\nend\n\
             b(3)[allocator = 9]\n",
        );
        assert!(out.contains("b(3::Int, 9::Int)"), "out: {out}");
    }

    #[test]
    fn missing_context_field_is_an_error() {
        let err = desugar_err(
            "fn b(n::Int)[allocator]\n    return n\nend\n\
             b(3)\n",
        );
        assert!(
            err.contains("allocator") && err.contains("context field"),
            "err: {err}"
        );
    }

    #[test]
    fn non_context_program_is_unchanged_in_shape() {
        // A program with no context features should not gain hidden params.
        let out = desugar_display("fn f(n::Int)\n    return n\nend\nf(1)\n");
        assert!(!out.contains("__ctx_"), "out: {out}");
    }
}
