use crate::backend::worklist::ErrandType;
use crate::frontend::ast::{GenericArg, TypeExpression};
use std::collections::HashMap;

/// Add built-in data constructors to the type inference context
///
/// These are the primitive types and their constructors that Errand supports:
/// - Int, Int32, Float, Bool, String, Unit
/// - Built-in constructors like true/false for Bool
pub fn add_builtin_data_constructors() -> HashMap<String, ErrandType> {
    const ENTRIES: &[(&str, &str)] = &[("true", "Bool"), ("false", "Bool"), ("unit", "Unit")];
    HashMap::from_iter(
        ENTRIES
            .iter()
            .map(|&(name, con)| (name.to_string(), ErrandType::Con(con.to_string()))),
    )
}

fn builtin_printf() -> ErrandType {
    ErrandType::Arrow(
        Box::new(ErrandType::Con("String".to_string())),
        Box::new(ErrandType::Arrow(
            Box::new(ErrandType::Var("_a".to_string())),
            Box::new(ErrandType::Con("Unit".to_string())),
        )),
    )
}

fn builtin_malloc() -> ErrandType {
    ErrandType::Arrow(
        Box::new(ErrandType::Con("Int".to_string())),
        Box::new(ErrandType::Con("Int".to_string())),
    )
}

fn builtin_free() -> ErrandType {
    ErrandType::Arrow(
        Box::new(ErrandType::Con("Int".to_string())),
        Box::new(ErrandType::Con("Unit".to_string())),
    )
}

fn builtin_as_ptr() -> ErrandType {
    ErrandType::Arrow(
        Box::new(ErrandType::Con("String".to_string())),
        Box::new(ErrandType::Con("Int".to_string())),
    )
}

fn builtin_as_string() -> ErrandType {
    ErrandType::Arrow(
        Box::new(ErrandType::Con("Int".to_string())),
        Box::new(ErrandType::Con("String".to_string())),
    )
}

fn builtin_strlen() -> ErrandType {
    ErrandType::Arrow(
        Box::new(ErrandType::Con("String".to_string())),
        Box::new(ErrandType::Con("Int".to_string())),
    )
}

fn builtin_strcpy() -> ErrandType {
    ErrandType::Arrow(
        Box::new(ErrandType::Con("String".to_string())),
        Box::new(ErrandType::Arrow(
            Box::new(ErrandType::Con("String".to_string())),
            Box::new(ErrandType::Con("Int".to_string())),
        )),
    )
}

fn builtin_getfield() -> ErrandType {
    ErrandType::Arrow(
        Box::new(ErrandType::Var("_struct".to_string())),
        Box::new(ErrandType::Arrow(
            Box::new(ErrandType::Con("String".to_string())),
            Box::new(ErrandType::Arrow(
                Box::new(ErrandType::Con("String".to_string())),
                Box::new(ErrandType::Con("Int".to_string())),
            )),
        )),
    )
}

fn builtin_new() -> ErrandType {
    ErrandType::Arrow(
        Box::new(ErrandType::Con("String".to_string())),
        Box::new(ErrandType::Arrow(
            Box::new(ErrandType::Var("_a".to_string())),
            Box::new(ErrandType::Arrow(
                Box::new(ErrandType::Var("_b".to_string())),
                Box::new(ErrandType::Var("_result".to_string())),
            )),
        )),
    )
}

/// Add built-in function types to the type inference context
///
/// These are functions that are available in the runtime environment:
/// - printf: String -> a -> ... -> Unit (variadic)
/// - malloc: Int -> Int (for memory allocation, returns pointer as int)
/// - free: Int -> Unit (for memory deallocation)
pub fn add_builtin_functions() -> HashMap<String, ErrandType> {
    const ENTRIES: &[(&str, fn() -> ErrandType)] = &[
        ("printf", builtin_printf),
        ("malloc", builtin_malloc),
        ("free", builtin_free),
        ("as_ptr", builtin_as_ptr),
        ("as_string", builtin_as_string),
        // Injected by `printf` lowering (see `frontend/lower.rs`) when computing the
        // size of the format string buffer to allocate. The Errand type system must
        // know about it, otherwise downstream analysis (e.g. annotated var decls
        // whose RHS contains the lowered call chain) will fail with UnboundVariable.
        ("strlen", builtin_strlen),
        // Also injected by `printf` lowering (`frontend/lower.rs`) to copy the format string into the
        // freshly malloc'd buffer.
        ("strcpy", builtin_strcpy),
        // Struct instance can be any struct; symbols are String in the type system
        ("getfield", builtin_getfield),
        // Polymorphic: first arg is type name (Symbol), then field values. Used by lowered
        // struct constructors like Point(x, y) -> return new(:Point, x, y).
        ("new", builtin_new),
    ];
    HashMap::from_iter(ENTRIES.iter().map(|&(name, mk)| (name.to_string(), mk())))
}

/// Convert from Errand's frontend TypeExpression to ErrandType for inference.
pub fn type_expr_to_errand_type(type_expr: &TypeExpression) -> ErrandType {
    type_expr_to_errand_type_with_params(type_expr, &[])
}

// TODO: This should return more than just ErrandType::Con
// Structs should be ErrandType::Product, Enums ErrandType::Sum
// Parametric types should be ErrandType::Forall
/// Like [`type_expr_to_errand_type`], but names in `type_params` map to [`ErrandType::Var`].
pub fn type_expr_to_errand_type_with_params(
    type_expr: &TypeExpression,
    type_params: &[String],
) -> ErrandType {
    match type_expr {
        TypeExpression::Int => ErrandType::Con("Int".to_string()),
        TypeExpression::Int32 => ErrandType::Con("Int32".to_string()),
        TypeExpression::Float => ErrandType::Con("Float".to_string()),
        TypeExpression::Bool => ErrandType::Con("Bool".to_string()),
        TypeExpression::String => ErrandType::Con("String".to_string()),
        TypeExpression::Void => ErrandType::Con("Unit".to_string()),
        TypeExpression::Struct(id, inner, generic_args) => {
            if type_params.iter().any(|p| p == &id.name) {
                return ErrandType::Var(id.name.clone());
            }
            if let Some(args) = generic_args {
                let arg_tys: Vec<ErrandType> = args
                    .iter()
                    .map(|ga| match ga {
                        GenericArg::Type(t) => type_expr_to_errand_type_with_params(t, type_params),
                    })
                    .collect();
                ErrandType::App(Box::new(ErrandType::Con(id.name.clone())), arg_tys)
            } else if inner.is_some() {
                ErrandType::Con(id.name.clone())
            } else {
                ErrandType::Con(id.name.clone())
            }
        }
    }
}
