use std::collections::HashMap;
use crate::backend::worklist::ErrandType;

/// Add built-in data constructors to the type inference context
/// 
/// These are the primitive types and their constructors that Errand supports:
/// - Int, Int32, Float, Bool, String, Unit
/// - Built-in constructors like true/false for Bool
pub fn add_builtin_data_constructors() -> HashMap<String, ErrandType> {
    let mut data_constructors = HashMap::new();
    
    // Bool constructors
    data_constructors.insert("true".to_string(), ErrandType::Con("Bool".to_string()));
    data_constructors.insert("false".to_string(), ErrandType::Con("Bool".to_string()));
    
    // Unit constructor (for void/empty expressions)
    data_constructors.insert("unit".to_string(), ErrandType::Con("Unit".to_string()));
    
    data_constructors
}

/// Add built-in function types to the type inference context
/// 
/// These are functions that are available in the runtime environment:
/// - printf: String -> Unit (for output)
/// - malloc: Int -> Int (for memory allocation, returns pointer as int)
/// - free: Int -> Unit (for memory deallocation)
pub fn add_builtin_functions() -> HashMap<String, ErrandType> {
    let mut function_types = HashMap::new();
    
    // printf :: String -> a -> ... -> Unit (variadic)
    function_types.insert(
        "printf".to_string(),
        ErrandType::Arrow(
            Box::new(ErrandType::Con("String".to_string())),
            Box::new(ErrandType::Arrow(
                Box::new(ErrandType::Var("_a".to_string())),
                Box::new(ErrandType::Con("Unit".to_string())),
            )),
        ),
    );
    
    // malloc :: Int -> Int (returns pointer as int)
    function_types.insert(
        "malloc".to_string(),
        ErrandType::Arrow(
            Box::new(ErrandType::Con("Int".to_string())),
            Box::new(ErrandType::Con("Int".to_string())),
        ),
    );
    
    // free :: Int -> Unit
    function_types.insert(
        "free".to_string(),
        ErrandType::Arrow(
            Box::new(ErrandType::Con("Int".to_string())),
            Box::new(ErrandType::Con("Unit".to_string())),
        ),
    );

    // as_ptr :: String -> Int (convert string to raw pointer)
    function_types.insert(
        "as_ptr".to_string(),
        ErrandType::Arrow(
            Box::new(ErrandType::Con("String".to_string())),
            Box::new(ErrandType::Con("Int".to_string())),
        ),
    );

    // as_string :: Int -> String
    function_types.insert(
        "as_string".to_string(),
        ErrandType::Arrow(
            Box::new(ErrandType::Con("Int".to_string())),
            Box::new(ErrandType::Con("String".to_string())),
        ),
    );

    // getfield :: (struct_instance, field_symbol, struct_type) -> field_type
    // Struct instance can be any struct; symbols are String in the type system
    function_types.insert(
        "getfield".to_string(),
        ErrandType::Arrow(
            Box::new(ErrandType::Var("_struct".to_string())),
            Box::new(ErrandType::Arrow(
                Box::new(ErrandType::Con("String".to_string())),
                Box::new(ErrandType::Arrow(
                    Box::new(ErrandType::Con("String".to_string())),
                    Box::new(ErrandType::Con("Int".to_string())),
                )),
            )),
        ),
    );

    // new :: Symbol -> Field1 -> Field2 -> ... -> Struct
    // Polymorphic: first arg is type name (Symbol), then field values. Used by lowered
    // struct constructors like Point(x, y) -> return new(:Point, x, y).
    // 3 arrows for 2-field structs (Point); extend chain for larger structs.
    function_types.insert(
        "new".to_string(),
        ErrandType::Arrow(
            Box::new(ErrandType::Con("String".to_string())),
            Box::new(ErrandType::Arrow(
                Box::new(ErrandType::Var("_a".to_string())),
                Box::new(ErrandType::Arrow(
                    Box::new(ErrandType::Var("_b".to_string())),
                    Box::new(ErrandType::Var("_result".to_string())),
                )),
            )),
        ),
    );

    function_types
}

/// Convert from Errand's frontend TypeExpression to ErrandType for inference
pub fn type_expr_to_errand_type(type_expr: &crate::frontend::ast::TypeExpression) -> ErrandType {
    match type_expr {
        crate::frontend::ast::TypeExpression::Int => ErrandType::Con("Int".to_string()),
        crate::frontend::ast::TypeExpression::Int32 => ErrandType::Con("Int32".to_string()),
        crate::frontend::ast::TypeExpression::Float => ErrandType::Con("Float".to_string()),
        crate::frontend::ast::TypeExpression::Bool => ErrandType::Con("Bool".to_string()),
        crate::frontend::ast::TypeExpression::String => ErrandType::Con("String".to_string()),
        crate::frontend::ast::TypeExpression::Void => ErrandType::Con("Unit".to_string()),
        crate::frontend::ast::TypeExpression::Struct(id, _fields) => {
            // For now, treat structs as opaque types with their name
            ErrandType::Con(id.name.clone())
        }
    }
}
