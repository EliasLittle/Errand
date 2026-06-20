//use crate::frontend::lexer::TokenType;

use std::fmt;

#[derive(Debug, Clone)]
pub enum Expression {
    Int(i64),
    Float(f64),
    Boolean(bool),
    String(String),
    Symbol(String),
    Identifier {
        id: Id,
        type_expr: Option<TypeExpression>,
    },
    UnaryOp {
        operator: UnaryOperator,
        operand: Box<Expression>,
    },
    BinaryOp {
        operator: BinaryOperator,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    FunctionCall {
        // TODO: Change to expression to allow functions to return functions
        // e.g. should be able to call `my_fn()()` which currently fails
        id: Id,
        arguments: Vec<Expression>,
    },
    FunctionDefinition {
        id: Id,
        parameters: Vec<Parameter>,
        body: Box<Expression>,
        return_type_expr: Option<TypeExpression>,
        foreign: bool,
    },
    StructDefinition {
        id: Id,
        fields: Vec<FieldDefinition>,
        /// Type parameter names declared on the struct, e.g. `struct Foo<T>` → `["T"]`.
        type_params: Vec<Id>,
    },
    EnumDefinition {
        id: Id,
        variants: Vec<EnumVariant>,
        type_params: Vec<Id>,
    },
    /// A reference to a unit variant of an enum, e.g. `Direction::North`.
    /// Carries only symbolic names; the integer tag is resolved at codegen time.
    // TODO: Remove this, we should add all variants to the main symbol table
    EnumVariantAccess {
        enum_name: String,
        variant: String,
    },
    /// Construction of a data-carrying enum variant, e.g. `Message::Move(1, 2)`.
    /// Args are positional, matching the variant's field declarations in order.
    EnumVariantConstruct {
        enum_name: String,
        variant: String,
        args: Vec<Expression>,
    },
    If {
        condition: Box<Expression>,
        then_branch: Box<Expression>,
        else_branch: Option<Box<Expression>>,
        //elseif_branches: Box<If>
    },
    While {
        condition: Box<Expression>,
        body: Box<Expression>,
    },
    For {
        iterator: Id, // TODO?: Accept Vec<Id> for unpacking
        range: Box<Expression>,
        body: Box<Expression>,
    },
    Block(Vec<Box<Expression>>),
    Return(Option<Box<Expression>>),
    Print(Box<Expression>),
    Match {
        value: Box<Expression>,
        cases: Vec<MatchCase>,
    },
    /* This should just be a binary operation
    VariableAssignment {
        id: Id,
        value: Box<Expression>,
    },
    */
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Id {
    pub name: String,
}

/*pub struct Block {
    pub expressions: Vec<Expression>,
}*/

#[derive(Debug, Clone)]
pub struct Program {
    pub expressions: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Not,
    Negate,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Power,
    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    And,
    Ampersand,
    Or,
    Pipe,
    Dot,
    Assignment,
}

// Parameters can optionally have a type
#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub id: Id,
    pub type_expr: Option<TypeExpression>,
}

// Fields must have a type
#[derive(Debug, Clone, PartialEq)]
pub struct FieldDefinition {
    pub id: Id,
    pub field_type: TypeExpression,
}

/// A single variant of an enum.
///
/// - Unit variant:  `fields` is empty, `is_tuple` is false.
/// - Tuple variant: `fields` are auto-named `_0`, `_1`, …; `is_tuple` is true.
/// - Struct variant: `fields` have user-provided names; `is_tuple` is false.
///
/// NOTE: `is_tuple` is a special-case flag that exists only because the language
/// does not yet have a first-class tuple type.  When generic tuple (and array)
/// types are added to `TypeExpression`, tuple variants should be represented as
/// an ordinary single-field variant whose `field_type` is a `TypeExpression::Tuple`
/// (or similar), and `is_tuple` should be removed.
#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
    pub name: String,
    pub fields: Vec<FieldDefinition>,
    /// SPECIAL-CASE: true when the variant was written with `(T, U, …)` syntax.
    /// Remove once first-class tuple types exist — see note on `EnumVariant`.
    pub is_tuple: bool,
}

/// A pattern in a match arm.
#[derive(Debug, Clone)]
pub enum MatchPattern {
    /// `EnumName::Variant` or `EnumName::Variant(x, y)`.
    EnumVariant {
        enum_name: String,
        variant: String,
        /// Positional binding variable names; empty for unit variants.
        bindings: Vec<String>,
    },
    /// `_` — matches anything.
    Wildcard,
}

#[derive(Debug, Clone)]
pub struct MatchCase {
    pub pattern: MatchPattern,
    pub body: Box<Expression>,
}

/// A single generic argument. Const generics are not implemented yet (parser rejects them).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GenericArg {
    Type(TypeExpression),
}

/// `Base` plus type arguments as `Base<Arg1, Arg2, …>` (each argument uses `TypeExpression::name`).
pub fn mangle_type_name(base: &str, args: &[GenericArg]) -> String {
    if args.is_empty() {
        return base.to_string();
    }
    let mut s = base.to_string();
    s.push('<');
    for (i, arg) in args.iter().enumerate() {
        if i > 0 {
            s.push_str(", ");
        }
        match arg {
            GenericArg::Type(te) => s.push_str(&te.name()),
        }
    }
    s.push('>');
    s
}

/// TODO: Add `Tuple(Vec<TypeExpression>)` and `Array(Box<TypeExpression>)` variants
/// when those types are introduced to the language.  Once that is done, the
/// `is_tuple` special-case on `EnumVariant` and the ad-hoc `(T, U)` parsing
/// inside `Parser::enum_variants` can be removed in favour of ordinary type
/// expressions.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeExpression {
    Int,
    Int32,
    Float,
    Bool,
    String,
    Void,
    /// User-defined type: `id`, optional inner composite type list `{A,B}`, optional generic args `<Int>`.
    Struct(Id, Option<Vec<TypeExpression>>, Option<Vec<GenericArg>>),
}

impl TypeExpression {
    pub fn name(&self) -> String {
        match self {
            TypeExpression::Int => "Int".to_string(),
            TypeExpression::Int32 => "Int32".to_string(),
            TypeExpression::Float => "Float".to_string(),
            TypeExpression::Bool => "Bool".to_string(),
            TypeExpression::String => "String".to_string(),
            TypeExpression::Void => "Void".to_string(),
            TypeExpression::Struct(id, _, generic_args) => match generic_args {
                Some(args) if !args.is_empty() => mangle_type_name(&id.name, args),
                _ => id.name.clone(),
            },
        }
    }
}

/// Renders `struct Foo<T, U>` / `enum Bar<T>` style angle brackets, or `""` if empty.
fn fmt_type_params(type_params: &[Id]) -> String {
    if type_params.is_empty() {
        return String::new();
    }
    let names: Vec<String> = type_params.iter().map(|p| p.name.clone()).collect();
    format!("<{}>", names.join(", "))
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        display_expression(self, f)
    }
}

fn display_expression(e: &Expression, f: &mut fmt::Formatter) -> fmt::Result {
    match e {
        Expression::Int(n) => write!(f, "{}::Int", n),
        Expression::Float(n) => write!(f, "{}::Float", n),
        Expression::Boolean(b) => write!(f, "{}::Bool", b),
        Expression::String(s) => write!(f, "\"{}\"::String", s),
        Expression::Symbol(s) => write!(f, ":{}", s),
        Expression::Identifier { id, type_expr } => match type_expr {
            Some(type_expr) => write!(f, "{}::{}", id.name, type_expr.name()),
            None => write!(f, "{}::Any", id.name),
        },
        Expression::UnaryOp { operator, operand } => write!(f, "({:?} {})", operator, operand),
        Expression::BinaryOp {
            operator,
            left,
            right,
        } => write!(f, "({} {:?} {})", left, operator, right),
        Expression::FunctionCall { id, arguments } => {
            let args: Vec<String> = arguments.iter().map(|arg| format!("{}", arg)).collect();
            write!(f, "{}({})", id.name, args.join(", "))
        }
        Expression::FunctionDefinition {
            id,
            parameters,
            body,
            return_type_expr,
            foreign,
        } => fmt_function_definition(f, id, parameters, body, return_type_expr, *foreign),
        Expression::StructDefinition {
            id,
            fields,
            type_params,
        } => fmt_struct_definition(f, id, fields, type_params),
        Expression::EnumDefinition {
            id,
            variants,
            type_params,
        } => fmt_enum_definition(f, id, variants, type_params),
        Expression::EnumVariantAccess { enum_name, variant } => {
            write!(f, "{}::{}", enum_name, variant)
        }
        Expression::EnumVariantConstruct {
            enum_name,
            variant,
            args,
        } => {
            let args_str: Vec<String> = args.iter().map(|a| format!("{}", a)).collect();
            write!(f, "{}::{}({})", enum_name, variant, args_str.join(", "))
        }
        Expression::If {
            condition,
            then_branch,
            else_branch,
        } => fmt_if(f, condition, then_branch, else_branch),
        Expression::While { condition, body } => {
            write!(f, "while {} {{ {} }}", condition, body)
        }
        Expression::For {
            iterator,
            range,
            body,
        } => write!(f, "for {} in {} {{ {} }}", iterator.name, range, body),
        Expression::Block(expressions) => {
            let block_str: Vec<String> =
                expressions.iter().map(|expr| format!("{}", expr)).collect();
            write!(f, "{{ {} }}", block_str.join("; "))
        }
        Expression::Return(expr) => write!(
            f,
            "return {}",
            expr.as_ref().map_or("".to_string(), |e| format!("{}", e))
        ),
        Expression::Print(expr) => write!(f, "print({})", expr),
        Expression::Match { value, cases } => fmt_match(f, value, cases),
    }
}

fn fmt_function_definition(
    f: &mut fmt::Formatter,
    id: &Id,
    parameters: &[Parameter],
    body: &Expression,
    return_type_expr: &Option<TypeExpression>,
    foreign: bool,
) -> fmt::Result {
    let params: Vec<String> = parameters
        .iter()
        .map(|param| format!("{}", param))
        .collect();
    let body_str = format!("{}", body);
    let fn_kw = if foreign { "foreign fn" } else { "fn" };
    match return_type_expr {
        Some(return_type_expr) => write!(
            f,
            "{} {}({}) -> {} {{ {} }}",
            fn_kw,
            id.name,
            params.join(", "),
            return_type_expr.name(),
            body_str
        ),
        None => write!(
            f,
            "{} {}({}) {{ {} }}",
            fn_kw,
            id.name,
            params.join(", "),
            body_str
        ),
    }
}

fn fmt_struct_definition(
    f: &mut fmt::Formatter,
    id: &Id,
    fields: &[FieldDefinition],
    type_params: &[Id],
) -> fmt::Result {
    let fields_str: Vec<String> = fields
        .iter()
        .map(|field| format!("{}", field.id.name))
        .collect();
    let tp = fmt_type_params(type_params);
    write!(
        f,
        "struct {}{} {{ {} }}",
        id.name,
        tp,
        fields_str.join(", ")
    )
}

fn fmt_enum_variant_display(v: &EnumVariant) -> String {
    if v.fields.is_empty() {
        v.name.clone()
    } else if v.is_tuple {
        let types: Vec<String> = v.fields.iter().map(|f| f.field_type.name()).collect();
        format!("{}::({})", v.name, types.join(", "))
    } else {
        let fields: Vec<String> = v
            .fields
            .iter()
            .map(|f| format!("{}::{}", f.id.name, f.field_type.name()))
            .collect();
        format!("{}::{{ {} }}", v.name, fields.join(", "))
    }
}

fn fmt_enum_definition(
    f: &mut fmt::Formatter,
    id: &Id,
    variants: &[EnumVariant],
    type_params: &[Id],
) -> fmt::Result {
    let variants_str: Vec<String> = variants.iter().map(fmt_enum_variant_display).collect();
    let tp = fmt_type_params(type_params);
    write!(
        f,
        "enum {}{} {{ {} }}",
        id.name,
        tp,
        variants_str.join(", ")
    )
}

fn fmt_if(
    f: &mut fmt::Formatter,
    condition: &Expression,
    then_branch: &Expression,
    else_branch: &Option<Box<Expression>>,
) -> fmt::Result {
    let else_str = if let Some(else_branch) = else_branch {
        format!(" else {}", else_branch)
    } else {
        String::new()
    };
    write!(f, "if {} {{ {} }}{}", condition, then_branch, else_str)?;
    Ok(())
}

fn fmt_match(f: &mut fmt::Formatter, value: &Expression, cases: &[MatchCase]) -> fmt::Result {
    let cases_str: Vec<String> = cases
        .iter()
        .map(|case| {
            let pat = match &case.pattern {
                MatchPattern::Wildcard => "_".to_string(),
                MatchPattern::EnumVariant {
                    enum_name,
                    variant,
                    bindings,
                } => {
                    if bindings.is_empty() {
                        format!("{}::{}", enum_name, variant)
                    } else {
                        format!("{}::{}({})", enum_name, variant, bindings.join(", "))
                    }
                }
            };
            format!("{} => {}", pat, case.body)
        })
        .collect();
    write!(f, "match {} {{ {} }}", value, cases_str.join("; "))
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let expressions_str: Vec<String> = self
            .expressions
            .iter()
            .map(|expr| format!("{}", expr))
            .collect();
        write!(f, "{{ {} }}", expressions_str.join("; "))
    }
}

impl fmt::Display for Parameter {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.type_expr {
            Some(type_expr) => write!(f, "{}::{}", self.id.name, type_expr.name()),
            None => write!(f, "{}::Any", self.id.name),
        }
    }
}

impl fmt::Display for FieldDefinition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}::{}", self.id.name, self.field_type.name())
        //write!(f, "{}: {:?}", self.id.name, self.type_expr)
    }
}

impl fmt::Display for TypeExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeExpression::Int => write!(f, "Int"),
            TypeExpression::Int32 => write!(f, "Int32"),
            TypeExpression::Float => write!(f, "Float"),
            TypeExpression::Bool => write!(f, "Bool"),
            TypeExpression::String => write!(f, "String"),
            TypeExpression::Void => write!(f, "Void"),
            TypeExpression::Struct(id, _, args) => write!(
                f,
                "{}",
                mangle_type_name(&id.name, args.as_deref().unwrap_or(&[]))
            ),
        }
    }
}
