//! Streaming recursive-descent parser over a lexer token stream.
//!
//! # Entry point
//!
//! - `Parser::new` primes the stream (first non-comment token becomes `bump`’s “current”).
//! - `Parser::parse` is the public driver: it repeatedly calls `parse_expression` until `at_end`,
//!   collecting top-level `Expression` nodes into a `Program`.
//!
//! # Where statements and expressions are parsed
//!
//! `parse_expression` is the main dispatcher for **both** top-level items and nested
//! statement/expression positions (e.g. inside `block`). It matches on the current
//! `TokenType`:
//!
//! - **Declaration / keyword statements** — delegates to `function`, `structure`,
//!   `enum_def`, `if_statement`, `while_statement`,
//!   `for_statement`, `return_statement`, `print_statement`,
//!   `match_expr`. These methods usually consume their own `end`/`newline` rules and may
//!   call `parse_expression` again for bodies, conditions, or operands (semi-recursive).
//! - **General expressions** — builds an atomic `primary` tree, then runs
//!   `parse_expr_bp` (Pratt / binding-power) for infix operators and chaining.
//!
//! Nested expression parsing reuses the same pipeline: `parse_full_expression` is
//! `primary` + `parse_expr_bp` (used for call arguments, `while` conditions, `return`/`print`
//! operands, etc.). `parenthesized_expression` parses `(` … `)` by parsing a primary and
//! then `parse_expr_bp` until `)`.
//!
//! # Section layout (matches inline `// --- … ---` headers in this file)
//!
//! | Section | Main methods |
//! |---------|--------------|
//! | Top-level | `parse` |
//! | Cursor | `bump`, `current_type`, `at`, `eat`, `expect`, `skip_newlines`, `parse_comma_separated_until`, `at_identifier` |
//! | Top-level statement / expression | `parse_expression` |
//! | Pratt parsing | `parse_expr_bp` |
//! | Infix operators | `apply_infix_operator`, `binary_operator_for_token`, `binary_op` |
//! | Primary expressions | `parse_full_expression`, `primary`, `inner_primary`, `parenthesized_expression`, prefix helpers |
//! | Symbols, identifiers & types | `identifier`, `symbol`, `id`, `type_expr`, generics (`generic_type_args`, `type_param_names`, `parse_optional_type_params`) |
//! | Literals | `literal` |
//! | Definitions | `function`, `structure`, `enum_def`, `enum_variants` |
//! | Delimited lists | `parameters`, `arguments`, `type_list`, `inline_field_list`, … |
//! | Control flow | `block`, `if_statement`, `match_expr`, `match_pattern`, `while_statement`, `for_statement`, `return_statement`, `print_statement` |

use super::ast::{
    BinaryOperator, EnumVariant, Expression, FieldDefinition, GenericArg, Id, MatchCase,
    MatchPattern, Parameter, Program, TypeExpression, UnaryOperator,
};
use super::lexer::{token_type, Token, TokenType};
use std::iter::Peekable;
use std::vec::IntoIter;

// --- Cursor / token stream ---

/// A streaming parser that uses pattern matching and Pratt parsing for expressions
pub struct Parser {
    tokens: Peekable<IntoIter<Token>>,
    /// Current token being processed
    current: Option<Token>,
    /// Any errors encountered during parsing
    errors: Vec<String>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        let mut parser = Parser {
            tokens: tokens.into_iter().peekable(),
            current: None,
            errors: Vec::new(),
        };
        parser.bump(); // Load first token
        parser
    }

    // --- Top-level ---

    /// Core parsing function that returns a Program AST
    pub fn parse(&mut self) -> Result<Program, Vec<String>> {
        let _span = tracing::debug_span!(target: "parser", "parser.parse_program").entered();
        let mut expressions = Vec::new();

        while !self.at_end() {
            match self.parse_expression() {
                Ok(expr) => expressions.push(expr),
                Err(e) => {
                    tracing::trace!(target: "parser", error = %e, "top-level statement parse error");
                    self.errors.push(e);
                    // Must advance the stream or we spin forever on the same token.
                    if self.bump().is_none() {
                        break;
                    }
                }
            }
        }

        if self.errors.is_empty() {
            tracing::debug!(
                target: "parser",
                top_level = expressions.len(),
                "parse succeeded"
            );
            Ok(Program { expressions })
        } else {
            tracing::debug!(
                target: "parser",
                errors = self.errors.len(),
                "parse failed"
            );
            Err(self.errors.clone())
        }
    }

    // --- Cursor ---

    /// Advance to next token and return previous, skipping comment tokens
    fn bump(&mut self) -> Option<Token> {
        let previous = self.current.take();
        while let Some(next) = self.tokens.next() {
            if !next.is_comment() {
                self.current = Some(next);
                break;
            }
        }
        previous
    }

    /// Look at current token without consuming
    fn current(&self) -> Result<&Token, String> {
        self.current.as_ref().ok_or("No current token".to_string())
    }

    fn current_type(&self) -> Option<&TokenType> {
        self.current.as_ref().map(|t| &t.token_type)
    }

    /// Check if we're at a specific token type
    fn at(&self, token_type: &TokenType) -> bool {
        match &self.current {
            Some(current_token) => current_token.var() == token_type.var(),
            None => false, // Return false if there is no current token
        }
    }

    /// Check if we're at the end of input
    fn at_end(&self) -> bool {
        self.current.is_none() || self.at(&TokenType::EOF)
    }

    /// True if the current token is any identifier (including `_`).
    fn at_identifier(&self) -> bool {
        matches!(self.current_type(), Some(TokenType::Identifier(_)))
    }

    /// Consume current token if it matches expected type
    /// Returns true if the token was consumed, false otherwise
    /// Used for tokens like parens, commas, etc.
    fn eat(&mut self, token_type: &TokenType) -> bool {
        if self.at(token_type) {
            self.bump();
            true
        } else {
            false
        }
    }

    /// Expect a specific token type or error
    /// Returns the token if it matches the expected type, otherwise returns an error
    /// Used for tokens like identifiers, keywords, etc.
    fn expect(&mut self, token_type: &TokenType) -> Result<Token, String> {
        tracing::trace!(target: "parser","Expecting {:?}", token_type);
        if let Some(token) = self.bump() {
            tracing::trace!(target: "parser","Expecting| actual token:{:?}", token);
            if token.var() == token_type.var() {
                Ok(token)
            } else {
                Err(format!(
                    "Expected {:?}, found {:?}",
                    token_type, token.token_type
                ))
            }
        } else {
            tracing::trace!(target: "parser","Unexpected end of input");
            Err("Unexpected end of input".to_string())
        }
    }

    fn skip_newlines(&mut self) {
        while self.eat(&TokenType::Newline) {}
    }

    /// Parses zero or more comma-separated items until `close` is the current token (`close` is not consumed).
    fn parse_comma_separated_until<T>(
        &mut self,
        close: &TokenType,
        expected_sep_msg: &'static str,
        mut parse_elem: impl FnMut(&mut Self) -> Result<T, String>,
    ) -> Result<Vec<T>, String> {
        let mut out = Vec::new();
        while !self.at(close) {
            out.push(parse_elem(self)?);
            if !(self.eat(&TokenType::Comma) ^ self.at(close)) {
                return Err(expected_sep_msg.to_string());
            }
        }
        Ok(out)
    }

    // --- Top-level statement / expression ---

    /// Parse expressions (functions, structs, if statements, for statements, return statements)
    /// TODO: Change to parse_statement()
    pub fn parse_expression(&mut self) -> Result<Expression, String> {
        tracing::trace!(target: "parser","Parsing expression| current:{:?}", self.current_type());
        let result = match self.current_type() {
            Some(TokenType::Foreign) => self.function(),
            Some(TokenType::Function) => self.function(),
            Some(TokenType::Struct) => self.structure(),
            Some(TokenType::Enum) => self.enum_def(),
            Some(TokenType::If) => self.if_statement(), // TODO: Move to parse_expr_bp
            Some(TokenType::While) => self.while_statement(),
            Some(TokenType::For) => self.for_statement(),
            Some(TokenType::Return) => self.return_statement(),
            Some(TokenType::Match) => self.match_expr(),
            Some(TokenType::Newline) => {
                self.bump();
                self.parse_expression()
            }
            Some(TokenType::Print) => self.print_statement(),
            Some(TokenType::End) => {
                // Skip End tokens at the top level - they should be handled by their respective constructs
                self.bump();
                Err("Unexpected End token at top level".to_string())
            }
            Some(_) => {
                let primary_expr = self.primary()?; // Store the result of primary
                self.parse_expr_bp(primary_expr, 0) // Pratt / binding-power parser
            }
            None => Err("No current token".to_string()),
        };
        tracing::trace!(target: "parser","Parsing expression| result:{:?}", result);
        self.skip_newlines();
        result
    }

    // --- Pratt parsing (binding power) ---

    /// Bottom-up operator-precedence parsing of expressions.
    /// Operators, literals, and calls (including parenthesized expressions).
    fn parse_expr_bp(
        &mut self,
        lhs: Expression,
        min_precedence: i32,
    ) -> Result<Expression, String> {
        tracing::trace!(
            target: "parser",
            lhs = ?lhs,
            current = ?self.current_type(),
            "parse_expr_bp"
        );
        let mut level_lhs = lhs;
        while let Some(current) = self.current_type() {
            if !current.is_infix() {
                break;
            }
            if !(current.precedence()? >= min_precedence) {
                tracing::trace!(target: "parser",
                    "parse_expr_bp| precedence:{:?} < {:?}",
                    current.precedence(),
                    min_precedence
                );
                break; // Exit the loop if precedence is not greater
            }
            let op = self.bump().ok_or("No operator found".to_string())?;
            let op_precedence = op.precedence()?;
            tracing::trace!(target: "parser",
                "parse_expr_bp| op:{:?}, op_precedence:{:?}",
                op,
                op_precedence
            );
            let mut rhs = self.primary()?;
            tracing::trace!(target: "parser","parse_expr_bp| rhs:{:?}", rhs);

            // Handle operator precedence and associativity
            while let Some(next) = self.current_type() {
                if !next.is_infix() {
                    // No infix operator found, break
                    break;
                }
                tracing::trace!(target: "parser","parse_expr_bp| next op:{:?}", next);
                let precedence = next.precedence()?;
                if precedence > op_precedence {
                    tracing::trace!(target: "parser",
                        "parse_expr_bp| left associative precedence:{:?}",
                        op_precedence + 1
                    );
                    rhs = self.parse_expr_bp(rhs.clone(), op_precedence + 1)?;
                } else if precedence == op_precedence && next.is_right_associative() {
                    tracing::trace!(target: "parser",
                        "parse_expr_bp| right associative precedence:{:?}",
                        op_precedence
                    );
                    rhs = self.parse_expr_bp(rhs.clone(), op_precedence)?;
                } else {
                    break; // Exit the loop if precedence is not greater
                }
            }

            tracing::trace!(target: "parser",
                "Applying infix operator: {:?} {:?} {:?}",
                level_lhs,
                op.token_type,
                rhs
            );
            level_lhs = self.apply_infix_operator(level_lhs.clone(), &op, rhs.clone())?;
        }

        // Don't consume newlines here - let the caller handle it
        Ok(level_lhs)
    }

    // --- Infix operators ---

    fn binary_op(lhs: Expression, operator: BinaryOperator, rhs: Expression) -> Expression {
        Expression::BinaryOp {
            operator,
            left: Box::new(lhs),
            right: Box::new(rhs),
        }
    }

    /// Maps infix token types to `BinaryOperator`, excluding `=` (handled separately).
    fn binary_operator_for_token(tt: &TokenType) -> Option<BinaryOperator> {
        match tt {
            TokenType::Dot => Some(BinaryOperator::Dot),
            TokenType::Plus => Some(BinaryOperator::Add),
            TokenType::Minus => Some(BinaryOperator::Subtract),
            TokenType::Asterisk => Some(BinaryOperator::Multiply),
            TokenType::Slash => Some(BinaryOperator::Divide),
            TokenType::Modulo => Some(BinaryOperator::Modulo),
            TokenType::Carrot => Some(BinaryOperator::Power),
            TokenType::Equal => Some(BinaryOperator::Equal),
            TokenType::NotEqual => Some(BinaryOperator::NotEqual),
            TokenType::LessThan => Some(BinaryOperator::LessThan),
            TokenType::GreaterThan => Some(BinaryOperator::GreaterThan),
            TokenType::LessThanEqual => Some(BinaryOperator::LessThanEqual),
            TokenType::GreaterThanEqual => Some(BinaryOperator::GreaterThanEqual),
            TokenType::And => Some(BinaryOperator::And),
            TokenType::Ampersand => Some(BinaryOperator::Ampersand),
            TokenType::Or => Some(BinaryOperator::Or),
            TokenType::Pipe => Some(BinaryOperator::Pipe),
            _ => None,
        }
    }

    fn apply_infix_operator(
        &mut self,
        lhs: Expression,
        op: &Token,
        rhs: Expression,
    ) -> Result<Expression, String> {
        if let Some(bo) = Self::binary_operator_for_token(&op.token_type) {
            return Ok(Self::binary_op(lhs, bo, rhs));
        }
        match &op.token_type {
            // TODO: Do we need to move assignment out of infix? It could be called in places it shouldn't be
            // e.g. my_fn(x = 1, y = 2). This is only valid if x and y are parameters.
            TokenType::Assignment => match &lhs {
                Expression::Identifier { .. } => {
                    Ok(Self::binary_op(lhs, BinaryOperator::Assignment, rhs))
                }
                _ => Err(format!("Cannot assign to {:?}", lhs)),
            },
            _ => Err(format!("Unsupported operator: {:?}", op.token_type)),
        }
    }

    // --- Primary expressions ---

    /// `primary()` followed by infix operators until precedence stops extension.
    fn parse_full_expression(&mut self) -> Result<Expression, String> {
        let primary_expr = self.primary()?;
        self.parse_expr_bp(primary_expr, 0)
    }

    /// prefix -> call -> literal
    /// primary ::= '-' inner_primary | '!' inner_primary
    fn primary(&mut self) -> Result<Expression, String> {
        tracing::trace!(target: "parser","Parsing primary| current:{:?}", self.current_type());
        let prefix_op = self.check_prefix_operator();

        let inner_primary = self.inner_primary()?;

        if let Some(op) = prefix_op {
            Ok(Expression::UnaryOp {
                operator: op,
                operand: Box::new(inner_primary),
            })
        } else {
            Ok(inner_primary)
        }
    }

    /// inner_primary ::= call | literal
    fn inner_primary(&mut self) -> Result<Expression, String> {
        tracing::trace!(target: "parser","Parsing inner primary| current:{:?}", self.current_type());
        match self.current_type() {
            Some(TokenType::Colon) => self.symbol(),
            Some(TokenType::LParen) => self.parenthesized_expression(),
            Some(TokenType::Int(n)) => self.literal(&TokenType::Int(*n)),
            Some(TokenType::Float(n)) => self.literal(&TokenType::Float(*n)),
            Some(TokenType::StringLiteral(s)) => {
                self.literal(&TokenType::StringLiteral(s.to_string()))
            }
            Some(TokenType::True) => self.literal(&TokenType::True),
            Some(TokenType::False) => self.literal(&TokenType::False),
            Some(TokenType::Match) => self.match_expr(),
            Some(TokenType::Identifier(_)) => self.identifier(),
            _ => Err(format!("Unexpected token: {:?}", self.current_type())),
        }
    }

    fn check_prefix_operator(&mut self) -> Option<UnaryOperator> {
        tracing::trace!(target: "parser",
            "Checking prefix operator| current:{:?}",
            self.current_type()
        );
        match self.current_type() {
            Some(TokenType::Minus) => {
                self.bump()?;
                Some(UnaryOperator::Negate)
            }
            Some(TokenType::Bang) => {
                self.bump()?;
                Some(UnaryOperator::Not)
            }
            _ => None,
        }
    }

    /// Parse parenthesized expressions
    /// Does not allow
    fn parenthesized_expression(&mut self) -> Result<Expression, String> {
        tracing::trace!(target: "parser",
            "Parsing parenthesized expression| current:{:?}",
            self.current_type()
        );
        self.expect(&TokenType::LParen)?;
        let primary_expr = self.primary()?; // Store the result of primary
        tracing::trace!(target: "parser","Parenthesized | primary expression: {:?}", primary_expr);
        let expression = self.parse_expr_bp(primary_expr, 0)?;
        self.expect(&TokenType::RParen)?;
        Ok(expression)
    }

    /// identifier ::= IDENTIFIER(::Type) | IDENTIFIER '(' parameters ')' (call) | IDENTIFIER.IDENTIFIER (field access)
    /// Also handles enum variant construction: `EnumName::Variant(args)`
    fn identifier(&mut self) -> Result<Expression, String> {
        tracing::trace!(target: "parser","Parsing identifier| current:{:?}", self.current_type());
        let id = self.id()?;

        if self.eat(&TokenType::TypeDef) {
            // After `::`, the next token must be an identifier (variant name or type name).
            let next_id = self.id()?;

            if self.eat(&TokenType::LParen) {
                // `Name::Variant(args)` — enum variant construction with positional args.
                tracing::trace!(target: "parser","Enum variant construction");
                let args = self.arguments()?;
                self.expect(&TokenType::RParen)?;
                Ok(Expression::EnumVariantConstruct {
                    enum_name: id.name,
                    variant: next_id.name,
                    args,
                })
            } else {
                // `Name::Type` or `Name::Foo<Int>` — type annotation.
                tracing::trace!(target: "parser","Identifier with type annotation");
                let gen = if self.eat(&TokenType::LessThan) {
                    Some(self.generic_type_args()?)
                } else {
                    None
                };
                Ok(Expression::Identifier {
                    id,
                    type_expr: Some(TypeExpression::Struct(next_id, None, gen)),
                })
            }
        } else if self.eat(&TokenType::LParen) {
            // `Name(args)` - function call with arguments
            tracing::trace!(target: "parser","Function call");
            let parameters = self.arguments()?;
            self.expect(&TokenType::RParen)?;
            Ok(Expression::FunctionCall {
                id,
                arguments: parameters,
            })
        } else {
            tracing::trace!(target: "parser","Identifier");
            Ok(Expression::Identifier {
                id,
                type_expr: None,
            })
        }
    }

    // --- Symbols, identifiers & types ---

    fn symbol(&mut self) -> Result<Expression, String> {
        self.expect(&TokenType::Colon)?;
        if let Some(TokenType::Identifier(s)) = self.current_type().cloned() {
            self.bump();
            Ok(Expression::Symbol(s))
        } else {
            Err("Expected identifier after ':' for symbol".to_string())
        }
    }

    fn id(&mut self) -> Result<Id, String> {
        match self.expect(&token_type("Identifier")?)?.token_type {
            TokenType::Identifier(id_str) => Ok(Id { name: id_str }),
            _ => {
                tracing::trace!(target: "parser","Expected identifier! found: {:?}", self.current_type());
                Err("Expected identifier".to_string())
            }
        }
    }

    fn type_expr(&mut self) -> Result<TypeExpression, String> {
        tracing::trace!(target: "parser","Parsing type expression| current:{:?}", self.current_type());

        let id = self.id()?; // Parse the base identifier
        if self.eat(&TokenType::LBrace) {
            let types = self.parse_comma_separated_until(
                &TokenType::RBrace,
                "Expected comma or right brace",
                |p| p.type_expr(),
            )?;
            self.expect(&TokenType::RBrace)?;
            Ok(TypeExpression::Struct(id, Some(types), None))
        } else {
            match id.name.as_str() {
                "Int" => Ok(TypeExpression::Int),
                "Float" => Ok(TypeExpression::Float),
                "Bool" => Ok(TypeExpression::Bool),
                "Void" => Ok(TypeExpression::Void),
                _ => {
                    let gen = if self.eat(&TokenType::LessThan) {
                        Some(self.generic_type_args()?)
                    } else {
                        None
                    };
                    Ok(TypeExpression::Struct(id, None, gen))
                }
            }
        }
    }

    /// Comma-separated type arguments inside `< ... >` (opening `<` already consumed).
    fn generic_type_args(&mut self) -> Result<Vec<GenericArg>, String> {
        let args = self.parse_comma_separated_until(
            &TokenType::GreaterThan,
            "Expected comma or '>' in generic type argument list",
            |p| {
                let te = p.type_expr()?;
                Ok(GenericArg::Type(te))
            },
        )?;
        self.expect(&TokenType::GreaterThan)?;
        Ok(args)
    }

    /// Type parameter names on `struct Foo<T, U>` / `enum Bar<T>`.
    fn type_param_names(&mut self) -> Result<Vec<Id>, String> {
        let names = self.parse_comma_separated_until(
            &TokenType::GreaterThan,
            "Expected comma or '>' in type parameter list",
            |p| p.id(),
        )?;
        self.expect(&TokenType::GreaterThan)?;
        Ok(names)
    }

    /// Optional `<T, U, …>` after a struct or enum name.
    fn parse_optional_type_params(&mut self) -> Result<Vec<Id>, String> {
        if self.eat(&TokenType::LessThan) {
            self.type_param_names()
        } else {
            Ok(vec![])
        }
    }

    // --- Literals ---

    /// Parse literals (String, int, float, boolean)
    fn literal(&mut self, token_type: &TokenType) -> Result<Expression, String> {
        tracing::trace!(target: "parser",
            "Parsing literal| current:{:?} expected:{:?}",
            self.current_type(),
            token_type
        );
        let literal = self.expect(token_type)?;
        match literal.token_type {
            TokenType::StringLiteral(s) => Ok(Expression::String(s)),
            TokenType::Int(n) => Ok(Expression::Int(n)),
            TokenType::Float(n) => Ok(Expression::Float(n)),
            TokenType::True => Ok(Expression::Boolean(true)),
            TokenType::False => Ok(Expression::Boolean(false)),
            _ => Err(format!("Unexpected token: {:?}", token_type)),
        }
    }

    // --- Definitions (fn, struct, enum) ---

    /// Parse function definitions
    /// Currently only supports standard block function definitions
    fn function(&mut self) -> Result<Expression, String> {
        tracing::trace!(target: "parser","Parsing function| current:{:?}", self.current_type());
        let mut foreign = false;
        if self.eat(&TokenType::Foreign) {
            foreign = true;
        }
        self.expect(&TokenType::Function)?;
        let id = self.id()?;
        self.expect(&TokenType::LParen)?;
        let arguments = self.parameters()?;
        self.expect(&TokenType::RParen)?;
        let return_type_expr = if self.eat(&TokenType::TypeDef) {
            Some(self.type_expr()?)
        } else {
            None
        };
        if foreign {
            // Foreign function: no body, no 'end', just a dummy body (e.g. Block([]))
            Ok(Expression::FunctionDefinition {
                id,
                parameters: arguments,
                body: Box::new(Expression::Block(vec![])),
                return_type_expr,
                foreign: true,
            })
        } else {
            self.expect(&TokenType::Newline)?; // TODO: Change to or '=' to support inline functions
            let body = self.block()?;
            self.expect(&TokenType::End)?;
            tracing::trace!(target: "parser","Parsing function| End of function");
            let _ = self.expect(&TokenType::Newline); // Should these be eats or expects?
            Ok(Expression::FunctionDefinition {
                id,
                parameters: arguments,
                body: Box::new(body),
                return_type_expr,
                foreign: false,
            })
        }
    }

    // struct <identifier>(<generic_params>)
    //     <field>
    //     <field>
    //     ...
    // end
    /// Parse struct definitions
    fn structure(&mut self) -> Result<Expression, String> {
        tracing::trace!(target: "parser","Parsing structure| current:{:?}", self.current_type());
        self.expect(&TokenType::Struct)?;
        let id = self.id()?;
        let type_params = self.parse_optional_type_params()?;
        self.expect(&TokenType::Newline)?;
        let fields = self.field_parameters()?;
        self.expect(&TokenType::End)?;
        Ok(Expression::StructDefinition {
            id,
            fields,
            type_params,
        })
    }

    // enum <identifier>
    //     <Variant>
    //     <Variant>
    //     ...
    // end
    /// Parse enum definitions.
    /// Each variant is a bare identifier on its own line.
    fn enum_def(&mut self) -> Result<Expression, String> {
        tracing::trace!(target: "parser","Parsing enum| current:{:?}", self.current_type());
        self.expect(&TokenType::Enum)?;
        let id = self.id()?;
        let type_params = self.parse_optional_type_params()?;
        self.expect(&TokenType::Newline)?;
        let variants = self.enum_variants()?;
        self.expect(&TokenType::End)?;
        Ok(Expression::EnumDefinition {
            id,
            variants,
            type_params,
        })
    }

    /// Parse enum variant names until `end`.
    ///
    /// Supported forms (using `::` type-annotation notation):
    ///   `Quit`                    → unit variant
    ///   `Move::(Int, Int)`        → tuple variant; fields auto-named _0, _1, …
    ///   `Write::String`           → single-type variant; field auto-named _0
    ///   `Point::{x::Int, y::Int}` → struct variant with named fields
    fn enum_variants(&mut self) -> Result<Vec<EnumVariant>, String> {
        tracing::trace!(target: "parser","Parsing enum variants| current:{:?}", self.current_type());
        let mut variants = Vec::new();
        while self.at_identifier() {
            let id = self.id()?;

            let (fields, is_tuple) = if self.eat(&TokenType::TypeDef) {
                if self.eat(&TokenType::LParen) {
                    // Tuple variant: Move::(Int, Int)
                    // SPECIAL-CASE: `(T, U, …)` is parsed here as an ad-hoc
                    // comma-separated type list because the language has no
                    // first-class tuple type yet.  When `TypeExpression::Tuple`
                    // is added, this branch should be replaced by a normal
                    // `type_expr()` call and `is_tuple` should be removed.
                    let types = self.type_list()?;
                    self.expect(&TokenType::RParen)?;
                    let fields = types
                        .into_iter()
                        .enumerate()
                        .map(|(i, t)| FieldDefinition {
                            id: Id {
                                name: format!("_{}", i),
                            },
                            field_type: t,
                        })
                        .collect();
                    (fields, true)
                } else if self.eat(&TokenType::LBrace) {
                    // Struct variant: Point::{x::Int, y::Int}
                    // Reuse existing field_parameters but without the newline terminator.
                    let fields = self.inline_field_list()?;
                    self.expect(&TokenType::RBrace)?;
                    (fields, false)
                } else {
                    // Single-type variant: Write::String
                    let t = self.type_expr()?;
                    let fields = vec![FieldDefinition {
                        id: Id {
                            name: "_0".to_string(),
                        },
                        field_type: t,
                    }];
                    (fields, true)
                }
            } else {
                // Unit variant
                (vec![], false)
            };

            self.expect(&TokenType::Newline)?;
            variants.push(EnumVariant {
                name: id.name,
                fields,
                is_tuple,
            });
        }
        Ok(variants)
    }

    /// Parse a comma-separated list of `TypeExpression`s up to `)`.
    fn type_list(&mut self) -> Result<Vec<TypeExpression>, String> {
        self.parse_comma_separated_until(
            &TokenType::RParen,
            "Expected comma or right parenthesis in type list",
            |p| p.type_expr(),
        )
    }

    /// Parse comma-separated `name::Type` field pairs inside `{ … }`.
    /// Does NOT consume the closing `}`.
    fn inline_field_list(&mut self) -> Result<Vec<FieldDefinition>, String> {
        self.parse_comma_separated_until(
            &TokenType::RBrace,
            "Expected comma or right brace in struct variant field list",
            |p| {
                let id = p.id()?;
                p.expect(&TokenType::TypeDef)?;
                let field_type = p.type_expr()?;
                Ok(FieldDefinition { id, field_type })
            },
        )
    }

    /// Parse expressions until 'END' token
    fn block(&mut self) -> Result<Expression, String> {
        tracing::trace!(target: "parser","Parsing block| Starting block: {:?}", self.current_type());
        let mut expressions = Vec::new();
        while !self.at(&TokenType::End) {
            expressions.push(Box::new(self.parse_expression()?));
        }
        tracing::trace!(target: "parser","Parsing block| End of block");
        Ok(Expression::Block(expressions))
    }

    // --- Delimited lists (comma-separated) ---

    /// Parse expressions until 'RParen' token
    fn parameters(&mut self) -> Result<Vec<Parameter>, String> {
        tracing::trace!(target: "parser","Parsing parameters| current:{:?}", self.current_type());
        self.parse_comma_separated_until(
            &TokenType::RParen,
            "Expected comma or right parenthesis",
            |p| {
                let id = p.id()?;
                let parameter = if p.eat(&TokenType::TypeDef) {
                    let type_expr = p.type_expr()?;
                    Parameter {
                        id,
                        type_expr: Some(type_expr),
                    }
                } else {
                    Parameter {
                        id,
                        type_expr: None,
                    }
                };
                Ok(parameter)
            },
        )
    }

    /// Parse field definitions
    /// id::type
    fn field_parameters(&mut self) -> Result<Vec<FieldDefinition>, String> {
        tracing::trace!(target: "parser",
            "Parsing field parameters| current:{:?}",
            self.current_type()
        );
        let mut fields = Vec::new();
        while self.at_identifier() {
            let id = self.id()?;
            self.expect(&TokenType::TypeDef)?;
            let field_type = self.type_expr()?; // Required here
            self.expect(&TokenType::Newline)?;
            let field = FieldDefinition { id, field_type };
            fields.push(field);
        }
        Ok(fields)
    }

    /// Parse arguments of a function call
    fn arguments(&mut self) -> Result<Vec<Expression>, String> {
        tracing::trace!(target: "parser","Parsing arguments| current:{:?}", self.current_type());
        self.parse_comma_separated_until(
            &TokenType::RParen,
            "Expected comma or right parenthesis",
            |p| p.parse_full_expression(),
        )
    }

    // --- Control flow ---

    /// Parse if-elseif-else-end expressions
    /// TODO: Support both block and inline if statements
    /// inline should be within parse_expr_bp() and blocks should be within parse_expression()
    fn if_statement(&mut self) -> Result<Expression, String> {
        tracing::trace!(target: "parser","Parsing if statement| current:{:?}", self.current_type());
        self.expect(&TokenType::If)?;
        let condition = self.parse_expression()?;
        self.eat(&TokenType::Newline); // Keep as eat to support inline
        let then_branch = self.block()?;

        //let mut elseif_branches = Vec::new();
        let mut else_branch = None;

        /*
        while self.at(&TokenType::ElseIf) {
            self.expect(&TokenType::ElseIf)?;
            let elseif_condition = self.parse_expression()?;
            self.eat(&TokenType::Newline)?;
            let elseif_branch = self.block()?;
            elseif_branches.push((elseif_condition, elseif_branch));
        }*/

        // Check for else
        if self.eat(&TokenType::Else) {
            else_branch = Some(Box::new(self.block()?));
        }

        self.expect(&TokenType::End)?;
        Ok(Expression::If {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: else_branch,
        })
    }

    /// Parse a `match` expression:
    /// ```text
    /// match <expr>
    ///     Pattern => <expr>
    ///     ...
    /// end
    /// ```
    fn match_expr(&mut self) -> Result<Expression, String> {
        tracing::trace!(target: "parser",
            "Parsing match expression| current:{:?}",
            self.current_type()
        );
        self.expect(&TokenType::Match)?;
        let value = self.parse_expression()?;
        // parse_expression already consumes a newline; eat it if still present
        self.eat(&TokenType::Newline);
        let mut cases = Vec::new();
        while !self.at(&TokenType::End) && !self.at_end() {
            let pattern = self.match_pattern()?;
            self.expect(&TokenType::Arrow)?;
            let body = Box::new(self.parse_expression()?);
            self.eat(&TokenType::Newline);
            cases.push(MatchCase { pattern, body });
        }
        self.expect(&TokenType::End)?;
        Ok(Expression::Match {
            value: Box::new(value),
            cases,
        })
    }

    /// Parse a single match arm pattern:
    /// - `_`                          → Wildcard
    /// - `EnumName::Variant`          → EnumVariant with no bindings
    /// - `EnumName::Variant(x, y)`    → EnumVariant with positional bindings
    fn match_pattern(&mut self) -> Result<MatchPattern, String> {
        tracing::trace!(target: "parser","Parsing match pattern| current:{:?}", self.current_type());
        // Wildcard
        if let Some(TokenType::Identifier(name)) = self.current_type() {
            if name == "_" {
                self.bump();
                return Ok(MatchPattern::Wildcard);
            }
        }
        // EnumName::Variant or EnumName::Variant(bindings)
        let enum_id = self.id()?;
        self.expect(&TokenType::TypeDef)?; // ::
        let variant_id = self.id()?;
        let bindings = if self.eat(&TokenType::LParen) {
            let names = self.parse_comma_separated_until(
                &TokenType::RParen,
                "Expected comma or ')' in match pattern bindings",
                |p| p.id().map(|id| id.name),
            )?;
            self.expect(&TokenType::RParen)?;
            names
        } else {
            vec![]
        };
        Ok(MatchPattern::EnumVariant {
            enum_name: enum_id.name,
            variant: variant_id.name,
            bindings,
        })
    }

    fn while_statement(&mut self) -> Result<Expression, String> {
        tracing::trace!(target: "parser","Parsing while statement| current:{:?}", self.current_type());
        self.expect(&TokenType::While)?;
        let condition = self.parse_full_expression()?;
        tracing::trace!(target: "parser","Parsing while statement| condition:{:?}", condition);
        self.eat(&TokenType::Newline); // parse_expr_bp leaves newline for the caller
        let body = self.block()?;
        self.eat(&TokenType::Newline);
        self.expect(&TokenType::End)?;
        tracing::trace!(target: "parser","Parsing while statement| body:{:?}", body);
        Ok(Expression::While {
            condition: Box::new(condition),
            body: Box::new(body),
        })
    }

    fn for_statement(&mut self) -> Result<Expression, String> {
        tracing::trace!(target: "parser","Parsing for statement| current:{:?}", self.current_type());
        self.expect(&TokenType::For)?;
        let iterator = self.id()?;
        self.expect(&TokenType::In)?;
        let range = self.parse_expression()?;
        self.expect(&TokenType::Newline)?;
        let body = self.block()?;
        self.expect(&TokenType::End)?;
        Ok(Expression::For {
            iterator,
            range: Box::new(range),
            body: Box::new(body),
        })
    }

    fn return_statement(&mut self) -> Result<Expression, String> {
        tracing::trace!(target: "parser",
            "Parsing return statement| current:{:?}",
            self.current_type()
        );
        self.expect(&TokenType::Return)?;
        let value = self.parse_full_expression()?;
        self.eat(&TokenType::Newline);
        tracing::trace!(target: "parser","Parsing return statement| end of return");
        Ok(Expression::Return(Some(Box::new(value))))
    }

    fn print_statement(&mut self) -> Result<Expression, String> {
        tracing::trace!(target: "parser","Parsing print statement| current:{:?}", self.current_type());
        self.expect(&TokenType::Print)?;
        let value = self.parse_full_expression()?;
        self.eat(&TokenType::Newline);
        Ok(Expression::Print(Box::new(value)))
    }
}
