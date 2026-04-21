use crate::{parser_log};
use super::lexer::{Token, TokenType, token_type};
use super::ast::{Expression, Program, UnaryOperator, BinaryOperator, Parameter, FieldDefinition, Id, TypeExpression, EnumVariant, MatchCase, MatchPattern, GenericArg};
use std::iter::Peekable;
use std::vec::IntoIter;

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

    /// Core parsing function that returns a Program AST
    pub fn parse(&mut self) -> Result<Program, Vec<String>> {
        let mut expressions = Vec::new();
        
        while !self.at_end() {
            parser_log!("------ Parsing new top level expression ------");
            match self.parse_expression() {
                Ok(expr) => expressions.push(expr),
                Err(e) => {
                    parser_log!("Error: {:?}", &e);
                    self.errors.push(e);
                    //self.synchronize(); // Error recovery
                }
            }
        }
        parser_log!("------ End of parsing ------");

        if self.errors.is_empty() {
            Ok(Program { expressions })
        } else {
            parser_log!("------ Errors found ------");
            Err(self.errors.clone())
        }
    }

    // Token Stream Management

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
        parser_log!("Expecting {:?}", token_type);
        if let Some(token) = self.bump() {
            parser_log!("Expecting| actual token:{:?}", token);
            if token.var() == token_type.var() {
                Ok(token)
            } else {
                Err(format!("Expected {:?}, found {:?}", token_type, token.token_type))
            }
        } else {
            parser_log!("Unexpected end of input");
            Err("Unexpected end of input".to_string())
        }
    }

    /// Parse expressions (functions, structs, if statements, for statements, return statements)
    /// TODO: Change to parse_statement()
    pub fn parse_expression(&mut self) -> Result<Expression, String> {
        parser_log!("Parsing expression| current:{:?}", self.current_type());
        let result = match self.current_type(){
            Some(TokenType::Foreign) => self.function(),
            Some(TokenType::Function) => self.function(),
            Some(TokenType::Struct) => self.structure(),
            Some(TokenType::Enum) => self.enum_def(),
            Some(TokenType::If) => self.if_statement(), // TODO: Move to parse_expression_1
            Some(TokenType::While) => self.while_statement(),
            Some(TokenType::For) => self.for_statement(),
            Some(TokenType::Return) => self.return_statement(),
            Some(TokenType::Match) => self.match_expr(),
            Some(TokenType::Newline) => {
                self.bump(); 
                self.parse_expression()
            },
            Some(TokenType::Print) => self.print_statement(),
            Some(TokenType::End) => {
                // Skip End tokens at the top level - they should be handled by their respective constructs
                self.bump();
                Err("Unexpected End token at top level".to_string())
            },
            Some(_) => {
                let primary_expr = self.primary()?; // Store the result of primary
                self.parse_expression_1(primary_expr, 0) // Delegate to the new expression parser
            }
            None => Err("No current token".to_string()),
        };
        parser_log!("Parsing expression| result:{:?}", result);
        let mut at_newline = self.eat(&TokenType::Newline);
        while at_newline {
            at_newline = self.eat(&TokenType::Newline);
        }
        result
    }

    /// Bottom-up operator-precedence parsing of expressions
    /// Any operators, literals, and calls, are parsed here (including parenthesized expressions)
    fn parse_expression_1(&mut self, lhs: Expression, min_precedence: i32) -> Result<Expression, String> {
        parser_log!("Parsing expression 1| Starting -------------------");
        parser_log!("Parsing expression 1| lhs:{:?}, current:{:?}", lhs, self.current_type());
        let mut level_lhs = lhs;
        while let Some(current) = self.current_type() {
            if !current.is_infix() {
                parser_log!("No infix operator found");
                break;
            }
            if !(current.precedence()? >= min_precedence) {
                parser_log!("Parsing expression 1| precedence:{:?} < {:?}", current.precedence(), min_precedence);
                break; // Exit the loop if precedence is not greater
            }
            let op = self.bump().ok_or("No operator found".to_string())?;
            let op_precedence = op.precedence()?;
            parser_log!("Parsing expression 1| op:{:?}, op_precedence:{:?}", op, op_precedence);
            let mut rhs = self.primary()?;
            parser_log!("Parsing expression 1| rhs:{:?}", rhs);

            // Handle operator precedence and associativity
            while let Some(next) = self.current_type() {
                if !next.is_infix() {
                    // No infix operator found, break
                    break;
                }
                parser_log!("Parsing expression 1| next op:{:?}", next);
                let precedence = next.precedence()?;
                if precedence > op_precedence {
                    parser_log!("Parsing expression 1| left associative precedence:{:?}", op_precedence+1);
                    rhs = self.parse_expression_1(rhs.clone(), op_precedence+1)?;
                } else if precedence == op_precedence && next.is_right_associative() {
                    parser_log!("Parsing expression 1| right associative precedence:{:?}", op_precedence);
                    rhs = self.parse_expression_1(rhs.clone(), op_precedence)?;
                } else {
                    break; // Exit the loop if precedence is not greater
                }
            }

            parser_log!("Applying infix operator: {:?} {:?} {:?}", level_lhs, op.token_type, rhs);
            level_lhs = self.apply_infix_operator(level_lhs.clone(), &op, rhs.clone())?;
        }

        parser_log!("Parsing expression 1| End of expression 1");
        // Don't consume newlines here - let the caller handle it
        Ok(level_lhs) // Return the final expression as a clone
    }

    fn apply_infix_operator(&mut self, lhs: Expression, op: &Token, rhs: Expression) -> Result<Expression, String> {
        match op.token_type {
            TokenType::Dot => Ok(Expression::BinaryOp { operator: BinaryOperator::Dot, left: Box::new(lhs), right: Box::new(rhs) }),
            TokenType::Plus => Ok(Expression::BinaryOp { operator: BinaryOperator::Add, left: Box::new(lhs), right: Box::new(rhs) }),
            TokenType::Minus => Ok(Expression::BinaryOp { operator: BinaryOperator::Subtract, left: Box::new(lhs), right: Box::new(rhs) }),
            TokenType::Asterisk => Ok(Expression::BinaryOp { operator: BinaryOperator::Multiply, left: Box::new(lhs), right: Box::new(rhs) }),
            TokenType::Slash => Ok(Expression::BinaryOp { operator: BinaryOperator::Divide, left: Box::new(lhs), right: Box::new(rhs) }),
            TokenType::Modulo => Ok(Expression::BinaryOp { operator: BinaryOperator::Modulo, left: Box::new(lhs), right: Box::new(rhs) }),
            TokenType::Carrot => Ok(Expression::BinaryOp { operator: BinaryOperator::Power, left: Box::new(lhs), right: Box::new(rhs) }),
            TokenType::Equal => Ok(Expression::BinaryOp { operator: BinaryOperator::Equal, left: Box::new(lhs), right: Box::new(rhs) }),
            TokenType::NotEqual => Ok(Expression::BinaryOp { operator: BinaryOperator::NotEqual, left: Box::new(lhs), right: Box::new(rhs) }),
            TokenType::LessThan => Ok(Expression::BinaryOp { operator: BinaryOperator::LessThan, left: Box::new(lhs), right: Box::new(rhs) }),
            TokenType::GreaterThan => Ok(Expression::BinaryOp { operator: BinaryOperator::GreaterThan, left: Box::new(lhs), right: Box::new(rhs) }),
            TokenType::LessThanEqual => Ok(Expression::BinaryOp { operator: BinaryOperator::LessThanEqual, left: Box::new(lhs), right: Box::new(rhs) }),
            TokenType::GreaterThanEqual => Ok(Expression::BinaryOp { operator: BinaryOperator::GreaterThanEqual, left: Box::new(lhs), right: Box::new(rhs) }),
            TokenType::And => Ok(Expression::BinaryOp { operator: BinaryOperator::And, left: Box::new(lhs), right: Box::new(rhs) }),
            TokenType::Ampersand => Ok(Expression::BinaryOp { operator: BinaryOperator::Ampersand, left: Box::new(lhs), right: Box::new(rhs) }),
            TokenType::Or => Ok(Expression::BinaryOp { operator: BinaryOperator::Or, left: Box::new(lhs), right: Box::new(rhs) }),
            TokenType::Pipe => Ok(Expression::BinaryOp { operator: BinaryOperator::Pipe, left: Box::new(lhs), right: Box::new(rhs) }),
            // TODO: Do we need to move assignment out of infix? It could be called in places it shouldn't be
            // e.g. my_fn(x = 1, y = 2). This is only valid if x and y are parameters.
            TokenType::Assignment => {
                match &lhs {
                    Expression::Identifier { id, type_expr } => Ok(Expression::BinaryOp { operator: BinaryOperator::Assignment, left: Box::new(lhs), right: Box::new(rhs) }),
                    _ => Err(format!("Cannot assign to {:?}", lhs)),
                }
            },
            _ => Err(format!("Unsupported operator: {:?}", op.token_type)),
        }
    }

    /// prefix -> call -> literal 
    /// primary ::= '-' inner_primary | '!' inner_primary
    fn primary(&mut self) -> Result<Expression, String> {
        parser_log!("Parsing primary| current:{:?}", self.current_type());
        let prefix_op = self.check_prefix_operator();

        let inner_primary = self.inner_primary()?;

        if let Some(op) = prefix_op {
            Ok(Expression::UnaryOp { operator: op, operand: Box::new(inner_primary) })
        } else {
            Ok(inner_primary)
        }
    }

    /// inner_primary ::= call | literal
    fn inner_primary(&mut self) -> Result<Expression, String> {
        parser_log!("Parsing inner primary| current:{:?}", self.current_type());
        match self.current_type() {
            Some(TokenType::Colon) => self.symbol(),
            Some(TokenType::LParen) => self.parenthesized_expression(),
            Some(TokenType::Int(n)) => self.literal(&TokenType::Int(*n)),
            Some(TokenType::Float(n)) => self.literal(&TokenType::Float(*n)),
            Some(TokenType::StringLiteral(s)) => self.literal(&TokenType::StringLiteral(s.to_string())),
            Some(TokenType::True) => self.literal(&TokenType::True),
            Some(TokenType::False) => self.literal(&TokenType::False),
            Some(TokenType::Match) => self.match_expr(),
            Some(TokenType::Identifier(_)) => self.identifier(),
            _ => Err(format!("Unexpected token: {:?}", self.current_type())),
        }
    }

    fn check_prefix_operator(&mut self) -> Option<UnaryOperator> {
        parser_log!("Checking prefix operator| current:{:?}", self.current_type());
        match self.current_type() {
            Some(TokenType::Minus) => {
                self.bump()?;
                Some(UnaryOperator::Negate)
            },
            Some(TokenType::Bang) => {
                self.bump()?;
                Some(UnaryOperator::Not)
            },
            _ => None,
        }
    }

    /// Parse parenthesized expressions
    /// Does not allow 
    fn parenthesized_expression(&mut self) -> Result<Expression, String> {
        parser_log!("Parsing parenthesized expression| current:{:?}", self.current_type());
        self.expect(&TokenType::LParen)?;
        let primary_expr = self.primary()?; // Store the result of primary
        parser_log!("Parenthesized | primary expression: {:?}", primary_expr);
        let expression = self.parse_expression_1(primary_expr, 0)?; // Use the stored result
        self.expect(&TokenType::RParen)?;
        Ok(expression)
    }

    /// identifier ::= IDENTIFIER(::Type) | IDENTIFIER '(' parameters ')' (call) | IDENTIFIER.IDENTIFIER (field access)
    /// Also handles enum variant construction: `EnumName::Variant(args)`
    fn identifier(&mut self) -> Result<Expression, String> {
        parser_log!("Parsing identifier| current:{:?}", self.current_type());
        let id = self.id()?;

        if self.eat(&TokenType::TypeDef) {
            // After `::`, the next token must be an identifier (variant name or type name).
            let next_id = self.id()?;

            if self.eat(&TokenType::LParen) {
                // `Name::Variant(args)` — enum variant construction with positional args.
                parser_log!("Enum variant construction");
                let args = self.arguments()?;
                self.expect(&TokenType::RParen)?;
                Ok(Expression::EnumVariantConstruct {
                    enum_name: id.name,
                    variant: next_id.name,
                    args,
                })

            } else {
                // `Name::Type` or `Name::Foo<Int>` — type annotation.
                parser_log!("Identifier with type annotation");
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
            parser_log!("Function call");
            let parameters = self.arguments()?;
            self.expect(&TokenType::RParen)?;
            Ok(Expression::FunctionCall { id, arguments: parameters })
        } else {
            parser_log!("Identifier");
            Ok(Expression::Identifier { id, type_expr: None })
        }
    }

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
                parser_log!("Expected identifier! found: {:?}", self.current_type());
                Err("Expected identifier".to_string())
            }
        }
    }

    fn type_expr(&mut self) -> Result<TypeExpression, String> {
        parser_log!("Parsing type expression| current:{:?}", self.current_type());
        
        let id = self.id()?; // Parse the base identifier
        if self.eat(&TokenType::LBrace) { // Check for the start of a composite type
            let mut types = Vec::new();
            while !self.at(&TokenType::RBrace) {
                let inner_type = self.type_expr()?; // Parse inner types
                types.push(inner_type);
                if !self.eat(&TokenType::Comma) && !self.at(&TokenType::RBrace) {
                    return Err("Expected comma or right brace".to_string());
                }
            }
            self.expect(&TokenType::RBrace)?; // Expect the closing brace
            Ok(TypeExpression::Struct(id, Some(types), None)) // Return struct type expression
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

    /// Comma-separated type arguments inside `< ... >` (closing `>` already consumed by caller for first `<`).
    fn generic_type_args(&mut self) -> Result<Vec<GenericArg>, String> {
        let mut args = Vec::new();
        while !self.at(&TokenType::GreaterThan) {
            let te = self.type_expr()?;
            args.push(GenericArg::Type(te));
            if !self.eat(&TokenType::Comma) && !self.at(&TokenType::GreaterThan) {
                return Err("Expected comma or '>' in generic type argument list".to_string());
            }
        }
        self.expect(&TokenType::GreaterThan)?;
        Ok(args)
    }

    /// Type parameter names on `struct Foo<T, U>` / `enum Bar<T>`.
    fn type_param_names(&mut self) -> Result<Vec<Id>, String> {
        let mut names = Vec::new();
        while !self.at(&TokenType::GreaterThan) {
            names.push(self.id()?);
            if !self.eat(&TokenType::Comma) && !self.at(&TokenType::GreaterThan) {
                return Err("Expected comma or '>' in type parameter list".to_string());
            }
        }
        self.expect(&TokenType::GreaterThan)?;
        Ok(names)
    }

    /// Parse literals (String, int, float, boolean)
    fn literal(&mut self, token_type: &TokenType) -> Result<Expression, String> {
        parser_log!("Parsing literal| current:{:?} expected:{:?}", self.current_type(), token_type);
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

    /// Parse function definitions
    /// Currently only supports standard block function definitions
    fn function(&mut self) -> Result<Expression, String> {
        parser_log!("Parsing function| current:{:?}", self.current_type());
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
            parser_log!("Parsing function| End of function");
            self.expect(&TokenType::Newline); // Should these be eats or expects?
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
        parser_log!("Parsing structure| current:{:?}", self.current_type());
        self.expect(&TokenType::Struct)?;
        let id = self.id()?;
        let type_params = if self.eat(&TokenType::LessThan) {
            self.type_param_names()?
        } else {
            vec![]
        };
        self.expect(&TokenType::Newline)?;
        let fields = self.field_parameters()?;
        self.expect(&TokenType::End)?;
        Ok(Expression::StructDefinition { id, fields, type_params })
    }

    // enum <identifier>
    //     <Variant>
    //     <Variant>
    //     ...
    // end
    /// Parse enum definitions.
    /// Each variant is a bare identifier on its own line.
    fn enum_def(&mut self) -> Result<Expression, String> {
        parser_log!("Parsing enum| current:{:?}", self.current_type());
        self.expect(&TokenType::Enum)?;
        let id = self.id()?;
        let type_params = if self.eat(&TokenType::LessThan) {
            self.type_param_names()?
        } else {
            vec![]
        };
        self.expect(&TokenType::Newline)?;
        let variants = self.enum_variants()?;
        self.expect(&TokenType::End)?;
        Ok(Expression::EnumDefinition { id, variants, type_params })
    }

    /// Parse enum variant names until `end`.
    ///
    /// Supported forms (using `::` type-annotation notation):
    ///   `Quit`                    → unit variant
    ///   `Move::(Int, Int)`        → tuple variant; fields auto-named _0, _1, …
    ///   `Write::String`           → single-type variant; field auto-named _0
    ///   `Point::{x::Int, y::Int}` → struct variant with named fields
    fn enum_variants(&mut self) -> Result<Vec<EnumVariant>, String> {
        parser_log!("Parsing enum variants| current:{:?}", self.current_type());
        let mut variants = Vec::new();
        while self.at(&token_type("Identifier")?) {
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
                            id: Id { name: format!("_{}", i) },
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
                        id: Id { name: "_0".to_string() },
                        field_type: t,
                    }];
                    (fields, true)
                }
            } else {
                // Unit variant
                (vec![], false)
            };

            self.expect(&TokenType::Newline)?;
            variants.push(EnumVariant { name: id.name, fields, is_tuple });
        }
        Ok(variants)
    }

    /// Parse a comma-separated list of `TypeExpression`s up to `)`.
    fn type_list(&mut self) -> Result<Vec<TypeExpression>, String> {
        let mut types = Vec::new();
        while !self.at(&TokenType::RParen) {
            types.push(self.type_expr()?);
            if !(self.eat(&TokenType::Comma) ^ self.at(&TokenType::RParen)) {
                return Err("Expected comma or right parenthesis in type list".to_string());
            }
        }
        Ok(types)
    }

    /// Parse comma-separated `name::Type` field pairs inside `{ … }`.
    /// Does NOT consume the closing `}`.
    fn inline_field_list(&mut self) -> Result<Vec<FieldDefinition>, String> {
        let mut fields = Vec::new();
        while self.at(&token_type("Identifier")?) {
            let id = self.id()?;
            self.expect(&TokenType::TypeDef)?;
            let field_type = self.type_expr()?;
            let field = FieldDefinition { id, field_type };
            fields.push(field);
            if !(self.eat(&TokenType::Comma) ^ self.at(&TokenType::RBrace)) {
                return Err("Expected comma or right brace in struct variant field list".to_string());
            }
        }
        Ok(fields)
    }

    /// Parse expressions until 'END' token
    fn block(&mut self) -> Result<Expression, String> {
        parser_log!("Parsing block| Starting block: {:?}", self.current_type());
        let mut expressions = Vec::new();
        while !self.at(&TokenType::End) {
            parser_log!("Parsing block| not end: {:?}", self.current_type());
            expressions.push(Box::new(self.parse_expression()?));
        }
        parser_log!("Parsing block| End of block");
        Ok(Expression::Block(expressions))
    }

    // TODO: Abstract parameters, field_parameters, and arguments into Delimited(self, parse_element_fn, delim, end)

    /// Parse expressions until 'RParen' token
    fn parameters(&mut self) -> Result<Vec<Parameter>, String> {
        parser_log!("Parsing parameters| current:{:?}", self.current_type());
       let mut parameters = Vec::new();
       while !self.at(&TokenType::RParen) {
        let id = self.id()?;
        let parameter = if self.eat(&TokenType::TypeDef) {
            let type_expr = self.type_expr()?;
            Parameter { id, type_expr: Some(type_expr) }
        } else {
            Parameter { id, type_expr: None }
        };
        parameters.push(parameter);
        if !(self.eat(&TokenType::Comma) ^ self.at(&TokenType::RParen)) {
            return Err("Expected comma or right parenthesis".to_string());
        }
       }
       Ok(parameters)
    }

    /// Parse field definitions
    /// id::type
    fn field_parameters(&mut self) -> Result<Vec<FieldDefinition>, String> {
        parser_log!("Parsing field parameters| current:{:?}", self.current_type());
        let mut fields = Vec::new();
        while self.at(&token_type("Identifier")?) {
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
        parser_log!("Parsing arguments| current:{:?}", self.current_type());
        let mut arguments = Vec::new();
        while !self.at(&TokenType::RParen) {
            let primary_expr = self.primary()?; // Store the result of primary
            let expression = self.parse_expression_1(primary_expr, 0)?; // Use the stored result
            arguments.push(expression);
            if !(self.eat(&TokenType::Comma) ^ self.at(&TokenType::RParen)) {
                return Err("Expected comma or right parenthesis".to_string());
            }
        }
        Ok(arguments)
    }

    /// Parse if-elseif-else-end expressions
    /// TODO: Support both block and inline if statements
    /// inline should be within parse_expression_1() and blocks should be within parse_expression()
    fn if_statement(&mut self) -> Result<Expression, String> {
        parser_log!("Parsing if statement| current:{:?}", self.current_type());
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
            else_branch: else_branch
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
        parser_log!("Parsing match expression| current:{:?}", self.current_type());
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
        Ok(Expression::Match { value: Box::new(value), cases })
    }

    /// Parse a single match arm pattern:
    /// - `_`                          → Wildcard
    /// - `EnumName::Variant`          → EnumVariant with no bindings
    /// - `EnumName::Variant(x, y)`    → EnumVariant with positional bindings
    fn match_pattern(&mut self) -> Result<MatchPattern, String> {
        parser_log!("Parsing match pattern| current:{:?}", self.current_type());
        // Wildcard
        if let Some(TokenType::Identifier(name)) = self.current_type() {
            if name == "_" {
                self.bump();
                return Ok(MatchPattern::Wildcard);
            }
        }
        // EnumName::Variant or EnumName::Variant(bindings)
        let enum_id = self.id()?;
        self.expect(&TokenType::TypeDef)?;  // ::
        let variant_id = self.id()?;
        let bindings = if self.eat(&TokenType::LParen) {
            let mut names = Vec::new();
            while !self.at(&TokenType::RParen) {
                let binding = self.id()?;
                names.push(binding.name);
                if !(self.eat(&TokenType::Comma) ^ self.at(&TokenType::RParen)) {
                    return Err("Expected comma or ')' in match pattern bindings".to_string());
                }
            }
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
        parser_log!("Parsing while statement| current:{:?}", self.current_type());
        self.expect(&TokenType::While)?;
        let primary_expr = self.primary()?;
        let condition = self.parse_expression_1(primary_expr, 0)?;
        parser_log!("Parsing while statement| condition:{:?}", condition);
        self.eat(&TokenType::Newline); // parse_expression_1() eats the newline
        parser_log!("Parsing while statement| newline");
        let body = self.block()?;
        self.eat(&TokenType::Newline);
        self.expect(&TokenType::End)?;
        parser_log!("Parsing while statement| body:{:?}", body);
        Ok(Expression::While { condition: Box::new(condition), body: Box::new(body) })
    }

    fn for_statement(&mut self) -> Result<Expression, String> {
        parser_log!("Parsing for statement| current:{:?}", self.current_type());
        self.expect(&TokenType::For)?;
        let iterator = self.id()?;
        self.expect(&TokenType::In)?;
        let range = self.parse_expression()?;
        self.expect(&TokenType::Newline)?;
        let body = self.block()?;
        self.expect(&TokenType::End)?;
        Ok(Expression::For { iterator, range: Box::new(range), body: Box::new(body) })
    }

    fn return_statement(&mut self) -> Result<Expression, String> {
        parser_log!("Parsing return statement| current:{:?}", self.current_type());
        self.expect(&TokenType::Return)?;
        let primary_expr = self.primary()?; // Store the result of primary
        let value = self.parse_expression_1(primary_expr, 0)?;
        self.eat(&TokenType::Newline);
        parser_log!("Parsing return statement| end of return");
        Ok(Expression::Return(Some(Box::new(value))))
    }

    fn print_statement(&mut self) -> Result<Expression, String> {
        parser_log!("Parsing print statement| current:{:?}", self.current_type());
        self.expect(&TokenType::Print)?;
        let primary_expr = self.primary()?;
        let value = self.parse_expression_1(primary_expr, 0)?;
        self.eat(&TokenType::Newline);
        Ok(Expression::Print(Box::new(value)))
    }

}
