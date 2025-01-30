use super::lexer::{Token, TokenType};
use super::ast::{Expression, Program, UnaryOperator, BinaryOperator, Parameter, FieldDefinition, TypeExpression};
use std::iter::Peekable;
use std::vec::IntoIter;

pub struct Parser {
    tokens: Peekable<IntoIter<Token>>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens: tokens.into_iter().peekable(),
        }
    }

    // Core parsing function
    pub fn parse(&mut self) -> Result<Program, String> {
        println!("Parsing tokens...");
        let mut expressions = Vec::new();
        
        while let Some(_) = self.peek() {
            match self.parse_expression() {
                Ok(expr) => expressions.push(expr),
                Err(e) => return Err(e),
            }
        }
        
        Ok(Program { expressions })
    }

    // Utility functions for token handling
    fn peek(&mut self) -> Option<&Token> {
        self.tokens.peek()
    }

    fn next(&mut self) -> Option<Token> {
        while let Some(token) = self.tokens.next() {
            match token.token_type {
                TokenType::Comment(_) | TokenType::BlockComment(_, _, _) | TokenType::Newline => continue,
                _ => {
                    println!("Next token: {:?}", token);
                    return Some(token);
                }
            }
        }
        None
    }

    fn expect(&mut self, expected: TokenType) -> Result<Token, String> {
        match self.next() {
            Some(token) if token.token_type == expected => Ok(token),
            Some(token) => Err(format!("Expected {:?}, found {:?}", expected, token.token_type)),
            None => Err("Unexpected end of input".to_string()),
        }
    }

    fn match_token(&mut self, token_type: TokenType) -> bool {
        if let Some(token) = self.peek() {
            if token.token_type == token_type {
                self.next();
                return true;
            }
        }
        false
    }

    // Expression parsing
    fn parse_expression(&mut self) -> Result<Expression, String> {
        println!("Parsing expression");
        self.parse_binary_expression()
    }


    fn parse_binary_expression(&mut self) -> Result<Expression, String> {
        println!("Parsing binary expression");
        let mut left = self.parse_unary_expression()?;

        while let Some(token) = self.peek() {
            let operator = match &token.token_type {
                TokenType::Plus => BinaryOperator::Add,
                TokenType::Minus => BinaryOperator::Subtract,
                TokenType::Multiply => BinaryOperator::Multiply,
                TokenType::Divide => BinaryOperator::Divide,
                TokenType::Modulus => BinaryOperator::Modulo,
                TokenType::LessThan => BinaryOperator::LessThan,
                TokenType::LessThanOrEqual => BinaryOperator::LessThanEqual,
                TokenType::GreaterThan => BinaryOperator::GreaterThan,
                TokenType::GreaterThanOrEqual => BinaryOperator::GreaterThanEqual,
                TokenType::And => BinaryOperator::And,
                TokenType::Or => BinaryOperator::Or,
                _ => break,
            };
            self.next(); // Consume operator token
            let right = self.parse_unary_expression()?;
            left = Expression::BinaryOp {
                operator,
                left: Box::new(left),
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn parse_unary_expression(&mut self) -> Result<Expression, String> {
        println!("Parsing unary expression");
        if let Some(token) = self.peek() {
            match &token.token_type {
                TokenType::Bang => {
                    self.next(); // Consume the operator
                    let operand = self.parse_unary_expression()?;
                    return Ok(Expression::UnaryOp {
                        operator: UnaryOperator::Not,
                        operand: Box::new(operand),
                    });
                }
                TokenType::Minus => {
                    self.next(); // Consume the operator
                    let operand = self.parse_unary_expression()?;
                    return Ok(Expression::UnaryOp {
                        operator: UnaryOperator::Negate,
                        operand: Box::new(operand),
                    });
                }
                _ => {}
            }
        }
        self.parse_primary_expression()
    }

    fn parse_primary_expression(&mut self) -> Result<Expression, String> {
        println!("Parsing primary expression");
        match self.next() {
            Some(token) => match token.token_type {
                TokenType::Number(n) => Ok(Expression::Number(n)),
                TokenType::True => Ok(Expression::Boolean(true)),
                TokenType::False => Ok(Expression::Boolean(false)),
                TokenType::StringLiteral(s) => Ok(Expression::String(s)),
                TokenType::Identifier(name) => {
                    if let Some(token) = self.peek() {
                        match &token.token_type {
                            // Function call
                            TokenType::LParen => {
                                self.next(); // Consume '('
                                let args = self.parse_arguments()?;
                                self.expect(TokenType::RParen)?;
                                Ok(Expression::FunctionCall {
                                    name,
                                    arguments: args,
                                })
                            }
                            // Variable assignment
                            TokenType::Equal => self.parse_variable_assignment(),
                            _ => Ok(Expression::Identifier(name)),
                        }
                    } else {
                        Ok(Expression::Identifier(name))
                    }
                }
                TokenType::Function => self.parse_function_definition(),
                TokenType::Struct => self.parse_struct_definition(),
                TokenType::If => self.parse_if_expression(),
                TokenType::For => self.parse_for_expression(),
                TokenType::Print => {
                    self.expect(TokenType::LParen)?;
                    let expr = self.parse_expression()?;
                    self.expect(TokenType::RParen)?;
                    Ok(Expression::Print(Box::new(expr)))
                }
                _ => Err(format!("Unexpected token: {:?}", token.token_type)),
            },
            None => Err("Unexpected end of input".to_string()),
        }
    }

    // Variable assignment parsing
    fn parse_variable_assignment(&mut self) -> Result<Expression, String> {
        println!("Parsing variable assignment");
        let name = match self.next() {
            Some(Token { token_type: TokenType::Identifier(name), .. }) => name,
            Some(token) => return Err(format!("Expected variable name, found {:?}", token.token_type)),
            None => return Err("Unexpected end of input while parsing variable name".to_string()),
        };

        self.expect(TokenType::Equal)?;

        let value = Box::new(self.parse_expression()?);

        Ok(Expression::VariableAssignment {
            name,
            value,
        })
    }

    // Helper function to parse function arguments
    fn parse_arguments(&mut self) -> Result<Vec<Expression>, String> {
        println!("Parsing arguments");
        let mut args = Vec::new();
        
        if self.peek().map_or(false, |t| matches!(t.token_type, TokenType::RParen)) {
            return Ok(args);
        }

        args.push(self.parse_expression()?);
        while self.match_token(TokenType::Comma) {
            args.push(self.parse_expression()?);
        }

        Ok(args)
    }

    fn parse_function_definition(&mut self) -> Result<Expression, String> {
        println!("Parsing function definition");
        // Parse function name
        let name = match self.next() {
            Some(Token { token_type: TokenType::Identifier(name), .. }) => name,
            Some(token) => return Err(format!("Expected function name, found {:?}", token.token_type)),
            None => return Err("Unexpected end of input while parsing function name".to_string()),
        };

        // Parse parameters
        self.expect(TokenType::LParen)?;
        let parameters = self.parse_parameters()?;
        self.expect(TokenType::RParen)?;

        // Check for the two function styles:
        // 1. fn name(params) = expr
        // 2. fn name(params) body end
        let body = if self.match_token(TokenType::Equal) {
            // Single expression function
            let expr = self.parse_expression()?;
            vec![expr]
        } else {
            self.parse_function_body()?
        };

        Ok(Expression::FunctionDefinition {
            name,
            parameters,
            body,
        })
    }

    fn parse_function_body(&mut self) -> Result<Vec<Expression>, String> {
        println!("Parsing function body");
        let mut body = Vec::new();
        
        while let Some(token) = self.peek() {
            if let TokenType::End = token.token_type {
                self.next(); // Consume the End token
                return Ok(body);
            }
            body.push(self.parse_expression()?);
        }
        
        Err("Unexpected end of input while parsing function body".to_string())
    }

    fn parse_parameters(&mut self) -> Result<Vec<Parameter>, String> {
        println!("Parsing parameters");
        let mut parameters = Vec::new();
        
        // Handle empty parameter list
        if self.peek().map_or(false, |t| matches!(t.token_type, TokenType::RParen)) {
            return Ok(parameters);
                }

        // Parse first parameter
        parameters.push(self.parse_parameter()?);

        // Parse remaining parameters
        while self.match_token(TokenType::Comma) {
            parameters.push(self.parse_parameter()?);
            }

        Ok(parameters)
        }

    fn parse_parameter(&mut self) -> Result<Parameter, String> {
        println!("Parsing parameter");
        // Parse parameter name
        let name = match self.next() {
            Some(Token { token_type: TokenType::Identifier(name), .. }) => name,
            Some(token) => return Err(format!("Expected parameter name, found {:?}", token.token_type)),
            None => return Err("Unexpected end of input while parsing parameter".to_string()),
        };

        // Parse optional type annotation
        let type_expr = if self.match_token(TokenType::Colon) {
            Some(self.parse_type_expression()?)
        } else {
            None
        };

        Ok(Parameter { name, type_expr })
    }

    fn parse_type_expression(&mut self) -> Result<TypeExpression, String> {
        println!("Parsing type expression");
        match self.next() {
            Some(token) => match token.token_type {
                TokenType::TypeInt => Ok(TypeExpression::Int),
                TokenType::TypeBool => Ok(TypeExpression::Bool),
                TokenType::TypeVoid => Ok(TypeExpression::Void),
                TokenType::Identifier(name) => {
                    // Check for generic type parameter
                    let type_param = if self.peek().map_or(false, |t| matches!(t.token_type, TokenType::LessThan)) {
                        self.next(); // Consume '<'
                        let param = Some(Box::new(self.parse_type_expression()?));
                        self.expect(TokenType::GreaterThan)?;
                        param
                    } else {
                        None
                    };
                    Ok(TypeExpression::Struct(name, type_param))
                    }
                _ => Err(format!("Expected type expression, found {:?}", token.token_type)),
            },
            None => Err("Unexpected end of input while parsing type expression".to_string()),
        }
    }

    fn parse_struct_definition(&mut self) -> Result<Expression, String> {
        println!("Parsing struct definition");
        // Parse struct name
        let name = match self.next() {
            Some(Token { token_type: TokenType::Identifier(name), .. }) => name,
            Some(token) => return Err(format!("Expected struct name, found {:?}", token.token_type)),
            None => return Err("Unexpected end of input while parsing struct name".to_string()),
        };
        
        // Parse fields
        let mut fields = Vec::new();
        while let Some(token) = self.peek() {
            if matches!(token.token_type, TokenType::End) {
                break;
            }
            fields.push(self.parse_field_definition()?);
        }

        self.expect(TokenType::End)?;

        Ok(Expression::StructDefinition {
            name,
            fields,
        })
        }

    fn parse_field_definition(&mut self) -> Result<FieldDefinition, String> {
        println!("Parsing field definition");
        // Parse field type
        let type_expr = self.parse_type_expression()?;

        // Parse field name
        let name = match self.next() {
            Some(Token { token_type: TokenType::Identifier(name), .. }) => name,
            Some(token) => return Err(format!("Expected field name, found {:?}", token.token_type)),
            None => return Err("Unexpected end of input while parsing field name".to_string()),
        };

        // Expect newline after field definition
        self.expect(TokenType::Newline)?;

        Ok(FieldDefinition { name, type_expr })
    }

    fn parse_if_expression(&mut self) -> Result<Expression, String> {
        println!("Parsing if expression");
        // Parse condition
        let condition = Box::new(self.parse_expression()?);

        // Parse then branch
        let then_branch = Box::new(self.parse_expression()?);

        // Parse optional else branch
        let else_branch = if self.match_token(TokenType::Else) {
            Some(Box::new(self.parse_expression()?))
        } else {
            None
        };

        self.expect(TokenType::End)?;

        Ok(Expression::If {
            condition,
            then_branch,
            else_branch,
        })
    }

    fn parse_for_expression(&mut self) -> Result<Expression, String> {
        println!("Parsing for expression");
        // Parse iterator variable
        let iterator = match self.next() {
            Some(Token { token_type: TokenType::Identifier(name), .. }) => name,
            Some(token) => return Err(format!("Expected iterator name, found {:?}", token.token_type)),
            None => return Err("Unexpected end of input while parsing iterator name".to_string()),
        };

        // Expect 'in' keyword
        self.expect(TokenType::In)?;

        // Parse range expression
        let range = Box::new(self.parse_expression()?);

        // Expect newline after range
        self.expect(TokenType::Newline)?;

        // Parse loop body
        let body = Box::new(self.parse_expression()?);

        // Expect 'end' keyword
        self.expect(TokenType::End)?;

        Ok(Expression::For {
            iterator,
            range,
            body,
        })
    }
}