use super::lexer::{Token, TokenType};
use super::ast::{Expression, Program, UnaryOperator, BinaryOperator, Parameter, FieldDefinition, TypeExpression, MatchCase};
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
            match self.parse_expression() {
                Ok(expr) => expressions.push(expr),
                Err(e) => {
                    self.errors.push(e);
                    self.synchronize(); // Error recovery
                }
            }
        }
        
        if self.errors.is_empty() {
            Ok(Program { expressions })
        } else {
            Err(self.errors.clone())
        }
    }

    // Token Stream Management

    /// Advance to next token and return previous
    fn bump(&mut self) -> Option<Token> {
        let previous = self.current.take();
        self.current = self.tokens.next();
        previous
    }

    /// Look at current token without consuming
    fn current(&self) -> Option<&Token> {
        self.current.as_ref()
    }

    fn current_type(&self) -> Option<TokenType> {
        self.current().map(|t| t.token_type)
    }

    /// Check if we're at a specific token type
    fn at(&self, token_type: &TokenType) -> bool {
        self.current().map_or(false, |t| &t.token_type == token_type)
    }

    /// Check if we're at the end of input
    fn at_end(&self) -> bool {
        self.current.is_none() || self.at(&TokenType::EOF)
    }

    /// Consume current token if it matches expected type
    fn eat(&mut self, token_type: &TokenType) -> bool {
        if self.at(token_type) {
            self.bump();
            true
        } else {
            false
        }
    }

    /// Expect a specific token type or error
    fn expect(&mut self, &token_type: TokenType) -> Result<Token, String> {
        if let Some(token) = self.bump() {
            if &token.token_type == token_type {
                Ok(token)
            } else {
                Err(format!("Expected {:?}, found {:?}", token_type, token.token_type))
            }
        } else {
            Err("Unexpected end of input".to_string())
        }
    }

    pub fn parse_expression(&mut self) -> Result<Expression, String> {
        println!("Parsing expression");
        match self.current_type(){
            Some(TokenType::Function) => self.function(),
            Some(TokenType::Struct) => self.structure(),
            Some(TokenType::If) => self.if_statement(),
            Some(TokenType::For) => self.for_statement(),
            Some(TokenType::Return) => self.return_statement(),
            //_ => Err("Unexpected end of input".to_string())
        }
        /*
        match token, peek with 
        | "identifier" "equal" => variable assignment
        | "identifier" "lparen" => function call
        | "function" "identifier" => function definition
        | "identifier" "binary_operator" => binary expression
        | etc...
         */
    }

    /// Currently only supports standard block function definitions
    fn function(&mut self) -> Result<Expression, String> {
        self.eat(&TokenType::Function)?;
        let identifier = self.expect(&TokenType::Identifier)?;
        self.eat(&TokenType::LParen)?;
        let arguments = self.parameters()?;
        self.eat(&TokenType::RParen)?;
        self.eat(&TokenType::Newline)?;
        let body = self.block()?;
        self.eat(&TokenType::End)?;
        Ok(Expression::FunctionDefinition(identifier, arguments, body))
    }

    /// TODO: Support variant structs
    /// struct <identifier>
    ///     <field>
    ///     <field>
    ///     ...
    /// end
    fn structure(&mut self) -> Result<Expression, String> {
        self.eat(&TokenType::Struct)?;
        let identifier = self.expect(&TokenType::Identifier)?;
        self.eat(&TokenType::Newline)?;
        let fields = self.field_parameters()?;
        self.eat(&TokenType::End)?;
        Ok(Expression::StructDefinition(identifier, fields))
    }

    // parse expressions until 'END' token
    fn block(&mut self) -> Result<Expression, String> {
        let mut expressions = Vec::new();
        while !self.at(&TokenType::End) {
            expressions.push(self.parse_expression()?);
        }
        Ok(Expression::Block(expressions))
    }

    // parse expressions until 'RParen' token
    fn parameters(&mut self) -> Result<Vec<Parameter>, String> {
       let mut parameters = Vec::new();
       while self.at(&TokenType::Identifier) {
        let identifier = self.bump().unwrap().clone();
        self.eat(&TokenType::Comma); 
        // If there is no comma, then it will not consume the token and will exit loop on next iteration
        // TODO: I don't think this would catch `my_fn(x,y z)` since it'd ignore the no comma
        let parameter = Parameter { name: identifier, type_expr: None };
        parameters.push(parameter);
       }
       Ok(parameters)
    }

    fn field_parameters(&mut self) -> Result<Vec<Parameter>, String> {
        let mut fields = Vec::new();
        while self.at(&TokenType::Identifier) {
            let identifier = self.bump().unwrap().clone();
            //self.eat(&TokenType::Colon); // TODO: Support type annotations
            self.eat(&TokenType::Newline);
            let field = Parameter { name: identifier, type_expr: None };
            fields.push(field);
        }
        Ok(fields)
    }

    /// Parse if-elseif-else-end expressions
    fn if_statement(&mut self) -> Result<Expression, String> {
        self.eat(&TokenType::If)?;
        let condition = self.parse_expression()?;
        self.eat(&TokenType::Newline)?;
        let then_branch = self.block()?;

        //let mut elseif_branches = Vec::new();
        let mut else_branch = None;

        /*
        while self.at(&TokenType::ElseIf) {
            self.eat(&TokenType::ElseIf)?;
            let elseif_condition = self.parse_expression()?;
            self.eat(&TokenType::Newline)?;
            let elseif_branch = self.block()?;
            elseif_branches.push((elseif_condition, elseif_branch));
        }*/
        
        // Check for else
        if self.eat(&TokenType::Else)? {
            else_branch = Some(self.block()?);
        }

        self.eat(&TokenType::End)?;
        Ok(Expression::If(condition, then_branch, else_branch))
    }

    fn for_statement(&mut self) -> Result<Expression, String> {
        self.eat(&TokenType::For)?;
        let iterator = self.expect(&TokenType::Identifier)?;
        self.eat(&TokenType::In)?;
        let range = self.parse_expression()?;
        self.eat(&TokenType::Newline)?;
        let body = self.block()?;
        self.eat(&TokenType::End)?;
        Ok(Expression::For(iterator, range, body))
    }

    fn return_statement(&mut self) -> Result<Expression, String> {
        self.eat(&TokenType::Return)?;
        let value = self.parse_expression()?;
        Ok(Expression::Return(value))
    }

}
