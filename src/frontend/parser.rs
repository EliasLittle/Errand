use super::lexer::{Token, TokenType};
use super::ast::{Expression, Program, UnaryOperator, BinaryOperator, Parameter, FieldDefinition, Id}; //, TypeExpression, MatchCase};
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
            println!("------ Parsing new top level expression ------");
            match self.parse_expression() {
                Ok(expr) => expressions.push(expr),
                Err(e) => {
                    println!("Error: {:?}", &e);
                    self.errors.push(e);
                    //self.synchronize(); // Error recovery
                }
            }
        }
        println!("------ End of parsing ------");

        if self.errors.is_empty() {
            Ok(Program { expressions })
        } else {
            println!("------ Errors found ------");
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
        if let Some(token) = self.bump() {
            if token.var() == token_type.var() {
                Ok(token)
            } else {
                Err(format!("Expected {:?}, found {:?}", token_type, token.token_type))
            }
        } else {
            Err("Unexpected end of input".to_string())
        }
    }

    /// Parse expressions (functions, structs, if statements, for statements, return statements)
    /// TODO: Change to parse_statement()
    pub fn parse_expression(&mut self) -> Result<Expression, String> {
        println!("Parsing expression| current:{:?}", self.current_type());
        let result = match self.current_type(){
            Some(TokenType::Function) => self.function(),
            Some(TokenType::Struct) => self.structure(),
            Some(TokenType::If) => self.if_statement(), // TODO: Move to parse_expression_1
            Some(TokenType::While) => self.while_statement(),
            Some(TokenType::For) => self.for_statement(),
            Some(TokenType::Return) => self.return_statement(),
            Some(TokenType::Newline) => {
                self.bump(); 
                self.parse_expression()
            },
            Some(TokenType::Print) => self.print_statement(),
            Some(_) => {
                let primary_expr = self.primary()?; // Store the result of primary
                self.parse_expression_1(primary_expr, 0) // Delegate to the new expression parser
            }
            None => Err("No current token".to_string()),
        };
        println!("Parsing expression| result:{:?}", result);
        result
    }

    /// Bottom-up operator-precedence parsing of expressions
    /// Any operators, literals, and calls, are parsed here (including parenthesized expressions)
    fn parse_expression_1(&mut self, lhs: Expression, min_precedence: i32) -> Result<Expression, String> {
        println!("Parsing expression 1| Starting -------------------");
        println!("Parsing expression 1| lhs:{:?}, current:{:?}", lhs, self.current_type());
        let mut level_lhs = lhs;
        while let Some(current) = self.current_type() {
            if !current.is_infix() {
                println!("No infix operator found");
                break;
            }
            if !(current.precedence()? >= min_precedence) {
                println!("Parsing expression 1| precedence:{:?} < {:?}", current.precedence(), min_precedence);
                break; // Exit the loop if precedence is not greater
            }
            let op = self.bump().ok_or("No operator found".to_string())?;
            println!("Parsing expression 1| op:{:?}, min_precedence:{:?}", op, min_precedence);
            let mut rhs = self.primary()?;
            println!("Parsing expression 1| rhs:{:?}", rhs);

            // Handle operator precedence and associativity
            while let Some(next) = self.current_type() {
                if !next.is_infix() {
                    // No infix operator found, break
                    break;
                }
                println!("Parsing expression 1| next op:{:?}", next);
                let precedence = next.precedence()?;
                if precedence > min_precedence {
                    println!("Parsing expression 1| left associative precedence:{:?}", min_precedence+1);
                    rhs = self.parse_expression_1(rhs.clone(), min_precedence+1)?;
                } else if precedence == min_precedence && next.is_right_associative() {
                    println!("Parsing expression 1| right associative precedence:{:?}", min_precedence);
                    rhs = self.parse_expression_1(rhs.clone(), min_precedence)?;
                } else {
                    break; // Exit the loop if precedence is not greater
                }
            }

            println!("Applying infix operator: {:?} {:?} {:?}", level_lhs, op.token_type, rhs);
            level_lhs = self.apply_infix_operator(level_lhs.clone(), &op, rhs.clone())?;
        }

        println!("Parsing expression 1| End of expression 1");
        self.eat(&TokenType::Newline); // Eat anyline newline after the expression 
        // Should this be an expect()?
        Ok(level_lhs) // Return the final expression as a clone
    }

    fn apply_infix_operator(&mut self, lhs: Expression, op: &Token, rhs: Expression) -> Result<Expression, String> {
        match op.token_type {
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
                match lhs {
                    Expression::Identifier(id) => Ok(Expression::VariableAssignment { id, value: Box::new(rhs) }),
                    _ => Err(format!("Cannot assign to {:?}", lhs)),
                }
            },
            _ => Err(format!("Unsupported operator: {:?}", op.token_type)),
        }
    }

    /// prefix -> call -> literal 
    /// primary ::= '-' inner_primary | '!' inner_primary
    fn primary(&mut self) -> Result<Expression, String> {
        println!("Parsing primary| current:{:?}", self.current_type());
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
        println!("Parsing inner primary| current:{:?}", self.current_type());
        match self.current_type() {
            Some(TokenType::LParen) => self.parenthesized_expression(),
            Some(TokenType::Number(n)) => self.literal(&TokenType::Number(*n)),
            Some(TokenType::StringLiteral(s)) => self.literal(&TokenType::StringLiteral(s.to_string())),
            Some(TokenType::True) => self.literal(&TokenType::True),
            Some(TokenType::False) => self.literal(&TokenType::False),
            Some(TokenType::Identifier(_)) => self.identifier(),
            _ => Err(format!("Unexpected token: {:?}", self.current_type())),
        }
    }

    fn check_prefix_operator(&mut self) -> Option<UnaryOperator> {
        println!("Checking prefix operator| current:{:?}", self.current_type());
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
        println!("Parsing parenthesized expression| current:{:?}", self.current_type());
        self.expect(&TokenType::LParen)?;
        let primary_expr = self.primary()?; // Store the result of primary
        println!("Parenthesized | primary expression: {:?}", primary_expr);
        let expression = self.parse_expression_1(primary_expr, 0)?; // Use the stored result
        self.expect(&TokenType::RParen)?;
        Ok(expression)
    }

    /// identifier ::= IDENTIFIER | IDENTIFIER '(' parameters ')' (call)
    fn identifier(&mut self) -> Result<Expression, String> {
        println!("Parsing identifier| current:{:?}", self.current_type());
        let id = self.id()?;
        if self.eat(&TokenType::LParen) {
            println!("Function call");
            let parameters = self.arguments()?;
            self.expect(&TokenType::RParen)?;
            Ok(Expression::FunctionCall { id, arguments: parameters })
        } else {
            println!("Identifier");
            Ok(Expression::Identifier(id))
        }
    }

    fn id(&mut self) -> Result<Id, String> {
        match self.expect(&TokenType("Identifier")?)?.token_type {
            TokenType::Identifier(id_str) => Ok(Id { name: id_str }),
            _ => {
                println!("Expected identifier! found: {:?}", self.current_type());
                Err("Expected identifier".to_string())
            }
        }
    }

    /// Parse literals (String, number, boolean)
    fn literal(&mut self, token_type: &TokenType) -> Result<Expression, String> {
        println!("Parsing literal| current:{:?} expected:{:?}", self.current_type(), token_type);
       let literal = self.expect(token_type)?;
       match literal.token_type {
        TokenType::StringLiteral(s) => Ok(Expression::String(s)),
        TokenType::Number(n) => Ok(Expression::Number(n)),
        TokenType::True => Ok(Expression::Boolean(true)),
        TokenType::False => Ok(Expression::Boolean(false)),
        _ => Err(format!("Unexpected token: {:?}", token_type)),
       }
    }

    /// Parse function definitions
    /// Currently only supports standard block function definitions
    fn function(&mut self) -> Result<Expression, String> {
        println!("Parsing function| current:{:?}", self.current_type());
        self.expect(&TokenType::Function)?;
        let id = self.id()?;
        self.expect(&TokenType::LParen)?;
        let arguments = self.parameters()?;
        self.expect(&TokenType::RParen)?;
        self.expect(&TokenType::Newline)?; // TODO: Change to or '=' to support inline functions
        let body = self.block()?;
        self.expect(&TokenType::End)?;
        println!("Parsing function| End of function");
        self.expect(&TokenType::Newline); // Should these be eats or expects?
        Ok(Expression::FunctionDefinition { id, parameters: arguments, body: Box::new(body) })
    }

    // TODO: Support variant structs
    // struct <identifier>
    //     <field>
    //     <field>
    //     ...
    // end
    /// Parse struct definitions (enums not supported yet)
    fn structure(&mut self) -> Result<Expression, String> {
        println!("Parsing structure| current:{:?}", self.current_type());
        self.expect(&TokenType::Struct)?;
        let id = self.id()?;
        self.expect(&TokenType::Newline)?;
        let fields = self.field_parameters()?;
        self.expect(&TokenType::End)?;
        Ok(Expression::StructDefinition { id, fields })
    }

    /// Parse expressions until 'END' token
    fn block(&mut self) -> Result<Expression, String> {
        println!("Parsing block| Starting block: {:?}", self.current_type());
        let mut expressions = Vec::new();
        while !self.at(&TokenType::End) {
            println!("Parsing block| not end: {:?}", self.current_type());
            expressions.push(Box::new(self.parse_expression()?));
        }
        println!("Parsing block| End of block");
        Ok(Expression::Block(expressions))
    }

    // TODO: Abstract parameters, field_parameters, and arguments into Delimited(self, parse_element_fn, delim, end)

    /// Parse expressions until 'RParen' token
    fn parameters(&mut self) -> Result<Vec<Parameter>, String> {
        println!("Parsing parameters| current:{:?}", self.current_type());
       let mut parameters = Vec::new();
       while !self.at(&TokenType::RParen) {
        let id = self.id()?;
        let parameter = Parameter { id }; //, type_expr: None };
        parameters.push(parameter);
        if !(self.eat(&TokenType::Comma) ^ self.at(&TokenType::RParen)) {
            return Err("Expected comma or right parenthesis".to_string());
        }
       }
       Ok(parameters)
    }

    fn field_parameters(&mut self) -> Result<Vec<FieldDefinition>, String> {
        println!("Parsing field parameters| current:{:?}", self.current_type());
        let mut fields = Vec::new();
        while self.at(&TokenType("Identifier")?) {
            let id = self.id()?;
            //self.eat(&TokenType::Colon); // TODO: Support type annotations
            self.expect(&TokenType::Newline)?;
            let field = FieldDefinition { id }; //, type_expr: None };
            fields.push(field);
        }
        Ok(fields)
    }

    /// Parse arguments of a function call
    fn arguments(&mut self) -> Result<Vec<Expression>, String> {
        println!("Parsing arguments| current:{:?}", self.current_type());
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
        println!("Parsing if statement| current:{:?}", self.current_type());
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


    fn while_statement(&mut self) -> Result<Expression, String> {
        println!("Parsing while statement| current:{:?}", self.current_type());
        self.expect(&TokenType::While)?;
        let primary_expr = self.primary()?;
        let condition = self.parse_expression_1(primary_expr, 0)?;
        self.expect(&TokenType::Newline)?;
        let body = self.block()?;
        Ok(Expression::While { condition: Box::new(condition), body: Box::new(body) })
    }

    fn for_statement(&mut self) -> Result<Expression, String> {
        println!("Parsing for statement| current:{:?}", self.current_type());
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
        println!("Parsing return statement| current:{:?}", self.current_type());
        self.expect(&TokenType::Return)?;
        let primary_expr = self.primary()?; // Store the result of primary
        let value = self.parse_expression_1(primary_expr, 0)?;
        self.eat(&TokenType::Newline);
        println!("Parsing return statement| end of return");
        Ok(Expression::Return(Some(Box::new(value))))
    }

    fn print_statement(&mut self) -> Result<Expression, String> {
        println!("Parsing print statement| current:{:?}", self.current_type());
        self.expect(&TokenType::Print)?;
        let primary_expr = self.primary()?;
        let value = self.parse_expression_1(primary_expr, 0)?;
        self.eat(&TokenType::Newline);
        Ok(Expression::Print(Box::new(value)))
    }
}
