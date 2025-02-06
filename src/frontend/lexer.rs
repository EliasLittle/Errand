use std::fmt;
use std::io::Write;
use std::mem::Discriminant;
#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Number(f64),
    Identifier(String),
    Print,
    EOF,
    None,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Modulo,
    Carrot,
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,
    And,
    Ampersand,
    Or,
    Pipe,
    Bang,
    True,
    False,
    Function,
    Struct,
    Return,
    TypeInt,
    TypeBool,
    TypeVoid,
    TypeDef,
    Begin,
    End,
    If,
    Else,
    //ElseIf,
    Equal,
    NotEqual,
    Assignment,
    //PlusEqual,
    //MinusEqual,
    //AsteriskEqual,
    //SlashEqual,
    //ModuloEqual,
    //CarrotEqual,
    While,
    For,
    In,
    Comment(String),
    BlockComment(String, String, String),
    StringLiteral(String),
    Newline,
    Arrow,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Dot,
    Colon,
    Semicolon,
}

impl TokenType {
    pub fn var(&self) -> Discriminant<TokenType> {
        return std::mem::discriminant(self)
    }

    pub fn precedence(&self) -> Result<i32, String> {
        match self {
            | TokenType::Carrot => Ok(3),
            | TokenType::Asterisk 
            | TokenType::Slash 
            | TokenType::Modulo => Ok(2),
            | TokenType::Plus 
            | TokenType::Minus => Ok(1),
            | TokenType::LessThan 
            | TokenType::GreaterThan 
            | TokenType::LessThanEqual 
            | TokenType::GreaterThanEqual
            | TokenType::And 
            | TokenType::Or 
            | TokenType::Equal 
            | TokenType::NotEqual
            | TokenType::Assignment => Ok(0),
            _ => Err("Invalid token type for precedence calculation".to_string()), // Default for other token types
        }
    }

    pub fn is_right_associative(&self) -> bool {
        match self {
            TokenType::Carrot
            | TokenType::Assignment => true,
            _ => false,
        }
    }
     
    pub fn is_infix(&self) -> bool {
        match self {
            TokenType::Plus
            | TokenType::Minus
            | TokenType::Asterisk
            | TokenType::Slash
            | TokenType::Modulo
            | TokenType::Carrot
            | TokenType::And
            | TokenType::Or
            | TokenType::Pipe
            | TokenType::LessThan
            | TokenType::GreaterThan
            | TokenType::LessThanEqual
            | TokenType::GreaterThanEqual
            | TokenType::Equal
            | TokenType::NotEqual
            | TokenType::Assignment => true,
            _ => false,
        }
    }

    pub fn is_prefix(&self) -> bool {
        match self {
            TokenType::Minus
            | TokenType::Bang => true,
            _ => false,
        }
    }
    
    pub fn is_postfix(&self) -> bool {
        match self {
            _ => false,
        }
    }

    pub fn is_comment(&self) -> bool {
        match self {
            TokenType::Comment(_)
            |TokenType::BlockComment(_, _, _) => true,
            _ => false,
        }
    }
}


pub fn TokenType(token_type: &str) -> Result<TokenType, String> {
    match token_type {
        "Identifier" => Ok(TokenType::Identifier("".to_string())),
        "Number" => Ok(TokenType::Number(0.0)),
        "String" => Ok(TokenType::StringLiteral("".to_string())),
        _ => Err("Invalid token type".to_string()),
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub line: usize,
    pub column: usize,
}

impl Token{
    pub fn var(&self) -> Discriminant<TokenType> {
        self.token_type.var()
    }

    pub fn precedence(&self) -> Result<i32, String> {
        self.token_type.precedence()
    }

    pub fn is_right_associative(&self) -> bool {
        self.token_type.is_right_associative()
    }

    pub fn is_infix(&self) -> bool {
        self.token_type.is_infix()
    }

    pub fn is_prefix(&self) -> bool {
        self.token_type.is_prefix()
    }

    pub fn is_postfix(&self) -> bool {
        self.token_type.is_postfix()
    }

    pub fn is_comment(&self) -> bool {
        self.token_type.is_comment()
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<{:?}>", self.token_type)
    }
}

pub struct Lexer {
    source: String,
    tokens: Vec<Token>,
    current: usize,
    line: usize,
    column: usize,
    chars: Vec<char>,
    current_char: char,
}

impl Lexer {
    pub fn new(source: &String) -> Self {
        let chars: Vec<char> = source.chars().collect();
        let current_char = if chars.is_empty() { '\0' } else { chars[0] };
        Lexer {
            source: source.to_string(),
            tokens: Vec::new(),
            current: 0,
            line: 1,
            column: 1,
            chars,
            current_char,
        }
    }

    fn advance(&mut self) {
        self.current += 1;
        self.column += 1;
        self.current_char = if self.current >= self.chars.len() {
            '\0'
        } else {
            self.chars[self.current]
        };
    }

    fn advance_n(&mut self, n: usize) {
        self.current += n;
        self.column += n;
        self.current_char = if self.current >= self.chars.len() {
            '\0'
        } else {
            self.chars[self.current]
        };
    }

    fn peek(&self) -> char {
        if self.current + 1 >= self.chars.len() {
            '\0'
        } else {
            self.chars[self.current + 1]
        }
    }

    fn peek_n(&self, n: usize) -> &[char] {
        if self.current + n >= self.chars.len() {
            &['\0']
        } else {
            &self.chars[self.current+1..self.current + n + 1]
        }
    }

    fn is_block_comment(&self) -> bool {
        let mut char = self.current_char;
        let mut i = 0;
        while char != '\n' && char != '\0' {
            char = self.chars[self.current + i];
            i += 1;
        }
        if is_comment_end(&self.chars, self.current + i) {
            return true;
        }
        return false;
    }

    fn is_identifier_start(&self, c: char) -> bool {
        c.is_alphabetic() || c == '_'
    }

    fn is_identifier_part(&self, c: char) -> bool {
        c.is_alphanumeric() || c == '_'
    }

    fn is_end_of_file(&self) -> bool {
        self.current_char == '\0'
    }

    fn skip_whitespace(&mut self) -> Result<Token, String> {
        while self.current_char.is_whitespace() && self.current_char != '\n' {
            self.advance();
        }
        self.read_token()
    }

    fn read_multiline_comment(&mut self) -> Result<Token, String> {
        self.read_block_comment()
    }

    fn read_block_comment(&mut self) -> Result<Token, String> {
        let start_line = self.line;
        let start_column = self.column;
        let mut header = String::new();
        let mut body = String::new();
        let mut footer = String::new();
        
        self.advance_n(3); // Skip '+--'
        
        // Read the header
        while self.peek_n(3) != ['-', '-', '+'] {
            if self.current_char == '\n' || self.current_char == '\0' {
                println!("Header: {}", header);
                println!("Current char: {}", self.current_char);
                return Err("Invalid block comment: header not properly terminated.".to_string());
            }
            header.push(self.current_char);
            self.advance();
        }
        header.push(self.current_char);
        self.advance();

        self.advance_n(3); // Skip '--+'

        // Read the body
        while self.peek_n(3) != ['+', '-', '-'] {
            if self.current_char == '\0' {
                return Err("Invalid block comment: body cannot be terminated by EOF.".to_string());
            }
            if self.current_char == '\n' {
                self.line += 1;
                self.column = 0;
            }
            body.push(self.current_char);
            self.advance();
        }
        body.push(self.current_char);
        self.advance();

        self.advance_n(3); // Skip '+--'
        // Read the footer
        while self.peek_n(3) != ['-', '-', '+'] {
            if self.current_char == '\n' || self.current_char == '\0' {
                return Err("Invalid block comment: footer not properly terminated.".to_string());
            }
            footer.push(self.current_char);
            self.advance();
        }
        footer.push(self.current_char);
        self.advance();

        self.advance_n(3); // Skip '--+'
        
        Ok(Token {
            token_type: TokenType::BlockComment(header, body, footer),
            line: start_line,
            column: start_column,
        })
    }

    fn read_inline_comment(&mut self) -> Result<Token, String> {
        let start_line = self.line;
        let start_column = self.column;
        let mut comment = String::new();
        
        // Skip the opening +--
        self.advance_n(3);
        
        while self.current_char != '\n' && self.current_char != '\0' {
            comment.push(self.current_char);
            self.advance();
        }
        
        Ok(Token {
            token_type: TokenType::Comment(comment),
            line: start_line,
            column: start_column,
        })
    }

    fn read_comment(&mut self) -> Result<Token, String> {
        if self.is_block_comment() {
            return self.read_block_comment()
        } else {
            return self.read_inline_comment()
        }
    }

    fn read_number(&mut self) -> Result<Token, String> {
        let start_line = self.line;
        let start_column = self.column;
        let mut number = String::new();
        let mut has_decimal = false;
        
        while self.current_char.is_digit(10) || (self.current_char == '.' && !has_decimal) {
            if self.current_char == '.' {
                has_decimal = true;
            }
            number.push(self.current_char);
            self.advance();
        }
        
        match number.parse::<f64>() {
            Ok(n) => Ok(Token {
                token_type: TokenType::Number(n),
                line: start_line,
                column: start_column,
            }),
            Err(_) => Err(format!("Invalid number: {}", number)),
        }
    }

    fn read_identifier_or_keyword(&mut self) -> Result<Token, String> {
        let start_line = self.line;
        let start_column = self.column;
        let mut identifier = String::new();
        
        while self.is_identifier_part(self.current_char) {
            identifier.push(self.current_char);
            self.advance();
        }
        
        // Check for keywords
        let token_type = match identifier.as_str() {
            "print" => TokenType::Print,
            "true" => TokenType::True,
            "false" => TokenType::False,
            "fn" => TokenType::Function,
            "struct" => TokenType::Struct,
            "int" => TokenType::TypeInt,
            "bool" => TokenType::TypeBool,
            "void" => TokenType::TypeVoid,
            "begin" => TokenType::Begin,
            "end" => TokenType::End,
            "if" => TokenType::If,
            "else" => TokenType::Else,
            //"elseif" => TokenType::ElseIf,
            "while" => TokenType::While,
            "for" => TokenType::For,
            "in" => TokenType::In,
            "return" => TokenType::Return,
            _ => TokenType::Identifier(identifier),
        };
        
        Ok(Token {
            token_type,
            line: start_line,
            column: start_column,
        })
    }

    fn read_string(&mut self) -> Result<Token, String> {
        let start_line = self.line;
        let start_column = self.column;
        let mut string = String::new();
        
        // Skip the opening quote
        self.advance();
        
        while self.current_char != '"' && self.current_char != '\0' {
            if self.current_char == '\\' {
                self.advance();
                match self.current_char {
                    'n' => string.push('\n'),
                    't' => string.push('\t'),
                    'r' => string.push('\r'),
                    '\\' => string.push('\\'),
                    '"' => string.push('"'),
                    _ => return Err(format!("Invalid escape sequence: \\{}", self.current_char)),
                }
            } else {
                if self.current_char == '\n' {
                    self.line += 1;
                    self.column = 0;
                }
                string.push(self.current_char);
            }
            self.advance();
        }
        
        if self.current_char == '\0' {
            return Err("Unterminated string literal".to_string());
        }
        
        // Skip the closing quote
        self.advance();
        
        Ok(Token {
            token_type: TokenType::StringLiteral(string),
            line: start_line,
            column: start_column,
        })
    }

    pub fn read_token(&mut self) -> Result<Token, String> {
        let mut advance_char = true;
        let tok = match self.current_char {
            '(' => Ok(Token { token_type: TokenType::LParen, line: self.line, column: self.column }),
            ')' => Ok(Token { token_type: TokenType::RParen, line: self.line, column: self.column }),
            '{' => Ok(Token { token_type: TokenType::LBrace, line: self.line, column: self.column }),
            '}' => Ok(Token { token_type: TokenType::RBrace, line: self.line, column: self.column }),
            ',' => Ok(Token { token_type: TokenType::Comma, line: self.line, column: self.column }),
            '.' => Ok(Token { token_type: TokenType::Dot, line: self.line, column: self.column }),
            ':' => if self.peek() == ':' {
                self.advance();
                Ok(Token { token_type: TokenType::TypeDef, line: self.line, column: self.column })
            } else {
                Ok(Token { token_type: TokenType::Colon, line: self.line, column: self.column })
            },
            ';' => Ok(Token { token_type: TokenType::Semicolon, line: self.line, column: self.column }),
            '=' => if self.peek() == '=' {
                self.advance();
                Ok(Token { token_type: TokenType::Equal, line: self.line, column: self.column })
            } else {
                Ok(Token { token_type: TokenType::Assignment, line: self.line, column: self.column })
            },
            '+' => if self.peek_n(2) == ['-', '-'] {
                advance_char = false;
                self.read_comment()
            } else {
                Ok(Token { token_type: TokenType::Plus, line: self.line, column: self.column })
            },
            '-' => if self.peek() == '>' {
                self.advance();
                Ok(Token { token_type: TokenType::Arrow, line: self.line, column: self.column })
            } else {
                Ok(Token { token_type: TokenType::Minus, line: self.line, column: self.column })
            },
            '*' => Ok(Token { token_type: TokenType::Asterisk, line: self.line, column: self.column }),
            '/' => Ok(Token { token_type: TokenType::Slash, line: self.line, column: self.column }),
            '%' => Ok(Token { token_type: TokenType::Modulo, line: self.line, column: self.column }),
            '^' => Ok(Token { token_type: TokenType::Carrot, line: self.line, column: self.column }),
            '!' => if self.peek() == '=' {
                self.advance();
                Ok(Token { token_type: TokenType::NotEqual, line: self.line, column: self.column })
            } else {
                Ok(Token { token_type: TokenType::Bang, line: self.line, column: self.column })
            },
            '"' => self.read_string(),
            '\n' => Ok(Token { token_type: TokenType::Newline, line: self.line, column: self.column }),
            '<' => if self.peek() == '=' {
                self.advance();
                Ok(Token { token_type: TokenType::LessThanEqual, line: self.line, column: self.column })
            } else {
                Ok(Token { token_type: TokenType::LessThan, line: self.line, column: self.column })
            },
            '>' => if self.peek() == '=' {
                self.advance();
                Ok(Token { token_type: TokenType::GreaterThanEqual, line: self.line, column: self.column })
            } else {
                Ok(Token { token_type: TokenType::GreaterThan, line: self.line, column: self.column })
            },
            '&' => if self.peek() == '&' {
                self.advance();
                Ok(Token { token_type: TokenType::And, line: self.line, column: self.column })
            } else {
                Ok(Token { token_type: TokenType::Ampersand, line: self.line, column: self.column })
            },
            '|' => if self.peek() == '|' {
                self.advance();
                Ok(Token { token_type: TokenType::Or, line: self.line, column: self.column })
            } else {
                Ok(Token { token_type: TokenType::Pipe, line: self.line, column: self.column })
            },
            'a'..='z' | 'A'..='Z' | '_' => {
                advance_char = false;
                self.read_identifier_or_keyword()
            },
            ' ' | '\t' => {
                advance_char = false;
                self.skip_whitespace()
            },
            '0'..='9' => {
                advance_char = false;
                self.read_number()
            },
            _ => Err(format!("Unexpected character: {}", self.current_char)),
        };
        if advance_char {
            self.advance();
        }
        return tok
    }

    pub fn lex(&mut self, file_path: &str) -> Result<Vec<Token>, String> {
        println!("Starting to lex the file"); 

        // Initialize the lexer with the file contents
        self.chars = self.source.chars().collect();
        self.current = 0;
        self.line = 1;
        self.column = 1;
        self.current_char = if self.chars.is_empty() { '\0' } else { self.chars[0] };

        // Start lexing
        let mut tokens = Vec::new();
        while !self.is_end_of_file() {
            match self.read_token() {
                Ok(token) => {
                    //println!("{}", token); // Debug print statement
                    tokens.push(token);
                },
                Err(e) => return Err(e),
            }
        }
        if file_path != "" {
            let output_file_path = format!("{}.lex", file_path);
            let mut output_file = std::fs::File::create(output_file_path).expect("Unable to create file");
            for token in &tokens {
                writeln!(output_file, "{}", token).expect("Unable to write to file");
            }
        }
        Ok(tokens)
    }

    pub fn print_tokens(&self, tokens: &[Token]) {
        println!("Printing tokens...");
        for token in tokens {
            println!("{}", token);
        }
    }
} 


fn is_comment_end(chars: &[char], current: usize) -> bool {
    let char_1 = chars[current - 4];
    let char_2 = chars[current - 3];
    let char_3 = chars[current - 2];
    if char_1 == '-' && char_2 == '-' && char_3 == '+' {
        return true;
    }
    return false;
}