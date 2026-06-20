use std::fmt;
use std::io::Write;
use std::mem::Discriminant;

use tracing::instrument;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Int(i64),
    Float(f64),
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
    Foreign,
    Struct,
    Enum,
    Return,
    TypeInt,
    TypeBool,
    TypeVoid,
    TypeDef,
    Subtype,
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
    Match,
    FatArrow,
    Comment(String),
    BlockComment(String, String, String),
    StringLiteral(String),
    Newline,
    Arrow,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    With,
    Comma,
    Dot,
    Colon,
    Semicolon,
}

impl TokenType {
    pub fn var(&self) -> Discriminant<TokenType> {
        std::mem::discriminant(self)
    }

    // Higher precedence means the token is evaluated first
    pub fn precedence(&self) -> Result<i32, String> {
        match self {
            TokenType::Dot => Ok(4),
            TokenType::Carrot => Ok(3),
            TokenType::Asterisk | TokenType::Slash | TokenType::Modulo => Ok(2),
            TokenType::Plus | TokenType::Minus => Ok(1),
            TokenType::LessThan
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
            TokenType::Carrot | TokenType::Assignment => true,
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
            | TokenType::Dot
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
            TokenType::Minus | TokenType::Bang => true,
            _ => false,
        }
    }

    /// Reserved for future postfix operators.
    pub fn is_postfix(&self) -> bool {
        false
    }

    pub fn is_comment(&self) -> bool {
        match self {
            TokenType::Comment(_) | TokenType::BlockComment(_, _, _) => true,
            _ => false,
        }
    }
}

pub fn token_type(token_type: &str) -> Result<TokenType, String> {
    match token_type {
        "Identifier" => Ok(TokenType::Identifier("".to_string())),
        "Int" => Ok(TokenType::Int(0)),
        "Float" => Ok(TokenType::Float(0.0)),
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

impl Token {
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
    chars: Vec<char>,
    current: usize,
    line: usize,
    column: usize,
    current_char: char,
}

impl Lexer {
    /// Takes ownership of the source text and expands it to UTF-32 scalars once.
    /// Pass the `String` from `read_to_string` directly to avoid an extra full-buffer copy.
    #[instrument(
        skip_all,
        fields(source_len = tracing::field::Empty),
        name = "lexer.new",
        target = "lexer",
        level = "trace"
    )]
    pub fn new(source: impl Into<String>) -> Self {
        let chars: Vec<char> = source.into().chars().collect();
        tracing::Span::current().record("source_len", chars.len());
        let current_char = if chars.is_empty() { '\0' } else { chars[0] };
        Lexer {
            chars,
            current: 0,
            line: 1,
            column: 1,
            current_char,
        }
    }

    /// Fills `#[instrument(..., fields(... = Empty))]` slots for the active span (Chrome/Perfetto `args`).
    fn record_lex_cursor(&self) {
        let span = tracing::Span::current();
        span.record("line", self.line);
        span.record("column", self.column);
        span.record("offset", self.current);
        span.record("at", tracing::field::debug(&self.current_char));
    }

    fn advance(&mut self) {
        let prev = self.current_char;
        self.current += 1;
        if prev == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        self.current_char = if self.current >= self.chars.len() {
            '\0'
        } else {
            self.chars[self.current]
        };
    }

    fn advance_n(&mut self, n: usize) {
        for _ in 0..n {
            if self.current >= self.chars.len() {
                break;
            }
            self.advance();
        }
    }

    fn peek(&self) -> char {
        if self.current + 1 >= self.chars.len() {
            '\0'
        } else {
            self.chars[self.current + 1]
        }
    }

    /// Lookahead of up to `n` characters after the current position. Shorter
    /// when near EOF; compare to `[char; n]` only when you need exactly `n`
    /// characters (length mismatch implies “not equal”).
    fn peek_n(&self, n: usize) -> &[char] {
        let start = self.current + 1;
        if start >= self.chars.len() {
            return &[];
        }
        let end = (start + n).min(self.chars.len());
        &self.chars[start..end]
    }

    fn is_block_comment(&self) -> bool {
        let mut ch = self.current_char;
        let mut i = 0;
        while ch != '\n' && ch != '\0' {
            ch = self.chars[self.current + i];
            i += 1;
        }
        is_comment_end(&self.chars, self.current + i)
    }

    fn is_identifier_start(&self, c: char) -> bool {
        matches!(c, 'a'..='z' | 'A'..='Z' | '_')
    }

    fn is_identifier_part(&self, c: char) -> bool {
        c.is_alphanumeric() || c == '_'
    }

    fn is_end_of_file(&self) -> bool {
        self.current_char == '\0'
    }

    #[instrument(
        skip(self),
        fields(
            line = tracing::field::Empty,
            column = tracing::field::Empty,
            offset = tracing::field::Empty,
            at = tracing::field::Empty
        ),
        name = "lexer.skip_whitespace",
        target = "lexer",
        level = "trace"
    )]
    fn skip_whitespace(&mut self) -> Result<Token, String> {
        self.record_lex_cursor();
        while self.current_char.is_whitespace() && self.current_char != '\n' {
            self.advance();
        }
        self.read_token()
    }

    #[instrument(
        skip(self),
        fields(
            line = tracing::field::Empty,
            column = tracing::field::Empty,
            offset = tracing::field::Empty,
            at = tracing::field::Empty
        ),
        name = "lexer.read_block_comment",
        target = "lexer",
        level = "trace"
    )]
    fn read_block_comment(&mut self) -> Result<Token, String> {
        self.record_lex_cursor();
        let start_line = self.line;
        let start_column = self.column;
        let mut header = String::new();
        let mut body = String::new();
        let mut footer = String::new();

        self.advance_n(3); // Skip '+--'

        // Read the header
        while self.peek_n(3) != ['-', '-', '+'] {
            if self.current_char == '\n' || self.current_char == '\0' {
                tracing::warn!(
                    %header,
                    line = self.line,
                    column = self.column,
                    at = %self.current_char,
                    "invalid block comment: header not terminated"
                );
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

    #[instrument(
        skip(self),
        fields(
            line = tracing::field::Empty,
            column = tracing::field::Empty,
            offset = tracing::field::Empty,
            at = tracing::field::Empty
        ),
        name = "lexer.read_inline_comment",
        target = "lexer",
        level = "trace"
    )]
    fn read_inline_comment(&mut self) -> Result<Token, String> {
        self.record_lex_cursor();
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

    #[instrument(
        skip(self),
        fields(
            line = tracing::field::Empty,
            column = tracing::field::Empty,
            offset = tracing::field::Empty,
            at = tracing::field::Empty,
            block = tracing::field::Empty
        ),
        name = "lexer.read_comment",
        target = "lexer",
        level = "trace"
    )]
    fn read_comment(&mut self) -> Result<Token, String> {
        self.record_lex_cursor();
        let block = self.is_block_comment();
        tracing::Span::current().record("block", block);
        if block {
            self.read_block_comment()
        } else {
            self.read_inline_comment()
        }
    }

    #[instrument(
        skip(self),
        fields(
            line = tracing::field::Empty,
            column = tracing::field::Empty,
            offset = tracing::field::Empty,
            at = tracing::field::Empty
        ),
        name = "lexer.read_number",
        target = "lexer",
        level = "trace"
    )]
    fn read_number(&mut self) -> Result<Token, String> {
        self.record_lex_cursor();
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

        if has_decimal {
            match number.parse::<f64>() {
                Ok(n) => Ok(Token {
                    token_type: TokenType::Float(n),
                    line: start_line,
                    column: start_column,
                }),
                Err(_) => Err(format!("Invalid float: {}", number)),
            }
        } else {
            match number.parse::<i64>() {
                Ok(n) => Ok(Token {
                    token_type: TokenType::Int(n),
                    line: start_line,
                    column: start_column,
                }),
                Err(_) => Err(format!("Invalid int: {}", number)),
            }
        }
    }

    #[instrument(
        skip(self),
        fields(
            line = tracing::field::Empty,
            column = tracing::field::Empty,
            offset = tracing::field::Empty,
            at = tracing::field::Empty
        ),
        name = "lexer.read_identifier_or_keyword",
        target = "lexer",
        level = "trace"
    )]
    fn read_identifier_or_keyword(&mut self) -> Result<Token, String> {
        self.record_lex_cursor();
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
            "foreign" => TokenType::Foreign,
            "struct" => TokenType::Struct,
            "enum" => TokenType::Enum,
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
            "match" => TokenType::Match,
            "with" => TokenType::With,
            "return" => TokenType::Return,
            _ => TokenType::Identifier(identifier),
        };

        Ok(Token {
            token_type,
            line: start_line,
            column: start_column,
        })
    }

    #[instrument(
        skip(self),
        fields(
            line = tracing::field::Empty,
            column = tracing::field::Empty,
            offset = tracing::field::Empty,
            at = tracing::field::Empty
        ),
        name = "lexer.read_string",
        target = "lexer",
        level = "trace"
    )]
    fn read_string(&mut self) -> Result<Token, String> {
        self.record_lex_cursor();
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

    #[inline]
    fn ok_token(&self, token_type: TokenType) -> Result<Token, String> {
        Ok(Token {
            token_type,
            line: self.line,
            column: self.column,
        })
    }

    #[instrument(
        skip(self),
        fields(
            line = tracing::field::Empty,
            column = tracing::field::Empty,
            offset = tracing::field::Empty,
            at = tracing::field::Empty
        ),
        name = "lexer.read_token",
        target = "lexer",
        level = "trace"
    )]
    pub fn read_token(&mut self) -> Result<Token, String> {
        self.record_lex_cursor();
        let mut advance_char = true;
        let tok = match self.current_char {
            '(' => self.ok_token(TokenType::LParen),
            ')' => self.ok_token(TokenType::RParen),
            '{' => self.ok_token(TokenType::LBrace),
            '}' => self.ok_token(TokenType::RBrace),
            '[' => self.ok_token(TokenType::LBracket),
            ']' => self.ok_token(TokenType::RBracket),
            ',' => self.ok_token(TokenType::Comma),
            '.' => self.ok_token(TokenType::Dot),
            ':' => {
                if self.peek() == ':' {
                    self.advance();
                    self.ok_token(TokenType::TypeDef)
                } else {
                    self.ok_token(TokenType::Colon)
                }
            }
            ';' => self.ok_token(TokenType::Semicolon),
            '=' => {
                if self.peek() == '=' {
                    self.advance();
                    self.ok_token(TokenType::Equal)
                } else if self.peek() == '>' {
                    self.advance();
                    self.ok_token(TokenType::FatArrow)
                } else {
                    self.ok_token(TokenType::Assignment)
                }
            }
            '+' => {
                if self.peek_n(2) == ['-', '-'] {
                    advance_char = false;
                    self.read_comment()
                } else {
                    self.ok_token(TokenType::Plus)
                }
            }
            '-' => {
                if self.peek() == '>' {
                    self.advance();
                    self.ok_token(TokenType::Arrow)
                } else {
                    self.ok_token(TokenType::Minus)
                }
            }
            '*' => self.ok_token(TokenType::Asterisk),
            '/' => self.ok_token(TokenType::Slash),
            '%' => self.ok_token(TokenType::Modulo),
            '^' => self.ok_token(TokenType::Carrot),
            '!' => {
                if self.peek() == '=' {
                    self.advance();
                    self.ok_token(TokenType::NotEqual)
                } else {
                    self.ok_token(TokenType::Bang)
                }
            }
            '"' => {
                advance_char = false;
                self.read_string()
            }
            '\n' => self.ok_token(TokenType::Newline),
            '<' => match self.peek() {
                ':' => {
                    self.advance();
                    self.ok_token(TokenType::Subtype)
                }
                '=' => {
                    self.advance();
                    self.ok_token(TokenType::LessThanEqual)
                }
                _ => self.ok_token(TokenType::LessThan),
            },
            '>' => {
                if self.peek() == '=' {
                    self.advance();
                    self.ok_token(TokenType::GreaterThanEqual)
                } else {
                    self.ok_token(TokenType::GreaterThan)
                }
            }
            '&' => {
                if self.peek() == '&' {
                    self.advance();
                    self.ok_token(TokenType::And)
                } else {
                    self.ok_token(TokenType::Ampersand)
                }
            }
            '|' => {
                if self.peek() == '|' {
                    self.advance();
                    self.ok_token(TokenType::Or)
                } else {
                    self.ok_token(TokenType::Pipe)
                }
            }
            c if self.is_identifier_start(c) => {
                advance_char = false;
                self.read_identifier_or_keyword()
            }
            ' ' | '\t' => {
                advance_char = false;
                self.skip_whitespace()
            }
            '0'..='9' => {
                advance_char = false;
                self.read_number()
            }
            '\0' => self.ok_token(TokenType::EOF),
            _ => Err(format!("Unexpected character: {}", self.current_char)),
        };
        if advance_char {
            self.advance();
        }
        tok
    }

    #[instrument(
        skip(self),
        fields(
            file = %file_path,
            line = tracing::field::Empty,
            column = tracing::field::Empty,
            offset = tracing::field::Empty,
            at = tracing::field::Empty,
            source_len = tracing::field::Empty
        ),
        name = "lexer.lex",
        target = "lexer",
        level = "info"
    )]
    pub fn lex(&mut self, file_path: &str) -> Result<Vec<Token>, String> {
        // Reset scan position (buffer unchanged; `lex` may be called again).
        self.current = 0;
        self.line = 1;
        self.column = 1;
        self.current_char = if self.chars.is_empty() {
            '\0'
        } else {
            self.chars[0]
        };

        let span = tracing::Span::current();
        span.record("source_len", self.chars.len());
        self.record_lex_cursor();

        // Start lexing
        let mut tokens = Vec::new();
        while !self.is_end_of_file() {
            match self.read_token() {
                Ok(token) => {
                    tracing::trace!(token = %token, "lex: token");
                    tokens.push(token);
                }
                Err(e) => return Err(e),
            }
        }
        if !file_path.is_empty() {
            let output_file_path = if let Some(stripped) = file_path.strip_suffix(".err") {
                format!("{}.lex", stripped)
            } else {
                format!("{}.lex", file_path)
            };
            let mut output_file = std::fs::File::create(&output_file_path)
                .map_err(|e| format!("Unable to create .lex file {output_file_path}: {e}"))?;
            for token in &tokens {
                writeln!(output_file, "{}", token)
                    .map_err(|e| format!("Unable to write .lex file {output_file_path}: {e}"))?;
            }
        }
        Ok(tokens)
    }

    /// Debug helper: not used on the normal compile path; only traces when called.
    #[instrument(
        skip(self),
        fields(
            count = tokens.len(),
            line = tracing::field::Empty,
            column = tracing::field::Empty,
            offset = tracing::field::Empty,
            at = tracing::field::Empty
        ),
        name = "lexer.print_tokens",
        target = "lexer",
        level = "trace"
    )]
    pub fn print_tokens(&self, tokens: &[Token]) {
        self.record_lex_cursor();
        for token in tokens {
            tracing::trace!(token = %token, "print_tokens");
        }
    }
}

fn is_comment_end(chars: &[char], current: usize) -> bool {
    if current < 4 {
        return false;
    }
    chars[current - 4] == '-' && chars[current - 3] == '-' && chars[current - 2] == '+'
}
