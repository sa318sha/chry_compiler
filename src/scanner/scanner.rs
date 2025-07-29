use super::scan_error::ScanError;
use crate::types::{literal::Literal, token::Token, token_type::TokenType};
use once_cell::sync::Lazy;
use std::collections::HashMap;
// use std::mem;

type ScanResult<T> = Result<T, ScanError>;

pub static TOKEN_MAP: Lazy<HashMap<&'static str, TokenType>> = Lazy::new(|| {
    let mut keywords = HashMap::new();
    keywords.insert("false", TokenType::False);
    keywords.insert("void", TokenType::Void);

    keywords.insert("fun", TokenType::Fun);
    keywords.insert("for", TokenType::For);
    keywords.insert("class", TokenType::Class);
    keywords.insert("else", TokenType::Else);
    keywords.insert("if", TokenType::If);
    keywords.insert("nil", TokenType::Nil);
    keywords.insert("return", TokenType::Return);
    keywords.insert("super", TokenType::Super);
    keywords.insert("this", TokenType::This);
    keywords.insert("true", TokenType::True);
    keywords.insert("var", TokenType::Var);
    keywords.insert("while", TokenType::While);
    keywords.insert("break", TokenType::Break);
    keywords.insert("print", TokenType::Print);

    keywords.insert("int", TokenType::Int);
    keywords.insert("float", TokenType::Float);
    // keywords.insert("double", TokenType::Double);
    keywords.insert("bool", TokenType::Bool);

    return keywords;
});

// pub fn scan(source: &str) -> Vec<Token> {
//     let mut scanner = Scanner::new(source);
//     scanner.scan_tokens()
// }

#[derive(Debug)]
pub struct Scanner {
    line: u32,
    start: usize,
    current: usize,
    end: usize,
    tokens: Vec<Token>,
    source: String,
    pub errors: Vec<ScanError>,
}

impl Scanner {
    pub fn new(source_file: &str) -> Self {
        Scanner {
            line: 1,
            start: 0,
            current: 0,
            end: 0,
            tokens: Vec::new(),
            source: source_file.to_string(),
            errors: Vec::new(),
        }
    }

    pub fn scan_tokens(&mut self) -> Vec<Token> {
        while !self.is_at_end() {
            self.start = self.current;
            if let Err(_res) = self.scan_token() {
                println!("{}", _res.format_log());

                self.errors.push(_res);
            }
        }

        self.start = self.current; // hard coded to move the start the end condition on EOF
        self.add_token(TokenType::Eof);

        return self.tokens.clone();
    }

    pub fn list_tokens(&self) {
        for token in &self.tokens {
            token.print_token();
        }
    }

    fn advance(&mut self) -> ScanResult<char> {
        let mut chars = self.source[self.current..].char_indices();

        let x = chars.next();
        if let Some((i, ch)) = x {
            self.current += ch.len_utf8(); // Move past this character
            return Ok(ch);
        } else {
            return Err(ScanError::InvalidCharacterStream {
                line: self.line,
                context: "expected char when scanning".to_string(),
            });
        }
    }

    // fn error(&mut self, message: &str) -> ScanError {
    //     return (ScanError {
    //         line: self.line,
    //         message: message.to_string(),
    //     });
    // }

    fn is_at_end(&self) -> bool {
        return self.current >= self.source.len();
    }

    fn number(&mut self) -> ScanResult<()> {
        while self.peek().is_ascii_digit() {
            self.advance()?;
        }
        let mut is_double = false;
        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            is_double = true;
            // Consume the "."
            self.advance()?;
            while self.peek().is_ascii_digit() {
                self.advance()?;
            }
        }

        let text: &str = &self.source[self.start..self.current];

        if (is_double) {
            match text.parse::<f64>() {
                Ok(n) => self.add_token_literal(
                    TokenType::FloatLiteral,
                    Some(Literal::Float(n, text.to_string())),
                ),
                Err(e) => {
                    return Err(ScanError::NumericParseError {
                        value: text.to_string(),
                        line: self.line,
                        context: format!("Failed to Scan: {}", e),
                    });
                }
            }
        } else {
            match text.parse::<i32>() {
                Ok(n) => self.add_token_literal(TokenType::IntLiteral, Some(Literal::Int(n))),
                Err(e) => {
                    return Err(ScanError::NumericParseError {
                        value: text.to_string(),
                        line: self.line,
                        context: format!("Failed to Scan: {}", e),
                    });
                }
            }
        }
        return Ok(());
        // let text: &str = &self.source[self.start..self.current];
    }
    fn identifier(&mut self) -> ScanResult<()> {
        while {
            let c = self.peek();
            c.is_alphanumeric() || c == '_'
        } {
            self.advance()?;
        }
        let text: &str = &self.source[self.start..self.current];

        let t = TOKEN_MAP.get(text);
        match t {
            None => self.add_token(TokenType::Identifier),
            Some(token) => match token {
                TokenType::True => self.add_token_literal(token.clone(), Some(Literal::Bool(true))),
                TokenType::False => {
                    self.add_token_literal(token.clone(), Some(Literal::Bool(false)))
                }

                _ => self.add_token(token.clone()),
            },
        }

        Ok(())
    }
    fn match_char(&mut self, c: char) -> bool {
        if (self.is_at_end()) {
            return false;
        }
        let a = self.source.chars().nth(self.current);
        match a {
            Some(_c) => {
                if c == _c {
                    self.current += 1;
                    return true;
                } else {
                    return false;
                }
            }
            None => {
                return false;
            }
        }
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }
        let mut chars = self.source[self.current..].chars();
        // let (i, ch) = chars.().expect("Unexpected end of input");
        return chars.next().unwrap();
    }

    fn peek_next(&mut self) -> char {
        if self.current + 1 >= self.source.len() {
            return '\0';
        }
        return self.source.chars().nth(self.current + 1).unwrap();
    }

    fn scan_token(&mut self) -> ScanResult<()> {
        let c: char = self.advance()?;

        match c {
            ':' => self.add_token(TokenType::Colon),
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '{' => self.add_token(TokenType::LeftBrace),
            '}' => self.add_token(TokenType::RightBrace),
            ',' => self.add_token(TokenType::Comma),
            '.' => self.add_token(TokenType::Dot),
            '-' => {
                if self.match_char('>') {
                    self.add_token(TokenType::Arrow);
                } else {
                    self.add_token(TokenType::Minus);
                }
            }
            '+' => self.add_token(TokenType::Plus),
            ';' => self.add_token(TokenType::Semicolon),
            '*' => self.add_token(TokenType::Star),

            '!' => {
                if self.match_char('=') {
                    self.add_token(TokenType::BangEqual);
                } else {
                    self.add_token(TokenType::Bang);
                }
            }
            '=' => {
                if self.match_char('=') {
                    self.add_token(TokenType::EqualEqual);
                } else {
                    self.add_token(TokenType::Equal);
                }
            }
            '<' => {
                if self.match_char('=') {
                    self.add_token(TokenType::LessEqual);
                } else {
                    self.add_token(TokenType::Less);
                }
            }
            '>' => {
                if self.match_char('=') {
                    self.add_token(TokenType::GreaterEqual);
                } else {
                    self.add_token(TokenType::Greater);
                }
            }

            '&' => {
                if self.match_char('&') {
                    self.add_token(TokenType::And);
                } else {
                    self.add_token(TokenType::BitWiseAnd);
                }
            }

            '|' => {
                if self.match_char('|') {
                    self.add_token(TokenType::Or);
                } else {
                    self.add_token(TokenType::BitWiseOr);
                }
            }
            '/' => {
                if self.match_char('/') {
                    // Single-line comment
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance()?;
                    }
                } else if self.match_char('*') {
                    // Multi-line comment
                    while !self.is_at_end() {
                        if self.peek() == '*' && self.peek_next() == '/' {
                            self.advance()?; // consume '*'
                            self.advance()?; // consume '/'
                            break;
                        }
                        if self.peek() == '\n' {
                            self.line += 1;
                        }
                        self.advance()?;
                    }
                } else {
                    self.add_token(TokenType::Slash);
                }
            }

            // Whitespace
            ' ' | '\r' | '\t' => {} // Ignore
            '\n' => self.line += 1,

            '"' => self.string()?,

            _ => {
                if c.is_ascii_digit() {
                    self.number()?;
                } else if c.is_ascii_alphabetic() || c == '_' {
                    self.identifier()?;
                } else {
                    return Err(ScanError::InvalidCharacterStream {
                        line: self.line,
                        context: format!("unexpected character {}", c).to_string(),
                    });
                    // Lox::error(self.line, "Unexpected character.");
                }
            }
        }
        return Ok(());
    }

    fn string(&mut self) -> ScanResult<()> {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance()?;
        }
        if self.is_at_end() {
            return Err(ScanError::UnterminatedString {
                value: self.source[self.start + 1..self.current - 1].to_string(),
                line: self.line,
                context: "expected character '\"'".to_string(),
            });
        }
        // The closing ".
        self.advance()?;
        // Trim the surrounding quotes.
        let text: &str = &self.source[self.start + 1..self.current - 1];

        self.add_token_literal(
            TokenType::StringLiteral,
            Some(Literal::String(String::from(text))),
        );
        return Ok(());
    }

    fn add_token_literal(&mut self, token: TokenType, literal: Option<Literal>) {
        let text: &str = &self.source[self.start..self.current];
        let t = Token::new(self.line, token, String::from(text), literal);

        self.tokens.push(t);
    }

    fn add_token(&mut self, token: TokenType) {
        // self.add_token_literal(token, null);
        let text: &str = &self.source[self.start..self.current];
        let t = Token::new(self.line, token, text.to_string(), None);

        self.tokens.push(t);
    }

    // fn add_token_lexeme(&mut self, token: TokenType, text: &str) {
    //     let t = Token::new(self.line, token, String::from(text));

    //     self.tokens.push(t);
    // }
}
