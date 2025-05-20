use crate::token::Token;
use crate::token_type::TokenType;
use once_cell::sync::Lazy;
use std::collections::HashMap;
use std::mem;

static TOKEN_MAP: Lazy<HashMap<&'static str, TokenType>> = Lazy::new(|| {
    let mut keywords = HashMap::new();
    keywords.insert("false", TokenType::False);
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
    keywords.insert("int", TokenType::Int);
    keywords.insert("float", TokenType::Float);
    keywords.insert("double", TokenType::Double);
    keywords.insert("bool", TokenType::Bool);

    return keywords;
});

#[derive(Debug)]
pub struct Scanner {
    line: u32,
    start: usize,
    current: usize,
    end: usize,
    tokens: Vec<Token>,
    source: String,
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
        }
    }

    pub fn list_tokens(&self) {
        for token in &self.tokens {
            token.print_token();
        }
    }

    fn advance(&mut self) -> char {
        let mut chars = self.source[self.current..].char_indices();
        let (i, ch) = chars.next().expect("Unexpected end of input");

        self.current += ch.len_utf8(); // Move past this character

        return ch;
    }

    fn is_at_end(&self) -> bool {
        return self.current >= self.source.len();
    }

    fn number(&mut self) {
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            // Consume the "."
            self.advance();
            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }
        let text: &str = &self.source[self.start..self.current];

        match text.parse::<i32>() {
            Ok(n) => self.add_token(TokenType::Number(n.into())),
            Err(e) => println!("Failed to parse: {}", e),
        }

        // let text: &str = &self.source[self.start..self.current];
    }
    fn identifier(&mut self) {
        while self.peek().is_alphanumeric() {
            self.advance();
        }
        let text: &str = &self.source[self.start..self.current];

        let t = TOKEN_MAP.get(text);
        match t {
            None => self.add_token(TokenType::Identifier(String::from(text))),
            Some(token) => self.add_token(token.clone()),
        }
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
        return '\0';
    }

    fn scan_token(&mut self) {
        let c: char = self.advance();

        match c {
            ':' => self.add_token(TokenType::Colon),
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '{' => self.add_token(TokenType::LeftBrace),
            '}' => self.add_token(TokenType::RightBrace),
            ',' => self.add_token(TokenType::Comma),
            '.' => self.add_token(TokenType::Dot),
            '-' => self.add_token(TokenType::Minus),
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
            '/' => {
                if self.match_char('/') {
                    // Single-line comment
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else if self.match_char('*') {
                    // Multi-line comment
                    while !self.is_at_end() {
                        if self.peek() == '*' && self.peek_next() == '/' {
                            self.advance(); // consume '*'
                            self.advance(); // consume '/'
                            break;
                        }
                        if self.peek() == '\n' {
                            self.line += 1;
                        }
                        self.advance();
                    }
                } else {
                    self.add_token(TokenType::Slash);
                }
            }

            // Whitespace
            ' ' | '\r' | '\t' => {} // Ignore
            '\n' => self.line += 1,

            '"' => self.string(),

            _ => {
                if c.is_ascii_digit() {
                    self.number();
                } else if c.is_ascii_alphabetic() || c == '_' {
                    self.identifier();
                } else {
                    // Lox::error(self.line, "Unexpected character.");
                }
            }
        }
    }

    fn string(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }
        if self.is_at_end() {
            // Lox.error(line, "Unterminated string.");
            return;
        }
        // The closing ".
        self.advance();
        // Trim the surrounding quotes.
        let text: &str = &self.source[self.start + 1..self.current - 1];

        self.add_token(TokenType::String(String::from(text)));
    }

    pub fn scan_tokens(&mut self) -> Vec<Token> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.add_token(TokenType::Eof);

        return self.tokens.clone();
    }

    fn add_token(&mut self, token: TokenType) {
        // self.add_token_literal(token, null);
        let text: &str = &self.source[self.start..self.current];
        let t = Token::new(self.line, token, String::from(text));

        self.tokens.push(t);
    }

    // fn add_token_lexeme(&mut self, token: TokenType, text: &str) {
    //     let t = Token::new(self.line, token, String::from(text));

    //     self.tokens.push(t);
    // }
}
