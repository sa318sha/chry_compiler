use crate::types::literal::Literal;
use crate::types::token_type::TokenType;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub line: u32,
    pub tokentype: TokenType,
    pub lexeme: String,
    pub literal: Option<Literal>, // length: usize,
}

impl Token {
    pub fn new(
        _line: u32,
        _tokentype: TokenType,
        _lexeme: String,
        literal: Option<Literal>,
    ) -> Self {
        return Token {
            line: _line,
            tokentype: _tokentype,
            lexeme: _lexeme,
            literal: literal,
            // length: _length,
        };
    }

    pub fn print_token(&self) {
        println!(
            "TokenType: {:?}\t Lexemme: {:?}\t line: {:?}\t ",
            self.tokentype, self.lexeme, self.line
        );
    }

    pub fn get_token_type(&self) -> TokenType {
        return self.tokentype.clone();
    }
}
