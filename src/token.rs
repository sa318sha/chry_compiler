use crate::token_type::TokenType;

#[derive(Debug, Clone)]
pub struct Token {
    pub line: u32,
    pub tokentype: TokenType,
    pub lexeme: String,
    // length: usize,
}

impl Token {
    pub fn new(_line: u32, _tokentype: TokenType, _lexeme: String) -> Self {
        return Token {
            line: _line,
            tokentype: _tokentype,
            lexeme: _lexeme,
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
