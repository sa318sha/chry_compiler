use crate::scanner::scanner::TOKEN_MAP;
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

    pub fn dummy_with_type(token_type: TokenType) -> Token {
        return Token::new(1, token_type, "".to_string(), None);
    }

    pub fn dummy_identifier(name: &str) -> Token {
        return Token {
            line: 1,
            tokentype: TokenType::Identifier,
            lexeme: name.to_string(),
            literal: None,
        };
    }

    pub fn dummy_keyword(keyword: &str) -> Token {
        let val = TOKEN_MAP.get(keyword);
        if let Some(t) = val {
            return Token {
                line: 1,
                tokentype: t.clone(),
                lexeme: "".to_string(),
                literal: None,
            };
        }
        panic!("should never reach here cmon champ");
    }
}
