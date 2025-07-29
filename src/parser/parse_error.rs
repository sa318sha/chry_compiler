use crate::types::{token::Token, token_type::TokenType};

#[derive(Debug, Clone)]
pub enum ParseError {
    UnexpectedToken {
        token: Token,
        expected: Vec<TokenType>,
        message: Option<String>,
    },
    UnexpectedEof {
        token: Token,
        expected: Vec<TokenType>,
    },
    InvalidLiteral {
        token: Token,
        reason: String,
    },
    InvalidType {
        token: Token,
        reason: String,
    },
    UnterminatedBlock {
        start: Token,
        end: Token,
    },
    Custom {
        token: Option<Token>,
        message: String,
    },
    InvalidVariableDeclaration {
        token: Token,
        message: String,
    },

}

impl ParseError {
    pub fn to_string_message(&self) -> String {
        match self {
            ParseError::UnexpectedToken {
                token,
                expected,
                message,
            } => {
                let extra = message.as_deref().unwrap_or("");
                format!(
                    "[line {}] Unexpected token '{}', expected {:?}. {}",
                    token.line, token.lexeme, expected, extra
                )
            }
            ParseError::UnexpectedEof { token, expected } => {
                format!(
                    "[line {}] Unexpected end of file, expected {:?}, got '{}'.",
                    token.line, expected, token.lexeme
                )
            }
            ParseError::InvalidLiteral { token, reason } => {
                format!(
                    "[line {}] Invalid literal '{}'. {}",
                    token.line, token.lexeme, reason
                )
            }
            ParseError::InvalidType { token, reason } => {
                format!(
                    "[line {}] Invalid type '{:#?}'. {}",
                    token.line, token.get_token_type(), reason
                )
            }
            ParseError::UnterminatedBlock { start, end} => {
                format!(
                    "[line {}] start of Unterminated block ends line: {}",
                    start.line, end.line
                )
            }
            ParseError::Custom { token, message } => {
                if let Some(tok) = token {
                    format!("[line {}] Error at '{}': {}", tok.line, tok.lexeme, message)
                } else {
                    format!("Error: {}", message)
                }
            }
            ParseError::InvalidVariableDeclaration { token, message } => {
                format!(
                    "[line {}] Invalid variable declaration '{}': {}",
                    token.line, token.lexeme, message
                )
            }
        }
    }
}
