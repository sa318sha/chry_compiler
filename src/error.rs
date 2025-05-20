// use
use crate::expr::Expr;
use crate::token::Token;
pub enum ParseError {
    NotFound,
    InvalidInput,
    ErrorMsg(String),
    ParseToken(Token, String),
}

pub type ParseResult = Result<Expr, ParseError>;
