// use
// use crate::types::expr::Expr;
// use crate::types::types::Type;
use crate::types::token::Token;
// use crate

#[derive(Debug)]
pub enum ParseError {
    NotFound,
    InvalidInput,
    ErrorMsg(String),
    ParseToken(Token, String),
}



// pub type ParseResult = Result<Expr, ParseError>;
