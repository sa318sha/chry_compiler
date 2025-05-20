use crate::token::Token;

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Type),
    // Number(i64),
    // String

    Call(Box<Expr>, Token, Box<Vec<Expr>>),

    Logical(Box<Expr>, Token, Box<Expr>),
    Set(Box<Expr>, Token, Box<Expr>),
    Get(Token, Box<Expr>),
    Variable(Token),
    Unary(Token, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Grouping(Box<Expr>),

    Empty,
    Error, // add Assign, Call, etc. later
}

#[derive(Debug, Clone)]
pub enum Type {
    Bool(bool),
    Number(i64),
    String(String),
}
