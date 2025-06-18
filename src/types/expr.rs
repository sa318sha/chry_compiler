use crate::types::literal::Literal;
use crate::types::token::Token;
use crate::types::types::Type;
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal),

    Call(Box<Expr>, Token, Box<Vec<Expr>>),

    Logical(Box<Expr>, Token, Box<Expr>),
    Set(Box<Expr>, Token, Box<Expr>),
    Get(Token, Box<Expr>),
    Variable(Token),
    Unary(Token, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Grouping(Box<Expr>),
    // add Assign, Call, etc. later
}

// #[derive(Debug, Clone)]
impl Literal {
    pub fn get_type(&self) -> Type {
        match self {
            Literal::Bool(_) => Type::Bool,
            Literal::Int(_) => Type::Int,
            Literal::String(_) => Type::String,
            Literal::Float(_) => Type::Float, // if supported
            Literal::Nil => Type::Nil,

            _ => todo!("Other literal types"),
        }
    }
}
