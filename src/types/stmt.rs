use crate::types::expr::Expr;
use crate::types::token::Token;
use crate::types::types::Type;
#[derive(Debug, Clone)]
pub enum Stmt {
    Block(Box<Vec<Stmt>>),

    Variable(Token, Option<Type>, Option<Expr>),

    Print(Box<Expr>),

    Expression(Box<Expr>),

    Function(Token, Box<Vec<(Token, Type)>>, Box<Vec<Stmt>>, Type),

    If(Box<Expr>, Box<Stmt>, Box<Option<Stmt>>),

    While(Box<Expr>, Box<Stmt>),

    Break(Token),

    Return(Token, Box<Option<Expr>>),

    Class(Token, Box<Vec<Stmt>>),
    // Error,
}
