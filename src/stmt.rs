use crate::expr::Expr;
use crate::token::Token;

#[derive(Debug, Clone)]
pub enum Stmt {
    Block(Box<Vec<Stmt>>),
    Variable(Token, Box<Expr>),
    Print(Box<Expr>),
    Expression(Box<Expr>),
    Function(Token, Box<Vec<Token>>, Box<Vec<Stmt>>),
    If(Box<Expr>, Box<Stmt>, Box<Stmt>),
    While(Box<Expr>, Box<Stmt>),
    // For(Box<Stmt>, Box<Expr>, Box<Expr>),
    Break(Token),
    Return(Token, Box<Expr>),
    Class(Token, Box<Vec<Stmt>>),

    Empty,
    Error,
}
