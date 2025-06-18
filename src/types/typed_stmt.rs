use super::typed_expr::TypedExpr;
use crate::types::{token::Token, types::Type};

// dont need this indirection to store a type for a statement because statements dont have an inherent type
// #[derive(Debug, Clone)]
// pub struct TypedStmt {
//     pub kind: TypedStmtKind,

//     // pub ty: Type, // This should be the actual resolved type
// }

#[derive(Debug, Clone)]
pub enum TypedStmt {
    Block(Box<Vec<TypedStmt>>),

    Variable(Token, Option<Type>, Option<TypedExpr>),

    Print(Box<TypedExpr>),

    Expression(Box<TypedExpr>),

    Function(
        Token,
        Box<Vec<(Token, Type)>>,
        Box<Vec<TypedStmt>>,
        Type,
    ),

    If(Box<TypedExpr>, Box<TypedStmt>, Box<Option<TypedStmt>>),

    While(Box<TypedExpr>, Box<TypedStmt>),

    Break(Token),

    Return(Token, Box<Option<TypedExpr>>),

    Class(Token, Box<Vec<TypedStmt>>),
}
