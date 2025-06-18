use crate::types::{token::Token, types::Type};


#[derive(Debug, Clone)]
pub struct TypedExpr {
    pub kind: TypedExprKind,
    pub ty: Type, // This should be the actual resolved type
}

#[derive(Debug, Clone)]
pub enum TypedExprKind {
    Literal(Type),

    Call(Box<TypedExpr>, Token, Vec<TypedExpr>),

    Logical(Box<TypedExpr>, Token, Box<TypedExpr>),
    Set(Box<TypedExpr>, Token, Box<TypedExpr>),
    Get(Token, Box<TypedExpr>),
    Variable(Token),
    Unary(Token, Box<TypedExpr>),
    Binary(Box<TypedExpr>, Token, Box<TypedExpr>),
    Grouping(Box<TypedExpr>),

}


