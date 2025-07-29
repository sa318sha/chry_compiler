use crate::types::{
    literal::{Literal, pretty_print_literal},
    token::Token,
    types::{Type, pretty_print_type},
};

#[derive(Debug, Clone)]
pub struct TypedExpr {
    pub kind: TypedExprKind,
    pub ty: Type, // This should be the actual resolved type
}

#[derive(Debug, Clone)]
pub enum TypedExprKind {
    Literal(Literal),

    Call(Token, Token, Vec<TypedExpr>),

    Logical(Box<TypedExpr>, Token, Box<TypedExpr>),
    Set(Box<TypedExpr>, Token, Box<TypedExpr>),
    Get(Token, Box<TypedExpr>),
    Variable(Token),
    Unary(Token, Box<TypedExpr>),
    Assignment(Token, Box<TypedExpr>),
    Binary(Box<TypedExpr>, Token, Box<TypedExpr>),
    Grouping(Box<TypedExpr>),
}
pub fn pretty_print_typed_expr(expr: &TypedExpr) -> String {
    let type_str = format!(": {}", pretty_print_type(&expr.ty));
    match &expr.kind {
        TypedExprKind::Literal(lit) => format!("{}{}", pretty_print_literal(lit), type_str),
        TypedExprKind::Variable(tok) => format!("{}{}", tok.lexeme, type_str),
        TypedExprKind::Unary(op, right) => {
            format!(
                "{}{}{}",
                op.lexeme,
                pretty_print_typed_expr(right),
                type_str
            )
        }
        TypedExprKind::Binary(left, op, right) => {
            format!(
                "{} {} {}{}",
                pretty_print_typed_expr(left),
                op.lexeme,
                pretty_print_typed_expr(right),
                type_str
            )
        }
        TypedExprKind::Logical(left, op, right) => {
            format!(
                "{} {} {}{}",
                pretty_print_typed_expr(left),
                op.lexeme,
                pretty_print_typed_expr(right),
                type_str
            )
        }
        TypedExprKind::Grouping(inner) => {
            format!("({}){}", pretty_print_typed_expr(inner), type_str)
        }
        TypedExprKind::Call(name, _, args) => {
            let args_str = args
                .iter()
                .map(|e| pretty_print_typed_expr(e))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}({}){}", name.lexeme, args_str, type_str)
        }
        TypedExprKind::Set(obj, _, val) => {
            format!(
                "{} = {}{}",
                pretty_print_typed_expr(obj),
                pretty_print_typed_expr(val),
                type_str
            )
        }
        TypedExprKind::Get(tok, obj) => {
            format!(
                "{}.{}{}",
                pretty_print_typed_expr(obj),
                tok.lexeme,
                type_str
            )
        }
        TypedExprKind::Assignment(var, rhs) => {
            format!(
                "{} = {}{}",
                var.lexeme,
                pretty_print_typed_expr(rhs),
                type_str
            )
        }
    }
}
