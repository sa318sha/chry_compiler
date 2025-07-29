use super::typed_expr::TypedExpr;
use crate::types::{
    token::Token,
    typed_expr::pretty_print_typed_expr,
    types::{Type, pretty_print_type},
};

// dont need this indirection to store a type for a statement because statements dont have an inherent type
// #[derive(Debug, Clone)]
// pub struct TypedStmt {
//     pub kind: TypedStmtKind,

//     // pub ty: Type, // This should be the actual resolved type
// }

#[derive(Debug, Clone)]
pub enum TypedStmt {
    Block(Box<Vec<TypedStmt>>),

    Variable(Token, Type, Option<TypedExpr>),

    Print(Box<TypedExpr>),

    Expression(Box<TypedExpr>),

    Function(Token, Box<Vec<(Token, Type)>>, Box<Vec<TypedStmt>>, Type),

    If(Box<TypedExpr>, Box<TypedStmt>, Box<Option<TypedStmt>>),

    While(Box<TypedExpr>, Box<TypedStmt>),

    Break(Token),

    Return(Token, Box<Option<TypedExpr>>),

    Class(Token, Box<Vec<TypedStmt>>),
}


pub fn pretty_print_typed_stmt(stmt: &TypedStmt) -> String {
    match stmt {
        TypedStmt::Variable(tok, ty, Some(init)) => {
            format!(
                "var {}: {} = {};",
                tok.lexeme,
                pretty_print_type(ty),
                pretty_print_typed_expr(init)
            )
        }
        TypedStmt::Variable(tok, ty, None) => {
            format!("var {}: {};", tok.lexeme, pretty_print_type(ty))
        }
        TypedStmt::Print(expr) => format!("print {};", pretty_print_typed_expr(expr)),
        TypedStmt::Expression(expr) => format!("{};", pretty_print_typed_expr(expr)),
        TypedStmt::Return(_, (val)) => {
            if let Some(v) = val.as_ref() {
                format!("return {};", pretty_print_typed_expr(v))
            } else {
                "return;".to_string()
            }
        }
        TypedStmt::Break(_) => "break;".to_string(),
        TypedStmt::Block(stmts) => {
            let body = stmts
                .iter()
                .map(|s| pretty_print_typed_stmt(s))
                .collect::<Vec<_>>()
                .join(" ");
            format!("{{ {} }}", body)
        }
        TypedStmt::If(cond, then_stmt, else_stmt) => {
            let then_str = pretty_print_typed_stmt(then_stmt);
            let else_str = else_stmt
                .as_ref()
                .as_ref()
                .map(|e| format!(" else {}", pretty_print_typed_stmt(e)))
                .unwrap_or_default();
            format!(
                "if ({}) {}{}",
                pretty_print_typed_expr(cond),
                then_str,
                else_str
            )
        }
        TypedStmt::While(cond, body) => {
            format!(
                "while ({}) {}",
                pretty_print_typed_expr(cond),
                pretty_print_typed_stmt(body)
            )
        }
        TypedStmt::Function(name, params, body, ret_ty) => {
            let params_str = params
                .iter()
                .map(|(tok, ty)| format!("{}: {}", tok.lexeme, pretty_print_type(ty)))
                .collect::<Vec<_>>()
                .join(", ");
            let body_str = body
                .iter()
                .map(|s| pretty_print_typed_stmt(s))
                .collect::<Vec<_>>()
                .join(" ");
            format!(
                "fun {}({}): {} {{ {} }}",
                name.lexeme,
                params_str,
                pretty_print_type(ret_ty),
                body_str
            )
        }
        TypedStmt::Class(name, methods) => {
            let body = methods
                .iter()
                .map(|s| pretty_print_typed_stmt(s))
                .collect::<Vec<_>>()
                .join(" ");
            format!("class {} {{ {} }}", name.lexeme, body)
        }
    }
}
