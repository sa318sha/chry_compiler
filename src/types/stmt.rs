use super::expr::pretty_print_expr;
use crate::types::expr::Expr;
use crate::types::token::Token;
use crate::types::types::{Type, pretty_print_type};

#[derive(Debug, Clone, PartialEq)]
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

//recreate stmt
pub fn pretty_print_stmt(stmt: &Stmt) -> String {
    match stmt {
        Stmt::Variable(token, ty, init) => {
            let ty_str = ty
                .as_ref()
                .map(|t| pretty_print_type(t))
                .unwrap_or_default();
            let init_str = init
                .as_ref()
                .map(|e| format!("= {}", pretty_print_expr(e)))
                .unwrap_or_default();
            format!(
                "var {}{} {};",
                token.lexeme,
                if ty_str.is_empty() {
                    "".to_string()
                } else {
                    format!(": {}", ty_str)
                },
                init_str
            )
            .trim_end()
            .to_string()
        }
        Stmt::Print(expr) => {
            format!("print {};", pretty_print_expr(expr))
        }
        Stmt::Expression(expr) => {
            format!("{};", pretty_print_expr(expr))
        }
        Stmt::Return(_, val) => {
            if let Some(v) = val.as_ref() {
                format!("return {};", pretty_print_expr(v))
            } else {
                "return;".to_string()
            }
        }
        Stmt::Break(_) => "break;".to_string(),
        Stmt::Block(block_stmts) => {
            let inner = block_stmts
                .iter()
                .map(|s| pretty_print_stmt(s))
                .collect::<Vec<_>>()
                .join(" ");
            format!("{{ {} }}", inner)
        }
        Stmt::If(cond, then, else_stmt) => {
            let then_str = pretty_print_stmt(then);
            let else_str = else_stmt
                .as_ref()
                .clone()
                .map(|e| format!("else {}", pretty_print_stmt(&e)))
                .unwrap_or_default();
            format!("if ({}) {} {}", pretty_print_expr(cond), then_str, else_str)
                .trim()
                .to_string()
        }
        Stmt::While(cond, body) => {
            format!(
                "while ({}) {}",
                pretty_print_expr(cond),
                pretty_print_stmt(body)
            )
        }
        Stmt::Function(token, params, body, return_type) => {
            let params_str = params
                .iter()
                .map(|(tok, ty)| format!("{}: {}", tok.lexeme, pretty_print_type(ty)))
                .collect::<Vec<_>>()
                .join(", ");
            let body_str = body
                .iter()
                .map(|s| pretty_print_stmt(s))
                .collect::<Vec<_>>()
                .join(" ");
            let ret_str = pretty_print_type(return_type);
            format!(
                "fun {}({}): {} {{ {} }}",
                token.lexeme, params_str, ret_str, body_str
            )
        }
        Stmt::Class(name, methods) => {
            let methods_str = methods
                .iter()
                .map(|m| pretty_print_stmt(m))
                .collect::<Vec<_>>()
                .join(" ");
            format!("class {} {{ {} }}", name.lexeme, methods_str)
        }
    }
}
