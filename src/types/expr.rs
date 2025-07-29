use super::literal::pretty_print_literal;
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
    Assign(Token, Box<Expr>), // add Assign, Call, etc. later
}

pub fn pretty_print_expr(expr: &Expr) -> String {
    match expr {
        Expr::Assign(var, val) => format!("{} {}", var.lexeme, pretty_print_expr(val)),
        Expr::Literal(lit) => pretty_print_literal(lit),
        Expr::Variable(tok) => tok.lexeme.clone(),
        Expr::Unary(op, right) => format!("{}{}", op.lexeme, pretty_print_expr(right)),
        Expr::Binary(left, op, right) => format!(
            "{} {} {}",
            pretty_print_expr(left),
            op.lexeme,
            pretty_print_expr(right)
        ),
        Expr::Logical(left, op, right) => format!(
            "{} {} {}",
            pretty_print_expr(left),
            op.lexeme,
            pretty_print_expr(right)
        ),
        Expr::Grouping(inner) => format!("({})", pretty_print_expr(inner)),
        Expr::Call(callee, _, args) => {
            let args_str = args
                .iter()
                .map(|a| pretty_print_expr(a))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}({})", pretty_print_expr(callee), args_str)
        }
        Expr::Set(obj, _, val) => {
            // For simplicity: treat as obj = val;
            format!("{} = {}", pretty_print_expr(obj), pretty_print_expr(val))
        }
        Expr::Get(tok, obj) => {
            format!("{}.{}", pretty_print_expr(obj), tok.lexeme)
        }
    }
}

// #[derive(Debug, Clone)]
impl Literal {
    pub fn get_type(&self) -> Type {
        match self {
            Literal::Bool(_) => Type::Bool,
            Literal::Int(_) => Type::Int,
            Literal::String(_) => Type::String,
            Literal::Float(_, _) => Type::Float, // if supported
            Literal::Nil => Type::Nil,

            _ => todo!("Other literal types"),
        }
    }
}
