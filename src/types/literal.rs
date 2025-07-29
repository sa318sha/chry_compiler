#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i32),
    Float(f64, String),
    Bool(bool),
    String(String),
    Nil,
}

pub fn pretty_print_literal(lit: &Literal) -> String {
    match lit {
        Literal::Int(i) => i.to_string(),
        Literal::Float(f, string) => string.clone(),
        Literal::Bool(b) => b.to_string(),
        Literal::String(s) => format!("\"{}\"", s),
        _ => "nil".to_string(),
    }
}
