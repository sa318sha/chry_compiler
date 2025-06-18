#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i32),
    Float(f64),
    Bool(bool),
    String(String),
    Char(char), // optional
    Unit,       // if you support ()
    Nil,
}
