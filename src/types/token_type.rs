#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenType {
    // Single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Colon,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens
    Arrow,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    BitWiseOr,
    BitWiseAnd,
    And,
    Or,

    Identifier,
    // String,
    // Number,

    // Types
    // LITERALS -> actual values
    StringLiteral,
    IntLiteral,
    FloatLiteral,
    // DoubleLiteral,
    // Float,
    // Double,
    BoolLiteral,

    // Keywords
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Break,

    //these are actual hardcoded types
    Int,
    Float,
    Bool,
    Void,
    // String,
    Eof,
}
