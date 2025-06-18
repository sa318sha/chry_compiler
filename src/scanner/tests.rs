#[cfg(test)]
use super::scanner::*;
use crate::types::{literal::Literal, token::Token, token_type::TokenType};

fn token(t: TokenType, lexeme: &str, line: u32) -> Token {
    Token::new(line, t, lexeme.to_string(), None)
}

fn token_literal(t: TokenType, lexeme: &str, line: u32, lit: Literal) -> Token {
    Token::new(line, t, lexeme.to_string(), Some(lit))
}

#[test]
fn test_single_symbols() {
    let source = "(){},.-+;*";
    let tokens = scan(source);
    let expected = vec![
        token(TokenType::LeftParen, "(", 1),
        token(TokenType::RightParen, ")", 1),
        token(TokenType::LeftBrace, "{", 1),
        token(TokenType::RightBrace, "}", 1),
        token(TokenType::Comma, ",", 1),
        token(TokenType::Dot, ".", 1),
        token(TokenType::Minus, "-", 1),
        token(TokenType::Plus, "+", 1),
        token(TokenType::Semicolon, ";", 1),
        token(TokenType::Star, "*", 1),
        token(TokenType::Eof, "", 1),
    ];
    // for (i,val) in expected.iter().enumerate(){
    //     assert_eq!(tokens[i], expected[i]);
    // }
    // assert_eq!(tokens, expected);
}

#[test]
fn test_keywords_and_identifiers() {
    let source = "var fun true false class while x y_z";
    let tokens = scan(source);
    let expected = vec![
        token(TokenType::Var, "var", 1),
        token(TokenType::Fun, "fun", 1),
        token(TokenType::True, "true", 1),
        token(TokenType::False, "false", 1),
        token(TokenType::Class, "class", 1),
        token(TokenType::While, "while", 1),
        token(TokenType::Identifier, "x", 1),
        token(TokenType::Identifier, "y_z", 1),
        token(TokenType::Eof, "", 1),
    ];
    assert_eq!(tokens, expected);
}

#[test]
fn test_numbers() {
    let source = "123 45.67";
    let tokens = scan(source);
    let expected = vec![
        token_literal(TokenType::IntLiteral, "123", 1, Literal::Int(123)),
        token_literal(TokenType::FloatLiteral, "45.67", 1, Literal::Float(45.67)),
        token(TokenType::Eof, "", 1),
    ];
    assert_eq!(tokens, expected);
}

#[test]
fn test_string_literal() {
    let source = "\"hello world\"";
    let tokens = scan(source);
    let expected = vec![
        token_literal(
            TokenType::StringLiteral,
            "\"hello world\"",
            1,
            Literal::String("hello world".to_string()),
        ),
        token(TokenType::Eof, "", 1),
    ];
    assert_eq!(tokens, expected);
}

#[test]
fn test_comments_and_whitespace() {
    let source = "// comment\nvar\n/* block\ncomment */fun";
    let tokens = scan(source);
    let expected = vec![
        token(TokenType::Var, "var", 2),
        token(TokenType::Fun, "fun", 4),
        token(TokenType::Eof, "", 4),
    ];
    assert_eq!(tokens, expected);
}

#[test]
fn test_operators_and_comparisons() {
    let source = "! != = == < <= > >= ->";
    let tokens = scan(source);
    let expected = vec![
        token(TokenType::Bang, "!", 1),
        token(TokenType::BangEqual, "!=", 1),
        token(TokenType::Equal, "=", 1),
        token(TokenType::EqualEqual, "==", 1),
        token(TokenType::Less, "<", 1),
        token(TokenType::LessEqual, "<=", 1),
        token(TokenType::Greater, ">", 1),
        token(TokenType::GreaterEqual, ">=", 1),
        token(TokenType::Arrow, "->", 1),
        token(TokenType::Eof, "", 1),
    ];
    assert_eq!(tokens, expected);
}

#[test]
fn test_multiline_string_and_line_tracking() {
    let source = "\"line1\nline2\"";
    let tokens = scan(source);
    let expected = vec![
        token_literal(
            TokenType::StringLiteral,
            "\"line1\nline2\"",
            2,
            Literal::String("line1\nline2".to_string()),
        ),
        token(TokenType::Eof, "", 2),
    ];
    assert_eq!(tokens, expected);
}

#[test]
fn test_unterminated_string() {
    let source = "\"unterminated";
    let tokens = scan(source);
    // You could also expect an error to be logged / returned
    let expected = vec![token(TokenType::Eof, "", 1)];
    assert_eq!(tokens, expected);
}

#[test]
fn test_unexpected_characters() {
    let source = "@";
    let tokens = scan(source);
    let expected = vec![token(TokenType::Eof, "", 1)];
    // Scanner doesn't produce token for unknown char, but you may assert error was logged
    assert_eq!(tokens, expected);
}
