use std::iter::zip;

#[cfg(test)]
use super::scanner::*;
use crate::{
    scanner::scan_error::ScanError,
    types::{literal::Literal, token::Token, token_type::TokenType},
};

fn token(t: TokenType, lexeme: &str, line: u32) -> Token {
    Token::new(line, t, lexeme.to_string(), None)
}

fn token_literal(t: TokenType, lexeme: &str, line: u32, lit: Literal) -> Token {
    Token::new(line, t, lexeme.to_string(), Some(lit))
}

fn run_scanner(source: &str) -> Scanner {
    let mut scanner = Scanner::new(source);
    scanner.scan_tokens();
    return scanner;
}

fn assert_scanner_ok(source: &str, expected: Vec<Token>) {
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens();
    assert_eq!(tokens, expected);
    assert!(
        scanner.errors.is_empty(),
        "Expected no errors but got: {:?}",
        scanner.errors
    );
}

fn assert_scanner_error<F>(source: &str, expected: Vec<Token>, matcher: F)
where
    F: Fn(&ScanError) -> bool,
{
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens();
    assert_eq!(tokens, expected);

    assert!(
        scanner.errors.iter().any(matcher),
        "Expected specific scan error not found. Errors: {:?}",
        scanner.errors
    );
}

#[test]
fn test_left_paren() {
    let val = ("(");
    let expected = vec![
        token(TokenType::LeftParen, "(", 1),
        token(TokenType::Eof, "", 1),
    ];
    assert_scanner_ok(val, expected);
}

#[test]
fn test_right_paren() {
    let val = (")");
    let expected = vec![
        token(TokenType::RightParen, ")", 1),
        token(TokenType::Eof, "", 1),
    ];
    assert_scanner_ok(val, expected);
}

#[test]
fn test_left_brace() {
    let val = ("{");
    let expected = vec![
        token(TokenType::LeftBrace, "{", 1),
        token(TokenType::Eof, "", 1),
    ];
    assert_scanner_ok(val, expected);
}

#[test]
fn test_right_brace() {
    let val = ("}");
    let expected = vec![
        token(TokenType::RightBrace, "}", 1),
        token(TokenType::Eof, "", 1),
    ];
    assert_scanner_ok(val, expected);
}

#[test]
fn test_comma() {
    let val = (",");
    let expected = vec![
        token(TokenType::Comma, ",", 1),
        token(TokenType::Eof, "", 1),
    ];
    assert_scanner_ok(val, expected);
}

#[test]
fn test_dot() {
    let val = (".");
    let expected = vec![token(TokenType::Dot, ".", 1), token(TokenType::Eof, "", 1)];
    assert_scanner_ok(val, expected);
}

#[test]
fn test_minus() {
    let val = ("-");
    let expected = vec![
        token(TokenType::Minus, "-", 1),
        token(TokenType::Eof, "", 1),
    ];
    assert_scanner_ok(val, expected);
}

#[test]
fn test_plus() {
    let val = ("+");
    let expected = vec![token(TokenType::Plus, "+", 1), token(TokenType::Eof, "", 1)];
    assert_scanner_ok(val, expected);
}
#[test]
fn test_bitwise_or() {
    let val = ("|");
    let expected = vec![
        token(TokenType::BitWiseOr, "|", 1),
        token(TokenType::Eof, "", 1),
    ];
    assert_scanner_ok(val, expected);
}

#[test]
fn test_logical_or() {
    let val = ("||");
    let expected = vec![token(TokenType::Or, "||", 1), token(TokenType::Eof, "", 1)];
    assert_scanner_ok(val, expected);
}

#[test]
fn test_mixed_valid_invalid_symbols() {
    let val = ("var @ fun # 123 $");
    let expected = vec![
        token(TokenType::Var, "var", 1),
        token(TokenType::Fun, "fun", 1),
        token_literal(TokenType::IntLiteral, "123", 1, Literal::Int(123)),
        token(TokenType::Eof, "", 1),
    ];
    assert_scanner_error(val, expected, |e| {
        matches!(e, ScanError::InvalidCharacterStream { .. })
    })
}

#[test]
fn test_semicolon() {
    let val = (";");
    let expected = vec![
        token(TokenType::Semicolon, ";", 1),
        token(TokenType::Eof, "", 1),
    ];
    assert_scanner_ok(val, expected);
}

#[test]
fn test_star() {
    let val = ("*");
    let expected = vec![token(TokenType::Star, "*", 1), token(TokenType::Eof, "", 1)];
    assert_scanner_ok(val, expected);
}

#[test]
fn test_identifier_simple() {
    let val = ("x");
    let expected = vec![
        token(TokenType::Identifier, "x", 1),
        token(TokenType::Eof, "", 1),
    ];
    assert_scanner_ok(val, expected);
}

#[test]
fn test_keyword_var() {
    let val = ("var");
    let expected = vec![
        token(TokenType::Var, "var", 1),
        token(TokenType::Eof, "", 1),
    ];
    assert_scanner_ok(val, expected);
}

#[test]
fn test_integer_literal() {
    let val = ("123");
    let expected = vec![
        token_literal(TokenType::IntLiteral, "123", 1, Literal::Int(123)),
        token(TokenType::Eof, "", 1),
    ];
    assert_scanner_ok(val, expected);
}

#[test]
fn test_float_literal() {
    let val = ("45.67");
    let expected = vec![
        token_literal(
            TokenType::FloatLiteral,
            "45.67",
            1,
            Literal::Float(45.67, "45.67".to_string()),
        ),
        token(TokenType::Eof, "", 1),
    ];
    assert_scanner_ok(val, expected);
}

#[test]
fn test_string_literal_simple() {
    let val = ("\"hello\"");
    let expected = vec![
        token_literal(
            TokenType::StringLiteral,
            "\"hello\"",
            1,
            Literal::String("hello".to_string()),
        ),
        token(TokenType::Eof, "", 1),
    ];
    assert_scanner_ok(val, expected);
}

#[test]
fn test_bang() {
    let val = ("!");
    let expected = vec![token(TokenType::Bang, "!", 1), token(TokenType::Eof, "", 1)];
    assert_scanner_ok(val, expected);
}

#[test]
fn test_bang_equal() {
    let val = ("!=");
    let expected = vec![
        token(TokenType::BangEqual, "!=", 1),
        token(TokenType::Eof, "", 1),
    ];
    assert_scanner_ok(val, expected);
}

#[test]
fn test_equal() {
    let val = ("=");
    let expected = vec![
        token(TokenType::Equal, "=", 1),
        token(TokenType::Eof, "", 1),
    ];
    assert_scanner_ok(val, expected);
}

#[test]
fn test_equal_equal() {
    let val = ("==");
    let expected = vec![
        token(TokenType::EqualEqual, "==", 1),
        token(TokenType::Eof, "", 1),
    ];
    assert_scanner_ok(val, expected);
}

#[test]
fn test_less() {
    let val = ("<");
    let expected = vec![token(TokenType::Less, "<", 1), token(TokenType::Eof, "", 1)];
    assert_scanner_ok(val, expected);
}

#[test]
fn test_less_equal() {
    let val = ("<=");
    let expected = vec![
        token(TokenType::LessEqual, "<=", 1),
        token(TokenType::Eof, "", 1),
    ];
    assert_scanner_ok(val, expected);
}

#[test]
fn test_greater() {
    let val = (">");
    let expected = vec![
        token(TokenType::Greater, ">", 1),
        token(TokenType::Eof, "", 1),
    ];
    assert_scanner_ok(val, expected);
}

#[test]
fn test_greater_equal() {
    let val = (">=");
    let expected = vec![
        token(TokenType::GreaterEqual, ">=", 1),
        token(TokenType::Eof, "", 1),
    ];
    assert_scanner_ok(val, expected);
}

#[test]
fn test_arrow() {
    let val = ("->");
    let expected = vec![
        token(TokenType::Arrow, "->", 1),
        token(TokenType::Eof, "", 1),
    ];
    assert_scanner_ok(val, expected);
}

#[test]
fn test_comment_skipping() {
    let val = ("// comment\nvar");
    let expected = vec![
        token(TokenType::Var, "var", 2),
        token(TokenType::Eof, "", 2),
    ];
    assert_scanner_ok(val, expected);
}

#[test]
fn test_multiline_comment() {
    let val = ("/* comment */ fun");
    let expected = vec![
        token(TokenType::Fun, "fun", 1),
        token(TokenType::Eof, "", 1),
    ];
    assert_scanner_ok(val, expected);
    // assert_eq!(sc)
}

#[test]
fn test_unterminated_string() {
    let val = ("\"unterminated");
    let expected = vec![token(TokenType::Eof, "", 1)];
    assert_scanner_error(val, expected, |e| {
        matches!(e, ScanError::UnterminatedString { .. })
    });
}

#[test]
fn test_unexpected_character() {
    let val = ("@");
    let expected = vec![token(TokenType::Eof, "", 1)];
    assert_scanner_error(val, expected, |e| {
        matches!(e, ScanError::InvalidCharacterStream { .. })
    });
}
