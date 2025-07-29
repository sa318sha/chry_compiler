use super::parse_error::ParseError;
use crate::parser::parser::Parser;
use crate::scanner::scanner::Scanner;
use crate::types::expr::Expr;
use crate::types::literal::Literal;
use crate::types::stmt::Stmt;
use crate::types::token_type::TokenType;
use crate::types::types::Type;

// Helper to run parser
fn parse_ok(source: &str) -> Vec<Stmt> {
    let tokens = Scanner::new(source).scan_tokens();
    let mut parser = Parser::new(tokens);
    parser.parse()
}

fn parse_err(source: &str) -> ParseError {
    let tokens = Scanner::new(source).scan_tokens();
    let mut parser = Parser::new(tokens);
    let _ = parser.parse(); // run the test parse

    parser.errors.first().cloned().expect("Expected error")
}

// === Integration tests ===

#[test]
fn test_malformed_expression_1_plus() {
    let err = parse_err("1 + ;");
    assert!(matches!(err, ParseError::UnexpectedToken { .. }));
}

#[test]
fn test_malformed_expression_a_star() {
    let err = parse_err("a * ;");
    assert!(matches!(err, ParseError::UnexpectedToken { .. }));
}

#[test]
fn test_dangling_else_associativity() {
    let stmts = parse_ok("if (true) if (false) var x: int = 0; else var y: int = 1;");
    assert_eq!(stmts.len(), 1);
    // Optionally check AST shape if you want to confirm associativity
}

#[test]
fn test_call_get_chain() {
    let stmts = parse_ok("a.b().c().d;");
    assert_eq!(stmts.len(), 1);
    // Optionally walk AST and confirm Expr::Get and Expr::Call nesting
}

#[test]
fn test_recovery_after_error() {
    let stmts = parse_ok("var x: int = ; var y: int = 1;");
    // First var should cause an error, but y should be valid
    assert!(stmts.iter().any(|s| matches!(s, Stmt::Variable(t, _, _) if t.lexeme == "y")));
}

#[test]
fn test_missing_separator_between_decls() {
    let err = parse_err("var x: int = 1 var y: int = 2;");
    assert!(matches!(err, ParseError::UnexpectedToken { .. }));
}

#[test]
fn test_invalid_for_header() {
    let err = parse_err("for var x: int = 0; x < 10; x = x + 1) {}");
    assert!(matches!(err, ParseError::UnexpectedToken { .. }));
}

#[test]
fn test_empty_input() {
    let stmts = parse_ok("");
    assert_eq!(stmts.len(), 0);
}

#[test]
fn test_var_decl_with_type_and_init() {
    let stmts = parse_ok("var x: int = 42;");
    assert_eq!(stmts.len(), 1);
    if let Stmt::Variable(token, var_type, init) = &stmts[0] {
        assert_eq!(token.lexeme, "x");
        assert_eq!(var_type, &Some(Type::Int));
        assert_eq!(init.as_ref().unwrap(), &Expr::Literal(Literal::Int(42)));
    } else {
        panic!("Expected variable declaration");
    }
}

#[test]
fn test_improper_fun_decl_no_type() {
    let err = parse_err("fun a(): { }");
    match err {
        ParseError::InvalidType { token, .. } => {
            assert_eq!(token.lexeme, "{");
        }
        _ => panic!("Expected InvalidType error due to missing return type."),
    }
}

#[test]
fn test_fun_decl_with_int_return_type() {
    let stmts = parse_ok("fun a(): int { var a: int = 5; }");
    assert_eq!(stmts.len(), 1);
    if let Stmt::Function(token, params, body, return_type) = &stmts[0] {
        assert_eq!(token.lexeme, "a");
        assert!(params.is_empty());
        assert!(!body.is_empty());
        assert_eq!(return_type, &Type::Int);
    } else {
        panic!("Expected function declaration");
    }
}

#[test]
fn test_missing_semicolon() {
    let err = parse_err("var x: int = 42");
    match err {
        ParseError::UnexpectedEof { token, .. } => {
            assert_eq!(token.tokentype, TokenType::Eof);
        }
        _ => panic!("Expected UnexpectedToken error due to missing semicolon."),
    }
}

#[test]
fn test_nested_if_else() {
    let stmts = parse_ok("if (true) if (false) var x: int = 0; else var y: int = 1;");
    assert_eq!(stmts.len(), 1);
    // Further AST assertions can be added as needed
}

#[test]
fn test_mismatched_braces() {
    let err = parse_err("if (true) { var x: int = 1; ");
    match err {
        ParseError::UnterminatedBlock { start, .. } => {
            assert_eq!(start.tokentype, TokenType::LeftBrace);
        }
        _ => panic!("Expected UnterminatedBlock error due to unmatched '{{'"),
    }
}

#[test]
fn test_function_no_params() {
    let stmts = parse_ok("fun foo() { return; }");
    assert_eq!(stmts.len(), 1);
    if let Stmt::Function(token, params, body, ret_type) = &stmts[0] {
        assert_eq!(token.lexeme, "foo");
        assert!(params.is_empty());
        assert_eq!(ret_type, &Type::Void);
        assert!(!body.is_empty());
    } else {
        panic!("Expected function declaration");
    }
}

#[test]
fn test_function_params_no_type() {
    let err = parse_err("fun foo(a,b) { return; }");
    match err {
        ParseError::UnexpectedToken { token, .. } => {
            assert_eq!(token.lexeme, ",");
        }
        ParseError::InvalidType { .. } => {
            // Alternative depending on your parser's flow
        }
        _ => panic!("Expected UnexpectedToken or InvalidType for missing param types."),
    }
}

#[test]
fn test_function_params_typed() {
    let stmts = parse_ok("fun foo(a: int) { return; }");
    assert_eq!(stmts.len(), 1);
    if let Stmt::Function(token, params, body, ret_type) = &stmts[0] {
        assert_eq!(token.lexeme, "foo");
        assert_eq!(params.len(), 1);
        let (param_token, param_type) = &params[0];
        assert_eq!(param_token.lexeme, "a");
        assert_eq!(param_type, &Type::Int);
        assert_eq!(ret_type, &Type::Void);
        assert!(!body.is_empty());
    } else {
        panic!("Expected function declaration");
    }
}

#[test]
fn test_invalid_function_improper_return() {
    let err = parse_err("fun foo(): { return; }");
    match err {
        ParseError::InvalidType { token, .. } => {
            assert_eq!(token.tokentype, TokenType::LeftBrace);
        }
        _ => panic!("Expected InvalidType error due to missing return type."),
    }
}

#[test]
fn test_class_no_methods() {
    let stmts = parse_ok("class C {}");
    assert_eq!(stmts.len(), 1);
    if let Stmt::Class(token, methods) = &stmts[0] {
        assert_eq!(token.lexeme, "C");
        assert!(methods.is_empty());
    } else {
        panic!("Expected class declaration");
    }
}
