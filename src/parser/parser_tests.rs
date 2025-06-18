use crate::parser::parser::ParseError;
use crate::parser::parser::Parser;
use crate::scanner::scanner::scan;
use crate::types::expr::Expr;
use crate::types::literal::Literal;
use crate::types::stmt::Stmt;
use crate::types::token::Token;
use crate::types::token_type::TokenType;
use crate::types::types::Type;

// Helper to run parser
fn parse_ok(source: &str) -> Vec<Stmt> {
    let tokens = scan(source);
    let mut parser = Parser::new(tokens);
    parser.parse_test()
}

fn parse_err(source: &str) -> ParseError {
    let tokens = scan(source);
    let mut parser = Parser::new(tokens);
    let res = parse_ok(source);
    match parser.parse_stmt_result() {
        Ok(_) => panic!("Expected parse error, but parsing succeeded."),
        Err(e) => e,
    }
}

// === Integration tests ===

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
fn improper_fun_decleration_no_type() {
    //todo -> double check correct error procedure
    let err = parse_err("fun a(): { }");
    if let ParseError::ParseToken(token, _) = err {
        assert_eq!(token.tokentype, TokenType::LeftBrace);
    } else {
        panic!("Expected ParseToken error due to missing semicolon");
    }
}

#[test]
fn fun_decleration_int_type() {
    let stmts = parse_ok("fun a(): int { var a: int = 5;}");
    assert_eq!(stmts.len(), 1);
    if let Stmt::Function(token, params, body, return_type) = &stmts[0] {
        assert_eq!(token.lexeme, "a");
        assert!(params.is_empty());
        assert!(!body.is_empty());
        assert_eq!(return_type, &Type::Int);
    } else {
        panic!("Expected variable declaration");
    }
}

#[test]
fn test_missing_semicolon() {
    let err = parse_err("var x: int = 42");
    if let ParseError::ParseToken(token, _) = err {
        assert_eq!(token.tokentype, TokenType::Eof);
    } else {
        panic!("Expected ParseToken error due to missing semicolon");
    }
}

#[test]
fn test_nested_if_else() {
    let stmts = parse_ok("if (true) if (false) var x: int = 0; else var y: int = 1;");
    assert_eq!(stmts.len(), 1);
    // Further assertions can dig into AST shape, omitted for brevity
}

#[test]
fn test_mismatched_braces() {
    let err = parse_err("if (true) { var x: int = 1; ");
    if let ParseError::ParseToken(_, msg) = err {
        assert!(msg.contains("Expect '}' after block"));
    } else {
        panic!("Expected ParseToken error due to mismatched braces");
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
    if let ParseError::ParseToken(_, msg) = err {
        assert!(msg.contains("Expected Type specifier ':'"));
    } else {
        panic!("Expected ParseToken error due to mismatched braces");
    }
}

#[test]
fn test_function_params_typed() {
    let stmts = parse_ok("fun foo(a: int) { return; }");
    assert_eq!(stmts.len(), 1);
    if let Stmt::Function(token, params, body, ret_type) = &stmts[0] {
        assert_eq!(token.lexeme, "foo");
        assert!(!params.is_empty());
        let a = params.as_ref();
        for val in a {
            assert_eq!(val.1, Type::Int);
            let something = val.0.clone();
            assert_eq!(something.lexeme, "a");
            assert_eq!(something.literal, None);
        }
        assert_eq!(ret_type, &Type::Void);
        assert!(!body.is_empty());
    } else {
        panic!("Expected function declaration");
    }
}

#[test]
fn invalid_function_improper_return() {
    let err = parse_err("fun foo(): { return; }");
    if let ParseError::ParseToken(token, _) = err {
        assert_eq!(token.tokentype, TokenType::LeftBrace);
    } else {
        panic!("Expected ParseToken error due to missing type");
    }
    // if let Stmt::Function(token, params, body, ret_type) = &stmts[0] {
    //     assert_eq!(token.lexeme, "foo");
    //     assert!(params.is_empty());
    //     assert!(ret_type.is_none());
    //     assert!(!body.is_empty());
    // } else {
    //     panic!("Expected function declaration");
    // }
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
