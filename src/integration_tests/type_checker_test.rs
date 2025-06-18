use ::chry_compiler::{Parser, Scanner, TokenType, Type, TypeChecker};
use chry_compiler::{Expr, Token, stmt::Stmt};

#[test]
fn var_test() {
    let source = "var x:int = 12";
    let mut scanner = Scanner::new(source);
    let mut parser = Parser::new(scanner.scan_tokens());

    let mut stmts = parser.parse();
    let mut type_checker = TypeChecker::new();

    let stmt = Stmt::Variable(
        Token::new(0, TokenType::Var, "var".to_string(), None),
        Some(Type::Int),
        Some(Expr::Literal(chry_compiler::Literal::Int(5))),
    );

    let res = type_checker.check_single_stmt(&stmt);

    if let Ok(r) = res {
        println!("passed {:?}", r);
    }

    // typeChecker.check_single_stmt(stmt);
}
