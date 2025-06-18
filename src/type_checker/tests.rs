use crate::parser::parser::Parser;
use crate::scanner::scanner::scan;
use crate::type_checker::type_checker::{TypeChecker, TypeError};

/// Run the typechecker and return the checker (so we can inspect errors and typed_stmts)
fn run_typecheck(source: &str) -> TypeChecker {
    let tokens = scan(source);
    let mut parser = Parser::new(tokens);
    let stmts = parser.parse();
    let mut checker = TypeChecker::new();
    checker.check(&stmts);
    checker
}

/// Assert type checking passes without errors
fn assert_typecheck_ok(source: &str) {
    let checker = run_typecheck(source);
    assert!(
        checker.errors.is_empty(),
        "Expected no errors but got: {:?}",
        checker.errors
    );
}

/// Assert type checking fails with at least one error of expected type
fn assert_typecheck_err<F>(source: &str, matcher: F)
where
    F: Fn(&TypeError) -> bool,
{
    let checker = run_typecheck(source);
    assert!(
        !checker.errors.is_empty(),
        "Expected error but type checking passed"
    );
    assert!(
        checker.errors.iter().any(matcher),
        "Expected specific error not found. Errors: {:?}",
        checker.errors
    );
}

#[test]
fn var_declarations_pass() {
    assert_typecheck_ok("var a : int = 5;");
    assert_typecheck_ok("var b : float = 3.14;");
    assert_typecheck_ok("var c : bool = true;");
    assert_typecheck_ok("var e = true;");
    assert_typecheck_ok("var f = 42;");
    assert_typecheck_ok("var g : int;");
}

#[test]
fn var_declarations_fail() {
    assert_typecheck_err("var d : int = 3.14;", |e| {
        matches!(e, TypeError::Mismatch { .. })
    });
    // assert_typecheck_err("var h;", |e| matches!(e, TypeError::Other(_)));
}

fn var_empty_declaration() {
    assert_typecheck_ok("var h;");
}

#[test]
fn binary_unary_pass() {
    assert_typecheck_ok("var x : int = 1 + 2;");
    assert_typecheck_ok("var y : float = 1.0 + 2;");
    assert_typecheck_ok("var z : bool = true && false;");
    assert_typecheck_ok("var u : bool = !false;");
    assert_typecheck_ok("var v : int = -3;");
}

#[test]
fn binary_unary_fail() {
    assert_typecheck_err("var w : int = 1 + true;", |e| {
        matches!(e, TypeError::Mismatch { .. })
    });
    assert_typecheck_err("var t : bool = !3;", |e| {
        matches!(e, TypeError::InvalidUnaryOperator { .. })
    });
}

#[test]
fn function_pass() {
    assert_typecheck_ok("fun add(a : int, b : int) : int { return a + b; }");
    assert_typecheck_ok("fun g() : void { return; }");
    assert_typecheck_ok(
        "
        fun add(a : int, b : int) : int { return a + b; }
        add(1, 2);
    ",
    );
}

#[test]
fn function_fail_mismatch_return() {
    assert_typecheck_err("fun bad(a : int) : float { return a; }", |e| {
        matches!(e, TypeError::Mismatch { .. })
    });
}
#[test]
fn mismatched_parameters_arguments() {
    assert_typecheck_err(
        "
    fun add(a : int, b : int) : int { return a + b; }
    add(1.0, 2);
",
        |e| matches!(e, TypeError::Mismatch { .. }),
    );
}

#[test]
fn invalid_void_function_return() {
    assert_typecheck_err("fun f() : int { return; }", |e| {
        matches!(e, TypeError::Mismatch { .. })
    });
}

#[test]
fn something() {
    assert_typecheck_err("fun h(x : int) : int { if (true) { return x; } }", |e| {
        matches!(e, TypeError::Mismatch { .. })
    });
}

#[test]
fn control_flow_pass() {
    assert_typecheck_ok("if (true) { var x : int = 1; } else { var y : int = 2; }");
    assert_typecheck_ok("while (false) {}");
    assert_typecheck_ok("while (true) { break; }");
}

#[test]
fn control_flow_fail() {
    assert_typecheck_err("if (1) {}", |e| matches!(e, TypeError::Mismatch { .. }));
    assert_typecheck_err("while (1) {}", |e| matches!(e, TypeError::Mismatch { .. }));
    assert_typecheck_err("break;", |e| matches!(e, TypeError::BreakOutsideLoop(_)));
}

#[test]
fn class_pass() {
    assert_typecheck_ok("class A { fun foo() : int { return 1; } }");
    assert_typecheck_ok("class A { fun foo() : int { return 1; } } var a : A = A(); a.foo();");
}

#[test]
fn class_fail() {
    assert_typecheck_err(
        "class A { fun foo() : int { return 1; } } var a : A = A(); a.bar();",
        |e| matches!(e, TypeError::UnknownMethod { .. }),
    );
    assert_typecheck_err(
        "class A { fun foo() : int { return 1; } } var a : A = A(); a.foo(1);",
        |e| matches!(e, TypeError::ArityMismatch { .. }),
    );
}

#[test]
fn edge_cases_fail() {
    assert_typecheck_err("var x : int = 5; x();", |e| {
        matches!(e, TypeError::NotCallable { .. })
    });
    assert_typecheck_err("return 5;", |e| {
        matches!(e, TypeError::ReturnOutsideFunction(_))
    });
    assert_typecheck_err("var x : int = 1; var x : int = 2;", |e| {
        matches!(e, TypeError::Redeclaration(_, _))
    });
    assert_typecheck_err("y = 1;", |e| {
        matches!(e, TypeError::UndeclaredVariable(_, _))
    });
}
