use crate::parser::parser::Parser;
use crate::scanner::scanner::Scanner;
// use crate::scanner::scanner::scan;
use super::type_error::TypeError;
use crate::type_checker::type_checker::TypeChecker;
/// Run the typechecker and return the checker (so we can inspect errors and typed_stmts)
fn run_typecheck(source: &str) -> TypeChecker {
    let tokens = Scanner::new(source).scan_tokens();
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

// ---------------- VAR DECLARATIONS ----------------

#[test]
fn var_int_ok() {
    assert_typecheck_ok("var a : int = 5;");
}

#[test]
fn var_float_ok() {
    assert_typecheck_ok("var b : float = 3.14;");
}

#[test]
fn var_bool_ok() {
    assert_typecheck_ok("var c : bool = true;");
}

#[test]
fn var_infer_bool_ok() {
    assert_typecheck_ok("var e = true;");
}

#[test]
fn basic_well_typed_function_does_not_error() {
    let src = "fun add(a : int, b : int) : int { return a + b; }";
    let tokens = crate::scanner::scanner::Scanner::new(src).scan_tokens();
    let mut parser = crate::parser::parser::Parser::new(tokens);
    let stmts = parser.parse();
    let mut checker = TypeChecker::new();
    checker.check(&stmts);
    assert!(
        checker.errors.is_empty(),
        "Expected no type errors, got: {:?}",
        checker.errors
    );
}

#[test]
fn var_infer_int_ok() {
    assert_typecheck_ok("var f = 42;");
}

#[test]
fn var_declared_uninit_ok() {
    assert_typecheck_ok("var g : int;");
}

#[test]
fn var_type_mismatch_err() {
    assert_typecheck_err("var d : int = 3.14;", |e| {
        matches!(e, TypeError::Mismatch { .. })
    });
}

// ---------------- BINARY / UNARY ----------------

#[test]
fn binary_add_int_ok() {
    assert_typecheck_ok("var x : int = 1 + 2;");
}

#[test]
fn binary_add_float_ok() {
    assert_typecheck_ok("var y : float = 1.0 + 2;");
}

#[test]
fn binary_and_ok() {
    assert_typecheck_ok("var z : bool = true && false;");
}

#[test]
fn unary_not_ok() {
    assert_typecheck_ok("var u : bool = !false;");
}

#[test]
fn unary_neg_ok() {
    assert_typecheck_ok("var v : int = -3;");
}

#[test]
fn binary_mismatch_err() {
    assert_typecheck_err("var w : int = 1 + true;", |e| {
        matches!(e, TypeError::Mismatch { .. })
    });
}

#[test]
fn unary_invalid_err() {
    assert_typecheck_err("var t : bool = !3;", |e| {
        matches!(e, TypeError::InvalidUnaryOperator { .. })
    });
}

#[test]
fn nested_variable_use_ok() {
    assert_typecheck_ok(
        "var t: int = 5; 
    {
        t = 6;
    }");
}

// ---------------- FUNCTIONS ----------------

#[test]
fn func_simple_ok() {
    assert_typecheck_ok("fun add(a : int, b : int) : int { return a + b; }");
}

#[test]
fn func_void_return_ok() {
    assert_typecheck_ok("fun g() : void { return; }");
}

#[test]
fn func_call_ok() {
    assert_typecheck_ok(
        "
        fun add(a : int, b : int) : int { return a + b; }
        add(1, 2);
    ",
    );
}

#[test]
fn func_return_type_mismatch_err() {
    assert_typecheck_err("fun bad(a : int) : float { return a; }", |e| {
        matches!(e, TypeError::Mismatch { .. })
    });
}

#[test]
fn func_call_arg_mismatch_err() {
    assert_typecheck_err(
        "
        fun add(a : int, b : int) : int { return a + b; }
        add(1.0, 2);
    ",
        |e| matches!(e, TypeError::Mismatch { .. }),
    );
}

#[test]
fn func_missing_return_err() {
    assert_typecheck_err("fun f() : int { return; }", |e| {
        matches!(e, TypeError::Mismatch { .. })
    });
}

#[test]
fn func_incomplete_return_err() {
    assert_typecheck_err("fun h(x : int) : int { if (true) { return x; } }", |e| {
        matches!(e, TypeError::NoReturnType { .. })
    });
}

// ---------------- CONTROL FLOW ----------------

#[test]
fn if_else_ok() {
    assert_typecheck_ok("if (true) { var x : int = 1; } else { var y : int = 2; }");
}

#[test]
fn while_false_ok() {
    assert_typecheck_ok("while (false) {}");
}

#[test]
fn while_break_ok() {
    assert_typecheck_ok("while (true) { break; }");
}

#[test]
fn if_condition_type_err() {
    assert_typecheck_err("if (1) {}", |e| matches!(e, TypeError::Mismatch { .. }));
}

#[test]
fn while_condition_type_err() {
    assert_typecheck_err("while (1) {}", |e| matches!(e, TypeError::Mismatch { .. }));
}

#[test]
fn break_outside_loop_err() {
    assert_typecheck_err("break;", |e| matches!(e, TypeError::BreakOutsideLoop(_)));
}

// ---------------- CLASSES ----------------

#[test]
#[ignore]
fn class_decl_ok() {
    assert_typecheck_ok("class A { fun foo() : int { return 1; } }");
}

#[test]
#[ignore]
fn class_method_call_ok() {
    assert_typecheck_ok("class A { fun foo() : int { return 1; } } var a : A = A(); a.foo();");
}

#[test]
#[ignore]
fn class_unknown_method_err() {
    assert_typecheck_err(
        "class A { fun foo() : int { return 1; } } var a : A = A(); a.bar();",
        |e| matches!(e, TypeError::UnknownMethod { .. }),
    );
}

#[test]
#[ignore]
fn class_arity_mismatch_err() {
    assert_typecheck_err(
        "class A { fun foo() : int { return 1; } } var a : A = A(); a.foo(1);",
        |e| matches!(e, TypeError::ArityMismatch { .. }),
    );
}

// ---------------- EDGE CASES ----------------

#[test]
fn call_non_callable_err() {
    assert_typecheck_err("var x : int = 5; x();", |e| {
        matches!(e, TypeError::NotCallable { .. })
    });
}

#[test]
fn return_outside_function_err() {
    assert_typecheck_err("return 5;", |e| {
        matches!(e, TypeError::ReturnOutsideFunction(_))
    });
}

#[test]
fn redeclaration_err() {
    assert_typecheck_err("var x : int = 1; var x : int = 2;", |e| {
        matches!(e, TypeError::Redeclaration(_, _))
    });
}

#[test]
fn assign_undeclared_err() {
    assert_typecheck_err("y = 1;", |e| {
        matches!(e, TypeError::UndeclaredVariable(_, _))
    });
}
