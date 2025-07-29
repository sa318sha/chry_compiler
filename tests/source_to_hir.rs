use chry_compiler::hir;
// use chry_compiler::hir::hir::HIR;
use chry_compiler::hir::hir::HIR;
use chry_compiler::hir::hir_error::HIRError;
use chry_compiler::parser::parser::Parser;
use chry_compiler::scanner::scanner::Scanner;
use chry_compiler::type_checker::type_checker::TypeChecker;
use chry_compiler::types::hir_types::{HIRFunction, HIRGlobal, HIRInstr};
pub fn lower_to_hir(source: &str) -> HIR {
    let tokens = Scanner::new(source).scan_tokens();

    // let tokens = scan(source);
    let stmts = Parser::new(tokens).parse();
    let typed = TypeChecker::new().check(&stmts);
    let mut hir = HIR::new();
    hir.lower_typed_asts(&typed);

    return hir;
    // hir
}

fn hir_err(source: &str) -> HIRError {
    let hir = lower_to_hir(source);

    hir.errors.first().cloned().expect("Expected error")
}

fn hir_function_ok(source: &str) -> Vec<HIRFunction> {
    let hir = lower_to_hir(source);

    hir.functions
}

fn hir_globals_ok(source: &str) -> Vec<HIRGlobal> {
    let hir = lower_to_hir(source);

    hir.globals
}

#[test]
fn test_global_variable_declaration() {
    let source = "var x: int = 5;";
    let globals = hir_globals_ok(source);
    // let func = &hir.functions[0];
    assert!(globals.iter().any(|instr| instr.name.lexeme == "x"));
}

#[test]
fn test_binary_expression() {
    let source = "fun add(): int { var x: int = 1 + 2; return x; }";
    let hir = lower_to_hir(source);
    let func = &hir.functions[0];
    assert!(
        func.body
            .iter()
            .any(|instr| matches!(instr, HIRInstr::BinaryOp { .. }))
    );
}

#[test]
fn test_function_call() {
    let source = "fun id(x: int): int { return x; } fun main(): void { id(1); }";
    let hir = lower_to_hir(source);
    let main_func = hir.functions.iter().find(|f| f.name == "main").unwrap();
    assert!(
        main_func
            .body
            .iter()
            .any(|instr| matches!(instr, HIRInstr::Call { .. }))
    );
}

#[test]
fn test_valid_global_const() {
    let source = "var g: int = 10;";
    let globals = hir_globals_ok(source);
    assert!(globals.iter().any(|g| g.name.lexeme == "g"));
}

#[test]
fn test_invalid_global_expr() {
    let source = "var g: int = 1 + 2;";
    let err = hir_err(source);
    assert!(matches!(err, HIRError::InvalidGlobalStatement { .. }));
}

#[test]
fn test_local_variable_assignment() {
    let source = "fun main(): void { var x: int = 42; }";
    let func = &lower_to_hir(source).functions[0];
    assert!(
        func.body
            .iter()
            .any(|i| matches!(i, HIRInstr::StoreVar { .. }))
    );
}

#[test]
fn test_expression_only_statement() {
    let source = "fun main(): void { 1 + 2; }";
    let func = &lower_to_hir(source).functions[0];
    assert!(
        func.body
            .iter()
            .any(|i| matches!(i, HIRInstr::BinaryOp { .. }))
    );
}

#[test]
fn test_unary_negation() {
    let source = "fun f(): void { var x: int = -5; }";
    let func = &lower_to_hir(source).functions[0];
    assert!(
        func.body
            .iter()
            .any(|i| matches!(i, HIRInstr::UnaryOp { .. }))
    );
}

#[test]
fn test_grouping_expression() {
    let source = "fun f(): void { var x: int = (1 + 2); }";
    let func = &lower_to_hir(source).functions[0];
    assert!(
        func.body
            .iter()
            .any(|i| matches!(i, HIRInstr::BinaryOp { .. }))
    );
}

#[test]
fn test_simple_if() {
    let source = "fun f(): void { if (true) { print 1; } }";
    let func = &lower_to_hir(source).functions[0];
    assert!(func.body.iter().any(|i| matches!(i, HIRInstr::If { .. })));
}

#[test]
fn test_if_else_structure() {
    let source = "fun f(): void { if (true) { print 1; } else { print 2; } }";
    let func = &lower_to_hir(source).functions[0];
    assert!(
        func.body
            .iter()
            .filter(|i| matches!(i, HIRInstr::If { .. }))
            .count()
            == 1
    );
}

#[test]
fn test_while_with_break() {
    let source = "fun f(): void { while (true) { break; } }";
    let func = &lower_to_hir(source).functions[0];
    assert!(func.body.iter().any(|i| matches!(i, HIRInstr::Goto { .. })));
}

#[test]
fn test_large_function_body() {
    let mut src = String::new();
    for i in 0..100 {
        src.push_str(&format!("print {};\n", i));
    }
    let source = format!("fun main(): void {{ {} }}", src);
    let hir = lower_to_hir(&source);
    let func = &hir.functions[0];
    let print_count = func
        .body
        .iter()
        .filter(|i| matches!(i, HIRInstr::Print { .. }))
        .count();
    assert_eq!(print_count, 100);
}

#[test]
fn test_pretty_print_contains_function() {
    let source = "fun f(): int { return 5; }";
    let hir = lower_to_hir(source);
    let pretty = hir.pretty_print_program();
    assert!(pretty.contains("fun f"));
    assert!(pretty.contains("Return"));
}

// #[test]
// fn test_missing_return_in_non_void_function() {
//     let source = "fun f(): int { var x: int = 10; }";
//     let err = hir_err(source);
//     assert!(matches!(err, HIRError::ExpectedReturn { .. }));
// }

// #[test]
// fn test_function_call_with_wrong_arity() {
//     let source = "fun id(x: int): int { return x; } fun main(): void { id(); }";
//     let err = hir_err(source);
//     assert!(matches!(err, HIRError::ArityMismatch { .. }));
// }

// #[test]
// fn test_undefined_variable_use() {
//     let source = "fun f(): void { print x; }";
//     let err = hir_err(source);
//     assert!(matches!(err, HIRError::UndefinedVariable { .. }));
// }

// #[test]
// fn test_return_outside_function() {
//     let source = "return 5;";
//     let err = hir_err(source);
//     assert!(matches!(err, HIRError::InvalidGlobalStatement { .. }));
// }

#[test]
fn test_logical_and() {
    let source = "fun f(): void { var x: bool = true && false; }";
    let func = &lower_to_hir(source).functions[0];
    assert!(func.body.iter().any(|i| matches!(i, HIRInstr::If { .. })));
}

#[test]
fn test_logical_or() {
    let source = "fun f(): void { var x: bool = false || true; }";
    let func = &lower_to_hir(source).functions[0];
    assert!(func.body.iter().any(|i| matches!(i, HIRInstr::If { .. })));
}

#[test]
fn test_variable_assignment_expression() {
    let source = "fun f(): void { var x: int = 0; x = 5; }";
    let func = &lower_to_hir(source).functions[0];
    let store_count = func
        .body
        .iter()
        .filter(|i| matches!(i, HIRInstr::StoreVar { .. }))
        .count();
    assert!(store_count >= 2); // declaration + assignment
}

#[test]
fn test_if_inside_while() {
    let source = "fun f(): void { while (true) { if (false) { print 1; } } }";
    let func = &lower_to_hir(source).functions[0];
    let ifs = func
        .body
        .iter()
        .filter(|i| matches!(i, HIRInstr::If { .. }))
        .count();
    assert!(ifs >= 2); // one for while, one for if
}
