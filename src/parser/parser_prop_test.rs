// proptest-based parser property test suite

use super::parse_error::ParseError;
use crate::parser::parser::Parser;
use crate::scanner::scanner::Scanner;
use crate::types::expr::Expr;
use crate::types::literal::Literal;
use crate::types::stmt::{Stmt, pretty_print_stmt};
use crate::types::types::Type;
use proptest::prelude::*;

// === Properties ===

fn simple_literal_program() -> impl Strategy<Value = String> {
    (arb_literal()).prop_map(|(lit)| format!("{};", lit))
}

proptest! {

    #[test]
    fn deep_expression_nesting_does_not_panic(depth in 10..20usize) {
        let nested = "(".repeat(depth) + "1" + &")".repeat(depth) + ";";
        // println!("{}",nested);
        let tokens = Scanner::new(&nested).scan_tokens();
        let mut parser = Parser::new(tokens);
        let _ = parser.parse();  // Ensure no panic
    }

    #[test]
    fn mixed_control_flow_does_not_panic(src in arb_mixed_control_flow_program()) {
        let tokens = Scanner::new(&src).scan_tokens();
        let mut parser = Parser::new(tokens);
        let _ = parser.parse();  // Ensure no panic
    }


    #[test]
    fn var_declaration_simple(src in simple_literal_program()) {
        let tokens = Scanner::new(&src).scan_tokens();
        let mut parser = Parser::new(tokens);
        let _ = parser.parse();

    }


    #[test]
    fn parse_does_not_panic_on_control_flow_program(src in arb_valid_program_with_control_flow()) {
        let tokens = Scanner::new(&src).scan_tokens();
        let mut parser = Parser::new(tokens);
        let _ = parser.parse();
    }
    #[test]
    fn parse_does_not_panic_on_valid_inputs(src in arb_valid_program()) {
        let tokens = Scanner::new(&src).scan_tokens();
        let mut parser = Parser::new(tokens);
        let _ast = parser.parse();
        // If we don't panic, test passes
    }

    #[test]
    fn parse_reports_error_on_invalid_input(src in arb_invalid_program()) {
        let tokens = Scanner::new(&src).scan_tokens();
        let result = std::panic::catch_unwind(|| {
            let mut parser = Parser::new(tokens);
            parser.parse()
        });
        prop_assert!(result.is_ok(), "Parser panicked rather than reporting error");
    }

    #[test]
    fn parse_print_parse_round_trip(src in arb_valid_program()) {

        let tokens = Scanner::new(&src).scan_tokens();
        let mut parser = Parser::new(tokens);
        let ast1 = parser.parse();

        let printed = pretty_print(&ast1);

        println!("initial src:      {}", src);
        println!("recreated src:    {}", printed);

        let tokens2 = Scanner::new(&printed).scan_tokens();
        let mut parser2 = Parser::new(tokens2);
        let ast2 = parser2.parse();

        prop_assert_eq!(ast1, ast2, "ASTs differ after round-trip parse");
    }
}

// === Helpers ===

pub fn pretty_print(stmts: &Vec<Stmt>) -> String {
    stmts
        .iter()
        .map(|stmt| pretty_print_stmt(stmt))
        .collect::<Vec<_>>()
        .join(" ")
}

// === Generators ===

fn arb_identifier() -> impl Strategy<Value = String> {
    // A simple identifier regex: [a-zA-Z_][a-zA-Z0-9_]*
    "[a-zA-Z_][a-zA-Z0-9_]{0,10}".prop_map(|s| s.to_string())
}

fn arb_mixed_control_flow_program() -> impl Strategy<Value = String> {
    prop::collection::vec(
        prop_oneof![arb_if_stmt(), arb_while_stmt(), arb_for_stmt(),],
        1..3,
    )
    .prop_map(|stmts| stmts.join(" "))
}

/// Generates a boolean expression: `true`, `false`, or a comparison
fn arb_bool_expr() -> impl Strategy<Value = String> {
    prop_oneof![
        Just("true".to_string()),
        Just("false".to_string()),
        arb_comparison_expr(),
        arb_logical_expr()
    ]
}

/// Generates a comparison: e.g., `x == y`, `x < y`
fn arb_comparison_expr() -> impl Strategy<Value = String> {
    (arb_identifier(), arb_comparison_op(), arb_literal())
        .prop_map(|(lhs, op, rhs)| format!("{} {} {}", lhs, op, rhs))
}

/// Comparison operators
fn arb_comparison_op() -> impl Strategy<Value = String> {
    prop_oneof![
        Just("==".to_string()),
        Just("!=".to_string()),
        Just("<".to_string()),
        Just("<=".to_string()),
        Just(">".to_string()),
        Just(">=".to_string())
    ]
}

/// Generates logical expressions: e.g., `x == y && z != w`
fn arb_logical_expr() -> impl Strategy<Value = String> {
    (
        arb_comparison_expr(),
        arb_logical_op(),
        arb_comparison_expr(),
    )
        .prop_map(|(lhs, op, rhs)| format!("{} {} {}", lhs, op, rhs))
}

/// Logical operators
fn arb_logical_op() -> impl Strategy<Value = String> {
    prop_oneof![Just("&&".to_string()), Just("||".to_string())]
}

/// Generates an `if` statement: `if (cond) stmt else stmt`
fn arb_if_stmt() -> impl Strategy<Value = String> {
    (arb_bool_expr(), arb_simple_stmt(), arb_simple_stmt()).prop_map(
        |(cond, then_stmt, else_stmt)| format!("if ({}) {} else {}", cond, then_stmt, else_stmt),
    )
}

/// Generates a `while` loop
fn arb_while_stmt() -> impl Strategy<Value = String> {
    (arb_bool_expr(), arb_simple_stmt())
        .prop_map(|(cond, body)| format!("while ({}) {}", cond, body))
}

/// Generates a `for` loop
fn arb_for_stmt() -> impl Strategy<Value = String> {
    (
        arb_var_decl(),
        arb_bool_expr(),
        arb_identifier().prop_map(|id| format!("{} = {}", id, rand_int_literal())),
        arb_simple_stmt(),
    )
        .prop_map(|(init, cond, incr, body)| format!("for ({} {}; {}) {}", init, cond, incr, body))
}

/// Generates a simple statement (either var decl or expression statement)
fn arb_simple_stmt() -> impl Strategy<Value = String> {
    prop_oneof![arb_var_decl(), arb_expr_stmt()]
}

/// Generates an expression statement
fn arb_expr_stmt() -> impl Strategy<Value = String> {
    arb_comparison_expr().prop_map(|expr| format!("{};", expr))
}

/// Utility: random int literal as string
fn rand_int_literal() -> String {
    use rand::Rng;
    rand::rng().random_range(0..100).to_string()
}

/// Generates a valid program with control flow
fn arb_valid_program_with_control_flow() -> impl Strategy<Value = String> {
    prop::collection::vec(
        prop_oneof![
            arb_var_decl(),
            arb_function_decl(),
            arb_if_stmt(),
            arb_while_stmt(),
            arb_for_stmt(),
        ],
        1..5,
    )
    .prop_map(|stmts| stmts.join(" "))
}

// fn single_var_program() -> impl Strategy<Value = String> {
//     arb_var_decl()
// }

fn arb_type() -> impl Strategy<Value = String> {
    prop_oneof![
        Just("int".to_string()),
        Just("float".to_string()),
        Just("bool".to_string()),
        Just("void".to_string())
    ]
}

fn arb_literal() -> impl Strategy<Value = String> {
    prop_oneof![
        any::<i32>().prop_map(|n| n.to_string()),
        any::<f64>().prop_filter_map("filter floats", |f| {
            if f.is_finite() {
                Some(format!("{:.6}", f)) // fixed-point with 6 decimal places
            } else {
                None // skip infinities/NaN
            }
        }),
        Just("true".to_string()),
        Just("false".to_string()),
        // Skipping string literals for simplicity
    ]
}

fn arb_var_decl() -> impl Strategy<Value = String> {
    (arb_identifier(), arb_type(), arb_literal())
        .prop_map(|(id, ty, lit)| format!("var {}: {} = {};", id, ty, lit))
}

fn arb_function_decl() -> impl Strategy<Value = String> {
    (
        arb_identifier(),
        arb_type(),
        prop::collection::vec((arb_identifier(), arb_type()), 0..3),
        arb_var_decl(),
    )
        .prop_map(|(name, ret_ty, params, body_stmt)| {
            let param_list = params
                .into_iter()
                .map(|(id, ty)| format!("{}: {}", id, ty))
                .collect::<Vec<_>>()
                .join(", ");
            format!(
                "fun {}({}): {} {{ {} }}",
                name, param_list, ret_ty, body_stmt
            )
        })
}

fn arb_valid_program() -> impl Strategy<Value = String> {
    prop::collection::vec(
        prop_oneof![
            arb_var_decl(),
            arb_function_decl(),
            Just("class C {}".to_string()),
        ],
        1..5,
    )
    .prop_map(|stmts| stmts.join(" "))
}

fn arb_invalid_program() -> impl Strategy<Value = String> {
    prop_oneof![
        Just("var 123;".to_string()),                // invalid identifier
        Just("fun (): int { }".to_string()),         // missing function name
        Just("if true var x: int = 1;".to_string()), // missing parentheses
        Just("var x int = 5;".to_string()),          // missing colon
    ]
}
