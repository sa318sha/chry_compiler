use crate::types::expr::Expr;
use crate::types::literal::Literal;
use crate::types::stmt::{Stmt, pretty_print_stmt};
use crate::types::token::Token;
use crate::types::token_type::TokenType;
use crate::types::types::Type;
use crate::{type_checker::type_checker::TypeChecker, types::typed_stmt::pretty_print_typed_stmt};
use proptest::prelude::*;
use proptest::strategy::BoxedStrategy;
use std::io::Write;
use std::{
    fs::File,
    sync::atomic::{AtomicUsize, Ordering},
};

static TEST_ID: AtomicUsize = AtomicUsize::new(0);

fn dump_case_to_file(stmts: &[Stmt], test_name: &str, welltyped_output: Option<&[String]>) {
    let id = TEST_ID.fetch_add(1, Ordering::SeqCst);
    let filename = format!("failed_case_{}_{}.chry", test_name, id);
    if let Ok(mut file) = File::create(&filename) {
        writeln!(file, "// Test failure: {}", test_name).ok();
        for stmt in stmts {
            let _ = writeln!(file, "{}", pretty_print_stmt(stmt));
        }
        if let Some(typed_stmts) = welltyped_output {
            writeln!(file, "\n// Typed Output:").ok();
            for typed in typed_stmts {
                writeln!(file, "{}", typed).ok();
            }
        }
    }
}

fn arb_var_name() -> impl Strategy<Value = String> {
    "[a-z][a-z0-9_]{0,5}".prop_map(|s| s.to_string())
}

fn arb_literal() -> impl Strategy<Value = Expr> {
    prop_oneof![
        any::<i32>().prop_map(|n| Expr::Literal(Literal::Int(n))),
        any::<f64>().prop_filter_map("finite float", |f| {
            if f.is_finite() {
                Some(Expr::Literal(Literal::Float(f, format!("{:.6}", f))))
            } else {
                None
            }
        }),
        any::<bool>().prop_map(|b| Expr::Literal(Literal::Bool(b)))
    ]
}

fn arb_variable_expr() -> impl Strategy<Value = Expr> {
    arb_var_name().prop_map(|name| Expr::Variable(Token::dummy_identifier(&name)))
}

fn arb_token_type() -> impl Strategy<Value = TokenType> {
    prop_oneof![
        Just(TokenType::Plus),
        Just(TokenType::Minus),
        Just(TokenType::Star),
        Just(TokenType::Slash),
        Just(TokenType::EqualEqual),
        Just(TokenType::BangEqual),
        Just(TokenType::Less),
        Just(TokenType::Greater),
        Just(TokenType::LessEqual),
        Just(TokenType::GreaterEqual)
    ]
}
fn arb_binary_expr(depth: u32) -> BoxedStrategy<Expr> {
    (arb_expr(depth), arb_token_type(), arb_expr(depth))
        .prop_map(|(l, op, r)| {
            let tok = Token::dummy_with_type(op);
            Expr::Binary(Box::new(l), tok, Box::new(r))
        })
        .boxed()
}

fn arb_expr(depth: u32) -> BoxedStrategy<Expr> {
    let leaf = prop_oneof![arb_literal(), arb_variable_expr()];
    leaf.prop_recursive(depth, 8, 2, move |inner| {
        prop_oneof![
            arb_binary_expr(depth - 1),
            inner
                .clone()
                .prop_map(|e| Expr::Grouping(Box::new(e)))
                .boxed(),
            inner
                .clone()
                .prop_map(|e| Expr::Unary(Token::dummy_with_type(TokenType::Minus), Box::new(e)))
                .boxed(),
            (inner.clone(), inner.clone())
                .prop_map(|(l, r)| {
                    Expr::Logical(
                        Box::new(l),
                        Token::dummy_with_type(TokenType::BangEqual),
                        Box::new(r),
                    )
                })
                .boxed()
        ]
    })
    .boxed()
}

fn arb_stmt(depth: u32) -> BoxedStrategy<Stmt> {
    let expr = arb_expr(depth);
    let var_stmt = (arb_var_name(), expr.clone()).prop_map(|(name, e)| {
        let token = Token::dummy_identifier(&name);
        Stmt::Variable(token, Some(Type::Int), Some(e))
    });

    let print_stmt = expr.clone().prop_map(|e| Stmt::Print(Box::new(e)));
    let expr_stmt = expr.clone().prop_map(|e| Stmt::Expression(Box::new(e)));

    if depth == 0 {
        return prop_oneof![var_stmt, print_stmt, expr_stmt,].boxed();
    }

    let if_stmt = (arb_expr(depth), arb_stmt(depth - 1), arb_stmt(depth - 1)).prop_map(
        |(cond, then_s, else_s)| Stmt::If(Box::new(cond), Box::new(then_s), Box::new(Some(else_s))),
    );

    let block_stmt = prop::collection::vec(arb_stmt(depth - 1), 1..3)
        .prop_map(|stmts| Stmt::Block(Box::new(stmts)));

    let while_stmt = (arb_expr(depth), arb_stmt(depth - 1))
        .prop_map(|(cond, body)| Stmt::While(Box::new(cond), Box::new(body)));

    let return_stmt = arb_expr(depth)
        .prop_map(|e| Stmt::Return(Token::dummy_keyword("return"), Box::new(Some(e))));

    prop_oneof![
        var_stmt,
        print_stmt,
        expr_stmt,
        if_stmt.boxed(),
        block_stmt.boxed(),
        while_stmt.boxed(),
        return_stmt.boxed(),
    ]
    .boxed()
}

proptest! {
    #[test]
    fn stress_type_checker_on_statements(stmts in prop::collection::vec(arb_stmt(3), 2..6)) {
        let mut checker = TypeChecker::new();
        let _ = checker.check(&stmts);
        // Accept either successful typechecking or a well-formed error set.
        prop_assert!(checker.errors.len() == 0);
    }

    #[test]
    fn stress_type_checker_with_failure_dump(stmts in prop::collection::vec(arb_stmt(3), 2..10)) {
        let mut checker = TypeChecker::new();
        let result = checker.check(&stmts);

        let is_welltyped = checker.errors.is_empty();
        if is_welltyped {
            // Try pretty printing all typed output. If any panic or error, dump source.
            let outputs: Vec<_> = result.iter().map(|s| pretty_print_typed_stmt(s)).collect();
            for line in &outputs {
                if line.contains("\u{0}") {
                    dump_case_to_file(&stmts, "welltyped_print_error", Some(&outputs));
                    panic!("Pretty printing failed on welltyped case.");
                }
            }
        } else if checker.errors.iter().any(|e| matches!(e, crate::type_checker::type_error::TypeError::Other(_))) {
            dump_case_to_file(&stmts, "type_error_other", None);
            panic!("Type checker returned TypeError::Other");
        }

        prop_assert!(true);
    }
}
