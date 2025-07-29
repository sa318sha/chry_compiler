use crate::hir::hir::HIR;
use crate::hir::hir_error::HIRError;
use crate::type_checker::type_checker::TypeChecker;
use crate::types::hir_types::{HIRInstr, Label, TempId};
use crate::types::literal::Literal;
use crate::types::token::Token;
use crate::types::token_type::TokenType;
use crate::types::typed_expr::{TypedExpr, TypedExprKind};
use crate::types::typed_stmt::{TypedStmt, pretty_print_typed_stmt};
use crate::types::types::Type;
use proptest::prelude::*;
use std::collections::HashSet;

fn lower_program_to_instrs(stmts: &[TypedStmt]) -> Result<Vec<HIRInstr>, Vec<String>> {
    let mut hir = HIR::new();
    hir.lower_typed_asts(&stmts.to_vec());
    if !hir.errors.is_empty() {
        return Err(hir.errors.iter().map(|e| format!("{:?}", e)).collect());
    }
    Ok(hir
        .functions
        .iter()
        .find(|f| f.name == "main")
        .map(|f| f.body.clone())
        .unwrap_or_default())
}

fn lower_program_report_errors(prog: &[TypedStmt]) -> Vec<HIRInstr> {
    let instrs = match lower_program_to_instrs(&prog) {
        Ok(i) => i,
        Err(errors) => {
            // Format the program to help debugging
            let input_str = prog
                .iter()
                .map(pretty_print_typed_stmt)
                .collect::<Vec<_>>()
                .join("\n");
            panic!(
                "HIR lowering failed:\nInput:\n{}\nErrors:\n{:#?}",
                input_str, errors
            );
        }
    };

    return instrs;
}

fn dummy_token(name: &str) -> Token {
    Token {
        lexeme: name.to_string(),
        line: 0,
        tokentype: TokenType::Identifier,
        literal: None,
    }
}


fn arb_token() -> impl Strategy<Value = Token> {
    any::<u32>().prop_map(|line| Token {
        lexeme: format!("t{}", line),
        line,
        tokentype: TokenType::Identifier,
        literal: None,
    })
}

fn arb_literal_expr() -> impl Strategy<Value = TypedExpr> {
    any::<i32>().prop_map(|n| TypedExpr {
        kind: TypedExprKind::Literal(Literal::Int(n)),
        ty: Type::Int,
    })
}

fn arb_literal() -> impl Strategy<Value = Literal> {
    prop_oneof![
        any::<i32>().prop_map(Literal::Int),
        any::<bool>().prop_map(Literal::Bool),
    ]
}

fn arb_print_stmt() -> impl Strategy<Value = TypedStmt> {
    arb_literal_expr().prop_map(|expr| TypedStmt::Print(Box::new(expr)))
}

fn arb_large_body_program() -> impl Strategy<Value = Vec<TypedStmt>> {
    prop::collection::vec(arb_print_stmt(), 30..60).prop_map(|stmts| {
        let func = TypedStmt::Function(
            dummy_token("main"),
            Box::new(vec![]),
            Box::new(stmts),
            Type::Void,
        );
        vec![func]
    })
}

fn arb_nested_if_stmt(depth: u32) -> BoxedStrategy<TypedStmt> {
    if depth == 0 {
        return arb_print_stmt().boxed();
    }
    (
        arb_literal_expr(),
        arb_nested_if_stmt(depth - 1),
        arb_nested_if_stmt(depth - 1),
    )
        .prop_map(|(cond, then_stmt, else_stmt)| {
            TypedStmt::If(
                Box::new(cond),
                Box::new(then_stmt),
                Box::new(Some(else_stmt)),
            )
        })
        .boxed()
}

fn arb_nested_control_flow_program() -> impl Strategy<Value = Vec<TypedStmt>> {
    arb_nested_if_stmt(5).prop_map(|nested| {
        vec![TypedStmt::Function(
            dummy_token("main"),
            Box::new(vec![]),
            Box::new(vec![nested]),
            Type::Void,
        )]
    })
}

fn arb_type() -> impl Strategy<Value = Type> {
    prop_oneof![Just(Type::Int), Just(Type::Bool), Just(Type::Void)]
}

fn arb_expr(depth: u32) -> impl Strategy<Value = TypedExpr> {
    let leaf = arb_literal().prop_map(|lit| TypedExpr {
        kind: TypedExprKind::Literal(lit.clone()),
        ty: match lit {
            Literal::Int(_) => Type::Int,
            Literal::Bool(_) => Type::Bool,
            _ => Type::Void,
        },
    });

    leaf.prop_recursive(depth, 4, 4, |inner| {
        prop_oneof![
            (arb_token(), inner.clone()).prop_map(|(tok, e)| TypedExpr {
                kind: TypedExprKind::Unary(tok.clone(), Box::new(e.clone())),
                ty: e.ty.clone(),
            }),
            (inner.clone(), arb_token(), inner.clone()).prop_map(|(lhs, op, rhs)| TypedExpr {
                kind: TypedExprKind::Binary(
                    Box::new(lhs.clone()),
                    op.clone(),
                    Box::new(rhs.clone())
                ),
                ty: lhs.ty.clone(),
            }),
            inner.clone().prop_map(|e| TypedExpr {
                kind: TypedExprKind::Grouping(Box::new(e.clone())),
                ty: e.ty.clone(),
            }),
        ]
    })
}

fn arb_global_var() -> impl Strategy<Value = TypedStmt> {
    (arb_token(), arb_type(), arb_literal()).prop_map(|(tok, ty, lit)| {
        let expr = TypedExpr {
            kind: TypedExprKind::Literal(lit.clone()),
            ty: match lit {
                Literal::Int(_) => Type::Int,
                Literal::Bool(_) => Type::Bool,
                _ => Type::Void,
            },
        };
        TypedStmt::Variable(tok, ty, Some(expr))
    })
}

fn arb_func_body_stmt() -> impl Strategy<Value = TypedStmt> {
    (arb_token(), arb_type(), arb_expr(2))
        .prop_map(|(tok, ty, expr)| TypedStmt::Variable(tok, ty, Some(expr)))
}

fn arb_valid_program() -> impl Strategy<Value = Vec<TypedStmt>> {
    prop::collection::vec(arb_global_var(), 0..3).prop_flat_map(|globals| {
        prop::collection::vec(arb_func_body_stmt(), 1..5).prop_map(move |body_stmts| {
            let func = TypedStmt::Function(
                Token {
                    lexeme: "main".to_string(),
                    line: 0,
                    tokentype: TokenType::Identifier,
                    literal: None,
                },
                Box::new(vec![]),
                Box::new(body_stmts),
                Type::Void,
            );
            let mut prog = globals.clone();
            prog.push(func);
            prog
        })
    })
}

fn extract_tempids(instrs: &[HIRInstr]) -> HashSet<TempId> {
    let mut temps = HashSet::new();
    for instr in instrs {
        match instr {
            HIRInstr::Const { dest, .. }
            | HIRInstr::UnaryOp { dest, .. }
            | HIRInstr::BinaryOp { dest, .. }
            | HIRInstr::LoadVar { dest, .. }
            | HIRInstr::Call { dest, .. }
            | HIRInstr::Move { dest, .. } => {
                temps.insert(*dest);
            }
            _ => {}
        }
    }
    temps
}

fn extract_labels(instrs: &[HIRInstr]) -> HashSet<Label> {
    instrs.iter().filter_map(|i| match i {
        HIRInstr::Label { label } => Some(label.clone()),
        _ => None,
    }).collect()
}



pub fn extract_label_targets(instrs: &[HIRInstr]) -> HashSet<Label> {
    let mut targets = HashSet::new();
    for instr in instrs {
        match instr {
            HIRInstr::If {
                then_label,
                else_label,
                ..
            } => {
                targets.insert(then_label.clone());
                targets.insert(else_label.clone());
            }
            HIRInstr::Goto { target } => {
                targets.insert(target.clone());
            }
            _ => {}
        }
    }
    targets
}


proptest! {
    #[test]
    fn nested_control_flow_does_not_panic(prog in arb_nested_control_flow_program()) {
        let _ = lower_program_to_instrs(&prog).unwrap();
    }

    #[test]
    fn large_program_does_not_panic(prog in arb_large_body_program()) {
        let _ = lower_program_to_instrs(&prog).unwrap();
    }

    #[test]
    fn deep_nesting_preserves_hir_shape(prog in arb_nested_control_flow_program()) {
        let instrs = lower_program_to_instrs(&prog).unwrap();
        let label_count = extract_labels(&instrs).len();
        let target_count = extract_label_targets(&instrs).len();
        prop_assert!(label_count >= target_count);
    }

    #[test]
    fn deterministic_lowering_for_large_program(prog in arb_large_body_program()) {
        let first = lower_program_to_instrs(&prog).unwrap();
        let second = lower_program_to_instrs(&prog).unwrap();
        prop_assert_eq!(first, second);
    }
}

