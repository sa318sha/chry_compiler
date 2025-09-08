use crate::ssa::block_builder::pretty_print_ssa_blocks;
use crate::ssa::cfg::CFG;
use crate::ssa::ssa_state::SSAState;
use crate::ssa::ssa_version::SSAVersion;
use crate::types::hir_types::pretty_print_hir_instr;
// Adjust path if needed
use crate::types::literal::Literal;
use crate::types::token::Token;
use crate::types::types::Type;
use crate::{
    ssa::{block_builder::BlockBuilder, dominator::Dominator},
    types::hir_types::{HIRInstr, Label, TempId},
};

// Utility to run phi insertion and extract inserted phi set for a block
fn run_phi_insertion(instrs: Vec<HIRInstr>, target: &Label) -> Vec<String> {
    let mut ssa_version = SSAVersion::new();

    let mut builder = BlockBuilder::new();
    builder.create_blocks(&instrs,&mut ssa_version);
    builder.calculate_preds();

    let mut cfg = CFG::from_blocks(&builder.block_instrs, &builder.block_order);
    cfg.dfs_entry();

    let mut dom = Dominator::new(cfg.pre_order.len());
    dom.compute_semi_dominators(&cfg);
    dom.compute_immediate_dominators(&cfg);
    dom.finalize_immediate_dominators(&cfg);
    dom.construct_dominator_tree(&cfg);
    dom.construct_dominance_frontiers(&cfg);

    dom.pretty_print_dominator_tree(&cfg);
    dom.pretty_print_dominance_frontier(&cfg);
    // builder.pretty_print_ssa_blocks();
    println!();
    for i in instrs {
        println!("{}", pretty_print_hir_instr(&i, 2));
    }

    let mut ssa = SSAState::new();
    ssa.insert_phi_nodes(
        &cfg,
        &mut builder.defsites,
        &dom.dominance_frontier,
        &mut builder.block_instrs,
    );
    pretty_print_ssa_blocks(&builder.block_instrs, &builder.block_order);

    builder
        .block_instrs
        .get(target)
        .map(|blk| blk.phis.keys().cloned().collect())
        .unwrap_or_default()
}

#[test]
fn test_phi_single_definition() {
    let l0 = Label("L0".into());
    let l1 = Label("L1".into());
    let l2 = Label("L2".into());
    let token_x = Token::dummy_identifier("x");
    let t = TempId(1);

    let instrs = vec![
        HIRInstr::Label { label: l0.clone() },
        HIRInstr::Goto { target: l1.clone() },
        HIRInstr::Label { label: l1.clone() },
        HIRInstr::StoreVar {
            var: token_x.clone(),
            src: t,
            ty: Type::Int,
        },
        HIRInstr::LoadVar {
            var: token_x.clone(),
            dest: t,
            ty: Type::Int,
        },
        HIRInstr::Goto { target: l2.clone() },
        HIRInstr::Label { label: l2.clone() },
        HIRInstr::Return {
            value: Some(t),
            ty: Type::Int,
        },
    ];

    let phi_vars = run_phi_insertion(instrs, &l2);
    assert!(phi_vars.is_empty());
}

#[test]
fn test_phi_insertion_diamond() {
    let l0 = Label("L0".into());
    let l1 = Label("L1".into());
    let l2 = Label("L2".into());
    let l3 = Label("L3".into());
    let token_x = Token::dummy_identifier("x");
    let t = TempId(1);

    let instrs = vec![
        HIRInstr::Label { label: l0.clone() },
        // HIRInstr::StoreVar {
        //     var: token_x.clone(),
        //     src: t,
        //     ty: Type::Int,
        // },
        HIRInstr::If {
            cond: TempId(0),
            then_label: l1.clone(),
            else_label: l2.clone(),
        },
        HIRInstr::Label { label: l1.clone() },
        HIRInstr::StoreVar {
            src: TempId(2),
            var: token_x.clone(),
            ty: Type::Int,
        },
        HIRInstr::Goto { target: l3.clone() },
        HIRInstr::Label { label: l2.clone() },
        HIRInstr::StoreVar {
            src: TempId(3),
            var: token_x.clone(),
            ty: Type::Int,
        },
        HIRInstr::Goto { target: l3.clone() },
        HIRInstr::Label { label: l3.clone() },
        HIRInstr::LoadVar {
            dest: TempId(4),
            var: token_x.clone(),
            ty: Type::Int,
        },
        HIRInstr::Return {
            value: Some(t),
            ty: Type::Int,
        },
    ];

    let phi_vars = run_phi_insertion(instrs, &l3);
    assert_eq!(phi_vars, vec!["x"]);
}

#[test]
fn test_phi_redundant_phi_avoidance() {
    let l0 = Label("L0".into());
    let l1 = Label("L1".into());
    let l2 = Label("L2".into());
    let l3 = Label("L3".into());
    let token_x = Token::dummy_identifier("x");
    let t = TempId(1);

    let instrs = vec![
        HIRInstr::Label { label: l0.clone() },
        HIRInstr::If {
            cond: TempId(0),
            then_label: l1.clone(),
            else_label: l2.clone(),
        },
        HIRInstr::Label { label: l1.clone() },
        HIRInstr::Goto { target: l3.clone() },
        HIRInstr::Label { label: l2.clone() },
        HIRInstr::StoreVar {
            var: token_x.clone(),
            src: t,
            ty: Type::Int,
        },
        HIRInstr::Goto { target: l3.clone() },
        HIRInstr::Label { label: l3.clone() },
        HIRInstr::Return {
            value: Some(t),
            ty: Type::Int,
        },
    ];

    let phi_vars = run_phi_insertion(instrs, &l3);
    assert_eq!(phi_vars, Vec::<String>::new());
}

#[test]
fn test_phi_single_branch_redefinition() {
    let l0 = Label("L0".into());
    let l1 = Label("L1".into());
    let l2 = Label("L2".into());
    let l3 = Label("L3".into());
    let token_y = Token::dummy_identifier("y");
    let t = TempId(1);

    let instrs = vec![
        HIRInstr::Label { label: l0.clone() },
        // Declare `y` initially
        HIRInstr::StoreVar {
            var: token_y.clone(),
            src: t,
            ty: Type::Int,
        },
        HIRInstr::If {
            cond: TempId(0),
            then_label: l1.clone(),
            else_label: l2.clone(),
        },
        HIRInstr::Label { label: l1.clone() },
        // Redefine `y` in L1
        HIRInstr::StoreVar {
            var: token_y.clone(),
            src: t,
            ty: Type::Int,
        },
        HIRInstr::Goto { target: l3.clone() },
        HIRInstr::Label { label: l2.clone() },
        // No redefinition here!
        HIRInstr::Goto { target: l3.clone() },
        HIRInstr::Label { label: l3.clone() },
        // Use `y` in L3
        HIRInstr::LoadVar {
            var: token_y.clone(),
            dest: TempId(2),
            ty: Type::Int,
        },
        HIRInstr::Return {
            value: Some(t),
            ty: Type::Int,
        },
    ];

    let mut phi_vars = run_phi_insertion(instrs, &l3);
    phi_vars.sort();
    assert_eq!(phi_vars, vec!["y"]);
    // assert!(phi_vars.is_empty());
}

#[test]
fn test_phi_control_flow_definition() {
    let l0 = Label("L0".into());
    let l1 = Label("L1".into());
    let l2 = Label("L2".into());
    let l3 = Label("L3".into());
    let token_x = Token::dummy_identifier("x");
    let t = TempId(1);

    let instrs = vec![
        HIRInstr::Label { label: l0.clone() },
        HIRInstr::StoreVar {
            var: token_x.clone(),
            src: t,
            ty: Type::Int,
        },
        HIRInstr::If {
            cond: TempId(0),
            then_label: l1.clone(),
            else_label: l2.clone(),
        },
        HIRInstr::Label { label: l1.clone() },
        HIRInstr::Goto { target: l3.clone() },
        HIRInstr::Label { label: l2.clone() },
        HIRInstr::Goto { target: l3.clone() },
        HIRInstr::Label { label: l3.clone() },
        HIRInstr::LoadVar {
            var: token_x.clone(),
            dest: TempId(2),
            ty: Type::Int,
        },
        HIRInstr::Return {
            value: Some(t),
            ty: Type::Int,
        },
    ];

    let mut phi_vars = run_phi_insertion(instrs, &l3);
    phi_vars.sort();
    assert_eq!(phi_vars, Vec::<String>::new()); // only y is defined on both paths
}

#[test]
fn test_phi_multiple_variables() {
    let l0 = Label("L0".into());
    let l1 = Label("L1".into());
    let l2 = Label("L2".into());
    let l3 = Label("L3".into());
    let token_x = Token::dummy_identifier("x");
    let token_y = Token::dummy_identifier("y");
    let t = TempId(1);

    let instrs = vec![
        HIRInstr::Label { label: l0.clone() },
        HIRInstr::StoreVar {
            var: token_x.clone(),
            src: t,
            ty: Type::Int,
        },
        HIRInstr::StoreVar {
            var: token_y.clone(),
            src: t,
            ty: Type::Int,
        },
        HIRInstr::If {
            cond: TempId(0),
            then_label: l1.clone(),
            else_label: l2.clone(),
        },
        HIRInstr::Label { label: l1.clone() },
        HIRInstr::Goto { target: l3.clone() },
        HIRInstr::Label { label: l2.clone() },
        HIRInstr::StoreVar {
            var: token_y.clone(),
            src: t,
            ty: Type::Int,
        },
        HIRInstr::Goto { target: l3.clone() },
        HIRInstr::Label { label: l3.clone() },
        HIRInstr::LoadVar {
            var: token_y.clone(),
            dest: TempId(2),
            ty: Type::Int,
        },
        HIRInstr::LoadVar {
            var: token_x.clone(),
            dest: TempId(2),
            ty: Type::Int,
        },
        HIRInstr::Return {
            value: Some(t),
            ty: Type::Int,
        },
    ];

    let mut phi_vars = run_phi_insertion(instrs, &l3);
    phi_vars.sort();
    assert_eq!(phi_vars, vec!["y"]); // only y is defined on both paths
}

// Add these tests to your phi insertion suite

#[test]
fn test_phi_insertion_loop_header() {
    let l0 = Label("L0".into());
    let l1 = Label("L1".into());
    let l2 = Label("L2".into());
    let l3 = Label("L3".into());
    let token_x = Token::dummy_identifier("x");
    let t = TempId(1);

    let instrs = vec![
        HIRInstr::Label { label: l0.clone() },
        HIRInstr::Goto { target: l1.clone() },
        HIRInstr::Label { label: l1.clone() },
        HIRInstr::If {
            cond: TempId(0),
            then_label: l2.clone(),
            else_label: l3.clone(),
        },
        HIRInstr::Label { label: l2.clone() },
        HIRInstr::StoreVar {
            var: token_x.clone(),
            src: t,
            ty: Type::Int,
        },
        HIRInstr::Goto { target: l1.clone() }, // Back edge
        HIRInstr::Label { label: l3.clone() },
        HIRInstr::LoadVar {
            dest: TempId(2),
            var: token_x.clone(),
            ty: Type::Int,
        },
        HIRInstr::Return {
            value: Some(t),
            ty: Type::Int,
        },
    ];

    let phi_vars = run_phi_insertion(instrs, &l1);
    assert_eq!(phi_vars, vec!["x"]);
}

// #[test]
// fn test_phi_one_pred_def() {
//     let l0 = Label("L0".into());
//     let l1 = Label("L1".into());
//     let l2 = Label("L2".into());
//     let l3 = Label("L3".into());
//     let token_x = Token::dummy_identifier("x");
//     let t = TempId(1);

//     let instrs = vec![
//         HIRInstr::Label { label: l0.clone() },
//         HIRInstr::If {
//             cond: TempId(0),
//             then_label: l1.clone(),
//             else_label: l2.clone(),
//         },
//         HIRInstr::Label { label: l1.clone() },
//         HIRInstr::StoreVar {
//             var: token_x.clone(),
//             src: t,
//             ty: Type::Int,
//         },
//         HIRInstr::Goto { target: l3.clone() },
//         HIRInstr::Label { label: l2.clone() },
//         HIRInstr::Goto { target: l3.clone() },
//         HIRInstr::Label { label: l3.clone() },
//         HIRInstr::LoadVar {
//             dest: TempId(2),
//             var: token_x.clone(),
//             ty: Type::Int,
//         },
//         HIRInstr::Return {
//             value: Some(t),
//             ty: Type::Int,
//         },
//     ];

//     let phi_vars = run_phi_insertion(instrs, &l3);
//     assert!(phi_vars.is_empty());
// }

//invalid
// #[test]
// fn test_phi_nested_diamond_merge() {
//     let l0 = Label("L0".into());
//     let l1 = Label("L1".into());
//     let l2 = Label("L2".into());
//     let l3 = Label("L3".into());
//     let l4 = Label("L4".into());
//     let l5 = Label("L5".into());
//     let token_x = Token::dummy_identifier("x");
//     let t = TempId(1);

//     let instrs = vec![
//         HIRInstr::Label { label: l0.clone() },
//         HIRInstr::If {
//             cond: TempId(0),
//             then_label: l1.clone(),
//             else_label: l2.clone(),
//         },
//         HIRInstr::Label { label: l1.clone() },
//         HIRInstr::If {
//             cond: TempId(0),
//             then_label: l3.clone(),
//             else_label: l4.clone(),
//         },
//         HIRInstr::Label { label: l3.clone() },
//         HIRInstr::StoreVar {
//             var: token_x.clone(),
//             src: t,
//             ty: Type::Int,
//         },
//         HIRInstr::Goto { target: l5.clone() },
//         HIRInstr::Label { label: l4.clone() },
//         HIRInstr::StoreVar {
//             var: token_x.clone(),
//             src: t,
//             ty: Type::Int,
//         },
//         HIRInstr::Goto { target: l5.clone() },
//         HIRInstr::Label { label: l2.clone() },
//         HIRInstr::Goto { target: l5.clone() },
//         HIRInstr::Label { label: l5.clone() },
//         HIRInstr::LoadVar {
//             dest: TempId(2),
//             var: token_x.clone(),
//             ty: Type::Int,
//         },
//         HIRInstr::Return {
//             value: Some(t),
//             ty: Type::Int,
//         },
//     ];

//     let phi_vars = run_phi_insertion(instrs, &l5);
//     assert_eq!(phi_vars, vec!["x"]);
// }
#[test]
fn test_phi_shadowed_variable_scope() {
    let l0 = Label("L0".into());
    let l1 = Label("L1".into());
    let l2 = Label("L2".into());
    let l3 = Label("L3".into());

    let token_x = Token::dummy_identifier("x");
    let token_x_shadow = Token::dummy_identifier("x_shadow");

    let t = TempId(1);

    let instrs = vec![
        HIRInstr::Label { label: l0.clone() },
        HIRInstr::StoreVar {
            var: token_x.clone(),
            src: t,
            ty: Type::Int,
        },
        HIRInstr::If {
            cond: t,
            then_label: l1.clone(),
            else_label: l2.clone(),
        },
        HIRInstr::Label { label: l1.clone() },
        // `x_shadow` is local to this branch and never used outside
        HIRInstr::StoreVar {
            var: token_x_shadow.clone(),
            src: t,
            ty: Type::Int,
        },
        HIRInstr::Goto { target: l3.clone() },
        HIRInstr::Label { label: l2.clone() },
        HIRInstr::StoreVar {
            var: token_x.clone(),
            src: TempId(2),
            ty: Type::Int,
        },
        HIRInstr::Goto { target: l3.clone() },
        HIRInstr::Label { label: l3.clone() },
        HIRInstr::LoadVar {
            dest: TempId(3),
            var: token_x.clone(),
            ty: Type::Int,
        },
        HIRInstr::Return {
            value: Some(t),
            ty: Type::Int,
        },
    ];

    let phi_vars = run_phi_insertion(instrs, &l3);
    assert_eq!(phi_vars, vec!["x"]); // only x flows through both paths and is used
}

#[test]
fn test_phi_dead_variable_eliminated() {
    let l0 = Label("L0".into());
    let l1 = Label("L1".into());
    let l2 = Label("L2".into());
    let l3 = Label("L3".into());

    let token_x = Token::dummy_identifier("x");
    let t = TempId(1);

    let instrs = vec![
        HIRInstr::Label { label: l0.clone() },
        HIRInstr::If {
            cond: TempId(0),
            then_label: l1.clone(),
            else_label: l2.clone(),
        },
        HIRInstr::Label { label: l1.clone() },
        HIRInstr::StoreVar {
            var: token_x.clone(),
            src: t,
            ty: Type::Int,
        },
        HIRInstr::Goto { target: l3.clone() },
        HIRInstr::Label { label: l2.clone() },
        HIRInstr::StoreVar {
            var: token_x.clone(),
            src: t,
            ty: Type::Int,
        },
        HIRInstr::Goto { target: l3.clone() },
        HIRInstr::Label { label: l3.clone() },
        // `x` is **not** loaded here.
        HIRInstr::Return {
            value: Some(t),
            ty: Type::Int,
        },
    ];

    let phi_vars = run_phi_insertion(instrs, &l3);
    assert!(phi_vars.is_empty()); // `x` is dead → no phi
}

// Additional SSA Phi Insertion Tests

#[test]
fn test_phi_loop_carried_variable() {
    let l0 = Label("L0".into());
    let l1 = Label("L1".into());
    let l2 = Label("L2".into());
    let loop_ = Label("Loop".into());

    let token_x = Token::dummy_identifier("x");
    let t = TempId(1);

    let instrs = vec![
        HIRInstr::Label { label: l0.clone() },
        HIRInstr::StoreVar {
            var: token_x.clone(),
            src: t,
            ty: Type::Int,
        },
        HIRInstr::Label { label: l1.clone() },
        HIRInstr::If {
            cond: TempId(0),
            then_label: loop_.clone(),
            else_label: l2.clone(),
        },
        HIRInstr::Label {
            label: loop_.clone(),
        },
        HIRInstr::StoreVar {
            var: token_x.clone(),
            src: t,
            ty: Type::Int,
        },
        HIRInstr::LoadVar {
            var: token_x.clone(),
            dest: TempId(2),
            ty: Type::Int,
        },
        HIRInstr::Goto { target: l1.clone() },
        HIRInstr::Label { label: l2.clone() },
        HIRInstr::Return {
            value: Some(t),
            ty: Type::Int,
        },
    ];

    let phi_vars = run_phi_insertion(instrs, &l1);
    assert_eq!(phi_vars, vec!["x"]);
}

// #[test]
// fn test_phi_nested_branch_merge() {
//     let l0 = Label("L0".into());
//     let l1 = Label("L1".into());
//     let l2 = Label("L2".into());
//     let l3 = Label("L3".into());
//     let l4 = Label("L4".into());
//     let l5 = Label("L5".into());
//     let token_x = Token::dummy_identifier("x");
//     let t = TempId(1);

//     let instrs = vec![
//         HIRInstr::Label { label: l0.clone() },
//         HIRInstr::If {
//             cond: TempId(0),
//             then_label: l1.clone(),
//             else_label: l4.clone(),
//         },
//         HIRInstr::Label { label: l1.clone() },
//         HIRInstr::If {
//             cond: TempId(0),
//             then_label: l2.clone(),
//             else_label: l3.clone(),
//         },
//         HIRInstr::Label { label: l2.clone() },
//         HIRInstr::StoreVar {
//             var: token_x.clone(),
//             src: t,
//             ty: Type::Int,
//         },
//         HIRInstr::Goto { target: l5.clone() },
//         HIRInstr::Label { label: l3.clone() },
//         HIRInstr::StoreVar {
//             var: token_x.clone(),
//             src: t,
//             ty: Type::Int,
//         },
//         HIRInstr::Goto { target: l5.clone() },
//         HIRInstr::Label { label: l4.clone() },
//         HIRInstr::Goto { target: l5.clone() },
//         HIRInstr::Label { label: l5.clone() },
//         HIRInstr::LoadVar {
//             var: token_x.clone(),
//             dest: TempId(2),
//             ty: Type::Int,
//         },
//         HIRInstr::Return {
//             value: Some(t),
//             ty: Type::Int,
//         },
//     ];

//     let phi_vars = run_phi_insertion(instrs, &l5);
//     // println!()
//     assert_eq!(phi_vars, vec!["x"]);
// }
