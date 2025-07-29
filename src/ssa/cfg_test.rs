use std::collections::HashSet;

use crate::{
    ssa::{block_builder::BlockBuilder, cfg::CFG},
    types::{
        hir_types::{HIRInstr, Label, TempId},
        literal::Literal,
        token::Token,
        types::Type,
    },
};

#[test]
fn test_cfg_straight_line() {
    let l0 = Label("L0".to_string());
    let l1 = Label("L1".to_string());

    let t = TempId(0);
    let tok = Token::dummy_identifier("x");

    let instrs = vec![
        HIRInstr::Label { label: l0.clone() },
        HIRInstr::Const {
            dest: t,
            value: Literal::Int(1),
            ty: Type::Int,
        },
        HIRInstr::StoreVar {
            var: tok.clone(),
            src: t,
            ty: Type::Int,
        },
        HIRInstr::Goto { target: l1.clone() },
        HIRInstr::Label { label: l1.clone() },
        HIRInstr::Return {
            value: Some(t),
            ty: Type::Int,
        },
    ];

    let mut builder = BlockBuilder::new();
    builder.create_blocks(&instrs);
    builder.calculate_preds();

    let cfg = CFG::from_blocks(&builder.block_instrs, &builder.block_order);

    let node0 = cfg.get_block(&l0);
    let node1 = cfg.get_block(&l1);

    assert_eq!(node0.successors, vec![l1.clone()]);
    assert_eq!(node1.successors.len(), 0);

    assert_eq!(node0.preds.len(), 0);
    assert_eq!(node1.preds, vec![l0.clone()]);
}

#[test]
fn test_cfg_if_join_diamond() {
    let l0 = Label("L0".to_string());
    let l1 = Label("L1".to_string());
    let l2 = Label("L2".to_string());
    let l3 = Label("L3".to_string());

    let cond = TempId(0);
    let tok = Token::dummy_identifier("x");

    let instrs = vec![
        HIRInstr::Label { label: l0.clone() },
        HIRInstr::If {
            cond,
            then_label: l1.clone(),
            else_label: l2.clone(),
        },
        HIRInstr::Label { label: l1.clone() },
        HIRInstr::Goto { target: l3.clone() },
        HIRInstr::Label { label: l2.clone() },
        HIRInstr::Goto { target: l3.clone() },
        HIRInstr::Label { label: l3.clone() },
        HIRInstr::Return {
            value: Some(cond),
            ty: Type::Int,
        },
    ];

    let mut builder = BlockBuilder::new();
    builder.create_blocks(&instrs);
    builder.calculate_preds();

    let cfg = CFG::from_blocks(&builder.block_instrs, &builder.block_order);

    let node0 = cfg.get_block(&l0);
    let node1 = cfg.get_block(&l1);
    let node2 = cfg.get_block(&l2);
    let node3 = cfg.get_block(&l3);

    assert_eq!(node0.successors, vec![l1.clone(), l2.clone()]);
    assert_eq!(node1.successors, vec![l3.clone()]);
    assert_eq!(node2.successors, vec![l3.clone()]);
    assert_eq!(node3.successors.len(), 0);

    assert_eq!(
        node3.preds.iter().collect::<HashSet<_>>(),
        [l1, l2].iter().collect()
    );
}

#[test]
fn test_cfg_dead_code_block() {
    let l0 = Label("L0".to_string());
    let l1 = Label("L1".to_string());
    let l_dead = Label("Dead".to_string());

    let t = TempId(0);
    let tok = Token::dummy_identifier("x");

    let instrs = vec![
        HIRInstr::Label { label: l0.clone() },
        HIRInstr::Goto { target: l1.clone() },
        HIRInstr::Label { label: l1.clone() },
        HIRInstr::Return {
            value: Some(t),
            ty: Type::Int,
        },
        HIRInstr::Label {
            label: l_dead.clone(),
        },
        HIRInstr::Return {
            value: Some(t),
            ty: Type::Int,
        },
    ];

    let mut builder = BlockBuilder::new();
    builder.create_blocks(&instrs);
    builder.calculate_preds();

    let cfg = CFG::from_blocks(&builder.block_instrs, &builder.block_order);

    let live_labels: HashSet<_> = cfg.label_to_index.keys().collect();
    assert!(live_labels.contains(&l0));
    assert!(live_labels.contains(&l1));
    assert!(live_labels.contains(&l_dead)); // dead block is present in CFG structure

    let dead_node = cfg.get_block(&l_dead);
    assert_eq!(dead_node.preds.len(), 0); // unreachable
    assert_eq!(dead_node.successors.len(), 0);
}

#[test]
fn test_cfg_while_loop_structure() {
    let l0 = Label("L0".to_string()); // loop header
    let l1 = Label("L1".to_string()); // loop body
    let l2 = Label("L2".to_string()); // exit

    let cond = TempId(0);
    let t = TempId(1);
    let tok = Token::dummy_identifier("x");

    let instrs = vec![
        HIRInstr::Label { label: l0.clone() },
        HIRInstr::If {
            cond,
            then_label: l1.clone(),
            else_label: l2.clone(),
        },
        HIRInstr::Label { label: l1.clone() },
        HIRInstr::Const {
            dest: t,
            value: Literal::Int(1),
            ty: Type::Int,
        },
        HIRInstr::StoreVar {
            var: tok.clone(),
            src: t,
            ty: Type::Int,
        },
        HIRInstr::Goto { target: l0.clone() }, // back to header
        HIRInstr::Label { label: l2.clone() },
        HIRInstr::Return {
            value: Some(t),
            ty: Type::Int,
        },
    ];

    let mut builder = BlockBuilder::new();
    builder.create_blocks(&instrs);
    builder.calculate_preds();

    let cfg = CFG::from_blocks(&builder.block_instrs, &builder.block_order);

    assert_eq!(cfg.get_block(&l0).successors, vec![l1.clone(), l2.clone()]);
    assert_eq!(cfg.get_block(&l1).successors, vec![l0.clone()]);
    assert_eq!(cfg.get_block(&l2).successors.len(), 0);

    assert_eq!(cfg.get_block(&l0).preds, vec![l1.clone()]);
    assert_eq!(cfg.get_block(&l1).preds, vec![l0.clone()]);
    assert_eq!(cfg.get_block(&l2).preds, vec![l0.clone()]);
}

#[test]
fn test_cfg_pre_order_and_parents_for_while() {
    let l0 = Label("L0".to_string());
    let l1 = Label("L1".to_string());
    let l2 = Label("L2".to_string());

    let cond = TempId(0);
    let t = TempId(1);
    let tok = Token::dummy_identifier("x");

    let instrs = vec![
        HIRInstr::Label { label: l0.clone() },
        HIRInstr::If {
            cond,
            then_label: l1.clone(),
            else_label: l2.clone(),
        },
        HIRInstr::Label { label: l1.clone() },
        HIRInstr::StoreVar {
            var: tok.clone(),
            src: t,
            ty: Type::Int,
        },
        HIRInstr::Goto { target: l0.clone() },
        HIRInstr::Label { label: l2.clone() },
        HIRInstr::Return {
            value: Some(t),
            ty: Type::Int,
        },
    ];

    let mut builder = BlockBuilder::new();
    builder.create_blocks(&instrs);
    builder.calculate_preds();

    let mut cfg = CFG::from_blocks(&builder.block_instrs, &builder.block_order);
    cfg.dfs_entry();

    let order = cfg
        .pre_order
        .iter()
        .map(|i| cfg.get_label(*i))
        .cloned()
        .collect::<Vec<_>>();
    assert_eq!(order, vec![l0.clone(), l1.clone(), l2.clone()]);

    let idx_l0 = cfg.get_index(&l0).unwrap();
    let idx_l1 = cfg.get_index(&l1).unwrap();
    let idx_l2 = cfg.get_index(&l2).unwrap();

    assert_eq!(cfg.get_dfs_parents(*idx_l0), None);
    assert_eq!(cfg.get_dfs_parents(*idx_l1), Some(*idx_l0));
    assert_eq!(cfg.get_dfs_parents(*idx_l2), Some(*idx_l0));
}
