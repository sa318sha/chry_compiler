use crate::ssa;
use crate::ssa::block_builder::BlockBuilder;
use crate::ssa::cfg::CFG;
use crate::ssa::dominator::Dominator;
use crate::ssa::ssa_version;
use crate::ssa::ssa_version::SSAVersion;
use crate::types::hir_types::HIRInstr;
use crate::types::hir_types::Label;
use crate::types::hir_types::TempId;
use crate::types::token::Token;
use crate::types::types::Type;

use std::collections::{HashMap, HashSet};

// Helper for running a dominator test
fn run_dom_test<F: FnOnce(&CFG, &Dominator)>(instrs: Vec<HIRInstr>, check: F) {
    let mut ssa_version = SSAVersion::new();
    let mut builder = BlockBuilder::new();
    builder.create_blocks(&instrs, &mut ssa_version);
    builder.calculate_preds();

    let mut cfg = CFG::from_blocks(&builder.block_instrs, &builder.block_order);
    cfg.dfs_entry();

    let mut dom = Dominator::new(cfg.pre_order.len());
    dom.compute_semi_dominators(&cfg);
    dom.compute_immediate_dominators(&cfg);
    dom.finalize_immediate_dominators(&cfg);
    dom.construct_dominator_tree(&cfg);
    dom.construct_dominance_frontiers(&cfg);

    // println!("hello world");

    check(&cfg, &dom);
}

/// Assert that the dominator tree structure matches the expected children.
///
/// `expected_tree` maps `Label`s to the list of Labels it should dominate directly.
fn assert_dominator_tree_eq(
    cfg: &CFG,
    dom: &Dominator,
    expected_tree: &HashMap<Label, Vec<Label>>,
) {
    for (label, expected_children) in expected_tree {
        let dfs = cfg.dfs_num(label).0;
        let actual_dfs_children = &dom.dom_tree[dfs];

        // Convert actual DFS indices to Labels
        let actual_children: HashSet<_> = actual_dfs_children
            .iter()
            .map(|child_dfs| {
                let hash_idx = cfg.pre_order[child_dfs.0];
                cfg.get_label(hash_idx).clone()
            })
            .collect();

        let expected_children_set: HashSet<_> = expected_children.iter().cloned().collect();

        assert_eq!(
            actual_children, expected_children_set,
            "Mismatch in dominator children for block {}",
            label.0
        );
    }
}

#[test]
fn test_single_entry_node() {
    // L0 only
    let l0 = Label("L0".into());
    let instrs = vec![
        HIRInstr::Label { label: l0.clone() },
        HIRInstr::Return {
            value: None,
            ty: Type::Void,
        },
    ];

    run_dom_test(instrs, |cfg, dom| {
        assert_eq!(dom.i_dominators[cfg.dfs_num(&l0).0], None);
        assert_dominator_tree_eq(cfg, dom, &HashMap::from([(l0.clone(), vec![])]));
    });
}

#[test]
fn test_linear_sequence() {
    // L0 -> L1 -> L2 -> L3
    let l0 = Label("L0".into());
    let l1 = Label("L1".into());
    let l2 = Label("L2".into());
    let l3 = Label("L3".into());

    let instrs = vec![
        HIRInstr::Label { label: l0.clone() },
        HIRInstr::Goto { target: l1.clone() },
        HIRInstr::Label { label: l1.clone() },
        HIRInstr::Goto { target: l2.clone() },
        HIRInstr::Label { label: l2.clone() },
        HIRInstr::Goto { target: l3.clone() },
        HIRInstr::Label { label: l3.clone() },
        HIRInstr::Return {
            value: None,
            ty: Type::Int,
        },
    ];

    run_dom_test(instrs, |cfg, dom| {
        assert_eq!(dom.i_dominators[cfg.dfs_num(&l1).0], Some(cfg.dfs_num(&l0)));
        assert_eq!(dom.i_dominators[cfg.dfs_num(&l2).0], Some(cfg.dfs_num(&l1)));
        assert_eq!(dom.i_dominators[cfg.dfs_num(&l3).0], Some(cfg.dfs_num(&l2)));

        assert_dominator_tree_eq(
            cfg,
            dom,
            &HashMap::from([
                (l0.clone(), vec![l1.clone()]),
                (l1.clone(), vec![l2.clone()]),
                (l2.clone(), vec![l3.clone()]),
                (l3.clone(), vec![]),
            ]),
        );
    });
}

#[test]
fn test_if_else_join() {
    // L0 -> L1 -> L3
    // L0 -> L2 -> L3
    let l0 = Label("L0".into());
    let l1 = Label("L1".into());
    let l2 = Label("L2".into());
    let l3 = Label("L3".into());

    let cond = TempId(0);
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
            value: None,
            ty: Type::Int,
        },
    ];

    run_dom_test(instrs, |cfg, dom| {
        assert_eq!(dom.i_dominators[cfg.dfs_num(&l1).0], Some(cfg.dfs_num(&l0)));
        assert_eq!(dom.i_dominators[cfg.dfs_num(&l2).0], Some(cfg.dfs_num(&l0)));
        assert_eq!(dom.i_dominators[cfg.dfs_num(&l3).0], Some(cfg.dfs_num(&l0)));
    });
}

#[test]
fn test_dominator_and_df_diamond() {
    let mut ssa_version = SSAVersion::new();

    let l0 = Label("L0".into());
    let l1 = Label("L1".into());
    let l2 = Label("L2".into());
    let l3 = Label("L3".into());

    let cond = TempId(0);
    let t = TempId(1);
    let tok = Token::dummy_identifier("x");

    let hir = vec![
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
            value: Some(t),
            ty: Type::Int,
        },
    ];

    let mut builder = BlockBuilder::new();
    builder.create_blocks(&hir, &mut ssa_version);
    builder.calculate_preds();

    let mut cfg = CFG::from_blocks(&builder.block_instrs, &builder.block_order);
    cfg.dfs_entry();

    let mut dom = Dominator::new(cfg.pre_order.len());
    dom.compute_semi_dominators(&cfg);
    dom.compute_immediate_dominators(&cfg);
    dom.finalize_immediate_dominators(&cfg);
    dom.construct_dominator_tree(&cfg);
    dom.construct_dominance_frontiers(&cfg);

    // IDOM checks
    let idom = |label: &Label| dom.i_dominators[cfg.dfs_num(label).0];

    assert_eq!(idom(&l1), Some(cfg.dfs_num(&l0)));
    assert_eq!(idom(&l2), Some(cfg.dfs_num(&l0)));
    assert_eq!(idom(&l3), Some(cfg.dfs_num(&l0))); // convergence point

    // DF checks
    let dfs_l1 = cfg.dfs_num(&l1).0;
    let dfs_l2 = cfg.dfs_num(&l2).0;
    let df_l1 = &dom.dominance_frontier[dfs_l1];
    let df_l2 = &dom.dominance_frontier[dfs_l2];
    assert!(df_l1.contains(&cfg.dfs_num(&l3)));
    assert!(df_l2.contains(&cfg.dfs_num(&l3)));
}

// #[test]
// fn test_dominator_and_df_loop() {
//     let l0 = Label("L0".into());
//     let l1 = Label("L1".into());
//     let l2 = Label("Exit".into());

//     let cond = TempId(0);
//     let t = TempId(1);
//     let tok = Token::dummy_identifier("x");

//     let hir = vec![
//         HIRInstr::Label { label: l0.clone() },
//         HIRInstr::If {
//             cond,
//             then_label: l1.clone(),
//             else_label: l2.clone(),
//         },
//         HIRInstr::Label { label: l1.clone() },
//         HIRInstr::Goto { target: l0.clone() },
//         HIRInstr::Label { label: l2.clone() },
//         HIRInstr::Return {
//             value: Some(t),
//             ty: Type::Int,
//         },
//     ];

//     let mut builder = BlockBuilder::new();
//     builder.create_blocks(&hir);
//     builder.calculate_preds();

//     let mut cfg = CFG::from_blocks(&builder.block_instrs, &builder.block_order);
//     cfg.dfs_entry();

//     let mut dom = Dominator::new(cfg.pre_order.len());
//     dom.compute_semi_dominators(&cfg);
//     dom.compute_immediate_dominatiors(&cfg);
//     dom.finialize_immediate_dominators(&cfg);
//     dom.construct_dominator_tree(&cfg);
//     dom.construct_dominance_frontiers(&cfg);

//     dom.pretty_print_semi_dominators(&cfg);
//     dom.pretty_print_immediate_dominators(&cfg);
//     dom.pretty_print_dominance_frontier(&cfg);
//     dom.pretty_print_dominator_tree(&cfg);

//     assert_eq!(dom.i_dominators[cfg.dfs_num(&l1).0], Some(cfg.dfs_num(&l0)));

//     // DF of L1 includes L0 (due to backedge)
//     let df_l1 = &dom.dominance_frontier[cfg.dfs_num(&l1).0];
//     assert!(df_l1.contains(&cfg.dfs_num(&l0)));
// }

// Loop with backedge test
#[test]
fn test_dominance_frontier_loop_with_backedge() {
    let l0 = Label("L0".into()); // loop header
    let l1 = Label("L1".into()); // body
    let l2 = Label("Exit".into());

    let cond = TempId(0);

    let hir = vec![
        HIRInstr::Label { label: l0.clone() },
        HIRInstr::If {
            cond,
            then_label: l1.clone(),
            else_label: l2.clone(),
        },
        HIRInstr::Label { label: l1.clone() },
        HIRInstr::Goto { target: l0.clone() }, // backedge
        HIRInstr::Label { label: l2.clone() },
        HIRInstr::Return {
            value: Some(cond),
            ty: Type::Int,
        },
    ];

    run_dom_test(hir, |cfg, dom| {
        assert_eq!(dom.i_dominators[cfg.dfs_num(&l1).0], Some(cfg.dfs_num(&l0)));
        assert_eq!(dom.i_dominators[cfg.dfs_num(&l2).0], Some(cfg.dfs_num(&l0)));
    });
}

#[test]
fn test_dominance_frontier_self_loop_on_entry() {
    let l0 = Label("L0".into());
    let cond = TempId(0);

    let hir = vec![
        HIRInstr::Label { label: l0.clone() },
        HIRInstr::Goto { target: l0.clone() }, // {
                                               //     cond,
                                               //     then_label: l0.clone(), // self-loop
                                               //     else_label: l0.clone(),
                                               // },
                                               // HIRInstr::Return {
                                               //     value: Some(cond),
                                               //     ty: Type::Int,
                                               // },
    ];

    run_dom_test(hir, |cfg, dom| {
        assert_eq!(dom.i_dominators[cfg.dfs_num(&l0).0], None);
        assert!(dom.dominance_frontier[cfg.dfs_num(&l0).0].is_empty());
    });
}

// Complex merge + fork
#[test]
fn test_dominance_frontier_complex_merge_fork() {
    let l0 = Label("L0".into());
    let l1 = Label("L1".into());
    let l2 = Label("L2".into());
    let l3 = Label("L3".into());
    let l4 = Label("L4".into());

    let cond = TempId(0);

    let hir = vec![
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
        HIRInstr::If {
            cond,
            then_label: l4.clone(),
            else_label: l2.clone(),
        },
        HIRInstr::Label { label: l4.clone() },
        HIRInstr::Return {
            value: Some(cond),
            ty: Type::Int,
        },
    ];

    run_dom_test(hir, |cfg, dom| {
        let dfs_l3 = cfg.dfs_num(&l3);
        let df_l2 = &dom.dominance_frontier[cfg.dfs_num(&l2).0];
        assert!(df_l2.contains(&dfs_l3));
    });
}

// Nested ifs and loops
#[test]
fn test_dominance_frontier_nested_ifs_and_loops() {
    let l0 = Label("L0".into());
    let l1 = Label("L1".into());
    let l2 = Label("L2".into());
    let l3 = Label("L3".into());
    let l4 = Label("L4".into());
    let l5 = Label("L5".into());
    let cond = TempId(0);

    let hir = vec![
        HIRInstr::Label { label: l0.clone() },
        HIRInstr::If {
            cond,
            then_label: l1.clone(),
            else_label: l5.clone(),
        },
        HIRInstr::Label { label: l1.clone() },
        HIRInstr::If {
            cond,
            then_label: l2.clone(),
            else_label: l3.clone(),
        },
        HIRInstr::Label { label: l2.clone() },
        HIRInstr::Goto { target: l4.clone() },
        HIRInstr::Label { label: l3.clone() },
        HIRInstr::Goto { target: l4.clone() },
        HIRInstr::Label { label: l4.clone() },
        HIRInstr::Goto { target: l1.clone() }, // backedge
        HIRInstr::Label { label: l5.clone() },
        HIRInstr::Return {
            value: Some(cond),
            ty: Type::Int,
        },
    ];

    run_dom_test(hir, |cfg, dom| {
        let df_l2 = &dom.dominance_frontier[cfg.dfs_num(&l2).0];
        assert!(df_l2.contains(&cfg.dfs_num(&l4)));
    });
}

#[test]
fn test_dominator_with_unreachable_block() {
    let mut ssa_version = SSAVersion::new();

    let l0 = Label("L0".into());
    let l1 = Label("L1".into());
    let l2 = Label("L2".into());
    let l_dead = Label("Dead".into());

    let t = TempId(1);
    let cond = TempId(0);

    let hir = vec![
        HIRInstr::Label { label: l0.clone() },
        HIRInstr::Goto { target: l1.clone() },
        HIRInstr::Label { label: l1.clone() },
        HIRInstr::Goto { target: l2.clone() },
        HIRInstr::Label { label: l2.clone() },
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
    builder.create_blocks(&hir, &mut ssa_version);
    builder.calculate_preds();

    let mut cfg = CFG::from_blocks(&builder.block_instrs, &builder.block_order);
    cfg.dfs_entry();

    let mut dom = Dominator::new(cfg.pre_order.len());
    dom.compute_semi_dominators(&cfg);
    dom.compute_immediate_dominators(&cfg);
    dom.finalize_immediate_dominators(&cfg);
    dom.construct_dominator_tree(&cfg);

    let dfs_labels: Vec<_> = cfg.pre_order.iter().map(|h| cfg.get_label(*h)).collect();
    assert!(!dfs_labels.contains(&&l_dead)); // unreachable, not in dfs
}
