#[cfg(test)]
mod tests {
    use super::*;
    use crate::ssa::block_builder::pretty_print_ssa_blocks;
    use crate::ssa::{
        block_builder::BlockBuilder, cfg::CFG, dominator::Dominator, ssa_state::SSAState,
    };
    use crate::types::hir_types::{HIRInstr, Label, TempId, pretty_print_hir_instr};
    use crate::types::ssa_types::BasicBlock;
    use crate::types::token::Token;
    use crate::types::types::Type;
    use std::collections::HashMap;
    use std::collections::HashSet;

    fn run_rename_pipeline(instrs: Vec<HIRInstr>) -> HashMap<Label, BasicBlock> {
        let mut builder = BlockBuilder::new();
        builder.create_blocks(&instrs);
        builder.calculate_preds();

        let mut cfg = CFG::from_blocks(&builder.block_instrs, &builder.block_order);
        cfg.dfs_entry();

        let mut dom = Dominator::new(cfg.pre_order.len());
        dom.compute_semi_dominators(&cfg);
        dom.compute_immediate_dominators(&cfg);
        dom.finalize_immediate_dominators(&cfg);
        dom.construct_dominator_tree(&cfg);
        dom.construct_dominance_frontiers(&cfg);

        let mut ssa = SSAState::new();
        ssa.insert_phi_nodes(
            &cfg,
            &mut builder.defsites,
            &dom.dominance_frontier,
            &mut builder.block_instrs,
        );
        ssa.rename_vars(
            &dom.dom_tree,
            &cfg,
            &mut builder.block_instrs,
            &cfg.entry_label,
        );

        dom.pretty_print_dominator_tree(&cfg);
        dom.pretty_print_dominance_frontier(&cfg);
        // builder.pretty_print_ssa_blocks();
        pretty_print_ssa_blocks(&builder.block_instrs, &builder.block_order);
        println!();
        for i in instrs {
            println!("{}", pretty_print_hir_instr(&i, 2));
        }

        builder.block_instrs
    }

    #[test]
    fn test_single_block_single_var() {
        let l0 = Label("L0".into());
        let tok_x = Token::dummy_identifier("x");
        let t1 = TempId(1);

        let instrs = vec![
            HIRInstr::Label { label: l0.clone() },
            HIRInstr::StoreVar {
                var: tok_x.clone(),
                src: t1,
                ty: Type::Int,
            },
            HIRInstr::Return {
                value: Some(t1),
                ty: Type::Int,
            },
        ];

        let blocks = run_rename_pipeline(instrs);
        let block = blocks.get(&l0).unwrap();

        let defs: Vec<_> = block
            .ssa_instrs
            .iter()
            .filter_map(|i| i.defines())
            .collect();

        assert_eq!(defs.len(), 1);
        assert_eq!(defs[0].name, "x");
    }

    #[test]
    fn test_straight_line_versioning() {
        let l0 = Label("L0".into());
        let l1 = Label("L1".into());
        let tok_x = Token::dummy_identifier("x");
        let t1 = TempId(1);

        let instrs = vec![
            HIRInstr::Label { label: l0.clone() },
            HIRInstr::StoreVar {
                var: tok_x.clone(),
                src: t1,
                ty: Type::Int,
            },
            HIRInstr::Goto { target: l1.clone() },
            HIRInstr::Label { label: l1.clone() },
            HIRInstr::StoreVar {
                var: tok_x.clone(),
                src: t1,
                ty: Type::Int,
            },
            HIRInstr::Return {
                value: Some(t1),
                ty: Type::Int,
            },
        ];

        let blocks = run_rename_pipeline(instrs);

        let block0 = blocks.get(&l0).unwrap();
        let block1 = blocks.get(&l1).unwrap();

        let v0 = &block0.ssa_instrs[0];
        let v1 = &block1.ssa_instrs[0];

        assert!(v0.defines().is_some());
        assert!(v1.defines().is_some());
        assert_ne!(v0.defines(), v1.defines());
    }
    #[test]
    fn test_diamond_join_with_phi() {
        let l0 = Label("L0".into());
        let l1 = Label("L1".into());
        let l2 = Label("L2".into());
        let l3 = Label("L3".into());

        let tok_x = Token::dummy_identifier("x");
        let t = TempId(1);

        let instrs = vec![
            HIRInstr::Label { label: l0.clone() },
            HIRInstr::StoreVar {
                var: tok_x.clone(),
                src: TempId(1),
                ty: Type::Int,
            },
            HIRInstr::If {
                cond: TempId(0),
                then_label: l1.clone(),
                else_label: l2.clone(),
            },
            HIRInstr::Label { label: l1.clone() },
            HIRInstr::StoreVar {
                var: tok_x.clone(),
                src: TempId(2),
                ty: Type::Int,
            },
            HIRInstr::Goto { target: l3.clone() },
            HIRInstr::Label { label: l2.clone() },
            HIRInstr::StoreVar {
                var: tok_x.clone(),
                src: TempId(3),
                ty: Type::Int,
            },
            HIRInstr::Goto { target: l3.clone() },
            HIRInstr::Label { label: l3.clone() },
            HIRInstr::LoadVar {
                var: tok_x.clone(),
                dest: TempId(4),
                ty: Type::Int,
            },
            HIRInstr::Return {
                value: Some(t),
                ty: Type::Int,
            },
        ];

        let blocks = run_rename_pipeline(instrs);
        let phi_block = blocks.get(&l3).unwrap();

        assert!(phi_block.phis.contains_key(&tok_x.lexeme));
        let phi_node = phi_block.phis.get(&tok_x.lexeme).unwrap();
        assert_eq!(phi_node.args.len(), 2);
    }

    #[test]
    fn test_loop_header_renaming() {
        let l0 = Label("L0".into());
        let l1 = Label("L1".into());
        let l2 = Label("L2".into());
        let l3 = Label("L3".into());

        let x = Token::dummy_identifier("x");
        let t = TempId(1);

        let instrs = vec![
            HIRInstr::Label { label: l0.clone() },
            HIRInstr::StoreVar {
                var: x.clone(),
                src: t,
                ty: Type::Int,
            },
            HIRInstr::Label { label: l1.clone() },
            HIRInstr::If {
                cond: TempId(0),
                then_label: l2.clone(),
                else_label: l3.clone(),
            },
            HIRInstr::Label { label: l2.clone() },
            HIRInstr::LoadVar {
                var: x.clone(),
                dest: t,
                ty: Type::Int,
            },
            HIRInstr::StoreVar {
                var: x.clone(),
                src: t,
                ty: Type::Int,
            },
            HIRInstr::Goto { target: l1.clone() },
            HIRInstr::Label { label: l3.clone() },
            HIRInstr::Return {
                value: Some(t),
                ty: Type::Int,
            },
        ];

        let blocks = run_rename_pipeline(instrs);
        let phi_block = blocks.get(&l1).unwrap();

        assert!(phi_block.phis.contains_key(&x.lexeme));
        let phi = phi_block.phis.get(&x.lexeme).unwrap();
        assert_eq!(phi.args.len(), 2); // from L0 and L2
    }

    #[test]
    fn test_nested_loops_variable_scope() {
        let l0 = Label("L0".into());
        let l1 = Label("L1".into());
        let l2 = Label("L2".into());
        let l3 = Label("L3".into());

        let x = Token::dummy_identifier("x");

        let instrs = vec![
            HIRInstr::Label { label: l0.clone() },
            HIRInstr::StoreVar {
                var: x.clone(),
                src: TempId(1),
                ty: Type::Int,
            },
            HIRInstr::Goto { target: l1.clone() },
            HIRInstr::Label { label: l1.clone() },
            HIRInstr::If {
                cond: TempId(0),
                then_label: l2.clone(),
                else_label: l3.clone(),
            },
            HIRInstr::Label { label: l2.clone() },
            HIRInstr::StoreVar {
                var: x.clone(),
                src: TempId(2),
                ty: Type::Int,
            },
            HIRInstr::Goto { target: l1.clone() },
            HIRInstr::Label { label: l3.clone() },
            HIRInstr::Return {
                value: Some(TempId(3)),
                ty: Type::Int,
            },
        ];

        let blocks = run_rename_pipeline(instrs);
        let outer_defs: Vec<_> = blocks
            .get(&l0)
            .unwrap()
            .ssa_instrs
            .iter()
            .filter_map(|i| i.defines())
            .collect();

        let inner_defs: Vec<_> = blocks
            .get(&l2)
            .unwrap()
            .ssa_instrs
            .iter()
            .filter_map(|i| i.defines())
            .collect();

        assert_eq!(outer_defs[0].name, "x");
        assert_eq!(inner_defs[0].name, "x");
        assert_ne!(outer_defs[0].version, inner_defs[0].version); // Different versions
    }

    #[test]
    fn test_multiple_variables_same_block() {
        let label = Label("L0".into());
        let tok_a = Token::dummy_identifier("a");
        let tok_b = Token::dummy_identifier("b");

        let instrs = vec![
            HIRInstr::Label {
                label: label.clone(),
            },
            HIRInstr::StoreVar {
                var: tok_a.clone(),
                src: TempId(1),
                ty: Type::Int,
            },
            HIRInstr::StoreVar {
                var: tok_b.clone(),
                src: TempId(2),
                ty: Type::Int,
            },
            HIRInstr::StoreVar {
                var: tok_a.clone(),
                src: TempId(3),
                ty: Type::Int,
            },
            HIRInstr::Return {
                value: None,
                ty: Type::Void,
            },
        ];

        let binding = run_rename_pipeline(instrs);
        let block = binding.get(&label).unwrap();

        let defs: Vec<_> = block
            .ssa_instrs
            .iter()
            .filter_map(|i| i.defines())
            .collect();

        assert_eq!(defs.iter().filter(|v| v.name == "a").count(), 2);
        assert_eq!(defs.iter().filter(|v| v.name == "b").count(), 1);
    }

    #[test]
    fn test_shadowed_variable() {
        let l0 = Label("L0".into());
        let l1 = Label("L1".into());

        let x = Token::dummy_identifier("x");

        let instrs = vec![
            HIRInstr::Label { label: l0.clone() },
            HIRInstr::StoreVar {
                var: x.clone(),
                src: TempId(1),
                ty: Type::Int,
            },
            HIRInstr::Goto { target: l1.clone() },
            HIRInstr::Label { label: l1.clone() },
            HIRInstr::StoreVar {
                var: x.clone(),
                src: TempId(2),
                ty: Type::Int,
            },
            HIRInstr::Return {
                value: Some(TempId(3)),
                ty: Type::Int,
            },
        ];

        let blocks = run_rename_pipeline(instrs);
        let defs: Vec<_> = blocks
            .values()
            .flat_map(|b| b.ssa_instrs.iter().filter_map(|i| i.defines()))
            .filter(|v| v.name == "x")
            .collect();

        assert_eq!(defs.len(), 2);
        assert_ne!(defs[0].version, defs[1].version); // distinct versions
    }

    #[test]
    fn test_phi_arguments_match_predecessors() {
        let l0 = Label("L0".into());
        let l1 = Label("L1".into());
        let l2 = Label("L2".into());
        let l3 = Label("L3".into());

        let x = Token::dummy_identifier("x");

        let instrs = vec![
            HIRInstr::Label { label: l0.clone() },
            HIRInstr::StoreVar {
                var: x.clone(),
                src: TempId(4),
                ty: Type::Int,
            },
            HIRInstr::If {
                cond: TempId(0),
                then_label: l1.clone(),
                else_label: l2.clone(),
            },
            HIRInstr::Label { label: l1.clone() },
            HIRInstr::StoreVar {
                var: x.clone(),
                src: TempId(1),
                ty: Type::Int,
            },
            HIRInstr::Goto { target: l3.clone() },
            HIRInstr::Label { label: l2.clone() },
            HIRInstr::StoreVar {
                var: x.clone(),
                src: TempId(2),
                ty: Type::Int,
            },
            HIRInstr::Goto { target: l3.clone() },
            HIRInstr::Label { label: l3.clone() },
            HIRInstr::LoadVar {
                var: x.clone(),
                dest: TempId(11),
                ty: Type::Int,
            },
            HIRInstr::Return {
                value: Some(TempId(3)),
                ty: Type::Int,
            },
        ];

        let binding = run_rename_pipeline(instrs);
        let block = binding.get(&l3).unwrap();
        let phi = block.phis.get(&x.lexeme).unwrap();

        assert_eq!(phi.args.len(), 2);
    }

    #[test]
    fn test_unused_vars_phi_not_needed() {
        let l0 = Label("L0".into());
        let l1 = Label("L1".into());
        let l2 = Label("L2".into());
        let l3 = Label("L3".into());

        let x = Token::dummy_identifier("x");

        let instrs = vec![
            HIRInstr::Label { label: l0.clone() },
            HIRInstr::StoreVar {
                var: x.clone(),
                src: TempId(12),
                ty: Type::Int,
            },
            HIRInstr::If {
                cond: TempId(0),
                then_label: l1.clone(),
                else_label: l2.clone(),
            },
            HIRInstr::Label { label: l1.clone() },
            HIRInstr::StoreVar {
                var: x.clone(),
                src: TempId(1),
                ty: Type::Int,
            },
            HIRInstr::Goto { target: l3.clone() },
            HIRInstr::Label { label: l2.clone() },
            HIRInstr::StoreVar {
                var: x.clone(),
                src: TempId(2),
                ty: Type::Int,
            },
            HIRInstr::Goto { target: l3.clone() },
            HIRInstr::Label { label: l3.clone() },
            HIRInstr::Return {
                value: Some(TempId(3)), // does not use `x`
                ty: Type::Int,
            },
        ];

        let binding = run_rename_pipeline(instrs);
        let block = binding.get(&l3).unwrap();
        // print!("{:?}",block.phis);
        assert!(!block.phis.contains_key(&x.lexeme)); // it's inserted anyway
    }

    // #[test]
    // fn test_self_loop_phi_guard() {
    //     let l0 = Label("L0".into());
    //     let x = Token::dummy_identifier("x");

    //     let instrs = vec![
    //         HIRInstr::Label { label: l0.clone() },
    //         HIRInstr::StoreVar {
    //             var: x.clone(),
    //             src: TempId(1),
    //             ty: Type::Int,
    //         },
    //         HIRInstr::Goto { target: l0.clone() },
    //     ];

    //     let binding = run_rename_pipeline(instrs);
    //     let block = binding.get(&l0).unwrap();
    //     let phi = block.phis.get(&x.lexeme).unwrap();

    //     assert_eq!(phi.args.len(), 1); // Self-loop should not cause double counting
    // }
}
