use std::collections::HashMap;

use crate::{
    ssa::{
        block_builder::{BlockBuilder, pretty_print_ssa_blocks},
        cfg::{CFG, DFSNumber},
        dominator::Dominator,
        ssa_state::SSAState,
    },
    types::{
        hir_types::{HIRFunction, Label},
        ssa_types::{BasicBlock, pretty_print_ssa_block},
        // ssa_types::SSAFunction,
    },
};

pub fn lower_functions_to_ssa(functions: &Vec<HIRFunction>) -> Vec<SSAContext> {
    let mut res: Vec<SSAContext> = Vec::new();
    for i in functions {
        let program = lower_to_ssa_program(i);
        res.push(program);
    }
    return res;
}

pub fn lower_to_ssa_program(function: &HIRFunction) -> SSAContext {
    let mut builder = BlockBuilder::new();
    builder.create_blocks(&function.body);
    builder.calculate_preds();

    let mut program =
        SSAProgram::from_blocks(builder.block_instrs.clone(), builder.block_order.clone());
    let metadata = SSAMetadata::compute_from(&program);

    let mut state = SSAState::new();
    state.insert_phi_nodes(
        &metadata.cfg,
        &mut builder.defsites,
        &metadata.dom_frontier,
        &mut program.blocks,
    );

    state.rename_vars(
        &metadata.dom_tree,
        &metadata.cfg,
        &mut program.blocks,
        &program.entry,
    );

    // builder.pretty_print_ssa_blocks();
    pretty_print_ssa_blocks(&program.blocks, &program.block_order);
    return SSAContext {
        program: program,
        metadata: metadata,
    };
}

pub struct SSAProgram {
    pub blocks: HashMap<Label, BasicBlock>,
    pub block_order: Vec<Label>,
    pub entry: Label,
}

impl SSAProgram {
    pub fn from_blocks(_blocks: HashMap<Label, BasicBlock>, block_order: Vec<Label>) -> SSAProgram {
        return Self {
            blocks: _blocks,
            entry: block_order.first().unwrap().clone(),
            block_order: block_order,
        };
    }
}

pub struct SSAMetadata {
    pub cfg: CFG,
    pub dom_tree: Vec<Vec<DFSNumber>>,
    pub dom_frontier: Vec<Vec<DFSNumber>>,
    // optionally: liveness, debug info, types
}

impl SSAMetadata {
    pub fn compute_from(program: &SSAProgram) -> Self {
        let mut cfg = CFG::from_blocks(&program.blocks, &program.block_order);
        cfg.dfs_entry();
        // cfg.dfs(program.entry.clone());

        let mut dominator = Dominator::new(cfg.pre_order.len());
        dominator.compute_semi_dominators(&cfg);
        dominator.compute_immediate_dominators(&cfg);
        dominator.finalize_immediate_dominators(&cfg);
        dominator.construct_dominator_tree(&cfg);
        dominator.construct_dominance_frontiers(&cfg);

        Self {
            cfg,
            dom_tree: dominator.dom_tree,
            dom_frontier: dominator.dominance_frontier,
        }
    }
}

pub struct SSAContext {
    pub program: SSAProgram,
    pub metadata: SSAMetadata,
}

impl SSAContext {
    pub fn from_function(function: &HIRFunction) -> Self {
        let mut builder = BlockBuilder::new();
        builder.create_blocks(&function.body);
        builder.calculate_preds();

        let mut program = SSAProgram::from_blocks(builder.block_instrs, builder.block_order);
        let metadata = SSAMetadata::compute_from(&program);

        let mut state = SSAState::new();
        state.insert_phi_nodes(
            &metadata.cfg,
            &mut builder.defsites,
            &metadata.dom_frontier,
            &mut program.blocks,
        );

        state.rename_vars(
            &metadata.dom_tree,
            &metadata.cfg,
            &mut program.blocks,
            &program.entry,
        );

        pretty_print_ssa_blocks(&program.blocks, &program.block_order);
        SSAContext { program, metadata }
    }
}
