use std::{collections::HashMap, fmt};

use crate::{
    ssa::{
        block_builder::{pretty_print_ssa_blocks, BlockBuilder},
        cfg::{DFSNumber, CFG},
        dominator::Dominator,
        ssa_state::SSAState,
        ssa_version::SSAVersion,
    },
    types::{
        hir_types::{HIRFunction, Label},
        ssa_types::{pretty_print_basic_block, pretty_print_basic_block_ssa, BasicBlock},
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
    let mut ssa_versioning = SSAVersion::new();

    let mut builder = BlockBuilder::new();
    builder.create_blocks(&function.body, &mut ssa_versioning);
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
        &mut ssa_versioning,
    );

    // builder.pretty_print_ssa_blocks();
    // pretty_print_ssa_blocks(&program.blocks, &program.block_order);
    return SSAContext {
        program: program,
        metadata: metadata,
    };
}

pub struct SSAProgram {
    pub blocks: HashMap<Label, BasicBlock>,
    pub block_order: Vec<Label>,
    // pub rpo: Vec<Label>,
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

/// Pretty print an SSAProgram
impl fmt::Display for SSAProgram {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, ";; SSA Program")?;
        writeln!(f, "entry: {}", self.entry.0)?;
        writeln!(f, "blocks: {}", self.blocks.len())?;
        writeln!(f)?;

        for label in &self.block_order {
            if let Some(block) = self.blocks.get(label) {
                writeln!(f, "{}", pretty_print_basic_block_ssa(block))?;
            } else {
                writeln!(f, "LABEL {}:\n  <missing block body>\n", label.0)?;
            }
        }
        Ok(())
    }
}

/// Pretty print SSAMetadata (CFG + dom tree + frontier sizes)
impl fmt::Display for SSAMetadata {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, ";; SSAMetadata")?;
        // writeln!(f, "CFG:")?;
        // writeln!(f, "{}", self.cfg)?; // assumes CFG implements Display
        writeln!(f, "Dom Tree:")?;
        for (i, children) in self.dom_tree.iter().enumerate() {
            let kids: Vec<String> = children.iter().map(|d| format!("{:?}", d)).collect();
            writeln!(f, "  {:?} -> [{}]", DFSNumber(i), kids.join(", "))?;
        }
        writeln!(f, "Dom Frontier:")?;
        for (i, frontier) in self.dom_frontier.iter().enumerate() {
            let fset: Vec<String> = frontier.iter().map(|d| format!("{:?}", d)).collect();
            writeln!(f, "  {:?} : [{}]", DFSNumber(i), fset.join(", "))?;
        }
        Ok(())
    }
}

/// Pretty print SSAContext
impl fmt::Display for SSAContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.program)
        // writeln!(f, "{}", self.metadata)
    }
}
