use crate::ssa::liveness::Liveness;
use crate::ssa::ssa_version::SSAVersion;
use crate::types::hir_types::{HIRInstr, TempId};
use crate::types::ssa_types::{BasicBlock, PhiNode, SSAExpr, SSAInstr, SSATempId};
use crate::types::types::Type;
use crate::{
    ssa::{
        // block_builder::{BasicBlock, PhiNode},
        cfg::{CFG, DFSNumber},
    },
    types::hir_types::Label,
};
use std::collections::{HashMap, HashSet};
use std::convert;
pub struct SSAState {
    var_stack: HashMap<String, Vec<SSATempId>>,
    anon_temp_counter: usize,
    pub var_type_map: HashMap<String, Type>,
    liveness: Liveness,
}

impl SSAState {
    pub fn new() -> SSAState {
        return SSAState {
            // var_version: HashMap::new(),
            var_type_map: HashMap::new(),
            var_stack: HashMap::new(),
            // hir_to_ssa_temp: HashMap::new(),
            anon_temp_counter: 0,
            liveness: Liveness::new(),
        };
    }

    pub fn extract_types_from_hir(&mut self, blocks: &HashMap<Label, BasicBlock>) {
        for block in blocks.values() {
            for instr in &block.instrs {
                match instr {
                    HIRInstr::Const { dest, ty, .. } => {
                        self.var_type_map
                            .insert(format!("_tmp{}", dest.0), ty.clone());
                    }
                    HIRInstr::StoreVar { var, ty, .. } => {
                        self.var_type_map.insert(var.lexeme.clone(), ty.clone());
                    }
                    HIRInstr::Move { dest, ty, .. } => {
                        self.var_type_map
                            .insert(format!("_tmp{}", dest.0), ty.clone());
                    }
                    HIRInstr::Call { dest, ty, .. } => {
                        self.var_type_map
                            .insert(format!("_tmp{}", dest.0), ty.clone());
                    }
                    // Add more if needed
                    _ => {}
                }
            }
        }
    }

    pub fn insert_phi_nodes(
        &mut self,
        cfg: &CFG,
        defsites: &mut HashMap<String, HashSet<Label>>,
        dominance_frontier: &Vec<Vec<DFSNumber>>,
        builder_blocks: &mut HashMap<Label, BasicBlock>,
    ) {
        self.extract_types_from_hir(&builder_blocks);
        self.liveness.compute_liveness(cfg, builder_blocks);
        // for (label, live_vars) in &liveness {
        //     println!("LIVE @ {:?}: {:?}", label, live_vars);
        // }
        // println!("Phi nodes");
        for (var, def_blocks) in defsites {
            // println!("Var: {}, Defsites: {:?}", var, def_blocks);
            let mut has_already = HashSet::new();
            // Always start from *live-in blocks* if no defs exist
            let mut worklist: Vec<Label> = def_blocks.iter().cloned().collect();
            // println!("--- Inserting phi nodes for variable `{}` ---", var);
            // println!("Defsites: {:?}", def_blocks);
            while let Some(block) = worklist.pop() {
                let dfs_num = cfg.dfs_num(&block);
                if let Some(df_blocks) = dominance_frontier.get(dfs_num.0) {
                    // println!(
                    //     "Dominance frontier of {:?}: {:?}",
                    //     block,
                    //     df_blocks
                    //         .iter()
                    //         .map(|df| cfg.get_label(cfg.pre_order[df.0]))
                    //         .collect::<Vec<_>>()
                    // );

                    for df in df_blocks {
                        let df_label = cfg.get_label(cfg.pre_order[df.0]);

                        if !self
                            .liveness
                            .live_in
                            .get(df_label)
                            .map_or(false, |live| live.contains(var))
                        {
                            // println!("Skip phi for `{}` at {:?} (not live)", var, df_label);
                            continue;
                        }

                        if has_already.insert(df_label.clone()) {
                            // println!("Insert phi for `{}` in block {:?}", var, df_label);
                            // Insert phi node at df_label
                            let block_entry = builder_blocks.get_mut(df_label).unwrap();
                            block_entry
                                .phis
                                .entry(var.clone())
                                .or_insert_with(|| PhiNode {
                                    dest: SSATempId {
                                        name: var.clone(),
                                        version: 0,
                                    },
                                    ty: self
                                        .var_type_map
                                        .get(var)
                                        .expect(&format!("No type info for phi node of `{}`", var))
                                        .clone(),
                                    args: HashMap::new(),
                                });

                            worklist.push(df_label.clone());
                        }
                    }
                }
            }
        }
    }

    pub fn rename_vars(
        &mut self,
        dominator_tree: &Vec<Vec<DFSNumber>>,
        cfg: &CFG,
        builder_blocks: &mut HashMap<Label, BasicBlock>,
        label: &Label,
        ssa_versioning: &mut SSAVersion,
    ) {
        let block = builder_blocks.get_mut(label).unwrap();
        {
            let mut ssa_instrs = Vec::new();

            for (var, phi) in block.phis.iter_mut() {
                let version = ssa_versioning.next_version(var);
                let ssa_id = SSATempId {
                    name: var.clone(),
                    version,
                };
                phi.dest = ssa_id.clone();
                self.var_stack.entry(var.clone()).or_default().push(ssa_id);
            }

            for hir_instr in &block.instrs {
                if let Some(ssa_instr) = self.convert_hir_instr_to_ssa(hir_instr, ssa_versioning) {
                    ssa_instrs.push(ssa_instr);
                }
            }
            block.ssa_instrs = ssa_instrs;
        }
        //phi args insertion

        for i in block.terminator.successors() {
            let successor_block = builder_blocks.get_mut(&i).unwrap();
            for (var, phi) in &mut successor_block.phis {
                if let Some(ssa_id) = self.var_stack.get(var).and_then(|v| v.last()).cloned() {
                    phi.args.insert(label.clone(), ssa_id);
                } else {
                    panic!("No SSA version for variable {}", var);
                }
            }
        }

        let dfs_num = cfg.dfs_num(label);
        for child_dfs in &dominator_tree[dfs_num.0] {
            let child_label = cfg.get_label(cfg.pre_order[child_dfs.0]);
            self.rename_vars(
                dominator_tree,
                cfg,
                builder_blocks,
                child_label,
                ssa_versioning,
            );
        }

        let block = builder_blocks.get(label).unwrap();
        for instr in &block.ssa_instrs {
            if let Some(var) = instr.defines() {
                // println!("Popping from var_stack[{}_{}]", var.name, var.version);
                match self.var_stack.get_mut(&var.name) {
                    Some(stack) => {
                        stack.pop();
                    }
                    None => {
                        panic!(
                            "No var_stack entry for `{}` when trying to pop (maybe it's a temp?)",
                            var.name
                        );
                    }
                }
            }
        }
        for var in block.phis.keys() {
            self.var_stack.get_mut(var).unwrap().pop();
        }
        // for instr in &mut block.instrs {
        //     // instr
        //      instr.rename_uses(&self.var_stack);
        //     // if let Some
        // }
    }

    fn convert_hir_instr_to_ssa(
        &mut self,
        hir_instr: &HIRInstr,
        ssa_versioning: &mut SSAVersion,
    ) -> Option<SSAInstr> {
        match hir_instr {
            HIRInstr::Const { dest, value, ty } => {
                return Some(SSAInstr::Assign {
                    dest: ssa_versioning.convert_hir_temp_to_ssa_temp(dest, None),
                    expr: SSAExpr::Const(value.clone()),
                    ty: ty.clone(),
                });
            }
            HIRInstr::UnaryOp { dest, op, src, ty } => {
                return Some(SSAInstr::Assign {
                    dest: ssa_versioning.convert_hir_temp_to_ssa_temp(dest, None),
                    expr: SSAExpr::Unary {
                        op: op.clone(),
                        src: ssa_versioning.convert_hir_temp_to_ssa_temp(src, None),
                    },
                    ty: ty.clone(),
                });
            }
            HIRInstr::BinaryOp {
                dest,
                op,
                lhs,
                rhs,
                ty,
            } => {
                // let lhs_ssa = self.var_stack[&lhs.name].last().unwrap().clone();
                // let rhs_ssa = self.var_stack[&rhs.name].last().unwrap().clone();

                return Some(SSAInstr::Assign {
                    dest: ssa_versioning.convert_hir_temp_to_ssa_temp(dest, None),
                    expr: SSAExpr::Binary {
                        op: op.clone(),
                        lhs: ssa_versioning.convert_hir_temp_to_ssa_temp(lhs, None),
                        rhs: ssa_versioning.convert_hir_temp_to_ssa_temp(rhs, None),
                    },
                    ty: ty.clone(),
                });
            }
            HIRInstr::LoadVar { dest, var, ty } => {
                let val = self
                    .var_stack
                    .get(&var.lexeme)
                    .and_then(|v| v.last())
                    .cloned();
                if let Some(version) = val {
                    return Some(SSAInstr::Assign {
                        dest: ssa_versioning.convert_hir_temp_to_ssa_temp(dest, None),
                        expr: SSAExpr::Var { val: version },
                        ty: ty.clone(),
                    });
                } else {
                    todo!("ERROR")
                }
            }
            HIRInstr::StoreVar { var, src, ty } => {
                self.var_type_map.insert(var.lexeme.clone(), ty.clone());

                let versioned_var =
                    ssa_versioning.convert_hir_temp_to_ssa_temp(src, Some(var.lexeme.clone()));
                self.var_stack
                    .entry(var.lexeme.clone())
                    .or_default()
                    .push(versioned_var.clone());
                return Some(SSAInstr::Assign {
                    dest: versioned_var,
                    expr: SSAExpr::Var {
                        val: ssa_versioning.convert_hir_temp_to_ssa_temp(src, None),
                    },
                    ty: ty.clone(),
                });
            }
            HIRInstr::Call {
                dest,
                func,
                args,
                ty,
            } => {
                let mut ssa_temp_ids: Vec<SSATempId> = vec![];
                // todo!();
                for arg in args {
                    ssa_temp_ids.push(ssa_versioning.convert_hir_temp_to_ssa_temp(arg, None));
                }

                return Some(SSAInstr::Assign {
                    dest: ssa_versioning.convert_hir_temp_to_ssa_temp(dest, None),
                    expr: SSAExpr::Call {
                        func: func.lexeme.clone(),
                        args: ssa_temp_ids,
                    },
                    ty: ty.clone(),
                });
            }
            HIRInstr::Print { src, ty } => None,
            HIRInstr::Move { dest, src, ty } => {
                return Some(SSAInstr::Assign {
                    dest: ssa_versioning.convert_hir_temp_to_ssa_temp(dest, None),
                    expr: SSAExpr::Var {
                        val: ssa_versioning.convert_hir_temp_to_ssa_temp(src, None),
                    },
                    ty: ty.clone(),
                });
            }
            _ => None,
        }
    }
}
