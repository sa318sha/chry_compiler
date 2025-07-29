use std::collections::HashMap;

use crate::ssa::ssa::SSAContext;
use crate::types::hir_types::HIRInstr;
use crate::types::lir_types::{LIRBasicBlock, LIRExpr, LIRTerminator, VirtualReg};
use crate::types::ssa_types::PhiNode;
use crate::{
    ssa::ssa::SSAProgram,
    types::{
        hir_types::Label,
        lir_types::{LIRInstr, VirtualRegAllocator},
        ssa_types::{BasicBlock, SSAExpr, SSAInstr, SSATempId, Terminator},
    },
};

pub struct LIRLowering {
    ssa_to_lir: HashMap<SSATempId, VirtualReg>, // SSA temp → vreg mapping
    reg_alloc: VirtualRegAllocator,             // fresh virtual register generator
    lir_blocks: HashMap<Label, LIRBasicBlock>,  // work-in-progress result
    block_order: Vec<Label>,                    // inherited from SSAProgram
    phi_dest_regs: HashMap<SSATempId, VirtualReg>,
}
pub struct LIRProgram {
    entry: Label,                          // Entry point to the function
    blocks: HashMap<Label, LIRBasicBlock>, // Block label → block body
    block_order: Vec<Label>,               // Ordered traversal (e.g. RPO)
}

pub fn lower_to_lir(programs: Vec<SSAContext>) -> Vec<LIRProgram> {
    let mut lir_programs = Vec::new();
    for program in &programs {
        lir_programs.push(LIRLowering::from_ssa_context(program));
    }
    return lir_programs;
}

impl LIRLowering {
    pub fn from_ssa_context(ssa_context: &SSAContext) -> LIRProgram {
        let mut lowering = LIRLowering {
            ssa_to_lir: HashMap::new(),
            reg_alloc: VirtualRegAllocator { next_id: 0 },
            lir_blocks: HashMap::new(),
            phi_dest_regs: HashMap::new(),
            block_order: ssa_context.program.block_order.clone(),
        };
        //0th pass
        for block_label in &ssa_context.program.block_order {
            let phis = &ssa_context.program.blocks[block_label].phis;
            for (var, phi_node) in phis {
                for (label, ssa_id) in &phi_node.args {
                    let dest_reg = lowering.reg_alloc.fresh();
                    lowering.phi_dest_regs.insert(phi_node.dest.clone(), dest_reg);
                }
            }
        }

        //1st pass
        for label in &ssa_context.program.block_order {
            let block = ssa_context.program.blocks.get(label).unwrap();
            let lir_block = lowering.lower_block(block);
            lowering.lir_blocks.insert(label.clone(), lir_block);
        }
        //2nd pass
        for block_label in &ssa_context.program.block_order {
            let phis = &ssa_context.program.blocks[block_label].phis;
            lowering.insert_phi_moves_for_block(block_label, phis)
            // let phis: &HashMap<String, PhiNode> =
            //     &ssa_context.program.blocks.get(label).unwrap().phis;
        }
        LIRProgram {
            entry: ssa_context.program.entry.clone(),
            blocks: lowering.lir_blocks,
            block_order: ssa_context.program.block_order.clone(),
        }
    }

    fn insert_phi_moves_for_block(&mut self, label: &Label, phis: &HashMap<String, PhiNode>) {
        // let phis = &ssa_context.program.blocks[block_label].phis

        let mut phi_moves_per_pred: HashMap<Label, Vec<(VirtualReg, VirtualReg)>> = HashMap::new();

        for (var, phi_node) in phis {
            for (pred_label, incoming_ssa_id) in &phi_node.args {
                let src_reg: VirtualReg = self.ssa_to_lir[&incoming_ssa_id]; //(or phi_dest_regs if it's a phi)
                let dest_reg = self.phi_dest_regs[&phi_node.dest];
                phi_moves_per_pred
                    .entry(pred_label.clone())
                    .or_insert(vec![])
                    .push((dest_reg, src_reg))
            }
        }

        for (pred_label, moves) in phi_moves_per_pred {
            let dests = moves.iter().map(|(dest, src)| dest).collect::<Vec<_>>();
            let mut temps: HashMap<VirtualReg, VirtualReg> = HashMap::new();
            let mut safe_moves: Vec<(VirtualReg, VirtualReg)> = Vec::new();

            for (dest, src) in &moves {
                if dests.contains(&src) {
                    let temp = self.reg_alloc.fresh();
                    let pred_block = self.lir_blocks.get_mut(&pred_label).unwrap();
                    pred_block.instrs.push(LIRInstr::Move {
                        dest: temp,
                        src: *src,
                    });
                    temps.insert(*dest, temp);
                } else {
                    safe_moves.push((*dest, *src));
                }
            }

            for (dest, temp) in &temps {
                let pred_block = self.lir_blocks.get_mut(&pred_label).unwrap();
                pred_block.instrs.push(LIRInstr::Move {
                    dest: *dest,
                    src: *temp,
                });
            }
            for (dest, src) in &safe_moves {
                let pred_block = self.lir_blocks.get_mut(&pred_label).unwrap();
                pred_block.instrs.push(LIRInstr::Move {
                    dest: *dest,
                    src: *src,
                });
            }
        }
    }
    fn lower_block(&mut self, block: &BasicBlock) -> LIRBasicBlock {
        let mut instrs = vec![];

        // Step 3: Lower SSA instructions (excluding phi)
        for ssa_instr in &block.ssa_instrs {
            if let Some(instr) = self.lower_ssa_instr(ssa_instr) {
                instrs.push(instr);
            }
        }

        // Step 4: Lower terminator
        let terminator = self.lower_terminator(&block.terminator);

        LIRBasicBlock {
            label: block.label.clone(),
            instrs,
            pending_moves: Vec::new(),
            terminator: terminator,
        }
    }

    fn lower_ssa_instr(&mut self, ssa_instr: &SSAInstr) -> Option<LIRInstr> {
        match ssa_instr {
            SSAInstr::Assign { dest, expr } => {
                let dest_reg = self.reg_alloc.fresh();
                self.ssa_to_lir.insert(dest.clone(), dest_reg);

                let lir_expr = match expr {
                    SSAExpr::Const(lit) => LIRExpr::Const(lit.clone()),
                    SSAExpr::Var { val } => {
                        println!("{}_{}", val.name, val.version);
                        let src = &self.resolve_lir_temp(val);
                        LIRExpr::Var(src.clone())
                    }
                    SSAExpr::Unary { op, src } => {
                        let src_reg = self.ssa_to_lir[src];
                        LIRExpr::Unary {
                            op: op.clone(),
                            val: src_reg,
                        }
                    }
                    SSAExpr::Binary { op, lhs, rhs } => {
                        let lhs_reg = self.ssa_to_lir[lhs];
                        let rhs_reg = self.ssa_to_lir[rhs];
                        LIRExpr::Binary {
                            op: op.clone(),
                            lhs: lhs_reg,
                            rhs: rhs_reg,
                        }
                    }
                    SSAExpr::Call { func, args } => {
                        let arg_regs = args.iter().map(|a| self.ssa_to_lir[a]).collect();
                        return Some(LIRInstr::Call {
                            func: func.clone(),
                            args: arg_regs,
                            dest: dest_reg,
                        });
                    }
                };
                let x = LIRInstr::Assign {
                    dest: dest_reg,
                    expr: lir_expr,
                };
                println!("{}", x.clone());
                Some(x)
                // println!("{}", LIRInstr::Assign {
                //     expr: lir_expr.,
                //     dest: dest_reg.clone(),
                // });
            }
        }
    }

    fn resolve_lir_temp(&self, id: &SSATempId) -> VirtualReg {
        self.ssa_to_lir
            .get(id)
            .or_else(|| self.phi_dest_regs.get(id))
            .copied()
            .expect("SSA value not found")
    }

    fn lower_terminator(&self, term: &Terminator) -> LIRTerminator {
        match term {
            Terminator::Goto(label) => LIRTerminator::Jump(label.clone()),
            Terminator::If {
                cond,
                then_label,
                else_label,
            } => {
                let cond_reg = VirtualReg(cond.0); // assumes TempId(u32) maps directly to vreg
                LIRTerminator::Branch {
                    cond: cond_reg,
                    then_label: then_label.clone(),
                    else_label: else_label.clone(),
                }
            }
            Terminator::Return(opt) => {
                let val = opt.as_ref().map(|t| VirtualReg(t.0));
                LIRTerminator::Return(val)
            }
        }
    }
}

impl std::fmt::Display for LIRProgram {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f, "entry: {}", self.entry.0)?;
        for label in &self.block_order {
            if let Some(block) = self.blocks.get(label) {
                writeln!(f, "{}", block)?;
            }
        }
        Ok(())
    }
}
