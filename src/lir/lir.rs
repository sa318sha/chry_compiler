use std::collections::HashMap;
use std::fmt;

use crate::ssa::ssa::SSAContext;
use crate::types::hir_types::HIRInstr;
use crate::types::lir_types::{LIRBasicBlock, LIRExpr, LIRTerminator, VirtualReg};
use crate::types::ssa_types::PhiNode;
use crate::types::types::Type;
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
    reverse_post_order: Vec<Label>,
    block_order: Vec<Label>, // inherited from SSAProgram
    phi_dest_regs: HashMap<SSATempId, VirtualReg>,
}
pub struct LIRProgram {
    entry: Label,                              // Entry point to the function
    pub blocks: HashMap<Label, LIRBasicBlock>, // Block label → block body
    pub block_order: Vec<Label>,               // linear traversal of blocks visited
    pub reverse_post_order: Vec<Label>,
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
            reverse_post_order: ssa_context.metadata.cfg.post_order.clone(),
        };
        //0th pass
        for block_label in &ssa_context.program.block_order {
            let phis = &ssa_context.program.blocks[block_label].phis;
            for (var, phi_node) in phis {
                let dest_reg = lowering.reg_alloc.fresh(&phi_node.ty);
                lowering
                    .phi_dest_regs
                    .insert(phi_node.dest.clone(), dest_reg);
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
            reverse_post_order: lowering.reverse_post_order,
            // predecessor:
        }
    }

    fn insert_phi_moves_for_block(&mut self, label: &Label, phis: &HashMap<String, PhiNode>) {
        // let phis = &ssa_context.program.blocks[block_label].phis

        let mut phi_moves_per_pred: HashMap<Label, Vec<(VirtualReg, VirtualReg)>> = HashMap::new();

        for (var, phi_node) in phis {
            for (pred_label, incoming_ssa_id) in &phi_node.args {
                let src_reg: VirtualReg = self.resolve_lir_temp(incoming_ssa_id); //(or phi_dest_regs if it's a phi)
                let dest_reg = self.phi_dest_regs[&phi_node.dest].clone();
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
                    let temp = self.reg_alloc.fresh_from_reg_size(&dest.reg_size);
                    let pred_block = self.lir_blocks.get_mut(&pred_label).unwrap();
                    pred_block.instrs.push(LIRInstr::Move {
                        dest: temp.clone(),
                        src: src.clone(),
                    });
                    temps.insert(dest.clone(), temp);
                } else {
                    safe_moves.push((dest.clone(), src.clone()));
                }
            }

            for (dest, temp) in &temps {
                let pred_block = self.lir_blocks.get_mut(&pred_label).unwrap();
                pred_block.instrs.push(LIRInstr::Move {
                    dest: dest.clone(),
                    src: temp.clone(),
                });
            }
            for (dest, src) in &safe_moves {
                let pred_block = self.lir_blocks.get_mut(&pred_label).unwrap();
                pred_block.instrs.push(LIRInstr::Move {
                    dest: dest.clone(),
                    src: src.clone(),
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
            preds: block.preds.clone(),
            label: block.label.clone(),
            instrs,
            terminator: terminator,
        }
    }

    fn lower_ssa_instr(&mut self, ssa_instr: &SSAInstr) -> Option<LIRInstr> {
        match ssa_instr {
            SSAInstr::Assign { dest, expr, ty } => {
                let dest_reg = self.reg_alloc.fresh(&ty);
                self.ssa_to_lir.insert(dest.clone(), dest_reg.clone());

                let lir_expr = match expr {
                    SSAExpr::Const(lit) => LIRExpr::Const(lit.clone()),
                    SSAExpr::Var { val } => {
                        // println!("{}_{}", val.name, val.version);
                        let src = &self.resolve_lir_temp(val);
                        LIRExpr::Var(src.clone())
                    }
                    SSAExpr::Unary { op, src } => {
                        let src_reg = &self.ssa_to_lir[src];
                        LIRExpr::Unary {
                            op: op.clone(),
                            val: src_reg.clone(),
                        }
                    }
                    SSAExpr::Binary { op, lhs, rhs } => {
                        let lhs_reg = self.ssa_to_lir[lhs].clone();
                        let rhs_reg = self.ssa_to_lir[rhs].clone();
                        LIRExpr::Binary {
                            op: op.clone(),
                            lhs: lhs_reg,
                            rhs: rhs_reg,
                        }
                    }
                    SSAExpr::Call { func, args } => {
                        let arg_regs = args.iter().map(|a| self.ssa_to_lir[a].clone()).collect();
                        return Some(LIRInstr::Call {
                            func: func.clone(),
                            args: arg_regs,
                            dest: dest_reg.clone(),
                            ty: ty.clone(),
                        });
                    }
                };
                let x = LIRInstr::Assign {
                    dest: dest_reg.clone(),
                    expr: lir_expr,
                    ty: ty.clone(),
                };
                // println!("{}", x.clone());
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
            .cloned()
            .expect("SSA value not found")
    }

    fn lower_terminator(&mut self, term: &Terminator) -> LIRTerminator {
        match term {
            Terminator::Goto(label) => LIRTerminator::Jump(label.clone()),
            Terminator::If {
                cond,
                then_label,
                else_label,
            } => {
                let cond_reg = self.resolve_lir_temp(&cond); // assumes TempId(u32) maps directly to vreg
                LIRTerminator::Branch {
                    cond: cond_reg,
                    then_label: then_label.clone(),
                    else_label: else_label.clone(),
                }
            }
            Terminator::Return(opt, ty) => {
                let val = opt.as_ref().map(|t| self.resolve_lir_temp(&t));
                LIRTerminator::Return(val)
            }
        }
    }
}

// pub fn pretty_print_lir_programs(programs: &[LIRProgram]) -> String {
//     let mut out = String::new();
//     for (i, p) in programs.iter().enumerate() {
//         let _ = writeln!(out, ";; ===========================================");
//         let _ = writeln!(out, ";; Function {}:", i);
//         let _ = writeln!(out, "{}", p);
//     }
//     out
// }

impl fmt::Display for LIRProgram {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, ";; LIR Program")?;
        writeln!(f, "entry: {}", self.entry.0)?;
        writeln!(f, "blocks: {}", self.blocks.len())?;
        writeln!(f)?;

        for lbl in &self.block_order {
            match self.blocks.get(lbl) {
                Some(bb) => {
                    // LIRBasicBlock already has Display
                    writeln!(f, "{}", bb)?;
                }
                None => {
                    // Keep debugging-friendly if block_order references a missing block.
                    writeln!(f, "LABEL {}:\n  <missing block body>\n", lbl.0)?;
                }
            }
        }
        Ok(())
    }
}
