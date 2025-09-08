use std::collections::{HashMap, HashSet};
use std::fmt::format;

use crate::ssa::cfg::CFG;
use crate::ssa::ssa_version::SSAVersion;
use crate::types::hir_types::pretty_print_hir_instr;
use crate::types::literal::Literal;
use crate::types::ssa_types::{
    BasicBlock, SSAInstr, pretty_print_basic_block, pretty_print_basic_block_ssa,
    pretty_print_ssa_instr, pretty_print_terminator,
};
use crate::types::token::Token;
use crate::types::types::Type;
use crate::types::{
    hir_types::{HIRFunction, HIRInstr, Label, TempId},
    ssa_types::{SSAExpr, SSATempId, Terminator},
};

pub struct BlockBuilder {
    pub block_instrs: HashMap<Label, BasicBlock>,
    pub block_order: Vec<Label>,
    pub defsites: HashMap<String, HashSet<Label>>,
}

impl BlockBuilder {
    pub fn new() -> BlockBuilder {
        return BlockBuilder {
            block_instrs: HashMap::new(),
            block_order: Vec::new(),
            defsites: HashMap::new(),
        };
    }

    pub fn calculate_preds(&mut self) {
        let mut preds_map: HashMap<Label, Vec<Label>> = HashMap::new();

        for (label, block) in &self.block_instrs {
            let from = &block.label;
            match &block.terminator {
                Terminator::Goto(target) => {
                    preds_map
                        .entry(target.clone())
                        .or_default()
                        .push(from.clone());
                }
                Terminator::If {
                    then_label,
                    else_label,
                    ..
                } => {
                    preds_map
                        .entry(then_label.clone())
                        .or_default()
                        .push(from.clone());
                    preds_map
                        .entry(else_label.clone())
                        .or_default()
                        .push(from.clone());
                }
                Terminator::Return(_, _) => {
                    // no successors
                }
            }
        }

        for (label, preds) in &preds_map {
            if let Some(block) = self.block_instrs.get_mut(label) {
                block.preds = preds.clone();
            }
        }
    }

    pub fn create_blocks(&mut self, func_body: &Vec<HIRInstr>, ssa_versioning: &mut SSAVersion) {
        let mut current_instrs: Vec<HIRInstr> = vec![];
        let mut current_label: Option<Label> = None;
        for instr in func_body {
            match instr {
                HIRInstr::Label { label } => {
                    if let Some(label_id) = current_label.take() {
                        let temp = BasicBlock {
                            label: label_id.clone(),
                            instrs: current_instrs,
                            terminator: Terminator::Goto(label.clone()),
                            preds: vec![],
                            ssa_instrs: vec![],
                            phis: HashMap::new(),
                        };
                        self.block_order.push(label_id.clone());
                        self.block_instrs.insert(label_id, temp);
                    }
                    current_label = Some(label.clone());
                    current_instrs = vec![];
                }
                HIRInstr::Return { value, ty } => {
                    if let Some(label_id) = current_label.take() {
                        let x: Option<SSATempId> = if let Some(x) = value {
                            Some(ssa_versioning.convert_hir_temp_to_ssa_temp(x, None))
                        } else {
                            None
                        };

                        let temp = BasicBlock {
                            label: label_id.clone(),
                            ssa_instrs: vec![],
                            instrs: current_instrs,
                            terminator: Terminator::Return(x, ty.clone()),
                            preds: vec![],
                            phis: HashMap::new(),
                        };
                        self.block_order.push(label_id.clone());

                        self.block_instrs.insert(label_id, temp);

                        current_instrs = vec![];
                    }
                }

                HIRInstr::If {
                    cond,
                    then_label,
                    else_label,
                } => {
                    if let Some(label_id) = current_label.take() {
                        let temp = BasicBlock {
                            label: label_id.clone(),
                            ssa_instrs: vec![],
                            instrs: current_instrs,
                            terminator: Terminator::If {
                                cond: ssa_versioning.convert_hir_temp_to_ssa_temp(cond, None),
                                then_label: then_label.clone(),
                                else_label: else_label.clone(),
                            },
                            preds: vec![],
                            phis: HashMap::new(),
                        };
                        // println!("{}", pretty_print_ssa_block(&temp));
                        self.block_order.push(label_id.clone());

                        // blocks.push(temp.clone());
                        self.block_instrs.insert(label_id, temp);

                        current_instrs = vec![];
                    }
                }
                HIRInstr::Goto { target } => {
                    if let Some(label_id) = current_label.take() {
                        // let label = current_label.take().unwrap();
                        let temp = BasicBlock {
                            label: label_id.clone(),
                            instrs: current_instrs,
                            terminator: Terminator::Goto(target.clone()),
                            ssa_instrs: vec![],
                            preds: vec![],
                            phis: HashMap::new(),
                        };
                        // println!("{}", pretty_print_ssa_block(&temp));
                        self.block_order.push(label_id.clone());

                        // blocks.push(temp.clone());
                        self.block_instrs.insert(label_id, temp);
                        current_instrs = vec![];
                        // current_label = Some(label_id)
                    }
                }
                _ => {
                    if let HIRInstr::StoreVar { var, src, ty } = instr {
                        let x = self.defsites.entry(var.lexeme.clone()).or_default();
                        x.insert(current_label.clone().unwrap());
                        // .insert(current_label.clone().unwrap());
                    }
                    if let HIRInstr::LoadVar { var, dest, ty } = instr {
                        if !self.defsites.contains_key(&var.lexeme) {
                            self.defsites.entry(var.lexeme.clone()).or_default();
                        }
                    }
                    current_instrs.push(instr.clone());
                }
            }
        }
    }

    pub fn pretty_print_blocks(&self) {
        for i in &self.block_order {
            let block = self.block_instrs.get(i).unwrap();
            println!("{}", pretty_print_basic_block(block));
        }
    }
}

pub fn pretty_print_ssa_blocks(
    block_instrs: &HashMap<Label, BasicBlock>,
    block_order: &Vec<Label>,
) {
    for i in block_order {
        let block = block_instrs.get(i).unwrap();
        println!("{}", pretty_print_basic_block_ssa(block));
    }
    println!();
}

#[test]
fn test_single_block_with_return() {
    let mut ssa_versioning = SSAVersion::new();

    let l0 = Label("L0".to_string());
    let t1 = TempId(1);
    let token_x = Token::dummy_identifier("x");

    let instrs = vec![
        HIRInstr::Label { label: l0.clone() },
        HIRInstr::Const {
            dest: t1,
            value: Literal::Int(42),
            ty: Type::Int,
        },
        HIRInstr::StoreVar {
            var: token_x.clone(),
            src: t1,
            ty: Type::Int,
        },
        HIRInstr::Return {
            value: Some(t1),
            ty: Type::Int,
        },
    ];

    let mut bb = BlockBuilder::new();
    bb.create_blocks(&instrs, &mut ssa_versioning);

    assert_eq!(bb.block_order, vec![l0.clone()]);
    assert_eq!(bb.block_instrs.len(), 1);

    let block = bb.block_instrs.get(&l0).unwrap();
    assert_eq!(block.instrs.len(), 2);
    assert!(matches!(block.terminator, Terminator::Return(Some(_), _)));
    assert!(bb.defsites[&token_x.lexeme].contains(&l0));
}

#[test]
fn test_if_with_join() {
    let mut ssa_versioning = SSAVersion::new();

    let l0 = Label("L0".to_string());
    let l1 = Label("L1".to_string());
    let l2 = Label("L2".to_string());
    let l3 = Label("L3".to_string());

    let cond = TempId(0);
    let t1 = TempId(1);
    let t2 = TempId(2);
    let token_x = Token::dummy_identifier("x");

    let instrs = vec![
        HIRInstr::Label { label: l0.clone() },
        HIRInstr::If {
            cond,
            then_label: l1.clone(),
            else_label: l2.clone(),
        },
        HIRInstr::Label { label: l1.clone() },
        HIRInstr::Const {
            dest: t1,
            value: Literal::Int(1),
            ty: Type::Int,
        },
        HIRInstr::StoreVar {
            var: token_x.clone(),
            src: t1,
            ty: Type::Int,
        },
        HIRInstr::Goto { target: l3.clone() },
        HIRInstr::Label { label: l2.clone() },
        HIRInstr::Const {
            dest: t2,
            value: Literal::Int(2),
            ty: Type::Int,
        },
        HIRInstr::StoreVar {
            var: token_x.clone(),
            src: t2,
            ty: Type::Int,
        },
        HIRInstr::Goto { target: l3.clone() },
        HIRInstr::Label { label: l3.clone() },
        HIRInstr::Return {
            value: Some(t1),
            ty: Type::Int,
        },
    ];

    let mut bb = BlockBuilder::new();
    bb.create_blocks(&instrs, &mut ssa_versioning);

    assert_eq!(
        bb.block_order,
        vec![l0.clone(), l1.clone(), l2.clone(), l3.clone()]
    );
    assert_eq!(bb.block_instrs.len(), 4);

    assert!(matches!(
        &bb.block_instrs[&l0].terminator,
        Terminator::If { then_label, else_label, .. }
            if then_label == &l1 && else_label == &l2
    ));

    assert!(matches!(bb.block_instrs[&l1].terminator, Terminator::Goto(ref tgt) if *tgt == l3));
    assert!(matches!(bb.block_instrs[&l2].terminator, Terminator::Goto(ref tgt) if *tgt == l3));
    assert!(matches!(
        bb.block_instrs[&l3].terminator,
        Terminator::Return(Some(_), _)
    ));

    let defs = bb.defsites.get(&token_x.lexeme).unwrap();
    assert!(defs.contains(&l1));
    assert!(defs.contains(&l2));
}

#[test]
fn test_goto_chain() {
    let mut ssa_versioning = SSAVersion::new();

    let a = Label("A".to_string());
    let b = Label("B".to_string());

    let t1 = TempId(1);
    let t2 = TempId(2);

    let token_y = Token::dummy_identifier("y");
    let token_x = Token::dummy_identifier("x");

    let instrs = vec![
        HIRInstr::Label { label: a.clone() },
        HIRInstr::Const {
            dest: t1,
            value: Literal::Int(5),
            ty: Type::Int,
        },
        HIRInstr::StoreVar {
            var: token_y.clone(),
            src: t1,
            ty: Type::Int,
        },
        HIRInstr::Goto { target: b.clone() },
        HIRInstr::Label { label: b.clone() },
        HIRInstr::Move {
            dest: t2,
            src: t1,
            ty: Type::Int,
        },
        HIRInstr::StoreVar {
            var: token_x.clone(),
            src: t2,
            ty: Type::Int,
        },
        HIRInstr::Return {
            value: Some(t2),
            ty: Type::Int,
        },
    ];

    let mut bb = BlockBuilder::new();
    bb.create_blocks(&instrs, &mut ssa_versioning);

    assert_eq!(bb.block_order, vec![a.clone(), b.clone()]);
    assert_eq!(bb.block_instrs.len(), 2);
    assert!(matches!(bb.block_instrs[&a].terminator, Terminator::Goto(ref tgt) if *tgt == b));
    assert!(matches!(
        bb.block_instrs[&b].terminator,
        Terminator::Return(Some(_),_)
    ));

    assert_eq!(
        bb.defsites[&token_x.lexeme],
        [b.clone()].iter().cloned().collect()
    );
    assert_eq!(
        bb.defsites[&token_y.lexeme],
        [a.clone()].iter().cloned().collect()
    );
}

#[test]
fn test_loop_backedge() {
    let mut ssa_versioning = SSAVersion::new();

    let loop_lbl = Label("Loop".to_string());
    let exit_lbl = Label("Exit".to_string());
    let t1 = TempId(1);

    let token_i = Token::dummy_identifier("i");

    let instrs = vec![
        HIRInstr::Label {
            label: loop_lbl.clone(),
        },
        HIRInstr::Const {
            dest: t1,
            value: Literal::Int(0),
            ty: Type::Int,
        },
        HIRInstr::StoreVar {
            var: token_i.clone(),
            src: t1,
            ty: Type::Int,
        },
        HIRInstr::If {
            cond: t1,
            then_label: loop_lbl.clone(), // backedge
            else_label: exit_lbl.clone(),
        },
        HIRInstr::Label {
            label: exit_lbl.clone(),
        },
        HIRInstr::Return {
            value: Some(t1),
            ty: Type::Int,
        },
    ];

    let mut bb = BlockBuilder::new();
    bb.create_blocks(&instrs, &mut ssa_versioning);

    assert_eq!(bb.block_order, vec![loop_lbl.clone(), exit_lbl.clone()]);
    assert_eq!(bb.block_instrs.len(), 2);

    assert!(matches!(
        &bb.block_instrs[&loop_lbl].terminator,
        Terminator::If { then_label, else_label, .. }
            if then_label == &loop_lbl && else_label == &exit_lbl
    ));

    assert!(matches!(
        bb.block_instrs[&exit_lbl].terminator,
        Terminator::Return(Some(_),_)
    ));
    assert_eq!(
        bb.defsites[&token_i.lexeme],
        [loop_lbl.clone()].iter().cloned().collect()
    );
}
