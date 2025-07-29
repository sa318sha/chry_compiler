use std::collections::HashMap;

use crate::types::{
    hir_types::{HIRFunction, HIRInstr, Label, TempId, pretty_print_hir_instr},
    literal::{Literal, pretty_print_literal},
    op::{BinaryOp, UnaryOp},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SSATempId {
    pub name: String,
    pub version: usize,
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub label: Label,
    pub instrs: Vec<HIRInstr>,
    pub ssa_instrs: Vec<SSAInstr>,
    pub terminator: Terminator,
    pub preds: Vec<Label>,
    pub phis: HashMap<String, PhiNode>,
}

pub fn pretty_print_basic_block_ssa(block: &BasicBlock) -> String {
    let label = format!("LABEL {}:", block.label.0);

    let phis = block
        .phis
        .iter()
        .map(|(string, x)| format!("  PHI: {} => {}", string, pretty_print_phi_node(x)))
        .collect::<Vec<_>>()
        .join("\n");

    let instrs = block
        .ssa_instrs
        .iter()
        .map(|instr| format!("  {}", pretty_print_ssa_instr(instr)))
        .collect::<Vec<_>>()
        .join("\n");

    let terminator = format!("  {}", pretty_print_terminator(&block.terminator));
    if phis.len() > 0 {
        format!("{}\n{}\n{}\n{}", label, phis, instrs, terminator)
    } else {
        format!("{}\n{}\n{}", label, instrs, terminator)
    }
}

pub fn pretty_print_basic_block(block: &BasicBlock) -> String {
    let label = format!("LABEL {}:", block.label.0);

    // let phis = block
    //     .phis
    //     .iter()
    //     .map(|(string, x)| format!("  {} {}", string, pretty_print_phi_node(x)))
    //     .collect::<Vec<_>>()
    //     .join("\n");

    let instrs = block
        .instrs
        .iter()
        .map(|instr| format!("{}", pretty_print_hir_instr(instr, 2)))
        .collect::<Vec<_>>()
        .join("\n");

    let terminator = format!("  {}", pretty_print_terminator(&block.terminator));

    format!("{}\n{}\n{}", label, instrs, terminator)
}

#[derive(Debug, Clone)]
pub struct PhiNode {
    pub(crate) dest: SSATempId,
    pub args: HashMap<Label, SSATempId>,
}

pub fn pretty_print_phi_node(phi_node: &PhiNode) -> String {
    let t = phi_node
        .args
        .iter()
        .map(|(label, tempid)| format!("{}_{}: {} ", tempid.name, tempid.version, label.0))
        .collect::<Vec<_>>()
        .join(", ");

    return format!(
        "dest: {}_{} <= {}",
        phi_node.dest.name, phi_node.dest.version, t
    );
}

#[derive(Debug, Clone)]
pub struct SSABlock {
    pub label: Label,
    pub instrs: Vec<SSAInstr>,
    pub terminator: Terminator,
    pub preds: Vec<Label>,
    pub phis: HashMap<String, PhiNode>,
}

pub fn pretty_print_ssa_block(block: &SSABlock) -> String {
    let label = format!("LABEL {}:", block.label.0);

    let phis = block
        .phis
        .iter()
        .map(|(string, x)| format!("  {} {}", string, pretty_print_phi_node(x)))
        .collect::<Vec<_>>()
        .join("\n");

    let instrs = block
        .instrs
        .iter()
        .map(|instr| format!("{}", pretty_print_ssa_instr(instr)))
        .collect::<Vec<_>>()
        .join("\n");

    let terminator = format!("  {}", pretty_print_terminator(&block.terminator));

    format!("{}\n{}\n{}\n{}", label, phis, instrs, terminator)
}

pub struct SSAFunction {
    pub name: String,
    pub args: Vec<(String, SSATempId)>,
    pub blocks: Vec<SSABlock>,
}

// pub fn pretty_print_ssa_function(func: &SSAFunction) -> String {
//     let args = func
//         .args
//         .iter()
//         .map(|(name, id)| format!("{}: {}_{}", name, id.name, id.version))
//         .collect::<Vec<_>>()
//         .join(", ");

//     let header = format!("fun {}({}) {{", func.name, args);

//     let body = func
//         .blocks
//         .iter()
//         .map(|block| pretty_print_ssa_block(block))
//         .collect::<Vec<_>>()
//         .join("\n");

//     format!("{}\n{}\n}}\n", header, body)
// }

// #[derive(Debug, Clone)]
// pub struct SSABasicBlock {
//     pub label: Label,
//     pub instrs: Vec<SSAInstr>,
//     pub terminator: SSATerminator,
//     pub preds: Vec<Label>,
// }

// impl SSATerminator {
//     pub fn successors(&self) -> Vec<Label> {
//         match &self {
//             SSATerminator::Goto(target) => vec![target.clone()],
//             SSATerminator::If {
//                 then_label,
//                 else_label,
//                 ..
//             } => {
//                 vec![then_label.clone(), else_label.clone()]
//             }
//             SSATerminator::Return(_) => vec![],
//         }
//     }
// }

// pub fn pretty_print_ssa_block(block: &SSABasicBlock) -> String {
//     let label = format!("LABEL {}:", block.label.0);

//     let instrs = block
//         .instrs
//         .iter()
//         .map(|instr| format!("  {}", pretty_print_ssa_instr(instr)))
//         .collect::<Vec<_>>()
//         .join("\n");

//     let terminator = format!("  {}", pretty_print_ssa_terminator(&block.terminator));

//     format!("{}\n{}\n{}", label, instrs, terminator)
// }

#[derive(Debug, Clone)]
pub enum SSAInstr {
    Assign { dest: SSATempId, expr: SSAExpr },
    // Phi {
    //     dest: SSATempId,
    //     sources: Vec<(Label, SSATempId)>,
    // },
}

impl SSAInstr {
    pub fn rename_uses(&mut self, stack: &HashMap<String, Vec<SSATempId>>) {
        match self {
            SSAInstr::Assign { expr, .. } => match expr {
                SSAExpr::Var { val } => {
                    *val = stack[&val.name].last().unwrap().clone();
                }
                SSAExpr::Unary { src, .. } => {
                    *src = stack[&src.name].last().unwrap().clone();
                }
                SSAExpr::Binary { lhs, rhs, .. } => {
                    *lhs = stack[&lhs.name].last().unwrap().clone();
                    *rhs = stack[&rhs.name].last().unwrap().clone();
                }
                SSAExpr::Call { args, .. } => {
                    for arg in args.iter_mut() {
                        *arg = stack[&arg.name].last().unwrap().clone();
                    }
                }
                _ => {}
            },
            // SSAInstr::Phi { dest, sources } => todo!(),
        }
    }

    pub fn defines(&self) -> Option<SSATempId> {
        match self {
            SSAInstr::Assign { dest, .. } => {
                if dest.name != "_tmp" {
                    Some(dest.clone())
                } else {
                    None
                }
            } // SSAInstr::Phi { dest, sources } => todo!(),
        }
    }

    pub fn rename_def(&mut self, new_id: SSATempId) {
        match self {
            SSAInstr::Assign { dest, .. } => *dest = new_id,
            // SSAInstr::Phi { dest, sources } => todo!(),
        }
    }
    pub fn uses(&self) -> Vec<SSATempId> {
        match self {
            SSAInstr::Assign { expr, .. } => match expr {
                SSAExpr::Var { val } => vec![val.clone()],
                SSAExpr::Unary { src, .. } => vec![src.clone()],
                SSAExpr::Binary { lhs, rhs, .. } => {
                    vec![lhs.clone(), rhs.clone()]
                }
                SSAExpr::Call { args, .. } => args.iter().map(|arg| arg.clone()).collect(),
                _ => vec![],
            },
        }
    }
}

pub fn pretty_print_ssa_instr(instr: &SSAInstr) -> String {
    match instr {
        SSAInstr::Assign { dest, expr } => {
            format!(
                "{}_{} = {}",
                dest.name,
                dest.version,
                pretty_print_ssa_expr(expr)
            )
        } // SSAInstr::Phi { dest, sources } => {
          //     let srcs = sources
          //         .iter()
          //         .map(|(label, id)| format!("{}: {}_{}", label.0, id.name, id.version))
          //         .collect::<Vec<_>>()
          //         .join(", ");
          //     format!("{}_{} = PHI({})", dest.name, dest.version, srcs)
          // }
    }
}

#[derive(Debug, Clone)]
pub enum Terminator {
    Goto(Label),
    If {
        cond: TempId,
        then_label: Label,
        else_label: Label,
    },
    Return(Option<TempId>),
}
impl Terminator {
    pub fn successors(&self) -> Vec<Label> {
        match self {
            Terminator::Goto(label) => vec![label.clone()],
            Terminator::If {
                then_label,
                else_label,
                ..
            } => vec![then_label.clone(), else_label.clone()],
            Terminator::Return(_) => vec![],
        }
    }
}

pub fn pretty_print_terminator(term: &Terminator) -> String {
    match term {
        Terminator::Goto(label) => format!("GOTO {}", label.0),
        Terminator::If {
            cond,
            then_label,
            else_label,
        } => format!("IF {} THEN {} ELSE {}", cond.0, then_label.0, else_label.0),
        Terminator::Return(Some(temp)) => format!("RETURN {}", temp.0),
        Terminator::Return(None) => "RETURN".to_string(),
    }
}

// #[derive(Debug, Clone)]
// pub enum SSATerminator {
//     Goto(Label),
//     If {
//         cond: SSATempId,
//         then_label: Label,
//         else_label: Label,
//     },
//     Return(Option<SSATempId>),
// }

// pub fn ssa_successors(terminator: &SSATerminator) -> Vec<Label> {
//     match terminator {
//         SSATerminator::Goto(label) => vec![label.clone()],
//         SSATerminator::If {
//             then_label,
//             else_label,
//             ..
//         } => vec![then_label.clone(), else_label.clone()],
//         SSATerminator::Return(_) => vec![],
//     }
// }

// pub fn pretty_print_ssa_terminator(term: &SSATerminator) -> String {
//     match term {
//         SSATerminator::Goto(label) => format!("GOTO {}", label.0),
//         SSATerminator::If {
//             cond,
//             then_label,
//             else_label,
//         } => format!(
//             "IF {}_{} THEN {} ELSE {}",
//             cond.name, cond.version, then_label.0, else_label.0
//         ),
//         SSATerminator::Return(Some(temp)) => format!("RETURN {}_{}", temp.name, temp.version),
//         SSATerminator::Return(None) => "RETURN".to_string(),
//     }
// }
#[derive(Debug, Clone)]
pub enum SSAExpr {
    Const(Literal),
    Binary {
        op: BinaryOp,
        lhs: SSATempId,
        rhs: SSATempId,
    },
    Unary {
        op: UnaryOp,
        src: SSATempId,
    },
    Call {
        func: String,
        args: Vec<SSATempId>,
    },
    Var {
        val: SSATempId,
    }, // etc.
}

pub fn pretty_print_ssa_expr(expr: &SSAExpr) -> String {
    match expr {
        SSAExpr::Const(lit) => pretty_print_literal(lit),
        SSAExpr::Binary { op, lhs, rhs } => {
            format!(
                "{}_{} {:?} {}_{}",
                lhs.name, lhs.version, op, rhs.name, rhs.version
            )
        }
        SSAExpr::Unary { op, src } => {
            format!("{:?} {}_{}", op, src.name, src.version)
        }
        SSAExpr::Call { func, args } => {
            let arg_strs = args
                .iter()
                .map(|a| format!("{}_{}", a.name, a.version))
                .collect::<Vec<_>>()
                .join(", ");
            format!("CALL {}({})", func, arg_strs)
        }
        SSAExpr::Var { val } => format!("{}_{}", val.name, val.version),
    }
}
