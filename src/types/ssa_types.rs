use std::collections::HashMap;

use crate::types::{
    hir_types::{HIRFunction, HIRInstr, Label, pretty_print_hir_instr},
    literal::{Literal, pretty_print_literal},
    op::{BinaryOp, UnaryOp},
    types::{Type, pretty_print_type},
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

#[derive(Debug, Clone)]
pub struct PhiNode {
    pub(crate) dest: SSATempId,
    pub ty: Type,
    pub args: HashMap<Label, SSATempId>,
}

#[derive(Debug, Clone)]
pub enum SSAInstr {
    Assign {
        dest: SSATempId,
        ty: Type,
        expr: SSAExpr,
    },
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

#[derive(Debug, Clone)]
pub enum Terminator {
    Goto(Label),
    If {
        cond: SSATempId,
        then_label: Label,
        else_label: Label,
    },
    Return(Option<SSATempId>, Type),
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
            Terminator::Return(_, _) => vec![],
        }
    }
}

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

    let instrs = block
        .instrs
        .iter()
        .map(|instr| format!("{}", pretty_print_hir_instr(instr, 2)))
        .collect::<Vec<_>>()
        .join("\n");

    let terminator = format!("  {}", pretty_print_terminator(&block.terminator));

    format!("{}\n{}\n{}", label, instrs, terminator)
}

pub fn pretty_print_terminator(term: &Terminator) -> String {
    match term {
        Terminator::Goto(label) => format!("GOTO {}", label.0),
        Terminator::If {
            cond,
            then_label,
            else_label,
        } => format!(
            "IF {}_{} THEN {} ELSE {}",
            cond.name, cond.version, then_label.0, else_label.0
        ),
        Terminator::Return(Some(temp, ), ty) => format!("RETURN {}_{}: {}", temp.name, temp.version, pretty_print_type(ty)),
        Terminator::Return(None, ty) => "RETURN".to_string(),
    }
}

pub fn pretty_print_ssa_instr(instr: &SSAInstr) -> String {
    match instr {
        SSAInstr::Assign { dest, expr, ty } => {
            format!(
                "{}_{} = {}: {}",
                dest.name,
                dest.version,
                pretty_print_ssa_expr(expr),
                pretty_print_type(ty),
            )
        } // SSAInstr::Phi { dest, sources } => {
    }
}
pub fn pretty_print_phi_node(phi_node: &PhiNode) -> String {
    let t = phi_node
        .args
        .iter()
        .map(|(label, tempid)| format!("{}_{}: {} ", tempid.name, tempid.version, label.0))
        .collect::<Vec<_>>()
        .join(", ");

    return format!(
        "dest: {}_{}: {} = {}",
        phi_node.dest.name,
        phi_node.dest.version,
        pretty_print_type(&phi_node.ty),
        t
    );
}
