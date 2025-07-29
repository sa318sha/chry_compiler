use std::collections::HashMap;

use crate::types::{
    hir_types::Label,
    literal::Literal,
    op::{BinaryOp, UnaryOp},
    ssa_types::SSATempId,
};

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
pub struct VirtualReg(pub u32); // with a name-to-id map

pub struct VirtualRegAllocator {
    pub next_id: u32,
}

impl VirtualRegAllocator {
    pub fn fresh(&mut self) -> VirtualReg {
        let id = self.next_id;
        self.next_id += 1;
        VirtualReg(id)
    }
}

pub struct LIRBasicBlock {
    pub label: Label,          // Same label from SSA
    pub instrs: Vec<LIRInstr>, // Sequential instructions
    pub pending_moves: Vec<LIRInstr>,
    pub terminator: LIRTerminator, // Final jump/branch/return/etc.
}

#[derive(Debug, Clone)]
pub enum LIRInstr {
    Assign {
        dest: VirtualReg,
        expr: LIRExpr, // Binary, Unary, Const, Var, etc.
    },
    Move {
        dest: VirtualReg,
        src: VirtualReg,
    },
    Call {
        func: String,
        args: Vec<VirtualReg>,
        dest: VirtualReg,
    },
    Print {
        src: VirtualReg,
    },
    Nop,
}

#[derive(Debug, Clone)]
pub enum LIRExpr {
    Const(Literal),
    Unary {
        op: UnaryOp,
        val: VirtualReg,
    },
    Binary {
        op: BinaryOp,
        lhs: VirtualReg,
        rhs: VirtualReg,
    },
    Var(VirtualReg),
}

pub enum LIRTerminator {
    Jump(Label),
    Branch {
        cond: VirtualReg,
        then_label: Label,
        else_label: Label,
    },
    Return(Option<VirtualReg>),
}

impl std::fmt::Display for LIRBasicBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f, "LABEL {}:", self.label.0)?;
        for instr in &self.instrs {
            writeln!(f, "  {}", instr)?;
        }
        writeln!(f, "  {}", self.terminator)
    }
}

impl std::fmt::Display for LIRInstr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            LIRInstr::Assign { dest, expr } => write!(f, "v{} = {}", dest.0, expr),
            LIRInstr::Move { dest, src } => write!(f, "v{} = MOVE v{}", dest.0, src.0),
            LIRInstr::Call { func, args, dest } => {
                let arg_strs = args
                    .iter()
                    .map(|v| format!("v{}", v.0))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "v{} = CALL {}({})", dest.0, func, arg_strs)
            }
            LIRInstr::Print { src } => write!(f, "PRINT v{}", src.0),
            LIRInstr::Nop => write!(f, "NOP"),
        }
    }
}

impl std::fmt::Display for LIRExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            LIRExpr::Const(lit) => write!(f, "CONST {:?}", lit),
            LIRExpr::Unary { op, val } => write!(f, "{:?} v{}", op, val.0),
            LIRExpr::Binary { op, lhs, rhs } => write!(f, "v{} {:?} v{}", lhs.0, op, rhs.0),
            LIRExpr::Var(v) => write!(f, "v{}", v.0),
        }
    }
}

impl std::fmt::Display for LIRTerminator {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            LIRTerminator::Jump(label) => write!(f, "GOTO {}", label.0),
            LIRTerminator::Branch {
                cond,
                then_label,
                else_label,
            } => {
                write!(
                    f,
                    "IF v{} THEN {} ELSE {}",
                    cond.0, then_label.0, else_label.0
                )
            }
            LIRTerminator::Return(Some(v)) => write!(f, "RETURN v{}", v.0),
            LIRTerminator::Return(None) => write!(f, "RETURN"),
        }
    }
}
