use std::collections::HashMap;

use crate::types::{
    hir_types::Label,
    literal::Literal,
    op::{BinaryOp, UnaryOp},
    ssa_types::SSATempId,
    types::{Type, pretty_print_type},
    x86_64::RegSize,
};

#[derive(Debug, Clone)]
pub struct VirtualReg {
    pub id: u32,
    pub reg_size: RegSize,
} // with a name-to-id map

impl PartialEq for VirtualReg {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
impl Eq for VirtualReg {}

impl std::hash::Hash for VirtualReg {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

pub fn type_to_reg_size(ty: &Type) -> RegSize {
    match ty {
        Type::Bool => RegSize::Byte,          // bool is typically 1 byte
        Type::Int => RegSize::QWord,          // assuming 64-bit integers (adjust if needed)
        Type::Float => RegSize::QWord,        // assuming 64-bit IEEE 754 (f64)
        Type::Nil => RegSize::QWord,          // could be a pointer or tagged null
        Type::String => RegSize::QWord,       // pointer to heap-allocated string
        Type::Function(..) => RegSize::QWord, // functions are pointers

        Type::Struct { .. } => RegSize::QWord, // handled via memory access / address
        Type::Enum { .. } => RegSize::QWord,   // assume tagged unions or pointer
        Type::Void => RegSize::QWord,          // safe default (not stored in a register)
    }
}

pub struct VirtualRegAllocator {
    pub next_id: u32,
}

impl VirtualRegAllocator {
    pub fn fresh(&mut self, ty: &Type) -> VirtualReg {
        let id = self.next_id;
        self.next_id += 1;
        VirtualReg {
            id: id,
            reg_size: type_to_reg_size(ty),
        }
    }
    pub fn fresh_from_reg_size(&mut self, reg_size: &RegSize) -> VirtualReg {
        let id = self.next_id;
        self.next_id += 1;
        VirtualReg {
            id: id,
            reg_size: reg_size.clone(),
        }
    }
}

pub struct LIRBasicBlock {
    pub label: Label, // Same label from SSA
    // pub instr_start: usize,
    pub preds: Vec<Label>,
    pub instrs: Vec<LIRInstr>, // Sequential instructions
    // pub pending_moves: Vec<LIRInstr>,
    pub terminator: LIRTerminator, // Final jump/branch/return/etc.
}

#[derive(Debug, Clone)]
pub enum LIRInstr {
    Assign {
        dest: VirtualReg,
        expr: LIRExpr, // Binary, Unary, Const, Var, etc.
        ty: Type,
    },
    Move {
        dest: VirtualReg,
        src: VirtualReg,
        // ty: Type,
    },
    Call {
        func: String,
        args: Vec<VirtualReg>,
        dest: VirtualReg,
        ty: Type,
    },
    // Print {
    //     src: VirtualReg,
    // },
    Nop,
}

impl LIRInstr {
    pub fn defs(&self) -> Vec<VirtualReg> {
        match self {
            LIRInstr::Assign { dest, .. } => vec![dest.clone()],
            LIRInstr::Move { dest, .. } => vec![dest.clone()],
            LIRInstr::Call { dest, .. } => vec![dest.clone()],
            LIRInstr::Nop => vec![],
        }
    }
    pub fn uses(&self) -> Vec<VirtualReg> {
        match self {
            LIRInstr::Assign { expr, .. } => expr.uses(),
            LIRInstr::Move { src, .. } => vec![src.clone()],
            LIRInstr::Call { args, .. } => args.clone(),
            LIRInstr::Nop => vec![],
        }
    }
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

impl LIRTerminator {
    pub fn successors(&self) -> Vec<Label> {
        match self {
            LIRTerminator::Jump(label) => vec![label.clone()],
            LIRTerminator::Branch {
                cond,
                then_label,
                else_label,
            } => vec![then_label.clone(), else_label.clone()],
            LIRTerminator::Return(virtual_reg) => vec![],
        }
    }
    pub fn uses(&self) -> Vec<VirtualReg> {
        match self {
            LIRTerminator::Jump(label) => vec![],
            LIRTerminator::Branch {
                cond,
                then_label,
                else_label,
            } => vec![cond.clone()],
            LIRTerminator::Return(virtual_reg) => {
                if let Some(x) = virtual_reg {
                    return vec![x.clone()];
                } else {
                    return vec![];
                }
            }
        }
    }
}

impl LIRExpr {
    fn uses(&self) -> Vec<VirtualReg> {
        match self {
            LIRExpr::Const(_) => vec![],
            LIRExpr::Var(v) => vec![v.clone()],
            LIRExpr::Unary { val, .. } => vec![val.clone()],
            LIRExpr::Binary { lhs, rhs, .. } => vec![lhs.clone(), rhs.clone()],
        }
    }
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
            LIRInstr::Assign { dest, expr, ty } => {
                write!(f, "{} = {}: {}", dest, expr, pretty_print_type(ty))
            }
            LIRInstr::Move { dest, src } => write!(
                f,
                "{} = MOVE {}",
                dest,
                src,
                // pretty_print_type(ty)
            ),
            LIRInstr::Call {
                func,
                args,
                dest,
                ty,
            } => {
                let arg_strs = args
                    .iter()
                    .map(|v| format!("{}", v))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(
                    f,
                    "{} = CALL {}({}): {}",
                    dest,
                    func,
                    arg_strs,
                    pretty_print_type(ty)
                )
            }
            // LIRInstr::Print { src } => write!(f, "PRINT {}", src.0),
            LIRInstr::Nop => write!(f, "NOP"),
        }
    }
}

impl std::fmt::Display for LIRExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            LIRExpr::Const(lit) => write!(f, "CONST {:?}", lit),
            LIRExpr::Unary { op, val } => write!(f, "{:?} {}", op, val),
            LIRExpr::Binary { op, lhs, rhs } => write!(f, "{} {:?} {}", lhs, op, rhs),
            LIRExpr::Var(v) => write!(f, "{}", v),
        }
    }
}

impl std::fmt::Display for VirtualReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "v{}: {}", self.id, self.reg_size)
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
                write!(f, "IF {} THEN {} ELSE {}", cond, then_label.0, else_label.0)
            }
            LIRTerminator::Return(Some(v)) => write!(f, "RETURN {}", v),
            LIRTerminator::Return(None) => write!(f, "RETURN"),
        }
    }
}
