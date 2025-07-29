use std::default;

use crate::types::literal::{Literal, pretty_print_literal};
use crate::types::op::{BinaryOp, UnaryOp};
use crate::types::token::Token;
use crate::types::types::{Type, pretty_print_type};

/// A unique ID for a temporary (SSA-friendly)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TempId(pub u32);
impl TempId {
    pub fn name(&self) -> String {
        format!("t{}", self.0)
    }
}

/// A unique label
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Label(pub String);

pub type ConstValue = Literal;

/// The HIR instruction set
#[derive(Debug, Clone, PartialEq)]
pub enum HIRInstr {
    /// dest = const value
    Const {
        dest: TempId,
        value: ConstValue,
        ty: Type,
    },

    /// dest = unary_op src
    UnaryOp {
        dest: TempId,
        op: UnaryOp,
        src: TempId,
        ty: Type,
    },

    /// dest = binary_op lhs rhs
    BinaryOp {
        dest: TempId,
        op: BinaryOp,
        lhs: TempId,
        rhs: TempId,
        ty: Type,
    },

    /// dest = src_var
    LoadVar {
        dest: TempId,
        var: Token,
        ty: Type,
    },

    /// var = src
    StoreVar {
        var: Token,
        src: TempId,
        ty: Type,
    },

    /// dest = call func_name(args)
    Call {
        dest: TempId,
        func: Token,
        args: Vec<TempId>,
        ty: Type,
    },

    /// Print a value (side effect)
    Print {
        src: TempId,
        ty: Type,
    },

    /// Return
    Return {
        value: Option<TempId>,
        ty: Type,
    },

    /// Conditional branch
    If {
        cond: TempId,
        then_label: Label,
        else_label: Label,
    },

    /// Unconditional jump
    Goto {
        target: Label,
    },

    /// Label marker
    Label {
        label: Label,
    },

    Move {
        dest: TempId,
        src: TempId,
        ty: Type,
    },
}

impl HIRInstr {
    pub fn defines(&self) -> Option<String> {
        match self {
            HIRInstr::StoreVar { var, src, .. } => Some(var.lexeme.clone()), // this is the SSA-def
            _ => None,
        }
    }
    pub fn uses(&self) -> Vec<String> {
        match self {
            HIRInstr::LoadVar { var, .. } => vec![var.lexeme.clone()],
            _ => vec![],
        }
    }
}
pub fn pretty_print_hir_instr(hir_instr: &HIRInstr, indent: usize) -> String {
    let pad = " ".repeat(indent);
    match hir_instr {
        HIRInstr::Move { dest, src, ty } => {
            format!("{pad}{:?} = MOVE {:?} : {:?}", dest, src, ty)
        }
        HIRInstr::Const { dest, value, ty } => {
            format!("{pad}{:?} = CONST {:?} : {:?}", dest, value, ty)
        }
        HIRInstr::UnaryOp { dest, op, src, ty } => {
            format!("{pad}{:?} = {:?} {:?} : {:?}", dest, op, src, ty)
        }
        HIRInstr::BinaryOp {
            dest,
            op,
            lhs,
            rhs,
            ty,
        } => {
            format!("{pad}{:?} = {:?} {:?} {:?} : {:?}", dest, lhs, op, rhs, ty)
        }
        HIRInstr::LoadVar { dest, var, ty } => {
            format!("{pad}{:?} = LOAD {:?} : {:?}", dest, var.lexeme, ty)
        }
        HIRInstr::StoreVar { var, src, ty } => {
            format!("{pad}STORE {:?} = {:?} : {:?}", var.lexeme, src, ty)
        }
        HIRInstr::Call {
            dest,
            func,
            args,
            ty,
        } => {
            let args_str = args
                .iter()
                .map(|a| format!("{:?}", a))
                .collect::<Vec<_>>()
                .join(", ");
            format!(
                "{pad}{:?} = CALL {}({}) : {:?}",
                dest, func.lexeme, args_str, ty
            )
        }
        HIRInstr::Print { src, ty } => {
            format!("{pad}PRINT {:?} : {:?}", src, ty)
        }
        HIRInstr::Return { value, ty } => match value {
            Some(v) => format!("{pad}RETURN {:?} : {:?}", v, ty),
            None => format!("{pad}RETURN void"),
        },
        HIRInstr::If {
            cond,
            then_label,
            else_label,
        } => {
            format!(
                "{pad}IF {:?} THEN {:?} ELSE {:?}",
                cond, then_label.0, else_label.0
            )
        }
        HIRInstr::Goto { target } => {
            format!("{pad}GOTO {:?}", target.0)
        }
        HIRInstr::Label { label } => {
            format!("LABEL {:?}:", label.0)
        }
    }
}

/// Constants
// #[derive(Debug, Clone)]
// pub enum ConstValue {
//     Bool(bool),
//     Int(i64),
//     Float(f64),
//     String(String),
//     Nil,
// }

/// HIR function
#[derive(Debug, Clone)]
pub struct HIRFunction {
    pub name: String,
    pub args: Vec<(Token, Type)>,
    pub body: Vec<HIRInstr>,
    pub ret_type: Type,
}

pub fn pretty_print_hir_function(func: &HIRFunction) -> String {
    let args = func
        .args
        .iter()
        .map(|(tok, ty)| format!("{}: {}", tok.lexeme, pretty_print_type(ty)))
        .collect::<Vec<_>>()
        .join(", ");

    let header = format!(
        "fun {}({}) -> {} {{",
        func.name,
        args,
        pretty_print_type(&func.ret_type)
    );

    let body = func
        .body
        .iter()
        .map(|instr| format!(" {}", pretty_print_hir_instr(instr, 2)))
        .collect::<Vec<_>>()
        .join("\n");

    format!("{}\n{}\n}}\n", header, body)
}

#[derive(Debug, Clone)]
pub struct HIRGlobal {
    pub name: Token,
    pub ty: Type,
    pub initializer: Option<ConstValue>,
}

pub fn pretty_print_hir_global(global: &HIRGlobal) -> String {
    let name = &global.name.lexeme;
    let ty_str = pretty_print_type(&global.ty);
    match &global.initializer {
        Some(lit) => {
            let lit_str = pretty_print_literal(lit);
            format!("var {}: {} = {};", name, ty_str, lit_str)
        }
        None => format!("var {}: {};", name, ty_str),
    }
}
