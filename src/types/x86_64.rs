#[derive(Debug, Clone)]
pub enum X86Instr {
    Mov { dst: Operand, src: Operand },
    Add { dst: Operand, src: Operand },
    Sub { dst: Operand, src: Operand },
    Mul { dst: Operand },
    Div { divisor: Operand },
    Cmp { lhs: Operand, rhs: Operand },
    Jmp { target: Operand }, // usually label
    Jcc { cond: Condition, target: Operand },
    Call { target: Operand },
    Ret,
    Label { name: String },

    Push { src: Operand },
    Pop { dst: Operand },

    Set { cond: Condition, dst: RegName }, //setcc, !, comparisons 0/1
    MovZX { dst: Register, src: RegName }, //To zero-extend a byte (al) into full register
    Test { lhs: Operand, rhs: Operand },   //	Bitwise test for zero (alternative to cmp x, 0)
    Shl { dst: Operand, src: Operand },
    Shr { dst: Operand, src: Operand },
    Sar { dst: Operand, src: Operand },
    Neg { dst: Operand },
    Inc { dst: Operand },
    Dec { dst: Operand },

    And { dst: Operand, src: Operand },
    Or { dst: Operand, src: Operand },
    Xor { dst: Operand, src: Operand },
    Not { dst: Operand },
    // Lea { dst, addr: MemoryOperand }},
}

impl X86Instr {}

struct AllocationContext {
    vreg_to_phys: HashMap<VirtualReg, RegName>,
    // vreg_to_stack: HashMap<VirtualReg, StackSlot>, // for spills
}

#[derive(Debug, Clone)]
pub struct SpillSlot {
    pub offset: i32,
    pub base: RegName,
    pub size: RegSize,
}

#[derive(Debug, Clone)]
pub enum Condition {
    Eq,
    Ne,
    Gt,
    Ge,
    Lt,
    Le, // etc.
}
#[derive(Debug, Clone)]

pub enum Operand {
    Reg(Register),
    Imm(i64),
    Mem(MemoryOperand),
    Label(String),
}

#[derive(Debug, Clone)]

pub enum Register {
    Virtual(VirtualReg),
    Physical(TypedRegister), // concrete register enum
}
#[derive(Debug, Clone)]
pub struct TypedRegister {
    pub name: RegName,
    pub size: RegSize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RegSize {
    Byte,  // 8-bit (e.g., al)
    Word,  // 16-bit (e.g., ax)
    DWord, // 32-bit (e.g., eax)
    QWord, // 64-bit (e.g., rax)
}

pub const CALLEE_SAVED: &[RegName] = &[
    RegName::RBX,
    RegName::R12,
    RegName::R13,
    RegName::R14,
    RegName::R15,
];

pub const ALLOCATABLE: &[RegName] = &[
    RegName::RAX,
    RegName::RBX,
    RegName::RCX,
    RegName::RDX,
    RegName::RSI,
    RegName::RDI,
    RegName::R8,
    RegName::R9,
    RegName::R10,
    RegName::R11,
    RegName::R12,
    RegName::R13,
    RegName::R14,
    RegName::R15,
];

#[derive(Debug, Clone)]

pub enum RegName {
    RAX,
    RBX,
    RCX,
    RDX,
    RSI,
    RDI,
    RBP,
    RSP,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

#[derive(Debug, Clone)]
pub struct MemoryOperand {
    base: Option<Register>,
    index: Option<Register>,
    scale: u8,   // usually 1, 2, 4, or 8
    offset: i32, // displacement
}

use std::{
    collections::HashMap,
    fmt::{self, write},
};

use crate::types::lir_types::VirtualReg;

impl fmt::Display for X86Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use X86Instr::*;
        match self {
            Mov { dst, src } => write!(f, "  mov {}, {}", dst, src),
            Add { dst, src } => write!(f, "  add {}, {}", dst, src),
            Sub { dst, src } => write!(f, "  sub {}, {}", dst, src),
            Mul { dst } => write!(f, "  mul {}", dst),
            Div { divisor } => write!(f, "  div {}", divisor),
            Cmp { lhs, rhs } => write!(f, "  cmp {}, {}", lhs, rhs),
            Jmp { target } => write!(f, "  jmp {}", target),
            Jcc { cond, target } => write!(f, "  j{} {}", cond, target),
            Call { target } => write!(f, "  call {}", target),
            Ret => write!(f, "  ret"),
            Label { name } => write!(f, "{}:", name),
            Push { src } => write!(f, "push {}", src),
            Pop { dst } => write!(f, "pop {}", dst),
            Set { cond, dst } => write!(f, "set {}: {}", dst, cond),
            MovZX { dst, src } => todo!(),
            Test { lhs, rhs } => todo!(),
            Shl { dst, src } => todo!(),
            Shr { dst, src } => todo!(),
            Sar { dst, src } => todo!(),
            Neg { dst } => todo!(),
            Inc { dst } => todo!(),
            Dec { dst } => todo!(),
            And { dst, src } => todo!(),
            Or { dst, src } => todo!(),
            Xor { dst, src } => todo!(),
            Not { dst } => todo!(),
        }
    }
}

impl fmt::Display for Condition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Condition::*;
        let cond_str = match self {
            Eq => "e",
            Ne => "ne",
            Gt => "g",
            Ge => "ge",
            Lt => "l",
            Le => "le",
        };
        write!(f, "{}", cond_str)
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Operand::*;
        match self {
            Reg(r) => write!(f, "{}", r),
            Imm(i) => write!(f, "{}", i),
            Label(s) => write!(f, "{}", s),
            Mem(mem) => write!(f, "{}", mem),
        }
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Register::Virtual(id) => write!(f, "v{}", id),
            Register::Physical(reg) => write!(f, "{}", reg),
        }
    }
}

impl fmt::Display for TypedRegister {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.size)
    }
}

impl fmt::Display for RegSize {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RegSize::Byte => write!(f, "byte"),

            RegSize::Word => write!(f, "word"),
            RegSize::DWord => write!(f, "dword"),
            RegSize::QWord => write!(f, "qword"),
        }
    }
}

impl fmt::Display for RegName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use RegName::*;
        let name = match self {
            RAX => "rax",
            RBX => "rbx",
            RCX => "rcx",
            RDX => "rdx",
            RSI => "rsi",
            RDI => "rdi",
            RBP => "rbp",
            RSP => "rsp",
            R8 => "r8",
            R9 => "r9",
            R10 => "r10",
            R11 => "r11",
            R12 => "r12",
            R13 => "r13",
            R14 => "r14",
            R15 => "r15",
        };
        write!(f, "{}", name)
    }
}

impl fmt::Display for MemoryOperand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut parts = vec![];

        if let Some(base) = &self.base {
            parts.push(format!("{}", base));
        }

        if let Some(index) = &self.index {
            parts.push(format!("{}*{}", index, self.scale));
        }

        let addr = if parts.is_empty() {
            format!("{}", self.offset)
        } else {
            format!(
                "{}{}",
                parts.join("+"),
                if self.offset != 0 {
                    format!("{:+}", self.offset)
                } else {
                    "".to_string()
                }
            )
        };

        write!(f, "[{}]", addr)
    }
}
