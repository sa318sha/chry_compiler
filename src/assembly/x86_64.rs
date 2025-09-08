// use crate::{
//     lir::{self, lir::LIRProgram, live_range_analyis::LiveRange},
//     types::{
//         lir_types::{LIRBasicBlock, LIRExpr, LIRInstr, LIRTerminator, VirtualReg},
//         literal::Literal,
//         op::{BinaryOp, UnaryOp},
//         x86_64::{Condition, Operand, RegName, RegSize, Register, TypedRegister, X86Instr},
//     },
// };
// use itertools::Itertools;
// use std::collections::HashMap;




// pub fn register_allocation(lir_programs: Vec<LIRProgram>, live_ranges: &Vec<LiveRange>) {
//     let mut active_registers = Vec::new();
//     let mut assignments: HashMap<VirtualReg, TypedRegister> = HashMap::new();
//     for range in live_ranges.iter().sorted_by(|a, b| a.start.cmp(&b.start)) {

//         // use sorted range
//     }
// }

// pub fn lower_to_x86_64(lir_programs: Vec<LIRProgram>, live_ranges: Vec<LiveRange>) {
//     for lir_program in &lir_programs {
//         let func: X86_64Function = X86_64Lowering::to_virtual(lir_program);
//         // func.live_range_analysis();
//         // funcs.allocate_register
//     }
// }

// pub struct X86_64Function {
//     pub instrs: Vec<X86Instr>,
//     // pub live_range: HashMap<VirtualReg, (usize, usize)>,
// }

// impl X86_64Function {
//     pub fn allocate_registers() {}
// }

// pub struct X86_64Lowering {}

// impl X86_64Lowering {
//     pub fn to_virtual(lir_program: &LIRProgram) -> X86_64Function {
//         let mut res: Vec<X86Instr> = Vec::new();
//         for label in &lir_program.block_order {
//             // let lir_block
//             if let Some(lir_block) = lir_program.blocks.get(&label) {
//                 res.extend(X86_64Lowering::lower_block(lir_block));

//                 for i in &res {
//                     println!("{}", i)
//                 }
//             } else {
//                 panic!()
//             }
//         }

//         return X86_64Function { instrs: res };
//     }

//     pub fn lower_block(lir_block: &LIRBasicBlock) -> Vec<X86Instr> {
//         let mut vec: Vec<X86Instr> = Vec::new();
//         vec.push(X86Instr::Label {
//             name: lir_block.label.0.clone(),
//         });
//         for instr in &lir_block.instrs {
//             vec.extend(X86_64Lowering::lower_initial_instr(&instr));
//         }

//         vec.extend(X86_64Lowering::lower_terminator(&lir_block.terminator));

//         return vec;
//     }

//     pub fn lower_terminator(terminator: &LIRTerminator) -> Vec<X86Instr> {
//         match terminator {
//             LIRTerminator::Jump(label) => {
//                 return vec![X86Instr::Jmp {
//                     target: Operand::Label(label.0.clone()),
//                 }];
//             }
//             LIRTerminator::Branch {
//                 cond,
//                 then_label,
//                 else_label,
//             } => {
//                 return vec![
//                     X86Instr::Cmp {
//                         lhs: Operand::Reg(Register::Virtual(cond.0)),
//                         rhs: Operand::Imm(0),
//                     },
//                     X86Instr::Jcc {
//                         cond: Condition::Ne,
//                         target: Operand::Label(then_label.0.clone()),
//                     },
//                     X86Instr::Jmp {
//                         target: Operand::Label(else_label.0.clone()),
//                     },
//                 ];
//             }
//             LIRTerminator::Return(virtual_reg) => match virtual_reg {
//                 Some(x) => {
//                     return vec![
//                         X86Instr::Mov {
//                             dst: Operand::Reg(Register::Physical(TypedRegister {
//                                 name: RegName::RAX,
//                                 size: RegSize::Word,
//                             })),
//                             src: Operand::Reg(Register::Virtual(x.0)),
//                         },
//                         X86Instr::Ret,
//                     ];
//                 }
//                 None => {
//                     return vec![X86Instr::Ret];
//                 }
//             },
//         }
//     }

//     pub fn lower_initial_instr(instr: &LIRInstr) -> Vec<X86Instr> {
//         match instr {
//             LIRInstr::Assign { dest, expr, .. } => {
//                 let dst = Operand::Reg(Register::Virtual(dest.0));
//                 match expr {
//                     LIRExpr::Const(lit) => {
//                         // mov dest, imm
//                         let imm = match lit {
//                             Literal::Int(x) => Operand::Imm(*x as i64),
//                             Literal::Bool(b) => Operand::Imm(if *b { 1 } else { 0 }),
//                             _ => unimplemented!("Other literal types"),
//                         };
//                         vec![X86Instr::Mov { dst, src: imm }]
//                     }

//                     LIRExpr::Var(src_reg) => {
//                         let src = Operand::Reg(Register::Virtual(src_reg.0));
//                         vec![X86Instr::Mov { dst, src }]
//                     }

//                     LIRExpr::Unary { op, val } => {
//                         let val_op = Operand::Reg(Register::Virtual(val.0));
//                         match op {
//                             UnaryOp::Neg => vec![
//                                 X86Instr::Mov {
//                                     dst: dst.clone(),
//                                     src: val_op,
//                                 },
//                                 X86Instr::Sub {
//                                     dst: dst.clone(),
//                                     src: Operand::Imm(0),
//                                 },
//                             ],
//                             UnaryOp::Not => vec![
//                                 X86Instr::Mov {
//                                     dst: dst.clone(),
//                                     src: val_op,
//                                 },
//                                 X86Instr::Cmp {
//                                     lhs: dst.clone(),
//                                     rhs: Operand::Imm(0),
//                                 },
//                                 X86Instr::Mov {
//                                     dst: dst.clone(),
//                                     src: Operand::Imm(0),
//                                 },
//                                 X86Instr::Jcc {
//                                     cond: Condition::Eq,
//                                     target: Operand::Label("set1".to_string()), // needs patching/fixup
//                                 },
//                             ],
//                             _ => unimplemented!("Other unary ops"),
//                         }
//                     }

//                     LIRExpr::Binary { op, lhs, rhs } => {
//                         let lhs_op = Operand::Reg(Register::Virtual(lhs.0));
//                         let rhs_op = Operand::Reg(Register::Virtual(rhs.0));
//                         match op {
//                             BinaryOp::Add => vec![
//                                 X86Instr::Mov {
//                                     dst: dst.clone(),
//                                     src: lhs_op,
//                                 },
//                                 X86Instr::Add {
//                                     dst: dst.clone(),
//                                     src: rhs_op,
//                                 },
//                             ],
//                             BinaryOp::Sub => vec![
//                                 X86Instr::Mov {
//                                     dst: dst.clone(),
//                                     src: lhs_op,
//                                 },
//                                 X86Instr::Sub {
//                                     dst: dst.clone(),
//                                     src: rhs_op,
//                                 },
//                             ],
//                             BinaryOp::Mul => vec![
//                                 X86Instr::Mov {
//                                     dst: dst.clone(),
//                                     src: lhs_op,
//                                 },
//                                 X86Instr::Mul { dst: rhs_op },
//                                 // result goes to RAX → move back needed
//                             ],
//                             BinaryOp::Div => vec![
//                                 X86Instr::Mov {
//                                     dst: Operand::Reg(Register::Physical(TypedRegister {
//                                         name: RegName::RAX,
//                                         size: RegSize::QWord,
//                                     })),
//                                     src: lhs_op,
//                                 },
//                                 X86Instr::Mov {
//                                     dst: Operand::Reg(Register::Physical(TypedRegister {
//                                         name: RegName::RDX,
//                                         size: RegSize::QWord,
//                                     })),
//                                     src: Operand::Reg(Register::Physical(TypedRegister {
//                                         name: RegName::RDX,
//                                         size: RegSize::QWord,
//                                     })),
//                                 },
//                                 X86Instr::Div { divisor: rhs_op },
//                                 X86Instr::Mov {
//                                     dst,
//                                     src: Operand::Reg(Register::Physical(TypedRegister {
//                                         name: RegName::RDX,
//                                         size: RegSize::QWord,
//                                     })),
//                                 },
//                             ],
//                             _ => unimplemented!("Other binary ops"),
//                         }
//                     }
//                 }
//             }

//             LIRInstr::Move { dest, src } => {
//                 vec![X86Instr::Mov {
//                     dst: Operand::Reg(Register::Virtual(dest.0)),
//                     src: Operand::Reg(Register::Virtual(src.0)),
//                 }]
//             }

//             LIRInstr::Call { .. } => {
//                 // Complex: need calling convention
//                 unimplemented!("Call lowering not implemented")
//             }

//             LIRInstr::Nop => vec![], // no-op
//         }
//     }

//     pub fn lower_lir_expr(lir_expr: LIRExpr) {}
// }
