use std::{default, vec};

use super::hir_error::HIRError;
use crate::{
    hir::hir_error::log_hir_error,
    types::{
        hir_types::*, literal::Literal, op::{BinaryOp, LogicalOp, UnaryOp}, token::Token, token_type::TokenType, typed_expr::{TypedExpr, TypedExprKind}, typed_stmt::TypedStmt, types::Type
    },
};

pub struct HIR {
    pub errors: Vec<HIRError>,
    pub functions: Vec<HIRFunction>,
    pub globals: Vec<HIRGlobal>,
    in_function: bool,
    next_temp: u32,
    next_label: u32,

    end_while_label: Label,
}

impl HIR {
    pub fn new() -> HIR {
        return HIR {
            errors: Vec::new(),
            functions: Vec::new(),
            globals: Vec::new(),
            next_temp: 0,
            next_label: 0,
            in_function: false,
            end_while_label: Label("invalid_label".to_string()),
        };
    }

    // pub fn get

    pub fn lower_typed_asts(&mut self, typed_stmts: &Vec<TypedStmt>) {
        for typed_stmt in typed_stmts {
            self.lower_typed_ast(typed_stmt);
        }

        if !self.errors.is_empty() {
            eprintln!("Type HIR found {} errors.", self.errors.len());
            for err in &self.errors {
                log_hir_error(err); // Or implement Display for nicer messages
            }
        }

        println!("{}", self.pretty_print_program());
    }

    fn lower_typed_ast(&mut self, typed_stmt: &TypedStmt) {
        match self.lower_stmt(&typed_stmt) {
            Ok(instrs) => {
                for i in instrs {
                    let val = pretty_print_hir_instr(&i, 2);
                    println!("{}", val);
                }
            }
            Err(e) => {
                self.errors.push(e);
            }
        };
    }

    fn lower_stmt(&mut self, typed_stmt: &TypedStmt) -> Result<Vec<HIRInstr>, HIRError> {
        match typed_stmt {
            TypedStmt::Block(typed_stmts) => {
                let mut vec: Vec<HIRInstr> = Vec::new();

                for block_typed_stmt in typed_stmts.as_ref() {
                    let val = self.lower_stmt(block_typed_stmt)?;
                    vec.extend(val);
                }
                return Ok(vec);
            }

            TypedStmt::Variable(token, var_type, typed_expr) => {
                if !self.in_function {
                    let const_value = match typed_expr {
                        Some(TypedExpr {
                            kind: TypedExprKind::Literal(lit),
                            ..
                        }) => Some(lit.clone()),
                        _ => {
                            return Err(HIRError::InvalidGlobalStatement {
                                message: "cant have expressions on global variables".to_string(),
                            });
                        } // only allow const exprs in globals
                    };
                    self.globals.push(HIRGlobal {
                        name: token.clone(),
                        ty: var_type.clone(),
                        initializer: const_value,
                    });
                    return Ok(vec![]);
                }

                let mut res: Vec<HIRInstr> = Vec::new();

                if let Some(initializer) = typed_expr {
                    let (expr_val, expr_id) = self.lower_expr(initializer)?;
                    res.extend(expr_val);

                    let i = HIRInstr::StoreVar {
                        var: token.clone(),
                        src: expr_id,
                        ty: var_type.clone(),
                    };
                    res.push(i);
                }
                return Ok(res);
                // todo - blah
            }
            TypedStmt::Print(typed_expr) => {
                let (val, id) = self.lower_expr(typed_expr)?;
                let mut res = val;

                let i = HIRInstr::Print {
                    src: id,
                    ty: typed_expr.ty.clone(),
                };
                res.push(i);
                return Ok(res);
            }
            TypedStmt::Expression(typed_expr) => {
                if !self.in_function {
                    return Err(HIRError::InvalidGlobalStatement {
                        message: "Global expressions and operations are unallowed".to_string(),
                    });
                }
                let (val, id) = self.lower_expr(typed_expr)?;
                return Ok(val);
            }
            TypedStmt::Function(token, items, typed_stmts, return_type) => {
                let temp_in_function = self.in_function;
                self.in_function = true;
                let mut val: Vec<HIRInstr> = Vec::new();
                val.push(HIRInstr::Label {
                    label: self.fresh_label(),
                });
                for i in typed_stmts.as_ref() {
                    val.extend(self.lower_stmt(i)?);
                }

                if let Some(last) = typed_stmts.last() {
                    if !matches!(last, TypedStmt::Return(_, _)) {
                        if (*return_type == Type::Void) {
                            val.push(HIRInstr::Return {
                                value: None,
                                ty: Type::Void,
                            });
                        } else {
                            return Err(HIRError::ExpectedReturn {
                                found: return_type.clone(),
                                name: token.clone(),
                            });
                        }
                    }
                } else if typed_stmts.is_empty() {
                    if (*return_type == Type::Void) {
                        val.push(HIRInstr::Return {
                            value: None,
                            ty: Type::Void,
                        });
                    } else {
                        return Err(HIRError::ExpectedReturn {
                            found: return_type.clone(),
                            name: token.clone(),
                        });
                    }
                }
                let val = HIRFunction {
                    name: token.clone().lexeme,
                    args: items.as_ref().to_vec(),
                    body: val,
                    ret_type: return_type.clone(),
                };

                self.functions.push(val);

                self.in_function = temp_in_function;
                return Ok((vec![]));
                // todo!()
            }
            TypedStmt::If(typed_expr, then_stmt, else_stmt_option) => {
                return self.lower_if(typed_expr, then_stmt, else_stmt_option);
            }
            TypedStmt::While(typed_expr, typed_stmt) => {
                return self.lower_while(typed_expr, typed_stmt);
            }
            TypedStmt::Break(token) => {
                return Ok(vec![HIRInstr::Goto {
                    target: self.end_while_label.clone(),
                }]);
            }
            TypedStmt::Return(token, typed_expr) => {
                let mut res: Vec<HIRInstr> = Vec::new();
                let return_val: HIRInstr;
                if let Some(return_typed_expr) = typed_expr.as_ref() {
                    let (val, id) = self.lower_expr(return_typed_expr)?;
                    res.extend(val);

                    return_val = HIRInstr::Return {
                        value: Some(id),
                        ty: return_typed_expr.ty.clone(),
                    };
                } else {
                    return_val = HIRInstr::Return {
                        value: None,
                        ty: Type::Void,
                    };
                }
                res.push(return_val);

                return Ok(res);
            }
            TypedStmt::Class(token, typed_stmts) => todo!(),
        }
    }

    fn lower_expr(&mut self, typed_expr: &TypedExpr) -> Result<(Vec<HIRInstr>, TempId), HIRError> {
        let kind = &typed_expr.kind;
        // let val = &typed_expr;
        let expr_type = typed_expr.ty.clone();
        match kind {
            TypedExprKind::Literal(lit) => {
                let temp = self.fresh_temp();

                let val = vec![
                    (HIRInstr::Const {
                        dest: temp,
                        value: lit.clone(),
                        ty: expr_type,
                    }),
                ];

                return Ok((val, temp));
                // todo!();
            }
            TypedExprKind::Call(name, token, typed_args) => {
                
                
                // self.type_check_expr
                let mut args: Vec<TempId> = Vec::new();

                let mut res: Vec<HIRInstr> = Vec::new();
                // let (var_val, var_tempid) = self.lower_expr(&typed_expr)?;
                // res.extend(var_val);
                for i in typed_args {
                    let (val, tempid) = self.lower_expr(i)?;
                    args.push(tempid);
                    res.extend(val);
                }

                let call_dest = self.fresh_temp();
                let call = HIRInstr::Call {
                    dest: call_dest,
                    func: name.clone(),
                    args: args,
                    ty: expr_type,
                };
                res.push(call);
                return Ok((res, call_dest));
            }
            TypedExprKind::Logical(lhs, token, rhs) => {
                let mut vec: Vec<HIRInstr> = Vec::new();

                if let Some(logical_op) = map_token_type_to_logicalop(&token.get_token_type()) {
                    let (lhs_instr, lhs_id) = self.lower_expr(lhs)?;
                    vec.extend(lhs_instr);

                    let condition_label = self.fresh_label();
                    let default = self.fresh_label();

                    let lhs_if_instr = HIRInstr::If {
                        cond: lhs_id,
                        then_label: match logical_op {
                            LogicalOp::And => condition_label.clone(),
                            LogicalOp::Or => default.clone(),
                        },
                        else_label: match logical_op {
                            LogicalOp::And => default.clone(),
                            LogicalOp::Or => condition_label.clone(),
                        },
                    };
                    vec.push(lhs_if_instr);

                    let condition_label_instr = HIRInstr::Label {
                        label: condition_label,
                    };
                    vec.push(condition_label_instr);

                    let (rhs_instr, rhs_id) = self.lower_expr(rhs)?;
                    vec.extend(rhs_instr);

                    let result_temp = self.fresh_temp();
                    let move_instr = HIRInstr::Move {
                        dest: result_temp,
                        src: rhs_id,
                        ty: rhs.ty.clone(),
                    };
                    vec.push(move_instr);

                    let end_label = self.fresh_label();
                    let goto_end = HIRInstr::Goto {
                        target: end_label.clone(),
                    };
                    vec.push(goto_end);

                    let default_label_instr = HIRInstr::Label { label: default };
                    vec.push(default_label_instr);

                    // this is different in and/or
                    // let short_temp = self.fresh_temp();
                    let default_instr = HIRInstr::Const {
                        dest: result_temp,
                        value: match logical_op {
                            LogicalOp::And => Literal::Bool(false),
                            LogicalOp::Or => Literal::Bool(true),
                        },
                        ty: Type::Bool,
                    };
                    vec.push(default_instr);

                    let end_label_instr = HIRInstr::Label { label: end_label };
                    vec.push(end_label_instr);

                    return (Ok((vec, result_temp)));
                } else {
                    return Err(HIRError::InternalError {
                        message: "a".to_string(),
                    });
                }
            }
            TypedExprKind::Set(typed_expr, token, typed_expr1) => todo!(),
            TypedExprKind::Get(token, typed_expr) => todo!(),
            TypedExprKind::Variable(token) => {
                let val = self.fresh_temp();
                let x = HIRInstr::LoadVar {
                    dest: val,
                    var: token.clone(),
                    ty: expr_type,
                };
                return Ok((vec![x], val));
            }
            TypedExprKind::Unary(token, typed_expr) => {
                let mut res: Vec<HIRInstr> = Vec::new();
                let new_unary_id = self.fresh_temp();
                let (unary_val, expr_id) = self.lower_expr(typed_expr)?;
                if let Some(unary_op) = map_token_type_to_unaryop(&token.get_token_type()) {
                    let val = HIRInstr::UnaryOp {
                        dest: new_unary_id,
                        op: unary_op,
                        src: expr_id,
                        ty: expr_type,
                    };
                    res.extend(unary_val);
                    res.push(val);
                    // unary_val.push(val);
                    return Ok((res, new_unary_id));
                } else {
                    return Err(HIRError::InternalError {
                        message: "a".to_string(),
                    });
                }
            }
            TypedExprKind::Binary(lhs, token, rhs) => {
                let mut vec: Vec<HIRInstr> = Vec::new();
                let (lhs_instr, lhs_id) = self.lower_expr(lhs)?;
                let (rhs_instr, rhs_id) = self.lower_expr(rhs)?;

                let binary_id = self.fresh_temp();

                if let Some(binop) = map_token_type_to_binop(&token.get_token_type()) {
                    vec.extend(lhs_instr);
                    vec.extend(rhs_instr);
                    let binaryop = HIRInstr::BinaryOp {
                        dest: binary_id,
                        op: binop,
                        lhs: lhs_id,
                        rhs: rhs_id,
                        ty: expr_type,
                    };
                    vec.push(binaryop);
                } else {
                    return Err(HIRError::InternalError {
                        message: "a".to_string(),
                    });
                }

                return Ok((vec, binary_id));
            }
            TypedExprKind::Grouping(typed_expr) => {
                return Ok(self.lower_expr(typed_expr.as_ref())?);
            }
            TypedExprKind::Assignment(var, rhs) => {
                let mut result: Vec<HIRInstr> = vec![];
                let result_temp = self.fresh_temp();
                let (rhs_val, rhs_id) = self.lower_expr(rhs)?;
                let store_val = HIRInstr::StoreVar {
                    var: var.clone(),
                    src: rhs_id,
                    ty: expr_type.clone(),
                };

                let move_val = HIRInstr::Move {
                    dest: result_temp,
                    src: rhs_id,
                    ty: expr_type,
                };
                result.extend(rhs_val);
                result.push(store_val);
                result.push(move_val);
                return Ok((result, rhs_id));
            }
        }
    }

    fn lower_if(
        &mut self,
        expr_condition: &Box<TypedExpr>,
        then_statement: &Box<TypedStmt>,
        else_statement_option: &Box<Option<TypedStmt>>,
    ) -> Result<Vec<HIRInstr>, HIRError> {
        let mut res: Vec<HIRInstr> = Vec::new();

        let (cond, condition_id) = self.lower_expr(expr_condition)?;

        let then_label = self.fresh_label();

        let then_label_instr = HIRInstr::Label {
            label: then_label.clone(),
        };

        let then_stmt = self.lower_stmt(then_statement)?;

        if let Some(else_stmt) = else_statement_option.as_ref() {
            let else_instrs = self.lower_stmt(else_stmt)?;
            let else_label = self.fresh_label();
            let else_label_instr = HIRInstr::Label {
                label: else_label.clone(),
            };

            let cond_instr = HIRInstr::If {
                cond: condition_id,
                then_label: then_label.clone(),
                else_label: else_label.clone(),
            };
            let end_label = self.fresh_label();

            let end_label_instr = HIRInstr::Label {
                label: end_label.clone(),
            };
            let goto_end = HIRInstr::Goto { target: end_label };

            res.extend(cond);
            res.push(cond_instr);
            res.push(then_label_instr);
            res.extend(then_stmt);
            res.push(goto_end);
            res.push(else_label_instr);
            res.extend(else_instrs);
            res.push(end_label_instr);
        } else {
            let end_label = self.fresh_label();

            let end_label_instr = HIRInstr::Label {
                label: end_label.clone(),
            };
            let cond_instr = HIRInstr::If {
                cond: condition_id,
                then_label: then_label.clone(),
                else_label: end_label.clone(),
            };
            res.extend(cond);
            res.push(cond_instr);
            res.push(then_label_instr);
            res.extend(then_stmt);
            res.push(end_label_instr);
        }

        return Ok(res);
    }
    fn lower_while(
        &mut self,
        expr_condition: &Box<TypedExpr>,
        statement: &Box<TypedStmt>,
    ) -> Result<Vec<HIRInstr>, HIRError> {
        let mut res: Vec<HIRInstr> = Vec::new();

        let start_label = self.fresh_label();
        let start_label_instr = HIRInstr::Label {
            label: start_label.clone(),
        };
        let continue_label = self.fresh_label();
        let continue_label_instr = HIRInstr::Label {
            label: continue_label.clone(),
        };
        let end_label = self.fresh_label();
        let end_label_instr = HIRInstr::Label {
            label: end_label.clone(),
        };

        let prev_end_while_label = self.end_while_label.clone();
        self.end_while_label = end_label.clone();

        let (condition_expr, condition_id) = self.lower_expr(expr_condition)?;

        let condition_instr = HIRInstr::If {
            cond: condition_id,
            then_label: continue_label,
            else_label: end_label,
        };

        let body_stmts = self.lower_stmt(statement)?;

        let goto_instr = HIRInstr::Goto {
            target: start_label,
        };

        res.push(start_label_instr);
        res.extend(condition_expr);
        res.push(condition_instr);
        res.push(continue_label_instr);
        res.extend(body_stmts);
        res.push(goto_instr);
        res.push(end_label_instr);

        self.end_while_label = prev_end_while_label;

        return Ok(res);
    }

    fn fresh_temp(&mut self) -> TempId {
        let id = self.next_temp;
        self.next_temp += 1;
        TempId(id)
    }
    fn fresh_temps(&mut self, amount: usize) -> Vec<TempId> {
        let mut res: Vec<TempId> = Vec::new();
        for i in 0..amount {
            let id = self.next_temp;
            self.next_temp += 1;
            res.push(TempId(id));
        }
        return res;
    }

    fn fresh_label(&mut self) -> Label {
        let id = self.next_label;
        self.next_label += 1;
        Label(format!("L{}", id))
    }

    fn fresh_labels(&mut self, amount: usize) -> Vec<Label> {
        let mut res: Vec<Label> = Vec::new();
        for i in 0..amount {
            let id = self.next_label;

            self.next_label += 1;
            res.push(Label(format!("L{}", id)));
        }
        return res;
    }

    pub fn pretty_print_program(&self) -> String {
        let mut result = String::new();

        for HIRGlobal {
            name,
            ty,
            initializer,
        } in &self.globals
        {
            match initializer {
                Some(v) => {
                    result.push_str(&format!("global {}: {:?} = {:?}\n", name.lexeme, ty, v));
                }
                None => {
                    result.push_str(&format!("global {}: {:?}\n", name.lexeme, ty));
                }
            }
        }

        for func in &self.functions {
            result.push_str(pretty_print_hir_function(func).as_str());
        }

        result
    }
}

fn map_token_type_to_binop(token: &TokenType) -> Option<BinaryOp> {
    match token {
        TokenType::Plus => Some(BinaryOp::Add),
        TokenType::Minus => Some(BinaryOp::Sub),
        TokenType::Star => Some(BinaryOp::Mul),
        TokenType::Slash => Some(BinaryOp::Div),
        // TokenType::And => Some(BinaryOp::And),
        // TokenType::Or => Some(BinaryOp::Or),
        TokenType::EqualEqual => Some(BinaryOp::Equal),
        TokenType::BangEqual => Some(BinaryOp::NotEqual),
        TokenType::Less => Some(BinaryOp::Less),
        TokenType::LessEqual => Some(BinaryOp::LessEqual),
        TokenType::Greater => Some(BinaryOp::Greater),
        TokenType::GreaterEqual => Some(BinaryOp::GreaterEqual),
        _ => None,
    }
}

fn map_token_type_to_logicalop(token: &TokenType) -> Option<LogicalOp> {
    match token {
        TokenType::And => Some(LogicalOp::And),
        TokenType::Or => Some(LogicalOp::Or),

        _ => None,
    }
}

fn map_token_type_to_unaryop(token: &TokenType) -> Option<UnaryOp> {
    match token {
        TokenType::Bang => Some(UnaryOp::Not),
        TokenType::Minus => Some(UnaryOp::Neg),

        _ => None,
    }
}
