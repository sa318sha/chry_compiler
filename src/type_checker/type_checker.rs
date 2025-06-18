use crate::types::expr::Expr;

use super::symbol_table::SymbolTable;
use crate::types::stmt::Stmt;
use crate::types::typed_expr::{TypedExpr, TypedExprKind};
use crate::types::typed_stmt::TypedStmt;
use crate::types::{token::Token, token_type::TokenType, types::Type};

type TypeResult<T> = Result<T, TypeError>;

pub struct TypeChecker {
    stack: SymbolTable,
    loop_depth: usize,
    typed_stmts: Vec<TypedStmt>,
    current_return_type: Option<Type>,
    pub errors: Vec<TypeError>,
}

impl TypeChecker {
    pub fn new() -> Self {
        TypeChecker {
            stack: SymbolTable::new(),
            loop_depth: 0,
            typed_stmts: Vec::new(),
            current_return_type: None,
            errors: Vec::new(),
        }
    }

    pub fn check(&mut self, stmts: &Vec<Stmt>) {
        for stmt in stmts {
            if let Some(typed) = self.check_single_stmt(stmt) {
                self.typed_stmts.push(typed);
            }
        }

        if !self.errors.is_empty() {
            eprintln!("Type checking found {} errors.", self.errors.len());
            for err in &self.errors {
                self.error_report(err); // Or implement Display for nicer messages
            }
        }
        //     let x = self.check_single_stmt(stmt);
        // }
    }

    pub fn check_single_stmt(&mut self, stmt: &Stmt) -> Option<TypedStmt> {
        match self.type_check_stmt(stmt) {
            Ok(typed_stmt) => Some(typed_stmt),
            Err(err) => {
                self.errors.push(err);
                None // Signal failure but keep going
            }
        }
    }

    pub fn debug_typedstmts(&self) {
        for typed_stmt in &self.typed_stmts {
            let x = self.pretty_print_typed_stmts(&typed_stmt, 0);
        }
    }

    fn check_block(&mut self, stmts: &Vec<Stmt>) -> TypeResult<Vec<TypedStmt>> {
        let mut statements: Vec<TypedStmt> = Vec::new();

        for stmt in stmts {
            statements.push(self.type_check_stmt(stmt)?);
        }

        return Ok(statements);
        // !todo!();
    }

    fn type_check_stmt(&mut self, stmt: &Stmt) -> TypeResult<TypedStmt> {
        match stmt {
            Stmt::Block(stmts) => {
                let mut statements: Vec<TypedStmt> = Vec::new();
                self.stack.enter_scope();
                for stmt in stmts.as_ref() {
                    statements.push(self.type_check_stmt(stmt)?);
                }
                self.stack.exit_scope();
                return Ok(TypedStmt::Block(Box::new(statements)));
            }
            Stmt::Variable(name, var_type, initializer) => {
                let mut init: Option<TypedExpr> = None;

                if let Some(i) = initializer {
                    //has some initializer
                    // let x = 5; <- 5 is initializer
                    let typed_initializer = self.type_check_expr(i)?;

                    // has some expected type
                    // let x: int = 5 <- "int" is the expected type
                    if let Some(expected_ty) = var_type {
                        // let x: int = "hello world" <- invalid because int != String
                        if expected_ty != &typed_initializer.ty {
                            return Err(TypeError::Mismatch {
                                expected: expected_ty.clone(),
                                found: typed_initializer.ty,
                                context: format!(
                                    "Type of '{}' does not match declared type",
                                    name.lexeme
                                ),
                            });
                        }
                    }

                    //declaring into symboltable of variables
                    // we know it has a value though since it has an initializer
                    let val = self
                        .stack
                        .declare_and_define(&name.lexeme, typed_initializer.ty.clone());
                    if let Err(e) = val {
                        return Err(TypeError::Redeclaration(
                            name.clone(),
                            "redecleration of variable".to_string(),
                        ));
                    }
                    init = Some(typed_initializer);
                } else {
                    // no initializer
                    // may have a optional declared type
                    let val;
                    if let Some(var) = var_type {
                        val = self.stack.define(&name.lexeme.to_string(), var.clone());
                    } else {
                        val = self.stack.declare(name.lexeme.as_str());
                    }
                    if let Err(error) = val {
                        return Err(TypeError::Redeclaration(
                            name.clone(),
                            "redecleration of variable".to_string(),
                        ));
                    }
                }
                return Ok(TypedStmt::Variable(name.clone(), var_type.clone(), init));
            }

            Stmt::Print(val) => {
                let res = self.type_check_expr(val.as_ref())?;
                return Ok(TypedStmt::Print(Box::new(res)));
            }

            Stmt::Expression(expr) => {
                let res = self.type_check_expr(expr.as_ref())?;
                return Ok(TypedStmt::Expression(Box::new(res)));
            }

            Stmt::Function(name, params, block, return_type) => {
                let mut type_array: Vec<Type> = Vec::new();

                for param in params.as_ref() {
                    type_array.push(param.1.clone());
                }

                let val = self.stack.declare_and_define(
                    name.lexeme.as_str(),
                    Type::Function(
                        name.lexeme.clone(),
                        type_array,
                        Box::new(return_type.clone()),
                    ),
                );

                if let Err(e) = val {
                    return Err(TypeError::Redeclaration(
                        name.clone(),
                        "redecleration of variable".to_string(),
                    ));
                }

                let previous_return_type: Option<Type> = self.current_return_type.clone();
                self.current_return_type = Some(return_type.clone());
                let saved_loop_depth = self.loop_depth; // save outer loop depth
                self.loop_depth = 0; // reset for function

                self.stack.enter_scope();

                for param in params.as_ref() {
                    let val = self
                        .stack
                        .declare_and_define(param.0.lexeme.as_str(), param.1.clone());
                    if let Err(e) = val {
                        return Err(TypeError::Redeclaration(
                            name.clone(),
                            "redecleration of variable".to_string(),
                        ));
                    }
                }

                let val = self.check_block(&block.as_ref())?;

                self.stack.exit_scope();
                self.loop_depth = saved_loop_depth; // restore when done
                self.current_return_type = previous_return_type;

                return Ok(TypedStmt::Function(
                    name.clone(),
                    params.clone(),
                    Box::new(val),
                    return_type.clone(),
                ));
            }

            Stmt::If(expr, then, else_stmt) => {
                self.stack.enter_scope();
                let if_expr = self.type_check_expr(expr.as_ref())?;
                let then_expr = self.type_check_stmt(then.as_ref())?;
                let mut else_typed = None;

                if let Some(else_expr) = else_stmt.as_ref() {
                    else_typed = Some(self.type_check_stmt(else_expr)?);
                }
                self.stack.exit_scope();

                return Ok(TypedStmt::If(
                    Box::new(if_expr),
                    Box::new(then_expr),
                    Box::new(else_typed),
                ));
            }
            Stmt::While(expr, stmt) => {
                self.stack.enter_scope();
                self.loop_depth += 1; // entering a loop
                let while_expr = self.type_check_expr(expr.as_ref())?;
                if while_expr.ty != Type::Bool {
                    return Err(TypeError::Mismatch {
                        expected: while_expr.ty,
                        found: Type::Bool,
                        context: "while condition doesnt evaluate to a boolean".to_string(),
                    });
                }
                let stmt = self.type_check_stmt(stmt.as_ref())?;
                self.loop_depth -= 1; // leaving a loop
                self.stack.exit_scope();

                return Ok(TypedStmt::While(Box::new(while_expr), Box::new(stmt)));
            }
            // For(Box<Stmt>, Box<Expr>, Box<Expr>),
            Stmt::Break(token) => {
                if self.loop_depth == 0 {
                    return Err(TypeError::BreakOutsideLoop(token.clone()));
                }
                Ok(TypedStmt::Break(token.clone()))
            }
            Stmt::Return(token, expr) => {
                // let while_expr = self.type_check_expr(expr.as_ref())?;
                let mut expr_typed = None;
                if let Some(val) = expr.as_ref() {
                    expr_typed = Some(self.type_check_expr(val)?);
                }

                match &self.current_return_type {
                    Some(t) => {
                        if let Some(expr) = expr_typed.clone() {
                            if &expr.ty == t {
                                return Ok(TypedStmt::Return(token.clone(), Box::new(expr_typed)));
                            } else {
                                return Err(TypeError::Mismatch{
                                    expected: expr.ty,
                                    found: t.clone(),
                                    context: "return condition doesnt evaluate to the same type of function signature".to_string(),
                            });
                            }
                        } else {
                            return Err(TypeError::Mismatch{
                                    expected: Type::Void,
                                    found: t.clone(),
                                    context: "return condition doesnt evaluate to the same type of function signature".to_string(),
                        });
                        }
                    }
                    None => {
                        if let Some(expr) = expr_typed {
                            return Err(TypeError::Mismatch{
                                    expected: expr.ty,
                                    found: Type::Void,
                                    context: "return condition doesnt evaluate to the same type of function signature".to_string(),
                        });
                        } else {
                            return Ok(TypedStmt::Return(token.clone(), Box::new(expr_typed)));
                        }
                    }
                }
            }
            Stmt::Class(token, stmts) => {
                todo!("class statement type checking not implemented")
            } // Stmt::Empty => {}
              // Stmt::Error => {
              //     return Err(TypeError::DefaultStringError("Error Statement".to_string()));
              // } // _ => todo!(), // Stmt::Print(expr) => {}
        }
    }

    pub fn type_check_expr(&self, expr: &Expr) -> TypeResult<TypedExpr> {
        match expr {
            Expr::Literal(value) => {
                let ty = value.get_type();
                Ok(TypedExpr {
                    kind: TypedExprKind::Literal(ty.clone()),
                    ty,
                })
            }
            Expr::Variable(name) => {
                let ty = self.stack.lookup(&name.lexeme);
                if let Some(var_type) = ty {
                    return Ok(TypedExpr {
                        kind: TypedExprKind::Variable(name.clone()),
                        ty: var_type.clone(),
                    });
                } else {
                    return Err(TypeError::UndeclaredVariable(
                        name.clone(),
                        "variable {} not defined".to_string(),
                    ));
                }
            }
            Expr::Call(callee, _paren, args) => {
                // First type check the callee expression (can be variable, or result of an expression returning function type)
                let callee_typed = self.type_check_expr(callee)?;

                // Ensure callee is of function type
                match &callee_typed.ty {
                    Type::Function(_name, param_types, ret_type) => {
                        // Check arity
                        if param_types.len() != args.len() {
                            return Err(TypeError::ArityMismatch {
                                expected: param_types.len(),
                                found: args.len(),
                                function: _name.to_string(),
                            });
                        }

                        // Check each argument
                        let mut typed_args = Vec::new();
                        for (arg_expr, expected_type) in args.iter().zip(param_types) {
                            let typed_arg_expr = self.type_check_expr(arg_expr)?;
                            if &typed_arg_expr.ty != expected_type {
                                return Err(TypeError::Mismatch {
                                    found: typed_arg_expr.ty.clone(),
                                    expected: expected_type.clone(),
                                    context: "Function argument type mismatch".to_string(),
                                });
                            }
                            typed_args.push(typed_arg_expr);
                        }

                        Ok(TypedExpr {
                            kind: TypedExprKind::Call(
                                Box::new(callee_typed.clone()),
                                _paren.clone(),
                                typed_args,
                            ),
                            ty: ret_type.as_ref().clone(),
                        })
                    }
                    other_type => Err(TypeError::NotCallable {
                        found: other_type.clone(),
                        location: _paren.clone(),
                    }),
                }
            }

            Expr::Binary(left, op, right) => {
                let left_typed = self.type_check_expr(left)?;
                let right_typed = self.type_check_expr(right)?;
                // You’d do actual type checking here
                match op.tokentype {
                    TokenType::Plus | TokenType::Minus | TokenType::Star | TokenType::Slash => {
                        if left_typed.ty.is_numeric() && right_typed.ty.is_numeric() {
                            return Ok(TypedExpr {
                                ty: left_typed.ty.check_numeric(&right_typed.ty),
                                kind: TypedExprKind::Binary(
                                    Box::new(left_typed),
                                    op.clone(),
                                    Box::new(right_typed),
                                ),
                            });
                        } else {
                            return Err(TypeError::Mismatch {
                                expected: left_typed.ty,
                                found: right_typed.ty,
                                context: "Unsupported type comparison in binary operation"
                                    .to_string(),
                            });
                        }
                    }
                    TokenType::And | TokenType::Or => {
                        if left_typed.ty == Type::Bool && right_typed.ty == Type::Bool {
                            return Ok(TypedExpr {
                                ty: Type::Bool,
                                kind: TypedExprKind::Binary(
                                    Box::new(left_typed),
                                    op.clone(),
                                    Box::new(right_typed),
                                ),
                            });
                        } else {
                            return Err(TypeError::Mismatch {
                                expected: left_typed.ty,
                                found: right_typed.ty,
                                context: "Unsupported type comparison in binary operation"
                                    .to_string(),
                            });
                        }
                    }
                    _ => panic!("unsupported binary operation"),
                }
            }

            Expr::Logical(left, token, right) => {
                let left_typed = self.type_check_expr(left)?;
                let right_typed = self.type_check_expr(right)?;

                if left_typed.ty == right_typed.ty {
                    let t = left_typed.ty.clone();

                    return Ok(TypedExpr {
                        kind: TypedExprKind::Logical(
                            Box::new(left_typed),
                            token.clone(),
                            Box::new(right_typed),
                        ),
                        ty: t,
                    });
                } else {
                    // todo!("proper error handling in typechecker expr::binary");
                    return Err(TypeError::Mismatch {
                        expected: left_typed.ty,
                        found: right_typed.ty,
                        context: "unsupported".to_string(),
                    });
                }
            }
            Expr::Grouping(expr) => {
                let expr_typed = self.type_check_expr(expr)?;
                return Ok(expr_typed);
            }

            Expr::Set(field, token, value) => {
                let left_typed = self.type_check_expr(field)?;
                let right_typed = self.type_check_expr(value)?;
                if left_typed.ty == right_typed.ty {
                    return Ok(TypedExpr {
                        kind: TypedExprKind::Set(
                            Box::new(left_typed.clone()),
                            token.clone(),
                            Box::new(right_typed),
                        ),
                        ty: left_typed.ty,
                    });
                } else {
                    // todo!("proper error handling in typechecker expr::binary");
                    return Err(TypeError::Mismatch {
                        expected: left_typed.ty,
                        found: right_typed.ty,
                        context: "unsupported".to_string(),
                    });
                }
            }
            Expr::Get(token, value) => {
                // let left_typed = self.type_check_expr(field)?;
                let right_typed = self.type_check_expr(value)?;
                return Ok(right_typed);
            }
            Expr::Unary(token, expr) => {
                let unary_typed = self.type_check_expr(expr)?;
                match token.get_token_type() {
                    TokenType::Minus => {
                        if unary_typed.ty.is_numeric() {
                            return Ok(unary_typed);
                        } else {
                            return Err(TypeError::InvalidUnaryOperator {
                                op: TokenType::Minus,
                                operand: unary_typed.ty,
                            });
                        }
                    }
                    TokenType::Bang => {
                        if unary_typed.ty == Type::Bool {
                            return Ok(unary_typed);
                        } else {
                            return Err(TypeError::InvalidUnaryOperator {
                                op: TokenType::Bang,
                                operand: unary_typed.ty,
                            });
                        }
                    }
                    _ => {
                        return Err(TypeError::Other(
                            "unary operation should not reach here".to_string(),
                        ));
                    }
                }
            }
        }
    }
    pub fn error_report(&self, err: &TypeError) {
        match err {
            TypeError::Redeclaration(token, msg) => {
                eprintln!(
                    "[line {}] Error: Redeclaration of '{}': {}",
                    token.line, token.lexeme, msg
                );
            }

            TypeError::UndeclaredVariable(token, name) => {
                eprintln!(
                    "[line {}] Error: Undeclared variable '{}'",
                    token.line, name
                );
            }

            TypeError::ArityMismatch {
                expected,
                found,
                function,
            } => {
                eprintln!(
                    "Error: Function '{}' expected {} arguments but got {}.",
                    function, expected, found
                );
            }

            TypeError::NotCallable { found, location } => {
                eprintln!(
                    "[line {}] Error: Type '{:?}' is not callable.",
                    location.line, found
                );
            }

            TypeError::InvalidOperator { op, left, right } => {
                eprintln!(
                    "Error: Cannot apply operator '{:?}' to types {:?} and {:?}.",
                    op, left, right
                );
            }

            TypeError::InvalidUnaryOperator { op, operand } => {
                eprintln!(
                    "Error: Cannot apply unary operator '{:?}' to type {:?}.",
                    op, operand
                );
            }

            TypeError::Mismatch {
                expected,
                found,
                context,
            } => {
                eprintln!(
                    "Error: Type mismatch in {}: expected {:?}, found {:?}.",
                    context, expected, found
                );
            }

            TypeError::ReturnTypeMismatch { expected, found } => {
                eprintln!(
                    "Error: Function return type mismatch: expected {:?}, found {:?}.",
                    expected, found
                );
            }

            TypeError::BreakOutsideLoop(token) => {
                eprintln!(
                    "[line {}] Error: 'break' used outside of a loop.",
                    token.line
                );
            }

            TypeError::ReturnOutsideFunction(token) => {
                eprintln!(
                    "[line {}] Error: 'return' used outside of a function.",
                    token.line
                );
            }

            TypeError::UnknownField { field, in_type } => {
                eprintln!("Error: Type {:?} has no field '{}'.", in_type, field);
            }

            TypeError::UnknownMethod { method, in_type } => {
                eprintln!("Error: Type {:?} has no method '{}'.", in_type, method);
            }

            TypeError::Other(msg) => {
                eprintln!("Error: {}", msg);
            }
        }
    }
    fn pretty_print_typed_expr(&self, expr: &TypedExpr, depth: usize) {
        // let print_expression = || -> () {
        println!("{}Expression Type: {:?}   ", "  ".repeat(depth), expr.ty);
        // };
        match &expr.kind {
            TypedExprKind::Literal(literal_type) => {
                println!("{}Literal Type: {:?}   ", "  ".repeat(depth), literal_type);
            }
            TypedExprKind::Call(typed_expr, token, typed_exprs) => {
                println!("{}call expression   ", "  ".repeat(depth));
                self.pretty_print_typed_expr(&typed_expr, depth + 1);
                println!("{}name: {}   ", "  ".repeat(depth), token.lexeme);
                println!("{}call arguments   ", "  ".repeat(depth));
                for i in typed_exprs {
                    self.pretty_print_typed_expr(i, depth + 1);
                }
            }
            TypedExprKind::Logical(typed_expr, token, typed_expr1) => {
                println!("{}left side   ", "  ".repeat(depth));
                self.pretty_print_typed_expr(&typed_expr, depth + 1);
                println!(
                    "{}logical comparison: {}   ",
                    "  ".repeat(depth),
                    token.lexeme
                );
                println!("{}right side   ", "  ".repeat(depth));
                self.pretty_print_typed_expr(&typed_expr1, depth + 1);
            }
            TypedExprKind::Set(typed_expr, token, typed_expr1) => {
                println!("{}field ", "  ".repeat(depth));
                self.pretty_print_typed_expr(&typed_expr, depth + 1);
                println!("{}set token: {}   ", "  ".repeat(depth), token.lexeme);
                println!("{}value", "  ".repeat(depth));
                self.pretty_print_typed_expr(&typed_expr1, depth + 1);
            }
            TypedExprKind::Get(token, typed_expr) => {
                println!("{}get token: {}   ", "  ".repeat(depth), token.lexeme);
                println!("{}value ", "  ".repeat(depth));
                self.pretty_print_typed_expr(&typed_expr, depth + 1);
            }
            TypedExprKind::Variable(token) => {
                println!("{}variable token: {}   ", "  ".repeat(depth), token.lexeme);
            }
            TypedExprKind::Unary(token, typed_expr) => {
                println!(
                    "{}unary token: {} unary expression:",
                    "  ".repeat(depth),
                    token.lexeme
                );
                self.pretty_print_typed_expr(&typed_expr, depth + 1);
            }
            TypedExprKind::Binary(typed_expr, token, typed_expr1) => {
                println!(
                    "{}binary operation token: {}   ",
                    "  ".repeat(depth),
                    token.lexeme
                );
                println!("{}left side: ", "  ".repeat(depth));
                self.pretty_print_typed_expr(&typed_expr, depth + 1);
                println!("{}right side", "  ".repeat(depth));
                self.pretty_print_typed_expr(&typed_expr1, depth + 1);
            }
            TypedExprKind::Grouping(typed_expr) => {
                println!("{}grouping: ", "  ".repeat(depth));
                self.pretty_print_typed_expr(&typed_expr, depth + 1);
            }
        }
    }
    fn pretty_print_typed_stmts(&self, stmt: &TypedStmt, depth: usize) {
        match stmt {
            TypedStmt::Block(typed_stmts) => {
                for i in typed_stmts.as_ref() {
                    self.pretty_print_typed_stmts(i, depth + 1);
                }
            }
            TypedStmt::Variable(token, _, typed_expr) => {
                print!("{}token: {}   ", "  ".repeat(depth), token.lexeme);
                if let Some(a) = typed_expr {
                    self.pretty_print_typed_expr(a, depth + 1)
                }
            }
            TypedStmt::Print(typed_expr) => {
                print!("{}print stmt: ", "  ".repeat(depth));

                self.pretty_print_typed_expr(&typed_expr, depth + 1)
            }
            TypedStmt::Expression(typed_expr) => {
                self.pretty_print_typed_expr(typed_expr, depth + 1)
            }

            TypedStmt::Function(token, items, typed_stmts, return_type) => {
                println!("{}token: {} ", "  ".repeat(depth), token.lexeme);
                println!("{}return type {:?}", "  ".repeat(depth), return_type);
                print!("{}params: ", "  ".repeat(depth));

                for i in items.as_ref() {
                    print!(
                        "{}token: {} type: {:?}",
                        "  ".repeat(depth),
                        i.0.lexeme,
                        i.1
                    )
                }
                println!();
                for stmt in typed_stmts.as_ref() {
                    self.pretty_print_typed_stmts(stmt, depth + 1);
                }
            }
            TypedStmt::If(typed_expr, typed_stmt, typed_stmt1) => {
                println!("{}if statement condtion: ", "  ".repeat(depth));
                self.pretty_print_typed_expr(&typed_expr, depth + 1);
                println!("{}then condition: ", "  ".repeat(depth));
                self.pretty_print_typed_stmts(&typed_stmt, depth + 1);
                if let Some(x) = typed_stmt1.as_ref() {
                    println!("{}else condition: ", "  ".repeat(depth));
                    self.pretty_print_typed_stmts(x, depth + 1);
                }
            }
            TypedStmt::While(typed_expr, typed_stmt) => {
                println!("{}while condtion: ", "  ".repeat(depth));
                self.pretty_print_typed_expr(&typed_expr, depth + 1);
                println!("{}while statement: ", "  ".repeat(depth));
                self.pretty_print_typed_stmts(&typed_stmt, depth + 1);
            }
            TypedStmt::Break(token) => {
                println!("{}break token: ", "  ".repeat(depth));
            }
            TypedStmt::Return(token, typed_expr) => {
                println!("{}return  token: ", "  ".repeat(depth));
            }
            TypedStmt::Class(token, typed_stmts) => {
                todo!();
            }
        }
    }

    // parse
}

#[derive(Debug)]
pub enum TypeError {
    // Variable and declaration errors
    Redeclaration(Token, String),
    UndeclaredVariable(Token, String),

    // Function and call errors
    ArityMismatch {
        expected: usize,
        found: usize,
        function: String,
    },
    NotCallable {
        found: Type,
        location: Token,
    },

    // Operator misuse
    InvalidOperator {
        op: TokenType,
        left: Type,
        right: Type,
    },
    InvalidUnaryOperator {
        op: TokenType,
        operand: Type,
    },

    // Type mismatches
    Mismatch {
        expected: Type,
        found: Type,
        context: String,
    },
    ReturnTypeMismatch {
        expected: Type,
        found: Type,
    },

    // Control flow errors
    BreakOutsideLoop(Token),
    ReturnOutsideFunction(Token),

    // Struct/class errors
    UnknownField {
        field: String,
        in_type: Type,
    },
    UnknownMethod {
        method: String,
        in_type: Type,
    },

    // Fallback / general
    Other(String),
}
