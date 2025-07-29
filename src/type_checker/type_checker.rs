use crate::type_checker::type_error::TypeError;
// use super::type_error::
use crate::types::expr::Expr;
use crate::types::literal::Literal;

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

    pub fn check(&mut self, stmts: &Vec<Stmt>) -> Vec<TypedStmt> {
        for stmt in stmts {
            if let Some(typed) = self.check_single_stmt(stmt) {
                self.typed_stmts.push(typed);
            }
        }

        if !self.errors.is_empty() {
            eprintln!("Type checking found {} errors.", self.errors.len());
            for err in &self.errors {
                err.error_report(); // Or implement Display for nicer messages
            }
        }

        return self.typed_stmts.clone();
        //     let x = self.check_single_stmt(stmt);
        // }
    }

    pub fn check_single_stmt(&mut self, stmt: &Stmt) -> Option<TypedStmt> {
        match self.type_check_stmt(stmt) {
            Ok((typed_stmt, _)) => Some(typed_stmt),
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

    fn check_block(&mut self, stmts: &Vec<Stmt>) -> TypeResult<(Vec<TypedStmt>, bool)> {
        let mut always_returns = false;
        let mut typed = Vec::new();
        self.stack.enter_scope();
        for stmt in stmts {
            let (typed_s, returns) = self.type_check_stmt(stmt)?;
            typed.push(typed_s);
            if returns {
                always_returns = true;
                break; // No point checking further stmts: this path returns
            }
        }
        self.stack.exit_scope();

        return Ok((typed, always_returns));
    }

    fn type_check_stmt(&mut self, stmt: &Stmt) -> TypeResult<(TypedStmt, bool)> {
        match stmt {
            Stmt::Block(stmts) => {
                let (val, always_returns) = self.check_block(stmts)?;
                return Ok((TypedStmt::Block(Box::new(val)), always_returns));
            }
            Stmt::Variable(name, var_type, initializer) => {
                let mut init: Option<TypedExpr> = None;
                let mut typed_var_type: Type;
                if let Some(initializer_expr) = initializer {
                    //has some initializer
                    // let x = 5; <- 5 is initializer
                    let typed_initializer = self.type_check_expr(initializer_expr)?;

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
                    init = Some(typed_initializer.clone());

                    typed_var_type = typed_initializer.ty;
                    // init = Some(typed_initializer);
                } else {
                    // no initializer
                    // may have a optional declared type
                    let val;
                    if let Some(var) = var_type {
                        typed_var_type = var.clone();
                        val = self.stack.define(&name.lexeme.to_string(), var.clone());
                    } else {
                        return Err(TypeError::UndeclaredVariable(
                            name.clone(),
                            format!("variable does not have a type or intializer"),
                        ));
                        // Error for now - todo - not implemented inferred types
                        // val = self.stack.declare(name.lexeme.as_str());
                    }
                    if let Err(error) = val {
                        return Err(TypeError::Redeclaration(
                            name.clone(),
                            "redecleration of variable".to_string(),
                        ));
                    }
                }
                return Ok((
                    TypedStmt::Variable(name.clone(), typed_var_type, init),
                    false,
                ));
            }

            Stmt::Print(val) => {
                let res = self.type_check_expr(val.as_ref())?;
                return Ok((TypedStmt::Print(Box::new(res)), false));
            }

            Stmt::Expression(expr) => {
                let res = self.type_check_expr(expr.as_ref())?;
                return Ok((TypedStmt::Expression(Box::new(res)), false));
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

                let (val, always_returns) = self.check_block(block)?;

                // let val = self.check_block(&block.as_ref())?;
                if !always_returns && return_type != &Type::Void {
                    return Err(TypeError::NoReturnType {
                        expected: return_type.clone(),
                        name: name.clone(),
                    });
                }

                self.stack.exit_scope();
                self.loop_depth = saved_loop_depth; // restore when done
                self.current_return_type = previous_return_type;

                return Ok((
                    TypedStmt::Function(
                        name.clone(),
                        params.clone(),
                        Box::new(val),
                        return_type.clone(),
                    ),
                    always_returns,
                ));
            }

            Stmt::If(expr, then, else_stmt) => {
                let if_expr = self.type_check_expr(expr.as_ref())?;

                if if_expr.ty != Type::Bool {
                    return Err(TypeError::Mismatch {
                        found: if_expr.ty,
                        expected: Type::Bool,
                        context: "expected boolean condition in control flow 'if'".to_string(),
                    });
                }
                self.stack.enter_scope();

                let (typed_then, then_returns) = self.type_check_stmt(then.as_ref())?;

                self.stack.exit_scope();

                let (typed_else, else_returns) = if let Some(e) = else_stmt.as_ref() {
                    self.stack.enter_scope();
                    let (t, r) = self.type_check_stmt(e)?;
                    self.stack.exit_scope();

                    (Some(t), r)
                } else {
                    (None, false)
                };

                return Ok((
                    TypedStmt::If(
                        Box::new(if_expr),
                        Box::new(typed_then),
                        Box::new(typed_else),
                    ),
                    then_returns && else_returns,
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
                let (stmt, return_val) = self.type_check_stmt(stmt.as_ref())?;

                let always_returns = match expr.as_ref() {
                    Expr::Literal(Literal::Bool(true)) => return_val,
                    _ => false,
                };
                self.loop_depth -= 1; // leaving a loop
                self.stack.exit_scope();

                return Ok((
                    TypedStmt::While(Box::new(while_expr), Box::new(stmt)),
                    always_returns,
                ));
            }
            // For(Box<Stmt>, Box<Expr>, Box<Expr>),
            Stmt::Break(token) => {
                if self.loop_depth == 0 {
                    return Err(TypeError::BreakOutsideLoop(token.clone()));
                }
                Ok((TypedStmt::Break(token.clone()), false))
            }
            Stmt::Return(token, expr) => {
                // let while_expr = self.type_check_expr(expr.as_ref())?;

                if self.current_return_type == None {
                    return Err(TypeError::ReturnOutsideFunction(token.clone()));
                }

                let mut expr_typed = None;
                if let Some(val) = expr.as_ref() {
                    expr_typed = Some(self.type_check_expr(val)?);
                }

                match &self.current_return_type {
                    Some(t) => {
                        if let Some(expr) = expr_typed.clone() {
                            if &expr.ty == t {
                                return Ok((
                                    TypedStmt::Return(token.clone(), Box::new(expr_typed)),
                                    true,
                                ));
                            } else {
                                return Err(TypeError::Mismatch{
                                    expected: expr.ty,
                                    found: t.clone(),
                                    context: "return condition doesnt evaluate to the same type of function signature".to_string(),
                            });
                            }
                        } else {
                            if t == &Type::Void {
                                return Ok((
                                    TypedStmt::Return(token.clone(), Box::new(expr_typed)),
                                    true,
                                ));
                            }
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
                            return Ok((
                                TypedStmt::Return(token.clone(), Box::new(expr_typed)),
                                true,
                            ));
                        }
                    }
                }
            }
            Stmt::Class(token, stmts) => {
                todo!("class statement type checking not implemented")
            } // Stmt::Empty => {}
        }
    }

    pub fn type_check_expr(&self, expr: &Expr) -> TypeResult<TypedExpr> {
        match expr {
            Expr::Assign(name, val) => {
                let assignment_expr = self.type_check_expr(val)?;
                let ty = self.stack.lookup(&name.lexeme);
                if let Some(var_type) = ty {
                    if assignment_expr.ty == *var_type {
                        return Ok(TypedExpr {
                            ty: assignment_expr.ty.clone(),
                            kind: TypedExprKind::Assignment(
                                name.clone(),
                                Box::new(assignment_expr),
                            ),
                        });
                    }
                }
                todo!();
            }
            Expr::Literal(value) => {
                let ty = value.get_type();
                Ok(TypedExpr {
                    kind: TypedExprKind::Literal(value.clone()),
                    ty: ty,
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
                let callee_typed = self.type_check_expr(callee)?;
                println!("{:?}", callee_typed);
                if let TypedExprKind::Variable(token) = callee_typed.kind {
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
                            // todo!();
                            return Ok(TypedExpr {
                                kind: TypedExprKind::Call(token, _paren.clone(), typed_args),
                                ty: ret_type.as_ref().clone(),
                            });
                        }
                        other_type => {
                            return Err(TypeError::NotCallable {
                                found: other_type.clone(),
                                location: _paren.clone(),
                            });
                        }
                    }
                }
                // Ensure callee is of function type
                return Err(TypeError::Other("".to_string()));
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
                    TokenType::BangEqual
                    | TokenType::EqualEqual
                    | TokenType::Less
                    | TokenType::LessEqual
                    | TokenType::Greater
                    | TokenType::GreaterEqual => {
                        if left_typed.ty.is_numeric() && right_typed.ty.is_numeric() {
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

                if left_typed.ty == Type::Bool && right_typed.ty == Type::Bool {
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
                            return Ok(TypedExpr {
                                ty: unary_typed.ty.clone(),

                                kind: TypedExprKind::Unary(token.clone(), Box::new(unary_typed)),
                            });
                        } else {
                            return Err(TypeError::InvalidUnaryOperator {
                                op: TokenType::Minus,
                                operand: unary_typed.ty,
                            });
                        }
                    }
                    TokenType::Bang => {
                        if unary_typed.ty == Type::Bool {
                            return Ok(TypedExpr {
                                ty: unary_typed.ty.clone(),
                                kind: TypedExprKind::Unary(token.clone(), Box::new(unary_typed)),
                            });
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

    fn pretty_print_typed_expr(&self, expr: &TypedExpr, depth: usize) {
        // let print_expression = || -> () {
        println!("{}Expression Type: {:?}   ", "  ".repeat(depth), expr.ty);
        // };
        match &expr.kind {
            TypedExprKind::Literal(literal_type) => {
                println!("{}Literal Type: {:?}   ", "  ".repeat(depth), literal_type);
            }
            TypedExprKind::Call(name, token, typed_exprs) => {
                println!("{}call expression   ", "  ".repeat(depth));
                // self.pretty_print_typed_expr(&typed_expr, depth + 1);
                println!("{}name: {}   ", "  ".repeat(depth), name.lexeme);
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
            TypedExprKind::Assignment(token, typed_expr1) => {
                println!(
                    "{}assignment operation token: {}   ",
                    "  ".repeat(depth),
                    token.lexeme
                );
                println!("{}right side", "  ".repeat(depth));
                self.pretty_print_typed_expr(&typed_expr1, depth + 1);
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
}
