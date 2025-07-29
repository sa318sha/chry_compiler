use std::fmt::format;

use crate::types::expr::Expr;
use crate::types::literal::Literal;
use crate::types::stmt::Stmt;
use crate::types::types::Type;

use super::parse_error::ParseError;
use crate::types::token::Token;
use crate::types::token_type::TokenType;

type ParseResult<T> = Result<T, ParseError>;

// ParseResult
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    // loop_depth: usize,
    stmts: Vec<Stmt>,
    pub errors: Vec<ParseError>,
}

impl Parser {
    pub fn new(_tokens: Vec<Token>) -> Parser {
        return Parser {
            tokens: _tokens,
            current: 0,
            // loop_depth: 0,
            stmts: Vec::new(),
            errors: Vec::new(),
        };
    }

    pub fn report_errors(&self) {
        if !self.errors.is_empty() {
            eprintln!("Parser found {} errors.", self.errors.len());
            for err in &self.errors {
                let msg = err.to_string_message();
                println!("{}", msg); // Or implement Display for nicer messages
            }
        }
    }

    pub fn parse(&mut self) -> Vec<Stmt> {
        while !self.is_at_end() {
            if let Some(stmt) = self.parse_single_stmt() {
                self.stmts.push(stmt);
            }
        }
        self.report_errors();

        return self.stmts.clone();
    }

    pub fn print_parser(&self) {
        for stmt in &self.stmts {
            self.pretty_print_ast(&stmt, 0);
        }
    }

    pub fn parse_stmt_result(&mut self) -> ParseResult<Stmt> {
        return self.declaration();
    }

    // pub fn parse_test(&mut self) -> Vec<Stmt> {
    //     while !self.is_at_end() {
    //         if let Some(stmt) = self.parse_single_stmt() {
    //             self.stmts.push(stmt);
    //         }
    //     }

    //     return self.stmts.clone();
    // }

    fn pretty_print_ast(&self, stmt: &Stmt, depth: usize) -> () {
        match stmt {
            Stmt::Block(block_stmts) => {
                println!("{}{:?}", "  ".repeat(depth), "Block Statement");
                for block_stmt in block_stmts.as_ref() {
                    self.pretty_print_ast(block_stmt, depth + 1);
                }
            }
            Stmt::Variable(token, variable_type, expr) => {
                match variable_type.as_ref() {
                    Some(x) => println!("{}{:?}Var: {:?}", "  ".repeat(depth), x, token.lexeme),
                    None => println!("{}Var: {:?}", "  ".repeat(depth), token.lexeme),
                }
                match (expr.as_ref()) {
                    Some(x) => self.pretty_print_expr(x, depth + 1),
                    _ => {}
                }
            }
            Stmt::Print(expr) => {}
            Stmt::Expression(expr) => {
                self.pretty_print_expr(expr.as_ref(), depth + 1);
            }
            Stmt::Function(token, param, func_stmts, return_type) => {
                println!("{}Func:{:?}", "  ".repeat(depth), token.lexeme);

                for func_stmt in func_stmts.as_ref() {
                    self.pretty_print_ast(func_stmt, depth + 1);
                }
            }

            Stmt::If(condition, then, else_stmt) => {
                println!("{}If Stmt", "  ".repeat(depth));

                self.pretty_print_expr(condition.as_ref(), depth);

                self.pretty_print_ast(then.as_ref(), depth + 1);

                // else_stmt.as_ref();
                if let Some(init) = else_stmt.as_ref() {
                    self.pretty_print_ast(init, depth + 1);
                }
            }
            Stmt::While(condition, stmt) => {
                println!("{}While Stmt", "  ".repeat(depth));

                self.pretty_print_expr(condition.as_ref(), depth);
                self.pretty_print_ast(stmt.as_ref(), depth + 1);
            }
            Stmt::Return(token, expr) => {
                println!("{}Return Stmt", "  ".repeat(depth));
                if let Some(x) = expr.as_ref() {
                    self.pretty_print_expr(x, depth);
                }
            }
            Stmt::Break(token) => {
                println!("{}Break Stmt", "  ".repeat(depth));
            }
            Stmt::Class(name, methods) => {
                println!("{}Class: {:?}", "  ".repeat(depth), name.lexeme);

                for method in methods.as_ref() {
                    self.pretty_print_ast(method, depth + 1);
                }
            } // Stmt::Error => {
              //     println!("Error In Stmt");
              // }
        }
    }

    fn pretty_print_expr(&self, expr: &Expr, depth: usize) -> () {
        match expr {
            Expr::Literal(n) => {
                // println!("{}{:?}", "  ".repeat(depth), n)
                match n {
                    Literal::String(str) => println!("{}{:?}", "  ".repeat(depth), str),
                    Literal::Bool(bool) => println!("{}{:?}", "  ".repeat(depth), bool),
                    Literal::Int(number) => println!("{}{:?}", "  ".repeat(depth), number),

                    _ => {
                        println!("{}{:?}", "  ".repeat(depth), "missed type");
                    }
                }
            }
            Expr::Unary(token, unary_expr) => {
                println!("{}{:?}", "  ".repeat(depth), token.lexeme);
                self.pretty_print_expr(unary_expr.as_ref(), depth + 1);
            }

            Expr::Binary(lhs, token, rhs) => {
                println!("{}{:?}", "  ".repeat(depth), token.lexeme);
                self.pretty_print_expr(lhs.as_ref(), depth + 1);

                self.pretty_print_expr(rhs.as_ref(), depth + 1);
            }
            Expr::Call(callee, token, args) => {
                println!("{} Call: ", "  ".repeat(depth));
                self.pretty_print_expr(&callee.as_ref(), depth + 1);
                for arg in args.as_ref() {
                    self.pretty_print_expr(arg, depth + 1);
                }
            }
            Expr::Variable(token) => {
                println!("{}{:?}", "  ".repeat(depth), token.lexeme);
            }
            Expr::Logical(lhs, token, rhs) => {
                self.pretty_print_expr(lhs.as_ref(), depth + 1);
                println!("{}{:?}", "  ".repeat(depth), token.lexeme);
                self.pretty_print_expr(rhs.as_ref(), depth + 1);
            }
            Expr::Set(lhs, token, val) => {
                println!("{}Set", "  ".repeat(depth));
                self.pretty_print_expr(&lhs, depth + 1);
                self.pretty_print_expr(&val, depth + 1);
            }
            _ => {
                println!("not implemented expression {:?}", expr)
            }
        }
    }

    fn parse_single_stmt(&mut self) -> Option<Stmt> {
        let result = self.declaration();

        match result {
            Ok(expr) => {
                return Some(expr);
            }
            Err(parse_error) => {
                self.errors.push(parse_error);
                self.synchronize();
                return None;
            }
        }
    }

    fn declaration(&mut self) -> ParseResult<Stmt> {
        let token = self.peek().clone();

        match token.tokentype {
            TokenType::Fun => {
                self.advance();
                return self.function();
            }
            _ => {
                return self.statements();
            }
        }
    }
    fn statements(&mut self) -> ParseResult<Stmt> {
        let token = self.peek().clone();

        match token.tokentype {
            // TokenType::Fun => {
            //     self.advance();
            //     return self.function();
            // }
            // TokenType::Var => {
            //     self.advance();
            //     return self.varDeclaration();
            // }
            TokenType::Var => {
                self.advance();
                return self.var_declaration();
            }
            TokenType::Class => {
                self.advance(); // now consume it
                return self.class_declaration();
            }
            TokenType::Return => {
                self.advance();
                return self.return_statement();
            }
            TokenType::Break => {
                self.advance();
                return self.break_statement();
            }
            TokenType::For => {
                self.advance();
                return self.for_statement();
            }
            TokenType::While => {
                self.advance();
                return self.while_statement();
            }
            TokenType::If => {
                self.advance();
                return self.if_statement();
            }
            TokenType::Print => {
                self.advance();
                return self.print_statement();
            }
            TokenType::LeftBrace => {
                self.advance();
                return Ok(Stmt::Block(Box::new(self.block()?)));
            }
            _ => {
                // println!("unhandled statement token {:?}", self.peek().tokentype);
                return self.expression_stmt();
            }
        }
    }

    fn parse_type(&mut self) -> ParseResult<Type> {
        match self.advance().tokentype {
            TokenType::Void => Ok(Type::Void),
            TokenType::Int => Ok(Type::Int),
            TokenType::Float => Ok(Type::Float),
            TokenType::Bool => Ok(Type::Bool),
            TokenType::Identifier => Err(ParseError::UnexpectedToken {
                token: self.previous().clone(),
                expected: vec![
                    TokenType::Void,
                    TokenType::Float,
                    TokenType::Int,
                    TokenType::Bool,
                ],
                message: Some("internal error, custom types are not allowed".to_string()),
            }),
            _ => Err(ParseError::InvalidType {
                token: self.previous().clone(),
                reason: (format!(
                    "{:?}, is an invalid type",
                    self.previous().clone().get_token_type()
                )),
            }),
        }
    }

    fn var_declaration(&mut self) -> ParseResult<Stmt> {
        let mut initializer: Option<Expr> = None;
        let identifier = self.consume_identifier("Expected identifer")?.clone();

        let mut var_type: Option<Type> = None;
        if self.match_tokens(&[TokenType::Colon]) {
            var_type = Some(self.parse_type()?);
        }

        if (self.match_tokens(&[TokenType::Equal])) {
            initializer = Some(self.expression()?);
        }

        self.consume(TokenType::Semicolon, "Expected ;")?;

        if var_type == None && initializer == None {
            return Err(ParseError::InvalidVariableDeclaration {
                token: identifier.clone(),
                message: format!(
                    "{} needs either initializer or type declaration",
                    identifier.lexeme
                ),
            });
        }

        return Ok(Stmt::Variable(identifier, var_type, initializer));
    }

    fn function(&mut self) -> ParseResult<Stmt> {
        // let tokenIdentifier = self.peek();
        let identifier = self.consume_identifier("expected identifier")?.clone();
        self.consume(TokenType::LeftParen, "expected (")?;

        let mut param_list: Vec<(Token, Type)> = Vec::new();
        // if self.match_tokens(&[TokenType::Colon]) {
        while (!self.is_at_end() && !self.check(&TokenType::RightParen)) {
            let identifier = self
                .consume_identifier("Expected parameter in function signature")?
                .clone();

            self.consume(
                TokenType::Colon,
                &format!(
                    "Expected Type specifier ':' after function parameter {}",
                    self.previous().lexeme
                ),
            )?;
            let param_type = self.parse_type()?;
            param_list.push((identifier, param_type));

            if self.check(&TokenType::Comma) {
                self.consume(TokenType::Comma, "Expected Comma in parameter list")?;
                continue;
            }
        }

        self.consume(
            TokenType::RightParen,
            "Expected ')' after function paremeter",
        )?;

        let mut func_type: Type = Type::Void;

        if self.match_tokens(&[TokenType::Colon]) {
            func_type = self.parse_type()?;
        }

        self.consume(TokenType::LeftBrace, "Expected { at function body")?;
        let block = self.block()?;

        return Ok(Stmt::Function(
            identifier,
            Box::new(param_list),
            Box::new(block),
            func_type,
        ));
    }

    fn class_declaration(&mut self) -> ParseResult<Stmt> {
        let name = self
            .consume(TokenType::Identifier, "Expect class name.")?
            .clone();

        self.consume(TokenType::LeftBrace, "Expected '{' before class body")?;
        let mut methods: Vec<Stmt> = Vec::new();
        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            methods.push(self.function()?);
        }

        self.consume(
            TokenType::RightBrace,
            "Expected } at the end of class definition",
        )?;

        return Ok(Stmt::Class(name, Box::new(methods)));
    }
    fn print_statement(&mut self) -> ParseResult<Stmt> {
        let val = self.expression()?;
        self.consume(TokenType::Semicolon, "expected ; after value")?;
        return Ok(Stmt::Print(Box::new(val)));
    }
    fn if_statement(&mut self) -> ParseResult<Stmt> {
        self.consume(TokenType::LeftParen, "expected (")?;

        let expr = self.expression()?;

        self.consume(TokenType::RightParen, "expected )")?;

        let then_stmt = self.statements()?;

        let mut else_stmt = None;

        if self.match_tokens(&[TokenType::Else]) {
            else_stmt = Some(self.statements()?);
        }

        return Ok(Stmt::If(
            Box::new(expr),
            Box::new(then_stmt),
            Box::new(else_stmt),
        ));
    }
    fn while_statement(&mut self) -> ParseResult<Stmt> {
        self.consume(TokenType::LeftParen, "expected (")?;

        let expr = self.expression()?;

        self.consume(TokenType::RightParen, "expected )")?;

        // self.consume(TokenType::LeftBrace, "expected {")?;

        let block = self.statements()?;

        return Ok(Stmt::While(Box::new(expr), Box::new(block)));
    }
    fn for_statement(&mut self) -> ParseResult<Stmt> {
        self.consume(TokenType::LeftParen, "expected (")?;

        let initializer;
        if self.match_tokens(&[TokenType::Semicolon]) {
            initializer = None;
        } else if (self.match_tokens(&[TokenType::Var])) {
            initializer = Some(self.var_declaration()?);
        } else {
            initializer = Some(self.expression_stmt()?);
        }

        let condition;
        if !self.match_tokens(&[TokenType::Semicolon]) {
            condition = (self.expression()?);
        } else {
            condition = (Expr::Literal(Literal::Bool(true)));
        }

        self.consume(
            TokenType::Semicolon,
            "Expected ';' after for loop condition",
        )?;

        let mut increment = None;
        if !self.check(&TokenType::RightParen) {
            increment = Some(self.expression()?);
        }

        self.consume(TokenType::RightParen, "Expected )")?;

        let mut body = self.statements()?;

        if let Some(inc) = increment {
            let expression = Stmt::Expression(Box::new(inc));
            let vec: Vec<Stmt> = vec![body, expression];

            body = Stmt::Block(Box::new(vec));
        }

        // if let Some(x) = condition {
        body = Stmt::While(Box::new(condition), Box::new(body));
        // }

        if let Some(init) = initializer {
            body = Stmt::Block(Box::new(vec![init, body]));
        }

        return Ok(body);
    }
    fn break_statement(&mut self) -> ParseResult<Stmt> {
        // if (self.loop_depth == 0) {
        //     return Err(ParseError::ParseToken(
        //         self.previous().clone(),
        //         String::from("invalid use of break"),
        //     ));
        // }
        self.consume(TokenType::Semicolon, "Expected ; after break")?;

        return Ok(Stmt::Break(self.previous().clone()));
    }
    fn return_statement(&mut self) -> ParseResult<Stmt> {
        // return Err(ParseError::InvalidInput);

        let keyword = self.previous().clone();
        let mut val = None;

        if !self.check(&TokenType::Semicolon) {
            val = Some(self.expression()?);
        }

        self.consume(TokenType::Semicolon, "Expect ';' after return value")?;
        return Ok(Stmt::Return(keyword, Box::new(val)));
    }

    fn block(&mut self) -> ParseResult<Vec<Stmt>> {
        let prev = self.previous().clone();
        let mut statements: Vec<Stmt> = Vec::new();
        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            statements.push(self.declaration()?);
        }
        if self.check(&TokenType::RightBrace) {
            self.advance();
            return Ok(statements);
        } else {
            return Err(ParseError::UnterminatedBlock {
                start: prev,
                end: self.peek().clone(),
            });
        }
        return Ok(statements);
    }

    fn expression_stmt(&mut self) -> ParseResult<Stmt> {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon, "Expected ;")?;
        return Ok(Stmt::Expression(Box::new(expr)));
    }

    fn expression(&mut self) -> ParseResult<Expr> {
        return self.assignment();
    }

    fn assignment(&mut self) -> ParseResult<Expr> {
        // None
        let mut expr = self.or()?;

        if self.match_tokens(&[TokenType::Equal]) {
            let token = self.previous().clone();
            let rhs = self.assignment()?;

            match expr {
                Expr::Variable(token) => {
                    return Ok(Expr::Assign(token, Box::new(rhs)));
                }
                _ => {
                    return Err(ParseError::UnexpectedToken {
                        token: token,
                        expected: vec![TokenType::Equal],
                        message: Some(format!("not a valid assignment expression")),
                    });
                }
            }
            // expr = Expr::Binary(Box::new(expr), token, Box::new(rhs));
        }

        return Ok(expr);
    }

    fn or(&mut self) -> ParseResult<Expr> {
        let mut expr = self.and()?;

        while self.match_tokens(&[TokenType::Or]) {
            let token = self.previous().clone();
            let rhs: Expr = self.and()?;
            expr = Expr::Logical(Box::new(expr), token, Box::new(rhs));
        }
        return Ok(expr);
    }

    fn and(&mut self) -> ParseResult<Expr> {
        let mut expr = self.equality()?;

        while self.match_tokens(&[TokenType::And]) {
            let token = self.previous().clone();
            let rhs: Expr = self.equality()?;
            expr = Expr::Logical(Box::new(expr), token, Box::new(rhs));
        }
        return Ok(expr);
    }

    fn equality(&mut self) -> ParseResult<Expr> {
        let mut expr = self.comparison()?;

        while self.match_tokens(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let token = self.previous().clone();
            let rhs: Expr = self.comparison()?;
            expr = Expr::Binary(Box::new(expr), token, Box::new(rhs));
        }
        return Ok(expr);
    }

    fn comparison(&mut self) -> ParseResult<Expr> {
        let mut expr = self.term()?;

        while self.match_tokens(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let token = self.previous().clone();
            let rhs: Expr = self.term()?;
            expr = Expr::Binary(Box::new(expr), token, Box::new(rhs));
        }
        return Ok(expr);
    }

    fn term(&mut self) -> ParseResult<Expr> {
        let mut expr = self.factor()?;

        while self.match_tokens(&[TokenType::Plus, TokenType::Minus]) {
            let token = self.previous().clone();
            let rhs: Expr = self.factor()?;
            expr = Expr::Binary(Box::new(expr), token, Box::new(rhs));
        }
        return Ok(expr);
    }

    fn factor(&mut self) -> ParseResult<Expr> {
        let mut expr = self.unary()?;

        while self.match_tokens(&[TokenType::Star, TokenType::Slash]) {
            let token = self.previous().clone();
            let rhs = self.unary()?;
            expr = Expr::Binary(Box::new(expr), token, Box::new(rhs));
        }

        return Ok(expr);
    }

    fn unary(&mut self) -> ParseResult<Expr> {
        while (self.match_tokens(&[TokenType::Bang, TokenType::Minus])) {
            let previous_token = self.previous().clone();
            let expr = self.unary()?;
            return Ok(Expr::Unary(previous_token, Box::new(expr)));
        }
        return self.call();
    }

    fn call(&mut self) -> ParseResult<Expr> {
        let mut expr = self.primary()?;
        loop {
            if (self.match_tokens(&[TokenType::LeftParen])) {
                expr = self.finish_call(expr)?;
            } else if (self.match_tokens(&[TokenType::Dot])) {
                let name = self.consume_identifier("Expect property name after '.'.")?;
                expr = Expr::Get(name.clone(), Box::new(expr));
            } else {
                break;
            }
        }
        return Ok(expr);
    }

    fn finish_call(&mut self, callee: Expr) -> ParseResult<Expr> {
        // if let Expr::Variable(t) = callee {
        let mut arg_list: Vec<Expr> = Vec::new();

        while !self.is_at_end() && !self.check(&TokenType::RightParen) {
            arg_list.push(self.expression()?);

            if self.check(&TokenType::Comma) {
                self.consume(TokenType::Comma, "Expected Comma in parameter list")?;
                continue;
            }
        }

        let right_paren = self
            .consume(
                TokenType::RightParen,
                "expected ')' at the end of function call",
            )?
            .clone();

        return Ok(Expr::Call(
            Box::new(callee),
            right_paren,
            Box::new(arg_list),
        ));
        // } else {
        //     // return Err(ParseError { token: (), reason: () });
        // }
    }

    fn primary(&mut self) -> ParseResult<Expr> {
        let t = self.peek().clone();
        // let lit = t.literal;
        match &t.tokentype {
            TokenType::Identifier => {
                self.advance();
                return Ok(Expr::Variable(t));
            }
            TokenType::False
            | TokenType::True
            | TokenType::IntLiteral
            | TokenType::FloatLiteral
            | TokenType::StringLiteral => {
                let l = t
                    .literal
                    .clone()
                    .ok_or_else(|| ParseError::InvalidLiteral {
                        token: t.clone(),
                        reason: String::from(format!(
                            "Expected literal value for {:?} got None instead",
                            t.tokentype
                        )),
                    })?;

                self.advance();
                // if let Some(l) = t.literal {}
                return Ok(Expr::Literal(l));
            }
            TokenType::LeftParen => {
                self.advance(); // consume '('
                let expr = self.expression()?;
                self.consume(TokenType::RightParen, "Expected right Parenthesis")?;
                Ok(Expr::Grouping(Box::new(expr)))
            }
            _ => {
                // println!("Unexpected token in primary: {:?}", t);
                return Err(ParseError::UnexpectedToken {
                    token: self.peek().clone(),
                    expected: vec![
                        TokenType::False,
                        TokenType::Identifier,
                        TokenType::True,
                        TokenType::IntLiteral,
                        TokenType::StringLiteral,
                        TokenType::LeftParen,
                        TokenType::True,
                    ],
                    message: Some(String::from("Unexpected token")),
                });
            }
        }
    }

    fn synchronize(&mut self) {
        self.advance();
        while (!self.is_at_end()) {
            if (self.previous().tokentype == TokenType::Semicolon) {
                return;
            }
            match self.peek().tokentype {
                TokenType::Class => {
                    return;
                }
                TokenType::Fun => {
                    return;
                }
                TokenType::Var => {
                    return;
                }
                TokenType::For => {
                    return;
                }
                TokenType::If => {
                    return;
                }
                TokenType::While => {
                    return;
                }
                TokenType::Return => {
                    return;
                }
                _ => {
                    self.advance();
                }
            }
        }
    }

    fn match_tokens(&mut self, tokens: &[TokenType]) -> bool {
        for token in tokens {
            if &self.tokens[self.current].tokentype == token {
                self.advance();
                return true;
            }
        }
        return false;
    }

    // fn match_token_types(&mut self, tokens:&[TokenType]) -> Type {
    //     match self.peek() {
    //         TokenType::Int() => {return Type::Int}
    //     }
    // }

    fn match_identifier(&mut self) -> bool {
        if matches!(self.peek().tokentype, TokenType::Identifier) {
            self.advance();
            return true;
        }

        return false;
    }

    // fn match_without_advance(&mut self, )

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        return self.previous();
    }

    fn consume(&mut self, token: TokenType, message: &str) -> ParseResult<&Token> {
        if self.check(&token) {
            return Ok(self.advance());
        }

        if self.is_at_end() {
            return Err(ParseError::UnexpectedEof {
                token: self.peek().clone(),
                expected: vec![token],
            });
        }

        Err(ParseError::UnexpectedToken {
            token: self.peek().clone(),
            expected: vec![token],
            message: Some(message.to_string()),
        })
    }

    // fn consume_type(&mut self, )

    fn check(&self, token: &TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        return &self.tokens[self.current].tokentype == token;
    }

    fn consume_identifier(&mut self, s: &str) -> ParseResult<&Token> {
        if self.check_identifier() {
            return Ok(self.advance());
        }
        return Err(ParseError::InvalidType {
            token: self.peek().clone(),
            reason: String::from(s),
        });
    }

    fn check_identifier(&self) -> bool {
        if self.is_at_end() {
            return false;
        }
        matches!(self.tokens[self.current].tokentype, TokenType::Identifier)
    }

    fn peek(&self) -> &Token {
        return &self.tokens[self.current];
    }

    fn is_at_end(&self) -> bool {
        return self.peek().tokentype == TokenType::Eof;
    }

    fn previous(&self) -> &Token {
        // if (self.current - 1 < 0) {
        return &self.tokens[self.current - 1];
        // } else {
        //     return None;
        // }
    }
}
