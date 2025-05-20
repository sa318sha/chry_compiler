use crate::error::ParseError;
// use crate::error::ParseResult;
use crate::expr::{Expr, Type};
use crate::stmt::Stmt;

use crate::token::Token;
use crate::token_type::TokenType;

type ParseResult<T> = Result<T, ParseError>;

// ParseResult
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    loop_depth: usize,
}

impl Parser {
    pub fn new(_tokens: Vec<Token>) -> Parser {
        return Parser {
            tokens: _tokens,
            current: 0,
            loop_depth: 0,
        };
    }

    fn pretty_print_ast(&self, stmt: &Stmt, depth: usize) -> () {
        match stmt {
            Stmt::Block(block_stmts) => {
                println!("{}{:?}", "  ".repeat(depth), "Block Statement");
                for block_stmt in block_stmts.as_ref() {
                    self.pretty_print_ast(block_stmt, depth + 1);
                }
                // self.pretty_print_ast(stmts, depth);
            }
            Stmt::Variable(token, expr) => {
                println!("{}Var: {:?}", "  ".repeat(depth), token.lexeme);
                self.pretty_print_expr(expr.as_ref(), depth + 1);
            }
            Stmt::Print(expr) => {}
            Stmt::Expression(expr) => {
                self.pretty_print_expr(expr.as_ref(), depth + 1);
            }
            Stmt::Function(token, param, func_stmts) => {
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
                if (!matches!(*else_stmt.as_ref(), Stmt::Empty)) {
                    self.pretty_print_ast(else_stmt.as_ref(), depth + 1);
                }
            }
            Stmt::While(condition, stmt) => {
                println!("{}While Stmt", "  ".repeat(depth));

                self.pretty_print_expr(condition.as_ref(), depth);
                self.pretty_print_ast(stmt.as_ref(), depth + 1);
            }
            Stmt::Return(token, expr) => {
                println!("{}Return Stmt", "  ".repeat(depth));
                self.pretty_print_expr(expr.as_ref(), depth);
            }
            Stmt::Break(token) => {
                println!("{}Break Stmt", "  ".repeat(depth));
            }
            Stmt::Class(name, methods) => {
                println!("{}Class: {:?}", "  ".repeat(depth), name.lexeme);

                for method in methods.as_ref() {
                    self.pretty_print_ast(method, depth + 1);
                }
            }

            Stmt::Error => {
                println!("Error In Stmt");
            }
            _ => {
                println!("UnImplemented pretty print")
            }
        }
    }

    fn pretty_print_expr(&self, expr: &Expr, depth: usize) -> () {
        match expr {
            Expr::Literal(n) => {
                // println!("{}{:?}", "  ".repeat(depth), n)
                match n {
                    Type::String(str) => println!("{}{:?}", "  ".repeat(depth), str),
                    Type::Bool(bool) => println!("{}{:?}", "  ".repeat(depth), bool),
                    Type::Number(number) => println!("{}{:?}", "  ".repeat(depth), number),

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
                println!("{}Call: ", "  ".repeat(depth));
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

    pub fn parse(&mut self) -> Vec<Stmt> {
        let mut stmts: Vec<Stmt> = Vec::new();

        while !self.is_at_end() {
            stmts.push(self.parsed_stmts());
        }
        // let expr = self.statements();
        for stmt in &stmts {
            self.pretty_print_ast(&stmt, 0);
        }

        return stmts.clone();
        // return Err(ParseError::InvalidInput);
    }

    pub fn parse_test(&mut self) -> Vec<Stmt> {
        let mut stmts: Vec<Stmt> = Vec::new();

        while !self.is_at_end() {
            stmts.push(self.parsed_stmts());
        }
        // let expr = self.statements();
        // for stmt in &stmts {
        //     self.pretty_print_ast(&stmt, 0);
        // }

        return stmts.clone();
    }

    fn parsed_stmts(&mut self) -> Stmt {
        let result = self.declaration();

        match result {
            Ok(expr) => {
                return expr;
            }
            Err(parse_error) => {
                match parse_error {
                    ParseError::ParseToken(token, str) => {
                        println!(
                            "line: {}, characters: {}, error msg: {}",
                            token.line, token.lexeme, str
                        )
                    }
                    _ => {}
                }
                self.synchronize();
                return Stmt::Error;
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
            TokenType::Var => {
                self.advance();
                return self.varDeclaration();
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
            // TokenType::Print => {
            //     self.advance();
            //     return self.print_statement();
            // }
            TokenType::LeftBrace => {
                self.advance();
                return Ok(Stmt::Block(Box::new(self.block()?)));
            }
            _ => {
                // println!("unhandled statement token {:?}", self.peek().tokentype);
                return self.expressionStmt();
            }
        }
    }

    fn varDeclaration(&mut self) -> ParseResult<Stmt> {
        let mut initializer: Expr = Expr::Empty;
        let identifier = self.consume_identifier("Expected identifer")?.clone();

        if (self.match_tokens(&[TokenType::Equal])) {
            initializer = self.expression()?;
        }

        self.consume(TokenType::Semicolon, "Expected ;")?;

        return Ok(Stmt::Variable(identifier, Box::new(initializer)));
    }

    fn function(&mut self) -> ParseResult<Stmt> {
        // let tokenIdentifier = self.peek();
        let identifier = self.consume_identifier("expected identifier")?.clone();
        self.consume(TokenType::LeftParen, "expected (")?;

        let mut param_list: Vec<Token> = Vec::new();

        while (!self.is_at_end() && !self.check(&TokenType::RightParen)) {
            let identifier = self
                .consume_identifier("Expected parameter in function signature")?
                .clone();
            param_list.push(identifier);

            if self.check(&TokenType::Comma) {
                self.consume(TokenType::Comma, "Expected Comma in parameter list")?;
                continue;
            }
        }

        self.consume(
            TokenType::RightParen,
            "Expected ')' after function paremeter",
        )?;

        self.consume(TokenType::LeftBrace, "Expected { at function body")?;
        let block = self.block()?;

        return Ok(Stmt::Function(
            identifier,
            Box::new(param_list),
            Box::new(block),
        ));
    }

    fn class_declaration(&mut self) -> ParseResult<Stmt> {
        let name = self
            .consume(
                TokenType::Identifier(String::from("a")),
                "Expect class name.",
            )?
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
    fn print_statement(&self) -> ParseResult<Stmt> {
        return Err(ParseError::InvalidInput);
    }
    fn if_statement(&mut self) -> ParseResult<Stmt> {
        self.consume(TokenType::LeftParen, "expected (")?;

        let expr = self.expression()?;

        self.consume(TokenType::RightParen, "expected )")?;

        let then_stmt = self.statements()?;

        let mut else_stmt = Stmt::Empty;

        if self.match_tokens(&[TokenType::Else]) {
            else_stmt = self.statements()?;
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

        self.consume(TokenType::LeftBrace, "expected {")?;

        let block = self.statements()?;

        return Ok(Stmt::While(Box::new(expr), Box::new(block)));
    }
    fn for_statement(&mut self) -> ParseResult<Stmt> {
        self.consume(TokenType::LeftParen, "expected (")?;

        let mut initializer = Stmt::Empty;
        if self.match_tokens(&[TokenType::Semicolon]) {
            initializer = Stmt::Empty;
        } else if (self.match_tokens(&[TokenType::Var])) {
            initializer = self.varDeclaration()?;
        } else {
            initializer = self.expressionStmt()?;
        }

        let mut condition = Expr::Empty;
        if !self.match_tokens(&[TokenType::Semicolon]) {
            condition = self.expression()?;
        }

        let mut increment = Expr::Empty;
        if !self.check(&TokenType::RightParen) {
            increment = self.expression()?;
        }

        self.consume(TokenType::RightParen, "Expected )")?;

        self.consume(TokenType::LeftBrace, "Expected '{' before function body")?;

        let mut body = self.statements()?;

        if !matches!(increment, Expr::Empty) {
            let expression = Stmt::Expression(Box::new(increment));
            let vec: Vec<Stmt> = vec![body, expression];

            body = Stmt::Block(Box::new(vec));
        }

        if matches!(condition, Expr::Empty) {
            condition = Expr::Literal(Type::Bool(true));
        }
        body = Stmt::While(Box::new(condition), Box::new(body));

        if !matches!(initializer, Stmt::Empty) {
            body = Stmt::Block(Box::new(vec![initializer, body]));
        }

        return Ok(body);
    }
    fn break_statement(&mut self) -> ParseResult<Stmt> {
        if (self.loop_depth == 0) {
            return Err(ParseError::ParseToken(
                self.previous().clone(),
                String::from("invalid use of break"),
            ));
        }
        self.consume(TokenType::Semicolon, "Expected ; after break")?;

        return Ok(Stmt::Break(self.previous().clone()));
    }
    fn return_statement(&mut self) -> ParseResult<Stmt> {
        // return Err(ParseError::InvalidInput);

        let keyword = self.previous().clone();
        let mut val = Expr::Empty;

        if !self.check(&TokenType::Semicolon) {
            val = self.expression()?;
        }

        self.consume(TokenType::Semicolon, "Expect ';' after return value")?;
        return Ok(Stmt::Return(keyword, Box::new(val)));
    }

    fn block(&mut self) -> ParseResult<Vec<Stmt>> {
        let mut statements: Vec<Stmt> = Vec::new();
        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            statements.push(self.declaration()?);
        }
        self.consume(TokenType::RightBrace, "Expect '}' after block.")?;
        return Ok(statements);
    }

    fn expressionStmt(&mut self) -> ParseResult<Stmt> {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon, "Expected ;")?;
        return Ok(Stmt::Expression(Box::new(expr)));
    }

    fn expression(&mut self) -> ParseResult<Expr> {
        return self.assignment();
    }

    fn assignment(&mut self) -> ParseResult<Expr> {
        // None
        let mut expr = self.term()?;

        if self.match_tokens(&[TokenType::Equal]) {
            let token = self.previous().clone();
            let rhs = self.term()?;

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
        let mut arg_list: Vec<Expr> = Vec::new();

        while (!self.is_at_end() && !self.check(&TokenType::RightParen)) {
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
    }

    fn primary(&mut self) -> ParseResult<Expr> {
        let t = self.peek().clone();
        match &t.tokentype {
            TokenType::Identifier(str) => {
                self.advance();
                return Ok(Expr::Variable(t));
            }
            TokenType::False => {
                return Ok(Expr::Literal(Type::Bool(false)));
            }
            TokenType::True => {
                self.advance();
                return Ok(Expr::Literal(Type::Bool(true)));
            }
            // TokenType::
            TokenType::Number(n) => {
                self.advance();
                return Ok(Expr::Literal(Type::Number(*n)));
            }

            TokenType::String(string) => {
                self.advance();
                return Ok(Expr::Literal(Type::String(string.clone())));
            }
            TokenType::LeftParen => {
                self.advance(); // consume '('
                let expr = self.expression()?;
                self.consume(TokenType::RightParen, "Expected right Parenthesis")?;
                Ok(Expr::Grouping(Box::new(expr)))
            }
            _ => {
                // println!("Unexpected token in primary: {:?}", t);
                return Err(ParseError::ParseToken(
                    self.peek().clone(),
                    String::from("Unexpected token"),
                ));
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

    fn match_identifier(&mut self) -> bool {
        if matches!(self.peek().tokentype, TokenType::Identifier(_)) {
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

    fn consume(&mut self, token: TokenType, msg: &str) -> ParseResult<&Token> {
        if self.check(&token) {
            return Ok(self.advance());
        }
        return Err(ParseError::ParseToken(
            self.peek().clone(),
            String::from(msg),
        ));
    }

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
        return Err(ParseError::ParseToken(self.peek().clone(), String::from(s)));
    }

    fn check_identifier(&self) -> bool {
        if self.is_at_end() {
            return false;
        }
        matches!(
            self.tokens[self.current].tokentype,
            TokenType::Identifier(_)
        )
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

// impl Parser {
//     fn expression(&mut self) -> Option<Expr> {
//         self.equality()
//     }

//     fn equality(&mut self) -> Option<Expr> {
//         let mut expr = self.comparison()?;

//         while self.match_token_types(&[TokenType::BangEqual, TokenType::EqualEqual]) {
//             let operator = self.previous().clone();
//             let right = self.comparison()?;
//             expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
//         }

//         Some(expr)
//     }

//     fn comparison(&mut self) -> Option<Expr> {
//         let mut expr = self.term()?;

//         while self.match_token_types(&[
//             TokenType::Greater,
//             TokenType::GreaterEqual,
//             TokenType::Less,
//             TokenType::LessEqual,
//         ]) {
//             let operator = self.previous().clone();
//             let right = self.term()?;
//             expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
//         }

//         Some(expr)
//     }

//     fn term(&mut self) -> Option<Expr> {
//         let mut expr = self.factor()?;

//         while self.match_token_types(&[TokenType::Plus, TokenType::Minus]) {
//             let operator = self.previous().clone();
//             let right = self.factor()?;
//             expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
//         }

//         Some(expr)
//     }

//     fn factor(&mut self) -> Option<Expr> {
//         let mut expr = self.unary()?;

//         while self.match_token_types(&[TokenType::Slash, TokenType::Star]) {
//             let operator = self.previous().clone();
//             let right = self.unary()?;
//             expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
//         }

//         Some(expr)
//     }

//     fn unary(&mut self) -> Option<Expr> {
//         if self.match_token_types(&[TokenType::Bang, TokenType::Minus]) {
//             let operator = self.previous().clone();
//             let right = self.unary()?;
//             return Some(Expr::Unary(operator, Box::new(right)));
//         }

//         self.primary()
//     }

//     fn primary(&mut self) -> Option<Expr> {
//         let t = self.peek().clone();
//         match &t.tokentype {
//             TokenType::Number(n) => {
//                 self.advance();
//                 Some(Expr::Literal(*n))
//             }
//             TokenType::LeftParen => {
//                 self.advance(); // consume '('
//                 let expr = self.expression()?;
//                 self.consume(TokenType::RightParen)?;
//                 Some(Expr::Grouping(Box::new(expr)))
//             }
//             _ => {
//                 println!("Unexpected token in primary: {:?}", t);
//                 None
//             }
//         }
//     }

//     // Helpers

//     fn match_token_types(&mut self, types: &[TokenType]) -> bool {
//         for tt in types {
//             if self.check(tt) {
//                 self.advance();
//                 return true;
//             }
//         }
//         false
//     }

//     fn check(&self, tt: &TokenType) -> bool {
//         if self.is_at_end() {
//             return false;
//         }
//         &self.peek().tokentype == tt
//     }

//     fn consume(&mut self, expected: TokenType) -> Option<&Token> {
//         if self.check(&expected) {
//             return Some(self.advance());
//         }
//         println!("Error: Expected {:?}, got {:?}", expected, self.peek().tokentype);
//         None
//     }
// }
