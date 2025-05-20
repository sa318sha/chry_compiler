use std::env;
use std::fs;

mod error;
mod parser;
mod scanner;
mod token;
mod token_type;
mod expr;
mod stmt;
// mod 


use crate::parser::Parser;
use crate::scanner::Scanner;
use crate::token::Token;

fn main() {
    let args: Vec<String> = env::args().collect();

    // args[0] is the program name
    let filename = args.get(1); // Returns Option<&String>
    match filename {
        Some(f) => run_file(f),
        None => run_file("simpleScannerTokens.chry"),
    }
}

fn run_file(a: &str) {
    // fs::read_to_string(a);

    match fs::read_to_string(a) {
        Ok(contents) => run(&contents),
        Err(e) => eprintln!("Failed to read file: {}", e),
    }
    // run(a);
}

fn run(source_str: &str) {
    let mut scanner = Scanner::new(source_str);
    let tokens: Vec<Token> = scanner.scan_tokens();

    scanner.list_tokens();

    let mut parser = Parser::new(tokens);

    let Stmts = parser.parse();
    // let tokens: &Vec<Token> = scanner.get_tokens();
}
