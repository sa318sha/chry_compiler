use std::env;
use std::fs;

// mod error;

// mod parser;
// mod scanner;

// mod type_checker;
// mod types;
// mod

// use chry_compiler::parser; // ← assuming crate name = chry_compiler
// use chry_compiler::Scanner;
// use crate::types::token::Token;
// use chry_compiler::TypeChecker;
mod parser;
use crate::parser::parser::Parser;

mod scanner;
use crate::scanner::scanner::Scanner;

mod type_checker;
use crate::type_checker::type_checker::TypeChecker;

mod types;
use crate::types::token::Token;

const SCAN_LEVEL: i32 = 1;
const PARSE_LEVEL: i32 = 2;
const CHECK_LEVEL: i32 = 3;

// static  mut DEBUG: bool = false;
const DEBUG: bool = true;
fn main() {
    let args: Vec<String> = env::args().collect();

    // args[0] is the program name
    let filename = args.get(1); // Returns Option<&String>
    let depth = args.get(2);

    // let depth = 55;
    let level;
    match depth {
        Some(f) => {
            level = parse_depth(f);
        }
        None => level = 99,
    }
    match filename {
        Some(f) => run_file(f, level),
        None => run_file("simpleScannerTokens.chry", level),
    }
}

fn parse_depth(depth: &str) -> i32 {
    match depth {
        "scan" => {
            println!("scanning only");
            return SCAN_LEVEL;
        }
        "parse" => {
            return PARSE_LEVEL;
        }
        "check" => {
            return CHECK_LEVEL;
        }
        _ => {
            return 99;
        }
    }
}

fn run_file(a: &str, level: i32) {
    // fs::read_to_string(a);

    match fs::read_to_string(a) {
        Ok(contents) => run(&contents, level),
        Err(e) => eprintln!("Failed to read file: {}", e),
    }
    // run(a);
}

fn run(source_str: &str, level: i32) {
    if level >= SCAN_LEVEL {
        let mut scanner = Scanner::new(source_str);
        let tokens: Vec<Token> = scanner.scan_tokens();

        if DEBUG {
            println!("scanning");

            scanner.list_tokens();
        }

        if level >= PARSE_LEVEL {
            let mut parser = Parser::new(tokens);

            let stmts = parser.parse();

            if DEBUG {
                println!("parsing");

                parser.print_parser();
            }

            if level >= CHECK_LEVEL {
                let mut type_checker = TypeChecker::new();

                type_checker.check(&stmts);

                if DEBUG {
                    println!("type checking");
                }
            }
        }
    }
}
