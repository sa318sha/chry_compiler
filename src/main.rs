use std::env;
use std::fs;

mod parser;
// use crate::lir::lir::lower_to_lir;
// use chry_compiler::log_warn;
// Define the missing constant for logging level

// use chry_compiler::lir::lir::lower_to_lir;

use crate::parser::parser::Parser;

mod scanner;
use crate::scanner::scanner::Scanner;

mod lir;
mod type_checker;
// use crate::lir::lir::lower_to_lir;
use crate::lir::lir::lower_to_lir;
use crate::ssa::ssa::lower_functions_to_ssa;
use crate::ssa::ssa::lower_to_ssa_program;
// use crate::ssa::ssa::pretty_print_ssa_funcs;
use crate::type_checker::type_checker::TypeChecker;

mod types;
use crate::types::token::Token;

mod hir;
use crate::hir::hir::HIR;

mod ssa;
use crate::ssa::ssa::SSAContext;

const SCAN_LEVEL: i32 = 1;
const PARSE_LEVEL: i32 = 2;
const CHECK_LEVEL: i32 = 3;

// pub const COMPILE_TIME_LOG_LEVEL: u8 = LOG_LEVEL_DEBUG;

// static  mut DEBUG: bool = false;
pub const DEBUG: bool = true;
fn main() {
    let args: Vec<String> = env::args().collect();

    // args[0] is the program name
    let filename = args.get(1); // Returns Option<&String>
    let depth = args.get(2);

    // let depth = 55;
    match filename {
        Some(f) => run_file(f),
        None => run_file("simpleScannerTokens.chry"),
    }
}

fn run_file(a: &str) {
    // fs::read_to_string(a);

    match fs::read_to_string(a) {
        Ok(contents) => run(&contents),
        Err(e) => println!("Failed to read file: {}", e),
    }
    // run(a);
}

fn run(source_str: &str) {
    let mut scanner = Scanner::new(source_str);
    let tokens: Vec<Token> = scanner.scan_tokens();

    let mut parser = Parser::new(tokens);

    let stmts = parser.parse();

    let mut type_checker = TypeChecker::new();

    let typed_stmts = type_checker.check(&stmts);

    let mut hir_lowerer = HIR::new();
    hir_lowerer.lower_typed_asts(&typed_stmts);

    let mut ssa_contexts = lower_functions_to_ssa(&hir_lowerer.functions);

    // let mut ssa_programs = Vec::new();
    // for i in ssa_contexts {
    //     ssa_programs.push(i.program);
    // }

    let lir_programs = lower_to_lir(ssa_contexts);

    for lir_program in lir_programs {
        println!("{}", lir_program);
    }
    // let mut x = lower_to_lir(ssa_contexts.iter().map(|f| f.program)::<Vec<_>>())
}
