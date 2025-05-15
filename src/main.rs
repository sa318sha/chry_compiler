use std::env;

mod tokenType;

use tokenType::TokenType;

fn main() {

    let args: Vec<String> = env::args().collect();

    // args[0] is the program name
    let filename = args.get(1); // Returns Option<&String>
    match filename {
        Some(f) => runFile(f),
        None => runFile("basicFile"),
    }
}


fn runFile (a: &str){
    println!("{}", a);
}