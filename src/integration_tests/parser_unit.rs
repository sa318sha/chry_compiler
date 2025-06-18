// // use crate::parser::Parser;
// // use crate::scanner::Scanner;

// #[cfg(test)]

// mod tests {
//     use super::*;

//     fn parse(source_str: &str) {
//         let mut scanner = Scanner::new(source_str);
//         let tokens: Vec<Token> = scanner.scan_tokens();

//         scanner.list_tokens();

//         let mut parser = Parser::new(tokens);

//         let Stmts = parser.parse_test();
//     }

//     #[test]
//     fn parse_simper_var() {
//         let input = "var input = 4 +3";
//     }
// }
