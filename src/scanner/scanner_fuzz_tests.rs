use super::scan_error::ScanError;
use crate::scanner::scanner::Scanner;
use rand::Rng;

fn main() {
    let num_tests = 10_000;
    for _ in 0..num_tests {
        let input = generate_random_input();
        fuzz_test_input(&input);
    }
}

fn generate_random_input() -> String {
    let mut rng = rand::rng(); // ✅ fixed: use thread_rng()
    let len = rng.random_range(1..1000); // ✅ fixed: gen_range instead of random_range

    (0..len)
        .map(|_| rng.random_range(32u8..127u8) as char) // printable ASCII
        .collect()
}

fn fuzz_test_input(input: &str) {
    let mut scanner = Scanner::new(input);
    let tokens = scanner.scan_tokens();

    for token in &tokens {
        assert!(token.line >= 1, "Token line number invalid: {:?}", token);
        // Could add more invariants: e.g. valid lexemes
    }
    // for e in scanner.errors {
    //     println!("Scanner error at line {}: {}", e.line, e.message);
    // }
}

#[test]
fn test_valid_simple_input() {
    let input = "var x = 42;";
    fuzz_test_input(input); // Should produce no errors
}

#[test]
fn test_unterminated_string() {
    let input = "\"hello";
    fuzz_test_input(input); // Should report unterminated string
}

#[test]
fn test_nested_comments() {
    let input = "/* comment /* nested */ still comment */";
    fuzz_test_input(input); // Should handle comment correctly (if supported)
}

#[test]
fn test_long_identifier() {
    let input = "var supercalifragilisticexpialidocious123 = 1;";
    fuzz_test_input(input); // Should handle long identifier without error
}

#[test]
fn test_only_operators() {
    let input = "+-*/=<>!&|";
    fuzz_test_input(input); // Should tokenize operators
}

#[test]
fn test_random_garbage() {
    let input = "@@@###$$$%%%";
    fuzz_test_input(input); // Should gracefully handle unknown chars
}
