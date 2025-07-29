use crate::types::{token::Token, token_type::TokenType, types::Type};

#[derive(Debug)]
pub enum TypeError {
    // Variable and declaration errors
    Redeclaration(Token, String),
    UndeclaredVariable(Token, String),

    //control flow errors
    ControlCondition {
        // token: Token,
        found: Type,
        context: String,
    },

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

    NoReturnType {
        expected: Type,
        name: Token,
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

impl TypeError {
    pub fn error_report(&self) {
        match &self {
            TypeError::NoReturnType { expected, name } => {
                eprintln!(
                    "[line {}] Error: missing return type {:?}, expected in function {:?}",
                    name.line, expected, name.lexeme
                );
            }
            TypeError::ControlCondition { found, context } => {
                eprintln!(
                    "Error: expected boolean condition, found {:?} in control statement: '{}'",
                    found, context
                );
            }

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
}
