// use crate::log_warn;
// use chry_compiler::log_error;
use crate::types::token::Token;
use crate::types::types::Type;

/// Main HIR error type
#[derive(Debug, Clone)]
pub enum HIRError {
    /// Attempt to use an undeclared variable
    UndefinedVariable {
        name: String,
        token: Token,
    },

    /// Type mismatch in an operation
    TypeMismatch {
        expected: Type,
        found: Type,
        context: String, // e.g., "in binary op +"
        token: Token,
    },

    ExpectedReturn {
        found: Type,
        name: Token,
    },

    /// Invalid operation for type (e.g. applying + to string)
    InvalidOperation {
        op: String,
        ty: Type,
        token: Token,
    },

    /// Calling a non-function
    NotCallable {
        callee_ty: Type,
        token: Token,
    },

    /// Arity mismatch: wrong number of arguments in a call
    ArityMismatch {
        expected: usize,
        found: usize,
        func_name: String,
        token: Token,
    },

    /// Control flow error: break/continue outside of loop
    MisplacedControlFlow {
        kind: String, // "break", "continue"
        token: Token,
    },

    /// Duplicate definition (e.g. variable or function)
    DuplicateDefinition {
        name: String,
        token: Token,
    },

    /// Internal error: something went wrong lowering HIR
    InternalError {
        message: String,
    },

    InvalidGlobalStatement {
        message: String,
    }
}
pub fn log_hir_error(error: &HIRError) {
    match error {
        HIRError::ExpectedReturn { found, name } => {
            println!(
                "{}: Expected Return in Non Void Function {} instead found type '{:?}'.",
                name.line, name.lexeme, found
            );
        }
        HIRError::UndefinedVariable { name, token } => {
            println!("{}: Undefined variable '{}'.", token.line, name);
        }
        HIRError::InvalidGlobalStatement { message } => {
            println!("{}", message)
        }
        HIRError::TypeMismatch {
            expected,
            found,
            context,
            token,
        } => {
            println!(
                "{}: Type mismatch: expected '{:?}', found '{:?}' {}.",
                token.line, expected, found, context
            );
        }
        HIRError::InvalidOperation { op, ty, token } => {
            println!(
                "{}: Invalid operation: cannot apply '{}' to type '{:?}'.",
                token.line, op, ty
            );
        }
        HIRError::NotCallable { callee_ty, token } => {
            println!("{}: Type '{:?}' is not callable.", token.line, callee_ty);
        }
        HIRError::ArityMismatch {
            expected,
            found,
            func_name,
            token,
        } => {
            println!(
                "{}: Arity mismatch in call to '{}': expected {} arguments, found {}.",
                token.line, func_name, expected, found
            );
        }
        HIRError::MisplacedControlFlow { kind, token } => {
            println!("{}: '{}' used outside of loop.", token.line, kind);
        }
        HIRError::DuplicateDefinition { name, token } => {
            println!("{}: Duplicate definition of '{}'.", token.line, name);
        }
        HIRError::InternalError { message } => {
            println!("???: Internal error: {}", message);
        }
    }
}
