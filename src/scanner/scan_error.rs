#[derive(Debug, Clone, PartialEq)]
pub enum ScanError {
    UnexpectedCharacter {
        character: char,
        line: u32,
        context: String,
    },

    NumericParseError {
        value: String,
        line: u32,
        context: String,
    },
    UnterminatedString {
        value: String,
        line: u32,
        context: String,
    },
    InvalidCharacterStream {
        line: u32,
        context: String,
    },
}

impl ScanError {
    pub fn format_log(&self) -> String {
        match self {
            ScanError::UnexpectedCharacter {
                character,
                line,
                context,
            } => {
                format!(
                    "[line {}] Unexpected character '{}'. Context: '{}'",
                    line, character, context
                )
            }
            ScanError::NumericParseError {
                value,
                line,
                context,
            } => {
                format!(
                    "[line {}] Failed to scan numeric value '{}'. Context: '{}'",
                    line, value, context
                )
            }
            ScanError::UnterminatedString {
                value,
                line,
                context,
            } => {
                format!(
                    "[line {}] Unterminated string '{}'. Context: '{}'",
                    line, value, context
                )
            }
            ScanError::InvalidCharacterStream { line, context } => {
                format!(
                    "[line {}] Invalid character stream. Context: '{}'",
                    line, context
                )
            }
        }
    }
}
