use thiserror::Error;

#[derive(Debug, Error, PartialEq, Eq, Clone)]
pub enum LexError {
    #[error("Unexpected character: '{0}' at position {1}")]
    UnexpectedCharacter(char, usize),

    #[error("Invalid token format: '{0}' at position {1}")]
    InvalidTokenFormat(String, usize),
}
