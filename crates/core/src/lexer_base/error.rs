use thiserror::Error;

use crate::error::{CompilerError, IntoCompilerError};

#[derive(Debug, Error, PartialEq, Eq, Clone)]
pub enum LexError {
    #[error("Unexpected character: '{0}'")]
    UnexpectedCharacter(char),

    #[error("Invalid token format: '{0}'")]
    InvalidTokenFormat(String),
}

impl IntoCompilerError for LexError {}

pub(super) type LexResult<T> = Result<T, CompilerError<LexError>>;
