use std::{error::Error, fmt};

use thiserror::Error;

use crate::grammar::Span;

/// A compiler error with location information
#[derive(Debug, Clone, PartialEq, Error)]
pub struct CompilerError<E>
where
    E: IntoCompilerError,
{
    /// The underlying error
    pub error: E,
    /// The source location where the error occurred
    pub span: Span,
}

impl<E> CompilerError<E>
where
    E: IntoCompilerError,
{
    pub fn new(error: E, span: Span) -> Self {
        Self { error, span }
    }

    pub fn convert_error<E2>(self) -> CompilerError<E2>
    where
        E2: IntoCompilerError + From<E>,
    {
        CompilerError {
            error: self.error.into(),
            span: self.span,
        }
    }
}

impl<E> fmt::Display for CompilerError<E>
where
    E: IntoCompilerError,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} [at {}:{}]",
            self.error, self.span.line, self.span.column
        )
    }
}

pub trait IntoCompilerError: Error + Clone + PartialEq {
    fn with_span(self, span: Span) -> CompilerError<Self> {
        CompilerError::new(self, span)
    }
}
