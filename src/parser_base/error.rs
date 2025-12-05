use crate::{
    error::{CompilerError, IntoCompilerError},
    lexer_base::{LexError, TokenType},
};
use thiserror::Error;

#[derive(Debug, Error, Clone, PartialEq)]
pub enum ParseError {
    #[error("Lexer error: {0}")]
    LexError(#[from] LexError),

    #[error("Expected {expected:?}, but found {found:?}")]
    UnexpectedToken {
        expected: String,
        found: Option<String>,
    },

    #[error("Expected any statement starter token")]
    ExpectedStatement,

    #[error("Expected identifier, but found {found:?}")]
    ExpectedIdentifier { found: Option<String> },

    #[error("Expected expression, but found end of input")]
    ExpectedExpression,

    #[error("Unexpected tokens after function definition: {tokens:?}")]
    UnexpectedTokensAfterFunction { tokens: String },

    #[error("Expected {expected:?}, but found end of input")]
    UnexpectedEndOfInput { expected: String },
}

impl ParseError {
    pub fn unexpected_token(expected: TokenType<'static>, found: Option<TokenType>) -> Self {
        ParseError::UnexpectedToken {
            expected: format!("{:?}", expected),
            found: found.map(|t| format!("{:?}", t)),
        }
    }

    pub fn expected_identifier(found: Option<TokenType>) -> Self {
        ParseError::ExpectedIdentifier {
            found: found.map(|t| format!("{:?}", t)),
        }
    }

    pub fn unexpected_end_of_input(expected: TokenType<'static>) -> Self {
        ParseError::UnexpectedEndOfInput {
            expected: format!("{:?}", expected),
        }
    }
}

impl IntoCompilerError for ParseError {}
pub type CompilerParseError = CompilerError<ParseError>;
