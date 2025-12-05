use crate::lexer_base::{error::LexError, token::Token};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("Lexer error: {0}")]
    LexError(#[from] LexError),

    #[error("Expected {expected:?}, but found {found:?}")]
    UnexpectedToken {
        expected: String,
        found: Option<String>,
    },

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
    pub fn unexpected_token(expected: Token<'static>, found: Option<Token>) -> Self {
        ParseError::UnexpectedToken {
            expected: format!("{:?}", expected),
            found: found.map(|t| format!("{:?}", t)),
        }
    }

    pub fn expected_identifier(found: Option<Token>) -> Self {
        ParseError::ExpectedIdentifier {
            found: found.map(|t| format!("{:?}", t)),
        }
    }

    pub fn unexpected_end_of_input(expected: Token<'static>) -> Self {
        ParseError::UnexpectedEndOfInput {
            expected: format!("{:?}", expected),
        }
    }
}
