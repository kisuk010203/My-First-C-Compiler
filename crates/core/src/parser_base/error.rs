use thiserror::Error;

use crate::{
    error::{CompilerError, IntoCompilerError},
    lexer_base::{LexError, TokenType},
};

#[derive(Debug, Error, Clone, PartialEq)]
pub enum ParseError {
    #[error("Lexer error: {0}")]
    LexError(#[from] LexError),

    /// Expected a specific token, but found something else
    #[error("Expected {expected}, but found {found}")]
    UnexpectedToken { expected: String, found: String },

    /// Expected a specific token, but reached end of input
    #[error("Expected {expected}, but reached end of file")]
    UnexpectedEof { expected: String },
}

impl ParseError {
    /// Create an error for when we expected a specific token but found another
    pub fn unexpected_token(expected: TokenType<'static>, found: TokenType) -> Self {
        ParseError::UnexpectedToken {
            expected: format!("{:?}", expected),
            found: format!("{:?}", found),
        }
    }

    /// Create an error for when we expected a specific token but hit EOF
    pub fn unexpected_eof(expected: TokenType<'static>) -> Self {
        ParseError::UnexpectedEof {
            expected: format!("{:?}", expected),
        }
    }

    /// Helper for expecting an identifier specifically
    pub fn expected_identifier(found: TokenType) -> Self {
        ParseError::UnexpectedToken {
            expected: "identifier".to_string(),
            found: format!("{:?}", found),
        }
    }

    /// Helper for expecting an identifier but hit EOF
    pub fn expected_identifier_eof() -> Self {
        ParseError::UnexpectedEof {
            expected: "identifier".to_string(),
        }
    }

    pub fn expected_type(found: TokenType) -> Self {
        ParseError::UnexpectedToken {
            expected: "type".to_string(),
            found: format!("{:?}", found),
        }
    }

    pub fn expected_type_eof() -> Self {
        ParseError::UnexpectedEof {
            expected: "type".to_string(),
        }
    }

    /// Helper for expecting an expression
    pub fn expected_expression(found: TokenType) -> Self {
        ParseError::UnexpectedToken {
            expected: "expression".to_string(),
            found: format!("{:?}", found),
        }
    }

    /// Helper for expecting an expression but hit EOF
    pub fn expected_expression_eof() -> Self {
        ParseError::UnexpectedEof {
            expected: "expression".to_string(),
        }
    }

    /// Helper for expecting a statement
    pub fn expected_statement(found: TokenType) -> Self {
        ParseError::UnexpectedToken {
            expected: "statement".to_string(),
            found: format!("{:?}", found),
        }
    }

    /// Helper for expecting a statement but hit EOF
    pub fn expected_statement_eof() -> Self {
        ParseError::UnexpectedEof {
            expected: "statement".to_string(),
        }
    }

    /// Helper for generic expected string
    pub fn unexpected_eof_with_message(expected: impl Into<String>) -> Self {
        ParseError::UnexpectedEof {
            expected: expected.into(),
        }
    }
}

impl IntoCompilerError for ParseError {}
pub type CompilerParseError = CompilerError<ParseError>;
