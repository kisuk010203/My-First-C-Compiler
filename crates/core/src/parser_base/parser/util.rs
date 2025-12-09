use super::Parser;
use crate::{
    error::IntoCompilerError,
    grammar::*,
    parser_base::{CompilerParseError, ParseError},
};

impl<'a> Parser<'a> {
    /// Expect a token with specific kind
    pub(super) fn expect_token(
        &mut self,
        expected: TokenType<'static>,
    ) -> Result<(), CompilerParseError> {
        match self.next_token()? {
            Some(token) if token.kind == expected => Ok(()),
            else_peek_result => {
                let tt = else_peek_result.map(|s| s.kind);
                Err(ParseError::unexpected("unary operator", tt.as_ref()).with_span(self.eof_span))
            }
        }
    }

    /// Expect multiple specific tokens in sequence
    pub(super) fn expect_sequence_of_tokens<I>(
        &mut self,
        expected: I,
    ) -> Result<(), CompilerParseError>
    where
        I: IntoIterator<Item = TokenType<'static>>,
    {
        for token_type in expected.into_iter() {
            self.expect_token(token_type)?;
        }
        Ok(())
    }

    /// Consumes the next token and attempts to convert it using the provided checker.
    ///
    /// This method peeks the upcoming token, advances the cursor, and passes the
    /// token kind to `checker`. If the checker returns `Some(_)`, the value is
    /// returned. If the checker returns `None`, an `UnexpectedToken` error is raised
    /// with the given `description`.
    ///
    /// # Errors
    ///
    /// Returns a `ParseError::UnexpectedToken` if the next token does not match the
    /// expected condition.
    /// Returns a `ParseError::UnexpectedEof` if no more tokens are available.
    ///
    /// # Parameters
    ///
    /// - `checker`: A function that receives a `TokenType` and returns `Option<R>`.
    /// - `description`: A human-readable description used to construct the error
    ///   message (e.g. `"type"`, `"identifier"`, etc.)
    ///
    /// # Example
    ///
    /// ```ignore
    /// self.expect_with(Type::from_token_type, "type")
    /// ```
    pub(super) fn expect_with<F, R>(
        &mut self,
        try_checker: F,
        description: &str,
    ) -> Result<R, CompilerParseError>
    where
        F: FnOnce(&TokenType<'a>) -> Option<R>,
    {
        match self.peek_token()? {
            Some(token) => {
                self.next_token()?;
                try_checker(&token.kind).ok_or(
                    ParseError::unexpected_token(description, &token.kind).with_span(token.span),
                )
            }
            None => Err(ParseError::unexpected_eof(description).with_span(self.eof_span)),
        }
    }

    /// Optionally consume a token if it matches
    /// and return boolean
    pub(super) fn eat(&mut self, expected: TokenType<'static>) -> Result<bool, CompilerParseError> {
        match self.peek_token()? {
            Some(token) if token.kind == expected => {
                self.next_token()?;
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    #[allow(dead_code)]
    pub(super) fn eat_when<F>(&mut self, when: F) -> Result<bool, CompilerParseError>
    where
        F: FnOnce(&TokenType<'a>) -> bool,
    {
        match self.peek_token()? {
            Some(token) if when(&token.kind) => {
                self.next_token()?;
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    /// Consumes the next token from the lexer.
    pub(super) fn next_token(&mut self) -> Result<Option<Token<'a>>, CompilerParseError> {
        self.lexer.next().transpose().map_err(|e| e.convert_error())
    }

    /// Peeks the next token from the lexer without consuming it.
    pub(super) fn peek_token(&mut self) -> Result<Option<Token<'a>>, CompilerParseError> {
        self.lexer
            .peek()
            .cloned()
            .transpose()
            .map_err(|e| e.convert_error())
    }

    pub(super) fn peek_token_type(&mut self) -> Result<Option<TokenType<'a>>, CompilerParseError> {
        Ok(self.peek_token()?.map(|token| token.kind))
    }
}
