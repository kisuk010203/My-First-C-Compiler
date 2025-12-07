use std::borrow::Cow;

use crate::{
    error::IntoCompilerError,
    grammar::*,
    parser_base::{CompilerParseError, ParseError},
};

use super::Parser;

impl<'a> Parser<'a> {
    /// Expect a specific token
    pub(super) fn expect(
        &mut self,
        expected: TokenType<'static>,
    ) -> Result<(), CompilerParseError> {
        match self.next_token()? {
            Some(token) if token.kind == expected => Ok(()),
            Some(unexpected) => {
                Err(ParseError::unexpected_token(expected, unexpected.kind)
                    .with_span(unexpected.span))
            }
            None => Err(ParseError::unexpected_eof(expected).with_span(self.eof_span)),
        }
    }

    /// Expect an identifier token
    pub(super) fn expect_identifier(&mut self) -> Result<Cow<'a, str>, CompilerParseError> {
        match self.next_token()? {
            Some(Token {
                kind: TokenType::Identifier(name),
                ..
            }) => Ok(name),
            Some(Token { kind, span }) => {
                Err(ParseError::expected_identifier(kind).with_span(span))
            }
            None => Err(ParseError::expected_identifier_eof().with_span(self.eof_span)),
        }
    }

    /// Optionally consume a token if it matches
    pub(super) fn expect_optional(
        &mut self,
        expected: TokenType<'static>,
    ) -> Result<Option<Token<'a>>, CompilerParseError> {
        match self.peek_token()? {
            Some(token) if token.kind == expected => {
                self.next_token()?;
                Ok(Some(token))
            }
            _ => Ok(None),
        }
    }

    pub(super) fn expect_type(&mut self) -> Result<Type, CompilerParseError> {
        match self.peek_token()? {
            Some(token) => {
                self.next_token()?;
                Type::from_token_type(&token.kind)
                    .ok_or(ParseError::expected_type(token.kind).with_span(token.span))
            }
            None => Err(ParseError::expected_type_eof().with_span(self.eof_span)),
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
}
