use std::borrow::Cow;
use std::iter::Peekable;

use crate::lexer_base::{
    error::LexError,
    lexer::Lexer,
    token::{Token, TokenType},
};
use crate::parser_base::{error::ParseError, grammar::*};
use crate::t;

#[derive(Clone)]
pub struct Parser<'a, I>
where
    I: Iterator<Item = Result<Token<'a>, LexError>>,
{
    lexer: Peekable<I>,
}

impl<'a> Parser<'a, Lexer<'a>> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            lexer: lexer.peekable(),
        }
    }
}

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Result<Token<'a>, LexError>>,
{
    pub fn parse(mut self) -> Result<Program<'a>, ParseError> {
        let fd = self.parse_function_def()?;
        let remaining_tokens = self.lexer.collect::<Result<Vec<Token<'a>>, LexError>>()?;
        if !remaining_tokens.is_empty() {
            Err(ParseError::UnexpectedTokensAfterFunction {
                tokens: format!("{:?}", remaining_tokens),
            })
        } else {
            Ok(Program::Program(fd))
        }
    }

    pub fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        match self.lexer.next().transpose()? {
            Some(token) if matches!(token.kind, TokenType::Constant(_)) => {
                if let TokenType::Constant(value) = token.kind {
                    let expr = Expression::Constant(value);
                    Ok(expr)
                } else {
                    unreachable!()
                }
            }
            Some(token) => Err(ParseError::UnexpectedToken {
                expected: "expression starter token".to_string(),
                found: Some(format!("{:?}", token.kind)),
            }),
            None => Err(ParseError::ExpectedExpression),
        }
    }

    pub fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        self.expect(t!("return"))?;
        let inner = self.parse_expression()?;
        self.expect(t!(";"))?;
        Ok(Statement::Return(inner))
    }

    pub fn parse_function_def(&mut self) -> Result<FuncDef<'a>, ParseError> {
        self.expect(t!("int"))?;
        let func_name = self.expect_identifier()?;
        self.expect(t!("("))?;
        self.expect_optional(t!("void"))?;
        self.expect(t!(")"))?;
        self.expect(t!("{"))?;
        let body = self.parse_statement()?;
        self.expect(t!("}"))?;

        Ok(FuncDef::Fn(func_name, body))
    }

    fn expect(&mut self, expected: TokenType<'static>) -> Result<(), ParseError> {
        match self.lexer.next().transpose()? {
            Some(token) if token.kind == expected => Ok(()),
            Some(unexpected) => Err(ParseError::unexpected_token(
                expected,
                Some(unexpected.kind),
            )),
            None => Err(ParseError::unexpected_end_of_input(expected)),
        }
    }

    fn expect_identifier(&mut self) -> Result<Cow<'a, str>, ParseError> {
        match self.lexer.next().transpose()? {
            Some(token) if matches!(token.kind, TokenType::Identifier(_)) => {
                if let TokenType::Identifier(name) = token.kind {
                    Ok(name)
                } else {
                    unreachable!()
                }
            }
            Some(token) => Err(ParseError::expected_identifier(Some(token.kind))),
            None => Err(ParseError::expected_identifier(None)),
        }
    }

    fn expect_optional(&mut self, expected: TokenType<'static>) -> Result<bool, ParseError> {
        match self.lexer.peek() {
            Some(Ok(token)) if token.kind == expected => {
                self.lexer.next(); // Consume it
                Ok(true)
            }
            Some(Ok(_)) => {
                // Token doesn't match, don't consume
                Ok(false)
            }
            Some(Err(_)) => {
                // Consume and return the error
                match self.lexer.next() {
                    Some(Err(err)) => Err(err.into()),
                    _ => unreachable!("next and peek should return same result"),
                }
            }
            None => Ok(false), // End of input
        }
    }
}
