use std::borrow::Cow;
use std::iter::Peekable;

use crate::lexer_base::lexer::Lexer;
use crate::lexer_base::token::Token;
use crate::parser_base::grammar::*;
use crate::t;

#[derive(Clone)]
pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self { lexer: lexer.peekable() }
    }

    pub fn parse(mut self) -> Result<Program<'a>, String> {
        let fd = self.parse_function_def()?;
        let remaining_tokens = self.lexer.collect::<Result<Vec<Token<'a>>, String>>()?;
        if !remaining_tokens.is_empty() {
            Err(format!(
                "Unexpected tokens after function definition: {:?}",
                remaining_tokens
            ))
        } else {
            Ok(Program::Program(fd))
        }
    }

    pub fn parse_expression(&mut self) -> Result<Expression, String> {
        match self.lexer.next().transpose()? {
            Some(Token::Constant(value)) => {
                let expr = Expression::Constant(value);
                Ok(expr)
            }
            Some(token) => Err(format!(
                "Expected starter token of any expression, but found {:?}",
                token
            )),
            None => Err("Expected expression, but found end of input".to_string()),
        }
    }

    pub fn parse_statement(&mut self) -> Result<Statement, String> {
        self.expect(t!("return"))?;
        let inner = self.parse_expression()?;
        self.expect(t!(";"))?;
        Ok(Statement::Return(inner))
    }

    pub fn parse_function_def(&mut self) -> Result<FuncDef<'a>, String> {
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

    fn expect(&mut self, expected: Token<'static>) -> Result<(), String> {
        match self.lexer.next().transpose()? {
            Some(token) if token == expected => Ok(()),
            Some(unexpected) => Err(format!(
                "Expected {:?}, but found {:?}",
                expected, unexpected
            )),
            None => Err(format!("Expected {:?}, but found end of input", expected)),
        }
    }

    fn expect_identifier(&mut self) -> Result<Cow<'a, str>, String> {
        match self.lexer.next().transpose()? {
            Some(Token::Identifier(name)) => Ok(name),
            Some(token) => Err(format!("Expected identifier, but found {:?}", token)),
            None => Err("Expected identifier, but found end of input".to_string()),
        }
    }

    fn expect_optional(&mut self, expected: Token<'static>) -> Result<bool, String> {
        match self.lexer.peek() {
            Some(Ok(token)) if *token == expected => {
                self.lexer.next(); // Consume it
                Ok(true)
            }
            Some(Ok(_)) => {
                // Token doesn't match, don't consume
                Ok(false)
            }
            Some(Err(_)) => {
                // Consume and return the error
                match self.lexer.next().transpose()? {
                    _ => unreachable!(),
                }
            }
            None => Ok(false), // End of input
        }
    }
}
