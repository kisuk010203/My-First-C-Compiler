use std::borrow::Cow;
use std::iter::Peekable;

use crate::{
    error::IntoCompilerError,
    lexer_base::{CompilerLexError, Lexer, Span, Token, TokenType},
    parser_base::{Block, CompilerParseError, Expression, FuncDef, ParseError, Program, Statement},
    t,
};

#[derive(Clone)]
pub struct Parser<'a, I>
where
    I: Iterator<Item = Result<Token<'a>, CompilerLexError>>,
{
    lexer: Peekable<I>,
    eof_span: Span,
}

impl<'a> Parser<'a, Lexer<'a>> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let eof_span = lexer.eof_span();
        Self {
            lexer: lexer.peekable(),
            eof_span,
        }
    }
}

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Result<Token<'a>, CompilerLexError>>,
{
    /// Parse a complete program (one or more function definitions)
    pub fn parse(mut self) -> Result<Program<'a>, CompilerParseError> {
        let mut functions = Vec::new();

        // Parse all functions until EOF
        while self.lexer.peek().is_some() {
            functions.push(self.parse_function()?);
        }

        if functions.is_empty() {
            return Err(ParseError::UnexpectedEndOfInput {
                expected: "function definition".to_string(),
            }
            .with_span(self.eof_span));
        }

        Ok(Program { functions })
    }

    /// Parse a function definition: int name(void) { ... }
    fn parse_function(&mut self) -> Result<FuncDef<'a>, CompilerParseError> {
        // Expect return type (currently only "int")
        self.expect(t!("int"))?;

        // Function name
        let name = self.expect_identifier()?;

        // Parameters
        self.expect(t!("("))?;
        self.expect_optional(t!("void"))?; // For now, only void params
        self.expect(t!(")"))?;

        // Function body (block)
        let body = self.parse_block()?;

        Ok(FuncDef {
            name,
            params: Vec::new(),
            body,
        })
    }

    /// Parse a block: { statement* }
    fn parse_block(&mut self) -> Result<Block<'a>, CompilerParseError> {
        self.expect(t!("{"))?;

        let mut statements = Vec::new();

        // Parse statements until we hit '}'
        loop {
            match self.lexer.peek() {
                Some(Ok(Token { kind: t!("}"), .. })) => break,
                Some(Err(err)) => return Err(err.clone().convert_error::<ParseError>()),
                Some(Ok(_)) => {
                    statements.push(self.parse_statement()?);
                }
                None => {
                    return Err(ParseError::UnexpectedEndOfInput {
                        expected: "'}' or statement".to_string(),
                    }
                    .with_span(self.eof_span));
                }
            }
        }

        self.expect(t!("}"))?;

        Ok(Block { statements })
    }

    /// Parse a statement
    fn parse_statement(&mut self) -> Result<Statement<'a>, CompilerParseError> {
        match self
            .lexer
            .peek()
            .map(|t| t.as_ref())
            .transpose()
            .map_err(|e| e.clone().convert_error())?
        {
            Some(Token {
                kind: t!("return"), ..
            }) => self.parse_return_statement(),
            Some(token) => Err(ParseError::UnexpectedToken {
                expected: "statement".to_string(),
                found: Some(format!("{:?}", token.kind)),
            }
            .with_span(token.span)),
            None => Err(ParseError::UnexpectedEndOfInput {
                expected: "statement".to_string(),
            }
            .with_span(self.eof_span)),
        }
    }

    /// Parse a return statement: return expr;
    fn parse_return_statement(&mut self) -> Result<Statement<'a>, CompilerParseError> {
        self.expect(t!("return"))?;
        let expr = self.parse_expression()?;
        self.expect(t!(";"))?;
        Ok(Statement::Return { expr })
    }

    /// Parse an expression
    fn parse_expression(&mut self) -> Result<Expression<'a>, CompilerParseError> {
        match self
            .lexer
            .next()
            .transpose()
            .map_err(|e| e.convert_error::<ParseError>())?
        {
            Some(Token {
                kind: TokenType::Constant(value),
                ..
            }) => Ok(Expression::Constant(value)),
            Some(Token {
                kind: TokenType::Identifier(name),
                ..
            }) => Ok(Expression::Variable(name)),
            Some(token) => Err(ParseError::UnexpectedToken {
                expected: "expression".to_string(),
                found: Some(format!("{:?}", token.kind)),
            }
            .with_span(token.span)),
            None => Err(ParseError::UnexpectedEndOfInput {
                expected: "expression".to_string(),
            }
            .with_span(self.eof_span)),
        }
    }

    /// Expect a specific token
    fn expect(&mut self, expected: TokenType<'static>) -> Result<(), CompilerParseError> {
        match self
            .lexer
            .next()
            .transpose()
            .map_err(|e| e.convert_error())?
        {
            Some(token) if token.kind == expected => Ok(()),
            Some(unexpected) => Err(
                ParseError::unexpected_token(expected, Some(unexpected.kind))
                    .with_span(unexpected.span),
            ),
            None => Err(ParseError::unexpected_end_of_input(expected).with_span(self.eof_span)),
        }
    }

    /// Expect an identifier token
    fn expect_identifier(&mut self) -> Result<Cow<'a, str>, CompilerParseError> {
        match self
            .lexer
            .next()
            .transpose()
            .map_err(|e| e.convert_error::<ParseError>())?
        {
            Some(token) if matches!(token.kind, TokenType::Identifier(_)) => {
                if let TokenType::Identifier(name) = token.kind {
                    Ok(name)
                } else {
                    unreachable!()
                }
            }
            Some(token) => {
                Err(ParseError::expected_identifier(Some(token.kind)).with_span(token.span))
            }
            None => Err(ParseError::expected_identifier(None).with_span(self.eof_span)),
        }
    }

    /// Optionally consume a token if it matches
    fn expect_optional(
        &mut self,
        expected: TokenType<'static>,
    ) -> Result<bool, CompilerParseError> {
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
                    Some(Err(err)) => Err(err.convert_error::<ParseError>()),
                    _ => unreachable!("next and peek should return same result"),
                }
            }
            None => Ok(false), // End of input
        }
    }
}
