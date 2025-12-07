use crate::{
    error::IntoCompilerError,
    grammar::*,
    parser_base::{CompilerParseError, ParseError},
    t,
};

use super::Parser;

impl<'a> Parser<'a> {
    /// Parse an expression
    pub(super) fn parse_expression(&mut self) -> Result<Expression<'a>, CompilerParseError> {
        let base = self.parse_unit_expression()?;
        self.parse_infix_operator(base, 0)
    }

    pub(super) fn parse_unit_expression(&mut self) -> Result<Expression<'a>, CompilerParseError> {
        match self.peek_token()? {
            Some(Token { kind: t!("("), .. }) => self.parse_grouped_expression(),
            Some(Token {
                kind: TokenType::Constant(value),
                ..
            }) => {
                self.next_token()?;
                Ok(Expression::Constant(value))
            }
            Some(Token {
                kind: TokenType::Identifier(name),
                ..
            }) => {
                self.next_token()?;
                Ok(Expression::Variable(name.clone()))
            }
            Some(Token { kind: t!("-"), .. }) => {
                self.next_token()?;
                Ok(Expression::Unary {
                    op: UnaryOp::Negate,
                    expr: Box::new(self.parse_unit_expression()?),
                })
            }
            Some(token) => Err(ParseError::expected_expression(token.kind).with_span(token.span)),
            None => Err(ParseError::expected_expression_eof().with_span(self.eof_span)),
        }
    }

    pub(super) fn parse_grouped_expression(
        &mut self,
    ) -> Result<Expression<'a>, CompilerParseError> {
        self.expect(t!("("))?;
        let expr = self.parse_expression()?;
        self.expect(t!(")"))?;
        Ok(Expression::Grouped(Box::new(expr)))
    }

    pub(super) fn parse_infix_operator(
        &mut self,
        mut lhs: Expression<'a>,
        min_bp: u8,
    ) -> Result<Expression<'a>, CompilerParseError> {
        while let Some(op) = self
            .peek_token()?
            .and_then(|t| BinaryOp::from_token_type(&t.kind))
        {
            let (left_bp, right_bp) = op.infix_binding_power();

            // If the binding power is too low, stop
            if left_bp < min_bp {
                break;
            }

            // Consume the operator token
            self.next_token()?;

            let rhs = self.parse_unit_expression()?;
            let rhs = self.parse_infix_operator(rhs, right_bp)?;

            lhs = Expression::Binary {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            };
        }

        Ok(lhs)
    }
}
