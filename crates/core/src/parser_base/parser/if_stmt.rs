use crate::{
    grammar::IfStmt,
    parser_base::{CompilerParseError, Parser},
    t,
};

impl<'a> Parser<'a> {
    /// Parse an if statement: if (condition) { body } else { body }
    pub(super) fn parse_if_statement(&mut self) -> Result<IfStmt<'a>, CompilerParseError> {
        self.expect(t!("if"))?;
        self.expect(t!("("))?;
        let condition = self.parse_expression()?;
        self.expect(t!(")"))?;
        let then_block = self.parse_statement()?;
        let else_block = self
            .expect_optional(t!("else"))?
            .map(|_| self.parse_statement())
            .transpose()?;

        Ok(IfStmt {
            cond: condition,
            then_block: Box::new(then_block),
            else_block: else_block.map(Box::new),
        })
    }
}
