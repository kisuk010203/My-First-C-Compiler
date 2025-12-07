use crate::{
    grammar::*,
    parser_base::{CompilerParseError, Parser},
    t,
};

impl<'a> Parser<'a> {
    pub(super) fn parse_do_while_statement(
        &mut self,
    ) -> Result<DoWhileStmt<'a>, CompilerParseError> {
        self.expect(t!("do"))?;
        let body = self.parse_statement()?;
        self.expect(t!("while"))?;
        self.expect(t!("("))?;
        let condition = self.parse_expression()?;
        self.expect(t!(")"))?;
        self.expect(t!(";"))?;
        Ok(DoWhileStmt {
            body: Box::new(body),
            cond: condition,
        })
    }

    pub(super) fn parse_while_statement(&mut self) -> Result<WhileStmt<'a>, CompilerParseError> {
        self.expect(t!("while"))?;
        self.expect(t!("("))?;
        let cond = self.parse_expression()?;
        self.expect(t!(")"))?;
        let body = self.parse_statement()?;
        Ok(WhileStmt {
            cond,
            body: Box::new(body),
        })
    }
}
