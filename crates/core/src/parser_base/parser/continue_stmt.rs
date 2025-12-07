use crate::{
    grammar::ContinueStmt,
    parser_base::{CompilerParseError, Parser},
    t,
};

impl<'a> Parser<'a> {
    pub(super) fn parse_continue_statement(
        &mut self,
    ) -> Result<ContinueStmt<'a>, CompilerParseError> {
        self.expect(t!("continue"))?;
        self.expect(t!(";"))?;
        Ok(ContinueStmt::new())
    }
}
