use crate::{
    grammar::BreakStmt,
    parser_base::{CompilerParseError, Parser},
    t,
};

impl<'a> Parser<'a> {
    pub(super) fn parse_break_statement(&mut self) -> Result<BreakStmt<'a>, CompilerParseError> {
        self.expect(t!("break"))?;
        self.expect(t!(";"))?;
        Ok(BreakStmt::new())
    }
}
