use crate::{
    grammar::ReturnStmt,
    parser_base::{CompilerParseError, Parser},
    t,
};

impl<'a> Parser<'a> {
    /// Parse a return statement: return expr;
    pub(super) fn parse_return_statement(&mut self) -> Result<ReturnStmt<'a>, CompilerParseError> {
        self.expect(t!("return"))?;
        let expr = self.parse_expression()?;
        self.expect(t!(";"))?;
        Ok(ReturnStmt { expr })
    }
}
