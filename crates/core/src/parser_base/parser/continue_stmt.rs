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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer_base::Lexer;

    fn parse_continue(input: &str) -> Result<ContinueStmt<'_>, CompilerParseError> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser.parse_continue_statement()
    }

    #[test]
    fn test_parse_continue() {
        let result = parse_continue("continue;");
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_continue_error_missing_semicolon() {
        let result = parse_continue("continue");
        assert!(result.is_err());
    }
}
