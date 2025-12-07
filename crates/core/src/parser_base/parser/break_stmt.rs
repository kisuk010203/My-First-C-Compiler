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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer_base::Lexer;

    fn parse_break(input: &str) -> Result<BreakStmt<'_>, CompilerParseError> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser.parse_break_statement()
    }

    #[test]
    fn test_parse_break() {
        let result = parse_break("break;");
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_break_error_missing_semicolon() {
        let result = parse_break("break");
        assert!(result.is_err());
    }
}
