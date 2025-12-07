use crate::{
    error::IntoCompilerError,
    grammar::{BlockStmt, Token},
    parser_base::{CompilerParseError, ParseError, Parser},
    t,
};

impl<'a> Parser<'a> {
    /// Parse a block: { statement* }
    pub(super) fn parse_block_statement(&mut self) -> Result<BlockStmt<'a>, CompilerParseError> {
        self.expect(t!("{"))?;

        let mut statements = Vec::new();

        // Parse statements until we hit '}'
        loop {
            match self.peek_token()? {
                Some(Token { kind: t!("}"), .. }) => break,
                Some(_) => {
                    statements.push(self.parse_statement()?);
                }
                None => {
                    return Err(ParseError::unexpected_eof_with_message("'}' or statement")
                        .with_span(self.eof_span));
                }
            }
        }

        self.expect(t!("}"))?;
        Ok(BlockStmt { statements })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer_base::Lexer;

    fn parse_block(input: &str) -> Result<BlockStmt<'_>, CompilerParseError> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser.parse_block_statement()
    }

    // === Block Statement Tests ===

    #[test]
    fn test_parse_empty_block() {
        let result = parse_block("{}");
        assert!(result.is_ok());
        let block = result.unwrap();
        assert_eq!(block.statements.len(), 0);
    }

    #[test]
    fn test_parse_block_single_statement() {
        let result = parse_block("{ return 0; }");
        assert!(result.is_ok());
        let block = result.unwrap();
        assert_eq!(block.statements.len(), 1);
    }

    #[test]
    fn test_parse_block_multiple_statements() {
        let result = parse_block("{ x; y; z; }");
        assert!(result.is_ok());
        let block = result.unwrap();
        assert_eq!(block.statements.len(), 3);
    }

    #[test]
    fn test_parse_block_nested() {
        let result = parse_block("{ { x; } }");
        assert!(result.is_ok());
        let block = result.unwrap();
        assert_eq!(block.statements.len(), 1);
    }

    #[test]
    fn test_parse_block_error_missing_closing_brace() {
        let result = parse_block("{ x;");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_block_error_missing_opening_brace() {
        let result = parse_block("x; }");
        assert!(result.is_err());
    }
}
