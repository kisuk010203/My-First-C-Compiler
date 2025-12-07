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
