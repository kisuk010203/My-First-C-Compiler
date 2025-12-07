use crate::{
    grammar::*,
    parser_base::{CompilerParseError, Parser},
    t,
};

impl<'a> Parser<'a> {
    pub(super) fn parse_for_statement(&mut self) -> Result<ForStmt<'a>, CompilerParseError> {
        self.expect_multiple([t!("for"), t!("(")])?;

        // Parse init expression (optional)
        let init = match self.peek_token()? {
            Some(Token { kind: t!(";"), .. }) => None,
            Some(_) => Some(self.parse_expression()?),
            None => None,
        };
        self.expect(t!(";"))?;

        // Parse condition expression (optional)
        let cond = match self.peek_token()? {
            Some(Token { kind: t!(";"), .. }) => None,
            Some(_) => Some(self.parse_expression()?),
            None => None,
        };
        self.expect(t!(";"))?;

        // Parse post expression (optional)
        let post = match self.peek_token()? {
            Some(Token { kind: t!(")"), .. }) => None,
            Some(_) => Some(self.parse_expression()?),
            None => None,
        };
        self.expect(t!(")"))?;

        let body = self.parse_statement()?;

        Ok(ForStmt {
            init,
            cond,
            post,
            body: Box::new(body),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer_base::Lexer;

    fn parse_for(input: &str) -> Result<ForStmt<'_>, CompilerParseError> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser.parse_for_statement()
    }

    // === For Statement Tests ===

    #[test]
    fn test_parse_for_full() {
        let result = parse_for("for (i; i < 10; i) { x; }");
        assert!(result.is_ok());
        let stmt = result.unwrap();
        assert!(stmt.init.is_some());
        assert!(stmt.cond.is_some());
        assert!(stmt.post.is_some());
        assert!(matches!(*stmt.body, Statement::Block(_)));
    }

    #[test]
    fn test_parse_for_empty_clauses() {
        let result = parse_for("for (;;) { x; }");
        assert!(result.is_ok());
        let stmt = result.unwrap();
        assert!(stmt.init.is_none());
        assert!(stmt.cond.is_none());
        assert!(stmt.post.is_none());
        assert!(matches!(*stmt.body, Statement::Block(_)));
    }

    #[test]
    fn test_parse_for_only_condition() {
        let result = parse_for("for (; i < 10;) { x; }");
        assert!(result.is_ok());
        let stmt = result.unwrap();
        assert!(stmt.init.is_none());
        assert!(stmt.cond.is_some());
        assert!(stmt.post.is_none());
    }

    #[test]
    fn test_parse_for_only_init() {
        let result = parse_for("for (i;;) { x; }");
        assert!(result.is_ok());
        let stmt = result.unwrap();
        assert!(stmt.init.is_some());
        assert!(stmt.cond.is_none());
        assert!(stmt.post.is_none());
    }

    #[test]
    fn test_parse_for_only_post() {
        let result = parse_for("for (;; i) { x; }");
        assert!(result.is_ok());
        let stmt = result.unwrap();
        assert!(stmt.init.is_none());
        assert!(stmt.cond.is_none());
        assert!(stmt.post.is_some());
    }

    #[test]
    fn test_parse_for_with_single_statement() {
        let result = parse_for("for (;;) x;");
        assert!(result.is_ok());
        let stmt = result.unwrap();
        assert!(matches!(*stmt.body, Statement::Expr(_)));
    }

    #[test]
    fn test_parse_for_nested() {
        let result = parse_for("for (;;) for (;;) { x; }");
        assert!(result.is_ok());
        let stmt = result.unwrap();
        match *stmt.body {
            Statement::For(_) => {}
            _ => panic!("Expected nested for statement"),
        }
    }

    #[test]
    fn test_parse_for_error_missing_paren() {
        let result = parse_for("for ;;) { x; }");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_for_error_missing_semicolons() {
        let result = parse_for("for (i i i) { x; }");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_for_error_missing_body() {
        let result = parse_for("for (;;)");
        assert!(result.is_err());
    }
}
