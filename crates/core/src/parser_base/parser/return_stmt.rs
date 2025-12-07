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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{grammar::Expression, lexer_base::Lexer};

    fn parse_return(input: &str) -> Result<ReturnStmt<'_>, CompilerParseError> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser.parse_return_statement()
    }

    #[test]
    fn test_parse_return_constant() {
        let result = parse_return("return 42;");
        assert!(result.is_ok());
        let stmt = result.unwrap();
        assert!(matches!(stmt.expr, Expression::Constant(42)));
    }

    #[test]
    fn test_parse_return_variable() {
        let result = parse_return("return x;");
        assert!(result.is_ok());
        let stmt = result.unwrap();
        assert!(matches!(stmt.expr, Expression::Variable(name) if name == "x"));
    }

    #[test]
    fn test_parse_return_expression() {
        let result = parse_return("return 1 + 2;");
        assert!(result.is_ok());
        let stmt = result.unwrap();
        assert!(matches!(stmt.expr, Expression::Binary { .. }));
    }

    #[test]
    fn test_parse_return_error_missing_semicolon() {
        let result = parse_return("return 0");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_return_error_missing_expression() {
        let result = parse_return("return;");
        assert!(result.is_err());
    }
}
