use crate::{
    grammar::*,
    parser_base::{CompilerParseError, Parser},
    t,
};

impl<'a> Parser<'a> {
    pub(super) fn parse_do_while_statement(
        &mut self,
    ) -> Result<DoWhileStmt<'a>, CompilerParseError> {
        self.expect_token(t!("do"))?;
        let body = self.parse_statement()?;
        self.expect_sequence_of_tokens([t!("while"), t!("(")])?;
        let condition = self.parse_expression()?;
        self.expect_sequence_of_tokens([t!(")"), t!(";")])?;
        Ok(DoWhileStmt {
            body: Box::new(body),
            cond: condition,
        })
    }

    pub(super) fn parse_while_statement(&mut self) -> Result<WhileStmt<'a>, CompilerParseError> {
        self.expect_sequence_of_tokens([t!("while"), t!("(")])?;
        let cond = self.parse_expression()?;
        self.expect_token(t!(")"))?;
        let body = self.parse_statement()?;
        Ok(WhileStmt {
            cond,
            body: Box::new(body),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer_base::Lexer;

    fn parse_while(input: &str) -> Result<WhileStmt<'_>, CompilerParseError> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser.parse_while_statement()
    }

    fn parse_do_while(input: &str) -> Result<DoWhileStmt<'_>, CompilerParseError> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser.parse_do_while_statement()
    }

    // === While Statement Tests ===

    #[test]
    fn test_parse_while_basic() {
        let result = parse_while("while (1) { return 0; }");
        assert!(result.is_ok());
        let stmt = result.unwrap();
        assert!(matches!(stmt.cond, Expression::Constant(1)));
        assert!(matches!(*stmt.body, Statement::Block(_)));
    }

    #[test]
    fn test_parse_while_with_comparison() {
        let result = parse_while("while (x < 10) { x; }");
        assert!(result.is_ok());
        let stmt = result.unwrap();
        match stmt.cond {
            Expression::Binary { op, lhs, rhs } => {
                assert!(matches!(op, BinaryOp::LessThan));
                assert!(matches!(*lhs, Expression::Variable(name) if name == "x"));
                assert!(matches!(*rhs, Expression::Constant(10)));
            }
            _ => panic!("Expected binary comparison"),
        }
    }

    #[test]
    fn test_parse_while_with_single_statement() {
        let result = parse_while("while (1) x;");
        assert!(result.is_ok());
        let stmt = result.unwrap();
        assert!(matches!(*stmt.body, Statement::Expr(_)));
    }

    #[test]
    fn test_parse_while_nested() {
        let result = parse_while("while (1) while (2) { x; }");
        assert!(result.is_ok());
        let stmt = result.unwrap();
        match *stmt.body {
            Statement::While(_) => {}
            _ => panic!("Expected nested while statement"),
        }
    }

    #[test]
    fn test_parse_while_error_missing_paren() {
        let result = parse_while("while 1) { x; }");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_while_error_missing_closing_paren() {
        let result = parse_while("while (1 { x; }");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_while_error_missing_body() {
        let result = parse_while("while (1)");
        assert!(result.is_err());
    }

    // === Do-While Statement Tests ===

    #[test]
    fn test_parse_do_while_basic() {
        let result = parse_do_while("do { return 0; } while (1);");
        assert!(result.is_ok());
        let stmt = result.unwrap();
        assert!(matches!(stmt.cond, Expression::Constant(1)));
        assert!(matches!(*stmt.body, Statement::Block(_)));
    }

    #[test]
    fn test_parse_do_while_with_comparison() {
        let result = parse_do_while("do { x; } while (x < 10);");
        assert!(result.is_ok());
        let stmt = result.unwrap();
        match stmt.cond {
            Expression::Binary { op, lhs, rhs } => {
                assert!(matches!(op, BinaryOp::LessThan));
                assert!(matches!(*lhs, Expression::Variable(name) if name == "x"));
                assert!(matches!(*rhs, Expression::Constant(10)));
            }
            _ => panic!("Expected binary comparison"),
        }
    }

    #[test]
    fn test_parse_do_while_with_single_statement() {
        let result = parse_do_while("do x; while (1);");
        assert!(result.is_ok());
        let stmt = result.unwrap();
        assert!(matches!(*stmt.body, Statement::Expr(_)));
    }

    #[test]
    fn test_parse_do_while_nested() {
        let result = parse_do_while("do do { x; } while (2); while (1);");
        assert!(result.is_ok());
        let stmt = result.unwrap();
        match *stmt.body {
            Statement::DoWhile(_) => {}
            _ => panic!("Expected nested do-while statement"),
        }
    }

    #[test]
    fn test_parse_do_while_error_missing_while() {
        let result = parse_do_while("do { x; } (1);");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_do_while_error_missing_paren() {
        let result = parse_do_while("do { x; } while 1);");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_do_while_error_missing_closing_paren() {
        let result = parse_do_while("do { x; } while (1;");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_do_while_error_missing_semicolon() {
        let result = parse_do_while("do { x; } while (1)");
        assert!(result.is_err());
    }
}
