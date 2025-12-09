use crate::{
    grammar::IfStmt,
    parser_base::{CompilerParseError, Parser},
    t,
};

impl<'a> Parser<'a> {
    /// Parse an if statement: if (condition) { body } else { body }
    pub(super) fn parse_if_statement(&mut self) -> Result<IfStmt<'a>, CompilerParseError> {
        self.expect_sequence_of_tokens([t!("if"), t!("(")])?;
        let condition = self.parse_expression()?;
        self.expect_token(t!(")"))?;
        let then_block = self.parse_statement()?;
        let else_block = self
            .eat(t!("else"))?
            .then(|| self.parse_statement())
            .transpose()?;

        Ok(IfStmt {
            cond: condition,
            then_block: Box::new(then_block),
            else_block: else_block.map(Box::new),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{grammar::*, lexer_base::Lexer};

    fn parse_if(input: &str) -> Result<IfStmt<'_>, CompilerParseError> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser.parse_if_statement()
    }

    // === If Statement Tests ===

    #[test]
    fn test_parse_if_basic() {
        let result = parse_if("if (1) { return 0; }");
        assert!(result.is_ok());
        let stmt = result.unwrap();
        assert!(matches!(stmt.cond, Expression::Constant(1)));
        assert!(matches!(*stmt.then_block, Statement::Block(_)));
        assert!(stmt.else_block.is_none());
    }

    #[test]
    fn test_parse_if_with_comparison() {
        let result = parse_if("if (x > 5) { x; }");
        assert!(result.is_ok());
        let stmt = result.unwrap();
        match stmt.cond {
            Expression::Binary { op, lhs, rhs } => {
                assert!(matches!(op, BinaryOp::GreaterThan));
                assert!(matches!(*lhs, Expression::Variable(name) if name == "x"));
                assert!(matches!(*rhs, Expression::Constant(5)));
            }
            _ => panic!("Expected binary comparison"),
        }
    }

    #[test]
    fn test_parse_if_with_single_statement() {
        let result = parse_if("if (1) x;");
        assert!(result.is_ok());
        let stmt = result.unwrap();
        assert!(matches!(*stmt.then_block, Statement::Expr(_)));
    }

    #[test]
    fn test_parse_if_else() {
        let result = parse_if("if (1) { return 0; } else { return 1; }");
        assert!(result.is_ok());
        let stmt = result.unwrap();
        assert!(matches!(stmt.cond, Expression::Constant(1)));
        assert!(matches!(*stmt.then_block, Statement::Block(_)));
        assert!(stmt.else_block.is_some());
        assert!(matches!(
            stmt.else_block.unwrap().as_ref(),
            Statement::Block(_)
        ));
    }

    #[test]
    fn test_parse_if_else_if_chain() {
        let result = parse_if("if (x < 0) { x; } else if (x > 0) { x; } else { x; }");
        assert!(result.is_ok());
        let stmt = result.unwrap();
        assert!(stmt.else_block.is_some());
        // The else block should contain another if statement
        match stmt.else_block.unwrap().as_ref() {
            Statement::If(inner_if) => {
                assert!(inner_if.else_block.is_some());
            }
            _ => panic!("Expected if statement in else block"),
        }
    }

    #[test]
    fn test_parse_if_nested() {
        let result = parse_if("if (1) if (2) { x; }");
        assert!(result.is_ok());
        let stmt = result.unwrap();
        match *stmt.then_block {
            Statement::If(_) => {}
            _ => panic!("Expected nested if statement"),
        }
    }

    #[test]
    fn test_parse_if_error_missing_paren() {
        let result = parse_if("if 1) { x; }");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_if_error_missing_closing_paren() {
        let result = parse_if("if (1 { x; }");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_if_error_missing_body() {
        let result = parse_if("if (1)");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_if_error_else_missing_body() {
        let result = parse_if("if (1) { x; } else");
        assert!(result.is_err());
    }
}
