use super::Parser;
use crate::{
    error::IntoCompilerError,
    grammar::*,
    parser_base::{CompilerParseError, ParseError},
    t,
};

impl<'a> Parser<'a> {
    /// Parse an expression using unified Pratt parser
    pub(super) fn parse_expression(&mut self) -> Result<Expression<'a>, CompilerParseError> {
        let base = self.parse_unit_expression()?;
        self.parse_infix_operator(base, 0)
    }

    pub(super) fn parse_unit_expression(&mut self) -> Result<Expression<'a>, CompilerParseError> {
        match self.peek_token()? {
            Some(Token { kind: t!("("), .. }) => self.parse_grouped_expression(),
            Some(Token {
                kind: TokenType::Constant(value),
                ..
            }) => {
                self.next_token()?;
                Ok(Expression::Constant(value))
            }
            Some(Token {
                kind: TokenType::Identifier(name),
                ..
            }) => {
                self.next_token()?;
                Ok(Expression::Variable(name.clone()))
            }
            Some(Token { kind, .. }) if UnaryOp::from_token_type(&kind).is_some() => {
                let op = UnaryOp::from_token_type(&kind).expect("asserted to be some");
                self.next_token()?;
                Ok(Expression::Unary {
                    op,
                    expr: Box::new(self.parse_unit_expression()?),
                })
            }
            else_token => Err(ParseError::unexpected(
                "expression",
                else_token.map(|t| t.kind).as_ref(),
            )
            .with_span(self.eof_span)),
        }
    }

    pub(super) fn parse_grouped_expression(
        &mut self,
    ) -> Result<Expression<'a>, CompilerParseError> {
        self.expect_token(t!("("))?;
        let expr = self.parse_expression()?;
        self.expect_token(t!(")"))?;
        Ok(Expression::Grouped(Box::new(expr)))
    }

    pub(super) fn parse_infix_operator(
        &mut self,
        mut lhs: Expression<'a>,
        min_bp: u8,
    ) -> Result<Expression<'a>, CompilerParseError> {
        loop {
            let token = match self.peek_token()? {
                Some(t) => t,
                None => break,
            };

            // Try to parse as binary operator
            if let Some(op) = BinaryOp::from_token_type(&token.kind) {
                let (left_bp, right_bp) = op.infix_binding_power();

                if left_bp < min_bp {
                    break;
                }

                self.next_token()?;

                let rhs = self.parse_unit_expression()?;
                let rhs = self.parse_infix_operator(rhs, right_bp)?;

                lhs = Expression::Binary {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                };
                continue;
            }

            // Try to parse as assignment operator
            if let Some(op) = AssignOp::from_token_type(&token.kind) {
                let (left_bp, right_bp) = op.infix_binding_power();

                if left_bp < min_bp {
                    break;
                }

                self.next_token()?;

                let rhs = self.parse_unit_expression()?;
                let rhs = self.parse_infix_operator(rhs, right_bp)?;

                lhs = Expression::Assignment {
                    op,
                    lvalue: Box::new(lhs),
                    rvalue: Box::new(rhs),
                };
                continue;
            }

            // Try to parse a function call
            if token.kind == t!("(") {
                let args = self.parse_function_call_arguments()?;
                lhs = Expression::FunctionCall {
                    callee: Box::new(lhs),
                    args,
                };
                continue;
            }

            // No operator found, stop
            break;
        }

        Ok(lhs)
    }

    fn parse_function_call_arguments(&mut self) -> Result<Vec<Expression<'a>>, CompilerParseError> {
        self.expect_token(t!("("))?;
        let mut args = Vec::new();
        while !self.eat(t!(")"))? {
            args.push(self.parse_expression()?);
            self.eat(t!(","))?;
        }
        Ok(args)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer_base::Lexer;

    fn parse_expr(input: &str) -> Result<Expression<'_>, CompilerParseError> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser.parse_expression()
    }

    // === Unit Expression Tests ===

    #[test]
    fn test_parse_constant() {
        let result = parse_expr("42");
        assert!(result.is_ok());
        assert!(matches!(result.unwrap(), Expression::Constant(42)));
    }

    #[test]
    fn test_parse_variable() {
        let result = parse_expr("foo");
        assert!(result.is_ok());
        match result.unwrap() {
            Expression::Variable(name) => assert_eq!(name, "foo"),
            _ => panic!("Expected variable"),
        }
    }

    #[test]
    fn test_parse_unary_negate() {
        let result = parse_expr("-5");
        assert!(result.is_ok());
        match result.unwrap() {
            Expression::Unary { op, expr } => {
                assert!(matches!(op, UnaryOp::Negate));
                assert!(matches!(*expr, Expression::Constant(5)));
            }
            _ => panic!("Expected unary negation"),
        }
    }

    #[test]
    fn test_parse_grouped_expression() {
        let result = parse_expr("(42)");
        assert!(result.is_ok());
        match result.unwrap() {
            Expression::Grouped(inner) => {
                assert!(matches!(*inner, Expression::Constant(42)));
            }
            _ => panic!("Expected grouped expression"),
        }
    }

    // === Binary Operation Tests ===

    #[test]
    fn test_parse_binary_addition() {
        let result = parse_expr("1 + 2");
        assert!(result.is_ok());
        match result.unwrap() {
            Expression::Binary { op, lhs, rhs } => {
                assert!(matches!(op, BinaryOp::Add));
                assert!(matches!(*lhs, Expression::Constant(1)));
                assert!(matches!(*rhs, Expression::Constant(2)));
            }
            _ => panic!("Expected binary addition"),
        }
    }

    #[test]
    fn test_parse_binary_subtraction() {
        let result = parse_expr("5 - 3");
        assert!(result.is_ok());
        match result.unwrap() {
            Expression::Binary { op, lhs, rhs } => {
                assert!(matches!(op, BinaryOp::Subtract));
                assert!(matches!(*lhs, Expression::Constant(5)));
                assert!(matches!(*rhs, Expression::Constant(3)));
            }
            _ => panic!("Expected binary subtraction"),
        }
    }

    #[test]
    fn test_parse_binary_multiplication() {
        let result = parse_expr("3 * 4");
        assert!(result.is_ok());
        match result.unwrap() {
            Expression::Binary { op, lhs, rhs } => {
                assert!(matches!(op, BinaryOp::Multiply));
                assert!(matches!(*lhs, Expression::Constant(3)));
                assert!(matches!(*rhs, Expression::Constant(4)));
            }
            _ => panic!("Expected binary multiplication"),
        }
    }

    #[test]
    fn test_parse_binary_division() {
        let result = parse_expr("10 / 2");
        assert!(result.is_ok());
        match result.unwrap() {
            Expression::Binary { op, lhs, rhs } => {
                assert!(matches!(op, BinaryOp::Divide));
                assert!(matches!(*lhs, Expression::Constant(10)));
                assert!(matches!(*rhs, Expression::Constant(2)));
            }
            _ => panic!("Expected binary division"),
        }
    }

    // === Operator Precedence Tests ===

    #[test]
    fn test_operator_precedence_multiply_before_add() {
        // 2 + 3 * 4 should be parsed as 2 + (3 * 4)
        let result = parse_expr("2 + 3 * 4");
        assert!(result.is_ok());
        match result.unwrap() {
            Expression::Binary { op, lhs, rhs } => {
                assert!(matches!(op, BinaryOp::Add));
                assert!(matches!(*lhs, Expression::Constant(2)));
                match *rhs {
                    Expression::Binary { op, lhs, rhs } => {
                        assert!(matches!(op, BinaryOp::Multiply));
                        assert!(matches!(*lhs, Expression::Constant(3)));
                        assert!(matches!(*rhs, Expression::Constant(4)));
                    }
                    _ => panic!("Expected multiplication on right side"),
                }
            }
            _ => panic!("Expected binary expression"),
        }
    }

    #[test]
    fn test_operator_precedence_divide_before_subtract() {
        // 10 - 6 / 2 should be parsed as 10 - (6 / 2)
        let result = parse_expr("10 - 6 / 2");
        assert!(result.is_ok());
        match result.unwrap() {
            Expression::Binary { op, lhs, rhs } => {
                assert!(matches!(op, BinaryOp::Subtract));
                assert!(matches!(*lhs, Expression::Constant(10)));
                match *rhs {
                    Expression::Binary { op, lhs, rhs } => {
                        assert!(matches!(op, BinaryOp::Divide));
                        assert!(matches!(*lhs, Expression::Constant(6)));
                        assert!(matches!(*rhs, Expression::Constant(2)));
                    }
                    _ => panic!("Expected division on right side"),
                }
            }
            _ => panic!("Expected binary expression"),
        }
    }

    #[test]
    fn test_left_associativity() {
        // 5 - 3 - 1 should be parsed as (5 - 3) - 1
        let result = parse_expr("5 - 3 - 1");
        assert!(result.is_ok());
        match result.unwrap() {
            Expression::Binary { op, lhs, rhs } => {
                assert!(matches!(op, BinaryOp::Subtract));
                assert!(matches!(*rhs, Expression::Constant(1)));
                match *lhs {
                    Expression::Binary { op, lhs, rhs } => {
                        assert!(matches!(op, BinaryOp::Subtract));
                        assert!(matches!(*lhs, Expression::Constant(5)));
                        assert!(matches!(*rhs, Expression::Constant(3)));
                    }
                    _ => panic!("Expected subtraction on left side"),
                }
            }
            _ => panic!("Expected binary expression"),
        }
    }

    #[test]
    fn test_grouped_expression_override_precedence() {
        // (2 + 3) * 4 should respect parentheses
        let result = parse_expr("(2 + 3) * 4");
        assert!(result.is_ok());
        match result.unwrap() {
            Expression::Binary { op, lhs, rhs } => {
                assert!(matches!(op, BinaryOp::Multiply));
                assert!(matches!(*rhs, Expression::Constant(4)));
                match *lhs {
                    Expression::Grouped(inner) => match *inner {
                        Expression::Binary { op, lhs, rhs } => {
                            assert!(matches!(op, BinaryOp::Add));
                            assert!(matches!(*lhs, Expression::Constant(2)));
                            assert!(matches!(*rhs, Expression::Constant(3)));
                        }
                        _ => panic!("Expected addition inside grouped expression"),
                    },
                    _ => panic!("Expected grouped expression on left side"),
                }
            }
            _ => panic!("Expected binary expression"),
        }
    }

    #[test]
    fn test_unary_with_binary() {
        // -3 + 5 should be parsed as (-3) + 5
        let result = parse_expr("-3 + 5");
        assert!(result.is_ok());
        match result.unwrap() {
            Expression::Binary { op, lhs, rhs } => {
                assert!(matches!(op, BinaryOp::Add));
                assert!(matches!(*rhs, Expression::Constant(5)));
                match *lhs {
                    Expression::Unary { op, expr } => {
                        assert!(matches!(op, UnaryOp::Negate));
                        assert!(matches!(*expr, Expression::Constant(3)));
                    }
                    _ => panic!("Expected unary negation on left side"),
                }
            }
            _ => panic!("Expected binary expression"),
        }
    }

    // === Comparison Operator Tests ===

    #[test]
    fn test_parse_less_than() {
        let result = parse_expr("1 < 2");
        assert!(result.is_ok());
        match result.unwrap() {
            Expression::Binary { op, lhs, rhs } => {
                assert!(matches!(op, BinaryOp::LT));
                assert!(matches!(*lhs, Expression::Constant(1)));
                assert!(matches!(*rhs, Expression::Constant(2)));
            }
            _ => panic!("Expected less than comparison"),
        }
    }

    #[test]
    fn test_parse_greater_than() {
        let result = parse_expr("5 > 3");
        assert!(result.is_ok());
        match result.unwrap() {
            Expression::Binary { op, lhs, rhs } => {
                assert!(matches!(op, BinaryOp::GT));
                assert!(matches!(*lhs, Expression::Constant(5)));
                assert!(matches!(*rhs, Expression::Constant(3)));
            }
            _ => panic!("Expected greater than comparison"),
        }
    }

    #[test]
    fn test_parse_less_than_or_equal() {
        let result = parse_expr("1 <= 2");
        assert!(result.is_ok());
        match result.unwrap() {
            Expression::Binary { op, lhs, rhs } => {
                assert!(matches!(op, BinaryOp::LTE));
                assert!(matches!(*lhs, Expression::Constant(1)));
                assert!(matches!(*rhs, Expression::Constant(2)));
            }
            _ => panic!("Expected less than or equal comparison"),
        }
    }

    #[test]
    fn test_parse_greater_than_or_equal() {
        let result = parse_expr("5 >= 3");
        assert!(result.is_ok());
        match result.unwrap() {
            Expression::Binary { op, lhs, rhs } => {
                assert!(matches!(op, BinaryOp::GTE));
                assert!(matches!(*lhs, Expression::Constant(5)));
                assert!(matches!(*rhs, Expression::Constant(3)));
            }
            _ => panic!("Expected greater than or equal comparison"),
        }
    }

    #[test]
    fn test_parse_equal_equal() {
        let result = parse_expr("1 == 1");
        assert!(result.is_ok());
        match result.unwrap() {
            Expression::Binary { op, lhs, rhs } => {
                assert!(matches!(op, BinaryOp::EQ));
                assert!(matches!(*lhs, Expression::Constant(1)));
                assert!(matches!(*rhs, Expression::Constant(1)));
            }
            _ => panic!("Expected equality comparison"),
        }
    }

    #[test]
    fn test_parse_not_equal() {
        let result = parse_expr("1 != 2");
        assert!(result.is_ok());
        match result.unwrap() {
            Expression::Binary { op, lhs, rhs } => {
                assert!(matches!(op, BinaryOp::NEQ));
                assert!(matches!(*lhs, Expression::Constant(1)));
                assert!(matches!(*rhs, Expression::Constant(2)));
            }
            _ => panic!("Expected not equal comparison"),
        }
    }

    #[test]
    fn test_comparison_precedence() {
        // 1 + 2 < 3 * 4 should be parsed as (1 + 2) < (3 * 4)
        let result = parse_expr("1 + 2 < 3 * 4");
        assert!(result.is_ok());
        match result.unwrap() {
            Expression::Binary { op, lhs, rhs } => {
                assert!(matches!(op, BinaryOp::LT));
                // Left side should be (1 + 2)
                match *lhs {
                    Expression::Binary { op, .. } => {
                        assert!(matches!(op, BinaryOp::Add));
                    }
                    _ => panic!("Expected addition on left side"),
                }
                // Right side should be (3 * 4)
                match *rhs {
                    Expression::Binary { op, .. } => {
                        assert!(matches!(op, BinaryOp::Multiply));
                    }
                    _ => panic!("Expected multiplication on right side"),
                }
            }
            _ => panic!("Expected binary expression"),
        }
    }

    #[test]
    fn test_comparison_equality_precedence() {
        // Test that 1 < 2 == 3 < 4 parses as (1 < 2) == (3 < 4)
        let result = parse_expr("1 < 2 == 3 < 4");
        assert!(result.is_ok());
        match result.unwrap() {
            Expression::Binary { op, lhs, rhs } => {
                assert!(matches!(op, BinaryOp::EQ));
                assert!(matches!(
                    *lhs,
                    Expression::Binary {
                        op: BinaryOp::LT,
                        ..
                    }
                ));
                assert!(matches!(
                    *rhs,
                    Expression::Binary {
                        op: BinaryOp::LT,
                        ..
                    }
                ));
            }
            _ => panic!("Expected binary expression"),
        }
    }

    // === Error Tests ===

    #[test]
    fn test_error_unexpected_token() {
        let result = parse_expr("+");
        assert!(result.is_err());
    }

    #[test]
    fn test_error_missing_closing_paren() {
        let result = parse_expr("(1 + 2");
        assert!(result.is_err());
    }

    #[test]
    fn test_error_empty_grouped_expression() {
        let result = parse_expr("()");
        assert!(result.is_err());
    }

    // === Assignment Expression Tests ===

    #[test]
    fn test_parse_simple_assignment() {
        let result = parse_expr("x = 5");
        assert!(result.is_ok());
        match result.unwrap() {
            Expression::Assignment { op, lvalue, rvalue } => {
                assert!(matches!(op, AssignOp::Assign));
                match *lvalue {
                    Expression::Variable(name) => assert_eq!(name, "x"),
                    _ => panic!("Expected variable on left side"),
                }
                assert!(matches!(*rvalue, Expression::Constant(5)));
            }
            _ => panic!("Expected assignment expression"),
        }
    }

    #[test]
    fn test_parse_right_associative_assignment() {
        // a = b = c should be parsed as a = (b = c)
        let result = parse_expr("a = b = c");
        assert!(result.is_ok());
        match result.unwrap() {
            Expression::Assignment { op, lvalue, rvalue } => {
                assert!(matches!(op, AssignOp::Assign));
                match *lvalue {
                    Expression::Variable(name) => assert_eq!(name, "a"),
                    _ => panic!("Expected variable 'a' on left side"),
                }
                // Right side should be another assignment (b = c)
                match *rvalue {
                    Expression::Assignment { op, lvalue, rvalue } => {
                        assert!(matches!(op, AssignOp::Assign));
                        match *lvalue {
                            Expression::Variable(name) => assert_eq!(name, "b"),
                            _ => panic!("Expected variable 'b'"),
                        }
                        match *rvalue {
                            Expression::Variable(name) => assert_eq!(name, "c"),
                            _ => panic!("Expected variable 'c'"),
                        }
                    }
                    _ => panic!("Expected assignment on right side"),
                }
            }
            _ => panic!("Expected assignment expression"),
        }
    }

    #[test]
    fn test_parse_assignment_with_expression() {
        // x = 1 + 2 * 3 should be parsed as x = (1 + (2 * 3))
        let result = parse_expr("x = 1 + 2 * 3");
        assert!(result.is_ok());
        match result.unwrap() {
            Expression::Assignment { op, lvalue, rvalue } => {
                assert!(matches!(op, AssignOp::Assign));
                match *lvalue {
                    Expression::Variable(name) => assert_eq!(name, "x"),
                    _ => panic!("Expected variable on left side"),
                }
                // Right side should be a binary expression
                match *rvalue {
                    Expression::Binary { op, .. } => {
                        assert!(matches!(op, BinaryOp::Add));
                    }
                    _ => panic!("Expected binary expression on right side"),
                }
            }
            _ => panic!("Expected assignment expression"),
        }
    }

    #[test]
    fn test_parse_plus_assign() {
        let result = parse_expr("x += 5");
        assert!(result.is_ok());
        match result.unwrap() {
            Expression::Assignment { op, lvalue, rvalue } => {
                assert!(matches!(op, AssignOp::PlusAssign));
                match *lvalue {
                    Expression::Variable(name) => assert_eq!(name, "x"),
                    _ => panic!("Expected variable on left side"),
                }
                assert!(matches!(*rvalue, Expression::Constant(5)));
            }
            _ => panic!("Expected assignment expression"),
        }
    }

    #[test]
    fn test_parse_minus_assign() {
        let result = parse_expr("y -= 10");
        assert!(result.is_ok());
        match result.unwrap() {
            Expression::Assignment { op, lvalue, rvalue } => {
                assert!(matches!(op, AssignOp::MinusAssign));
                match *lvalue {
                    Expression::Variable(name) => assert_eq!(name, "y"),
                    _ => panic!("Expected variable on left side"),
                }
                assert!(matches!(*rvalue, Expression::Constant(10)));
            }
            _ => panic!("Expected assignment expression"),
        }
    }

    #[test]
    fn test_parse_mul_assign() {
        let result = parse_expr("z *= 3");
        assert!(result.is_ok());
        match result.unwrap() {
            Expression::Assignment { op, lvalue, rvalue } => {
                assert!(matches!(op, AssignOp::MulAssign));
                match *lvalue {
                    Expression::Variable(name) => assert_eq!(name, "z"),
                    _ => panic!("Expected variable on left side"),
                }
                assert!(matches!(*rvalue, Expression::Constant(3)));
            }
            _ => panic!("Expected assignment expression"),
        }
    }

    #[test]
    fn test_parse_div_assign() {
        let result = parse_expr("a /= 2");
        assert!(result.is_ok());
        match result.unwrap() {
            Expression::Assignment { op, lvalue, rvalue } => {
                assert!(matches!(op, AssignOp::DivAssign));
                match *lvalue {
                    Expression::Variable(name) => assert_eq!(name, "a"),
                    _ => panic!("Expected variable on left side"),
                }
                assert!(matches!(*rvalue, Expression::Constant(2)));
            }
            _ => panic!("Expected assignment expression"),
        }
    }

    #[test]
    fn test_parse_mod_assign() {
        let result = parse_expr("b %= 5");
        assert!(result.is_ok());
        match result.unwrap() {
            Expression::Assignment { op, lvalue, rvalue } => {
                assert!(matches!(op, AssignOp::ModAssign));
                match *lvalue {
                    Expression::Variable(name) => assert_eq!(name, "b"),
                    _ => panic!("Expected variable on left side"),
                }
                assert!(matches!(*rvalue, Expression::Constant(5)));
            }
            _ => panic!("Expected assignment expression"),
        }
    }

    #[test]
    fn test_parse_and_assign() {
        let result = parse_expr("c &= 7");
        assert!(result.is_ok());
        match result.unwrap() {
            Expression::Assignment { op, lvalue, rvalue } => {
                assert!(matches!(op, AssignOp::AndAssign));
                match *lvalue {
                    Expression::Variable(name) => assert_eq!(name, "c"),
                    _ => panic!("Expected variable on left side"),
                }
                assert!(matches!(*rvalue, Expression::Constant(7)));
            }
            _ => panic!("Expected assignment expression"),
        }
    }

    #[test]
    fn test_parse_or_assign() {
        let result = parse_expr("d |= 8");
        assert!(result.is_ok());
        match result.unwrap() {
            Expression::Assignment { op, lvalue, rvalue } => {
                assert!(matches!(op, AssignOp::OrAssign));
                match *lvalue {
                    Expression::Variable(name) => assert_eq!(name, "d"),
                    _ => panic!("Expected variable on left side"),
                }
                assert!(matches!(*rvalue, Expression::Constant(8)));
            }
            _ => panic!("Expected assignment expression"),
        }
    }

    #[test]
    fn test_parse_xor_assign() {
        let result = parse_expr("e ^= 9");
        assert!(result.is_ok());
        match result.unwrap() {
            Expression::Assignment { op, lvalue, rvalue } => {
                assert!(matches!(op, AssignOp::XorAssign));
                match *lvalue {
                    Expression::Variable(name) => assert_eq!(name, "e"),
                    _ => panic!("Expected variable on left side"),
                }
                assert!(matches!(*rvalue, Expression::Constant(9)));
            }
            _ => panic!("Expected assignment expression"),
        }
    }

    #[test]
    fn test_parse_left_shift_assign() {
        let result = parse_expr("f <<= 2");
        assert!(result.is_ok());
        match result.unwrap() {
            Expression::Assignment { op, lvalue, rvalue } => {
                assert!(matches!(op, AssignOp::LShiftAssign));
                match *lvalue {
                    Expression::Variable(name) => assert_eq!(name, "f"),
                    _ => panic!("Expected variable on left side"),
                }
                assert!(matches!(*rvalue, Expression::Constant(2)));
            }
            _ => panic!("Expected assignment expression"),
        }
    }

    #[test]
    fn test_parse_right_shift_assign() {
        let result = parse_expr("g >>= 3");
        assert!(result.is_ok());
        match result.unwrap() {
            Expression::Assignment { op, lvalue, rvalue } => {
                assert!(matches!(op, AssignOp::RShiftAssign));
                match *lvalue {
                    Expression::Variable(name) => assert_eq!(name, "g"),
                    _ => panic!("Expected variable on left side"),
                }
                assert!(matches!(*rvalue, Expression::Constant(3)));
            }
            _ => panic!("Expected assignment expression"),
        }
    }

    #[test]
    fn test_assignment_lower_precedence_than_comparison() {
        // x = 1 < 2 should be parsed as x = (1 < 2)
        let result = parse_expr("x = 1 < 2");
        assert!(result.is_ok());
        match result.unwrap() {
            Expression::Assignment { op, lvalue, rvalue } => {
                assert!(matches!(op, AssignOp::Assign));
                match *lvalue {
                    Expression::Variable(name) => assert_eq!(name, "x"),
                    _ => panic!("Expected variable on left side"),
                }
                // Right side should be a comparison
                match *rvalue {
                    Expression::Binary { op, .. } => {
                        assert!(matches!(op, BinaryOp::LT));
                    }
                    _ => panic!("Expected comparison on right side"),
                }
            }
            _ => panic!("Expected assignment expression"),
        }
    }

    // === Function Call Tests ===

    #[test]
    fn test_parse_function_call_no_args() {
        let result = parse_expr("foo()");
        assert!(result.is_ok());
        match result.unwrap() {
            Expression::FunctionCall { callee, args } => {
                match *callee {
                    Expression::Variable(name) => assert_eq!(name, "foo"),
                    _ => panic!("Expected function name"),
                }
                assert_eq!(args.len(), 0);
            }
            _ => panic!("Expected function call expression"),
        }
    }

    #[test]
    fn test_parse_function_call_single_arg() {
        let result = parse_expr("bar(42)");
        assert!(result.is_ok());
        match result.unwrap() {
            Expression::FunctionCall { callee, args } => {
                match *callee {
                    Expression::Variable(name) => assert_eq!(name, "bar"),
                    _ => panic!("Expected function name"),
                }
                assert_eq!(args.len(), 1);
                assert!(matches!(args[0], Expression::Constant(42)));
            }
            _ => panic!("Expected function call expression"),
        }
    }

    #[test]
    fn test_parse_function_call_multiple_args() {
        let result = parse_expr("add(1, 2, 3)");
        assert!(result.is_ok());
        match result.unwrap() {
            Expression::FunctionCall { callee, args } => {
                match *callee {
                    Expression::Variable(name) => assert_eq!(name, "add"),
                    _ => panic!("Expected function name"),
                }
                assert_eq!(args.len(), 3);
                assert!(matches!(args[0], Expression::Constant(1)));
                assert!(matches!(args[1], Expression::Constant(2)));
                assert!(matches!(args[2], Expression::Constant(3)));
            }
            _ => panic!("Expected function call expression"),
        }
    }

    #[test]
    fn test_parse_function_call_with_expression_args() {
        let result = parse_expr("calculate(1 + 2, x * 3)");
        assert!(result.is_ok());
        match result.unwrap() {
            Expression::FunctionCall { callee, args } => {
                match *callee {
                    Expression::Variable(name) => assert_eq!(name, "calculate"),
                    _ => panic!("Expected function name"),
                }
                assert_eq!(args.len(), 2);
                // First argument: 1 + 2
                match &args[0] {
                    Expression::Binary { op, lhs, rhs } => {
                        assert!(matches!(op, BinaryOp::Add));
                        assert!(matches!(**lhs, Expression::Constant(1)));
                        assert!(matches!(**rhs, Expression::Constant(2)));
                    }
                    _ => panic!("Expected binary expression as first argument"),
                }
                // Second argument: x * 3
                match &args[1] {
                    Expression::Binary { op, lhs, rhs } => {
                        assert!(matches!(op, BinaryOp::Multiply));
                        match &**lhs {
                            Expression::Variable(name) => assert_eq!(name, "x"),
                            _ => panic!("Expected variable in second argument"),
                        }
                        assert!(matches!(**rhs, Expression::Constant(3)));
                    }
                    _ => panic!("Expected binary expression as second argument"),
                }
            }
            _ => panic!("Expected function call expression"),
        }
    }

    #[test]
    fn test_parse_nested_function_calls() {
        // outer(inner(5))
        let result = parse_expr("outer(inner(5))");
        assert!(result.is_ok());
        match result.unwrap() {
            Expression::FunctionCall { callee, args } => {
                match *callee {
                    Expression::Variable(name) => assert_eq!(name, "outer"),
                    _ => panic!("Expected outer function name"),
                }
                assert_eq!(args.len(), 1);
                // Argument should be inner(5)
                match &args[0] {
                    Expression::FunctionCall { callee, args } => {
                        match &**callee {
                            Expression::Variable(name) => assert_eq!(name, "inner"),
                            _ => panic!("Expected inner function name"),
                        }
                        assert_eq!(args.len(), 1);
                        assert!(matches!(args[0], Expression::Constant(5)));
                    }
                    _ => panic!("Expected nested function call"),
                }
            }
            _ => panic!("Expected function call expression"),
        }
    }

    #[test]
    fn test_parse_function_call_in_binary_expression() {
        // foo(1) + bar(2) should be parsed as (foo(1)) + (bar(2))
        let result = parse_expr("foo(1) + bar(2)");
        assert!(result.is_ok());
        match result.unwrap() {
            Expression::Binary { op, lhs, rhs } => {
                assert!(matches!(op, BinaryOp::Add));
                // Left side: foo(1)
                match *lhs {
                    Expression::FunctionCall { callee, args } => {
                        match *callee {
                            Expression::Variable(name) => assert_eq!(name, "foo"),
                            _ => panic!("Expected foo function"),
                        }
                        assert_eq!(args.len(), 1);
                        assert!(matches!(args[0], Expression::Constant(1)));
                    }
                    _ => panic!("Expected function call on left side"),
                }
                // Right side: bar(2)
                match *rhs {
                    Expression::FunctionCall { callee, args } => {
                        match *callee {
                            Expression::Variable(name) => assert_eq!(name, "bar"),
                            _ => panic!("Expected bar function"),
                        }
                        assert_eq!(args.len(), 1);
                        assert!(matches!(args[0], Expression::Constant(2)));
                    }
                    _ => panic!("Expected function call on right side"),
                }
            }
            _ => panic!("Expected binary expression"),
        }
    }

    #[test]
    fn test_parse_function_call_in_assignment() {
        // x = getValue(10) should be parsed as x = (getValue(10))
        let result = parse_expr("x = getValue(10)");
        assert!(result.is_ok());
        match result.unwrap() {
            Expression::Assignment { op, lvalue, rvalue } => {
                assert!(matches!(op, AssignOp::Assign));
                match *lvalue {
                    Expression::Variable(name) => assert_eq!(name, "x"),
                    _ => panic!("Expected variable on left side"),
                }
                // Right side should be getValue(10)
                match *rvalue {
                    Expression::FunctionCall { callee, args } => {
                        match *callee {
                            Expression::Variable(name) => assert_eq!(name, "getValue"),
                            _ => panic!("Expected getValue function"),
                        }
                        assert_eq!(args.len(), 1);
                        assert!(matches!(args[0], Expression::Constant(10)));
                    }
                    _ => panic!("Expected function call on right side"),
                }
            }
            _ => panic!("Expected assignment expression"),
        }
    }

    #[test]
    fn test_parse_function_call_with_trailing_comma() {
        // foo(1,2,) should parse successfully
        let result = parse_expr("foo(1, 2,)");
        assert!(result.is_ok());
        match result.unwrap() {
            Expression::FunctionCall { callee, args } => {
                match *callee {
                    Expression::Variable(name) => assert_eq!(name, "foo"),
                    _ => panic!("Expected function name"),
                }
                assert_eq!(args.len(), 2);
                assert!(matches!(args[0], Expression::Constant(1)));
                assert!(matches!(args[1], Expression::Constant(2)));
            }
            _ => panic!("Expected function call expression"),
        }
    }
}
