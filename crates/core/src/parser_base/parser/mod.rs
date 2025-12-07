use std::{borrow::Cow, iter::Peekable};

use crate::{
    error::IntoCompilerError,
    grammar::*,
    lexer_base::{CompilerLexError, Lexer},
    parser_base::{CompilerParseError, ParseError},
    t,
};

#[derive(Clone)]
pub struct Parser<'a, I>
where
    I: Iterator<Item = Result<Token<'a>, CompilerLexError>>,
{
    lexer: Peekable<I>,
    eof_span: Span,
}

impl<'a> Parser<'a, Lexer<'a>> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let eof_span = lexer.eof_span();
        Self {
            lexer: lexer.peekable(),
            eof_span,
        }
    }
}

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Result<Token<'a>, CompilerLexError>>,
{
    /// Parse a complete program (one or more function definitions)
    pub fn parse(mut self) -> Result<Program<'a>, CompilerParseError> {
        let mut functions = Vec::new();

        // Parse all functions until EOF
        while self.peek_token()?.is_some() {
            functions.push(self.parse_function()?);
        }

        if functions.is_empty() {
            return Err(
                ParseError::unexpected_eof_with_message("function definition")
                    .with_span(self.eof_span),
            );
        }

        Ok(Program { functions })
    }

    /// Parse a function definition: int name(void) { ... }
    fn parse_function(&mut self) -> Result<FuncDef<'a>, CompilerParseError> {
        // Expect return type
        let return_type = self.expect_type()?;

        // Function name
        let name = self.expect_identifier()?;

        // Parameters
        self.expect(t!("("))?;
        self.expect_optional(t!("void"))?; // For now, only void params
        self.expect(t!(")"))?;

        // Function body (block)
        let body = self.parse_block_statement()?;

        Ok(FuncDef {
            return_type,
            name,
            params: Vec::new(),
            body,
        })
    }

    /// Parse a statement
    fn parse_statement(&mut self) -> Result<Statement<'a>, CompilerParseError> {
        match self.peek_token()? {
            Some(Token {
                kind: t!("break"), ..
            }) => self.parse_break_statement().map(Into::into),
            Some(Token {
                kind: t!("continue"),
                ..
            }) => self.parse_continue_statement().map(Into::into),
            Some(Token { kind: t!("do"), .. }) => self.parse_do_while_statement().map(Into::into),
            Some(Token { kind: t!("if"), .. }) => self.parse_if_statement().map(Into::into),
            Some(Token {
                kind: t!("return"), ..
            }) => self.parse_return_statement().map(Into::into),
            Some(Token {
                kind: t!("while"), ..
            }) => self.parse_while_statement().map(Into::into),
            Some(Token { kind: t!("{"), .. }) => self.parse_block_statement().map(Into::into),
            Some(token) => {
                Err(ParseError::expected_statement(token.kind.clone()).with_span(token.span))
            }
            None => Err(ParseError::expected_statement_eof().with_span(self.eof_span)),
        }
    }

    fn parse_break_statement(&mut self) -> Result<BreakStmt<'a>, CompilerParseError> {
        self.expect(t!("break"))?;
        self.expect(t!(";"))?;
        Ok(BreakStmt::new())
    }

    fn parse_continue_statement(&mut self) -> Result<ContinueStmt<'a>, CompilerParseError> {
        self.expect(t!("continue"))?;
        self.expect(t!(";"))?;
        Ok(ContinueStmt::new())
    }

    /// Parse a block: { statement* }
    fn parse_block_statement(&mut self) -> Result<BlockStmt<'a>, CompilerParseError> {
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

    fn parse_do_while_statement(&mut self) -> Result<DoWhileStmt<'a>, CompilerParseError> {
        self.expect(t!("do"))?;
        let body = self.parse_statement()?;
        self.expect(t!("while"))?;
        self.expect(t!("("))?;
        let condition = self.parse_expression()?;
        self.expect(t!(")"))?;
        self.expect(t!(";"))?;
        Ok(DoWhileStmt {
            body: Box::new(body),
            cond: condition,
        })
    }

    /// Parse an if statement: if (condition) { body } else { body }
    fn parse_if_statement(&mut self) -> Result<IfStmt<'a>, CompilerParseError> {
        self.expect(t!("if"))?;
        self.expect(t!("("))?;
        let condition = self.parse_expression()?;
        self.expect(t!(")"))?;
        let then_block = self.parse_statement()?;
        let else_block = self
            .expect_optional(t!("else"))?
            .map(|_| self.parse_statement())
            .transpose()?;

        Ok(IfStmt {
            cond: condition,
            then_block: Box::new(then_block),
            else_block: else_block.map(Box::new),
        })
    }

    /// Parse a return statement: return expr;
    fn parse_return_statement(&mut self) -> Result<ReturnStmt<'a>, CompilerParseError> {
        self.expect(t!("return"))?;
        let expr = self.parse_expression()?;
        self.expect(t!(";"))?;
        Ok(ReturnStmt { expr })
    }

    fn parse_while_statement(&mut self) -> Result<WhileStmt<'a>, CompilerParseError> {
        self.expect(t!("while"))?;
        self.expect(t!("("))?;
        let cond = self.parse_expression()?;
        self.expect(t!(")"))?;
        let body = self.parse_statement()?;
        Ok(WhileStmt {
            cond,
            body: Box::new(body),
        })
    }

    /// Parse an expression
    fn parse_expression(&mut self) -> Result<Expression<'a>, CompilerParseError> {
        let base = self.parse_unit_expression()?;
        self.parse_infix_operator(base, 0)
    }

    fn parse_unit_expression(&mut self) -> Result<Expression<'a>, CompilerParseError> {
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
            Some(Token { kind: t!("-"), .. }) => {
                self.next_token()?;
                Ok(Expression::Unary {
                    op: UnaryOp::Negate,
                    expr: Box::new(self.parse_unit_expression()?),
                })
            }
            Some(token) => Err(ParseError::expected_expression(token.kind).with_span(token.span)),
            None => Err(ParseError::expected_expression_eof().with_span(self.eof_span)),
        }
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression<'a>, CompilerParseError> {
        self.expect(t!("("))?;
        let expr = self.parse_expression()?;
        self.expect(t!(")"))?;
        Ok(Expression::Grouped(Box::new(expr)))
    }

    fn parse_infix_operator(
        &mut self,
        mut lhs: Expression<'a>,
        min_bp: u8,
    ) -> Result<Expression<'a>, CompilerParseError> {
        while let Some(op) = self
            .peek_token()?
            .and_then(|t| BinaryOp::from_token_type(&t.kind))
        {
            let (left_bp, right_bp) = op.infix_binding_power();

            // If the binding power is too low, stop
            if left_bp < min_bp {
                break;
            }

            // Consume the operator token
            self.next_token()?;

            let rhs = self.parse_unit_expression()?;
            let rhs = self.parse_infix_operator(rhs, right_bp)?;

            lhs = Expression::Binary {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            };
        }

        Ok(lhs)
    }

    /// Expect a specific token
    fn expect(&mut self, expected: TokenType<'static>) -> Result<(), CompilerParseError> {
        match self.next_token()? {
            Some(token) if token.kind == expected => Ok(()),
            Some(unexpected) => {
                Err(ParseError::unexpected_token(expected, unexpected.kind)
                    .with_span(unexpected.span))
            }
            None => Err(ParseError::unexpected_eof(expected).with_span(self.eof_span)),
        }
    }

    /// Expect an identifier token
    fn expect_identifier(&mut self) -> Result<Cow<'a, str>, CompilerParseError> {
        match self.next_token()? {
            Some(Token {
                kind: TokenType::Identifier(name),
                ..
            }) => Ok(name),
            Some(Token { kind, span }) => {
                Err(ParseError::expected_identifier(kind).with_span(span))
            }
            None => Err(ParseError::expected_identifier_eof().with_span(self.eof_span)),
        }
    }

    /// Optionally consume a token if it matches
    fn expect_optional(
        &mut self,
        expected: TokenType<'static>,
    ) -> Result<Option<Token<'a>>, CompilerParseError> {
        match self.peek_token()? {
            Some(token) if token.kind == expected => {
                self.next_token()?;
                Ok(Some(token))
            }
            _ => Ok(None),
        }
    }

    fn expect_type(&mut self) -> Result<Type, CompilerParseError> {
        match self.peek_token()? {
            Some(token) => {
                self.next_token()?;
                Type::from_token_type(&token.kind)
                    .ok_or(ParseError::expected_type(token.kind).with_span(token.span))
            }
            None => Err(ParseError::expected_type_eof().with_span(self.eof_span)),
        }
    }

    /// Consumes the next token from the lexer.
    fn next_token(&mut self) -> Result<Option<Token<'a>>, CompilerParseError> {
        self.lexer.next().transpose().map_err(|e| e.convert_error())
    }

    /// Peeks the next token from the lexer without consuming it.
    fn peek_token(&mut self) -> Result<Option<Token<'a>>, CompilerParseError> {
        self.lexer
            .peek()
            .cloned()
            .transpose()
            .map_err(|e| e.convert_error())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{grammar::Type, lexer_base::Lexer};

    fn parse_program(input: &str) -> Result<Program<'_>, CompilerParseError> {
        let lexer = Lexer::new(input);
        Parser::new(lexer).parse()
    }

    // === Success Cases ===

    #[test]
    fn test_parse_simple_function() {
        let input = "int main(void) { return 0; }";
        let result = parse_program(input);
        assert!(result.is_ok());
        let program = result.unwrap();
        assert_eq!(program.functions.len(), 1);
        assert_eq!(program.functions[0].return_type, Type::Int);
        assert_eq!(program.functions[0].name, "main");
        assert_eq!(program.functions[0].body.statements.len(), 1);
    }

    #[test]
    fn test_parse_function_with_constant() {
        let input = "int main(void) { return 42; }";
        let result = parse_program(input);
        assert!(result.is_ok());
        let program = result.unwrap();
        assert_eq!(program.functions.len(), 1);
        assert_eq!(program.functions[0].return_type, Type::Int);
        match &program.functions[0].body.statements[0] {
            Statement::Return(ReturnStmt {
                expr: Expression::Constant(42),
            }) => {}
            _ => panic!("Expected return 42"),
        }
    }

    #[test]
    fn test_parse_void_function() {
        let input = "void foo(void) { return 0; }";
        let result = parse_program(input);
        assert!(result.is_ok());
        let program = result.unwrap();
        assert_eq!(program.functions[0].return_type, Type::Void);
        assert_eq!(program.functions[0].name, "foo");
    }

    #[test]
    fn test_parse_function_without_void() {
        let input = "int main() { return 0; }";
        let result = parse_program(input);
        assert!(result.is_ok());
        let program = result.unwrap();
        assert_eq!(program.functions[0].return_type, Type::Int);
    }

    #[test]
    fn test_parse_function_with_spaces() {
        let input = "int   main  (  void  )  {  return   0  ;  }";
        let result = parse_program(input);
        assert!(result.is_ok());
        let program = result.unwrap();
        assert_eq!(program.functions[0].return_type, Type::Int);
    }

    #[test]
    fn test_parse_multiple_functions() {
        let input = r#"
            int foo(void) { return 1; }
            int bar(void) { return 2; }
            void baz() { return 0; }
        "#;
        let result = parse_program(input);
        assert!(result.is_ok());
        let program = result.unwrap();
        assert_eq!(program.functions.len(), 3);

        assert_eq!(program.functions[0].name, "foo");
        assert_eq!(program.functions[0].return_type, Type::Int);

        assert_eq!(program.functions[1].name, "bar");
        assert_eq!(program.functions[1].return_type, Type::Int);

        assert_eq!(program.functions[2].name, "baz");
        assert_eq!(program.functions[2].return_type, Type::Void);
    }

    #[test]
    fn test_parse_expression_constant() {
        let input = "int main(void) { return 999; }";
        let result = parse_program(input);
        assert!(result.is_ok());
        let program = result.unwrap();
        match &program.functions[0].body.statements[0] {
            Statement::Return(ReturnStmt {
                expr: Expression::Constant(999),
            }) => {}
            _ => panic!("Expected return 999"),
        }
    }

    #[test]
    fn test_parse_expression_variable() {
        let input = "int main(void) { return foo; }";
        let result = parse_program(input);
        assert!(result.is_ok());
        let program = result.unwrap();
        match &program.functions[0].body.statements[0] {
            Statement::Return(ReturnStmt {
                expr: Expression::Variable(name),
            }) if name == "foo" => {}
            _ => panic!("Expected return foo"),
        }
    }

    #[test]
    fn test_parse_function_name_with_underscore() {
        let input = "int _main(void) { return 0; }";
        let result = parse_program(input);
        assert!(result.is_ok());
        let program = result.unwrap();
        assert_eq!(program.functions[0].name, "_main");
        assert_eq!(program.functions[0].return_type, Type::Int);
    }

    // === Expression Parsing Tests ===

    #[test]
    fn test_parse_binary_addition() {
        let input = "int main(void) { return 1 + 2; }";
        let result = parse_program(input);
        assert!(result.is_ok());
        let program = result.unwrap();
        match &program.functions[0].body.statements[0] {
            Statement::Return(ReturnStmt {
                expr: Expression::Binary { op, lhs, rhs },
            }) => {
                assert!(matches!(op, BinaryOp::Add));
                assert!(matches!(**lhs, Expression::Constant(1)));
                assert!(matches!(**rhs, Expression::Constant(2)));
            }
            _ => panic!("Expected binary addition"),
        }
    }

    #[test]
    fn test_parse_binary_subtraction() {
        let input = "int main(void) { return 5 - 3; }";
        let result = parse_program(input);
        assert!(result.is_ok());
        let program = result.unwrap();
        match &program.functions[0].body.statements[0] {
            Statement::Return(ReturnStmt {
                expr: Expression::Binary { op, lhs, rhs },
            }) => {
                assert!(matches!(op, BinaryOp::Subtract));
                assert!(matches!(**lhs, Expression::Constant(5)));
                assert!(matches!(**rhs, Expression::Constant(3)));
            }
            _ => panic!("Expected binary subtraction"),
        }
    }

    #[test]
    fn test_parse_binary_multiplication() {
        let input = "int main(void) { return 3 * 4; }";
        let result = parse_program(input);
        assert!(result.is_ok());
        let program = result.unwrap();
        match &program.functions[0].body.statements[0] {
            Statement::Return(ReturnStmt {
                expr: Expression::Binary { op, lhs, rhs },
            }) => {
                assert!(matches!(op, BinaryOp::Multiply));
                assert!(matches!(**lhs, Expression::Constant(3)));
                assert!(matches!(**rhs, Expression::Constant(4)));
            }
            _ => panic!("Expected binary multiplication"),
        }
    }

    #[test]
    fn test_parse_binary_division() {
        let input = "int main(void) { return 10 / 2; }";
        let result = parse_program(input);
        assert!(result.is_ok());
        let program = result.unwrap();
        match &program.functions[0].body.statements[0] {
            Statement::Return(ReturnStmt {
                expr: Expression::Binary { op, lhs, rhs },
            }) => {
                assert!(matches!(op, BinaryOp::Divide));
                assert!(matches!(**lhs, Expression::Constant(10)));
                assert!(matches!(**rhs, Expression::Constant(2)));
            }
            _ => panic!("Expected binary division"),
        }
    }

    #[test]
    fn test_parse_operator_precedence_multiply_before_add() {
        // 2 + 3 * 4 should be parsed as 2 + (3 * 4)
        let input = "int main(void) { return 2 + 3 * 4; }";
        let result = parse_program(input);
        assert!(result.is_ok());
        let program = result.unwrap();
        match &program.functions[0].body.statements[0] {
            Statement::Return(ReturnStmt {
                expr: Expression::Binary { op, lhs, rhs },
            }) => {
                // Top level should be addition
                assert!(matches!(op, BinaryOp::Add));
                assert!(matches!(**lhs, Expression::Constant(2)));
                // Right side should be multiplication
                match &**rhs {
                    Expression::Binary { op, lhs, rhs } => {
                        assert!(matches!(op, BinaryOp::Multiply));
                        assert!(matches!(**lhs, Expression::Constant(3)));
                        assert!(matches!(**rhs, Expression::Constant(4)));
                    }
                    _ => panic!("Expected multiplication on right side"),
                }
            }
            _ => panic!("Expected binary expression"),
        }
    }

    #[test]
    fn test_parse_operator_precedence_divide_before_subtract() {
        // 10 - 6 / 2 should be parsed as 10 - (6 / 2)
        let input = "int main(void) { return 10 - 6 / 2; }";
        let result = parse_program(input);
        assert!(result.is_ok());
        let program = result.unwrap();
        match &program.functions[0].body.statements[0] {
            Statement::Return(ReturnStmt {
                expr: Expression::Binary { op, lhs, rhs },
            }) => {
                // Top level should be subtraction
                assert!(matches!(op, BinaryOp::Subtract));
                assert!(matches!(**lhs, Expression::Constant(10)));
                // Right side should be division
                match &**rhs {
                    Expression::Binary { op, lhs, rhs } => {
                        assert!(matches!(op, BinaryOp::Divide));
                        assert!(matches!(**lhs, Expression::Constant(6)));
                        assert!(matches!(**rhs, Expression::Constant(2)));
                    }
                    _ => panic!("Expected division on right side"),
                }
            }
            _ => panic!("Expected binary expression"),
        }
    }

    #[test]
    fn test_parse_left_associativity() {
        // 5 - 3 - 1 should be parsed as (5 - 3) - 1
        let input = "int main(void) { return 5 - 3 - 1; }";
        let result = parse_program(input);
        assert!(result.is_ok());
        let program = result.unwrap();
        match &program.functions[0].body.statements[0] {
            Statement::Return(ReturnStmt {
                expr: Expression::Binary { op, lhs, rhs },
            }) => {
                // Top level should be subtraction
                assert!(matches!(op, BinaryOp::Subtract));
                assert!(matches!(**rhs, Expression::Constant(1)));
                // Left side should be subtraction (5 - 3)
                match &**lhs {
                    Expression::Binary { op, lhs, rhs } => {
                        assert!(matches!(op, BinaryOp::Subtract));
                        assert!(matches!(**lhs, Expression::Constant(5)));
                        assert!(matches!(**rhs, Expression::Constant(3)));
                    }
                    _ => panic!("Expected subtraction on left side"),
                }
            }
            _ => panic!("Expected binary expression"),
        }
    }

    #[test]
    fn test_parse_grouped_expression() {
        // (2 + 3) * 4 should respect parentheses
        let input = "int main(void) { return (2 + 3) * 4; }";
        let result = parse_program(input);
        assert!(result.is_ok());
        let program = result.unwrap();
        match &program.functions[0].body.statements[0] {
            Statement::Return(ReturnStmt {
                expr: Expression::Binary { op, lhs, rhs },
            }) => {
                // Top level should be multiplication
                assert!(matches!(op, BinaryOp::Multiply));
                assert!(matches!(**rhs, Expression::Constant(4)));
                // Left side should be grouped expression
                match &**lhs {
                    Expression::Grouped(inner) => match &**inner {
                        Expression::Binary { op, lhs, rhs } => {
                            assert!(matches!(op, BinaryOp::Add));
                            assert!(matches!(**lhs, Expression::Constant(2)));
                            assert!(matches!(**rhs, Expression::Constant(3)));
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
    fn test_parse_unary_negate() {
        let input = "int main(void) { return -5; }";
        let result = parse_program(input);
        assert!(result.is_ok());
        let program = result.unwrap();
        match &program.functions[0].body.statements[0] {
            Statement::Return(ReturnStmt {
                expr: Expression::Unary { op, expr },
            }) => {
                assert!(matches!(op, UnaryOp::Negate));
                assert!(matches!(**expr, Expression::Constant(5)));
            }
            _ => panic!("Expected unary negation"),
        }
    }

    #[test]
    fn test_parse_unary_with_binary() {
        // -3 + 5 should be parsed as (-3) + 5
        let input = "int main(void) { return -3 + 5; }";
        let result = parse_program(input);
        assert!(result.is_ok());
        let program = result.unwrap();
        match &program.functions[0].body.statements[0] {
            Statement::Return(ReturnStmt {
                expr: Expression::Binary { op, lhs, rhs },
            }) => {
                assert!(matches!(op, BinaryOp::Add));
                assert!(matches!(**rhs, Expression::Constant(5)));
                match &**lhs {
                    Expression::Unary { op, expr } => {
                        assert!(matches!(op, UnaryOp::Negate));
                        assert!(matches!(**expr, Expression::Constant(3)));
                    }
                    _ => panic!("Expected unary negation on left side"),
                }
            }
            _ => panic!("Expected binary expression"),
        }
    }

    #[test]
    fn test_parse_complex_expression() {
        // 2 + 3 * 4 - 5 / (1 + 1)
        let input = "int main(void) { return 2 + 3 * 4 - 5 / (1 + 1); }";
        let result = parse_program(input);
        assert!(result.is_ok());
        // Just verify it parses without panicking - detailed structure check
        // would be very verbose
    }

    // === Comparison Operator Tests ===

    #[test]
    fn test_parse_less_than() {
        let input = "int main(void) { return 1 < 2; }";
        let result = parse_program(input);
        assert!(result.is_ok());
        let program = result.unwrap();
        match &program.functions[0].body.statements[0] {
            Statement::Return(ReturnStmt {
                expr: Expression::Binary { op, lhs, rhs },
            }) => {
                assert!(matches!(op, BinaryOp::LT));
                assert!(matches!(**lhs, Expression::Constant(1)));
                assert!(matches!(**rhs, Expression::Constant(2)));
            }
            _ => panic!("Expected less than comparison"),
        }
    }

    #[test]
    fn test_parse_greater_than() {
        let input = "int main(void) { return 5 > 3; }";
        let result = parse_program(input);
        assert!(result.is_ok());
        let program = result.unwrap();
        match &program.functions[0].body.statements[0] {
            Statement::Return(ReturnStmt {
                expr: Expression::Binary { op, lhs, rhs },
            }) => {
                assert!(matches!(op, BinaryOp::GT));
                assert!(matches!(**lhs, Expression::Constant(5)));
                assert!(matches!(**rhs, Expression::Constant(3)));
            }
            _ => panic!("Expected greater than comparison"),
        }
    }

    #[test]
    fn test_parse_less_than_or_equal() {
        let input = "int main(void) { return 1 <= 2; }";
        let result = parse_program(input);
        assert!(result.is_ok());
        let program = result.unwrap();
        match &program.functions[0].body.statements[0] {
            Statement::Return(ReturnStmt {
                expr: Expression::Binary { op, lhs, rhs },
            }) => {
                assert!(matches!(op, BinaryOp::LTE));
                assert!(matches!(**lhs, Expression::Constant(1)));
                assert!(matches!(**rhs, Expression::Constant(2)));
            }
            _ => panic!("Expected less than or equal comparison"),
        }
    }

    #[test]
    fn test_parse_greater_than_or_equal() {
        let input = "int main(void) { return 5 >= 3; }";
        let result = parse_program(input);
        assert!(result.is_ok());
        let program = result.unwrap();
        match &program.functions[0].body.statements[0] {
            Statement::Return(ReturnStmt {
                expr: Expression::Binary { op, lhs, rhs },
            }) => {
                assert!(matches!(op, BinaryOp::GTE));
                assert!(matches!(**lhs, Expression::Constant(5)));
                assert!(matches!(**rhs, Expression::Constant(3)));
            }
            _ => panic!("Expected greater than or equal comparison"),
        }
    }

    #[test]
    fn test_parse_equal_equal() {
        let input = "int main(void) { return 1 == 1; }";
        let result = parse_program(input);
        assert!(result.is_ok());
        let program = result.unwrap();
        match &program.functions[0].body.statements[0] {
            Statement::Return(ReturnStmt {
                expr: Expression::Binary { op, lhs, rhs },
            }) => {
                assert!(matches!(op, BinaryOp::EQ));
                assert!(matches!(**lhs, Expression::Constant(1)));
                assert!(matches!(**rhs, Expression::Constant(1)));
            }
            _ => panic!("Expected equality comparison"),
        }
    }

    #[test]
    fn test_parse_not_equal() {
        let input = "int main(void) { return 1 != 2; }";
        let result = parse_program(input);
        assert!(result.is_ok());
        let program = result.unwrap();
        match &program.functions[0].body.statements[0] {
            Statement::Return(ReturnStmt {
                expr: Expression::Binary { op, lhs, rhs },
            }) => {
                assert!(matches!(op, BinaryOp::NEQ));
                assert!(matches!(**lhs, Expression::Constant(1)));
                assert!(matches!(**rhs, Expression::Constant(2)));
            }
            _ => panic!("Expected not equal comparison"),
        }
    }

    #[test]
    fn test_parse_comparison_precedence() {
        // 1 + 2 < 3 * 4 should be parsed as (1 + 2) < (3 * 4)
        let input = "int main(void) { return 1 + 2 < 3 * 4; }";
        let result = parse_program(input);
        assert!(result.is_ok());
        let program = result.unwrap();
        match &program.functions[0].body.statements[0] {
            Statement::Return(ReturnStmt {
                expr: Expression::Binary { op, lhs, rhs },
            }) => {
                // Top level should be less than
                assert!(matches!(op, BinaryOp::LT));

                // Left side should be (1 + 2)
                match &**lhs {
                    Expression::Binary { op, lhs, rhs } => {
                        assert!(matches!(op, BinaryOp::Add));
                        assert!(matches!(**lhs, Expression::Constant(1)));
                        assert!(matches!(**rhs, Expression::Constant(2)));
                    }
                    _ => panic!("Expected addition on left side"),
                }

                // Right side should be (3 * 4)
                match &**rhs {
                    Expression::Binary { op, lhs, rhs } => {
                        assert!(matches!(op, BinaryOp::Multiply));
                        assert!(matches!(**lhs, Expression::Constant(3)));
                        assert!(matches!(**rhs, Expression::Constant(4)));
                    }
                    _ => panic!("Expected multiplication on right side"),
                }
            }
            _ => panic!("Expected binary expression"),
        }
    }

    #[test]
    fn test_parse_comparison_chaining() {
        // Test that 1 < 2 == 3 < 4 parses as (1 < 2) == (3 < 4)
        let input = "int main(void) { return 1 < 2 == 3 < 4; }";
        let result = parse_program(input);
        assert!(result.is_ok());
        let program = result.unwrap();
        match &program.functions[0].body.statements[0] {
            Statement::Return(ReturnStmt {
                expr: Expression::Binary { op, lhs, rhs },
            }) => {
                // Top level should be equality
                assert!(matches!(op, BinaryOp::EQ));

                // Left side should be (1 < 2)
                match &**lhs {
                    Expression::Binary { op, .. } => {
                        assert!(matches!(op, BinaryOp::LT));
                    }
                    _ => panic!("Expected less than on left side"),
                }

                // Right side should be (3 < 4)
                match &**rhs {
                    Expression::Binary { op, .. } => {
                        assert!(matches!(op, BinaryOp::LT));
                    }
                    _ => panic!("Expected less than on right side"),
                }
            }
            _ => panic!("Expected binary expression"),
        }
    }

    // === Error Cases (Parser-specific) ===

    #[test]
    fn test_parse_error_missing_type() {
        let input = "main(void) { return 0; }";
        let result = parse_program(input);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(matches!(err.error, ParseError::UnexpectedToken { .. }));
    }

    #[test]
    fn test_parse_error_missing_semicolon() {
        let input = "int main(void) { return 0 }";
        let result = parse_program(input);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(matches!(err.error, ParseError::UnexpectedToken { .. }));
    }

    #[test]
    fn test_parse_error_missing_closing_brace() {
        let input = "int main(void) { return 0;";
        let result = parse_program(input);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(matches!(err.error, ParseError::UnexpectedEof { .. }));
    }

    #[test]
    fn test_parse_error_missing_expression() {
        let input = "int main(void) { return; }";
        let result = parse_program(input);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(matches!(err.error, ParseError::UnexpectedToken { .. }));
    }

    #[test]
    fn test_parse_error_unexpected_eof_in_statement() {
        let input = "int main(void) { return";
        let result = parse_program(input);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(matches!(err.error, ParseError::UnexpectedEof { .. }));
    }

    #[test]
    fn test_parse_error_empty_input() {
        let input = "";
        let result = parse_program(input);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(matches!(err.error, ParseError::UnexpectedEof { .. }));
    }

    #[test]
    fn test_parse_error_missing_opening_paren() {
        let input = "int main void) { return 0; }";
        let result = parse_program(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_error_missing_closing_paren() {
        let input = "int main(void { return 0; }";
        let result = parse_program(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_error_missing_opening_brace() {
        let input = "int main(void) return 0; }";
        let result = parse_program(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_error_unexpected_token_in_block() {
        let input = "int main(void) { int x; }";
        let result = parse_program(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_error_incomplete_function() {
        let input = "int main(void)";
        let result = parse_program(input);
        assert!(result.is_err());
    }

    // === While Statement Tests ===

    #[test]
    fn test_parse_while_statement() {
        let input = "int main(void) { while (1) { return 0; } }";
        let result = parse_program(input);
        assert!(result.is_ok());
        let program = result.unwrap();
        match &program.functions[0].body.statements[0] {
            Statement::While(WhileStmt { cond, body }) => {
                assert!(matches!(*cond, Expression::Constant(1)));
                match &**body {
                    Statement::Block(block) => {
                        assert_eq!(block.statements.len(), 1);
                    }
                    _ => panic!("Expected block statement in while body"),
                }
            }
            _ => panic!("Expected while statement"),
        }
    }

    #[test]
    fn test_parse_while_with_comparison() {
        let input = "int main(void) { while (x < 10) { return x; } }";
        let result = parse_program(input);
        assert!(result.is_ok());
        let program = result.unwrap();
        match &program.functions[0].body.statements[0] {
            Statement::While(WhileStmt { cond, .. }) => match cond {
                Expression::Binary { op, lhs, rhs } => {
                    assert!(matches!(op, BinaryOp::LT));
                    assert!(matches!(&**lhs, Expression::Variable(name) if name == "x"));
                    assert!(matches!(&**rhs, Expression::Constant(10)));
                }
                _ => panic!("Expected binary comparison in while condition"),
            },
            _ => panic!("Expected while statement"),
        }
    }

    #[test]
    fn test_parse_while_nested() {
        let input = "int main(void) { while (1) { while (2) { return 0; } } }";
        let result = parse_program(input);
        assert!(result.is_ok());
        let program = result.unwrap();
        match &program.functions[0].body.statements[0] {
            Statement::While(WhileStmt { body, .. }) => match &**body {
                Statement::Block(block) => {
                    assert_eq!(block.statements.len(), 1);
                    assert!(matches!(&block.statements[0], Statement::While(_)));
                }
                _ => panic!("Expected block in while body"),
            },
            _ => panic!("Expected while statement"),
        }
    }

    #[test]
    fn test_parse_while_error_missing_paren() {
        let input = "int main(void) { while 1) { return 0; } }";
        let result = parse_program(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_while_error_missing_closing_paren() {
        let input = "int main(void) { while (1 { return 0; } }";
        let result = parse_program(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_while_error_missing_body() {
        let input = "int main(void) { while (1) }";
        let result = parse_program(input);
        assert!(result.is_err());
    }

    // === Do-While Statement Tests ===

    #[test]
    fn test_parse_do_while_statement() {
        let input = "int main(void) { do { return 0; } while (1); }";
        let result = parse_program(input);
        assert!(result.is_ok());
        let program = result.unwrap();
        match &program.functions[0].body.statements[0] {
            Statement::DoWhile(DoWhileStmt { body, cond }) => {
                assert!(matches!(*cond, Expression::Constant(1)));
                match &**body {
                    Statement::Block(block) => {
                        assert_eq!(block.statements.len(), 1);
                    }
                    _ => panic!("Expected block statement in do-while body"),
                }
            }
            _ => panic!("Expected do-while statement"),
        }
    }

    #[test]
    fn test_parse_do_while_with_comparison() {
        let input = "int main(void) { do { return x; } while (x < 10); }";
        let result = parse_program(input);
        assert!(result.is_ok());
        let program = result.unwrap();
        match &program.functions[0].body.statements[0] {
            Statement::DoWhile(DoWhileStmt { cond, .. }) => match cond {
                Expression::Binary { op, lhs, rhs } => {
                    assert!(matches!(op, BinaryOp::LT));
                    assert!(matches!(&**lhs, Expression::Variable(name) if name == "x"));
                    assert!(matches!(&**rhs, Expression::Constant(10)));
                }
                _ => panic!("Expected binary comparison in do-while condition"),
            },
            _ => panic!("Expected do-while statement"),
        }
    }

    #[test]
    fn test_parse_do_while_nested() {
        let input = "int main(void) { do { do { return 0; } while (2); } while (1); }";
        let result = parse_program(input);
        assert!(result.is_ok());
        let program = result.unwrap();
        match &program.functions[0].body.statements[0] {
            Statement::DoWhile(DoWhileStmt { body, .. }) => match &**body {
                Statement::Block(block) => {
                    assert_eq!(block.statements.len(), 1);
                    assert!(matches!(&block.statements[0], Statement::DoWhile(_)));
                }
                _ => panic!("Expected block in do-while body"),
            },
            _ => panic!("Expected do-while statement"),
        }
    }

    #[test]
    fn test_parse_do_while_error_missing_while() {
        let input = "int main(void) { do { return 0; } (1); }";
        let result = parse_program(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_do_while_error_missing_paren() {
        let input = "int main(void) { do { return 0; } while 1); }";
        let result = parse_program(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_do_while_error_missing_closing_paren() {
        let input = "int main(void) { do { return 0; } while (1; }";
        let result = parse_program(input);
        assert!(result.is_err());
    }

    // === If Statement Tests ===

    #[test]
    fn test_parse_if_statement() {
        let input = "int main(void) { if (1) { return 0; } }";
        let result = parse_program(input);
        assert!(result.is_ok());
        let program = result.unwrap();
        match &program.functions[0].body.statements[0] {
            Statement::If(IfStmt {
                cond,
                then_block,
                else_block,
            }) => {
                assert!(matches!(*cond, Expression::Constant(1)));
                match &**then_block {
                    Statement::Block(block) => {
                        assert_eq!(block.statements.len(), 1);
                    }
                    _ => panic!("Expected block in if then_block"),
                }
                assert!(else_block.is_none());
            }
            _ => panic!("Expected if statement"),
        }
    }

    #[test]
    fn test_parse_if_with_comparison() {
        let input = "int main(void) { if (x > 5) { return x; } }";
        let result = parse_program(input);
        assert!(result.is_ok());
        let program = result.unwrap();
        match &program.functions[0].body.statements[0] {
            Statement::If(IfStmt { cond, .. }) => match cond {
                Expression::Binary { op, lhs, rhs } => {
                    assert!(matches!(op, BinaryOp::GT));
                    assert!(matches!(&**lhs, Expression::Variable(name) if name == "x"));
                    assert!(matches!(&**rhs, Expression::Constant(5)));
                }
                _ => panic!("Expected binary comparison in if condition"),
            },
            _ => panic!("Expected if statement"),
        }
    }

    #[test]
    fn test_parse_if_else_statement() {
        let input = "int main(void) { if (1) { return 0; } else { return 1; } }";
        let result = parse_program(input);
        assert!(result.is_ok());
        let program = result.unwrap();
        match &program.functions[0].body.statements[0] {
            Statement::If(IfStmt {
                cond,
                then_block,
                else_block,
            }) => {
                assert!(matches!(*cond, Expression::Constant(1)));
                match &**then_block {
                    Statement::Block(block) => {
                        assert_eq!(block.statements.len(), 1);
                    }
                    _ => panic!("Expected block in if then_block"),
                }
                assert!(else_block.is_some());
                match else_block.as_ref().unwrap().as_ref() {
                    Statement::Block(block) => {
                        assert_eq!(block.statements.len(), 1);
                    }
                    _ => panic!("Expected block in else block"),
                }
            }
            _ => panic!("Expected if statement"),
        }
    }

    #[test]
    fn test_parse_if_else_if_chain() {
        let input = "int main(void) { if (x < 0) { return -1; } else if (x > 0) { return 1; } else { return 0; } }";
        let result = parse_program(input);
        assert!(result.is_ok());
        let program = result.unwrap();
        match &program.functions[0].body.statements[0] {
            Statement::If(IfStmt { else_block, .. }) => {
                assert!(else_block.is_some());
                // The else block should contain another if statement
                match else_block.as_ref().unwrap().as_ref() {
                    Statement::If(inner_if) => {
                        assert!(inner_if.else_block.is_some());
                    }
                    _ => panic!("Expected if statement in else block"),
                }
            }
            _ => panic!("Expected if statement"),
        }
    }

    #[test]
    fn test_parse_if_nested() {
        let input = "int main(void) { if (1) { if (2) { return 0; } } }";
        let result = parse_program(input);
        assert!(result.is_ok());
        let program = result.unwrap();
        match &program.functions[0].body.statements[0] {
            Statement::If(IfStmt { then_block, .. }) => match &**then_block {
                Statement::Block(block) => {
                    assert_eq!(block.statements.len(), 1);
                    assert!(matches!(&block.statements[0], Statement::If(_)));
                }
                _ => panic!("Expected block in if then_block"),
            },
            _ => panic!("Expected if statement"),
        }
    }

    #[test]
    fn test_parse_if_error_missing_paren() {
        let input = "int main(void) { if 1) { return 0; } }";
        let result = parse_program(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_if_error_missing_closing_paren() {
        let input = "int main(void) { if (1 { return 0; } }";
        let result = parse_program(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_if_error_missing_body() {
        let input = "int main(void) { if (1) }";
        let result = parse_program(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_if_else_error_missing_else_body() {
        let input = "int main(void) { if (1) { return 0; } else }";
        let result = parse_program(input);
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_continue_and_break() {
        let input = "int main(void) { continue; break; }";
        let result = parse_program(input);
        assert!(result.is_ok());
    }
}
