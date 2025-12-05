use std::borrow::Cow;
use std::iter::Peekable;

use crate::{
    error::IntoCompilerError,
    lexer_base::{CompilerLexError, Lexer, Span, Token, TokenType},
    parser_base::{
        Block, CompilerParseError, Expression, FuncDef, ParseError, Program, Statement,
        grammar::Type,
    },
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
        while self.lexer.peek().is_some() {
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
        let body = self.parse_block()?;

        Ok(FuncDef {
            return_type,
            name,
            params: Vec::new(),
            body,
        })
    }

    /// Parse a block: { statement* }
    fn parse_block(&mut self) -> Result<Block<'a>, CompilerParseError> {
        self.expect(t!("{"))?;

        let mut statements = Vec::new();

        // Parse statements until we hit '}'
        loop {
            match self.lexer.peek() {
                Some(Ok(Token { kind: t!("}"), .. })) => break,
                Some(Err(err)) => return Err(err.clone().convert_error::<ParseError>()),
                Some(Ok(_)) => {
                    statements.push(self.parse_statement()?);
                }
                None => {
                    return Err(ParseError::unexpected_eof_with_message("'}' or statement")
                        .with_span(self.eof_span));
                }
            }
        }

        self.expect(t!("}"))?;

        Ok(Block { statements })
    }

    /// Parse a statement
    fn parse_statement(&mut self) -> Result<Statement<'a>, CompilerParseError> {
        match self
            .lexer
            .peek()
            .map(|t| t.as_ref())
            .transpose()
            .map_err(|e| e.clone().convert_error())?
        {
            Some(Token {
                kind: t!("return"), ..
            }) => self.parse_return_statement(),
            Some(token) => {
                Err(ParseError::expected_statement(token.kind.clone()).with_span(token.span))
            }
            None => Err(ParseError::expected_statement_eof().with_span(self.eof_span)),
        }
    }

    /// Parse a return statement: return expr;
    fn parse_return_statement(&mut self) -> Result<Statement<'a>, CompilerParseError> {
        self.expect(t!("return"))?;
        let expr = self.parse_expression()?;
        self.expect(t!(";"))?;
        Ok(Statement::Return { expr })
    }

    /// Parse an expression
    fn parse_expression(&mut self) -> Result<Expression<'a>, CompilerParseError> {
        match self
            .lexer
            .next()
            .transpose()
            .map_err(|e| e.convert_error::<ParseError>())?
        {
            Some(Token {
                kind: TokenType::Constant(value),
                ..
            }) => Ok(Expression::Constant(value)),
            Some(Token {
                kind: TokenType::Identifier(name),
                ..
            }) => Ok(Expression::Variable(name)),
            Some(token) => Err(ParseError::expected_expression(token.kind).with_span(token.span)),
            None => Err(ParseError::expected_expression_eof().with_span(self.eof_span)),
        }
    }

    /// Expect a specific token
    fn expect(&mut self, expected: TokenType<'static>) -> Result<(), CompilerParseError> {
        match self
            .lexer
            .next()
            .transpose()
            .map_err(|e| e.convert_error())?
        {
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
        match self
            .lexer
            .next()
            .transpose()
            .map_err(|e| e.convert_error::<ParseError>())?
        {
            Some(token) if matches!(token.kind, TokenType::Identifier(_)) => {
                if let TokenType::Identifier(name) = token.kind {
                    Ok(name)
                } else {
                    unreachable!()
                }
            }
            Some(token) => Err(ParseError::expected_identifier(token.kind).with_span(token.span)),
            None => Err(ParseError::expected_identifier_eof().with_span(self.eof_span)),
        }
    }

    /// Optionally consume a token if it matches
    fn expect_optional(
        &mut self,
        expected: TokenType<'static>,
    ) -> Result<bool, CompilerParseError> {
        match self.lexer.peek() {
            Some(Ok(token)) if token.kind == expected => {
                self.lexer.next(); // Consume it
                Ok(true)
            }
            Some(Ok(_)) => {
                // Token doesn't match, don't consume
                Ok(false)
            }
            Some(Err(_)) => {
                // Consume and return the error
                match self.lexer.next() {
                    Some(Err(err)) => Err(err.convert_error::<ParseError>()),
                    _ => unreachable!("next and peek should return same result"),
                }
            }
            None => Ok(false), // End of input
        }
    }

    fn expect_type(&mut self) -> Result<Type, CompilerParseError> {
        match self.lexer.peek() {
            Some(Ok(Token {
                kind: t!("int"), ..
            })) => {
                self.lexer.next();
                Ok(Type::Int)
            }
            Some(Ok(Token {
                kind: t!("void"), ..
            })) => {
                self.lexer.next();
                Ok(Type::Void)
            }
            Some(Ok(t)) => Err(ParseError::expected_type(t.kind.clone()).with_span(t.span)),
            _ => Err(ParseError::expected_type_eof().with_span(self.eof_span)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer_base::Lexer;
    use crate::parser_base::grammar::Type;

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
            Statement::Return {
                expr: Expression::Constant(42),
            } => {}
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
            Statement::Return {
                expr: Expression::Constant(999),
            } => {}
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
            Statement::Return {
                expr: Expression::Variable(name),
            } if name == "foo" => {}
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
}
