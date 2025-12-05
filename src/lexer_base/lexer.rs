use std::sync::LazyLock;

use crate::{
    lexer_base::{
        error::LexError,
        token::{ALL_KEYWORDS, Span, Token, TokenType},
    },
    t,
};
use regex::Regex;

static WORD_REGEX: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^\w+").unwrap());
static INTEGER_REGEX: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^[0-9]+$").unwrap());
static IDENTIFIER_REGEX: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^[a-zA-Z_]\w*$").unwrap());

#[derive(Clone)]
pub struct Lexer<'a> {
    source: &'a str,
    idx: usize,
    line: usize,   // 1-based line number
    column: usize, // 1-based column number
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Lexer {
            source,
            idx: 0,
            line: 1,
            column: 1,
        }
    }

    /// Returns the remaining part of the source string.
    fn remaining(&self) -> &'a str {
        &self.source[self.idx..]
    }

    /// Trims leading ASCII whitespace from the remaining source string.
    /// Updates line and column tracking.
    fn trim_ascii_start(&mut self) {
        for ch in self.remaining().chars() {
            if !ch.is_ascii_whitespace() {
                break;
            }
            self.idx += ch.len_utf8();
            if ch == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
        }
    }

    /// Advances the lexer position by the given length and updates line/column
    fn advance(&mut self, len: usize) {
        let text = &self.source[self.idx..self.idx + len];
        for ch in text.chars() {
            self.idx += ch.len_utf8();
            if ch == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
        }
    }

    fn next_symbolic_token(&self) -> Option<TokenType<'a>> {
        match self.remaining().chars().next() {
            Some(';') => Some(t!(";")),
            Some('{') => Some(t!("{")),
            Some('}') => Some(t!("}")),
            Some('(') => Some(t!("(")),
            Some(')') => Some(t!(")")),
            _ => None,
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.trim_ascii_start();

        if self.remaining().is_empty() {
            return None;
        }

        let start_idx = self.idx;
        let start_line = self.line;
        let start_column = self.column;

        // Symbols : single char
        if let Some(kind) = self.next_symbolic_token() {
            let len = kind.ascii_length();
            let end_idx = self.idx + len;
            self.advance(len);
            let span = Span::new(start_idx, end_idx, start_line, start_column);
            return Some(Ok(Token::new(kind, span)));
        }

        // others
        if let Some(m) = WORD_REGEX.find(self.remaining()) {
            for kw in ALL_KEYWORDS {
                if m.as_str() == kw.as_str() {
                    let len = m.len();
                    let end_idx = self.idx + len;
                    self.advance(len);
                    let span = Span::new(start_idx, end_idx, start_line, start_column);
                    return Some(Ok(Token::new(kw.clone(), span)));
                }
            }

            // constants?
            if let Some(m) = INTEGER_REGEX.find(m.as_str()) {
                let constant = m.as_str().parse::<i32>().unwrap();
                let len = m.len();
                let end_idx = self.idx + len;
                self.advance(len);
                let span = Span::new(start_idx, end_idx, start_line, start_column);
                return Some(Ok(Token::new(TokenType::Constant(constant), span)));
            }

            // identifiers
            if let Some(m) = IDENTIFIER_REGEX.find(m.as_str()) {
                let len = m.len();
                let end_idx = self.idx + len;
                self.advance(len);
                let span = Span::new(start_idx, end_idx, start_line, start_column);
                return Some(Ok(Token::new(TokenType::identifier(m.as_str()), span)));
            }

            // Invalid token format (word that doesn't match any pattern)
            let token_str = m.as_str().to_string();
            self.advance(m.len());
            return Some(Err(LexError::InvalidTokenFormat(token_str, start_idx)));
        }

        let unexpected_char = self.remaining().chars().next().unwrap();
        self.advance(unexpected_char.len_utf8());
        Some(Err(LexError::UnexpectedCharacter(
            unexpected_char,
            start_idx,
        )))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_lexer_success(
        input: &str,
        expected_kinds: Vec<TokenType<'static>>,
    ) -> Result<(), LexError> {
        let tokens: Vec<Token> = Lexer::new(input).collect::<Result<Vec<_>, _>>()?;
        let kinds: Vec<TokenType> = tokens.iter().map(|t| t.kind.clone()).collect();
        assert_eq!(kinds, expected_kinds);
        Ok(())
    }

    fn test_lexer_fail(input: &str) {
        assert!(
            Lexer::new(input)
                .collect::<Result<Vec<Token>, LexError>>()
                .is_err()
        )
    }

    #[test]
    fn test_lexer_case_constant_and_semicolon_are_adjacent() {
        test_lexer_success(
            "return 3;",
            vec![t!("return"), TokenType::Constant(3), t!(";")],
        )
        .unwrap();
    }

    #[test]
    fn test_lexer_case_identifier_with_digits() {
        test_lexer_success("int3", vec![TokenType::identifier("int3")]).unwrap();
    }

    #[test]
    fn test_lexer_case_fail_for_unprocessable_identifier() {
        test_lexer_fail("123abc");
    }

    #[test]
    fn test_lexer_error_unexpected_character() {
        let result: Result<Vec<Token>, LexError> = Lexer::new("int main() { return @; }").collect();
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert_eq!(err, LexError::UnexpectedCharacter('@', 20));
    }

    #[test]
    fn test_lexer_error_unexpected_character_at_start() {
        let result: Result<Vec<Token>, LexError> = Lexer::new("@invalid").collect();
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert_eq!(err, LexError::UnexpectedCharacter('@', 0));
    }

    #[test]
    fn test_lexer_error_invalid_token_format() {
        let result: Result<Vec<Token>, LexError> = Lexer::new("123abc").collect();
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert_eq!(err, LexError::InvalidTokenFormat("123abc".to_string(), 0));
    }

    #[test]
    fn test_lexer_valid_identifier_with_underscore_prefix() {
        // _123 is actually a valid identifier (starts with underscore, contains digits)
        test_lexer_success("_123", vec![TokenType::identifier("_123")]).unwrap();
    }

    #[test]
    fn test_lexer_error_multiple_special_chars() {
        let result: Result<Vec<Token>, LexError> = Lexer::new("int main() #$").collect();
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert_eq!(err, LexError::UnexpectedCharacter('#', 11));
    }

    #[test]
    fn test_lexer_error_position_after_whitespace() {
        let result: Result<Vec<Token>, LexError> = Lexer::new("int   @").collect();
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert_eq!(err, LexError::UnexpectedCharacter('@', 6));
    }
}
