use std::sync::LazyLock;

use crate::{
    lexer_base::{
        error::LexError,
        token::{ALL_KEYWORDS, Token},
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
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Lexer { source, idx: 0 }
    }

    /// Returns the remaining part of the source string.
    fn remaining(&self) -> &'a str {
        &self.source[self.idx..]
    }

    /// Trims leading ASCII whitespace from the remaining source string.
    fn trim_ascii_start(&mut self) {
        if let Some(additional_idx) = self.remaining().find(|c: char| !c.is_ascii_whitespace()) {
            self.idx += additional_idx;
        }
    }

    fn next_symbolic_token(&self) -> Option<Token<'a>> {
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

        let current_idx = self.idx;

        // Symbols : single char
        if let Some(t) = self.next_symbolic_token() {
            self.idx += t.ascii_length();
            return Some(Ok(t));
        }

        // others
        if let Some(m) = WORD_REGEX.find(self.remaining()) {
            for kw in ALL_KEYWORDS {
                if m.as_str() == kw.as_str() {
                    self.idx += m.len();
                    return Some(Ok(Token::Keyword(*kw)));
                }
            }

            // constants?
            if let Some(m) = INTEGER_REGEX.find(m.as_str()) {
                let constant = m.as_str().parse::<i32>().unwrap();
                self.idx += m.len();
                return Some(Ok(Token::Constant(constant)));
            }

            // identifiers
            if let Some(m) = IDENTIFIER_REGEX.find(m.as_str()) {
                self.idx += m.len();
                return Some(Ok(Token::identifier(m.as_str())));
            }

            // Invalid token format (word that doesn't match any pattern)
            let token_str = m.as_str().to_string();
            self.idx += m.len();
            return Some(Err(LexError::InvalidTokenFormat(token_str, current_idx)));
        }

        let unexpected_char = self.remaining().chars().next().unwrap();
        self.idx += unexpected_char.len_utf8();
        Some(Err(LexError::UnexpectedCharacter(
            unexpected_char,
            current_idx,
        )))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_lexer_success(
        input: &str,
        expected_tokens: Vec<Token<'static>>,
    ) -> Result<(), LexError> {
        for (token, expected) in Lexer::new(input).zip(expected_tokens) {
            assert!(token.is_ok());
            assert_eq!(token.unwrap(), expected);
        }

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
        test_lexer_success("return 3;", vec![t!("return"), Token::Constant(3), t!(";")]).unwrap();
    }

    #[test]
    fn test_lexer_case_identifier_with_digits() {
        test_lexer_success("int3", vec![Token::identifier("int3")]).unwrap();
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
        test_lexer_success("_123", vec![Token::identifier("_123")]).unwrap();
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
