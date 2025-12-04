use std::sync::LazyLock;

use crate::{
    lexer_base::token::{ALL_KEYWORDS, Token},
    t,
};
use regex::Regex;

static INTEGER_REGEX: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^[0-9]+").unwrap());
static IDENTIFIER_REGEX: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^[a-zA-Z_]\w*").unwrap());

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
    type Item = Result<Token<'a>, String>;

    fn next(&mut self) -> Option<Self::Item> {
        self.trim_ascii_start();

        if self.remaining().is_empty() {
            return None;
        }

        // Symbols : single char
        if let Some(t) = self.next_symbolic_token() {
            self.idx += t.ascii_length();
            return Some(Ok(t));
        }

        // keywords
        for kw in ALL_KEYWORDS {
            if self.remaining().starts_with(kw.as_str()) {
                self.idx += kw.as_str().len();
                return Some(Ok(Token::Keyword(*kw)));
            }
        }

        // constants?
        if let Some(m) = INTEGER_REGEX.find(self.remaining()) {
            let constant = m.as_str().parse::<i32>().unwrap();
            self.idx += m.len();
            return Some(Ok(Token::Constant(constant)));
        }

        // identifiers?
        if let Some(m) = IDENTIFIER_REGEX.find(self.remaining()) {
            let identifier = m.as_str();
            self.idx += identifier.len();
            return Some(Ok(Token::identifier(identifier)));
        }

        Some(Err(format!(
            "Unexpected character: {}",
            self.remaining().chars().next().unwrap()
        )))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_lexer_success(input: &str, expected_tokens: Vec<Token<'static>>) {
        for (token, expected) in Lexer::new(input).zip(expected_tokens) {
            assert!(token.is_ok());
            assert_eq!(token.unwrap(), expected);
        }
    }

    #[test]
    fn test_lexer_case_constant_and_semicolon_are_adjacent() {
        test_lexer_success("return 3;", vec![t!("return"), Token::Constant(3), t!(";")]);
    }
}
