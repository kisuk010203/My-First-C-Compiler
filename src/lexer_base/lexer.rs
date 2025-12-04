use std::sync::LazyLock;

use crate::{lexer_base::token::Token, t};
use regex::Regex;

static INTEGER_REGEX: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\A^[0-9]+").unwrap());
static IDENTIFIER_REGEX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"\A^[a-zA-Z_]\w*").unwrap());

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
    fn remaining(&self) -> &str {
        &self.source[self.idx..]
    }

    /// Trims leading ASCII whitespace from the remaining source string.
    fn trim_ascii_start(&mut self) {
        if let Some(additional_idx) = self.remaining().find(|c: char| !c.is_ascii_whitespace()) {
            self.idx += additional_idx;
        }
    }

    /// Returns the next chunk of the source string.
    /// If empty or remaining characters are all whitespace, returns None.
    fn next_non_whitespace_chunk(&self) -> Option<&'a str> {
        self.remaining()
            .find(|c: char| !c.is_ascii_whitespace())
            .map(|idx| &self.source[self.idx..self.idx + idx])
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

        // Symbols : single char
        if let Some(t) = self.next_symbolic_token() {
            self.idx += t.ascii_length();
            return Some(Ok(t));
        }

        match self.next_non_whitespace_chunk() {
            Some("int") => Some(Ok(t!("int"))),
            Some("void") => Some(Ok(t!("void"))),
            Some("return") => Some(Ok(t!("return"))),
            Some(number_string) if INTEGER_REGEX.is_match(number_string) => {
                self.idx += number_string.len();
                let parsed = number_string.parse::<i32>().unwrap(); // asserted to not panic
                Some(Ok(Token::Constant(parsed)))
            }
            Some(identifier) if IDENTIFIER_REGEX.is_match(identifier) => {
                self.idx += identifier.len();
                Some(Ok(Token::identifier(identifier)))
            }
            Some(else_chunk) => Some(Err(format!("Unexpected token: {}", else_chunk))),
            None => None,
        }
    }
}
