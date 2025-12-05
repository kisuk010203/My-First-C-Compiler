use std::borrow::Cow;

pub const ALL_KEYWORDS: &[TokenType] = &[TokenType::Int, TokenType::Void, TokenType::Return];

/// Span represents the location information of a token in the source code
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,  // byte offset in source (inclusive)
    pub end: usize,    // byte offset in source (exclusive)
    pub line: usize,   // 1-based line number
    pub column: usize, // 1-based column number (at start of token)
}

impl Span {
    pub fn new(start: usize, end: usize, line: usize, column: usize) -> Self {
        Self {
            start,
            end,
            line,
            column,
        }
    }
}

/// Token represents a lexical token with its type and location
#[derive(Debug, Clone)]
pub struct Token<'a> {
    pub kind: TokenType<'a>,
    pub span: Span,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenType<'a>, span: Span) -> Self {
        Self { kind, span }
    }
}

/// For testing convenience, compare only the kind, ignoring span
impl<'a> PartialEq for Token<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType<'a> {
    // Identifiers and literals
    Identifier(Cow<'a, str>), // [a-zA-Z_]\w*
    Constant(i32),            // [0-9]+

    // Keywords
    Int,
    Void,
    Return,

    // Symbols
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
}

impl<'a> TokenType<'a> {
    pub fn identifier(input: &'a str) -> Self {
        TokenType::Identifier(Cow::Borrowed(input))
    }

    pub fn as_str(&self) -> &str {
        match self {
            TokenType::Int => "int",
            TokenType::Void => "void",
            TokenType::Return => "return",
            TokenType::Semicolon => ";",
            TokenType::LParen => "(",
            TokenType::RParen => ")",
            TokenType::LBrace => "{",
            TokenType::RBrace => "}",
            TokenType::Identifier(s) => s.as_ref(),
            TokenType::Constant(_) => {
                // Note: This returns a reference to a temporary string
                // For constants, ascii_length() should be used instead
                unreachable!("as_str() should not be called on Constant tokens")
            }
        }
    }

    pub fn ascii_length(&self) -> usize {
        match self {
            TokenType::Identifier(s) => s.len(),
            TokenType::Constant(n) => n.to_string().len(),
            TokenType::Int => 3,
            TokenType::Void => 4,
            TokenType::Return => 6,
            TokenType::Semicolon
            | TokenType::LParen
            | TokenType::RParen
            | TokenType::LBrace
            | TokenType::RBrace => 1,
        }
    }
}
