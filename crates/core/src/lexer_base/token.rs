use std::borrow::Cow;

/// Static tokens (keywords and symbols with fixed string representation)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StaticToken {
    // Keywords
    Int,
    Void,
    Return,
    If,
    Else,
    While,
    Do,

    // Symbols
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,

    // Operators
    Minus,
    Plus,
    Star,
    Slash,
    Not,

    // Comparison operators
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    EqualEqual,
    NotEqual,
}

impl StaticToken {
    pub const fn as_str(&self) -> &'static str {
        match self {
            StaticToken::Int => "int",
            StaticToken::Void => "void",
            StaticToken::Return => "return",
            StaticToken::If => "if",
            StaticToken::Else => "else",
            StaticToken::While => "while",
            StaticToken::Do => "do",

            StaticToken::Semicolon => ";",
            StaticToken::LParen => "(",
            StaticToken::RParen => ")",
            StaticToken::LBrace => "{",
            StaticToken::RBrace => "}",
            StaticToken::Minus => "-",
            StaticToken::Plus => "+",
            StaticToken::Star => "*",
            StaticToken::Slash => "/",
            StaticToken::Not => "!",

            StaticToken::LessThan => "<",
            StaticToken::GreaterThan => ">",
            StaticToken::LessThanOrEqual => "<=",
            StaticToken::GreaterThanOrEqual => ">=",
            StaticToken::EqualEqual => "==",
            StaticToken::NotEqual => "!=",
        }
    }
}

pub const ALL_KEYWORDS: &[StaticToken] =
    &[StaticToken::Int, StaticToken::Void, StaticToken::Return];

/// Span represents the location information of a token in the source code
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    /// byte offset in source (inclusive)
    pub start: usize,
    /// byte offset in source (exclusive)
    pub end: usize,
    /// 1-based line number
    pub line: usize,
    /// 1-based column number (at start of token)
    pub column: usize,
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

    /// Returns a `Span` representing the end-of-file position of the given
    /// source.
    pub fn eof(src: &str) -> Self {
        let len = src.len();

        let mut line = 1;
        let mut col = 1;

        for b in src.bytes() {
            if b == b'\n' {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
        }

        Self {
            start: len,
            end: len,
            line,
            column: col,
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

    /// Create a constant-type token
    pub fn constant(value: i32, span: Span) -> Self {
        Self::new(TokenType::Constant(value), span)
    }

    /// Create an identifier-type token
    pub fn identifier(value: &'a str, span: Span) -> Self {
        Self::new(TokenType::identifier(value), span)
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

    // Static tokens (keywords and symbols)
    Static(StaticToken),
}

impl<'a> TokenType<'a> {
    pub fn identifier(input: &'a str) -> Self {
        TokenType::Identifier(Cow::Borrowed(input))
    }

    pub fn ascii_length(&self) -> usize {
        match self {
            TokenType::Identifier(s) => s.len(),
            TokenType::Constant(n) => n.to_string().len(),
            TokenType::Static(st) => st.as_str().len(),
        }
    }
}

impl<'a> std::fmt::Display for TokenType<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::Identifier(name) => write!(f, "identifier '{}'", name),
            TokenType::Constant(value) => write!(f, "constant {}", value),
            TokenType::Static(st) => write!(f, "'{}'", st.as_str()),
        }
    }
}
