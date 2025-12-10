use std::borrow::Cow;

/// Static tokens (keywords and symbols with fixed string representation)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StaticToken {
    // Keywords
    Break,
    Continue,
    Do,
    Else,
    For,
    If,
    Int,
    Return,
    Void,
    While,

    // Grouping & Delimiters
    Semicolon, // ;
    LParen,    // (
    RParen,    // )
    LBrace,    // {
    RBrace,    // }
    LBracket,  // [
    RBracket,  // ]
    Comma,     // ,
    Colon,     // :
    Question,  // ?

    // Arithmetic operators
    Plus,    // +
    Minus,   // -
    Star,    // *
    Slash,   // /
    Percent, // %

    // Bitwise operators
    Ampersand,  // &
    Pipe,       // |
    Caret,      // ^
    Tilde,      // ~
    LeftShift,  // <<
    RightShift, // >>

    // Logical operators
    Not,        // !
    LogicalAnd, // &&
    LogicalOr,  // ||

    // Comparison operators
    LessThan,           // <
    GreaterThan,        // >
    LessThanOrEqual,    // <=
    GreaterThanOrEqual, // >=
    EqualEqual,         // ==
    NotEqual,           // !=

    // Assignment operators
    Equal,           // =
    PlusEqual,       // +=
    MinusEqual,      // -=
    StarEqual,       // *=
    SlashEqual,      // /=
    PercentEqual,    // %=
    AmpersandEqual,  // &=
    PipeEqual,       // |=
    CaretEqual,      // ^=
    LeftShiftEqual,  // <<=
    RightShiftEqual, // >>=

    // Increment/Decrement
    PlusPlus,   // ++
    MinusMinus, // --

    // Member access
    Dot,   // .
    Arrow, // ->

    // Other
    Ellipsis,   // ...
    Hash,       // #
    DoubleHash, // ##
}

impl StaticToken {
    pub const fn as_str(&self) -> &'static str {
        match self {
            // Keywords
            StaticToken::Break => "break",
            StaticToken::Continue => "continue",
            StaticToken::Do => "do",
            StaticToken::Else => "else",
            StaticToken::For => "for",
            StaticToken::If => "if",
            StaticToken::Int => "int",
            StaticToken::Return => "return",
            StaticToken::Void => "void",
            StaticToken::While => "while",

            // Grouping & Delimiters
            StaticToken::Semicolon => ";",
            StaticToken::LParen => "(",
            StaticToken::RParen => ")",
            StaticToken::LBrace => "{",
            StaticToken::RBrace => "}",
            StaticToken::LBracket => "[",
            StaticToken::RBracket => "]",
            StaticToken::Comma => ",",
            StaticToken::Colon => ":",
            StaticToken::Question => "?",

            // Arithmetic operators
            StaticToken::Plus => "+",
            StaticToken::Minus => "-",
            StaticToken::Star => "*",
            StaticToken::Slash => "/",
            StaticToken::Percent => "%",

            // Bitwise operators
            StaticToken::Ampersand => "&",
            StaticToken::Pipe => "|",
            StaticToken::Caret => "^",
            StaticToken::Tilde => "~",
            StaticToken::LeftShift => "<<",
            StaticToken::RightShift => ">>",

            // Logical operators
            StaticToken::Not => "!",
            StaticToken::LogicalAnd => "&&",
            StaticToken::LogicalOr => "||",

            // Comparison operators
            StaticToken::LessThan => "<",
            StaticToken::GreaterThan => ">",
            StaticToken::LessThanOrEqual => "<=",
            StaticToken::GreaterThanOrEqual => ">=",
            StaticToken::EqualEqual => "==",
            StaticToken::NotEqual => "!=",

            // Assignment operators
            StaticToken::Equal => "=",
            StaticToken::PlusEqual => "+=",
            StaticToken::MinusEqual => "-=",
            StaticToken::StarEqual => "*=",
            StaticToken::SlashEqual => "/=",
            StaticToken::PercentEqual => "%=",
            StaticToken::AmpersandEqual => "&=",
            StaticToken::PipeEqual => "|=",
            StaticToken::CaretEqual => "^=",
            StaticToken::LeftShiftEqual => "<<=",
            StaticToken::RightShiftEqual => ">>=",

            // Increment/Decrement
            StaticToken::PlusPlus => "++",
            StaticToken::MinusMinus => "--",

            // Member access
            StaticToken::Dot => ".",
            StaticToken::Arrow => "->",

            // Other
            StaticToken::Ellipsis => "...",
            StaticToken::Hash => "#",
            StaticToken::DoubleHash => "##",
        }
    }
}

pub const ALL_KEYWORDS: &[StaticToken] = &[
    StaticToken::Break,
    StaticToken::Continue,
    StaticToken::Do,
    StaticToken::Else,
    StaticToken::For,
    StaticToken::If,
    StaticToken::Int,
    StaticToken::Return,
    StaticToken::Void,
    StaticToken::While,
];

/// Three-character punctuators
pub const THREE_CHAR_PUNCTUATORS: &[StaticToken] = &[
    StaticToken::LeftShiftEqual,  // <<=
    StaticToken::RightShiftEqual, // >>=
    StaticToken::Ellipsis,        // ...
];

/// Two-character punctuators
pub const TWO_CHAR_PUNCTUATORS: &[StaticToken] = &[
    StaticToken::LessThanOrEqual,    // <=
    StaticToken::GreaterThanOrEqual, // >=
    StaticToken::EqualEqual,         // ==
    StaticToken::NotEqual,           // !=
    StaticToken::LeftShift,          // <<
    StaticToken::RightShift,         // >>
    StaticToken::LogicalAnd,         // &&
    StaticToken::LogicalOr,          // ||
    StaticToken::PlusPlus,           // ++
    StaticToken::MinusMinus,         // --
    StaticToken::PlusEqual,          // +=
    StaticToken::MinusEqual,         // -=
    StaticToken::StarEqual,          // *=
    StaticToken::SlashEqual,         // /=
    StaticToken::PercentEqual,       // %=
    StaticToken::AmpersandEqual,     // &=
    StaticToken::PipeEqual,          // |=
    StaticToken::CaretEqual,         // ^=
    StaticToken::Arrow,              // ->
    StaticToken::DoubleHash,         // ##
];

/// Single-character punctuators
pub const ONE_CHAR_PUNCTUATORS: &[StaticToken] = &[
    StaticToken::Semicolon,   // ;
    StaticToken::LBrace,      // {
    StaticToken::RBrace,      // }
    StaticToken::LParen,      // (
    StaticToken::RParen,      // )
    StaticToken::LBracket,    // [
    StaticToken::RBracket,    // ]
    StaticToken::Comma,       // ,
    StaticToken::Colon,       // :
    StaticToken::Question,    // ?
    StaticToken::Plus,        // +
    StaticToken::Minus,       // -
    StaticToken::Star,        // *
    StaticToken::Slash,       // /
    StaticToken::Percent,     // %
    StaticToken::Ampersand,   // &
    StaticToken::Pipe,        // |
    StaticToken::Caret,       // ^
    StaticToken::Tilde,       // ~
    StaticToken::Not,         // !
    StaticToken::LessThan,    // <
    StaticToken::GreaterThan, // >
    StaticToken::Equal,       // =
    StaticToken::Dot,         // .
    StaticToken::Hash,        // #
];

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
    pub fn constant(value: i64, span: Span) -> Self {
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
    Constant(i64),            // [0-9]+

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
