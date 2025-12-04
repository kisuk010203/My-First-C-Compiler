use std::borrow::Cow;

pub const ALL_KEYWORDS: &[KeywordType] =
    &[KeywordType::Int, KeywordType::Void, KeywordType::Return];

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum KeywordType {
    Int,
    Void,
    Return,
}

impl KeywordType {
    pub fn as_str(&self) -> &'static str {
        match self {
            KeywordType::Int => "int",
            KeywordType::Void => "void",
            KeywordType::Return => "return",
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SymbolType {
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
}

impl SymbolType {
    pub fn as_str(&self) -> &'static str {
        match self {
            SymbolType::Semicolon => ";",
            SymbolType::LParen => "(",
            SymbolType::RParen => ")",
            SymbolType::LBrace => "{",
            SymbolType::RBrace => "}",
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'a> {
    Identifier(Cow<'a, str>), // [a-zA-Z_]\w*
    Constant(i32),            // [0-9]+
    Keyword(KeywordType),     // int void return
    Symbol(SymbolType),       // ; , ( ) { }
}

impl<'a> Token<'a> {
    pub fn identifier(input: &'a str) -> Self {
        Token::Identifier(Cow::Borrowed(input))
    }

    pub fn ascii_length(&self) -> usize {
        match self {
            Token::Identifier(s) => s.len(),
            Token::Constant(n) => n.to_string().len(),
            Token::Keyword(kw) => kw.as_str().len(),
            Token::Symbol(sym) => sym.as_str().len(),
        }
    }
}
