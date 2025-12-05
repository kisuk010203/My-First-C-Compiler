use std::borrow::Cow;

#[derive(Debug)]
pub enum Expression<'a> {
    Constant(i32),
    Variable(Cow<'a, str>),
    Grouped(Box<Expression<'a>>),
    Binary {
        op: BinaryOp,
        lhs: Box<Expression<'a>>,
        rhs: Box<Expression<'a>>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expression<'a>>,
    },
}

#[derive(Debug)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    LT,
    GT,
    EQ,
    NEQ,
    LTE,
    GTE,
}

impl BinaryOp {
    pub fn infix_binding_power(&self) -> (u8, u8) {
        match self {
            // Multiplicative operators (highest precedence)
            BinaryOp::Multiply | BinaryOp::Divide => (7, 8),
            // Additive operators
            BinaryOp::Add | BinaryOp::Subtract => (5, 6),
            // Relational operators
            BinaryOp::LT | BinaryOp::GT | BinaryOp::LTE | BinaryOp::GTE => (3, 4),
            // Equality operators (lowest precedence)
            BinaryOp::EQ | BinaryOp::NEQ => (1, 2),
        }
    }

    pub fn from_token_type(token: &crate::lexer_base::TokenType) -> Option<Self> {
        use crate::lexer_base::{StaticToken, TokenType};
        match token {
            TokenType::Static(StaticToken::Plus) => Some(BinaryOp::Add),
            TokenType::Static(StaticToken::Minus) => Some(BinaryOp::Subtract),
            TokenType::Static(StaticToken::Star) => Some(BinaryOp::Multiply),
            TokenType::Static(StaticToken::Slash) => Some(BinaryOp::Divide),
            TokenType::Static(StaticToken::LessThan) => Some(BinaryOp::LT),
            TokenType::Static(StaticToken::GreaterThan) => Some(BinaryOp::GT),
            TokenType::Static(StaticToken::LessThanOrEqual) => Some(BinaryOp::LTE),
            TokenType::Static(StaticToken::GreaterThanOrEqual) => Some(BinaryOp::GTE),
            TokenType::Static(StaticToken::EqualEqual) => Some(BinaryOp::EQ),
            TokenType::Static(StaticToken::NotEqual) => Some(BinaryOp::NEQ),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum UnaryOp {
    Negate,
    Not,
}
