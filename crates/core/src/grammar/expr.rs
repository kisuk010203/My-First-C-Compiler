use std::borrow::Cow;

use crate::{grammar::*, t};

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

    pub fn from_token_type(token: &TokenType) -> Option<Self> {
        match token {
            t!("+") => Some(BinaryOp::Add),
            t!("-") => Some(BinaryOp::Subtract),
            t!("*") => Some(BinaryOp::Multiply),
            t!("/") => Some(BinaryOp::Divide),
            t!("<") => Some(BinaryOp::LT),
            t!(">") => Some(BinaryOp::GT),
            t!("<=") => Some(BinaryOp::LTE),
            t!(">=") => Some(BinaryOp::GTE),
            t!("==") => Some(BinaryOp::EQ),
            t!("!=") => Some(BinaryOp::NEQ),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Negate,
    Not,
}
