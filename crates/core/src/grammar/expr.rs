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
    Assignment {
        op: AssignOp,
        lvalue: Box<Expression<'a>>,
        rvalue: Box<Expression<'a>>,
    },
    FunctionCall {
        callee: Box<Expression<'a>>,
        args: Vec<Expression<'a>>,
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
    pub const fn infix_binding_power(&self) -> (u8, u8) {
        match self {
            // Multiplicative operators (highest precedence)
            BinaryOp::Multiply | BinaryOp::Divide => (11, 12),
            // Additive operators
            BinaryOp::Add | BinaryOp::Subtract => (9, 10),
            // Relational operators
            BinaryOp::LT | BinaryOp::GT | BinaryOp::LTE | BinaryOp::GTE => (7, 8),
            // Equality operators
            BinaryOp::EQ | BinaryOp::NEQ => (5, 6),
            // Assignment has lower precedence (handled separately)
        }
    }

    pub const fn from_token_type(token: &TokenType) -> Option<Self> {
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

impl UnaryOp {
    pub const fn from_token_type(token: &TokenType) -> Option<Self> {
        match token {
            t!("!") => Some(UnaryOp::Not),
            t!("-") => Some(UnaryOp::Negate),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum AssignOp {
    Assign,       // =
    PlusAssign,   // +=
    MinusAssign,  // -=
    MulAssign,    // *=
    DivAssign,    // /=
    ModAssign,    // %=
    AndAssign,    // &=
    OrAssign,     // |=
    XorAssign,    // ^=
    LShiftAssign, // <<=
    RShiftAssign, // >>=
}

impl AssignOp {
    /// Returns binding power for assignment operators
    /// Assignment is right-associative, so right BP is lower than left BP
    /// This gives it the lowest precedence of all operators
    #[inline]
    pub const fn infix_binding_power(&self) -> (u8, u8) {
        // All assignment operators have the same precedence
        // Right-associative: (left_bp=1, right_bp=0)
        // This means: if we see another assignment on the right, we parse it first
        (1, 0)
    }

    pub const fn from_token_type(token: &TokenType) -> Option<Self> {
        match token {
            t!("=") => Some(AssignOp::Assign),
            t!("+=") => Some(AssignOp::PlusAssign),
            t!("-=") => Some(AssignOp::MinusAssign),
            t!("*=") => Some(AssignOp::MulAssign),
            t!("/=") => Some(AssignOp::DivAssign),
            t!("%=") => Some(AssignOp::ModAssign),
            t!("&=") => Some(AssignOp::AndAssign),
            t!("|=") => Some(AssignOp::OrAssign),
            t!("^=") => Some(AssignOp::XorAssign),
            t!("<<=") => Some(AssignOp::LShiftAssign),
            t!(">>=") => Some(AssignOp::RShiftAssign),
            _ => None,
        }
    }
}
