use std::borrow::Cow;

#[derive(Debug)]
pub enum Expression<'a> {
    Constant(i32),
    Variable(Cow<'a, str>),
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

#[derive(Debug)]
pub enum UnaryOp {
    Negate,
    Not,
    BitwiseNot,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
}
