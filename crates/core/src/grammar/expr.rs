use std::borrow::Cow;

use crate::grammar::operator::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Expression<'a> {
    Constant(i64),
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
        lhs: Box<Expression<'a>>,
        rhs: Box<Expression<'a>>,
    },
    FunctionCall {
        callee: Box<Expression<'a>>,
        args: Vec<Expression<'a>>,
    },
}
