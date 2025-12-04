use std::borrow::Cow;

#[derive(Debug)]
pub enum Program<'a> {
    Program(FuncDef<'a>),
}
#[derive(Debug)]
pub enum FuncDef<'a> {
    Fn(Cow<'a, str>, Statement), // Name and Body of function
}
#[derive(Debug)]
pub enum Statement {
    Return(Expression),
    If(BinaryExpression, Box<Statement>, Option<Box<Statement>>),
    Binary(BinaryOperator, Expression, Expression),
    Unary(UnaryOperator, Expression),
}
#[derive(Debug)]
pub enum Expression {
    Constant(i32),
}
#[derive(Debug)]
pub enum BinaryExpression {
    LT(Expression, Expression),
    GT(Expression, Expression),
    EQ(Expression, Expression),
    NEQ(Expression, Expression),
    LTE(Expression, Expression),
    GTE(Expression, Expression),
}
#[derive(Debug)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
}
#[derive(Debug)]
pub enum UnaryOperator {
    Negate,
}
