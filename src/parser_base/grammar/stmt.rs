use std::borrow::Cow;

use crate::parser_base::Expression;

/// A function definition
#[derive(Debug)]
pub struct FuncDef<'a> {
    pub name: Cow<'a, str>,
    pub params: Vec<Cow<'a, str>>,
    pub body: Block<'a>,
}

/// A block of statements
#[derive(Debug)]
pub struct Block<'a> {
    pub statements: Vec<Statement<'a>>,
}

/// A statement within a function
#[derive(Debug)]
pub enum Statement<'a> {
    Return { expr: Expression<'a> },
}
