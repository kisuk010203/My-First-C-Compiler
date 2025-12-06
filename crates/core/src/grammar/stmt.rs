use compiler_macros::statement_enum;
use std::borrow::Cow;

use crate::grammar::{Expression, Type};

/// A function definition
#[derive(Debug)]
pub struct FuncDef<'a> {
    pub return_type: Type,
    pub name: Cow<'a, str>,
    pub params: Vec<Cow<'a, str>>,
    pub body: BlockStmt<'a>,
}

statement_enum! {
    #[derive(Debug, Clone)]
    pub struct BlockStmt<'a> {
        pub statements: Vec<Statement<'a>>,
    }

    #[derive(Debug, Clone)]
    pub struct DoWhileStmt<'a> {
        pub cond: Expression<'a>,
        pub body: Box<Statement<'a>>,
    }

    #[derive(Debug, Clone)]
    pub struct IfStmt<'a> {
        pub cond: Expression<'a>,
        pub then_block: Box<Statement<'a>>,
        pub else_block: Option<Box<Statement<'a>>>,
    }

    #[derive(Debug, Clone)]
    pub struct ReturnStmt<'a> {
        pub expr: Expression<'a>,
    }

    #[derive(Debug, Clone)]
    pub struct WhileStmt<'a> {
        pub cond: Expression<'a>,
        pub body: Box<Statement<'a>>,
    }
}
