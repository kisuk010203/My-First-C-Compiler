mod expr;
mod stmt;

pub use expr::*;
pub use stmt::*;

/// Root of the AST - a complete C program
#[derive(Debug)]
pub struct Program<'a> {
    pub functions: Vec<FuncDef<'a>>,
}
