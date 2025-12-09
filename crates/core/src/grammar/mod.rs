mod _type;
mod expr;
mod mac;
mod operator;
mod stmt;
mod token;

pub use _type::Type;
pub use expr::*;
pub use operator::*;
pub use stmt::*;
pub use token::*;

/// Root of the AST - a complete C program
#[derive(Debug)]
pub struct Program<'a> {
    pub functions: Vec<FuncDef<'a>>,
}
