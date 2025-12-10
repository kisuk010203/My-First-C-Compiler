mod emitter;
mod instruction;
mod mac;
mod operand;
pub mod reg;

pub use crate::ir_base::{emitter::*, instruction::*, operand::*};

/// Complete program in IR
#[derive(Debug, Clone)]
pub struct IRProgram<'a> {
    pub functions: Vec<IRFuncDef<'a>>,
}
impl<'a> IRProgram<'a> {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
        }
    }

    pub fn add_function(&mut self, func: IRFuncDef<'a>) {
        self.functions.push(func);
    }
}
