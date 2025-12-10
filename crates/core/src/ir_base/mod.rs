mod instruction;
mod mac;
mod operand;
mod reg;

pub use crate::ir_base::instruction::*;
pub use crate::ir_base::operand::*;
pub use crate::ir_base::reg::*;

/// Complete program in IR
#[derive(Debug, Clone)]
pub struct IRProgram<'a> {
    pub functions: Vec<IRFuncDef<'a>>,
}
impl<'a> IRProgram<'a> {
    pub fn new() -> Self {
        Self { functions: Vec::new() }
    }

    pub fn add_function(&mut self, func: IRFuncDef<'a>) {
        self.functions.push(func);
    }
}
