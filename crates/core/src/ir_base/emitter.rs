use std::fmt::Write;

use crate::ir_base::{
    IRProgram,
    instruction::{IRFuncDef, Instruction},
};

pub struct Emitter {
    pub output: String,
}
impl Emitter {
    pub fn new() -> Self {
        Self {
            output: String::new(),
        }
    }

    pub fn emit_program(mut self, program: &IRProgram) -> String {
        for func in &program.functions {
            self.emit_function(func);
            self.output.push('\n');
        }
        self.output
    }

    pub fn emit_function(&mut self, func: &IRFuncDef) {
        if func.is_global {
            writeln!(self.output, "    .global {}", func.name).unwrap();
        }
        writeln!(self.output, "{}:", func.name).unwrap();

        for inst in &func.instructions {
            self.emit_instruction(inst);
        }
    }

    fn emit_instruction(&mut self, inst: &Instruction) {
        writeln!(self.output, "    {}", inst.as_assembly_inline()).unwrap();
    }
}
