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

    pub fn emit_program(&mut self, program: &IRProgram) -> String {
        for func in &program.functions {
            self.emit_function(func);
            self.output.push('\n');
        }
        std::mem::take(&mut self.output)
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ir_base::Operand, r};

    fn normalize_whitespace(s: &str) -> String {
        s.split_whitespace().collect::<Vec<_>>().join(" ")
    }
    fn contains_normalized(haystack: &str, needle: &str) -> bool {
        normalize_whitespace(haystack).contains(&normalize_whitespace(needle))
    }

    #[test]
    fn test_emit_empty_program() {
        let program = IRProgram::new();
        let mut emitter = Emitter::new();
        let assembly = emitter.emit_program(&program);

        // Should be empty or just whitespace
        assert!(assembly.trim().is_empty());
    }

    #[test]
    fn test_emit_simple_function() {
        let instructions = vec![
            Instruction::Push(Operand::Register(r!("rbp"))),
            Instruction::Mov {
                src: Operand::Register(r!("rsp")),
                dst: Operand::Register(r!("rbp")),
            },
            Instruction::Mov {
                src: Operand::Immediate(42),
                dst: Operand::Register(r!("rax")),
            },
            Instruction::Pop(Operand::Register(r!("rbp"))),
            Instruction::Ret,
        ];

        let func = IRFuncDef::new("main".into(), true, &instructions);
        let mut program = IRProgram::new();
        program.add_function(func);

        let mut emitter = Emitter::new();
        let assembly = emitter.emit_program(&program);
        println!("{}", assembly);

        assert!(contains_normalized(&assembly, ".global _main"));
        assert!(contains_normalized(&assembly, "_main:"));
        assert!(contains_normalized(&assembly, "pushq   %rbp"));
        assert!(contains_normalized(&assembly, "movq    %rsp, %rbp"));
        assert!(contains_normalized(&assembly, "movq    $42, %rax"));
        assert!(contains_normalized(&assembly, "popq    %rbp"));
        assert!(contains_normalized(&assembly, "retq"));
    }

    #[test]
    fn test_emit_non_global_function() {
        let func = IRFuncDef::new("helper".into(), false, &vec![Instruction::Ret]);
        let mut program = IRProgram::new();
        program.add_function(func);

        let mut emitter = Emitter::new();
        let assembly = emitter.emit_program(&program);

        assert!(contains_normalized(&assembly, "_helper:"));
        assert!(!contains_normalized(&assembly, ".global _helper"));
        assert!(contains_normalized(&assembly, "retq"));
    }

    #[test]
    fn test_emit_mov_sizes() {
        let instructions = vec![
            Instruction::Mov {
                src: Operand::Immediate(1),
                dst: Operand::Register(r!("rax")),
            },
            Instruction::Mov {
                src: Operand::Immediate(2),
                dst: Operand::Register(r!("rbx")),
            },
            Instruction::Mov {
                src: Operand::Immediate(3),
                dst: Operand::Register(r!("rcx")),
            },
            Instruction::Mov {
                src: Operand::Immediate(4),
                dst: Operand::Register(r!("rdx")),
            },
        ];

        let func = IRFuncDef::new("sizes".into(), true, &instructions);
        let mut program = IRProgram::new();
        program.add_function(func);

        let mut emitter = Emitter::new();
        let assembly = emitter.emit_program(&program);

        assert!(contains_normalized(&assembly, "movq    $1, %rax"));
        assert!(contains_normalized(&assembly, "movq    $2, %rbx"));
        assert!(contains_normalized(&assembly, "movq    $3, %rcx"));
        assert!(contains_normalized(&assembly, "movq    $4, %rdx"));
    }

    #[test]
    fn test_emit_arithmetic() {
        let instructions = vec![
            Instruction::Add {
                src: Operand::Immediate(5),
                dst: Operand::Register(r!("rax")),
            },
            Instruction::Sub {
                src: Operand::Register(r!("rcx")),
                dst: Operand::Register(r!("rax")),
            },
            Instruction::IMul {
                src: Operand::Immediate(3),
                dst: Operand::Register(r!("rax")),
            },
        ];

        let func = IRFuncDef::new("arith".into(), true, &instructions);
        let mut program = IRProgram::new();
        program.add_function(func);

        let mut emitter = Emitter::new();
        let assembly = emitter.emit_program(&program);

        assert!(contains_normalized(&assembly, "addl    $5, %rax"));
        assert!(contains_normalized(&assembly, "subl    %rcx, %rax"));
        assert!(contains_normalized(&assembly, "imull   $3, %rax"));
    }

    #[test]
    fn test_emit_unary() {
        let instructions = vec![
            Instruction::Neg {
                dst: Operand::Register(r!("rax")),
            },
            Instruction::Not {
                dst: Operand::Register(r!("rbx")),
            },
        ];

        let func = IRFuncDef::new("unary".into(), true, &instructions);
        let mut program = IRProgram::new();
        program.add_function(func);

        let mut emitter = Emitter::new();
        let assembly = emitter.emit_program(&program);

        assert!(contains_normalized(&assembly, "negl    %rax"));
        assert!(contains_normalized(&assembly, "notl    %rbx"));
    }

    #[test]
    fn test_emit_call() {
        let instructions = vec![
            Instruction::Call("printf".to_string()),
            Instruction::Call("_helper".to_string()),
        ];

        let func = IRFuncDef::new("caller".into(), true, &instructions);
        let mut program = IRProgram::new();
        program.add_function(func);

        let mut emitter = Emitter::new();
        let assembly = emitter.emit_program(&program);

        assert!(contains_normalized(&assembly, "call    printf"));
        assert!(contains_normalized(&assembly, "call    _helper"));
    }

    #[test]
    fn test_emit_multiple_functions() {
        let func1 = IRFuncDef::new("main".into(), true, &vec![Instruction::Ret]);

        let func2 = IRFuncDef::new("helper".into(), false, &vec![Instruction::Ret]);

        let mut program = IRProgram::new();
        program.add_function(func1);
        program.add_function(func2);

        let mut emitter = Emitter::new();
        let assembly = emitter.emit_program(&program);
        // Check both functions are present
        assert!(contains_normalized(&assembly, "_main:"));
        assert!(contains_normalized(&assembly, "_helper:"));
        assert!(contains_normalized(&assembly, ".global _main"));
        assert!(!contains_normalized(&assembly, ".global _helper"));

        // Check they're separated by blank line
        let functions: Vec<&str> = assembly.split("\n\n").collect();
        assert!(functions.len() >= 2);
    }

    #[test]
    fn test_emit_memory_operands() {
        let instructions = vec![
            Instruction::Mov {
                src: Operand::Memory {
                    base: Some(r!("rbp")),
                    offset: -8,
                },
                dst: Operand::Register(r!("rax")),
            },
            Instruction::Mov {
                src: Operand::Register(r!("rcx")),
                dst: Operand::Memory {
                    base: Some(r!("rsp")),
                    offset: 0,
                },
            },
        ];

        let func = IRFuncDef::new("mem".into(), true, &instructions);
        let mut program = IRProgram::new();
        program.add_function(func);

        let mut emitter = Emitter::new();
        let assembly = emitter.emit_program(&program);

        assert!(contains_normalized(&assembly, "movq    -8(%rbp), %rax"));
        assert!(contains_normalized(&assembly, "movq    %rcx, 0(%rsp)"));
    }
}
