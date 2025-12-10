use std::io;

use crate::ir_base::{
    IRProgram,
    instruction::{IRFuncDef, Instruction},
};

pub struct Emitter<'a, W: io::Write> {
    pub output: &'a mut W,
}

impl<'a, W: io::Write> Emitter<'a, W> {
    pub fn new(output: &'a mut W) -> Self {
        Self { output }
    }

    pub fn emit_program(mut self, program: &IRProgram) -> io::Result<&'a mut W> {
        for func in &program.functions {
            self.emit_function(func)?;
            self.output.write_all(b"\n")?;
        }

        Ok(self.output)
    }

    pub fn emit_function(&mut self, func: &IRFuncDef) -> io::Result<()> {
        if func.is_global {
            writeln!(self.output, "    .global {}", func.name)?;
        }
        writeln!(self.output, "{}:", func.name)?;

        for inst in &func.instructions {
            self.emit_instruction(inst)?;
        }

        Ok(())
    }

    fn emit_instruction(&mut self, inst: &Instruction) -> io::Result<()> {
        writeln!(self.output, "    {}", inst.as_assembly_inline())
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

    fn emit_program_as_string(program: &IRProgram) -> String {
        let mut buffer = Vec::new();
        let emitter = Emitter::new(&mut buffer);
        emitter.emit_program(&program).unwrap();
        String::from_utf8(buffer).unwrap()
    }

    #[test]
    fn test_emit_empty_program() {
        let program = IRProgram::new();
        let assembly = emit_program_as_string(&program);

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

        let func = IRFuncDef::new_global("main".into(), instructions);
        let mut program = IRProgram::new();
        program.add_function(func);

        let assembly = emit_program_as_string(&program);
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
        let func = IRFuncDef::new_local("helper".into(), vec![Instruction::Ret]);
        let mut program = IRProgram::new();
        program.add_function(func);

        let assembly = emit_program_as_string(&program);

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

        let func = IRFuncDef::new_global("sizes".into(), instructions);
        let mut program = IRProgram::new();
        program.add_function(func);

        let assembly = emit_program_as_string(&program);

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

        let func = IRFuncDef::new_global("arith".into(), instructions);
        let mut program = IRProgram::new();
        program.add_function(func);

        let assembly = emit_program_as_string(&program);

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

        let func = IRFuncDef::new_global("unary".into(), instructions);
        let mut program = IRProgram::new();
        program.add_function(func);

        let assembly = emit_program_as_string(&program);

        assert!(contains_normalized(&assembly, "negl    %rax"));
        assert!(contains_normalized(&assembly, "notl    %rbx"));
    }

    #[test]
    fn test_emit_call() {
        let instructions = vec![
            Instruction::Call("printf".to_string()),
            Instruction::Call("_helper".to_string()),
        ];

        let func = IRFuncDef::new_global("caller".into(), instructions);
        let mut program = IRProgram::new();
        program.add_function(func);

        let assembly = emit_program_as_string(&program);

        assert!(contains_normalized(&assembly, "call    printf"));
        assert!(contains_normalized(&assembly, "call    _helper"));
    }

    #[test]
    fn test_emit_multiple_functions() {
        let func1 = IRFuncDef::new_global("main".into(), vec![Instruction::Ret]);

        let func2 = IRFuncDef::new_local("helper".into(), vec![Instruction::Ret]);

        let mut program = IRProgram::new();
        program.add_function(func1);
        program.add_function(func2);

        let assembly = emit_program_as_string(&program);
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

        let func = IRFuncDef::new_global("mem".into(), instructions);
        let mut program = IRProgram::new();
        program.add_function(func);

        let assembly = emit_program_as_string(&program);

        assert!(contains_normalized(&assembly, "movq    -8(%rbp), %rax"));
        assert!(contains_normalized(&assembly, "movq    %rcx, 0(%rsp)"));
    }
}
