use std::borrow::Cow;

use crate::ir_base::operand::Operand;

/// Individual assembly instruction representation
#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    // Data movement
    Mov { src: Operand, dst: Operand },
    Push(Operand),
    Pop(Operand),

    // Arithmetic operations
    Add { src: Operand, dst: Operand },
    Sub { src: Operand, dst: Operand },
    IMul { src: Operand, dst: Operand },
    IDiv { divisor: Operand },
    Neg { dst: Operand },

    // Logical operations
    And { src: Operand, dst: Operand },
    Or { src: Operand, dst: Operand },
    Xor { src: Operand, dst: Operand },
    Not { dst: Operand },

    // Comparison
    Cmp { src: Operand, dst: Operand },

    // Jumps

    // Function calls
    Call(String),
    Ret,
}
impl Instruction {
    pub fn as_assembly_inline(&self) -> String {
        match self {
            Instruction::Mov { src, dst } => {
                format!("movq {}, {}", src, dst)
            }
            Instruction::Push(operand) => {
                format!("pushq {}", operand)
            }
            Instruction::Pop(operand) => {
                format!("popq {}", operand)
            }
            Instruction::Add { src, dst } => {
                format!("addl {}, {}", src, dst)
            }
            Instruction::Sub { src, dst } => {
                format!("subl {}, {}", src, dst)
            }
            Instruction::IMul { src, dst } => {
                format!("imull {}, {}", src, dst)
            }
            Instruction::IDiv { divisor } => {
                format!("idivl {}", divisor)
            }
            Instruction::Neg { dst } => {
                format!("negl {}", dst)
            }
            Instruction::And { src, dst } => {
                format!("andl {}, {}", src, dst)
            }
            Instruction::Or { src, dst } => {
                format!("orl {}, {}", src, dst)
            }
            Instruction::Xor { src, dst } => {
                format!("xorl {}, {}", src, dst)
            }
            Instruction::Not { dst } => {
                format!("notl {}", dst)
            }
            Instruction::Cmp { src, dst } => {
                format!("cmpl {}, {}", src, dst)
            }
            Instruction::Call(function) => {
                format!("call {}", function)
            }
            Instruction::Ret => "retq".to_string(),
        }
    }
}

/// Size specifier for instructions
/// TODO: Use this in instructions, but for now we will keep it simple, only
/// 32-bit
#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Size {
    Byte,
    Word,
    Long,
    Quad,
}

/// IR Function Definition
#[derive(Debug, Clone)]
pub struct IRFuncDef<'a> {
    pub name: Cow<'a, str>,
    pub is_global: bool,
    pub instructions: Vec<Instruction>,
}
impl<'a> IRFuncDef<'a> {
    fn platfrom_mangle_name(name: &str) -> String {
        // On macos, symbol names should be prefixed with `_`
        #[cfg(target_os = "macos")]
        {
            format!("_{}", name)
        }
        // On other platforms, no prefix is needed
        #[cfg(not(target_os = "macos"))]
        {
            name.to_string()
        }
    }
    pub fn new(name: Cow<'a, str>, is_global: bool, instructions: &[Instruction]) -> Self {
        Self {
            name: Cow::Owned(Self::platfrom_mangle_name(&name)),
            is_global,
            instructions: instructions.to_vec(),
        }
    }
}
