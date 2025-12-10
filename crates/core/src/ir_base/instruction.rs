use std::borrow::Cow;

use crate::ir_base::operand::Operand;

/// Individual assembly instruction representation
#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    // Data movement
    Mov {
        src: Operand,
        dst: Operand,
    },
    Push(Operand),
    Pop(Operand),

    // Arithmetic operations
    Add {
        src: Operand,
        dst: Operand,
    },
    Sub {
        src: Operand,
        dst: Operand,
    },
    Mul {
        src: Operand,
        dst: Operand,
    },
    Div {
        src: Operand,
        dst: Operand,
    },
    Neg {
        dst: Operand,
    },

    // Logical operations
    And {
        src: Operand,
        dst: Operand,
    },
    Or {
        src: Operand,
        dst: Operand,
        size: Size,
    },
    Xor {
        src: Operand,
        dst: Operand,
    },
    Not {
        dst: Operand,
    },

    // Comparison
    Cmp {
        src: Operand,
        dst: Operand,
    },

    // Jumps

    // Function calls
    Call(String),
    Ret,
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
