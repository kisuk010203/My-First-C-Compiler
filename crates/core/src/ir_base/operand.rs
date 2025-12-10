use crate::ir_base::reg::PhyRegister;
/// Operand for instructions
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operand {
    Immediate(i64),
    Register(PhyRegister),
    Memory {
        base: Option<PhyRegister>,
        offset: i64,
    },
    // Label(String),
}
impl std::fmt::Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operand::Immediate(int) => write!(f, "${}", int),
            Operand::Register(reg) => write!(f, "%{}", reg.as_str()),
            Operand::Memory { base, offset } => match base {
                Some(reg) => write!(f, "{}(%{})", offset, reg.as_str()),
                //TODO: Choose how to represent memory operands without base registers
                None => write!(f, "{}(%rsp)", offset),
            },
        }
    }
}
