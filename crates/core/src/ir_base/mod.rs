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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ir_base::Operand, r};

    #[test]
    fn test_ir_func_def_creation() {
        let instructions = vec![
            Instruction::Push(Operand::Register(r!("rbp"))),
            Instruction::Ret,
        ];

        let func = IRFuncDef::new("test".into(), true, &instructions);

        assert_eq!(func.name, "_test");
        assert!(func.is_global);
        assert_eq!(func.instructions.len(), 2);
    }

    #[test]
    fn test_ir_func_def_non_global() {
        let func = IRFuncDef::new("helper".into(), false, &vec![]);

        assert_eq!(func.name, "_helper");
        assert!(!func.is_global);
        assert_eq!(func.instructions.len(), 0);
    }

    #[test]
    fn test_ir_program_creation() {
        let mut program = IRProgram::new();
        assert_eq!(program.functions.len(), 0);

        let func1 = IRFuncDef::new("main".into(), true, &vec![]);
        let func2 = IRFuncDef::new("helper".into(), false, &vec![]);

        program.add_function(func1);
        program.add_function(func2);

        assert_eq!(program.functions.len(), 2);
        assert_eq!(program.functions[0].name, "_main");
        assert_eq!(program.functions[1].name, "_helper");
    }
}
