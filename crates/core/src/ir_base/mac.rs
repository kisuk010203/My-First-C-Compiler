/// macro for creating register instances
#[macro_export]
macro_rules! r {
    ("rax") => {
        $crate::ir_base::reg::PhyRegister::RAX
    };
    ("rbx") => {
        $crate::ir_base::reg::PhyRegister::RBX
    };
    ("rcx") => {
        $crate::ir_base::reg::PhyRegister::RCX
    };
    ("rdx") => {
        $crate::ir_base::reg::PhyRegister::RDX
    };
    ("rsi") => {
        $crate::ir_base::reg::PhyRegister::RSI
    };
    ("rdi") => {
        $crate::ir_base::reg::PhyRegister::RDI
    };
    ("rsp") => {
        $crate::ir_base::reg::PhyRegister::RSP
    };
    ("rbp") => {
        $crate::ir_base::reg::PhyRegister::RBP
    };
    ("r8") => {
        $crate::ir_base::Register::R8
    };
    ("r9") => {
        $crate::ir_base::Register::R9
    };
    ("r10") => {
        $crate::ir_base::Register::R10
    };
    ("r11") => {
        $crate::ir_base::Register::R11
    };
    ("r12") => {
        $crate::ir_base::Register::R12
    };
    ("r13") => {
        $crate::ir_base::Register::R13
    };
    ("r14") => {
        $crate::ir_base::Register::R14
    };
    ("r15") => {
        $crate::ir_base::Register::R15
    };
}
