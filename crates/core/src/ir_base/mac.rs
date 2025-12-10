/// macro for creating register instances
#[macro_export]
macro_rules! reg {
    ("rax") => {
        $crate::ir_base::reg::Register::RAX
    };
    ("rbx") => {
        $crate::ir_base::reg::Register::RBX
    };
    ("rcx") => {
        $crate::ir_base::reg::Register::RCX
    };
    ("rdx") => {
        $crate::ir_base::reg::Register::RDX
    };
    ("rsi") => {
        $crate::ir_base::reg::Register::RSI
    };
    ("rdi") => {
        $crate::ir_base::reg::Register::RDI
    };
    ("rsp") => {
        $crate::ir_base::reg::Register::RSP
    };
    ("rbp") => {
        $crate::ir_base::reg::Register::RBP
    };
}
