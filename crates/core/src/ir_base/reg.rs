/// Physical CPU registers for x86-64 architecture.
#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PhyRegister {
    RAX,
    RBX,
    RCX,
    RDX,
    RSI,
    RDI,
    RBP,
    RSP,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}
impl PhyRegister {
    #[allow(dead_code)]
    pub const fn as_str(&self) -> &'static str {
        match self {
            PhyRegister::RAX => "rax",
            PhyRegister::RBX => "rbx",
            PhyRegister::RCX => "rcx",
            PhyRegister::RDX => "rdx",
            PhyRegister::RSI => "rsi",
            PhyRegister::RDI => "rdi",
            PhyRegister::RBP => "rbp",
            PhyRegister::RSP => "rsp",
            PhyRegister::R8 => "r8",
            PhyRegister::R9 => "r9",
            PhyRegister::R10 => "r10",
            PhyRegister::R11 => "r11",
            PhyRegister::R12 => "r12",
            PhyRegister::R13 => "r13",
            PhyRegister::R14 => "r14",
            PhyRegister::R15 => "r15",
        }
    }
}
