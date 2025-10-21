use crate::types::*;
use dynasmrt::{dynasm, DynasmApi};

pub fn instr_to_string(instr: &Instr) -> String {
    match instr {
        Instr::Mov(reg, val) => format!("mov {}, {}", reg_to_string(reg), val),
        Instr::Add(reg, val) => format!("add {}, {}", reg_to_string(reg), val),
        Instr::Sub(reg, val) => format!("sub {}, {}", reg_to_string(reg), val),
        Instr::MovReg(reg1, reg2) => {
            format!("mov {}, {}", reg_to_string(reg1), reg_to_string(reg2))
        }
        Instr::AddReg(reg1, reg2) => {
            format!("add {}, {}", reg_to_string(reg1), reg_to_string(reg2))
        }
        Instr::MinusReg(reg1, reg2) => {
            format!("sub {}, {}", reg_to_string(reg1), reg_to_string(reg2))
        }
        Instr::iMul(reg1, reg2) => {
            format!("imul {}, {}", reg_to_string(reg1), reg_to_string(reg2))
        }
        Instr::MovToStack(reg, offset) => format!("mov [rsp - {}], {}", offset, reg_to_string(reg)),
        Instr::MovFromStack(reg, offset) => {
            format!("mov {}, [rsp - {}]", reg_to_string(reg), offset)
        }
        Instr::MovDeref(dest, src) => {
            format!("mov {}, [{}]", reg_to_string(dest), reg_to_string(src))
        }
        Instr::Cmp(reg1, reg2) => {
            format!("cmp {}, {}", reg_to_string(reg1), reg_to_string(reg2))
        }
        Instr::SetL(reg) => {
            format!("setl {}", reg_to_byte_string(reg))
        }
        Instr::SetG(reg) => {
            format!("setg {}", reg_to_byte_string(reg))
        }
        Instr::Shl(reg, val) => {
            format!("shl {}, {}", reg_to_string(reg), val)
        }
        Instr::Or(reg, val) => {
            format!("or {}, {}", reg_to_string(reg), val)
        }
        Instr::Test(reg, val) => {
            format!("test {}, {}", reg_to_string(reg), val)
        }
        Instr::Jne(label) => {
            format!("jne {}", label)
        }
    }
}

pub fn instrs_to_string(instrs: &Vec<Instr>) -> String {
    instrs
        .iter()
        .map(instr_to_string)
        .collect::<Vec<String>>()
        .join("\n")
}


fn reg_to_string(reg: &Reg) -> &str {
    match reg {
        Reg::Rax => "rax",
        Reg::Rbx => "rbx",
        Reg::Rcx => "rcx",
        Reg::Rdx => "rdx",
        Reg::Rsi => "rsi",
        Reg::Rdi => "rdi",
        Reg::Rsp => "rsp",
        Reg::Rbp => "rbp",
    }
}

fn reg_to_byte_string(reg: &Reg) -> &str {
    match reg {
        Reg::Rax => "al",
        Reg::Rbx => "bl",
        Reg::Rcx => "cl",
        Reg::Rdx => "dl",
        _ => panic!("No byte register for {:?}", reg),
    }
}

pub fn instr_to_asm(i: &Instr, ops: &mut dynasmrt::x64::Assembler) {
    match i {
        Instr::Mov(reg, val) => {
            let r = reg.to_num();
            dynasm!(
                ops; .arch x64; 
                mov rax, QWORD *val as i64;
                mov Rq(r), rax
            );
        }
        Instr::Add(reg, val) => {
            let r = reg.to_num();
            dynasm!(ops; .arch x64; add Rq(r), *val);
        }
        Instr::Sub(reg, val) => {
            let r = reg.to_num();
            dynasm!(ops; .arch x64; sub Rq(r), *val);
        }
        Instr::iMul(reg1, reg2) => {
            let r1 = reg1.to_num();
            let r2 = reg2.to_num();
            dynasm!(ops; .arch x64 ; imul Rq(r1), Rq(r2));
        }
        Instr::AddReg(reg1, reg2) => {
            let r1 = reg1.to_num();
            let r2 = reg2.to_num();
            dynasm!(ops; .arch x64; add Rq(r1), Rq(r2));
        }
        Instr::MinusReg(reg1, reg2) => {
            let r1 = reg1.to_num();
            let r2 = reg2.to_num();
            dynasm!(ops; .arch x64; sub Rq(r1), Rq(r2));
        }
        Instr::MovReg(reg1, reg2) => {
            let r1 = reg1.to_num();
            let r2 = reg2.to_num();
            dynasm!(ops; .arch x64; mov Rq(r1), Rq(r2));
        }
        Instr::MovToStack(reg, offset) => {
            let r = reg.to_num();
            dynasm!(ops; .arch x64; mov [rsp - *offset], Rq(r));
        }
        Instr::MovFromStack(reg, offset) => {
            let r: u8 = reg.to_num();
            dynasm!(ops; .arch x64; mov Rq(r), [rsp - *offset]);
        }
        Instr::MovDeref(dest, src) => {
            let d = dest.to_num();
            let s = src.to_num();
            dynasm!(ops; .arch x64; mov Rq(d), [Rq(s)]);
        }
        Instr::Cmp(reg1, reg2) => {
            let r1 = reg1.to_num();
            let r2 = reg2.to_num();
            dynasm!(ops; .arch x64; cmp Rq(r1), Rq(r2));
        }
        Instr::SetL(reg) => {
            let r = reg.to_num();
            dynasm!(ops; .arch x64; setl Rb(r));
            dynasm!(ops; .arch x64; movzx Rq(r), Rb(r));
        }
        Instr::SetG(reg) => {
            let r = reg.to_num();
            dynasm!(ops; .arch x64; setg Rb(r));
            dynasm!(ops; .arch x64; movzx Rq(r), Rb(r));
        }
        Instr::Shl(reg, val) => {
            let r = reg.to_num();
            dynasm!(ops; .arch x64; shl Rq(r), *val as i8);
        }
        Instr::Or(reg, val) => {
            let r = reg.to_num();
            dynasm!(ops; .arch x64; or Rq(r), *val as i32);
        }
        Instr::Test(reg, val) => {
            let r = reg.to_num();
            dynasm!(ops; .arch x64; test Rq(r), *val as i32);
        }
        Instr::Jne(_label) => {
            panic!("invalid argument");
        }
    }
}
