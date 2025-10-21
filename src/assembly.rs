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

pub fn instr_to_asm(i: &Instr, ops: &mut dynasmrt::x64::Assembler) {
    match i {
        Instr::Mov(reg, val) => {
            let r = reg.to_num();
            dynasm!(ops; .arch x64; mov Rq(r), *val);
        },
        // Instr::Mov(reg, val) => {
        //     let r = reg.to_num();
        //     dynasm!(
        //         ops; .arch x64; 
        //         mov r11, Rq(r);
        //         movabs rax, QWORD *val as i64
        // );
        // }
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
    }
}
