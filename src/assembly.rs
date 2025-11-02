use crate::types::*;
use dynasmrt::{dynasm, DynamicLabel, DynasmApi, DynasmLabelApi};
use im::HashMap;

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
        Instr::XorReg(reg1, reg2) => {
            format!("xor {}, {}", reg_to_string(reg1), reg_to_string(reg2))
        }
        Instr::Push(reg) => format!("push {}", reg_to_string(reg)),
        Instr::Pop(reg) => format!("pop {}", reg_to_string(reg)),
        Instr::MovToStack(reg, offset) => format!("mov [rbp - {}], {}", offset, reg_to_string(reg)),
        Instr::MovFromStack(reg, offset) => {
            format!("mov {}, [rbp - {}]", reg_to_string(reg), offset)
        }
        Instr::MovDeref(dest, src) => {
            format!("mov {}, [{}]", reg_to_string(dest), reg_to_string(src))
        }
        Instr::MovToMem(dest, src) => {
            format!("mov [{}], {}", reg_to_string(dest), reg_to_string(src))
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
        Instr::SetLE(reg) => {
            format!("setle {}", reg_to_byte_string(reg))
        }
        Instr::SetGE(reg) => {
            format!("setge {}", reg_to_byte_string(reg))
        }
        Instr::SetEq(reg) => {
            format!("sete {}", reg_to_byte_string(reg))
        }
        Instr::Shl(reg, val) => {
            format!("shl {}, {}", reg_to_string(reg), val)
        }
        Instr::Sar(reg, val) => {
            format!("sar {}, {}", reg_to_string(reg), val)
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
        Instr::CmpImm(reg, val) => {
            format!("cmp {}, {}", reg_to_string(reg), val)
        }
        Instr::Je(label) => {
            format!("je {}", label)
        }
        Instr::Jnz(label) => {
            format!("jnz {}", label)
        }
        Instr::Jz(label) => {
            format!("jz {}", label)
        }
        Instr::Jo(label) => {
            format!("jo {}", label)
        }
        Instr::Jmp(label) => {
            format!("jmp {}", label)
        }
        Instr::Call(label) => {
            format!("call {}", label)
        }
        Instr::Ret => {
            format!("ret")
        }
        Instr::Label(label) => {
            format!("{}:", label)
        }
    }
}

pub fn instrs_to_string(instrs: &Vec<Instr>) -> String {
    let mut result = String::new();
    //space intrs except labels
    for instr in instrs {
        match instr {
            Instr::Label(label) => {
            }
            _ => {
                result+="  ";
            }

        }
        result+=&instr_to_string(instr);
        result +="\n";
    }
    result
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
// Dynasm
pub fn instr_to_asm(
    i: &Instr,
    ops: &mut dynasmrt::x64::Assembler,
    labels: &HashMap<String, DynamicLabel>,
) {
    match i {
        Instr::Mov(reg, val) => {
            let r: u8 = reg.to_num();
            dynasm!(ops; .arch x64; mov Rq(r), QWORD *val);
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
        Instr::XorReg(reg1, reg2) => {
            let r1 = reg1.to_num();
            let r2 = reg2.to_num();
            dynasm!(ops; .arch x64; xor Rq(r1), Rq(r2));
        }
        Instr::Push(reg) => {
            let r = reg.to_num();
            dynasm!(ops; .arch x64; push Rq(r));
        }
        Instr::Pop(reg) => {
            let r = reg.to_num();
            dynasm!(ops; .arch x64; pop Rq(r));
        }
        Instr::MovToStack(reg, offset) => {
            let r = reg.to_num();
            dynasm!(ops; .arch x64; mov [rbp - *offset], Rq(r));
        }
        Instr::MovFromStack(reg, offset) => {
            let r: u8 = reg.to_num();
            dynasm!(ops; .arch x64; mov Rq(r), [rbp - *offset]);
        }
        Instr::MovDeref(dest, src) => {
            let d = dest.to_num();
            let s = src.to_num();
            dynasm!(ops; .arch x64; mov Rq(d), [Rq(s)]);
        }
        Instr::MovToMem(dest, src) => {
            let d = dest.to_num();
            let s = src.to_num();
            dynasm!(ops; .arch x64; mov [Rq(d)], Rq(s));
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
        Instr::SetEq(reg) => {
            let r = reg.to_num();
            dynasm!(ops; .arch x64; sete Rb(r));
            dynasm!(ops; .arch x64; movzx Rq(r), Rb(r));
        }
        Instr::SetG(reg) => {
            let r = reg.to_num();
            dynasm!(ops; .arch x64; setg Rb(r));
            dynasm!(ops; .arch x64; movzx Rq(r), Rb(r));
        }
        Instr::SetLE(reg) => {
            let r = reg.to_num();
            dynasm!(ops; .arch x64; setle Rb(r));
            dynasm!(ops; .arch x64; movzx Rq(r), Rb(r));
        }
        Instr::SetGE(reg) => {
            let r = reg.to_num();
            dynasm!(ops; .arch x64; setge Rb(r));
            dynasm!(ops; .arch x64; movzx Rq(r), Rb(r));
        }
        Instr::Shl(reg, val) => {
            let r = reg.to_num();
            dynasm!(ops; .arch x64; shl Rq(r), *val as i8);
        }
        Instr::Sar(reg, val) => {
            let r = reg.to_num();
            dynasm!(ops; .arch x64; sar Rq(r), *val as i8);
        }
        Instr::Or(reg, val) => {
            let r = reg.to_num();
            dynasm!(ops; .arch x64; or Rq(r), *val as i32);
        }
        Instr::Test(reg, val) => {
            let r = reg.to_num();
            dynasm!(ops; .arch x64; test Rq(r), *val as i32);
        }
        Instr::Jne(label_name) => {
            let lbl = labels.get(label_name).unwrap();
            // let lbl = labels
            //     .entry(label_name.clone())
            //     .or_insert_with(|| ops.new_dynamic_label());
            dynasm!(ops; .arch x64; jne => *lbl);
        }
        Instr::CmpImm(reg, val) => {
            let r = reg.to_num();
            dynasm!(ops; .arch x64; cmp Rq(r), *val as i32);
        }
        Instr::Je(label_name) => {
            let lbl = labels.get(label_name).unwrap();
            dynasm!(ops; .arch x64; je => *lbl);
        }
        Instr::Jnz(label_name) => {
            let lbl = labels.get(label_name).unwrap();
            dynasm!(ops; .arch x64; jnz => *lbl);
        }
        Instr::Jz(label_name) => {
            let lbl = labels.get(label_name).unwrap();
            dynasm!(ops; .arch x64; jz => *lbl);
        }
        Instr::Jo(label_name) => {
            let lbl = labels.get(label_name).unwrap();
            dynasm!(ops; .arch x64; jo => *lbl);
        }
        Instr::Jmp(label_name) => {
            let lbl = labels.get(label_name).unwrap();
            dynasm!(ops; .arch x64; jmp => *lbl);
        }
        Instr::Call(label_name) => {
            let lbl = labels.get(label_name).unwrap();
            dynasm!(ops; .arch x64; call => *lbl);
        }
        Instr::Ret => {
            dynasm!(ops; .arch x64; ret);
        }
        Instr::Label(label_name) => {
            let lbl = labels.get(label_name).unwrap();
            dynasm!(ops; .arch x64; => *lbl);
        }
    }
}
