
#[derive(Debug, Clone, Copy)]
pub enum Reg {
    Rax = 0,
    Rbx = 3,
    Rcx = 1,
    Rdx = 2,
    Rsi = 6,
    Rdi = 7,
    Rsp = 4,
    Rbp = 5,
}

impl Reg {
    pub fn to_num(&self) -> u8 {
        *self as u8
    }
}
#[derive(Debug, Clone)]
pub enum Val {
    Reg(Reg),
    Imm(i64),
}

#[derive(Debug, Clone)]
pub enum Instr {
    // IMov(Reg, RegOrImm),
    Mov(Reg, i32), // mov register, immediate
    Add(Reg, i32), // add register, immediate
    Sub(Reg, i32), // sub register, immediate
    iMul(Reg, Reg),
    AddReg(Reg, Reg),
    MinusReg(Reg, Reg),
    MovReg(Reg, Reg),
    MovToStack(Reg, i32), // Register, stackdepth => mov [rsp - offset], register
    MovFromStack(Reg, i32), // mov register, [rsp - offset]
    MovDeref(Reg, Reg),   // mov dest_reg, [src_reg] - dereference src_reg and put in dest_reg
}

#[derive(Debug)]
pub enum Op1 {
    Add1,
    Sub1,
}

#[derive(Debug, Clone)]
pub enum Op2 {
    Plus,
    Minus,
    Times,
}

#[derive(Debug)]
pub enum Expr {
    // Num(i64),
    Number(i64),
    Boolean(bool),
    Let(Vec<(String, Expr)>, Box<Expr>),
    Id(String),
    UnOp(Op1, Box<Expr>),
    Define(String, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
    Empty,
}