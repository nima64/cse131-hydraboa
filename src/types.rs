
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
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    Equal,
}

pub const TRUE_TAGGED:i64 = 3;
pub const FLASE_TAGGED:i64 = 1;
pub const BOOL_TAG: i64 = 1;
pub const NUM_TAG: i64 = 0;

#[derive(Debug)]
pub enum Expr {
    // Num(i64),
    Number(i64),
    Boolean(bool),
    Let(Vec<(String, Expr)>, Box<Expr>),
    Id(String),
    UnOp(Op1, Box<Expr>),
    Define(String, Box<Expr>),
    Block(Vec<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Loop(Box<Expr>),
    Break(Box<Expr>),
    Set(String, Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum Instr {
    // IMov(Reg, RegOrImm),
    Mov(Reg, i64), // mov register, immediate
    Add(Reg, i32), // add register, immediate
    Sub(Reg, i32), // sub register, immediate
    iMul(Reg, Reg),
    AddReg(Reg, Reg),
    MinusReg(Reg, Reg),
    MovReg(Reg, Reg),
    XorReg(Reg, Reg),
    MovToStack(Reg, i32), // Register, stackdepth => mov [rsp - offset], register
    MovFromStack(Reg, i32), // mov register, [rsp - offset]
    MovDeref(Reg, Reg),   // mov dest_reg, [src_reg] - dereference src_reg and put in dest_reg
    Cmp(Reg, Reg),        // cmp reg1, reg2
    CmpImm(Reg, i64),     // cmp reg, immediate
    SetL(Reg),            // setl reg - set if less
    SetG(Reg),            // setg reg - set if greater
    SetEq(Reg),            // setg reg - set if greater
    Shl(Reg, i32),        // shl reg, immediate - shift left
    Or(Reg, i64),         // or reg, immediate - for setting bits
    Test(Reg, i64),       // test reg, immediate - for checking bits
    Jne(String),          // jne label - jump if not equal
    Je(String),           // je label - jump if equal
    Jmp(String),          // jmp label - unconditional jump
    Label(String),        // label: - assembly label
    Error(i32),
}


#[derive(Clone, Copy)]
pub struct CompileCtx {
    pub loop_depth: i32,
    pub label_counter: i32,
    pub current_loop_id: i32,  // -1 means not in a loop, otherwise the loop's label ID
}

pub fn get_tag(n: i64) ->i64{
    n & 1
}

pub fn tag_number(n:i64) -> i64{
    return n << 1;
}
pub fn untag_number(n:i64) -> i64{
    return n >> 1;
}

// Helper to check if a value is a number (tag bit is 0)
pub fn is_number_tag(val: i64) -> bool {
    (val & 1) == 0
}

// Helper to check if a value is a boolean (tag bit is 1)
pub fn is_bool_tag(val: i64) -> bool {
    (val & 1) == 1
}