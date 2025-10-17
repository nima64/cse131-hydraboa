use dynasmrt::{dynasm, DynamicLabel, DynasmApi};
use im::HashMap;
use sexp::Atom::*;
use sexp::*;
use std::env;
use std::fs::File;
use std::io;
use std::io::*;
use std::mem;
use std::panic;

#[derive(Debug, Clone, Copy)]
enum Reg {
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
    fn to_num(&self) -> u8 {
        *self as u8
    }
}
#[derive(Debug, Clone)]
enum Val {
    Reg(Reg),
    Imm(i32),
}

#[derive(Debug, Clone)]
enum Instr {
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
enum Op1 {
    Add1,
    Sub1,
}

#[derive(Debug, Clone)]
enum Op2 {
    Plus,
    Minus,
    Times,
}

#[derive(Debug)]
enum Expr {
    Num(i32),
    // Add1(Box<Expr>),
    Let(Vec<(String, Expr)>, Box<Expr>),

    // Plus(Box<Expr>, Box<Expr>),
    Id(String),
    UnOp(Op1, Box<Expr>),             // unary operation
    Define(String, Box<Expr>),        // Add this directly
    BinOp(Op2, Box<Expr>, Box<Expr>), //binary operation
    Empty,
}

fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => Expr::Num(i32::try_from(*n).unwrap()),
        Sexp::Atom(S(name)) => Expr::Id(name.to_string()),
        Sexp::List(vec) => {
            /* */
            match &vec[..] {
                [Sexp::Atom(S(op)), e] if op == "add1" => {
                    Expr::UnOp(Op1::Add1, Box::new(parse_expr(e)))
                } // rescursively call parse
                [Sexp::Atom(S(op)), e] if op == "sub1" => {
                    Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e)))
                }
                [Sexp::Atom(S(op)), e1, e2] if op == "+" => Expr::BinOp(
                    Op2::Plus,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                ),
                [Sexp::Atom(S(op)), e1, e2] if op == "-" => Expr::BinOp(
                    Op2::Minus,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                ),
                [Sexp::Atom(S(op)), e1, e2] if op == "*" => Expr::BinOp(
                    Op2::Times,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                ),

                [Sexp::Atom(S(op)), Sexp::List(bindings), body] if op == "let" => {
                    // Map each binding to (var_name, parsed_expr) tuple
                    let parsed_bindings: Vec<(String, Expr)> = bindings
                        .iter()
                        .map(|binding| match binding {
                            Sexp::List(pair) => match &pair[..] {
                                [Sexp::Atom(S(var)), val] => (var.to_string(), parse_expr(val)),
                                _ => panic!("Invalid binding: expected (variable value)"),
                            },
                            _ => panic!("Invalid binding: expected a list"),
                        })
                        .collect();

                    Expr::Let(parsed_bindings, Box::new(parse_expr(body)))
                }
                [Sexp::Atom(S(op)), Sexp::Atom(S(name)), e] if op == "define" => {
                    Expr::Define(name.to_string(), Box::new(parse_expr(e)))
                }

                _ => panic!("parse error!"),
            }
        }
        _ => panic!("parse error"),
    }
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

fn instr_to_string(instr: &Instr) -> String {
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

fn instr_to_asm(i: &Instr, ops: &mut dynasmrt::x64::Assembler) {
    match i {
        Instr::Mov(reg, val) => {
            let r = reg.to_num();
            dynasm!(ops; .arch x64; mov Rq(r), *val);
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
            let r = reg.to_num();
            dynasm!(ops; .arch x64; mov Rq(r), [rsp - *offset]);
        }
        Instr::MovDeref(dest, src) => {
            let d = dest.to_num();
            let s = src.to_num();
            dynasm!(ops; .arch x64; mov Rq(d), [Rq(s)]);
        }
    }
}

fn compile_expr_with_env_repl(
    e: &Expr,
    stack_depth: i32,
    env: &HashMap<String, i32>,
    replEnv: &mut HashMap<String, Box<i64>>,
) -> Vec<Instr> {
    match e {
        Expr::Num(n) => vec![Instr::Mov(Reg::Rax, *n)],
        Expr::Id(name) => {
            // Check env (stack) first for local variables
            if let Some(offset) = env.get(name) {
                vec![Instr::MovFromStack(Reg::Rax, *offset)]
            } else if let Some(boxed_value) = replEnv.get(name) {
                // If not in env, check replEnv for defined variables
                println!("value held in {}: {}", name, **boxed_value);
                vec![Instr::Mov(Reg::Rax, **boxed_value as i32)]
            } else {
                panic!("Unbound variable identifier {}", name)
            }
        }
        Expr::UnOp(op, subexpr) => {
            let mut instrs = compile_expr_with_env_repl(subexpr, stack_depth, env, replEnv);
            if matches!(op, Op1::Add1) {
                instrs.push(Instr::Add(Reg::Rax, 1));
            } else if matches!(op, Op1::Sub1) {
                instrs.push(Instr::Add(Reg::Rax, -1));
            } else {
                panic!("Invalid op {:?}!", op);
            }
            instrs
        }
        Expr::BinOp(op, e1, e2) => {
            let mut instrs = compile_expr_with_env_repl(e1, stack_depth, env, replEnv);
            instrs.push(Instr::MovToStack(Reg::Rax, stack_depth));
            instrs.extend(compile_expr_with_env_repl(
                e2,
                stack_depth + 8,
                env,
                replEnv,
            ));
            if matches!(op, Op2::Plus) {
                instrs.push(Instr::MovFromStack(Reg::Rcx, stack_depth));
                instrs.push(Instr::AddReg(Reg::Rax, Reg::Rcx));
            } else if matches!(op, Op2::Minus) {
                instrs.push(Instr::MovFromStack(Reg::Rcx, stack_depth));
                instrs.push(Instr::MinusReg(Reg::Rcx, Reg::Rax)); // Rcx - Rax = 10 - 2
                instrs.push(Instr::MovReg(Reg::Rax, Reg::Rcx)); // Move result back to Rax
            } else if matches!(op, Op2::Times) {
                instrs.push(Instr::MovFromStack(Reg::Rcx, stack_depth));
                instrs.push(Instr::iMul(Reg::Rax, Reg::Rcx));
            } else {
                panic!("Invalid op {:?}!", op);
            }
            instrs
        }

        Expr::Let(bindings, body) => {
            let mut instrs = Vec::new();
            let mut new_env = env.clone();
            let mut current_depth = stack_depth;

            for (var, val_expr) in bindings {
                instrs.extend(compile_expr_with_env_repl(
                    val_expr,
                    current_depth,
                    &new_env,
                    replEnv,
                ));
                instrs.push(Instr::MovToStack(Reg::Rax, current_depth));

                new_env.insert(var.clone(), current_depth);
                current_depth += 8;
            }

            instrs.extend(compile_expr_with_env_repl(
                body,
                current_depth,
                &new_env,
                replEnv,
            ));
            instrs
        }

        Expr::Define(name, e) => {
            let instrs = compile_expr_with_env_repl(e, stack_depth, env, replEnv);
            let val = jitCode(&instrs);

            let boxed_val = Box::new(val);
            if !replEnv.contains_key(name) {
                // Key already exists, you can handle this case if needed
                replEnv.insert(name.clone(), boxed_val); // Store the Box directly
            } else {
                println!("Duplicate binding");
            }

            vec![]
        }
        Expr::Empty => vec![],
    }
}

fn compile_expr(e: &Expr) -> Vec<Instr> {
    compile_expr_with_env_repl(e, 16, &HashMap::new(), &mut HashMap::new())
}

fn compile_expr_repl(e: &Expr, replEnv: &mut HashMap<String, Box<i64>>) -> Vec<Instr> {
    compile_expr_with_env_repl(e, 16, &HashMap::new(), replEnv)
}

fn instrs_to_string(instrs: &Vec<Instr>) -> String {
    instrs
        .iter()
        .map(instr_to_string)
        .collect::<Vec<String>>()
        .join("\n")
}

fn jitCode(instrs: &Vec<Instr>) -> i64 {
    let mut ops: dynasmrt::Assembler<dynasmrt::x64::X64Relocation> =
        dynasmrt::x64::Assembler::new().unwrap();
    let start = ops.offset();
    dynasm!(ops; .arch x64);

    for instr in instrs.iter() {
        instr_to_asm(instr, &mut ops);
    }

    dynasm!(ops; .arch x64; ret);

    let buf = ops.finalize().unwrap();
    let jitted_fn: extern "C" fn() -> i64 = unsafe { mem::transmute(buf.ptr(start)) };
    let result = jitted_fn();
    result
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    // Check for flags
    let use_jit = args.iter().any(|arg| arg == "-e");
    let use_aot = args.iter().any(|arg| arg == "-c");
    let use_repl = args.iter().any(|arg| arg == "-i");

    let mut ops: dynasmrt::Assembler<dynasmrt::x64::X64Relocation> =
        dynasmrt::x64::Assembler::new().unwrap();
    let start = ops.offset();

    if use_jit {
        // JIT compilation path (-e or -g)
        let in_name = &args[2]; // Second arg after flag

        let mut in_file = File::open(in_name)?;
        let mut in_contents = String::new();
        in_file.read_to_string(&mut in_contents)?;

        let expr: Expr = parse_expr(&parse(&in_contents).unwrap());
        let instrs = compile_expr(&expr);

        let mut ops: dynasmrt::Assembler<dynasmrt::x64::X64Relocation> =
            dynasmrt::x64::Assembler::new().unwrap();
        let start = ops.offset();
        jitCode(&instrs);
    } else if use_repl {
        let mut replEnv: HashMap<String, Box<i64>> = HashMap::new();
        while true {
            print!("> ");
            io::stdout().flush().unwrap();
            let mut input = String::new();
            io::stdin().read_line(&mut input)?;
            if (input.trim() == "quit") {
                break;
            }
            let sexp = match parse(&input) {
                Ok(s) => s,
                Err(e) => {
                    println!("Invalid: parse error - {}", e);
                    continue;
                }
            };

            let expr_result = panic::catch_unwind(|| {
                parse_expr(&sexp)
            });

            let expr = match expr_result {
                Ok(e) => e,
                Err(_) => {
                    println!("Invalid: expression error");
                    continue;
                }
            };

            // Step 3: Compile the expression (might panic)
            let compile_result = panic::catch_unwind(panic::AssertUnwindSafe(|| {
                compile_expr_repl(&expr, &mut replEnv)
            }));

            let instrs = match compile_result {
                Ok(i) => i,
                Err(_) => {
                    println!("Invalid: compilation error");
                    continue;
                }
            };

            // Step 4: Execute and print result
            if !instrs.is_empty() {
                let result = jitCode(&instrs);
                println!("result: {}", result);
            }
        }
    } else if use_aot {
        let in_name: &_ = &args[2];
        let out_name = &args[3];

        let mut in_file = File::open(in_name)?;
        let mut in_contents = String::new();
        in_file.read_to_string(&mut in_contents)?;

        let expr = parse_expr(&parse(&in_contents).unwrap());
        let instrs = compile_expr(&expr);
        let result = instrs_to_string(&instrs);

        let asm_program = format!(
            "section .text\nglobal our_code_starts_here\nour_code_starts_here:\n  {}\n  ret\n",
            result
        );

        let mut out_file = File::create(out_name)?;
        out_file.write_all(asm_program.as_bytes())?;
    }
    Ok(())
}
