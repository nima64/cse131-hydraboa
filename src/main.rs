use im::HashMap;
use sexp::Atom::*;
use sexp::*;
use std::env;
use std::fs::File;
use std::io::*;

#[derive(Debug, Clone)]
enum Reg {
    Rax,
    Rbx,
    Rcx,
    Rdx,
    Rsi,
    Rdi,
    Rsp,
    Rbp,
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

enum Expr {
    Num(i32),
    // Add1(Box<Expr>),
    Let(Vec<(String, Expr)>, Box<Expr>),

    // Plus(Box<Expr>, Box<Expr>),
    Id(String),
    UnOp(Op1, Box<Expr>),             // unary operation
    BinOp(Op2, Box<Expr>, Box<Expr>), //binary operation
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
    }
}

fn compile_expr_with_env(e: &Expr, stack_depth: i32, env: &HashMap<String, i32>) -> Vec<Instr> {
    match e {
        Expr::Num(n) => vec![Instr::Mov(Reg::Rax, *n)],
        Expr::Id(name) => match env.get(name) {
            Some(offset) => vec![Instr::MovFromStack(Reg::Rax, *offset)],
            None => panic!("Unbound variable: {}", name),
        },
        Expr::UnOp(op, subexpr) => {
            let mut instrs = compile_expr_with_env(subexpr, stack_depth, env);
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
            let mut instrs = compile_expr_with_env(e1, stack_depth, env);
            instrs.push(Instr::MovToStack(Reg::Rax, stack_depth));
            instrs.extend(compile_expr_with_env(e2, stack_depth + 8, env));
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

            // Process bindings ONE BY ONE
            for (var, val_expr) in bindings {
                // Compile value with environment that includes PREVIOUS bindings
                instrs.extend(compile_expr_with_env(val_expr, current_depth, &new_env));
                instrs.push(Instr::MovToStack(Reg::Rax, current_depth));

                // Add to environment BEFORE next binding
                new_env.insert(var.clone(), current_depth);
                current_depth += 8;
            }

            // Compile body with ALL bindings in scope
            instrs.extend(compile_expr_with_env(body, current_depth, &new_env));
            instrs
        }
    }
}

fn compile_expr(e: &Expr) -> Vec<Instr> {
    compile_expr_with_env(e, 16, &HashMap::new())
}

fn instrs_to_string(instrs: &Vec<Instr>) -> String {
    instrs
        .iter()
        .map(instr_to_string)
        .collect::<Vec<String>>()
        .join("\n")
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let expr = parse_expr(&parse(&in_contents).unwrap());
    let instrs = compile_expr(&expr);
    let result = instrs_to_string(&instrs);
    let asm_program = format!(
        "
section .text
global our_code_starts_here
our_code_starts_here:
  {}
  ret
",
        result
    );

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
    //
}
