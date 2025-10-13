use sexp::Atom::*;
use sexp::*;
use std::env;
use std::fs::File;
use std::io::*;
use im::HashMap;


#[derive(Debug, Clone)]
enum Reg{
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
enum Instr {
  Mov(Reg, i32),         // mov register, immediate
//   Plus(Reg, i32, i32),         // mov register, immediate
  Add(Reg, i32),         // add register, immediate
  Sub(Reg, i32),         // sub register, immediate
  AddReg(Reg, Reg),      // add register, register
  MovToStack(Reg, i32),  // mov [rsp - offset], register
  MovFromStack(Reg, i32), // mov register, [rsp - offset]
}



enum Expr {
    Num(i32),
    Add1(Box<Expr>),
    Let(String, Box<Expr>, Box<Expr>),
    // Plus(Box<Expr>, Box<Expr>),
    Id(String),
    // UnOp(Op1, Box<Expr>),
    // BinOp(Op2, Box<Expr>, Box<Expr>),
}

fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => Expr::Num(i32::try_from(*n).unwrap()),
        Sexp::Atom(S(name)) => Expr::Id(name.to_string()),
        Sexp::List(vec) => {
            /* */
            match &vec[..] {
                [Sexp::Atom(S(op)), e] if op == "add1" => Expr::Add1(Box::new(parse_expr(e))), // rescursively call parse
                // [Sexp::Atom(S(op)), e1, e2]  if op == "+" => Expr::Plus(Box::new(parse_expr(e1)), Box::new(parse_expr(e2))), // rescursively call parse
                [Sexp::Atom(S(op)), Sexp::List(binding),body ]  if op == "let" =>
                match &binding[..] {
                    [Sexp::Atom(S(var)), val] =>
                        Expr::Let(var.to_string(), Box::new(parse_expr(val)), Box::new(parse_expr(body))),
                        _ => panic!("parse error in let binding")

                    }

                // [Sexp::Atom(S(op)), e] if op == "sub1" => Expr::Sub1(Box::new(parse_expr(e))),
                // [Sexp::Atom(S(op)), e] if op == "negate" => Expr::Negate(Box::new(parse_expr(e))),
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
    Instr::AddReg(reg1, reg2) => format!("add {}, {}", reg_to_string(reg1), reg_to_string(reg2)),
    Instr::MovToStack(reg, offset) => format!("mov [rsp - {}], {}", offset, reg_to_string(reg)),
    Instr::MovFromStack(reg, offset) => format!("mov {}, [rsp - {}]", reg_to_string(reg), offset),
  }
}

// fn compile_expr(e: &Expr) -> String {
//     match e {
//         Expr::Num(n) => format!("mov rax, {}", *n),
//         Expr::Add1(subexpr) => compile_expr(subexpr) + "\nadd rax, 1",
//         Expr::Sub1(subexpr) => compile_expr(subexpr) + "\nsub rax, 1",
//         Expr::Negate(subexpr) => compile_expr(subexpr) + "\nimul rax, rax, -1",
//     }
// }

// fn compile_expr(e: &Expr, si: i32) -> String {
//     match e{
//         Expr::Num(n) => format!("mov rax, {}", *n),
//         Expr::Add1(subexpr) =>{
//             compile_expr(subexpr, si ) + "\nadd rax , 1"
//         },
//         Expr::Plus(e1, e2) => {
//             let e1_instr = compile_expr(e1, si);
//             let e2_instr = compile_expr(e2, si +1);
//             let stack_offset = si * 8;
//             format!("
//     {e1_instr}
//     mov [rsp - {stack_offset}], rax
//     {e2_instr}
//     add rax, [rsp - {stack_offset}]
//                 ")
//         },
//         // Expr::Id() => {}
//     }

//     // match e {
//     //     Expr::Num(n) => format!("mov rax, {}", *n),
//     //     Expr::Add1(subexpr) => compile_expr(subexpr) + "\nadd rax, 1",
//     //     Expr::Sub1(subexpr) => compile_expr(subexpr) + "\nsub rax, 1",
//     //     Expr::Negate(subexpr) => compile_expr(subexpr) + "\nimul rax, rax, -1",
//     // }
// }

fn compile_expr_with_env(e: &Expr, stack_depth: i32, env: &HashMap<String, i32>) -> Vec<Instr> {
  match e {
	Expr::Num(n) => vec![Instr::Mov(Reg::Rax, *n)],
	Expr::Id(name) => {
      match env.get(name) {
        Some(offset) => vec![Instr::MovFromStack(Reg::Rax, *offset)],
        None => panic!("Unbound variable: {}", name)
      }
    },
	Expr::Add1(subexpr) => {
      let mut instrs = compile_expr_with_env(subexpr, stack_depth, env);
      instrs.push(Instr::Add(Reg::Rax, 1));
      instrs
    },
	// Expr::Plus(subexpr1, subexpr2) => {
    //   let mut instrs = compile_expr_with_env(subexpr, stack_depth, env);
    //   instrs.push(Instr::Plus(Reg::Rax, 1));
    //   instrs
    // },
    Expr::Let(var, val_expr, body_expr) => {
      let mut instrs = compile_expr_with_env(val_expr, stack_depth, env);  // Compile value expression
      instrs.push(Instr::MovToStack(Reg::Rax, stack_depth));              // Store value on stack
      
      // Create new environment with this variable mapped to its stack location
      let mut new_env = env.clone();
      new_env.insert(var.clone(), stack_depth);
      
      instrs.extend(compile_expr_with_env(body_expr, stack_depth + 8, &new_env)); // Compile body with extended env
      instrs
    }
  }
}


fn compile_expr(e : &Expr) -> Vec<Instr> {
  compile_expr_with_env(e, 16, &HashMap::new())
}

// fn compile_expr(e: &Expr) -> String {
//     match e {
//         Expr::Num(n) => format!("mov rax, {}", *n),
//         Expr::Add1(subexpr) => compile_expr(subexpr) + "\nadd rax, 1",
//         Expr::Sub1(subexpr) => compile_expr(subexpr) + "\nsub rax, 1",
//         Expr::Negate(subexpr) => compile_expr(subexpr) + "\nimul rax, rax, -1",
//     }
// }

fn instrs_to_string(instrs: &Vec<Instr>) -> String {
  instrs.iter()
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
  let asm_program = format!("
section .text
global our_code_starts_here
our_code_starts_here:
  {}
  ret
", result);

  let mut out_file = File::create(out_name)?;
  out_file.write_all(asm_program.as_bytes())?;

  Ok(())
    //

}
