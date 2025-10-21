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

mod types;
mod assembly;
use types::*;
use assembly::*;

fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => Expr::Number(tag_number(*n)),
        Sexp::Atom(S(name)) => {
            if(name == "true"){
                return Expr::Boolean(true);
            }else if (name == "false"){
                return Expr::Boolean(false);
            }
           Expr::Id(name.to_string())
        },
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


fn compile_expr_with_env_repl(
    e: &Expr,
    stack_depth: i32,
    env: &HashMap<String, i32>,
    replEnv: &mut HashMap<String, Box<i64>>,
) -> Vec<Instr> {
    match e {
        Expr::Number(n) => vec![Instr::Mov(Reg::Rax, *n)],
        Expr::Id(name) => {
                        // Check env (stack) first for local variables
                        if let Some(offset) = env.get(name) {
                            vec![Instr::MovFromStack(Reg::Rax, *offset)]
                        } else if let Some(boxed_value) = replEnv.get(name) {
                            // If not in env, check replEnv for defined variables
                            println!("value held in {}: {}", name, **boxed_value);
                            vec![Instr::Mov(Reg::Rax, **boxed_value)]
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
                let mut duplicateBinding = HashMap::new();
                for (var, val_expr) in bindings {
                    if duplicateBinding.contains_key(var) {
                        panic!("Duplicate binding");
                    }
                    duplicateBinding.insert(var, 1);
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
            },
        Expr::Boolean(b) => {
            if *b == true {
                return vec![Instr::Mov(Reg::Rax, TRUE_TAGGED)];
            }
            vec![Instr::Mov(Reg::Rax, FLASE_TAGGED)]
        }
    }
}

fn compile_expr(e: &Expr) -> Vec<Instr> {
    compile_expr_with_env_repl(e, 16, &HashMap::new(), &mut HashMap::new())
}

fn compile_expr_repl(e: &Expr, replEnv: &mut HashMap<String, Box<i64>>) -> Vec<Instr> {
    compile_expr_with_env_repl(e, 16, &HashMap::new(), replEnv)
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

fn format_result(res: i64) -> String{
    let tag: i64 = res & 1;
    if(tag == BOOL_TAG){
        if (res == TRUE_TAGGED){
            return "true".to_string();
        }else{
            return "false".to_string();
        }
    }

    return untag_number(res).to_string();
        
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    // Check for flags
    let use_jit = args.iter().any(|arg| arg == "-e");
    let use_aot = args.iter().any(|arg| arg == "-c");
    let use_repl = args.iter().any(|arg| arg == "-i");


    if use_jit {
        // JIT compilation path (-e or -g)
        let in_name = &args[2]; // Second arg after flag

        let mut in_file = File::open(in_name)?;
        let mut in_contents = String::new();
        in_file.read_to_string(&mut in_contents)?;

        let expr: Expr = parse_expr(&parse(&in_contents).unwrap());
        let instrs = compile_expr(&expr);

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
                println!("{}", format_result(result));
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
