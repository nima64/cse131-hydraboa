use dynasmrt::{dynasm, DynamicLabel, DynasmApi, DynasmLabelApi};
use im::HashMap;
use sexp::Atom::*;
use sexp::*;
use std::env;
use std::fs::File;
use std::io;
use std::io::*;
use std::mem;
use std::panic;

mod assembly;
mod types;
use assembly::*;
use types::*;

#[derive(Clone, Copy)] // Add Copy!
struct CompileCtx {
    label_counter: i32,
    loop_depth: i32,
    current_loop_id: i32, // current_loop_end: Option<String>
}

fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => Expr::Number(tag_number(*n)),
        Sexp::Atom(S(name)) => {
            //
            /* Check is boolean */
            if (name == "true") {
                return Expr::Boolean(true);
            } else if (name == "false") {
                return Expr::Boolean(false);
            }
            Expr::Id(name.to_string())
        }
        Sexp::List(vec) => {
            /* */
            match &vec[..] {
                [Sexp::Atom(S(op)), e] if op == "add1" => {
                    Expr::UnOp(Op1::Add1, Box::new(parse_expr(e)))
                }
                [Sexp::Atom(S(op)), e] if op == "sub1" => {
                    Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e)))
                }
                [Sexp::Atom(S(op)), e] if op == "loop" => Expr::Loop(Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "break" => Expr::Break(Box::new(parse_expr(e))),
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
                [Sexp::Atom(S(op)), e1, e2] if op == "<" => Expr::BinOp(
                    Op2::Less,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                ),
                [Sexp::Atom(S(op)), e1, e2] if op == ">" => Expr::BinOp(
                    Op2::Greater,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                ),
                [Sexp::Atom(S(op)), e1, e2] if op == "<=" => Expr::BinOp(
                    Op2::LessEqual,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                ),
                [Sexp::Atom(S(op)), e1, e2] if op == ">=" => Expr::BinOp(
                    Op2::GreaterEqual,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                ),
                [Sexp::Atom(S(op)), e1, e2] if op == "=" => Expr::BinOp(
                    Op2::Equal,
                    Box::new(parse_expr(e1)),
                    Box::new(parse_expr(e2)),
                ),
                [Sexp::Atom(S(op)), exprs @ ..] if op == "block" => {
                    if exprs.is_empty() {
                        panic!("Invalid: block needs at least one expression");
                    }
                    Expr::Block(exprs.iter().map(parse_expr).collect())
                }

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
                [Sexp::Atom(S(op)), Sexp::Atom(S(name)), e] if op == "set!" => {
                    Expr::Set(name.to_string(), Box::new(parse_expr(e)))
                }

                [Sexp::Atom(S(op)), cond, then_expr, else_expr] if op == "if" => Expr::If(
                    Box::new(parse_expr(cond)),
                    Box::new(parse_expr(then_expr)),
                    Box::new(parse_expr(else_expr)),
                ),

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
    mut ctx: CompileCtx, // Pass by value (copied each call)
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
            let mut instrs = compile_expr_with_env_repl(subexpr, stack_depth, env, replEnv, ctx);
            if matches!(op, Op1::Add1) {
                instrs.push(Instr::Mov(Reg::Rcx, tag_number(1)));
                instrs.push(Instr::AddReg(Reg::Rax, Reg::Rcx));
            } else if matches!(op, Op1::Sub1) {
                // copy num into rcx
                instrs.push(Instr::Mov(Reg::Rcx, tag_number(-1)));
                // add -1 to num
                instrs.push(Instr::AddReg(Reg::Rax, Reg::Rcx));
            } else {
                panic!("Invalid op {:?}!", op);
            }
            instrs
        }
        Expr::BinOp(op, e1, e2) => {
            let mut instrs = compile_expr_with_env_repl(e1, stack_depth, env, replEnv, ctx);
            instrs.push(Instr::MovToStack(Reg::Rax, stack_depth));
            instrs.extend(compile_expr_with_env_repl(
                e2,
                stack_depth + 8,
                env,
                replEnv,
                ctx,
            ));
            match op {
                Op2::Equal => {
                    instrs.push(Instr::MovFromStack(Reg::Rcx, stack_depth)); // e1 in RCX, e2 in RAX

                    // Type check: both must be same type
                    // XOR the values - if tags differ, bit 0 will be 1
                    instrs.push(Instr::MovReg(Reg::Rdx, Reg::Rcx)); // Copy e1 to RDX
                    instrs.push(Instr::XorReg(Reg::Rdx, Reg::Rax)); // XOR e1 with e2
                    instrs.push(Instr::Test(Reg::Rdx, 1)); // Test bit 0

                    // If bit 0 is set, types differ -> error
                    instrs.push(Instr::Jnz("error".to_string()));

                    // Types match, do comparison
                    instrs.push(Instr::Cmp(Reg::Rcx, Reg::Rax));
                    instrs.push(Instr::SetEq(Reg::Rax));

                    // Tag as boolean
                    instrs.push(Instr::Shl(Reg::Rax, 1));
                    instrs.push(Instr::Or(Reg::Rax, 1));
                }
                Op2::Less | Op2::Greater => {
                    instrs.push(Instr::MovFromStack(Reg::Rcx, stack_depth));
                    instrs.push(Instr::Cmp(Reg::Rcx, Reg::Rax));

                    match op {
                        Op2::Less => instrs.push(Instr::SetL(Reg::Rax)),
                        Op2::Greater => instrs.push(Instr::SetG(Reg::Rax)),
                        _ => unreachable!(),
                    }

                    // Tag as boolean
                    instrs.push(Instr::Shl(Reg::Rax, 1));
                    instrs.push(Instr::Or(Reg::Rax, 1));
                }
                Op2::Plus => {
                    instrs.push(Instr::MovFromStack(Reg::Rcx, stack_depth));
                    instrs.push(Instr::AddReg(Reg::Rax, Reg::Rcx));
                }
                Op2::Minus => {
                    instrs.push(Instr::MovFromStack(Reg::Rcx, stack_depth));
                    instrs.push(Instr::MinusReg(Reg::Rcx, Reg::Rax));
                    instrs.push(Instr::MovReg(Reg::Rax, Reg::Rcx));
                }
                Op2::Times => {
                    instrs.push(Instr::MovFromStack(Reg::Rcx, stack_depth));
                    instrs.push(Instr::iMul(Reg::Rax, Reg::Rcx));
                }
                _ => panic!("Invalid op {:?}!", op),
            }
            instrs
        }
        Expr::Let(bindings, body) => {
            let mut instrs = Vec::new();
            let mut new_env = env.clone();
            let mut current_depth = stack_depth;
            let mut duplicate_binding = HashMap::new();
            for (var, val_expr) in bindings {
                if duplicate_binding.contains_key(var) {
                    panic!("Duplicate binding");
                }
                duplicate_binding.insert(var, 1);
                instrs.extend(compile_expr_with_env_repl(
                    val_expr,
                    current_depth,
                    &new_env,
                    replEnv,
                    ctx,
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
                ctx,
            ));
            instrs
        }
        Expr::Define(name, e) => {
            let instrs = compile_expr_with_env_repl(e, stack_depth, env, replEnv, ctx);
            let val = jit_code(&instrs);

            let boxed_val = Box::new(val);
            if !replEnv.contains_key(name) {
                // Key already exists, you can handle this case if needed
                replEnv.insert(name.clone(), boxed_val); // Store the Box directly
            } else {
                println!("Duplicate binding");
            }

            vec![]
        }
        Expr::Boolean(b) => {
            if *b == true {
                return vec![Instr::Mov(Reg::Rax, TRUE_TAGGED)];
            }
            vec![Instr::Mov(Reg::Rax, FLASE_TAGGED)]
        }
        Expr::Block(exprs) => {
            let mut instrs: Vec<Instr> = vec![];

            for expr in exprs {
                instrs.extend(compile_expr_with_env_repl(
                    expr,
                    stack_depth,
                    env,
                    replEnv,
                    ctx,
                ));
            }
            instrs
        }
        Expr::Loop(e) => {
            let loop_id = ctx.label_counter;
            ctx.label_counter += 1;

            let loop_label = format!("loop_{}", loop_id);
            let end_loop_label = format!("endloop_{}", loop_id);

            // Create new context with this loop's ID
            let mut loop_ctx = ctx;
            loop_ctx.current_loop_id = loop_id;
            loop_ctx.loop_depth += 1;

            let mut instrs = vec![];
            instrs.push(Instr::Label(loop_label.clone()));
            instrs.extend(compile_expr_with_env_repl(
                e,
                stack_depth,
                env,
                replEnv,
                loop_ctx,
            ));
            instrs.push(Instr::Jmp(loop_label.clone()));
            instrs.push(Instr::Label(end_loop_label));
            instrs
        }
        Expr::Break(e) => {
            if ctx.loop_depth == 0 {
                panic!("Invalid: break outside of loop");
            }
            let mut instrs = vec![];
            instrs.extend(compile_expr_with_env_repl(
                e,
                stack_depth,
                env,
                replEnv,
                ctx,
            ));
            let loop_end_label = format!("endloop_{}", ctx.current_loop_id);
            instrs.push(Instr::Jmp(loop_end_label));
            instrs
        }
        Expr::Set(name, e) => {
            // Check if variable is bound in the environment
            if !env.contains_key(name) && !replEnv.contains_key(name) {
                panic!("Unbound variable identifier {}", name);
            }

            let mut instrs = vec![];

            // Compile the expression to assign (result in RAX)
            instrs.extend(compile_expr_with_env_repl(
                e,
                stack_depth,
                env,
                replEnv,
                ctx,
            ));

            // Store RAX into the variable's stack location
            if let Some(offset) = env.get(name) {
                // Variable is on the stack (from let binding)
                instrs.push(Instr::MovToStack(Reg::Rax, *offset));
            } else if replEnv.contains_key(name) {
                // Variable is in replEnv (from define) - for now just panic
                // We'll handle this properly when implementing REPL with set!
                panic!("set! on define variables not yet supported in basic compilation");
            }

            // set! evaluates to the new value (already in RAX)
            instrs
        }
        Expr::If(cond, then_expr, else_expr) => {
            let mut instrs = vec![];
            // Generate unique labels using the current label counter
            let label_id = ctx.label_counter;
            ctx.label_counter += 1; // Increment for next label (propagates automatically!)

            let else_label = format!("else_{}", label_id);
            let end_label = format!("end_if_{}", label_id);

            // 1. Compile and evaluate the condition (result in RAX)
            instrs.extend(compile_expr_with_env_repl(
                cond,
                stack_depth,
                env,
                replEnv,
                ctx,
            ));

            instrs.push(Instr::CmpImm(Reg::Rax, FLASE_TAGGED));

            // 3. Jump to else branch if condition equals false
            instrs.push(Instr::Je(else_label.clone()));

            // 4. Compile then branch (executed if condition is NOT false)
            instrs.extend(compile_expr_with_env_repl(
                then_expr,
                stack_depth,
                env,
                replEnv,
                ctx,
            ));

            // 5. Jump to end (skip else branch)
            instrs.push(Instr::Jmp(end_label.clone()));

            // 6. Else branch label
            instrs.push(Instr::Label(else_label));

            instrs.extend(compile_expr_with_env_repl(
                else_expr,
                stack_depth,
                env,
                replEnv,
                ctx,
            ));

            // 8. End label
            instrs.push(Instr::Label(end_label));

            instrs
        }
    }
}

fn compile_expr(e: &Expr) -> Vec<Instr> {
    let stack_depth = 16;
    let mut env = HashMap::new();
    //mov input into stack
    let mut prepend_instrs = vec![];
    prepend_instrs.push(Instr::MovToStack(Reg::Rdi, stack_depth));
    env.insert("input".to_string(), stack_depth);
    let ctx = CompileCtx {
        label_counter: 0,
        loop_depth: 0,
        current_loop_id: -1,
    };
    let instrs = compile_expr_with_env_repl(e, stack_depth + 8, &env, &mut HashMap::new(), ctx);
    prepend_instrs.extend(instrs);
    prepend_instrs
}

fn compile_expr_repl(e: &Expr, replEnv: &mut HashMap<String, Box<i64>>) -> Vec<Instr> {
    let ctx = CompileCtx {
        label_counter: 0,
        loop_depth: 0,
        current_loop_id: -1,
    };
    compile_expr_with_env_repl(e, 16, &HashMap::new(), replEnv, ctx)
}

pub extern "C" fn snek_error(errorcode: i64) {
    match errorcode {
        1 => {
            eprintln!("cannot compare different types!");
            std::process::exit(1);
        }
        _ => {
            panic!("invalid error code {}!", errorcode);
        }
    }
}

fn jit_code_input(instrs: &Vec<Instr>, input: i64) -> i64 {
    let mut ops: dynasmrt::Assembler<dynasmrt::x64::X64Relocation> =
        dynasmrt::x64::Assembler::new().unwrap();
    let start = ops.offset();
    dynasm!(ops; .arch x64);

    let mut labels: HashMap<String, DynamicLabel> = HashMap::new();
    let error = ops.new_dynamic_label();
    labels.insert("error".to_string(), error);
    let c_func_ptr: extern "C" fn(i64) -> i64 = unsafe { mem::transmute(snek_error as *const ()) };

    
    // Pre-create all labels
    for instr in instrs {
        if let Instr::Label(name) = instr {
            labels.insert(name.clone(), ops.new_dynamic_label());
        }
    }
    
    // Now emit all instructions
    for instr in instrs {
        instr_to_asm(instr, &mut ops, &labels);
    }


    dynasm!(ops
        ; jmp ->done
    );
    dynasm!(ops
        ; =>error
        // ; mov rax, tag_number(6999999) as i32
        ; mov rax, QWORD c_func_ptr as i64 // Load the C function address into RAX
        ; mov rdi, 1 // Set the argument for the C function (assuming x64 calling convention)
        ; call rax // Call the C function
        ; ret // Return from the generated code
    );

    dynasm!(ops
        ; ->done:
        ; ret
    );

    let buf = ops.finalize().unwrap();
    let jitted_fn: extern "C" fn(i64) -> i64 = unsafe { mem::transmute(buf.ptr(start)) };
    let result = jitted_fn(input);
    result
}

fn jit_code(instrs: &Vec<Instr>) -> i64 {
    jit_code_input(instrs, FLASE_TAGGED)
}

fn format_result(res: i64) -> String {
    let tag = get_tag(res);
    if tag == BOOL_TAG {
        if res == TRUE_TAGGED {
            return "true".to_string();
        } else {
            return "false".to_string();
        }
    }
    return untag_number(res).to_string();
}

fn parse_input(input: &str) -> i64 {
    let trimmed = input.trim();
    let res = trimmed.parse::<i64>();
    if let Ok(n) = res {
        return tag_number(n);
    }
    match trimmed {
        "true" => TRUE_TAGGED ,
        "false" => FLASE_TAGGED ,
        _ => FLASE_TAGGED,
    }
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
        let input_arg = if args.len() >= 4 { &args[3] } else { "false" };
        let input = parse_input(input_arg);

        let mut in_file = File::open(in_name)?;
        let mut in_contents = String::new();
        in_file.read_to_string(&mut in_contents)?;

        let expr: Expr = parse_expr(&parse(&in_contents).unwrap());
        let instrs = compile_expr(&expr);

        let result = jit_code_input(&instrs, input);
        println!("{}", format_result(result));
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

            let expr_result = panic::catch_unwind(|| parse_expr(&sexp));

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
                let result = jit_code(&instrs);
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
            "
section .text
extern snek_error
global our_code_starts_here
our_code_starts_here:
{}  
ret
            ",
            result
        );

        let mut out_file = File::create(out_name)?;
        out_file.write_all(asm_program.as_bytes())?;
    }
    Ok(())
}
