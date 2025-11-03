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
mod common;
mod types;
use assembly::*;
use types::*;
use types::{Defn, Prog};

#[derive(Clone, Copy)] // Add Copy!
struct CompileCtx {
    label_counter: *mut i32,
    loop_depth: i32,
    current_loop_id: i32,
    max_depth: *mut i32,
}

// fn parse_expr(s: &Sexp, func_names: &HashSet<String>) -> Expr {
fn parse_expr(s: &Sexp) -> Expr {
    match s {
        Sexp::Atom(I(n)) => {
            if !(*n >= -2_i64.pow(62) && *n <= 2_i64.pow(62)-1){
                panic!("not a valid number must be an integer between -2^62 and 2^62-1");
            }
            Expr::Number(tag_number(*n))
        },
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
                [Sexp::Atom(S(op)), e] if op == "isnum" => {
                    Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e)))
                }
                [Sexp::Atom(S(op)), e] if op == "isbool" => {
                    Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e)))
                }
                [Sexp::Atom(S(op)), e] if op == "break" => Expr::Break(Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "loop" => Expr::Loop(Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "break" => Expr::Break(Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "print" => Expr::Print(Box::new(parse_expr(e))),
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

                // Function call: (<name> <expr>*)
                [Sexp::Atom(S(name)), args @ ..] => {
                    let parsed_args = args.iter().map(|arg| parse_expr(arg)).collect();
                    Expr::FunCall(name.to_string(), parsed_args)
                }

                _ => panic!("parse error!"),
            }
        }
        _ => panic!("parse error"),
    }
}

fn parse_defn(s: &Sexp) -> Defn {
    match s {
        Sexp::List(vec) => match &vec[..] {
            [Sexp::Atom(S(keyword)), Sexp::List(signature), body] if keyword == "fun" => {
                match &signature[..] {
                    [Sexp::Atom(S(name)), params @ ..] => {
                        let param_names: Vec<String> = params
                            .iter()
                            .map(|p| match p {
                                Sexp::Atom(S(param_name)) => param_name.to_string(),
                                _ => panic!("Invalid: function parameter must be an identifier"),
                            })
                            .collect();

                        Defn {
                            name: name.to_string(),
                            params: param_names,
                            body: Box::new(parse_expr(body)),
                        }
                    }
                    _ => panic!("Invalid: function definition must have a name"),
                }
            }
            _ => panic!("Invalid: expected function definition (fun ...)"),
        },
        _ => panic!("Invalid: function definition must be a list"),
    }
}

fn parse_prog(s: &Sexp) -> Prog {
    match s {
        Sexp::List(items) => {
            let mut defns = Vec::new();
            let mut main_expr = None;

            for item in items {
                match item {
                    Sexp::List(inner) => {
                        if let Some(Sexp::Atom(S(keyword))) = inner.first() {
                            if keyword == "fun" {
                                defns.push(parse_defn(item));
                                continue;
                            }
                        }
                        // If we get here, it's not a function definition, so it's the main expr
                        if main_expr.is_none() {
                            main_expr = Some(parse_expr(item));
                        } else {
                            panic!("Invalid: only one main expression allowed");
                        }
                    }
                    _ => {
                        // Any other expression (atoms, etc.) is the main expression
                        if main_expr.is_none() {
                            main_expr = Some(parse_expr(item));
                        } else {
                            panic!("Invalid: only one main expression allowed");
                        }
                    }
                }
            }

            Prog {
                defns,
                main: Box::new(
                    main_expr
                        .unwrap_or_else(|| panic!("Invalid: program must have a main expression")),
                ),
            }
        }
        _ => {
            // Single expression, no function definitions
            Prog {
                defns: Vec::new(),
                main: Box::new(parse_expr(s)),
            }
        }
    }
}

fn compile_expr_with_env_repl(
    e: &Expr,
    stack_depth: i32,
    env: &HashMap<String, i32>,
    define_env: &mut HashMap<String, Box<i64>>,
    defns: &Vec<Defn>,
    mut ctx: CompileCtx, // Pass by value (copied each call)
) -> Vec<Instr> {
    // Track max depth (unsafe to dereference raw pointer)
    unsafe {
        if stack_depth > *ctx.max_depth {
            *ctx.max_depth = stack_depth;
        }
    }

    match e {
        Expr::Number(n) => vec![Instr::Mov(Reg::Rax, *n)],
        Expr::Id(name) => {
            // Check env (stack) first for local variables
            if let Some(offset) = env.get(name) {
                vec![Instr::MovFromStack(Reg::Rax, *offset)]
            } else if let Some(boxed_value) = define_env.get(name) {
                // If not in env, check replEnv for defined variables
                println!("value held in {}: {}", name, untag_number(**boxed_value));
                vec![Instr::Mov(Reg::Rax, **boxed_value)]
            } else {
                panic!("Unbound variable identifier {}", name)
            }
        }
        Expr::UnOp(op, subexpr) => {
            let mut instrs =
                compile_expr_with_env_repl(subexpr, stack_depth, env, define_env, defns, ctx);
            match op {
                Op1::Add1 => {
                    instrs.push(Instr::Mov(Reg::Rcx, tag_number(1)));
                    instrs.push(Instr::AddReg(Reg::Rax, Reg::Rcx));
                    instrs.push(Instr::Jo("overflow_error".to_string()));
                }
                Op1::Sub1 => {
                    instrs.push(Instr::Mov(Reg::Rcx, tag_number(-1)));
                    instrs.push(Instr::AddReg(Reg::Rax, Reg::Rcx));
                    instrs.push(Instr::Jo("overflow_error".to_string()));
                }
                Op1::IsNum => {
                    // Check if tag bit (bit 0) is 0 (number)
                    instrs.push(Instr::Test(Reg::Rax, 1)); // Test bit 0
                    instrs.push(Instr::Mov(Reg::Rax, 0)); // Clear RAX
                    instrs.push(Instr::SetEq(Reg::Rax)); // Set AL to 1 if zero flag is set (tag == 0)
                    instrs.push(Instr::Shl(Reg::Rax, 1)); // Shift left to make room for tag
                    instrs.push(Instr::Or(Reg::Rax, 1)); // Set tag bit to 1 (boolean)
                }
                Op1::IsBool => {
                    let label_id = unsafe {
                        let id = *ctx.label_counter;
                        *ctx.label_counter += 1;
                        id
                    };
                    let skip_label = format!("isbool_skip_{}", label_id);

                    instrs.push(Instr::Test(Reg::Rax, 1)); // Test bit 0
                    instrs.push(Instr::Mov(Reg::Rax, 0)); // Clear RAX
                    instrs.push(Instr::Jz(skip_label.clone())); // If zero (not bool), skip set
                    instrs.push(Instr::Mov(Reg::Rax, 1)); // Set to 1 if bool
                    instrs.push(Instr::Label(skip_label));
                    instrs.push(Instr::Shl(Reg::Rax, 1)); // Shift left to make room for tag
                    instrs.push(Instr::Or(Reg::Rax, 1)); // Set tag bit to 1 (boolean)
                }
            }
            instrs
        }
        Expr::BinOp(op, e1, e2) => {
            let mut instrs =
                compile_expr_with_env_repl(e1, stack_depth, env, define_env, defns, ctx);
            instrs.push(Instr::MovToStack(Reg::Rax, stack_depth));
            // e2 is on rax and e1 is on the stack
            instrs.extend(compile_expr_with_env_repl(
                e2,
                stack_depth + 8,
                env,
                define_env,
                defns,
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
                    instrs.push(Instr::Jnz("type_mismatch_error".to_string()));

                    // Types match, do comparison
                    instrs.push(Instr::Cmp(Reg::Rcx, Reg::Rax));
                    instrs.push(Instr::SetEq(Reg::Rax));

                    // Tag as boolean
                    instrs.push(Instr::Shl(Reg::Rax, 1));
                    instrs.push(Instr::Or(Reg::Rax, 1));
                }
                Op2::Less | Op2::Greater | Op2::LessEqual | Op2::GreaterEqual => {
                    instrs.push(Instr::MovFromStack(Reg::Rcx, stack_depth));
                    // Type check: both must be same type
                    instrs.push(Instr::MovReg(Reg::Rdx, Reg::Rcx)); // Copy e1 to RDX
                    instrs.push(Instr::XorReg(Reg::Rdx, Reg::Rax)); // XOR e1 with e2
                    instrs.push(Instr::Test(Reg::Rdx, 1)); // Test bit 0
                    instrs.push(Instr::Jnz("type_mismatch_error".to_string()));

                    instrs.push(Instr::Cmp(Reg::Rcx, Reg::Rax));

                    match op {
                        Op2::Less => instrs.push(Instr::SetL(Reg::Rax)),
                        Op2::Greater => instrs.push(Instr::SetG(Reg::Rax)),
                        Op2::LessEqual => instrs.push(Instr::SetLE(Reg::Rax)),
                        Op2::GreaterEqual => instrs.push(Instr::SetGE(Reg::Rax)),
                        _ => unreachable!(),
                    }

                    // Tag as boolean
                    instrs.push(Instr::Shl(Reg::Rax, 1));
                    instrs.push(Instr::Or(Reg::Rax, 1));
                }
                Op2::Plus => {
                    instrs.push(Instr::MovFromStack(Reg::Rcx, stack_depth));
                    // type check 
                    instrs.push(Instr::Test(Reg::Rcx, 1));// AND with 1 to check LSB and see if its 1 aka BOOL 
                    instrs.push(Instr::Jnz("type_error_arithmetic".to_string())); // jump to error if it is boolean 
                    instrs.push(Instr::Test(Reg::Rax, 1));
                    instrs.push(Instr::Jnz("type_error_arithmetic".to_string())); 
                    // end type check 

                    instrs.push(Instr::AddReg(Reg::Rax, Reg::Rcx));
                }
                Op2::Minus => {
                    instrs.push(Instr::MovFromStack(Reg::Rcx, stack_depth));
                    // type check 
                    instrs.push(Instr::Test(Reg::Rcx, 1));// AND with 1 to check LSB and see if its 1 aka BOOL 
                    instrs.push(Instr::Jnz("type_error_arithmetic".to_string())); // jump to error if it is boolean 
                    instrs.push(Instr::Test(Reg::Rax, 1));
                    instrs.push(Instr::Jnz("type_error_arithmetic".to_string())); 
                    // end type check 
                    instrs.push(Instr::MinusReg(Reg::Rcx, Reg::Rax));
                    instrs.push(Instr::MovReg(Reg::Rax, Reg::Rcx));
                }
                Op2::Times => {
                    instrs.push(Instr::MovFromStack(Reg::Rcx, stack_depth));
                    // type check 
                    instrs.push(Instr::Test(Reg::Rcx, 1));// AND with 1 to check LSB and see if its 1 aka BOOL 
                    instrs.push(Instr::Jnz("type_error_arithmetic".to_string())); // jump to error if it is boolean 
                    instrs.push(Instr::Test(Reg::Rax, 1));
                    instrs.push(Instr::Jnz("type_error_arithmetic".to_string())); 
                    // end type check 
                    instrs.push(Instr::Sar(Reg::Rax, 1)); // untag rax by shifting right
                    instrs.push(Instr::Sar(Reg::Rcx, 1)); // untag rcx by shifting right
                    instrs.push(Instr::iMul(Reg::Rax, Reg::Rcx)); // multiply untagged * untagged
                    instrs.push(Instr::Jo("overflow_error".to_string()));
                    instrs.push(Instr::Shl(Reg::Rax, 1)); // shift result left to tag it
                    instrs.push(Instr::Jo("overflow_error".to_string()));
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
                    define_env,
                    defns,
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
                define_env,
                defns,
                ctx,
            ));
            instrs
        }
        Expr::Define(name, e) => {
            let instrs = compile_expr_with_env_repl(e, stack_depth, env, define_env, defns, ctx);
            let val = jit_code(&instrs);

            let boxed_val = Box::new(val);
            if !define_env.contains_key(name) {
                define_env.insert(name.clone(), boxed_val);
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
                    define_env,
                    defns,
                    ctx,
                ));
            }
            instrs
        }
        Expr::Loop(e) => {
            let loop_id = unsafe {
                let id = *ctx.label_counter;
                *ctx.label_counter += 1;
                id
            };

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
                define_env,
                defns,
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
                define_env,
                defns,
                ctx,
            ));
            let loop_end_label = format!("endloop_{}", ctx.current_loop_id);
            instrs.push(Instr::Jmp(loop_end_label));
            instrs
        }
        Expr::Set(name, e) => {
            // Check if variable is bound in the environment
            if !env.contains_key(name) && !define_env.contains_key(name) {
                panic!("Unbound variable identifier {}", name);
            }

            let mut instrs = vec![];

            // Compile the expression to assign (result in RAX)
            instrs.extend(compile_expr_with_env_repl(
                e,
                stack_depth,
                env,
                define_env,
                defns,
                ctx,
            ));

            if let Some(offset) = env.get(name) {
                instrs.push(Instr::MovToStack(Reg::Rax, *offset));
            } else if define_env.contains_key(name) {
                // Get the pointer to the heap-allocated i64
                let ptr_addr = &**define_env.get(name).unwrap() as *const i64 as i64;
                instrs.push(Instr::Mov(Reg::Rcx, ptr_addr));
                instrs.push(Instr::MovToMem(Reg::Rcx, Reg::Rax));
            }

            instrs
        }
        Expr::If(cond, then_expr, else_expr) => {
            let mut instrs = vec![];
            let label_id = unsafe {
                let id = *ctx.label_counter;
                *ctx.label_counter += 1;
                id
            };

            let else_label = format!("else_{}", label_id);
            let end_label = format!("end_if_{}", label_id);

            instrs.extend(compile_expr_with_env_repl(
                cond,
                stack_depth,
                env,
                define_env,
                defns,
                ctx,
            ));

            instrs.push(Instr::CmpImm(Reg::Rax, FLASE_TAGGED));

            // 3. Jump to else branch if condition equals false
            instrs.push(Instr::Je(else_label.clone()));

            instrs.extend(compile_expr_with_env_repl(
                then_expr,
                stack_depth,
                env,
                define_env,
                defns,
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
                define_env,
                defns,
                ctx,
            ));

            instrs.push(Instr::Label(end_label));

            instrs
        }
        Expr::FunCall(name, args) => {
            // Find the function definition
            let defn = defns.iter().find(|d| &d.name == name);

            if defn.is_none() {
                panic!("Undefined function: {}", name);
            }

            let defn = defn.unwrap();

            // Check argument count matches
            if args.len() != defn.params.len() {
                panic!(
                    "Function {} expects {} arguments, got {}",
                    name,
                    defn.params.len(),
                    args.len()
                );
            }

            let mut instrs = Vec::new();

            for arg in args {
                instrs.extend(compile_expr_with_env_repl(
                    arg, stack_depth, env,
                    define_env, defns, ctx,
                ));
                instrs.push(Instr::Push(Reg::Rax));
            }

            instrs.push(Instr::Call(name.clone()));

            // Result is now in RAX
            instrs
        }
        Expr::Print(e ) => {
            let mut instrs = compile_expr_with_env_repl(e, stack_depth, env, define_env, defns, ctx);
            instrs.push(Instr::MovReg(Reg::Rdi, Reg::Rax));
            instrs.push(Instr::Call("print_fun_external".to_string()));
            instrs
        }
    }
}

fn compile_prog(prog: &Prog) -> Vec<Instr> {
    let base_input_slot = 16;
    let mut env = HashMap::new();
    env.insert("input".to_string(), base_input_slot);
    let mut max_depth = base_input_slot;
    let mut label_counter = 0;

    let mut ctx = CompileCtx {
        label_counter: &mut label_counter as *mut i32,
        loop_depth: 0,
        current_loop_id: -1,
        max_depth: &mut max_depth as *mut i32,
    };

    let mut instrs = Vec::new();

    instrs.push(Instr::Jmp("main_start".to_string()));

    for defn in &prog.defns {
        // println!("pushing {}", defn.name.clone());
        instrs.push(Instr::Label(defn.name.clone()));
        instrs.extend(compile_defn(defn, &prog.defns, ctx));
        instrs.push(Instr::Ret);
    }

    instrs.push(Instr::Label("main_start".to_string()));



    let body_instrs = compile_expr_with_env_repl(
        &prog.main,
        base_input_slot + 8,
        &env,
        &mut HashMap::new(),
        &prog.defns,
        ctx,
    );

    let frame_size: i32 = ((max_depth + 15) / 16) * 16;

    // Prologue
    instrs.push(Instr::Push(Reg::Rbp));
    instrs.push(Instr::MovReg(Reg::Rbp, Reg::Rsp));
    instrs.push(Instr::Sub(Reg::Rsp, frame_size));

    instrs.push(Instr::MovToStack(Reg::Rdi, base_input_slot));
    instrs.extend(body_instrs);

    // Epilogue
    instrs.push(Instr::MovReg(Reg::Rsp, Reg::Rbp));
    instrs.push(Instr::Pop(Reg::Rbp));

    instrs
}

fn compile_defn(defn: &Defn, defns: &Vec<Defn>, mut ctx: CompileCtx) -> Vec<Instr> {
    let mut current_depth = 16; // makespace for rdi
    let mut max_depth = current_depth; // at least the input slot exists
    let mut env = HashMap::new();
    env.insert("input".to_string(), current_depth);

    //assume args are already allocated
    for arg_name in &defn.params {
        env.insert(arg_name.clone(), -current_depth);
        current_depth += 8;
    }

    // First temp will be at 24 (= 16 + 8)
    let body_instrs = compile_expr_with_env_repl(
        &defn.body,
        current_depth + 8,
        &env,
        &mut HashMap::new(),
        defns,
        ctx,
    );

    // finalize frame size (multiple of 16)
    let frame_size: i32 = ((max_depth + 15) / 16) * 16; //rounds to the mutiple of 16

    // Prologue + initialize input slot, then body, then epilogue
    let mut instrs = Vec::new();
    instrs.push(Instr::Push(Reg::Rbp));
    instrs.push(Instr::MovReg(Reg::Rbp, Reg::Rsp));
    instrs.push(Instr::Sub(Reg::Rsp, frame_size)); // sub rsp, frame_size (16-byte aligned)

    // store arg: input (rdi) at [rbp-16]
    instrs.push(Instr::MovToStack(Reg::Rdi, current_depth));

    instrs.extend(body_instrs);

    // Epilogue
    instrs.push(Instr::MovReg(Reg::Rsp, Reg::Rbp)); // mov rsp, rbp
    instrs.push(Instr::Pop(Reg::Rbp)); // pop rbp
                                       // (RET comes from your jit trampoline / AOT wrapper)

    instrs
}

fn compile_expr(e: &Expr) -> Vec<Instr> {
    let base_input_slot = 16; // makespace for rdi
    let mut env = HashMap::new();
    env.insert("input".to_string(), base_input_slot);

    let mut max_depth = base_input_slot; // at least the input slot exists
    let mut label_counter = 0;

    let mut ctx = CompileCtx {
        label_counter: &mut label_counter as *mut i32,
        loop_depth: 0,
        current_loop_id: -1,
        max_depth: &mut max_depth as *mut i32,
    };

    // First temp will be at 24 (= 16 + 8)
    let body_instrs = compile_expr_with_env_repl(
        e,
        base_input_slot + 8,
        &env,
        &mut HashMap::new(),
        &vec![],
        ctx,
    );

    // finalize frame size (multiple of 16)
    let frame_size: i32 = ((max_depth + 15) / 16) * 16; //rounds to the mutiple of 16

    // Prologue + initialize input slot, then body, then epilogue
    let mut instrs = Vec::new();
    instrs.push(Instr::Push(Reg::Rbp));
    instrs.push(Instr::MovReg(Reg::Rbp, Reg::Rsp));
    instrs.push(Instr::Sub(Reg::Rsp, frame_size)); // sub rsp, frame_size (16-byte aligned)

    // store arg: input (rdi) at [rbp-16]
    instrs.push(Instr::MovToStack(Reg::Rdi, base_input_slot));

    instrs.extend(body_instrs);

    // Epilogue
    instrs.push(Instr::MovReg(Reg::Rsp, Reg::Rbp)); // mov rsp, rbp
    instrs.push(Instr::Pop(Reg::Rbp)); // pop rbp
                                       // (RET comes from your jit trampoline / AOT wrapper)
    instrs
}

fn compile_expr_repl(e: &Expr, replEnv: &mut HashMap<String, Box<i64>>) -> Vec<Instr> {
    let mut max_depth = 0;
    let mut label_counter = 0;
    let ctx = CompileCtx {
        label_counter: &mut label_counter as *mut i32,
        loop_depth: 0,
        current_loop_id: -1,
        max_depth: &mut max_depth as *mut i32,
    };
    // let temp = Vec<Defn>![];
    compile_expr_with_env_repl(e, 16, &HashMap::new(), replEnv, &vec![], ctx)
}

fn jit_code_input(instrs: &Vec<Instr>, input: i64) -> i64 {
    let mut ops: dynasmrt::Assembler<dynasmrt::x64::X64Relocation> =
        dynasmrt::x64::Assembler::new().unwrap();
    let start = ops.offset();
    dynasm!(ops; .arch x64);

    let mut labels: HashMap<String, DynamicLabel> = HashMap::new();
    let error_type_mismatch = ops.new_dynamic_label();
    let error_overflow = ops.new_dynamic_label();
    let error_arithmetic = ops.new_dynamic_label();
    let error_common = ops.new_dynamic_label();
    let print_fun_external = ops.new_dynamic_label();

    labels.insert("type_mismatch_error".to_string(), error_type_mismatch);
    labels.insert("overflow_error".to_string(), error_overflow);
    labels.insert("type_error_arithmetic".to_string(), error_arithmetic);
    labels.insert("print_fun_external".to_string(), print_fun_external);
    let c_func_ptr: extern "C" fn(i64) -> i64 =
        unsafe { mem::transmute(common::snek_error as *const ()) };
    let print_func_ptr: extern "C" fn(i64) -> i64 =
        unsafe { mem::transmute(common::print_fun as *const ()) };

    // Pre-create all labels
    for instr in instrs {
        if let Instr::Label(name) = instr {
            labels.insert(name.clone(), ops.new_dynamic_label());
        }
    }

    for instr in instrs {
        instr_to_asm(instr, &mut ops, &labels);
    }

    dynasm!(ops
        ; jmp ->done
        ; =>error_overflow
        ; mov rdi, 2
        ; jmp =>error_common
        ; =>error_type_mismatch
        ; mov rdi, 1
        ; jmp =>error_common
        ; =>error_arithmetic
        ; mov rdi, 3
        ; =>error_common
        ; mov rax, QWORD c_func_ptr as i64
        ; call rax
        ; ret
        ; =>print_fun_external
        ; sub rsp, 8
        ; mov rax, QWORD print_func_ptr as i64
        ; call rax
        ; add rsp, 8
        ; ret
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

        // Wrap in parentheses to create a program
        in_contents = format!("({})", in_contents);
        let sexpr = parse(&in_contents).unwrap();
        let prog = parse_prog(&sexpr);
        let instrs = compile_prog(&prog);

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
        in_contents = format!("({})", in_contents);
        // parse_prog();
        let sexpr = parse(&in_contents).unwrap();
        let prog = parse_prog(&sexpr);
        let instrs = compile_prog(&prog);
        let result = instrs_to_string(&instrs);

        let asm_program = format!(
            "
section .text
extern snek_error
extern print_fun
global our_code_starts_here
our_code_starts_here:
{}
  jmp done
overflow_error:
  mov rdi, 2
  jmp error_common
type_mismatch_error:
  mov rdi, 1
  jmp error_common
type_error_arithmetic:
  mov rdi, 3
error_common:
  call snek_error
  jmp done
print_fun_external:
  sub rsp, 8 ; alignment for 16 bytes to prevent segfaulting
  call print_fun
  add rsp, 8
  ret
done:
  ret
            ",
            result
        );

        let mut out_file = File::create(out_name)?;
        out_file.write_all(asm_program.as_bytes())?;
    }
    Ok(())
}
