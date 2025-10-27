use std::env;

// Import the common module directly from src (no external dependencies)
#[path = "../src/common.rs"]
mod common;

use common::*;

#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM (which Rust uses) that ensures
    // it does not add an underscore in front of the name, which happens on OSX
    // Courtesy of Max New
    // (https://maxsnew.com/teaching/eecs-483-fa22/hw_adder_assignment.html)
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here(input: i64) -> i64;
}

#[export_name = "\x01snek_error"]
pub extern "C" fn snek_error(errorcode:i64){
  eprintln!("an error occured {errorcode}");
  std::process::exit(1);
}

fn main() {
  let args: Vec<String> = env::args().collect();
  let input = if args.len() == 2 { &args[1]} else { "false"};
  let input = parse_input(&input);

  let i: i64 = unsafe { our_code_starts_here(input) };
  let res = format_result(i);

  println!("{res}");
}
