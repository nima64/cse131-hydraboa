use std::env;

pub const TRUE_TAGGED:i64 = 3;
pub const FALSE_TAGGED:i64 = 1;
pub const BOOL_TAG: i64 = 1;
pub const NUM_TAG: i64 = 0;

pub fn tag_number(n:i64) -> i64{
    return n << 1;
}
pub fn untag_number(n:i64) -> i64{
    return n >> 1;
}

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
pub fn get_tag(n: i64) -> i64{
    n & 1
}

fn format_result(res: i64) -> String{
    let tag = get_tag(res);
    if tag == BOOL_TAG {
        if res == TRUE_TAGGED {
            return "true".to_string();
        }else{
            return "false".to_string();
        }
    }
    return untag_number(res).to_string();
}

fn parse_input(input: &str) -> i64 {
  let trimmed = input.trim();
  let res =trimmed.parse::<i64>(); 
  if let Ok(n) = res {
    return tag_number(n);
  }
  match trimmed {
    "true" => TRUE_TAGGED as i64,
    "false" => FALSE_TAGGED as i64,
    _=> {
      // println!("not a valid input {}", _);
      FALSE_TAGGED as i64
    }
  } 
}

fn main() {
  let args: Vec<String> = env::args().collect();
  let input = if args.len() == 2 { &args[1]} else { "false"};
  let input = parse_input(&input);

  let i: i64 = unsafe { our_code_starts_here(input) };
  let res = format_result(i);

  println!("{res}");
}
