// Common shared code between runtime and compiler
// This module has NO external dependencies

pub const TRUE_TAGGED: i64 = 3;
pub const FALSE_TAGGED: i64 = 1;
pub const FLASE_TAGGED: i64 = 1; // Kept for backwards compatibility
pub const BOOL_TAG: i64 = 1;
pub const NUM_TAG: i64 = 0;

pub fn get_tag(n: i64) -> i64 {
    n & 1
}

pub fn tag_number(n: i64) -> i64 {
    n << 1
}

pub fn untag_number(n: i64) -> i64 {
    n >> 1
}

pub fn format_result(res: i64) -> String {
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

pub fn parse_input(input: &str) -> i64 {
    let trimmed = input.trim();
    let res = trimmed.parse::<i64>();
    if let Ok(n) = res {
        return tag_number(n);
    }
    match trimmed {
        "true" => TRUE_TAGGED,
        "false" => FALSE_TAGGED,
        _ => FALSE_TAGGED,
    }
}
