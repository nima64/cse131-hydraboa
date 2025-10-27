// Library module for shared code between runtime and compiler

pub mod common;
pub mod types;
pub mod assembly;

// Re-export commonly used items from common (no external dependencies)
pub use common::{
    format_result, parse_input, tag_number, untag_number, get_tag,
    TRUE_TAGGED, FALSE_TAGGED, FLASE_TAGGED, BOOL_TAG, NUM_TAG,
};

// Re-export compiler-specific types
pub use types::{
    Reg, Op1, Op2, Expr, Instr, CompileCtx,
    is_number_tag, is_bool_tag,
};
