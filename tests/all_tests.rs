mod infra;

// Your tests go here!
success_tests! {
    // test_input: { file: "input", input: "2", expected: "2" },
    nested_loop: { file: "nested_loop", input: "", expected: "-6" },
    binop_plus: { file: "plus", input: "", expected: "8" },
    binop_minus: { file: "minus", input: "", expected: "6" },
    binop_times: { file: "times", input: "", expected: "42" },
    binop_less: { file: "less", input: "", expected: "true" },
    binop_greater: { file: "greater", input: "", expected: "true" },
    binop_less_equal: { file: "less_equal", input: "", expected: "true" },
    binop_greater_equal: { file: "greater_equal", input: "", expected: "true" },
    binop_equal: { file: "equal", input: "", expected: "true" },
    unop_add1: { file: "add1", input: "", expected: "4" },
    unop_sub1: { file: "sub1", input: "", expected: "-4" },
    fun_fact: {file:"fact", input: "9", expected: "362880"},
    fun_iseven: {file:"fun_iseven", input: "99", expected: "99
false
false"}

}

runtime_error_tests! {
    integer_overflow_add1: { file: "integer_overflow_add1", input: "4611686018427387903", expected: "integer overflow" },
    integer_overflow_sub1: { file: "integer_overflow_sub1", input: "-4611686018427387904", expected: "integer overflow" },
    integer_overflow_mul: { file: "integer_overflow_mul", input: "2", expected: "integer overflow" },
    integer_overflow_mul_negative: { file: "integer_overflow_mul_negative", input: "2", expected: "integer overflow" },
}

static_error_tests! {
    // test_parse_error: { file: "parse", input: "2", expected: "Invalid" },
    test_loop_break_error: { file: "loop_break", input: "", expected: "break outside of loop" },
}


repl_tests! {
    test_simple_bools: ["(define x true)", "x", "false"] => ["true", "false"],
}
