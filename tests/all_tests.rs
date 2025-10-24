mod infra;

// Your tests go here!
success_tests! {
    test_input: { file: "input", input: "2", expected: "2" },
    basic_loop: { file: "basic_loop", input: "", expected: "6" },
    cobra_example_2: { file: "cobra_example_2", input: "", expected: "-6" },

}

// runtime_error_tests! {
//     test_overflow_error: { file: "overflow", expected: "overflow" },
// }

// static_error_tests! {
//     test_parse_error: { file: "parse", input: "2", expected: "Invalid" },
//     test_loop_break_error: { file: "loop_break", input: "", expected: "break outside of loop" },
// }


// repl_tests! {
//     test_simple_bools: ["(define x true)", "x", "false"] => ["true", "false"],
// }
