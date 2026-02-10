use std::process::Command;

fn run_rexx(expr: &str) -> String {
    let output = Command::new(env!("CARGO_BIN_EXE_rexx"))
        .args(["-e", expr])
        .output()
        .expect("failed to run patch-rexx");
    assert!(
        output.status.success(),
        "patch-rexx exited with error for input '{expr}': {}",
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8(output.stdout)
        .expect("non-utf8 output")
        .trim()
        .to_string()
}

fn run_rexx_fail(expr: &str) -> String {
    let output = Command::new(env!("CARGO_BIN_EXE_rexx"))
        .args(["-e", expr])
        .output()
        .expect("failed to run patch-rexx");
    assert!(
        !output.status.success(),
        "expected patch-rexx to fail for input '{expr}'"
    );
    String::from_utf8(output.stderr)
        .expect("non-utf8 output")
        .trim()
        .to_string()
}

// ── IF / THEN / ELSE ────────────────────────────────────────────

#[test]
fn if_then_true() {
    assert_eq!(run_rexx("say 2 + 3; if 1 then say 'yes'"), "5\nyes");
}

#[test]
fn if_then_false() {
    assert_eq!(run_rexx("if 0 then say 'no'"), "");
}

#[test]
fn if_then_else() {
    assert_eq!(run_rexx("if 0 then say 'no'; else say 'yes'"), "yes");
}

#[test]
fn if_comparison() {
    assert_eq!(run_rexx("if 3 > 2 then say 'bigger'"), "bigger");
}

// ── DO simple block ─────────────────────────────────────────────

#[test]
fn do_simple_block() {
    assert_eq!(run_rexx("do; say 'a'; say 'b'; end"), "a\nb");
}

// ── DO count ────────────────────────────────────────────────────

#[test]
fn do_count() {
    assert_eq!(run_rexx("x = 0; do 3; x = x + 1; end; say x"), "3");
}

// ── DO FOREVER + LEAVE ──────────────────────────────────────────

#[test]
fn do_forever_leave() {
    assert_eq!(
        run_rexx("x = 0; do forever; x = x + 1; if x = 3 then leave; end; say x"),
        "3"
    );
}

// ── DO WHILE ────────────────────────────────────────────────────

#[test]
fn do_while() {
    assert_eq!(
        run_rexx("x = 1; do while x < 5; x = x + 1; end; say x"),
        "5"
    );
}

// ── DO UNTIL ────────────────────────────────────────────────────

#[test]
fn do_until() {
    assert_eq!(
        run_rexx("x = 1; do until x >= 5; x = x + 1; end; say x"),
        "5"
    );
}

// ── Controlled DO ───────────────────────────────────────────────

#[test]
fn controlled_do_basic() {
    // REXX: after loop, var is one past the limit
    assert_eq!(run_rexx("do i = 1 to 5; end; say i"), "6");
}

#[test]
fn controlled_do_by() {
    assert_eq!(run_rexx("do i = 0 to 10 by 3; end; say i"), "12");
}

#[test]
fn controlled_do_for() {
    assert_eq!(
        run_rexx("s = ''; do i = 1 to 100 for 3; s = s || i || ' '; end; say s"),
        "1 2 3"
    );
}

#[test]
fn controlled_do_print_values() {
    assert_eq!(
        run_rexx("s = ''; do i = 1 to 3; s = s || i; end; say s"),
        "123"
    );
}

// ── ITERATE ─────────────────────────────────────────────────────

#[test]
fn iterate_in_loop() {
    assert_eq!(
        run_rexx("s = ''; do i = 1 to 5; if i = 3 then iterate; s = s || i; end; say s"),
        "1245"
    );
}

// ── SELECT ──────────────────────────────────────────────────────

#[test]
fn select_when() {
    assert_eq!(
        run_rexx(
            "x = 2; select; when x = 1 then say 'one'; when x = 2 then say 'two'; otherwise say 'other'; end"
        ),
        "two"
    );
}

#[test]
fn select_otherwise() {
    assert_eq!(
        run_rexx(
            "x = 9; select; when x = 1 then say 'one'; when x = 2 then say 'two'; otherwise say 'other'; end"
        ),
        "other"
    );
}

#[test]
fn select_no_match_no_otherwise() {
    let stderr =
        run_rexx_fail("x = 9; select; when x = 1 then say 'one'; when x = 2 then say 'two'; end");
    assert!(stderr.contains("Error 7"));
}

// ── Nested DO ───────────────────────────────────────────────────

#[test]
fn nested_do() {
    assert_eq!(
        run_rexx("s = ''; do i = 1 to 3; do j = 1 to 2; s = s || i || j || ' '; end; end; say s"),
        "11 12 21 22 31 32"
    );
}

// ── FizzBuzz ────────────────────────────────────────────────────

#[test]
fn fizzbuzz() {
    let expected = "1\n2\nFizz\n4\nBuzz\nFizz\n7\n8\nFizz\nBuzz\n11\nFizz\n13\n14\nFizzBuzz";
    assert_eq!(
        run_rexx(
            "do i = 1 to 15; select; when i // 15 = 0 then say 'FizzBuzz'; when i // 3 = 0 then say 'Fizz'; when i // 5 = 0 then say 'Buzz'; otherwise say i; end; end"
        ),
        expected
    );
}

// ── Factorial ───────────────────────────────────────────────────

#[test]
fn factorial() {
    assert_eq!(
        run_rexx("n = 10; f = 1; do i = 1 to n; f = f * i; end; say f"),
        "3628800"
    );
}

// ── EXIT ────────────────────────────────────────────────────────

#[test]
fn exit_stops_execution() {
    assert_eq!(run_rexx("say 'before'; exit; say 'after'"), "before");
}

// ── Error cases ─────────────────────────────────────────────────

#[test]
fn stray_end_error() {
    let stderr = run_rexx_fail("end");
    assert!(stderr.contains("Error 10"));
}

#[test]
fn stray_then_error() {
    let stderr = run_rexx_fail("then");
    assert!(stderr.contains("Error 8"));
}

#[test]
fn leave_outside_loop() {
    let stderr = run_rexx_fail("leave");
    assert!(stderr.contains("Error 28"));
}

// ── IF across clause boundaries ─────────────────────────────────

#[test]
fn if_then_across_lines() {
    // THEN can be on a separate line from IF
    assert_eq!(run_rexx("if 1\nthen say 'yes'"), "yes");
}

#[test]
fn if_else_across_lines() {
    assert_eq!(run_rexx("if 0\nthen say 'no'\nelse say 'yes'"), "yes");
}

// ── Edge cases from PR review ─────────────────────────────────

#[test]
fn zero_by_step_error() {
    let stderr = run_rexx_fail("do i = 1 to 10 by 0; say i; end");
    assert!(stderr.contains("BY value"));
}

#[test]
fn negative_by_step() {
    assert_eq!(
        run_rexx("s = ''; do i = 5 to 1 by -1; s = s || i; end; say s"),
        "54321"
    );
}

#[test]
fn large_exponent_error() {
    let stderr = run_rexx_fail("say 2 ** 9999999999");
    assert!(stderr.contains("exponent"));
}

#[test]
fn negative_loop_count_error() {
    let stderr = run_rexx_fail("do -3; say 'x'; end");
    assert!(stderr.contains("negative"));
}

#[test]
fn fractional_loop_count_error() {
    let stderr = run_rexx_fail("do 3.7; say 'x'; end");
    assert!(stderr.contains("whole number"));
}

#[test]
fn duplicate_to_keyword_error() {
    let stderr = run_rexx_fail("do i = 1 to 5 to 10; say i; end");
    assert!(stderr.contains("Error 27"));
}

#[test]
fn duplicate_by_keyword_error() {
    let stderr = run_rexx_fail("do i = 1 to 10 by 2 by 3; say i; end");
    assert!(stderr.contains("Error 27"));
}

// ── ITERATE in DO UNTIL ───────────────────────────────────────
// Per ANSI REXX, ITERATE skips to the UNTIL check (not back to
// the top). If UNTIL is false, the body runs again.

#[test]
fn iterate_in_do_until() {
    // ITERATE on i=2 skips SAY but UNTIL (2>3 = false) continues the loop.
    assert_eq!(
        run_rexx(
            "s = ''; i = 0; do until i > 3; i = i + 1; if i = 2 then iterate; s = s || i || ' '; end; say s"
        ),
        "1 3 4"
    );
}

// ── Controlled DO with UNTIL — final variable value ───────────
// When UNTIL is satisfied the loop exits WITHOUT incrementing
// the control variable.

#[test]
fn controlled_do_until_final_value() {
    // i steps through 1, 3, 5, 7. UNTIL (i>5) first true at i=7.
    // Loop exits without incrementing, so final i = 7 (not 9).
    assert_eq!(
        run_rexx(
            "s = ''; do i = 1 to 10 by 2 until i > 5; s = s || i || ' '; end; say s || 'final=' || i"
        ),
        "1 3 5 7 final=7"
    );
}

// ── Controlled DO with TO — final variable is one-past-limit ──

#[test]
fn controlled_do_to_final_value() {
    // i steps 1,3,5. Next would be 7 which exceeds TO 5, so final i=7.
    assert_eq!(run_rexx("do i = 1 to 5 by 2; end; say i"), "7");
}
