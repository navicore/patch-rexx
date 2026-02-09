use std::process::Command;

fn run_rexx(expr: &str) -> String {
    let output = Command::new(env!("CARGO_BIN_EXE_patch-rexx"))
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
    let output = Command::new(env!("CARGO_BIN_EXE_patch-rexx"))
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

// ── SIGNAL label (basic jump) ──────────────────────────────────────

#[test]
fn signal_basic_jump() {
    assert_eq!(run_rexx("signal skip; say 'NO'; skip: say 'YES'"), "YES");
}

#[test]
fn signal_skips_intermediate_code() {
    assert_eq!(run_rexx("x = 1; signal done; x = 99; done: say x"), "1");
}

#[test]
fn signal_case_insensitive_label() {
    assert_eq!(run_rexx("signal MyLabel; myLabel: say 'OK'"), "OK");
}

// ── SIGNAL VALUE expr (computed jump) ──────────────────────────────

#[test]
fn signal_value_basic() {
    assert_eq!(
        run_rexx("target = 'THERE'; signal value target; here: say 'NO'; there: say 'YES'"),
        "YES"
    );
}

#[test]
fn signal_value_expression() {
    assert_eq!(
        run_rexx("signal value 'LA' || 'BEL'; label: say 'FOUND'"),
        "FOUND"
    );
}

// ── SIGNAL from nested DO (abandons all blocks) ────────────────────

#[test]
fn signal_exits_nested_do() {
    assert_eq!(
        run_rexx("do i = 1 to 10; if i = 3 then signal out; end; out: say 'escaped at' i"),
        "escaped at 3"
    );
}

#[test]
fn signal_exits_deeply_nested() {
    assert_eq!(
        run_rexx("do i = 1 to 5; do j = 1 to 5; signal done; end; end; done: say i j"),
        "1 1"
    );
}

// ── SIGNAL ON NOVALUE ──────────────────────────────────────────────

#[test]
fn signal_on_novalue_fires() {
    assert_eq!(
        run_rexx("signal on novalue; x = unsetvar; exit; novalue: say 'TRAPPED'"),
        "TRAPPED"
    );
}

#[test]
fn signal_on_novalue_with_name() {
    assert_eq!(
        run_rexx("signal on novalue name handler; x = unsetvar; exit; handler: say 'CAUGHT'"),
        "CAUGHT"
    );
}

#[test]
fn signal_off_novalue_disables() {
    // With NOVALUE off, accessing unset variable just returns its name (normal REXX behavior)
    assert_eq!(
        run_rexx("signal on novalue; signal off novalue; say unsetvar"),
        "UNSETVAR"
    );
}

#[test]
fn novalue_condition_info() {
    assert_eq!(
        run_rexx("signal on novalue; x = badvar; exit; novalue: say condition('C')"),
        "NOVALUE"
    );
}

#[test]
fn novalue_condition_description() {
    assert_eq!(
        run_rexx("signal on novalue; x = badvar; exit; novalue: say condition('D')"),
        "BADVAR"
    );
}

// ── SIGNAL ON SYNTAX ───────────────────────────────────────────────

#[test]
fn signal_on_syntax_fires() {
    assert_eq!(
        run_rexx("signal on syntax; x = 1 / 0; exit; syntax: say 'CAUGHT'"),
        "CAUGHT"
    );
}

#[test]
fn signal_on_syntax_sets_rc() {
    // Division by zero is ArithmeticOverflow (Error 42)
    assert_eq!(
        run_rexx("signal on syntax; x = 1 / 0; exit; syntax: say rc"),
        "42"
    );
}

#[test]
fn signal_on_syntax_with_name() {
    assert_eq!(
        run_rexx("signal on syntax name handler; x = 'abc' + 1; exit; handler: say 'GOT IT'"),
        "GOT IT"
    );
}

// ── CONDITION() BIF ────────────────────────────────────────────────

#[test]
fn condition_bif_instruction() {
    assert_eq!(
        run_rexx("signal on novalue; x = unsetvar; exit; novalue: say condition('I')"),
        "SIGNAL"
    );
}

#[test]
fn condition_bif_status() {
    assert_eq!(
        run_rexx("signal on syntax; x = 1 / 0; exit; syntax: say condition('S')"),
        "ON"
    );
}

#[test]
fn condition_bif_no_arg_returns_instruction() {
    // CONDITION() with no args returns 'I' (instruction) per REXX spec
    assert_eq!(
        run_rexx("signal on novalue; x = unsetvar; exit; novalue: say condition()"),
        "SIGNAL"
    );
}

// ── Error cases ────────────────────────────────────────────────────

#[test]
fn signal_nonexistent_label_error_16() {
    let err = run_rexx_fail("signal nonexistent");
    assert!(err.contains("16"), "expected Error 16, got: {err}");
}

#[test]
fn signal_bad_condition_error_25() {
    let err = run_rexx_fail("signal on badcondition");
    assert!(err.contains("25"), "expected Error 25, got: {err}");
}

// ── RC variable accessible after SIGNAL ON SYNTAX ──────────────────

#[test]
fn rc_accessible_after_syntax_trap() {
    // Verify RC is a real variable that can be used in expressions
    assert_eq!(
        run_rexx("signal on syntax; x = 1 / 0; exit; syntax: say rc > 0"),
        "1"
    );
}

// ── SIGNAL ON fires only once (trap auto-disables) ─────────────────

#[test]
fn novalue_trap_fires_once() {
    // After the first NOVALUE trap fires, the trap is auto-disabled.
    // A second unset variable access should NOT trap — it returns the name as usual.
    assert_eq!(
        run_rexx("signal on novalue; x = bad1; exit; novalue: say condition('D'); say bad2"),
        "BAD1\nBAD2"
    );
}
