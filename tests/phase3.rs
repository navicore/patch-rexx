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

// ── Basic subroutine ───────────────────────────────────────────

#[test]
fn call_basic_subroutine() {
    assert_eq!(run_rexx("call hello; exit; hello: say 'hi'; return"), "hi");
}

// ── Call with return value (RESULT variable) ───────────────────

#[test]
fn call_with_result() {
    assert_eq!(
        run_rexx("call double 5; say result; exit; double: arg n; return n * 2"),
        "10"
    );
}

// ── Function call in expression ────────────────────────────────

#[test]
fn function_call_in_expression() {
    assert_eq!(
        run_rexx("say double(5); exit; double: arg n; return n * 2"),
        "10"
    );
}

// ── Multiple arguments ─────────────────────────────────────────

#[test]
fn multiple_arguments() {
    assert_eq!(
        run_rexx("say add(3, 4); exit; add: arg a, b; return a + b"),
        "7"
    );
}

// ── PROCEDURE isolation ────────────────────────────────────────

#[test]
fn procedure_isolation() {
    assert_eq!(
        run_rexx("x = 'outer'; call inner; say x; exit; inner: procedure; x = 'inner'; return"),
        "outer"
    );
}

// ── PROCEDURE EXPOSE ───────────────────────────────────────────

#[test]
fn procedure_expose() {
    assert_eq!(
        run_rexx(
            "x = 'outer'; call inner; say x; exit; inner: procedure expose x; x = 'changed'; return"
        ),
        "changed"
    );
}

// ── Recursive factorial ────────────────────────────────────────

#[test]
fn recursive_factorial() {
    assert_eq!(
        run_rexx(
            "say fact(10); exit; fact: procedure; arg n; if n <= 1 then return 1; return n * fact(n - 1)"
        ),
        "3628800"
    );
}

// ── Recursive Fibonacci ────────────────────────────────────────

#[test]
fn recursive_fibonacci() {
    assert_eq!(
        run_rexx(
            "say fib(10); exit; fib: procedure; arg n; if n <= 1 then return n; return fib(n-1) + fib(n-2)"
        ),
        "55"
    );
}

// ── Nested calls ───────────────────────────────────────────────

#[test]
fn nested_calls() {
    assert_eq!(
        run_rexx(
            "say outer(2); exit; outer: procedure; arg n; return inner(n) + 1; inner: procedure; arg n; return n * 10"
        ),
        "21"
    );
}

// ── RESULT dropped when no return value ────────────────────────

#[test]
fn result_dropped_when_no_return_value() {
    assert_eq!(
        run_rexx("result = 'old'; call noval; say result; exit; noval: return"),
        "RESULT"
    );
}

// ── ARG uppercases ─────────────────────────────────────────────

#[test]
fn arg_uppercases() {
    assert_eq!(
        run_rexx("call greet 'hello'; exit; greet: arg msg; say msg; return"),
        "HELLO"
    );
}

// ── Call with no args ──────────────────────────────────────────

#[test]
fn call_no_args() {
    assert_eq!(
        run_rexx("call greet; exit; greet: say 'hello'; return"),
        "hello"
    );
}

// ── EXIT prevents fall-through ─────────────────────────────────

#[test]
fn exit_prevents_fall_through() {
    assert_eq!(run_rexx("say 'main'; exit; sub: say 'sub'; return"), "main");
}

// ── DROP instruction ───────────────────────────────────────────

#[test]
fn drop_instruction() {
    assert_eq!(run_rexx("x = 5; drop x; say x"), "X");
}

// ── Error: function without return value → Error 44 ────────────

#[test]
fn error_function_no_return_data() {
    let stderr = run_rexx_fail("say noval(); exit; noval: return");
    assert!(
        stderr.contains("Error 44"),
        "expected Error 44, got: {stderr}"
    );
}

// ── Error: routine not found → Error 43 ────────────────────────

#[test]
fn error_routine_not_found() {
    let stderr = run_rexx_fail("call nonexistent");
    assert!(
        stderr.contains("Error 43"),
        "expected Error 43, got: {stderr}"
    );
}

// ── Error: misplaced PROCEDURE → Error 17 ──────────────────────

#[test]
fn error_misplaced_procedure() {
    let stderr = run_rexx_fail("procedure");
    assert!(
        stderr.contains("Error 17"),
        "expected Error 17, got: {stderr}"
    );
}
