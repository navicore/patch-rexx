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

// ── Basic execution ─────────────────────────────────────────────────

#[test]
fn interpret_simple_say() {
    assert_eq!(run_rexx("interpret \"say 'hello'\""), "hello");
}

#[test]
fn interpret_expression() {
    assert_eq!(run_rexx("interpret 'say 2 + 3'"), "5");
}

#[test]
fn interpret_empty_string() {
    // INTERPRET "" is a no-op — no output
    assert_eq!(run_rexx("interpret ''"), "");
}

#[test]
fn interpret_multiple_clauses() {
    assert_eq!(run_rexx("interpret 'x = 1; y = 2; say x + y'"), "3");
}

// ── Variable sharing (caller scope) ────────────────────────────────

#[test]
fn interpret_reads_caller_variable() {
    assert_eq!(run_rexx("x = 10; interpret 'say x'"), "10");
}

#[test]
fn interpret_modifies_caller_variable() {
    assert_eq!(run_rexx("x = 1; interpret 'x = 2'; say x"), "2");
}

#[test]
fn interpret_new_variable_visible_after() {
    assert_eq!(run_rexx("interpret 'y = 99'; say y"), "99");
}

// ── Dynamic code construction ──────────────────────────────────────

#[test]
fn interpret_dynamic_instruction() {
    assert_eq!(
        run_rexx("cmd = \"say 'dynamic'\"; interpret cmd"),
        "dynamic"
    );
}

#[test]
fn interpret_constructed_expression() {
    assert_eq!(run_rexx("op = '+'; interpret 'say 3' op '4'"), "7");
}

// ── Control flow inside INTERPRET ──────────────────────────────────

#[test]
fn interpret_if_then_else() {
    assert_eq!(
        run_rexx("interpret 'if 1 then say \"yes\"; else say \"no\"'"),
        "yes"
    );
}

#[test]
fn interpret_do_loop() {
    assert_eq!(
        run_rexx("interpret 'r = \"\"; do i = 1 to 3; r = r || i; end; say r'"),
        "123"
    );
}

#[test]
fn interpret_select() {
    assert_eq!(
        run_rexx(
            "interpret 'x = 2; select; when x = 1 then say \"one\"; when x = 2 then say \"two\"; otherwise say \"other\"; end'"
        ),
        "two"
    );
}

// ── Labels and SIGNAL inside INTERPRET ─────────────────────────────

#[test]
fn interpret_local_signal() {
    assert_eq!(
        run_rexx("interpret 'signal skip; say \"NO\"; skip: say \"YES\"'"),
        "YES"
    );
}

#[test]
fn interpret_signal_propagates_out() {
    // SIGNAL to a label that only exists in the outer program
    assert_eq!(
        run_rexx("interpret 'signal outer'; say 'NO'; outer: say 'OUTER'"),
        "OUTER"
    );
}

#[test]
fn interpret_local_label_priority() {
    // Local label inside INTERPRET wins over outer label with same name.
    // Use exit inside INTERPRET so the outer label isn't reached.
    assert_eq!(
        run_rexx("interpret 'signal target; target: say \"INNER\"; exit'"),
        "INNER"
    );
}

// ── EXIT / RETURN / LEAVE / ITERATE propagation ────────────────────

#[test]
fn interpret_exit_propagates() {
    // EXIT inside INTERPRET terminates the program
    assert_eq!(run_rexx("interpret 'exit'; say 'NEVER'"), "");
}

#[test]
fn interpret_leave_propagates() {
    // LEAVE inside INTERPRET exits enclosing DO
    assert_eq!(
        run_rexx("r = ''; do i = 1 to 5; if i = 3 then interpret 'leave'; r = r || i; end; say r"),
        "12"
    );
}

#[test]
fn interpret_iterate_propagates() {
    // ITERATE inside INTERPRET continues enclosing DO
    assert_eq!(
        run_rexx(
            "r = ''; do i = 1 to 5; if i = 3 then interpret 'iterate'; r = r || i; end; say r"
        ),
        "1245"
    );
}

// ── Recursive INTERPRET ────────────────────────────────────────────

#[test]
fn interpret_nested() {
    // INTERPRET within INTERPRET
    assert_eq!(run_rexx("interpret 'interpret \"say 42\"'"), "42");
}

#[test]
fn interpret_depth_limit() {
    // Recursive INTERPRET that actually executes to depth limit (Error 5)
    let stderr = run_rexx_fail("code = 'interpret code'; interpret code");
    assert!(
        stderr.contains("Error 5"),
        "expected Error 5 (ResourceExhausted), got: {stderr}"
    );
}

// ── Error handling ─────────────────────────────────────────────────

#[test]
fn interpret_parse_error_fires_syntax_trap() {
    // Bad syntax inside INTERPRET fires SYNTAX trap
    assert_eq!(
        run_rexx("signal on syntax; interpret 'do'; say 'NO'; syntax: say 'TRAPPED'"),
        "TRAPPED"
    );
}

#[test]
fn interpret_runtime_error_fires_syntax_trap() {
    // Division by zero inside INTERPRET fires SYNTAX trap
    assert_eq!(
        run_rexx("signal on syntax; interpret 'say 1 / 0'; say 'NO'; syntax: say 'TRAPPED'"),
        "TRAPPED"
    );
}

#[test]
fn interpret_parse_error_no_trap() {
    // Bad syntax without trap → error exit
    let stderr = run_rexx_fail("interpret 'do'");
    assert!(stderr.contains("Error"), "expected an error, got: {stderr}");
}

// ── Trap interaction ──────────────────────────────────────────────

#[test]
fn interpret_inherits_traps() {
    // NOVALUE trap set before INTERPRET fires inside it.
    // Use a non-SAY context so NOVALUE fires before output.
    assert_eq!(
        run_rexx("signal on novalue; interpret 'x = unsetvar + 1'; novalue: say 'TRAPPED'"),
        "TRAPPED"
    );
}

#[test]
fn interpret_can_set_traps() {
    // Traps set inside INTERPRET persist after.
    // Use a non-SAY context so NOVALUE fires before output.
    assert_eq!(
        run_rexx("interpret 'signal on novalue'; x = unsetvar + 1; novalue: say 'TRAPPED'"),
        "TRAPPED"
    );
}

// ── Named DO loop interaction ──────────────────────────────────────

#[test]
fn interpret_leave_named_do() {
    // LEAVE targeting a named DO from inside INTERPRET
    assert_eq!(
        run_rexx(
            "r = ''; do i = 1 to 5; if i = 3 then interpret 'leave i'; r = r || i; end; say r"
        ),
        "12"
    );
}

// ── Edge cases ─────────────────────────────────────────────────────

#[test]
fn interpret_bare_keyword() {
    // INTERPRET with no expression uses empty string (no-op)
    assert_eq!(run_rexx("interpret; say 'ok'"), "ok");
}

#[test]
fn interpret_signal_to_trailing_label() {
    // Label at end of interpreted code — signal lands past last clause, completes normally
    assert_eq!(
        run_rexx("interpret 'signal done; done:'; say 'after'"),
        "after"
    );
}

#[test]
fn interpret_assignment_only() {
    // INTERPRET that only does assignments, no output
    assert_eq!(run_rexx("interpret 'a = 1; b = 2'; say a + b"), "3");
}
