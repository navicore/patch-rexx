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

fn run_rexx_stderr(expr: &str) -> (String, String) {
    let output = Command::new(env!("CARGO_BIN_EXE_patch-rexx"))
        .args(["-e", expr])
        .output()
        .expect("failed to run patch-rexx");
    assert!(
        output.status.success(),
        "patch-rexx exited with error for input '{expr}': {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8(output.stdout)
        .expect("non-utf8 stdout")
        .trim()
        .to_string();
    let stderr = String::from_utf8(output.stderr)
        .expect("non-utf8 stderr")
        .trim()
        .to_string();
    (stdout, stderr)
}

fn run_rexx_expect_error(expr: &str) -> String {
    let output = Command::new(env!("CARGO_BIN_EXE_patch-rexx"))
        .args(["-e", expr])
        .output()
        .expect("failed to run patch-rexx");
    assert!(
        !output.status.success(),
        "expected error for input '{expr}' but got success"
    );
    String::from_utf8(output.stderr)
        .expect("non-utf8 stderr")
        .trim()
        .to_string()
}

// ── TRACE instruction ─────────────────────────────────────────────────

#[test]
fn trace_off_no_output() {
    let (_stdout, stderr) = run_rexx_stderr("trace off; say 'hello'");
    assert!(
        stderr.is_empty(),
        "trace off should produce no stderr, got: {stderr}"
    );
}

#[test]
fn trace_results_shows_result() {
    let (stdout, stderr) = run_rexx_stderr("trace r; say 1 + 2");
    assert_eq!(stdout, "3");
    assert!(
        stderr.contains(">>>"),
        "trace r should show >>> on stderr, got: {stderr}"
    );
    assert!(
        stderr.contains("\"3\""),
        "trace r should show result value, got: {stderr}"
    );
}

#[test]
fn trace_results_shows_source_line() {
    let (_stdout, stderr) = run_rexx_stderr("trace r; say 'hello'");
    assert!(
        stderr.contains("*-*"),
        "trace r should show *-* source line, got: {stderr}"
    );
}

#[test]
fn trace_intermediates_shows_tags() {
    let (_stdout, stderr) = run_rexx_stderr("trace i; say 1 + 2");
    // Should show >L> for literals and >O> for operator result
    assert!(
        stderr.contains(">L>"),
        "trace i should show >L> for literals, got: {stderr}"
    );
    assert!(
        stderr.contains(">O>"),
        "trace i should show >O> for operators, got: {stderr}"
    );
}

#[test]
fn trace_commands_shows_command() {
    let (_stdout, stderr) = run_rexx_stderr("trace c; 'echo hi'");
    assert!(
        stderr.contains("*-*"),
        "trace c should show *-* for commands, got: {stderr}"
    );
}

// ── TRACE setting parsing ─────────────────────────────────────────────

#[test]
fn trace_single_letter() {
    // 'r' is same as 'results'
    let (stdout, stderr) = run_rexx_stderr("trace r; say 'hi'");
    assert_eq!(stdout, "hi");
    assert!(
        stderr.contains(">>>"),
        "trace r should produce trace output"
    );
}

#[test]
fn trace_case_insensitive() {
    let (_stdout, stderr) = run_rexx_stderr("trace RESULTS; say 42");
    assert!(
        stderr.contains(">>>"),
        "trace RESULTS should produce trace output, got: {stderr}"
    );
}

#[test]
fn trace_question_toggle() {
    // TRACE ?R enables interactive mode; TRACE() BIF should return "?R"
    assert_eq!(run_rexx("trace ?r; say trace()"), "?R");
}

#[test]
fn trace_invalid_errors() {
    let stderr = run_rexx_expect_error("trace xyz");
    assert!(
        stderr.contains("24"),
        "invalid trace should raise Error 24, got: {stderr}"
    );
}

// ── TRACE() BIF ───────────────────────────────────────────────────────

#[test]
fn trace_bif_returns_current() {
    assert_eq!(run_rexx("say trace()"), "N");
}

#[test]
fn trace_bif_default_is_normal() {
    assert_eq!(run_rexx("say trace()"), "N");
}

#[test]
fn trace_bif_sets_new() {
    // TRACE('R') sets to R and returns old setting 'N'
    assert_eq!(run_rexx("x = trace('R'); say x"), "N");
}

#[test]
fn trace_bif_roundtrip() {
    // Set to R, then query — should return R
    let result = run_rexx("call trace 'R'; say trace()");
    assert_eq!(result, "R");
}

#[test]
fn trace_bif_interactive_flag() {
    // Set to ?R, query returns ?R
    assert_eq!(run_rexx("x = trace('?R'); say trace()"), "?R");
}

#[test]
fn trace_bif_returns_previous() {
    // Set to R, then set to I — should return R
    assert_eq!(run_rexx("call trace 'R'; x = trace('I'); say x"), "R");
}

#[test]
fn trace_bif_toggle_interactive() {
    // trace('?') toggles interactive on; second toggle turns it off
    assert_eq!(run_rexx("call trace '?'; say trace()"), "?N");
}

// ── Trace output format ──────────────────────────────────────────────

#[test]
fn trace_source_line_format() {
    let (_stdout, stderr) = run_rexx_stderr("trace r; say 'hello'");
    // Should have line number and *-* prefix
    assert!(stderr.contains("*-*"), "should contain *-* prefix");
    // Line number should be right-justified
    let lines: Vec<&str> = stderr.lines().collect();
    let source_line = lines.iter().find(|l| l.contains("*-*"));
    assert!(source_line.is_some(), "should have a *-* line");
}

#[test]
fn trace_result_format() {
    let (_stdout, stderr) = run_rexx_stderr("trace r; say 'hello'");
    // Result should be in quotes: >>> "hello"
    assert!(stderr.contains(">>>"), "should contain >>> tag");
    assert!(
        stderr.contains("\"hello\""),
        "result should be in quotes, got: {stderr}"
    );
}

#[test]
fn trace_variable_tag() {
    let (_stdout, stderr) = run_rexx_stderr("trace i; x = 42; say x");
    assert!(
        stderr.contains(">V>"),
        "should show >V> for variable lookup, got: {stderr}"
    );
}

#[test]
fn trace_operator_tag() {
    let (_stdout, stderr) = run_rexx_stderr("trace i; say 2 + 3");
    assert!(
        stderr.contains(">O>"),
        "should show >O> for operator result, got: {stderr}"
    );
}

#[test]
fn trace_literal_tag() {
    let (_stdout, stderr) = run_rexx_stderr("trace i; say 'hello'");
    assert!(
        stderr.contains(">L>"),
        "should show >L> for string literal, got: {stderr}"
    );
}

#[test]
fn trace_unary_tag() {
    let (_stdout, stderr) = run_rexx_stderr("trace i; say -5");
    assert!(
        stderr.contains(">P>"),
        "should show >P> for unary prefix, got: {stderr}"
    );
}

#[test]
fn trace_function_tag() {
    let (_stdout, stderr) = run_rexx_stderr("trace i; say length('hello')");
    assert!(
        stderr.contains(">F>"),
        "should show >F> for function result, got: {stderr}"
    );
}

// ── Integration tests ────────────────────────────────────────────────

#[test]
fn trace_in_do_loop() {
    let (stdout, stderr) = run_rexx_stderr("trace r; do i = 1 to 3; say i; end");
    assert_eq!(stdout, "1\n2\n3");
    // Should have trace output for each iteration
    assert!(stderr.contains(">>>"), "should trace results in loop");
}

#[test]
fn trace_with_subroutine() {
    let (stdout, stderr) =
        run_rexx_stderr("trace r; call myFunc; say result; exit; myFunc: return 42");
    assert_eq!(stdout, "42");
    assert!(
        stderr.contains("*-*"),
        "should trace across subroutine calls"
    );
}

#[test]
fn trace_with_interpret() {
    let (stdout, stderr) = run_rexx_stderr("trace r; interpret \"say 'hi'\"");
    assert_eq!(stdout, "hi");
    assert!(
        stderr.contains("*-*"),
        "should have trace output with interpret"
    );
}

#[test]
fn trace_preserves_execution() {
    // Trace should not change program output
    let without_trace = run_rexx("x = 2 + 3; say x");
    let (with_trace, _stderr) = run_rexx_stderr("trace r; x = 2 + 3; say x");
    assert_eq!(
        without_trace, with_trace,
        "trace should not change stdout output"
    );
}

#[test]
fn trace_all_traces_labels() {
    let (_stdout, stderr) = run_rexx_stderr("trace a; signal myLabel; myLabel:; say 'found'");
    assert!(
        stderr.contains("*-*"),
        "trace all should trace labels, got: {stderr}"
    );
}

#[test]
fn trace_labels_level() {
    // At Labels level, labels should be traced but regular clauses should not
    // (Labels level is below Results)
    let (_stdout, stderr) = run_rexx_stderr("trace l; signal myLabel; myLabel:; say 'found'");
    // Should trace the label
    assert!(
        stderr.contains("*-*"),
        "trace l should trace label lines, got: {stderr}"
    );
}

#[test]
fn trace_off_suppresses_all() {
    let (_stdout, stderr) = run_rexx_stderr("trace r; trace off; say 'hello'");
    // After trace off, the say should NOT be traced
    // But the trace r / trace off statements themselves may have been traced
    // The important thing: "hello" result should not appear in trace
    let trace_after_off: Vec<&str> = stderr.lines().filter(|l| l.contains("\"hello\"")).collect();
    assert!(
        trace_after_off.is_empty(),
        "trace off should suppress tracing, got: {stderr}"
    );
}

#[test]
fn trace_instruction_change_midstream() {
    // Start with off, switch to results mid-program
    let (stdout, stderr) = run_rexx_stderr("trace off; say 'a'; trace r; say 'b'");
    assert_eq!(stdout, "a\nb");
    // Only 'b' should have trace output
    assert!(
        !stderr.contains("\"a\""),
        "should not trace before trace r was set"
    );
    assert!(
        stderr.contains("\"b\""),
        "should trace after trace r was set, got: {stderr}"
    );
}

#[test]
fn trace_compound_via_symbol() {
    // Compound variables like a.1 are accessed via Expr::Symbol in expression position,
    // so they show >V> tag (variable lookup) at intermediates level.
    let (_stdout, stderr) = run_rexx_stderr("trace i; a.1 = 'hi'; say a.1");
    assert!(
        stderr.contains(">V>"),
        "compound via symbol should show >V>, got: {stderr}"
    );
}
