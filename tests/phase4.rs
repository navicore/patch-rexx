use std::io::Write;
use std::process::{Command, Stdio};

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

fn run_rexx_with_stdin(expr: &str, stdin_data: &str) -> String {
    let mut child = Command::new(env!("CARGO_BIN_EXE_patch-rexx"))
        .args(["-e", expr])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to spawn patch-rexx");

    if let Some(mut stdin) = child.stdin.take() {
        stdin
            .write_all(stdin_data.as_bytes())
            .expect("failed to write stdin");
    }

    let output = child.wait_with_output().expect("failed to wait for output");
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

// ── Word parsing ──────────────────────────────────────────

#[test]
fn parse_var_single_var_gets_all() {
    assert_eq!(
        run_rexx("data = 'hello world'; parse var data x; say x"),
        "hello world"
    );
}

#[test]
fn parse_var_two_vars_word_split() {
    assert_eq!(
        run_rexx("data = 'hello world'; parse var data a b; say a '|' b"),
        "hello | world"
    );
}

#[test]
fn parse_var_three_vars_last_gets_rest() {
    assert_eq!(
        run_rexx("data = 'one two three four'; parse var data a b c; say a '|' b '|' c"),
        "one | two | three four"
    );
}

#[test]
fn parse_var_excess_vars_get_empty() {
    // b and c are empty; use || abuttal to avoid extra spaces
    assert_eq!(
        run_rexx(r#"data = 'hello'; parse var data a b c; say a || "|" || b || "|" || c"#),
        "hello||"
    );
}

// ── Literal patterns ─────────────────────────────────────

#[test]
fn parse_var_literal_slash_delimited() {
    assert_eq!(
        run_rexx("data = '2025/02/08'; parse var data year '/' month '/' day; say year month day"),
        "2025 02 08"
    );
}

#[test]
fn parse_var_literal_multi_char_pattern() {
    assert_eq!(
        run_rexx("data = 'foo::bar'; parse var data a '::' b; say a '|' b"),
        "foo | bar"
    );
}

#[test]
fn parse_var_literal_not_found() {
    assert_eq!(
        run_rexx("data = 'hello world'; parse var data a '/' b; say a '|' b"),
        "hello world |"
    );
}

// ── Positional patterns ──────────────────────────────────

#[test]
fn parse_var_absolute_position() {
    assert_eq!(
        run_rexx("data = 'abcdefghij'; parse var data a 5 b; say a '|' b"),
        "abcd | efghij"
    );
}

#[test]
fn parse_var_relative_forward() {
    assert_eq!(
        run_rexx("data = 'abcdefghij'; parse var data a +3 b; say a '|' b"),
        "abc | defghij"
    );
}

#[test]
fn parse_var_backward_reposition() {
    // Position 11 is past end: a gets all. Position 6 goes backward: b is empty. c gets pos 6+.
    assert_eq!(
        run_rexx(
            r#"data = 'abcdefghij'; parse var data a 11 b 6 c; say a || "|" || b || "|" || c"#
        ),
        "abcdefghij||fghij"
    );
}

// ── Dot placeholder ───────────────────────────────────────

#[test]
fn parse_var_dot_placeholder() {
    assert_eq!(
        run_rexx(
            "data = 'one two three four'; parse var data . second . rest; say second '|' rest"
        ),
        "two | four"
    );
}

// ── PARSE UPPER ───────────────────────────────────────────

#[test]
fn parse_upper_var() {
    assert_eq!(
        run_rexx("data = 'Hello World'; parse upper var data a b; say a '|' b"),
        "HELLO | WORLD"
    );
}

// ── PARSE VALUE ... WITH ──────────────────────────────────

#[test]
fn parse_value_with_simple() {
    assert_eq!(
        run_rexx("parse value 'hello world' with a b; say a '|' b"),
        "hello | world"
    );
}

#[test]
fn parse_value_with_expression() {
    assert_eq!(
        run_rexx("x = 'hello'; parse value x || ' world' with a b; say a '|' b"),
        "hello | world"
    );
}

// ── PARSE ARG (upgraded) ─────────────────────────────────

#[test]
fn arg_word_split_in_subroutine() {
    assert_eq!(
        run_rexx("say test('hello world'); exit; test: arg first rest; return first '|' rest"),
        "HELLO | WORLD"
    );
}

#[test]
fn arg_literal_pattern_in_subroutine() {
    assert_eq!(
        run_rexx(
            "say test('2025/02/08'); exit; test: arg year '/' month '/' day; return year month day"
        ),
        "2025 02 08"
    );
}

#[test]
fn arg_comma_multiple_args() {
    assert_eq!(
        run_rexx("say add(3, 4); exit; add: arg a, b; return a + b"),
        "7"
    );
}

// ── PARSE ARG (no uppercase) ─────────────────────────────

#[test]
fn parse_arg_preserves_case() {
    assert_eq!(
        run_rexx("say test('Hello World'); exit; test: parse arg msg; return msg"),
        "Hello World"
    );
}

// ── PARSE SOURCE / VERSION ────────────────────────────────

#[test]
fn parse_source() {
    assert_eq!(run_rexx("parse source sys . .; say sys"), "UNIX");
}

#[test]
fn parse_version() {
    let output = run_rexx("parse version name . .; say name");
    assert_eq!(output, "REXX-patch-rexx");
}

// ── PULL (with piped stdin) ───────────────────────────────

#[test]
fn pull_uppercases_stdin() {
    assert_eq!(
        run_rexx_with_stdin("pull line; say line", "hello world\n"),
        "HELLO WORLD"
    );
}

#[test]
fn pull_with_template() {
    assert_eq!(
        run_rexx_with_stdin("pull a b; say a '|' b", "hello world\n"),
        "HELLO | WORLD"
    );
}

// ── Variable pattern ─────────────────────────────────────

#[test]
fn parse_var_variable_pattern() {
    assert_eq!(
        run_rexx(
            "sep = '/'; data = '2025/02/08'; parse var data year (sep) month (sep) day; say year month day"
        ),
        "2025 02 08"
    );
}

// ── Backward compatibility ────────────────────────────────

#[test]
fn backward_compat_add() {
    assert_eq!(
        run_rexx("say add(3, 4); exit; add: arg a, b; return a + b"),
        "7"
    );
}

#[test]
fn backward_compat_factorial() {
    assert_eq!(
        run_rexx(
            "say fact(10); exit; fact: procedure; arg n; if n <= 1 then return 1; return n * fact(n - 1)"
        ),
        "3628800"
    );
}

// ── Edge cases ────────────────────────────────────────────

#[test]
fn parse_var_empty_source() {
    assert_eq!(run_rexx("data = ''; parse var data a b; say a '|' b"), "|");
}

#[test]
fn parse_var_no_template() {
    // No crash when template is empty
    assert_eq!(
        run_rexx("data = 'hello'; parse var data; say data"),
        "hello"
    );
}

#[test]
fn parse_var_single_word_multiple_targets() {
    assert_eq!(
        run_rexx(r#"data = 'word'; parse var data a b c; say a || "|" || b || "|" || c"#),
        "word||"
    );
}

// ── PARSE LINEIN ─────────────────────────────────────────

#[test]
fn parse_linein_reads_stdin() {
    assert_eq!(
        run_rexx_with_stdin("parse linein line; say line", "test input\n"),
        "test input"
    );
}

// ── PARSE with no arguments (missing args return empty) ──

#[test]
fn arg_missing_returns_empty() {
    // Missing arguments: variable gets empty string from empty arg
    assert_eq!(run_rexx("say greet(); exit; greet: arg a; return a"), "");
}

// ── PARSE VALUE UPPER ────────────────────────────────────

#[test]
fn parse_upper_value() {
    assert_eq!(
        run_rexx("parse upper value 'Hello World' with a b; say a '|' b"),
        "HELLO | WORLD"
    );
}

// ── Tab handling ─────────────────────────────────────────

#[test]
fn parse_var_tab_as_blank() {
    // Tabs should be treated as word delimiters
    assert_eq!(
        run_rexx("data = 'hello\tworld'; parse var data a b; say a '|' b"),
        "hello | world"
    );
}

// ── Error paths ──────────────────────────────────────────

#[test]
fn error_parse_invalid_subkeyword() {
    let stderr = run_rexx_fail("parse upper bogus x");
    assert!(
        stderr.contains("Error 25"),
        "expected Error 25, got: {stderr}"
    );
}

#[test]
fn error_parse_value_missing_with() {
    let stderr = run_rexx_fail("parse value 'hello' x");
    assert!(
        stderr.contains("Error 25"),
        "expected Error 25, got: {stderr}"
    );
}

#[test]
fn error_parse_var_missing_name() {
    let stderr = run_rexx_fail("parse var 123 x");
    assert!(
        stderr.contains("Error 20"),
        "expected Error 20, got: {stderr}"
    );
}

#[test]
fn error_template_plus_without_number() {
    let stderr = run_rexx_fail("data = 'test'; parse var data a + b");
    assert!(
        stderr.contains("Error 38"),
        "expected Error 38, got: {stderr}"
    );
}

#[test]
fn error_template_minus_without_number() {
    let stderr = run_rexx_fail("data = 'test'; parse var data a - b");
    assert!(
        stderr.contains("Error 38"),
        "expected Error 38, got: {stderr}"
    );
}

#[test]
fn error_template_paren_missing_close() {
    let stderr = run_rexx_fail("data = 'test'; parse var data a (sep x");
    assert!(
        stderr.contains("Error 38"),
        "expected Error 38, got: {stderr}"
    );
}

// ── UTF-8 safety ─────────────────────────────────────────

#[test]
fn parse_var_multibyte_absolute_position() {
    // 🎯 is 4 bytes in UTF-8; position 2 means character 2, not byte 2
    assert_eq!(
        run_rexx("data = '🎯test'; parse var data a 2 b; say a '|' b"),
        "🎯 | test"
    );
}

#[test]
fn parse_var_multibyte_relative_position() {
    assert_eq!(
        run_rexx("data = '🎯🎯rest'; parse var data a +2 b; say a '|' b"),
        "🎯🎯 | rest"
    );
}

#[test]
fn parse_var_multibyte_word_split() {
    // Word parsing on multi-byte characters
    assert_eq!(
        run_rexx("data = 'café latte'; parse var data a b; say a '|' b"),
        "café | latte"
    );
}

// ── Empty variable pattern ───────────────────────────────

#[test]
fn parse_var_empty_variable_pattern() {
    // Empty pattern should be treated as not found; a gets all, b gets empty
    assert_eq!(
        run_rexx(r"sep = ''; data = 'hello'; parse var data a (sep) b; say a '|' b"),
        "hello |"
    );
}
