//! ANSI X3.274-1996 Conformance Test Suite
//!
//! Organized by spec section. Each test exercises edge cases and spec compliance
//! gaps not already covered by phases 1–12a.

use std::process::Command;
use tempfile::TempDir;

fn run_rexx(dir: &TempDir, filename: &str) -> std::process::Output {
    let path = dir.path().join(filename);
    Command::new(env!("CARGO_BIN_EXE_rexx"))
        .arg(&path)
        .output()
        .expect("failed to run rexx")
}

fn write_file(dir: &TempDir, name: &str, content: &str) {
    std::fs::write(dir.path().join(name), content).unwrap();
}

fn stdout(output: &std::process::Output) -> String {
    String::from_utf8(output.stdout.clone())
        .expect("non-utf8 output")
        .trim()
        .to_string()
}

/// stdout without trimming, but removes the trailing newline from SAY
fn stdout_raw(output: &std::process::Output) -> String {
    let s = String::from_utf8(output.stdout.clone()).expect("non-utf8 output");
    s.strip_suffix('\n').unwrap_or(&s).to_string()
}

fn stderr(output: &std::process::Output) -> String {
    String::from_utf8(output.stderr.clone())
        .expect("non-utf8 stderr")
        .trim()
        .to_string()
}

// ============================================================================
// Section 6: Lexical
// ============================================================================

#[test]
fn sec6_hex_string_say() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say '48454C4C4F'x");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "HELLO");
}

#[test]
fn sec6_binary_string_say() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say '01000001'b");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "A");
}

#[test]
fn sec6_hex_string_with_spaces() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say '48 45 4C'x");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "HEL");
}

#[test]
fn sec6_nested_comments_deep() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "/* a /* b /* c */ d */ e */ say 'OK'");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "OK");
}

#[test]
fn sec6_line_comment_after_code() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say 'hello' -- comment");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "hello");
}

#[test]
fn sec6_empty_program() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "");
}

#[test]
fn sec6_comment_only_program() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "/* just a comment */");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "");
}

#[test]
fn sec6_doubled_quote_double_quotes() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", r#"say "it""s""#);
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), r#"it"s"#);
}

#[test]
fn sec6_mixed_quote_styles() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", r#"say "hello" 'world'"#);
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "hello world");
}

#[test]
fn sec6_hex_string_empty() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say ''x");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "");
}

// ============================================================================
// Section 7: Clauses & Instructions
// ============================================================================

#[test]
fn sec7_multiple_empty_clauses() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", ";;; say 'hello' ;;;");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "hello");
}

#[test]
fn sec7_nop_in_if() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "if 1 then nop; say 'after'");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "after");
}

#[test]
fn sec7_nop_in_select_otherwise() {
    let dir = TempDir::new().unwrap();
    write_file(
        &dir,
        "test.rexx",
        "select; when 0 then nop; otherwise nop; end; say 'OK'",
    );
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "OK");
}

#[test]
fn sec7_compound_variable_complex_tail() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "i=3; j=5; arr.i.j='found'; say arr.3.5");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "found");
}

#[test]
fn sec7_compound_variable_default_name() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say arr.1");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "ARR.1");
}

#[test]
fn sec7_abuttal_variable_string() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "x='hello'; say x'world'");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "helloworld");
}

#[test]
fn sec7_blank_concat_vs_abuttal() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "a='AB'; say a 'CD' a'EF'");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "AB CD ABEF");
}

#[test]
fn sec7_function_call_space_before_paren() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "x='hello'; say x ('world')");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "hello world");
}

#[test]
fn sec7_say_no_expression() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    // SAY with no args outputs an empty line, so stdout is "\n" which trims to ""
    assert_eq!(stdout(&output), "");
}

#[test]
fn sec7_stem_default_assignment() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "arr.='default'; say arr.99");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "default");
}

// ============================================================================
// Section 8: Expressions & Operators
// ============================================================================

#[test]
fn sec8_power_right_assoc_execution() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say 2 ** 3 ** 2");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "512");
}

#[test]
fn sec8_numeric_cmp_leading_zeros() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say (007 = 7)");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "1");
}

#[test]
fn sec8_numeric_cmp_trailing_zeros() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say (3.0 = 3)");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "1");
}

#[test]
fn sec8_numeric_cmp_whitespace() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say (' 3 ' = 3)");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "1");
}

#[test]
fn sec8_string_cmp_non_numeric() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say ('abc' > 'abb')");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "1");
}

#[test]
fn sec8_strict_eq_leading_space() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say (' 1' == '1')");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "0");
}

#[test]
fn sec8_strict_eq_trailing_space() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say ('abc' == 'abc ')");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "0");
}

#[test]
fn sec8_normal_eq_pads_shorter() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say ('abc' = 'abc ')");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "1");
}

#[test]
fn sec8_strict_gt_string_order() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say ('9' >> '10')");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "1");
}

#[test]
fn sec8_normal_gt_numeric_order() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say (9 > 10)");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "0");
}

#[test]
fn sec8_logical_and() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say (1 & 1)");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "1");
}

#[test]
fn sec8_logical_or() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say (0 | 1)");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "1");
}

#[test]
fn sec8_logical_xor_different() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say (1 && 0)");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "1");
}

#[test]
fn sec8_logical_xor_same() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say (1 && 1)");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "0");
}

#[test]
fn sec8_logical_not_one() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say \\1");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "0");
}

#[test]
fn sec8_logical_not_zero() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say \\0");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "1");
}

#[test]
fn sec8_logical_non_boolean_error() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say (2 & 1)");
    let output = run_rexx(&dir, "test.rexx");
    assert!(
        !output.status.success(),
        "expected error for non-boolean operand"
    );
}

#[test]
fn sec8_intdiv_negative() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say (-7 % 2)");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "-3");
}

#[test]
fn sec8_remainder_negative() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say (-7 // 2)");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "-1");
}

#[test]
fn sec8_division_precision() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "numeric digits 5; say 2/3");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "0.66667");
}

// ============================================================================
// Section 9: PARSE
// ============================================================================

#[test]
fn sec9_parse_comma_separates_args() {
    let dir = TempDir::new().unwrap();
    write_file(
        &dir,
        "test.rexx",
        r"call myfunc 'John 42', 'NYC'
exit
myfunc:
  parse arg name age, city
  say name '/' age '/' city
  return",
    );
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "John / 42 / NYC");
}

#[test]
fn sec9_parse_backward_reposition() {
    let dir = TempDir::new().unwrap();
    write_file(
        &dir,
        "test.rexx",
        "data = 'ABCDEFGH'; parse var data a 5 b 3 c; say a '|' b '|' c",
    );
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "ABCD |  | CDEFGH");
}

#[test]
fn sec9_parse_relative_zero() {
    let dir = TempDir::new().unwrap();
    write_file(
        &dir,
        "test.rexx",
        "data = 'ABCDEF'; parse var data a +3 b +0 c; say a '|' b '|' c",
    );
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "ABC |  | DEF");
}

#[test]
fn sec9_parse_pattern_not_found() {
    let dir = TempDir::new().unwrap();
    write_file(
        &dir,
        "test.rexx",
        "data = 'hello world'; parse var data a 'NOPE' b; say a '|' b",
    );
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "hello world |");
}

#[test]
fn sec9_parse_multiple_literal_patterns() {
    let dir = TempDir::new().unwrap();
    write_file(
        &dir,
        "test.rexx",
        "data = 'name=John;age=42'; parse var data k1 '=' v1 ';' k2 '=' v2; say k1 v1 k2 v2",
    );
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "name John age 42");
}

#[test]
fn sec9_parse_upper_vs_parse() {
    let dir = TempDir::new().unwrap();
    write_file(
        &dir,
        "test.rexx",
        "d = 'Hello'; parse var d a; parse upper var d b; say a b",
    );
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "Hello HELLO");
}

#[test]
fn sec9_parse_value_computed() {
    let dir = TempDir::new().unwrap();
    write_file(
        &dir,
        "test.rexx",
        "x = 'foo'; y = 'bar'; parse value x||':'||y with a ':' b; say a '|' b",
    );
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "foo | bar");
}

#[test]
fn sec9_parse_three_comma_args() {
    let dir = TempDir::new().unwrap();
    write_file(
        &dir,
        "test.rexx",
        r"call myfunc 'a', 'b', 'c'
exit
myfunc:
  parse arg x, y, z
  say x y z
  return",
    );
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "a b c");
}

// ============================================================================
// Section 10: Conditions & SIGNAL
// ============================================================================

#[test]
fn sec10_signal_value_computed() {
    let dir = TempDir::new().unwrap();
    write_file(
        &dir,
        "test.rexx",
        r"x = 'LA'; signal value x||'BEL'
say 'NEVER'
exit
label:
  say 'YES'
  exit",
    );
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "YES");
}

#[test]
fn sec10_trap_reenable_different_handler() {
    let dir = TempDir::new().unwrap();
    write_file(
        &dir,
        "test.rexx",
        r"signal on syntax name h1
call bad1
exit
bad1:
  interpret 'say 1 +'
  return
h1:
  say 'H1'
  signal on syntax name h2
  call bad2
  exit
bad2:
  interpret 'say 2 +'
  return
h2:
  say 'H2'
  exit",
    );
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "H1\nH2");
}

#[test]
fn sec10_condition_description_nonempty() {
    let dir = TempDir::new().unwrap();
    write_file(
        &dir,
        "test.rexx",
        r"signal on syntax name handler
interpret 'say 1 /'
exit
handler:
  d = condition('D')
  say (d \= '')
  exit",
    );
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "1");
}

#[test]
fn sec10_signal_exits_triple_nested() {
    let dir = TempDir::new().unwrap();
    write_file(
        &dir,
        "test.rexx",
        r"signal on syntax name handler
do i = 1 to 3
  do j = 1 to 3
    do k = 1 to 3
      if k = 2 then interpret 'say 1 +'
    end
  end
end
exit
handler:
  say i j k
  exit",
    );
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "1 1 2");
}

#[test]
fn sec10_novalue_in_concat_expr() {
    let dir = TempDir::new().unwrap();
    write_file(
        &dir,
        "test.rexx",
        r"signal on novalue name handler
x = 'hello' badvar
say x
exit
handler:
  say 'TRAPPED'
  exit",
    );
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "TRAPPED");
}

#[test]
fn sec10_signal_off_then_on() {
    let dir = TempDir::new().unwrap();
    write_file(
        &dir,
        "test.rexx",
        r"signal on syntax name old_handler
signal off syntax
signal on syntax name new_handler
interpret 'say 1 +'
exit
old_handler:
  say 'OLD_HANDLER'
  exit
new_handler:
  say 'NEW_HANDLER'
  exit",
    );
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "NEW_HANDLER");
}

#[test]
fn sec10_signal_from_otherwise() {
    let dir = TempDir::new().unwrap();
    write_file(
        &dir,
        "test.rexx",
        r"x = 99
select
  when x = 1 then say 'ONE'
  otherwise signal done
end
exit
done:
  say 'DONE'
  exit",
    );
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "DONE");
}

// ============================================================================
// Section 11: BIF Edge Cases
// ============================================================================

#[test]
fn sec11_substr_beyond_end_pads() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say length(substr('abc',2,5))");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "5");
}

#[test]
fn sec11_substr_start_past_end() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say substr('abc',5,3,'*')");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "***");
}

#[test]
fn sec11_substr_custom_pad() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say substr('abc',4,3,'-')");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "---");
}

#[test]
fn sec11_left_zero_length() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say left('hello',0)");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "");
}

#[test]
fn sec11_right_zero_length() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say right('hello',0)");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "");
}

#[test]
fn sec11_left_with_custom_pad() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say left('hi',8,'.')");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "hi......");
}

#[test]
fn sec11_right_with_custom_pad() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say right('hi',8,'.')");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "......hi");
}

#[test]
fn sec11_translate_incomplete_output() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say translate('abcde','12','abcde')");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout_raw(&output), "12   ");
}

#[test]
fn sec11_translate_with_pad_char() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say translate('abcde','12','abcde','*')");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "12***");
}

#[test]
fn sec11_datatype_empty_string() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say datatype('')");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "CHAR");
}

#[test]
fn sec11_datatype_alpha_empty() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say datatype('','A')");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "0");
}

#[test]
fn sec11_datatype_whole_number() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say datatype('42','W')");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "1");
}

#[test]
fn sec11_datatype_decimal_not_whole() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say datatype('3.5','W')");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "0");
}

#[test]
fn sec11_wordpos_phrase_past_end() {
    let dir = TempDir::new().unwrap();
    write_file(
        &dir,
        "test.rexx",
        "say wordpos('three four five','one two three four')",
    );
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "0");
}

#[test]
fn sec11_format_integer_width() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say format(3.14,3,0)");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout_raw(&output), "  3");
}

#[test]
fn sec11_format_before_overflow() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say format(123,2,0)");
    let output = run_rexx(&dir, "test.rexx");
    assert!(
        !output.status.success(),
        "expected error for integer exceeding width"
    );
}

#[test]
fn sec11_compare_equal_padded() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say compare('abc','abc   ')");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "0");
}

#[test]
fn sec11_verify_match_mode() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say verify('123abc','0123456789','M')");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "1");
}

#[test]
fn sec11_verify_nomatch_with_start() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say verify('aaabbb','a','N',4)");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "4");
}

#[test]
fn sec11_strip_leading_zeros() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say strip('000123000','L','0')");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "123000");
}
