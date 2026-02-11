//! Phase 12A: ANSI Conformance Fixes
//!
//! Tests for NUMERIC DIGITS/FORM/FUZZ, PUSH/QUEUE/PULL data queue,
//! `QUEUED()` BIF, `DIGITS()`/`FORM()`/`FUZZ()` BIFs, and line continuation.

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

fn stderr(output: &std::process::Output) -> String {
    String::from_utf8(output.stderr.clone())
        .expect("non-utf8 stderr")
        .trim()
        .to_string()
}

// ── NUMERIC DIGITS ───────────────────────────────────────────────────

#[test]
fn numeric_digits_default() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say digits()");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "9");
}

#[test]
fn numeric_digits_basic() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "numeric digits 15; say digits()");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "15");
}

#[test]
fn numeric_digits_reset_default() {
    let dir = TempDir::new().unwrap();
    write_file(
        &dir,
        "test.rexx",
        "numeric digits 20; numeric digits; say digits()",
    );
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "9");
}

#[test]
fn numeric_digits_affects_arithmetic() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "numeric digits 3; say 1/3");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "0.333");
}

#[test]
fn numeric_digits_high_precision() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "numeric digits 20; say 1/3");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "0.33333333333333333333");
}

// ── NUMERIC FORM ─────────────────────────────────────────────────────

#[test]
fn numeric_form_default() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say form()");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "SCIENTIFIC");
}

#[test]
fn numeric_form_scientific() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "numeric form scientific; say form()");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "SCIENTIFIC");
}

#[test]
fn numeric_form_engineering() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "numeric form engineering; say form()");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "ENGINEERING");
}

// ── NUMERIC FUZZ ─────────────────────────────────────────────────────

#[test]
fn numeric_fuzz_default() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say fuzz()");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "0");
}

#[test]
fn numeric_fuzz_basic() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "numeric fuzz 3; say fuzz()");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "3");
}

#[test]
fn numeric_fuzz_must_be_less_than_digits() {
    let dir = TempDir::new().unwrap();
    // FUZZ 9 with DIGITS 9 should fail
    write_file(&dir, "test.rexx", "numeric fuzz 9");
    let output = run_rexx(&dir, "test.rexx");
    assert!(!output.status.success());
    assert!(
        stderr(&output).contains("26") || stderr(&output).contains("FUZZ"),
        "stderr: {}",
        stderr(&output)
    );
}

// ── PUSH / QUEUE / PULL ──────────────────────────────────────────────

#[test]
fn push_and_pull() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "push 'hello'\nparse pull x\nsay x");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "hello");
}

#[test]
fn queue_and_pull() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "queue 'hello'\nparse pull x\nsay x");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    // PARSE PULL does not uppercase (only shorthand PULL does)
    assert_eq!(stdout(&output), "hello");
}

#[test]
fn push_lifo_order() {
    let dir = TempDir::new().unwrap();
    write_file(
        &dir,
        "test.rexx",
        "push 'a'\npush 'b'\nparse pull x\nparse pull y\nsay x y",
    );
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    // PUSH is LIFO: 'b' was pushed last → pulled first
    // PARSE PULL does not uppercase
    assert_eq!(stdout(&output), "b a");
}

#[test]
fn queue_fifo_order() {
    let dir = TempDir::new().unwrap();
    write_file(
        &dir,
        "test.rexx",
        "queue 'a'\nqueue 'b'\nparse pull x\nparse pull y\nsay x y",
    );
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    // QUEUE is FIFO: 'a' was queued first → pulled first
    // PARSE PULL does not uppercase
    assert_eq!(stdout(&output), "a b");
}

#[test]
fn push_empty_line() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "push\nparse pull x\nsay length(x)");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "0");
}

// ── QUEUED() BIF ─────────────────────────────────────────────────────

#[test]
fn queued_bif_empty() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say queued()");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "0");
}

#[test]
fn queued_bif_after_push() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "push 'a'\npush 'b'\nsay queued()");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "2");
}

#[test]
fn queued_bif_decreases_after_pull() {
    let dir = TempDir::new().unwrap();
    write_file(
        &dir,
        "test.rexx",
        "push 'a'\npush 'b'\nparse pull .\nsay queued()",
    );
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "1");
}

// ── PULL shorthand (uppercases, checks queue) ─────────────────────────

#[test]
fn pull_shorthand_from_queue() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "push 'World'\npull x\nsay x");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "WORLD");
}

// ── Line continuation ────────────────────────────────────────────────

#[test]
fn line_continuation_basic() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "say 'hello',\n  'world'");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "hello world");
}

#[test]
fn line_continuation_in_expr() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "x = 1 +,\n  2\nsay x");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "3");
}

#[test]
fn line_continuation_multiple() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "x = 1 +,\n  2 +,\n  3\nsay x");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "6");
}

#[test]
fn line_continuation_with_comment() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "test.rexx", "x = 1 +, /* continue */\n  2\nsay x");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "3");
}

#[test]
fn comma_in_function_not_continuation() {
    let dir = TempDir::new().unwrap();
    // Comma inside function call (not at end of line) should NOT be continuation
    write_file(&dir, "test.rexx", "say max(1, 2, 3)");
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "3");
}

// ── PARSE PULL with data queue ───────────────────────────────────────

#[test]
fn parse_pull_from_queue() {
    let dir = TempDir::new().unwrap();
    write_file(
        &dir,
        "test.rexx",
        "queue 'John 42'\nparse pull name age\nsay name age",
    );
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    // PARSE PULL does not uppercase (only shorthand PULL does)
    assert_eq!(stdout(&output), "John 42");
}

// ── Mixed PUSH/QUEUE ordering ────────────────────────────────────────

#[test]
fn mixed_push_queue_order() {
    let dir = TempDir::new().unwrap();
    write_file(
        &dir,
        "test.rexx",
        r"queue 'first'
push 'urgent'
queue 'last'
parse pull a
parse pull b
parse pull c
say a b c",
    );
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    // PUSH 'urgent' goes to front, QUEUE items go to back in order
    // PARSE PULL does not uppercase
    assert_eq!(stdout(&output), "urgent first last");
}

// ── NUMERIC FORM VALUE ───────────────────────────────────────────────

#[test]
fn numeric_form_value_expr() {
    let dir = TempDir::new().unwrap();
    write_file(
        &dir,
        "test.rexx",
        "x = 'ENGINEERING'\nnumeric form value x\nsay form()",
    );
    let output = run_rexx(&dir, "test.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "ENGINEERING");
}
