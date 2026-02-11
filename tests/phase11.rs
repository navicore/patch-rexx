//! Phase 11: External Function Libraries
//!
//! Tests for external function resolution — searching the filesystem for
//! `.rexx`/`.rex` files when a call cannot resolve to an internal label or BIF.

use std::process::Command;
use tempfile::TempDir;

fn run_rexx_file(dir: &TempDir, filename: &str) -> std::process::Output {
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

// ── Test 1: Basic external CALL ──────────────────────────────────────

#[test]
fn external_call_basic() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "greet.rexx", "parse arg name; return 'Hello,' name");
    write_file(&dir, "main.rexx", "call greet 'World'; say result");
    let output = run_rexx_file(&dir, "main.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "Hello, World");
}

// ── Test 2: External function in expression ──────────────────────────

#[test]
fn external_function_in_expr() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "add.rexx", "parse arg a, b; return a + b");
    write_file(&dir, "main.rexx", "say add(3, 4)");
    let output = run_rexx_file(&dir, "main.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "7");
}

// ── Test 3: External not found → Error 43 ───────────────────────────

#[test]
fn external_not_found() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "main.rexx", "call nosuch");
    let output = run_rexx_file(&dir, "main.rexx");
    assert!(!output.status.success());
    assert!(
        stderr(&output).contains("43"),
        "stderr: {}",
        stderr(&output)
    );
}

// ── Test 4: Internal label takes priority over external ──────────────

#[test]
fn internal_label_priority() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "greet.rexx", "return 'EXTERNAL'");
    write_file(
        &dir,
        "main.rexx",
        "say greet(); exit; greet: return 'INTERNAL'",
    );
    let output = run_rexx_file(&dir, "main.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "INTERNAL");
}

// ── Test 5: BIF takes priority over external ─────────────────────────

#[test]
fn bif_priority() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "length.rexx", "parse arg s; return 999");
    write_file(&dir, "main.rexx", "say length('abc')");
    let output = run_rexx_file(&dir, "main.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "3");
}

// ── Test 6: REXXPATH resolution ──────────────────────────────────────

#[test]
fn rexxpath_resolution() {
    let dir = TempDir::new().unwrap();
    let lib_dir = TempDir::new().unwrap();
    write_file(&lib_dir, "helper.rexx", "return 'FROM_LIB'");
    write_file(&dir, "main.rexx", "say helper()");
    let output = Command::new(env!("CARGO_BIN_EXE_rexx"))
        .arg(dir.path().join("main.rexx"))
        .env("REXXPATH", lib_dir.path())
        .output()
        .expect("failed to run rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "FROM_LIB");
}

// ── Test 7: External scope isolation ─────────────────────────────────

#[test]
fn external_scope_isolation() {
    let dir = TempDir::new().unwrap();
    // An unset variable returns its own name in REXX, so if CALLER_VAR
    // is not visible, it returns 'CALLER_VAR' instead of 'secret'.
    write_file(
        &dir,
        "check.rexx",
        "if caller_var = 'CALLER_VAR' then return 'ISOLATED'; else return 'LEAKED'",
    );
    write_file(&dir, "main.rexx", "caller_var = 'secret'; say check()");
    let output = run_rexx_file(&dir, "main.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "ISOLATED");
}

// ── Test 8: External calls external (chain) ──────────────────────────

#[test]
fn external_calls_external() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "double.rexx", "parse arg n; return n * 2");
    write_file(&dir, "quadruple.rexx", "parse arg n; return double(n) * 2");
    write_file(&dir, "main.rexx", "say quadruple(3)");
    let output = run_rexx_file(&dir, "main.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "12");
}

// ── Test 9: PARSE SOURCE returns actual filename ─────────────────────

#[test]
fn parse_source_filename() {
    let dir = TempDir::new().unwrap();
    write_file(
        &dir,
        "main.rexx",
        "parse source sys calltype fname; say fname",
    );
    let output = run_rexx_file(&dir, "main.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "main.rexx");
}

// ── Test 10: Case-insensitive resolution ─────────────────────────────

#[test]
fn case_insensitive_resolution() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "myutil.rexx", "return 'UTIL_OK'");
    write_file(&dir, "main.rexx", "say MYUTIL()");
    let output = run_rexx_file(&dir, "main.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "UTIL_OK");
}

// ── Test 11: External CALL with no RETURN → RESULT dropped ──────────

#[test]
fn external_no_return_call() {
    let dir = TempDir::new().unwrap();
    // When an external CALL returns without a value, RESULT should be dropped.
    // An unset variable returns its own name in REXX.
    write_file(&dir, "noret.rexx", "x = 1");
    write_file(
        &dir,
        "main.rexx",
        "result = 'BEFORE'; call noret; if result = 'RESULT' then say 'DROPPED'; else say result",
    );
    let output = run_rexx_file(&dir, "main.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "DROPPED");
}

// ── Test 12: External function with no RETURN in expression → Error 44

#[test]
fn external_no_return_expr() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "noret.rexx", "x = 1");
    write_file(&dir, "main.rexx", "say noret()");
    let output = run_rexx_file(&dir, "main.rexx");
    assert!(!output.status.success());
    assert!(
        stderr(&output).contains("44"),
        "stderr: {}",
        stderr(&output)
    );
}

// ── Test 13: External calls external in subdirectory ─────────────────

#[test]
fn external_calls_external_subdir() {
    let dir = TempDir::new().unwrap();
    std::fs::create_dir(dir.path().join("lib")).unwrap();
    write_file(&dir, "lib/util.rexx", "parse arg n; return n * 10");
    // helper.rexx is in lib/ and calls util.rexx which is also in lib/
    write_file(&dir, "lib/helper.rexx", "parse arg n; return util(n)");
    // main calls lib/helper via REXXPATH
    write_file(&dir, "main.rexx", "say helper(5)");
    let output = Command::new(env!("CARGO_BIN_EXE_rexx"))
        .arg(dir.path().join("main.rexx"))
        .env("REXXPATH", dir.path().join("lib"))
        .output()
        .expect("failed to run rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "50");
}

// ── Test 14: .rex extension works ────────────────────────────────────

#[test]
fn rex_extension() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "greet.rex", "return 'HELLO_REX'");
    write_file(&dir, "main.rexx", "say greet()");
    let output = run_rexx_file(&dir, "main.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "HELLO_REX");
}

// ── Test 15: External file with parse error propagates ───────────────

#[test]
fn external_parse_error() {
    let dir = TempDir::new().unwrap();
    write_file(&dir, "bad.rexx", "if then");
    write_file(&dir, "main.rexx", "call bad");
    let output = run_rexx_file(&dir, "main.rexx");
    assert!(!output.status.success());
    // Should get a parse error (Error 18 = THEN expected, or similar)
    let err = stderr(&output);
    assert!(!err.is_empty(), "expected a parse error");
}

// ── Test 16: Multiple REXXPATH entries ───────────────────────────────

#[test]
fn rexxpath_multiple_entries() {
    let dir = TempDir::new().unwrap();
    let lib1 = TempDir::new().unwrap();
    let lib2 = TempDir::new().unwrap();
    write_file(&lib1, "from_lib1.rexx", "return 'LIB1'");
    write_file(&lib2, "from_lib2.rexx", "return 'LIB2'");
    write_file(&dir, "main.rexx", "say from_lib1() from_lib2()");
    let rexxpath = format!("{}:{}", lib1.path().display(), lib2.path().display());
    let output = Command::new(env!("CARGO_BIN_EXE_rexx"))
        .arg(dir.path().join("main.rexx"))
        .env("REXXPATH", &rexxpath)
        .output()
        .expect("failed to run rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "LIB1 LIB2");
}

// ── Test 17: source_path restored after external error ───────────────

#[test]
fn source_path_restored_after_error() {
    // If an external function errors, subsequent external calls from the
    // same caller should still resolve relative to the caller's directory.
    let dir = TempDir::new().unwrap();
    write_file(&dir, "bad.rexx", "x = 1 +");
    write_file(&dir, "good.rexx", "return 'OK'");
    // Use SIGNAL ON SYNTAX to trap the error from bad(), then call good()
    write_file(
        &dir,
        "main.rexx",
        "signal on syntax; call bad; syntax: say good()",
    );
    let output = run_rexx_file(&dir, "main.rexx");
    assert!(output.status.success(), "stderr: {}", stderr(&output));
    assert_eq!(stdout(&output), "OK");
}
