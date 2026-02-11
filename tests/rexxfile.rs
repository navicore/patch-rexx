use std::io::Write;
use std::process::Command;

fn run_rexx(expr: &str) -> String {
    let output = Command::new(env!("CARGO_BIN_EXE_rexx"))
        .args(["-e", expr])
        .output()
        .expect("failed to run patch-rexx");
    assert!(
        output.status.success(),
        "patch-rexx exited with error: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8(output.stdout)
        .expect("non-utf8 output")
        .trim()
        .to_string()
}

fn run_rexx_file(source: &str) -> std::process::Output {
    let mut tmp = tempfile::NamedTempFile::with_suffix(".rexx").expect("failed to create temp file");
    tmp.write_all(source.as_bytes()).expect("failed to write");
    tmp.flush().expect("failed to flush");
    Command::new(env!("CARGO_BIN_EXE_rexx"))
        .arg(tmp.path())
        .output()
        .expect("failed to run patch-rexx")
}

#[test]
fn shebang_line_ignored() {
    let out = run_rexx_file("#!/usr/bin/env rexx\nSAY 'hello from shebang'\n");
    assert!(out.status.success());
    let stdout = String::from_utf8(out.stdout).unwrap();
    assert_eq!(stdout.trim(), "hello from shebang");
}

#[test]
fn shebang_with_comment_and_code() {
    let source = "#!/usr/bin/env rexx\n/* a comment */\nSAY 'works'\n";
    let out = run_rexx_file(source);
    assert!(out.status.success());
    let stdout = String::from_utf8(out.stdout).unwrap();
    assert_eq!(stdout.trim(), "works");
}

#[test]
fn exit_code_zero() {
    let out = run_rexx_file("EXIT 0\n");
    assert!(out.status.success());
}

#[test]
fn exit_code_nonzero() {
    let out = run_rexx_file("EXIT 2\n");
    assert_eq!(out.status.code(), Some(2));
}

#[test]
fn exit_code_42() {
    let out = run_rexx_file("EXIT 42\n");
    assert_eq!(out.status.code(), Some(42));
}

#[test]
fn exit_code_from_variable() {
    let out = run_rexx_file("rc = 7\nEXIT rc\n");
    assert_eq!(out.status.code(), Some(7));
}

#[test]
fn exit_no_value_is_zero() {
    let out = run_rexx_file("SAY 'hi'\nEXIT\n");
    assert!(out.status.success());
    let stdout = String::from_utf8(out.stdout).unwrap();
    assert_eq!(stdout.trim(), "hi");
}

#[test]
fn no_shebang_still_works() {
    assert_eq!(run_rexx("SAY 'no shebang'"), "no shebang");
}
