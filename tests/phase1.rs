use std::process::Command;

fn run_rexx(expr: &str) -> String {
    let output = Command::new(env!("CARGO_BIN_EXE_patch-rexx"))
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

#[test]
fn say_addition() {
    assert_eq!(run_rexx("say 2 + 3"), "5");
}

#[test]
fn say_precedence() {
    assert_eq!(run_rexx("say 2 + 3 * 4"), "14");
}

#[test]
fn say_variable() {
    assert_eq!(run_rexx("x = 10; say x + 5"), "15");
}

#[test]
fn say_blank_concat() {
    assert_eq!(run_rexx("say 'hello' 'world'"), "hello world");
}

#[test]
fn say_string_literal() {
    assert_eq!(run_rexx("say 'Hello, World!'"), "Hello, World!");
}

#[test]
fn say_power() {
    assert_eq!(run_rexx("say 2 ** 10"), "1024");
}

#[test]
fn say_comparison() {
    assert_eq!(run_rexx("say 3 > 2"), "1");
}

#[test]
fn say_multiple_statements() {
    assert_eq!(run_rexx("x = 3; y = 4; say x * y"), "12");
}
