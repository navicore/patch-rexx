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

// ── Command execution ────────────────────────────────────────────────

#[test]
fn command_simple() {
    assert_eq!(run_rexx("'echo hello'"), "hello");
}

#[test]
fn command_rc_zero() {
    assert_eq!(run_rexx("'echo hello > /dev/null'; say rc"), "0");
}

#[test]
fn command_rc_nonzero() {
    assert_eq!(run_rexx("'sh -c \"exit 3\"'; say rc"), "3");
}

#[test]
fn command_variable() {
    assert_eq!(run_rexx("cmd = 'echo hi'; cmd"), "hi");
}

#[test]
fn command_expression() {
    assert_eq!(run_rexx("'echo' 'hello'"), "hello");
}

#[test]
fn command_preserves_rc() {
    assert_eq!(
        run_rexx("'sh -c \"exit 5\"'; x = rc; 'echo done > /dev/null'; say x rc"),
        "5 0"
    );
}

#[test]
fn command_in_do_loop() {
    // Run a command inside a DO loop
    assert_eq!(run_rexx("do i = 1 to 3; 'echo' i; end"), "1\n2\n3");
}

// ── ADDRESS instruction ─────────────────────────────────────────────

#[test]
fn address_default_is_system() {
    assert_eq!(run_rexx("say address()"), "SYSTEM");
}

#[test]
fn address_set_environment() {
    assert_eq!(run_rexx("address sh; say address()"), "SH");
}

#[test]
fn address_swap() {
    // Set to SH, then bare ADDRESS swaps back to SYSTEM
    assert_eq!(run_rexx("address sh; address; say address()"), "SYSTEM");
}

#[test]
fn address_swap_double() {
    // Swap twice returns to SH
    assert_eq!(
        run_rexx("address sh; address; address; say address()"),
        "SH"
    );
}

#[test]
fn address_temporary() {
    // Temporary command runs in specified env, then reverts
    assert_eq!(
        run_rexx("address sh 'echo temp'; say address()"),
        "temp\nSYSTEM"
    );
}

#[test]
fn address_value() {
    assert_eq!(
        run_rexx("env = 'SH'; address value env; say address()"),
        "SH"
    );
}

#[test]
fn address_case_insensitive() {
    // Environment names are uppercased
    assert_eq!(run_rexx("address sh; say address()"), "SH");
}

#[test]
fn address_temporary_preserves_rc() {
    assert_eq!(run_rexx("address sh 'sh -c \"exit 7\"'; say rc"), "7");
}

// ── ERROR/FAILURE conditions ────────────────────────────────────────

#[test]
fn error_trap_fires() {
    assert_eq!(
        run_rexx("signal on error; 'sh -c \"exit 1\"'; say 'NOT REACHED'; error: say 'TRAPPED'"),
        "TRAPPED"
    );
}

#[test]
fn error_trap_sets_rc() {
    assert_eq!(
        run_rexx("signal on error; 'sh -c \"exit 42\"'; error: say rc"),
        "42"
    );
}

#[test]
fn error_no_trap_continues() {
    // Without a trap, execution continues after a failed command
    assert_eq!(
        run_rexx("'sh -c \"exit 1\"'; say 'continued' rc"),
        "continued 1"
    );
}

#[test]
fn failure_trap_fires() {
    // A command that can't be spawned at all triggers FAILURE.
    // We use a nonexistent path that sh -c will fail on with exit 127.
    // Since sh -c can execute, this is an ERROR (rc!=0), not FAILURE.
    // True FAILURE (spawn failure) is hard to trigger portably.
    // Instead, test that error traps work for nonexistent commands via sh.
    assert_eq!(
        run_rexx("signal on error; '/nonexistent_command_xyz_abc'; error: say 'GOT ERROR'"),
        "GOT ERROR"
    );
}

#[test]
fn error_condition_info() {
    assert_eq!(
        run_rexx("signal on error; 'sh -c \"exit 1\"'; error: say condition('C')"),
        "ERROR"
    );
}

#[test]
fn error_trap_fires_once() {
    // Trap auto-disables after firing; second command continues normally
    assert_eq!(
        run_rexx(
            "signal on error; 'sh -c \"exit 1\"'; say 'SKIP'; error: say 'FIRST'; 'sh -c \"exit 2\"'; say rc"
        ),
        "FIRST\n2"
    );
}

// ── Integration ─────────────────────────────────────────────────────

#[test]
fn command_in_interpret() {
    assert_eq!(run_rexx("interpret \"'echo interp'\""), "interp");
}

#[test]
fn address_bif_no_args() {
    // ADDRESS() with no args returns current environment
    assert_eq!(run_rexx("x = address(); say x"), "SYSTEM");
}

#[test]
fn address_in_subroutine() {
    // ADDRESS persists across subroutine calls (it's not scope-dependent)
    assert_eq!(
        run_rexx("address sh; call myproc; say address(); exit; myproc: say address(); return"),
        "SH\nSH"
    );
}

#[test]
fn command_with_signal_on_syntax() {
    // SYNTAX trap catches parse errors in INTERPRET, command execution works fine
    assert_eq!(
        run_rexx("signal on syntax; interpret 'say 1 +'; syntax: say 'CAUGHT'"),
        "CAUGHT"
    );
}

#[test]
fn address_temporary_reverts_after_error() {
    // Even if a temporary command fails, the address reverts
    assert_eq!(
        run_rexx("address sh 'sh -c \"exit 1\"'; say address() rc"),
        "SYSTEM 1"
    );
}

#[test]
fn command_rc_persists_across_say() {
    // RC is a normal variable, persists after SAY
    assert_eq!(run_rexx("'sh -c \"exit 4\"'; say rc; say rc"), "4\n4");
}

#[test]
fn address_value_expression() {
    // ADDRESS VALUE can use a computed expression
    assert_eq!(
        run_rexx("x = 'SY'; y = 'STEM'; address value x || y; say address()"),
        "SYSTEM"
    );
}
