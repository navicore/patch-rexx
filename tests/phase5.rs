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

// ── String Functions ────────────────────────────────────────────────

#[test]
fn bif_length_basic() {
    assert_eq!(run_rexx("say length('hello')"), "5");
}

#[test]
fn bif_length_empty() {
    assert_eq!(run_rexx("say length('')"), "0");
}

#[test]
fn bif_substr_basic() {
    assert_eq!(run_rexx("say substr('hello world', 7)"), "world");
}

#[test]
fn bif_substr_with_length() {
    assert_eq!(run_rexx("say substr('hello world', 1, 5)"), "hello");
}

#[test]
fn bif_substr_with_pad() {
    assert_eq!(run_rexx("say substr('abc', 2, 5, '*')"), "bc***");
}

#[test]
fn bif_left_basic() {
    assert_eq!(run_rexx("say left('hello world', 5)"), "hello");
}

#[test]
fn bif_left_with_pad() {
    assert_eq!(run_rexx("say left('hi', 5, '.')"), "hi...");
}

#[test]
fn bif_right_basic() {
    assert_eq!(run_rexx("say right('hello world', 5)"), "world");
}

#[test]
fn bif_right_with_pad() {
    assert_eq!(run_rexx("say right('hi', 5, '.')"), "...hi");
}

#[test]
fn bif_pos_basic() {
    assert_eq!(run_rexx("say pos('ll', 'hello')"), "3");
}

#[test]
fn bif_pos_not_found() {
    assert_eq!(run_rexx("say pos('xyz', 'hello')"), "0");
}

#[test]
fn bif_pos_with_start() {
    assert_eq!(run_rexx("say pos('l', 'hello', 4)"), "4");
}

#[test]
fn bif_lastpos_basic() {
    assert_eq!(run_rexx("say lastpos('l', 'hello')"), "4");
}

#[test]
fn bif_index_basic() {
    assert_eq!(run_rexx("say index('hello', 'll')"), "3");
}

#[test]
fn bif_copies_basic() {
    assert_eq!(run_rexx("say copies('ab', 3)"), "ababab");
}

#[test]
fn bif_copies_zero() {
    assert_eq!(run_rexx("say copies('ab', 0)"), "");
}

#[test]
fn bif_reverse_basic() {
    assert_eq!(run_rexx("say reverse('hello')"), "olleh");
}

#[test]
fn bif_strip_both() {
    assert_eq!(run_rexx("say strip('  hello  ')"), "hello");
}

#[test]
fn bif_strip_leading() {
    assert_eq!(run_rexx("say strip('  hello  ', 'L')"), "hello");
}

#[test]
fn bif_strip_trailing() {
    // Use quotes to preserve leading spaces since run_rexx trims output
    assert_eq!(run_rexx("say '>'strip('  hello  ', 'T')'<'"), ">  hello<");
}

#[test]
fn bif_strip_custom_char() {
    assert_eq!(run_rexx("say strip('**hello**', 'B', '*')"), "hello");
}

#[test]
fn bif_space_basic() {
    assert_eq!(
        run_rexx("say space('  one   two  three  ')"),
        "one two three"
    );
}

#[test]
fn bif_space_with_n() {
    assert_eq!(
        run_rexx("say space('one two three', 3)"),
        "one   two   three"
    );
}

#[test]
fn bif_space_zero() {
    assert_eq!(run_rexx("say space('one two three', 0)"), "onetwothree");
}

#[test]
fn bif_overlay_basic() {
    assert_eq!(run_rexx("say overlay('***', 'abcdefgh', 4)"), "abc***gh");
}

#[test]
fn bif_insert_basic() {
    assert_eq!(run_rexx("say insert('123', 'abc', 2)"), "ab123c");
}

#[test]
fn bif_delstr_basic() {
    assert_eq!(run_rexx("say delstr('abcdef', 3, 2)"), "abef");
}

#[test]
fn bif_delstr_to_end() {
    assert_eq!(run_rexx("say delstr('abcdef', 3)"), "ab");
}

#[test]
fn bif_translate_uppercase() {
    assert_eq!(run_rexx("say translate('hello')"), "HELLO");
}

#[test]
fn bif_translate_with_tables() {
    assert_eq!(
        run_rexx("say translate('hello', 'AEIOU', 'aeiou')"),
        "hEllO"
    );
}

#[test]
fn bif_changestr_basic() {
    assert_eq!(
        run_rexx("say changestr('o', 'hello world', '0')"),
        "hell0 w0rld"
    );
}

#[test]
fn bif_countstr_basic() {
    assert_eq!(run_rexx("say countstr('l', 'hello world')"), "3");
}

#[test]
fn bif_compare_equal() {
    assert_eq!(run_rexx("say compare('hello', 'hello')"), "0");
}

#[test]
fn bif_compare_different() {
    assert_eq!(run_rexx("say compare('abc', 'axc')"), "2");
}

#[test]
fn bif_abbrev_true() {
    assert_eq!(run_rexx("say abbrev('PRINT', 'PRI')"), "1");
}

#[test]
fn bif_abbrev_false() {
    assert_eq!(run_rexx("say abbrev('PRINT', 'PRX')"), "0");
}

#[test]
fn bif_abbrev_min_length() {
    assert_eq!(run_rexx("say abbrev('PRINT', 'PR', 2)"), "1");
}

// ── Word Functions ──────────────────────────────────────────────────

#[test]
fn bif_words_basic() {
    assert_eq!(run_rexx("say words('one two three')"), "3");
}

#[test]
fn bif_words_empty() {
    assert_eq!(run_rexx("say words('')"), "0");
}

#[test]
fn bif_word_basic() {
    assert_eq!(run_rexx("say word('one two three', 2)"), "two");
}

#[test]
fn bif_word_out_of_range() {
    assert_eq!(run_rexx("say word('one two', 5)"), "");
}

#[test]
fn bif_wordindex_basic() {
    assert_eq!(run_rexx("say wordindex('  one two three', 2)"), "7");
}

#[test]
fn bif_wordlength_basic() {
    assert_eq!(run_rexx("say wordlength('one two three', 2)"), "3");
}

#[test]
fn bif_subword_basic() {
    assert_eq!(
        run_rexx("say subword('one two three four', 2, 2)"),
        "two three"
    );
}

#[test]
fn bif_subword_to_end() {
    assert_eq!(
        run_rexx("say subword('one two three four', 3)"),
        "three four"
    );
}

#[test]
fn bif_wordpos_basic() {
    assert_eq!(
        run_rexx("say wordpos('two three', 'one two three four')"),
        "2"
    );
}

#[test]
fn bif_wordpos_not_found() {
    assert_eq!(run_rexx("say wordpos('five', 'one two three')"), "0");
}

#[test]
fn bif_delword_basic() {
    assert_eq!(
        run_rexx("say delword('one two three four', 2, 2)"),
        "one four"
    );
}

#[test]
fn bif_delword_to_end() {
    assert_eq!(run_rexx("say delword('one two three four', 3)"), "one two");
}

// ── Numeric Functions ───────────────────────────────────────────────

#[test]
fn bif_abs_positive() {
    assert_eq!(run_rexx("say abs(42)"), "42");
}

#[test]
fn bif_abs_negative() {
    assert_eq!(run_rexx("say abs(-42)"), "42");
}

#[test]
fn bif_sign_positive() {
    assert_eq!(run_rexx("say sign(42)"), "1");
}

#[test]
fn bif_sign_negative() {
    assert_eq!(run_rexx("say sign(-42)"), "-1");
}

#[test]
fn bif_sign_zero() {
    assert_eq!(run_rexx("say sign(0)"), "0");
}

#[test]
fn bif_max_basic() {
    assert_eq!(run_rexx("say max(1, 5, 3)"), "5");
}

#[test]
fn bif_min_basic() {
    assert_eq!(run_rexx("say min(1, 5, 3)"), "1");
}

#[test]
fn bif_trunc_basic() {
    assert_eq!(run_rexx("say trunc(3.75)"), "3");
}

#[test]
fn bif_trunc_with_decimals() {
    assert_eq!(run_rexx("say trunc(3.75, 1)"), "3.7");
}

#[test]
fn bif_format_basic() {
    // Use quotes to preserve leading space since run_rexx trims output
    assert_eq!(run_rexx("say '>'format(3.14, 2, 3)'<'"), "> 3.140<");
}

#[test]
fn bif_random_range() {
    // RANDOM returns a number — just check it doesn't error
    let result = run_rexx("say random(1, 10)");
    let n: u64 = result.parse().expect("should be a number");
    assert!((1..=10).contains(&n));
}

#[test]
fn bif_random_default() {
    // RANDOM() returns 0..999
    let result = run_rexx("say random()");
    let n: u64 = result.parse().expect("should be a number");
    assert!(n <= 999);
}

// ── Conversion Functions ────────────────────────────────────────────

#[test]
fn bif_c2x_basic() {
    assert_eq!(run_rexx("say c2x('AB')"), "4142");
}

#[test]
fn bif_x2c_basic() {
    assert_eq!(run_rexx("say x2c('4142')"), "AB");
}

#[test]
fn bif_d2x_basic() {
    assert_eq!(run_rexx("say d2x(255)"), "FF");
}

#[test]
fn bif_x2d_basic() {
    assert_eq!(run_rexx("say x2d('FF')"), "255");
}

#[test]
fn bif_d2c_basic() {
    assert_eq!(run_rexx("say c2x(d2c(65))"), "41");
}

#[test]
fn bif_c2d_basic() {
    assert_eq!(run_rexx("say c2d('A')"), "65");
}

#[test]
fn bif_b2x_basic() {
    assert_eq!(run_rexx("say b2x('11000011')"), "C3");
}

#[test]
fn bif_x2b_basic() {
    assert_eq!(run_rexx("say x2b('C3')"), "11000011");
}

#[test]
fn bif_d2x_with_length() {
    assert_eq!(run_rexx("say d2x(255, 4)"), "00FF");
}

#[test]
fn bif_x2d_signed() {
    assert_eq!(run_rexx("say x2d('FF', 2)"), "-1");
}

// ── Informational Functions ─────────────────────────────────────────

#[test]
fn bif_datatype_num() {
    assert_eq!(run_rexx("say datatype(42)"), "NUM");
}

#[test]
fn bif_datatype_char() {
    assert_eq!(run_rexx("say datatype('hello')"), "CHAR");
}

#[test]
fn bif_datatype_type_check() {
    assert_eq!(run_rexx("say datatype('hello', 'A')"), "1");
}

#[test]
fn bif_datatype_num_check() {
    assert_eq!(run_rexx("say datatype('42', 'N')"), "1");
}

#[test]
fn bif_datatype_hex_check() {
    assert_eq!(run_rexx("say datatype('1F', 'X')"), "1");
}

#[test]
fn bif_verify_basic() {
    assert_eq!(run_rexx("say verify('abc123', 'abcdefgh')"), "4");
}

#[test]
fn bif_verify_all_match() {
    assert_eq!(run_rexx("say verify('abc', 'abcdefgh')"), "0");
}

#[test]
fn bif_xrange_basic() {
    // XRANGE('a','f') should give 'abcdef'
    assert_eq!(run_rexx("say xrange('a', 'f')"), "abcdef");
}

// ── Date/Time Functions ─────────────────────────────────────────────

#[test]
fn bif_date_sorted() {
    // DATE('S') returns YYYYMMDD — just verify it's 8 digits
    let result = run_rexx("say date('S')");
    assert_eq!(result.len(), 8);
    assert!(result.chars().all(|c| c.is_ascii_digit()));
}

#[test]
fn bif_date_normal() {
    // DATE('N') returns "dd Mon yyyy"
    let result = run_rexx("say date('N')");
    assert!(result.contains(' '));
}

#[test]
fn bif_date_days_of_year() {
    // DATE('D') returns day number 1-366
    let result = run_rexx("say date('D')");
    let n: u32 = result.parse().expect("should be a number");
    assert!((1..=366).contains(&n));
}

#[test]
fn bif_date_weekday() {
    // DATE('W') returns a weekday name
    let result = run_rexx("say date('W')");
    let valid = [
        "Monday",
        "Tuesday",
        "Wednesday",
        "Thursday",
        "Friday",
        "Saturday",
        "Sunday",
    ];
    assert!(valid.contains(&result.as_str()), "got '{result}'");
}

#[test]
fn bif_time_normal() {
    // TIME('N') returns HH:MM:SS
    let result = run_rexx("say time('N')");
    assert_eq!(result.len(), 8);
    assert_eq!(&result[2..3], ":");
    assert_eq!(&result[5..6], ":");
}

#[test]
fn bif_time_hours() {
    let result = run_rexx("say time('H')");
    let n: u32 = result.parse().expect("should be a number");
    assert!(n < 24);
}

#[test]
fn bif_time_seconds() {
    let result = run_rexx("say time('S')");
    let n: u32 = result.parse().expect("should be a number");
    assert!(n < 86400);
}

// ── Error Paths ─────────────────────────────────────────────────────

#[test]
fn bif_wrong_arg_count() {
    let err = run_rexx_fail("say length('a', 'b')");
    assert!(err.contains("Error 40"), "got: {err}");
}

#[test]
fn bif_non_numeric_arg() {
    let err = run_rexx_fail("say abs('hello')");
    assert!(err.contains("Error 41"), "got: {err}");
}

#[test]
fn bif_routine_not_found() {
    let err = run_rexx_fail("say notafunction('x')");
    assert!(err.contains("Error 43"), "got: {err}");
}

// ── Resolution Order ────────────────────────────────────────────────

#[test]
fn internal_label_overrides_bif() {
    // Internal label named LENGTH should override the BIF
    let result = run_rexx("say length('hello'); exit; length: ; return 999");
    assert_eq!(result, "999");
}

#[test]
fn call_bif_sets_result() {
    let result = run_rexx("call length 'hello'; say result");
    assert_eq!(result, "5");
}

#[test]
fn call_bif_abs_sets_result() {
    let result = run_rexx("call abs -42; say result");
    assert_eq!(result, "42");
}
