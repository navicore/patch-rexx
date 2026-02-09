//! REXX built-in functions (BIFs).
//!
//! Implements ~50 standard REXX built-in functions covering string manipulation,
//! word processing, numeric operations, conversions, informational queries,
//! and date/time.

use crate::error::{RexxDiagnostic, RexxError, RexxResult};
use crate::value::{NumericSettings, RexxValue};
use bigdecimal::BigDecimal;
use rand::Rng;
use std::fmt::Write;

// ── Public dispatch ─────────────────────────────────────────────────

/// Try to call a built-in function by name.
///
/// Returns:
/// - `None` — not a BIF (caller should try next resolution step)
/// - `Some(Ok(val))` — BIF succeeded
/// - `Some(Err(..))` — BIF found but call was invalid
pub fn call_builtin(
    name: &str,
    args: &[RexxValue],
    settings: &NumericSettings,
) -> Option<RexxResult<RexxValue>> {
    let result = match name {
        // String functions
        "LENGTH" => bif_length(args),
        "SUBSTR" => bif_substr(args),
        "LEFT" => bif_left(args),
        "RIGHT" => bif_right(args),
        "POS" => bif_pos(args),
        "LASTPOS" => bif_lastpos(args),
        "INDEX" => bif_index(args),
        "COPIES" => bif_copies(args),
        "REVERSE" => bif_reverse(args),
        "STRIP" => bif_strip(args),
        "SPACE" => bif_space(args),
        "OVERLAY" => bif_overlay(args),
        "INSERT" => bif_insert(args),
        "DELSTR" => bif_delstr(args),
        "TRANSLATE" => bif_translate(args),
        "CHANGESTR" => bif_changestr(args),
        "COUNTSTR" => bif_countstr(args),
        "COMPARE" => bif_compare(args),
        "ABBREV" => bif_abbrev(args),

        // Word functions
        "WORDS" => bif_words(args),
        "WORD" => bif_word(args),
        "WORDINDEX" => bif_wordindex(args),
        "WORDLENGTH" => bif_wordlength(args),
        "SUBWORD" => bif_subword(args),
        "WORDPOS" => bif_wordpos(args),
        "DELWORD" => bif_delword(args),

        // Numeric functions
        "ABS" => bif_abs(args),
        "SIGN" => bif_sign(args),
        "MAX" => bif_max(args),
        "MIN" => bif_min(args),
        "TRUNC" => bif_trunc(args),
        "FORMAT" => bif_format(args, settings),
        "RANDOM" => bif_random(args),

        // Conversion functions
        "D2C" => bif_d2c(args),
        "C2D" => bif_c2d(args),
        "D2X" => bif_d2x(args),
        "X2D" => bif_x2d(args),
        "C2X" => bif_c2x(args),
        "X2C" => bif_x2c(args),
        "B2X" => bif_b2x(args),
        "X2B" => bif_x2b(args),

        // Informational functions
        "DATATYPE" => bif_datatype(args),
        "VERIFY" => bif_verify(args),
        "XRANGE" => bif_xrange(args),

        // Date/Time
        "DATE" => bif_date(args),
        "TIME" => bif_time(args),

        _ => return None,
    };
    Some(result)
}

// ── Argument validation helpers ─────────────────────────────────────

fn check_args(name: &str, args: &[RexxValue], min: usize, max: usize) -> RexxResult<()> {
    if args.len() < min || args.len() > max {
        Err(
            RexxDiagnostic::new(RexxError::IncorrectCall).with_detail(format!(
                "{name} requires {min} to {max} arguments; got {}",
                args.len()
            )),
        )
    } else {
        Ok(())
    }
}

fn to_whole_number(name: &str, val: &RexxValue) -> RexxResult<i64> {
    let s = val.as_str().trim();
    s.parse::<i64>().map_err(|_| {
        RexxDiagnostic::new(RexxError::IncorrectCall)
            .with_detail(format!("{name}: '{s}' is not a valid whole number"))
    })
}

#[allow(clippy::cast_possible_truncation)]
fn to_nonneg_whole(name: &str, val: &RexxValue) -> RexxResult<usize> {
    let n = to_whole_number(name, val)?;
    if n < 0 {
        return Err(RexxDiagnostic::new(RexxError::IncorrectCall)
            .with_detail(format!("{name}: value must not be negative; got {n}")));
    }
    #[allow(clippy::cast_sign_loss)]
    Ok(n as usize)
}

fn to_positive_whole(name: &str, val: &RexxValue) -> RexxResult<usize> {
    let n = to_nonneg_whole(name, val)?;
    if n == 0 {
        return Err(RexxDiagnostic::new(RexxError::IncorrectCall)
            .with_detail(format!("{name}: value must be positive; got 0")));
    }
    Ok(n)
}

fn to_pad_char(name: &str, val: &RexxValue) -> RexxResult<char> {
    let s = val.as_str();
    let mut chars = s.chars();
    match (chars.next(), chars.next()) {
        (Some(c), None) => Ok(c),
        _ => Err(RexxDiagnostic::new(RexxError::IncorrectCall)
            .with_detail(format!("{name}: pad must be exactly one character"))),
    }
}

fn to_number_val(name: &str, val: &RexxValue) -> RexxResult<BigDecimal> {
    val.to_decimal().ok_or_else(|| {
        RexxDiagnostic::new(RexxError::BadArithmetic)
            .with_detail(format!("{name}: '{}' is not a number", val.as_str()))
    })
}

// ── Word utility ────────────────────────────────────────────────────

fn rexx_words(s: &str) -> Vec<&str> {
    s.split([' ', '\t']).filter(|w| !w.is_empty()).collect()
}

// ── String character helpers ────────────────────────────────────────

fn char_len(s: &str) -> usize {
    s.chars().count()
}

/// Get a substring by character positions (0-based start, count).
fn substr_chars(s: &str, start: usize, count: usize) -> String {
    s.chars().skip(start).take(count).collect()
}

// ── String BIFs ─────────────────────────────────────────────────────

fn bif_length(args: &[RexxValue]) -> RexxResult<RexxValue> {
    check_args("LENGTH", args, 1, 1)?;
    Ok(RexxValue::new(char_len(args[0].as_str()).to_string()))
}

fn bif_substr(args: &[RexxValue]) -> RexxResult<RexxValue> {
    check_args("SUBSTR", args, 2, 4)?;
    let s = args[0].as_str();
    let start = to_positive_whole("SUBSTR", &args[1])?;
    let slen = char_len(s);
    let length = if args.len() >= 3 {
        to_nonneg_whole("SUBSTR", &args[2])?
    } else {
        slen.saturating_sub(start - 1)
    };
    let pad = if args.len() >= 4 {
        to_pad_char("SUBSTR", &args[3])?
    } else {
        ' '
    };

    let start0 = start - 1; // convert to 0-based
    let mut result = substr_chars(s, start0, length);
    let got = char_len(&result);
    if got < length {
        for _ in 0..(length - got) {
            result.push(pad);
        }
    }
    Ok(RexxValue::new(result))
}

fn bif_left(args: &[RexxValue]) -> RexxResult<RexxValue> {
    check_args("LEFT", args, 2, 3)?;
    let s = args[0].as_str();
    let length = to_nonneg_whole("LEFT", &args[1])?;
    let pad = if args.len() >= 3 {
        to_pad_char("LEFT", &args[2])?
    } else {
        ' '
    };
    let slen = char_len(s);
    if length <= slen {
        Ok(RexxValue::new(substr_chars(s, 0, length)))
    } else {
        let mut result: String = s.to_string();
        for _ in 0..(length - slen) {
            result.push(pad);
        }
        Ok(RexxValue::new(result))
    }
}

fn bif_right(args: &[RexxValue]) -> RexxResult<RexxValue> {
    check_args("RIGHT", args, 2, 3)?;
    let s = args[0].as_str();
    let length = to_nonneg_whole("RIGHT", &args[1])?;
    let pad = if args.len() >= 3 {
        to_pad_char("RIGHT", &args[2])?
    } else {
        ' '
    };
    let slen = char_len(s);
    if length <= slen {
        Ok(RexxValue::new(substr_chars(s, slen - length, length)))
    } else {
        let pad_count = length - slen;
        let mut result = String::with_capacity(length);
        for _ in 0..pad_count {
            result.push(pad);
        }
        result.push_str(s);
        Ok(RexxValue::new(result))
    }
}

fn bif_pos(args: &[RexxValue]) -> RexxResult<RexxValue> {
    check_args("POS", args, 2, 3)?;
    let needle = args[0].as_str();
    let haystack = args[1].as_str();
    let start = if args.len() >= 3 {
        to_positive_whole("POS", &args[2])?
    } else {
        1
    };
    if needle.is_empty() {
        return Ok(RexxValue::new("0"));
    }
    // Convert start to byte offset
    let start0 = start - 1;
    let byte_start = haystack
        .char_indices()
        .nth(start0)
        .map_or(haystack.len(), |(i, _)| i);
    match haystack[byte_start..].find(needle) {
        Some(byte_pos) => {
            let char_pos = haystack[..byte_start + byte_pos].chars().count() + 1;
            Ok(RexxValue::new(char_pos.to_string()))
        }
        None => Ok(RexxValue::new("0")),
    }
}

fn bif_lastpos(args: &[RexxValue]) -> RexxResult<RexxValue> {
    check_args("LASTPOS", args, 2, 3)?;
    let needle = args[0].as_str();
    let haystack = args[1].as_str();
    let start = if args.len() >= 3 {
        to_positive_whole("LASTPOS", &args[2])?
    } else {
        char_len(haystack)
    };
    if needle.is_empty() {
        return Ok(RexxValue::new("0"));
    }
    // Search from position 1 up to `start`
    let search_end = haystack
        .char_indices()
        .nth(start)
        .map_or(haystack.len(), |(i, _)| i);
    let search_area = &haystack[..search_end];
    match search_area.rfind(needle) {
        Some(byte_pos) => {
            let char_pos = haystack[..byte_pos].chars().count() + 1;
            Ok(RexxValue::new(char_pos.to_string()))
        }
        None => Ok(RexxValue::new("0")),
    }
}

fn bif_index(args: &[RexxValue]) -> RexxResult<RexxValue> {
    check_args("INDEX", args, 2, 3)?;
    // INDEX(haystack, needle [,start]) — like POS but args swapped
    let haystack_val = args[0].clone();
    let needle_val = args[1].clone();
    let mut new_args = vec![needle_val, haystack_val];
    if args.len() >= 3 {
        new_args.push(args[2].clone());
    }
    bif_pos(&new_args)
}

fn bif_copies(args: &[RexxValue]) -> RexxResult<RexxValue> {
    check_args("COPIES", args, 2, 2)?;
    let s = args[0].as_str();
    let n = to_nonneg_whole("COPIES", &args[1])?;
    Ok(RexxValue::new(s.repeat(n)))
}

fn bif_reverse(args: &[RexxValue]) -> RexxResult<RexxValue> {
    check_args("REVERSE", args, 1, 1)?;
    let reversed: String = args[0].as_str().chars().rev().collect();
    Ok(RexxValue::new(reversed))
}

fn bif_strip(args: &[RexxValue]) -> RexxResult<RexxValue> {
    check_args("STRIP", args, 1, 3)?;
    let s = args[0].as_str();
    let option = if args.len() >= 2 {
        args[1].as_str().to_uppercase()
    } else {
        "B".to_string()
    };
    let ch = if args.len() >= 3 {
        to_pad_char("STRIP", &args[2])?
    } else {
        ' '
    };

    let result = match option.as_str() {
        "B" | "BOTH" => s.trim_matches(ch).to_string(),
        "L" | "LEADING" => s.trim_start_matches(ch).to_string(),
        "T" | "TRAILING" => s.trim_end_matches(ch).to_string(),
        _ => {
            return Err(RexxDiagnostic::new(RexxError::IncorrectCall)
                .with_detail(format!("STRIP: option must be B, L, or T; got '{option}'")));
        }
    };
    Ok(RexxValue::new(result))
}

fn bif_space(args: &[RexxValue]) -> RexxResult<RexxValue> {
    check_args("SPACE", args, 1, 3)?;
    let s = args[0].as_str();
    let n = if args.len() >= 2 {
        to_nonneg_whole("SPACE", &args[1])?
    } else {
        1
    };
    let pad = if args.len() >= 3 {
        to_pad_char("SPACE", &args[2])?
    } else {
        ' '
    };
    let words = rexx_words(s);
    let separator: String = std::iter::repeat_n(pad, n).collect();
    Ok(RexxValue::new(words.join(&separator)))
}

fn bif_overlay(args: &[RexxValue]) -> RexxResult<RexxValue> {
    check_args("OVERLAY", args, 2, 5)?;
    let new = args[0].as_str();
    let target = args[1].as_str();
    let start = if args.len() >= 3 {
        to_positive_whole("OVERLAY", &args[2])?
    } else {
        1
    };
    let length = if args.len() >= 4 {
        to_nonneg_whole("OVERLAY", &args[3])?
    } else {
        char_len(new)
    };
    let pad = if args.len() >= 5 {
        to_pad_char("OVERLAY", &args[4])?
    } else {
        ' '
    };

    let tlen = char_len(target);
    let start0 = start - 1;

    // Build result: target[0..start0] + padded_new + target[start0+length..]
    let mut result = String::new();

    // Part before overlay
    if start0 <= tlen {
        result.push_str(&substr_chars(target, 0, start0));
    } else {
        result.push_str(target);
        for _ in 0..(start0 - tlen) {
            result.push(pad);
        }
    }

    // Overlay portion: pad or truncate `new` to `length`
    let new_len = char_len(new);
    if new_len >= length {
        result.push_str(&substr_chars(new, 0, length));
    } else {
        result.push_str(new);
        for _ in 0..(length - new_len) {
            result.push(pad);
        }
    }

    // Part after overlay
    let after = start0 + length;
    if after < tlen {
        result.push_str(&substr_chars(target, after, tlen - after));
    }

    Ok(RexxValue::new(result))
}

fn bif_insert(args: &[RexxValue]) -> RexxResult<RexxValue> {
    check_args("INSERT", args, 2, 5)?;
    let new = args[0].as_str();
    let target = args[1].as_str();
    let start = if args.len() >= 3 {
        to_nonneg_whole("INSERT", &args[2])?
    } else {
        0
    };
    let length = if args.len() >= 4 {
        to_nonneg_whole("INSERT", &args[3])?
    } else {
        char_len(new)
    };
    let pad = if args.len() >= 5 {
        to_pad_char("INSERT", &args[4])?
    } else {
        ' '
    };

    let tlen = char_len(target);
    let mut result = String::new();

    // Part before insertion point
    if start <= tlen {
        result.push_str(&substr_chars(target, 0, start));
    } else {
        result.push_str(target);
        for _ in 0..(start - tlen) {
            result.push(pad);
        }
    }

    // Insert portion: pad or truncate `new` to `length`
    let new_len = char_len(new);
    if new_len >= length {
        result.push_str(&substr_chars(new, 0, length));
    } else {
        result.push_str(new);
        for _ in 0..(length - new_len) {
            result.push(pad);
        }
    }

    // Rest of target
    if start < tlen {
        result.push_str(&substr_chars(target, start, tlen - start));
    }

    Ok(RexxValue::new(result))
}

fn bif_delstr(args: &[RexxValue]) -> RexxResult<RexxValue> {
    check_args("DELSTR", args, 2, 3)?;
    let s = args[0].as_str();
    let start = to_positive_whole("DELSTR", &args[1])?;
    let slen = char_len(s);
    let length = if args.len() >= 3 {
        to_nonneg_whole("DELSTR", &args[2])?
    } else {
        slen.saturating_sub(start - 1)
    };

    let start0 = start - 1;
    if start0 >= slen {
        return Ok(RexxValue::new(s));
    }

    let mut result = substr_chars(s, 0, start0);
    let after = start0 + length;
    if after < slen {
        result.push_str(&substr_chars(s, after, slen - after));
    }
    Ok(RexxValue::new(result))
}

#[allow(clippy::similar_names)]
fn bif_translate(args: &[RexxValue]) -> RexxResult<RexxValue> {
    check_args("TRANSLATE", args, 1, 4)?;
    let s = args[0].as_str();

    if args.len() == 1 {
        // No tables: uppercase
        return Ok(RexxValue::new(s.to_uppercase()));
    }

    let output_table = if args.len() >= 2 {
        args[1].as_str()
    } else {
        ""
    };
    let input_table = if args.len() >= 3 {
        args[2].as_str()
    } else {
        ""
    };
    let pad = if args.len() >= 4 {
        to_pad_char("TRANSLATE", &args[3])?
    } else {
        ' '
    };

    let out_chars: Vec<char> = output_table.chars().collect();
    let in_chars: Vec<char> = input_table.chars().collect();

    let result: String = s
        .chars()
        .map(|c| {
            if let Some(pos) = in_chars.iter().position(|&ic| ic == c) {
                if pos < out_chars.len() {
                    out_chars[pos]
                } else {
                    pad
                }
            } else {
                c
            }
        })
        .collect();

    Ok(RexxValue::new(result))
}

fn bif_changestr(args: &[RexxValue]) -> RexxResult<RexxValue> {
    check_args("CHANGESTR", args, 3, 3)?;
    let needle = args[0].as_str();
    let haystack = args[1].as_str();
    let new = args[2].as_str();
    if needle.is_empty() {
        return Ok(RexxValue::new(haystack));
    }
    Ok(RexxValue::new(haystack.replace(needle, new)))
}

fn bif_countstr(args: &[RexxValue]) -> RexxResult<RexxValue> {
    check_args("COUNTSTR", args, 2, 2)?;
    let needle = args[0].as_str();
    let haystack = args[1].as_str();
    if needle.is_empty() {
        return Ok(RexxValue::new("0"));
    }
    Ok(RexxValue::new(haystack.matches(needle).count().to_string()))
}

fn bif_compare(args: &[RexxValue]) -> RexxResult<RexxValue> {
    check_args("COMPARE", args, 2, 3)?;
    let s1 = args[0].as_str();
    let s2 = args[1].as_str();
    let pad = if args.len() >= 3 {
        to_pad_char("COMPARE", &args[2])?
    } else {
        ' '
    };

    let len = char_len(s1).max(char_len(s2));
    let c1: Vec<char> = s1
        .chars()
        .chain(std::iter::repeat_n(pad, len))
        .take(len)
        .collect();
    let c2: Vec<char> = s2
        .chars()
        .chain(std::iter::repeat_n(pad, len))
        .take(len)
        .collect();

    for (i, (a, b)) in c1.iter().zip(c2.iter()).enumerate() {
        if a != b {
            return Ok(RexxValue::new((i + 1).to_string()));
        }
    }
    Ok(RexxValue::new("0"))
}

fn bif_abbrev(args: &[RexxValue]) -> RexxResult<RexxValue> {
    check_args("ABBREV", args, 2, 3)?;
    let information = args[0].as_str();
    let info = args[1].as_str();
    let min_length = if args.len() >= 3 {
        to_nonneg_whole("ABBREV", &args[2])?
    } else {
        char_len(info)
    };

    let info_len = char_len(info);
    if info_len < min_length {
        return Ok(RexxValue::new("0"));
    }
    if information.starts_with(info) {
        Ok(RexxValue::new("1"))
    } else {
        Ok(RexxValue::new("0"))
    }
}

// ── Word BIFs ───────────────────────────────────────────────────────

fn bif_words(args: &[RexxValue]) -> RexxResult<RexxValue> {
    check_args("WORDS", args, 1, 1)?;
    Ok(RexxValue::new(
        rexx_words(args[0].as_str()).len().to_string(),
    ))
}

fn bif_word(args: &[RexxValue]) -> RexxResult<RexxValue> {
    check_args("WORD", args, 2, 2)?;
    let n = to_positive_whole("WORD", &args[1])?;
    let words = rexx_words(args[0].as_str());
    Ok(RexxValue::new(
        words.get(n - 1).copied().unwrap_or("").to_string(),
    ))
}

fn bif_wordindex(args: &[RexxValue]) -> RexxResult<RexxValue> {
    check_args("WORDINDEX", args, 2, 2)?;
    let s = args[0].as_str();
    let n = to_positive_whole("WORDINDEX", &args[1])?;

    let mut word_count = 0usize;
    let mut in_word = false;
    for (i, c) in s.chars().enumerate() {
        if c == ' ' || c == '\t' {
            in_word = false;
        } else if !in_word {
            in_word = true;
            word_count += 1;
            if word_count == n {
                return Ok(RexxValue::new((i + 1).to_string()));
            }
        }
    }
    Ok(RexxValue::new("0"))
}

fn bif_wordlength(args: &[RexxValue]) -> RexxResult<RexxValue> {
    check_args("WORDLENGTH", args, 2, 2)?;
    let n = to_positive_whole("WORDLENGTH", &args[1])?;
    let words = rexx_words(args[0].as_str());
    Ok(RexxValue::new(
        words.get(n - 1).map_or(0, |w| char_len(w)).to_string(),
    ))
}

fn bif_subword(args: &[RexxValue]) -> RexxResult<RexxValue> {
    check_args("SUBWORD", args, 2, 3)?;
    let s = args[0].as_str();
    let n = to_positive_whole("SUBWORD", &args[1])?;
    let words = rexx_words(s);
    let count = if args.len() >= 3 {
        to_nonneg_whole("SUBWORD", &args[2])?
    } else {
        words.len().saturating_sub(n - 1)
    };

    if n > words.len() {
        return Ok(RexxValue::new(""));
    }
    let end = (n - 1 + count).min(words.len());
    Ok(RexxValue::new(words[n - 1..end].join(" ")))
}

fn bif_wordpos(args: &[RexxValue]) -> RexxResult<RexxValue> {
    check_args("WORDPOS", args, 2, 3)?;
    let phrase_words = rexx_words(args[0].as_str());
    let string_words = rexx_words(args[1].as_str());
    let start = if args.len() >= 3 {
        to_positive_whole("WORDPOS", &args[2])?
    } else {
        1
    };

    if phrase_words.is_empty() || start > string_words.len() {
        return Ok(RexxValue::new("0"));
    }

    let start0 = start - 1;
    for i in start0..string_words.len() {
        if i + phrase_words.len() > string_words.len() {
            break;
        }
        let matched = phrase_words
            .iter()
            .enumerate()
            .all(|(j, pw)| pw.eq_ignore_ascii_case(string_words[i + j]));
        if matched {
            return Ok(RexxValue::new((i + 1).to_string()));
        }
    }
    Ok(RexxValue::new("0"))
}

fn bif_delword(args: &[RexxValue]) -> RexxResult<RexxValue> {
    check_args("DELWORD", args, 2, 3)?;
    let s = args[0].as_str();
    let n = to_positive_whole("DELWORD", &args[1])?;
    let words = rexx_words(s);
    let count = if args.len() >= 3 {
        to_nonneg_whole("DELWORD", &args[2])?
    } else {
        words.len().saturating_sub(n - 1)
    };

    if n > words.len() {
        return Ok(RexxValue::new(s));
    }

    // Find character positions of words to delete
    let mut word_positions: Vec<(usize, usize)> = Vec::new();
    let mut in_word = false;
    let mut word_start = 0;
    for (i, c) in s.chars().enumerate() {
        if c == ' ' || c == '\t' {
            if in_word {
                word_positions.push((word_start, i));
                in_word = false;
            }
        } else if !in_word {
            in_word = true;
            word_start = i;
        }
    }
    if in_word {
        word_positions.push((word_start, char_len(s)));
    }

    let del_start = n - 1;
    let del_end = (del_start + count).min(word_positions.len());

    if del_start >= word_positions.len() {
        return Ok(RexxValue::new(s));
    }

    let chars_vec: Vec<char> = s.chars().collect();
    let before_end = word_positions[del_start].0;
    let after_start = if del_end < word_positions.len() {
        word_positions[del_end].0
    } else {
        chars_vec.len()
    };

    let before: String = chars_vec[..before_end].iter().collect();
    let after: String = chars_vec[after_start..].iter().collect();

    // Trim trailing spaces from `before` if `after` is empty
    let result = if after.is_empty() {
        before.trim_end().to_string()
    } else {
        format!("{before}{after}")
    };

    Ok(RexxValue::new(result))
}

// ── Numeric BIFs ────────────────────────────────────────────────────

fn bif_abs(args: &[RexxValue]) -> RexxResult<RexxValue> {
    check_args("ABS", args, 1, 1)?;
    let d = to_number_val("ABS", &args[0])?;
    let result = d.abs();
    Ok(RexxValue::new(format_number_plain(&result)))
}

fn bif_sign(args: &[RexxValue]) -> RexxResult<RexxValue> {
    check_args("SIGN", args, 1, 1)?;
    let d = to_number_val("SIGN", &args[0])?;
    let zero = BigDecimal::from(0);
    let result = match d.cmp(&zero) {
        std::cmp::Ordering::Greater => "1",
        std::cmp::Ordering::Less => "-1",
        std::cmp::Ordering::Equal => "0",
    };
    Ok(RexxValue::new(result))
}

fn bif_max(args: &[RexxValue]) -> RexxResult<RexxValue> {
    if args.is_empty() {
        return Err(RexxDiagnostic::new(RexxError::IncorrectCall)
            .with_detail("MAX requires at least 1 argument"));
    }
    let mut max = to_number_val("MAX", &args[0])?;
    for arg in &args[1..] {
        let d = to_number_val("MAX", arg)?;
        if d > max {
            max = d;
        }
    }
    Ok(RexxValue::new(format_number_plain(&max)))
}

fn bif_min(args: &[RexxValue]) -> RexxResult<RexxValue> {
    if args.is_empty() {
        return Err(RexxDiagnostic::new(RexxError::IncorrectCall)
            .with_detail("MIN requires at least 1 argument"));
    }
    let mut min = to_number_val("MIN", &args[0])?;
    for arg in &args[1..] {
        let d = to_number_val("MIN", arg)?;
        if d < min {
            min = d;
        }
    }
    Ok(RexxValue::new(format_number_plain(&min)))
}

fn bif_trunc(args: &[RexxValue]) -> RexxResult<RexxValue> {
    check_args("TRUNC", args, 1, 2)?;
    let d = to_number_val("TRUNC", &args[0])?;
    let n = if args.len() >= 2 {
        to_nonneg_whole("TRUNC", &args[1])?
    } else {
        0
    };

    #[allow(clippy::cast_possible_wrap)]
    let truncated = d.with_scale_round(n as i64, bigdecimal::RoundingMode::Down);
    // Format with exactly n decimal places
    if n == 0 {
        let s = truncated.to_string();
        if let Some(dot) = s.find('.') {
            Ok(RexxValue::new(&s[..dot]))
        } else {
            Ok(RexxValue::new(s))
        }
    } else {
        let s = truncated.to_string();
        if let Some(dot) = s.find('.') {
            let decimals = s.len() - dot - 1;
            if decimals < n {
                let mut padded = s;
                for _ in 0..(n - decimals) {
                    padded.push('0');
                }
                Ok(RexxValue::new(padded))
            } else {
                Ok(RexxValue::new(s))
            }
        } else {
            let mut result = s;
            result.push('.');
            for _ in 0..n {
                result.push('0');
            }
            Ok(RexxValue::new(result))
        }
    }
}

fn bif_format(args: &[RexxValue], settings: &NumericSettings) -> RexxResult<RexxValue> {
    check_args("FORMAT", args, 1, 5)?;
    let d = to_number_val("FORMAT", &args[0])?;
    let before = if args.len() >= 2 && !args[1].as_str().trim().is_empty() {
        Some(to_nonneg_whole("FORMAT", &args[1])?)
    } else {
        None
    };
    let after = if args.len() >= 3 && !args[2].as_str().trim().is_empty() {
        Some(to_nonneg_whole("FORMAT", &args[2])?)
    } else {
        None
    };

    // Format the number
    let formatted = RexxValue::from_decimal(&d, settings.digits, settings.form);
    let s = formatted.as_str().to_string();

    // Split into integer and decimal parts
    let (sign, abs_str) = s
        .strip_prefix('-')
        .map_or(("", s.as_str()), |rest| ("-", rest));

    let (int_part, dec_part) = if let Some(dot) = abs_str.find('.') {
        (&abs_str[..dot], Some(&abs_str[dot + 1..]))
    } else {
        (abs_str, None)
    };

    // Apply `after` — decimal places
    let dec_str = match after {
        Some(n) => {
            let current = dec_part.unwrap_or("");
            let cur_len = current.len();
            if cur_len >= n {
                if n == 0 {
                    String::new()
                } else {
                    current[..n].to_string()
                }
            } else {
                let mut padded = current.to_string();
                for _ in 0..(n - cur_len) {
                    padded.push('0');
                }
                padded
            }
        }
        None => dec_part.unwrap_or("").to_string(),
    };

    // Build the number without leading-space padding
    let number_str = if dec_str.is_empty() {
        format!("{sign}{int_part}")
    } else {
        format!("{sign}{int_part}.{dec_str}")
    };

    // Apply `before` — integer part width (includes sign)
    match before {
        Some(width) => {
            let int_with_sign = format!("{sign}{int_part}");
            let int_len = int_with_sign.len();
            if int_len > width {
                Err(
                    RexxDiagnostic::new(RexxError::IncorrectCall).with_detail(format!(
                        "FORMAT: integer part '{int_with_sign}' exceeds width {width}"
                    )),
                )
            } else {
                let padding = width - int_len;
                let padded = if dec_str.is_empty() {
                    format!("{:>width$}", number_str, width = number_str.len() + padding)
                } else {
                    format!(
                        "{:>width$}.{dec_str}",
                        int_with_sign,
                        width = int_len + padding
                    )
                };
                Ok(RexxValue::new(padded))
            }
        }
        None => Ok(RexxValue::new(number_str)),
    }
}

#[allow(clippy::cast_possible_truncation)]
fn bif_random(args: &[RexxValue]) -> RexxResult<RexxValue> {
    check_args("RANDOM", args, 0, 3)?;

    let min_val = if !args.is_empty() && !args[0].as_str().trim().is_empty() {
        to_nonneg_whole("RANDOM", &args[0])? as u64
    } else {
        0
    };
    let max_val = if args.len() >= 2 && !args[1].as_str().trim().is_empty() {
        to_nonneg_whole("RANDOM", &args[1])? as u64
    } else if args.len() == 1 {
        // RANDOM(max) — range is 0..max
        return bif_random_range(0, min_val);
    } else {
        999
    };

    bif_random_range(min_val, max_val)
}

fn bif_random_range(min: u64, max: u64) -> RexxResult<RexxValue> {
    if min > max {
        return Err(RexxDiagnostic::new(RexxError::IncorrectCall)
            .with_detail(format!("RANDOM: min ({min}) must not exceed max ({max})")));
    }
    let mut rng = rand::rng();
    let val = rng.random_range(min..=max);
    Ok(RexxValue::new(val.to_string()))
}

/// Format a `BigDecimal` as a plain number string (no exponential notation).
fn format_number_plain(d: &BigDecimal) -> String {
    let s = d.normalized().to_string();
    // BigDecimal may produce exponential notation for large/small numbers
    if s.contains('E') || s.contains('e') {
        let rv = RexxValue::from_decimal(d, 9, crate::value::NumericForm::Scientific);
        rv.into_string()
    } else {
        s
    }
}

// ── Conversion BIFs ─────────────────────────────────────────────────

fn bif_d2c(args: &[RexxValue]) -> RexxResult<RexxValue> {
    check_args("D2C", args, 1, 2)?;
    let n = to_whole_number("D2C", &args[0])?;
    if args.len() >= 2 {
        let length = to_nonneg_whole("D2C", &args[1])?;
        #[allow(clippy::cast_sign_loss)]
        let mut bytes = to_be_bytes(n as u64);
        if n >= 0 {
            pad_or_truncate_bytes(&mut bytes, length);
        } else {
            pad_or_truncate_bytes_signed(&mut bytes, length);
        }
        let s: String = bytes.into_iter().map(|b| b as char).collect();
        Ok(RexxValue::new(s))
    } else if n < 0 {
        Err(RexxDiagnostic::new(RexxError::IncorrectCall)
            .with_detail("D2C: negative value requires length argument"))
    } else {
        #[allow(clippy::cast_sign_loss)]
        let n = n as u64;
        if n > 255 {
            let bytes = to_be_bytes(n);
            let s: String = bytes.into_iter().map(|b| b as char).collect();
            Ok(RexxValue::new(s))
        } else {
            #[allow(clippy::cast_possible_truncation)]
            let c = n as u8 as char;
            Ok(RexxValue::new(c.to_string()))
        }
    }
}

fn bif_c2d(args: &[RexxValue]) -> RexxResult<RexxValue> {
    check_args("C2D", args, 1, 2)?;
    let s = args[0].as_str();
    if s.is_empty() {
        return Ok(RexxValue::new("0"));
    }
    if args.len() >= 2 {
        let length = to_nonneg_whole("C2D", &args[1])?;
        if length == 0 {
            return Ok(RexxValue::new("0"));
        }
        let bytes: Vec<u8> = s.bytes().collect();
        let start = bytes.len().saturating_sub(length);
        let relevant = &bytes[start..];
        let mut padded = vec![0u8; length.saturating_sub(relevant.len())];
        padded.extend_from_slice(relevant);

        if padded[0] & 0x80 != 0 {
            let mut val: i128 = 0;
            for &b in &padded {
                val = (val << 8) | i128::from(b);
            }
            let modulus = 1i128 << (length * 8);
            val -= modulus;
            Ok(RexxValue::new(val.to_string()))
        } else {
            let mut val: u128 = 0;
            for &b in &padded {
                val = (val << 8) | u128::from(b);
            }
            Ok(RexxValue::new(val.to_string()))
        }
    } else {
        let mut val: u128 = 0;
        for b in s.bytes() {
            val = (val << 8) | u128::from(b);
        }
        Ok(RexxValue::new(val.to_string()))
    }
}

fn bif_d2x(args: &[RexxValue]) -> RexxResult<RexxValue> {
    check_args("D2X", args, 1, 2)?;
    let n = to_whole_number("D2X", &args[0])?;
    if args.len() >= 2 {
        let length = to_nonneg_whole("D2X", &args[1])?;
        #[allow(clippy::cast_sign_loss)]
        let hex = format!("{:X}", n as u64);
        if hex.len() >= length {
            Ok(RexxValue::new(&hex[hex.len() - length..]))
        } else {
            let pad_char = if n >= 0 { '0' } else { 'F' };
            let mut result = String::new();
            for _ in 0..(length - hex.len()) {
                result.push(pad_char);
            }
            result.push_str(&hex);
            Ok(RexxValue::new(result))
        }
    } else if n < 0 {
        Err(RexxDiagnostic::new(RexxError::IncorrectCall)
            .with_detail("D2X: negative value requires length argument"))
    } else {
        #[allow(clippy::cast_sign_loss)]
        Ok(RexxValue::new(format!("{:X}", n as u64)))
    }
}

fn bif_x2d(args: &[RexxValue]) -> RexxResult<RexxValue> {
    check_args("X2D", args, 1, 2)?;
    let hex = args[0].as_str().trim();
    if hex.is_empty() {
        return Ok(RexxValue::new("0"));
    }
    if !hex.chars().all(|c| c.is_ascii_hexdigit()) {
        return Err(RexxDiagnostic::new(RexxError::IncorrectCall)
            .with_detail(format!("X2D: '{hex}' is not valid hexadecimal")));
    }
    if args.len() >= 2 {
        let length = to_nonneg_whole("X2D", &args[1])?;
        if length == 0 {
            return Ok(RexxValue::new("0"));
        }
        let effective = if hex.len() > length {
            &hex[hex.len() - length..]
        } else {
            hex
        };
        let val = u64::from_str_radix(effective, 16).map_err(|_| {
            RexxDiagnostic::new(RexxError::IncorrectCall)
                .with_detail(format!("X2D: cannot convert '{effective}'"))
        })?;
        let bits = length * 4;
        if bits < 64 && val >= (1u64 << (bits - 1)) {
            let modulus = 1u128 << bits;
            let signed = i128::from(val) - i128::try_from(modulus).unwrap_or(i128::MAX);
            Ok(RexxValue::new(signed.to_string()))
        } else {
            Ok(RexxValue::new(val.to_string()))
        }
    } else {
        let val = u64::from_str_radix(hex, 16).map_err(|_| {
            RexxDiagnostic::new(RexxError::IncorrectCall)
                .with_detail(format!("X2D: cannot convert '{hex}'"))
        })?;
        Ok(RexxValue::new(val.to_string()))
    }
}

fn bif_c2x(args: &[RexxValue]) -> RexxResult<RexxValue> {
    check_args("C2X", args, 1, 1)?;
    let s = args[0].as_str();
    let mut hex = String::with_capacity(s.len() * 2);
    for b in s.bytes() {
        let _ = write!(hex, "{b:02X}");
    }
    Ok(RexxValue::new(hex))
}

fn bif_x2c(args: &[RexxValue]) -> RexxResult<RexxValue> {
    check_args("X2C", args, 1, 1)?;
    let hex = args[0].as_str().replace(' ', "");
    if hex.is_empty() {
        return Ok(RexxValue::new(""));
    }
    if !hex.chars().all(|c| c.is_ascii_hexdigit()) {
        return Err(RexxDiagnostic::new(RexxError::IncorrectCall)
            .with_detail(format!("X2C: '{hex}' is not valid hexadecimal")));
    }
    // Pad to even length
    let padded = if hex.len().is_multiple_of(2) {
        hex
    } else {
        format!("0{hex}")
    };
    let mut result = String::new();
    let mut i = 0;
    while i < padded.len() {
        let byte_val = u8::from_str_radix(&padded[i..i + 2], 16).map_err(|_| {
            RexxDiagnostic::new(RexxError::IncorrectCall)
                .with_detail(format!("X2C: invalid hex pair '{}'", &padded[i..i + 2]))
        })?;
        result.push(byte_val as char);
        i += 2;
    }
    Ok(RexxValue::new(result))
}

fn bif_b2x(args: &[RexxValue]) -> RexxResult<RexxValue> {
    check_args("B2X", args, 1, 1)?;
    let bin = args[0].as_str().replace(' ', "");
    if bin.is_empty() {
        return Ok(RexxValue::new(""));
    }
    if !bin.chars().all(|c| c == '0' || c == '1') {
        return Err(RexxDiagnostic::new(RexxError::IncorrectCall)
            .with_detail(format!("B2X: '{bin}' is not valid binary")));
    }
    // Pad to multiple of 4
    let pad_len = (4 - bin.len() % 4) % 4;
    let padded = format!("{}{bin}", "0".repeat(pad_len));
    let mut result = String::new();
    let mut i = 0;
    while i < padded.len() {
        let nibble = u8::from_str_radix(&padded[i..i + 4], 2).unwrap_or(0);
        let _ = write!(result, "{nibble:X}");
        i += 4;
    }
    Ok(RexxValue::new(result))
}

fn bif_x2b(args: &[RexxValue]) -> RexxResult<RexxValue> {
    check_args("X2B", args, 1, 1)?;
    let hex = args[0].as_str().replace(' ', "");
    if hex.is_empty() {
        return Ok(RexxValue::new(""));
    }
    if !hex.chars().all(|c| c.is_ascii_hexdigit()) {
        return Err(RexxDiagnostic::new(RexxError::IncorrectCall)
            .with_detail(format!("X2B: '{hex}' is not valid hexadecimal")));
    }
    let mut result = String::with_capacity(hex.len() * 4);
    for c in hex.chars() {
        let val = c.to_digit(16).unwrap_or(0);
        let _ = write!(result, "{val:04b}");
    }
    Ok(RexxValue::new(result))
}

// Byte conversion helpers
fn to_be_bytes(n: u64) -> Vec<u8> {
    let bytes = n.to_be_bytes();
    let first_nonzero = bytes.iter().position(|&b| b != 0).unwrap_or(7);
    bytes[first_nonzero..].to_vec()
}

fn pad_or_truncate_bytes(v: &mut Vec<u8>, length: usize) {
    if v.len() > length {
        let start = v.len() - length;
        *v = v[start..].to_vec();
    } else {
        while v.len() < length {
            v.insert(0, 0);
        }
    }
}

fn pad_or_truncate_bytes_signed(v: &mut Vec<u8>, length: usize) {
    if v.len() > length {
        let start = v.len() - length;
        *v = v[start..].to_vec();
    } else {
        while v.len() < length {
            v.insert(0, 0xFF);
        }
    }
}

// ── Informational BIFs ──────────────────────────────────────────────

fn bif_datatype(args: &[RexxValue]) -> RexxResult<RexxValue> {
    check_args("DATATYPE", args, 1, 2)?;
    let s = args[0].as_str();

    if args.len() == 1 {
        let rv = RexxValue::new(s);
        return Ok(RexxValue::new(if rv.is_number() { "NUM" } else { "CHAR" }));
    }

    let type_char = args[1].as_str().to_uppercase();
    let result = match type_char.as_str() {
        "A" | "ALPHANUMERIC" => !s.is_empty() && s.chars().all(char::is_alphanumeric),
        "B" | "BINARY" => !s.is_empty() && s.chars().all(|c| c == '0' || c == '1' || c == ' '),
        "L" | "LOWERCASE" => !s.is_empty() && s.chars().all(char::is_lowercase),
        "M" | "MIXED" => !s.is_empty() && s.chars().all(char::is_alphabetic),
        "N" | "NUMBER" => RexxValue::new(s).is_number(),
        "S" | "SYMBOL" => {
            !s.is_empty()
                && s.chars()
                    .all(|c| c.is_alphanumeric() || c == '_' || c == '.' || c == '!' || c == '?')
        }
        "U" | "UPPERCASE" => !s.is_empty() && s.chars().all(char::is_uppercase),
        "W" | "WHOLENUMBER" => RexxValue::new(s).is_whole_number(9),
        "X" | "HEXADECIMAL" => s.is_empty() || s.chars().all(|c| c.is_ascii_hexdigit() || c == ' '),
        _ => {
            return Err(RexxDiagnostic::new(RexxError::IncorrectCall)
                .with_detail(format!("DATATYPE: unknown type '{type_char}'")));
        }
    };
    Ok(RexxValue::new(if result { "1" } else { "0" }))
}

fn bif_verify(args: &[RexxValue]) -> RexxResult<RexxValue> {
    check_args("VERIFY", args, 2, 4)?;
    let s = args[0].as_str();
    let reference = args[1].as_str();
    let option = if args.len() >= 3 {
        args[2].as_str().to_uppercase()
    } else {
        "N".to_string()
    };
    let start = if args.len() >= 4 {
        to_positive_whole("VERIFY", &args[3])?
    } else {
        1
    };

    let nomatch = match option.as_str() {
        "N" | "NOMATCH" => true,
        "M" | "MATCH" => false,
        _ => {
            return Err(RexxDiagnostic::new(RexxError::IncorrectCall)
                .with_detail(format!("VERIFY: option must be N or M; got '{option}'")));
        }
    };

    let ref_chars: Vec<char> = reference.chars().collect();

    for (i, c) in s.chars().enumerate().skip(start - 1) {
        let in_ref = ref_chars.contains(&c);
        if (nomatch && !in_ref) || (!nomatch && in_ref) {
            return Ok(RexxValue::new((i + 1).to_string()));
        }
    }
    Ok(RexxValue::new("0"))
}

fn bif_xrange(args: &[RexxValue]) -> RexxResult<RexxValue> {
    check_args("XRANGE", args, 0, 2)?;
    let start: u8 = if !args.is_empty() && !args[0].as_str().is_empty() {
        let s = args[0].as_str();
        if s.len() == 1 {
            s.as_bytes()[0]
        } else {
            return Err(RexxDiagnostic::new(RexxError::IncorrectCall)
                .with_detail("XRANGE: start must be a single character"));
        }
    } else {
        0x00
    };
    let end: u8 = if args.len() >= 2 && !args[1].as_str().is_empty() {
        let s = args[1].as_str();
        if s.len() == 1 {
            s.as_bytes()[0]
        } else {
            return Err(RexxDiagnostic::new(RexxError::IncorrectCall)
                .with_detail("XRANGE: end must be a single character"));
        }
    } else {
        0xFF
    };

    let mut result = String::new();
    if start <= end {
        for b in start..=end {
            result.push(b as char);
        }
    } else {
        for b in start..=0xFF {
            result.push(b as char);
        }
        for b in 0x00..=end {
            result.push(b as char);
        }
    }
    Ok(RexxValue::new(result))
}

// ── Date/Time BIFs ──────────────────────────────────────────────────

fn bif_date(args: &[RexxValue]) -> RexxResult<RexxValue> {
    check_args("DATE", args, 0, 1)?;
    let option = if args.is_empty() {
        "N".to_string()
    } else {
        args[0].as_str().to_uppercase()
    };

    let now = current_date();

    let result = match option.as_str() {
        "N" | "NORMAL" => {
            let month_names = [
                "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
            ];
            format!(
                "{} {} {}",
                now.day,
                month_names[now.month as usize - 1],
                now.year
            )
        }
        "B" | "BASEDATE" => days_since_base(now.year, now.month, now.day).to_string(),
        "C" | "CENTURY" => {
            let base = days_since_base(now.year, now.month, now.day);
            let century_start = days_since_base(now.year - now.year % 100, 1, 1);
            (base - century_start).to_string()
        }
        "D" | "DAYS" => day_of_year(now.year, now.month, now.day).to_string(),
        "E" | "EUROPEAN" => format!("{:02}/{:02}/{:02}", now.day, now.month, now.year % 100),
        "J" | "JULIAN" => format!(
            "{}{:03}",
            now.year % 100,
            day_of_year(now.year, now.month, now.day)
        ),
        "M" | "MONTH" => {
            let month_names = [
                "January",
                "February",
                "March",
                "April",
                "May",
                "June",
                "July",
                "August",
                "September",
                "October",
                "November",
                "December",
            ];
            month_names[now.month as usize - 1].to_string()
        }
        "O" | "ORDERED" => format!("{:02}/{:02}/{:02}", now.year % 100, now.month, now.day),
        "S" | "SORTED" | "STANDARD" => format!("{}{:02}{:02}", now.year, now.month, now.day),
        "U" | "USA" => format!("{:02}/{:02}/{:02}", now.month, now.day, now.year % 100),
        "W" | "WEEKDAY" => {
            let day_names = [
                "Monday",
                "Tuesday",
                "Wednesday",
                "Thursday",
                "Friday",
                "Saturday",
                "Sunday",
            ];
            let base = days_since_base(now.year, now.month, now.day);
            #[allow(clippy::cast_sign_loss)]
            let weekday = (base % 7) as usize;
            day_names[weekday].to_string()
        }
        _ => {
            return Err(RexxDiagnostic::new(RexxError::IncorrectCall)
                .with_detail(format!("DATE: unknown option '{option}'")));
        }
    };
    Ok(RexxValue::new(result))
}

fn bif_time(args: &[RexxValue]) -> RexxResult<RexxValue> {
    check_args("TIME", args, 0, 1)?;
    let option = if args.is_empty() {
        "N".to_string()
    } else {
        args[0].as_str().to_uppercase()
    };

    let now = current_time();

    let result = match option.as_str() {
        "N" | "NORMAL" => format!("{:02}:{:02}:{:02}", now.hour, now.minute, now.second),
        "C" | "CIVIL" => {
            let (h12, ampm) = if now.hour == 0 {
                (12, "am")
            } else if now.hour < 12 {
                (now.hour, "am")
            } else if now.hour == 12 {
                (12, "pm")
            } else {
                (now.hour - 12, "pm")
            };
            format!("{h12}:{:02}{ampm}", now.minute)
        }
        "H" | "HOURS" => now.hour.to_string(),
        "L" | "LONG" => format!(
            "{:02}:{:02}:{:02}.{:06}",
            now.hour, now.minute, now.second, now.micros
        ),
        "M" | "MINUTES" => {
            let total = u32::from(now.hour) * 60 + u32::from(now.minute);
            total.to_string()
        }
        "S" | "SECONDS" => {
            let total =
                u32::from(now.hour) * 3600 + u32::from(now.minute) * 60 + u32::from(now.second);
            total.to_string()
        }
        _ => {
            return Err(RexxDiagnostic::new(RexxError::IncorrectCall)
                .with_detail(format!("TIME: unknown option '{option}'")));
        }
    };
    Ok(RexxValue::new(result))
}

// ── Date/Time helpers ───────────────────────────────────────────────

struct DateInfo {
    year: i32,
    month: u32,
    day: u32,
}

struct TimeInfo {
    hour: u8,
    minute: u8,
    second: u8,
    micros: u32,
}

fn current_date() -> DateInfo {
    use std::time::{SystemTime, UNIX_EPOCH};
    let secs = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs();
    #[allow(clippy::cast_possible_wrap)]
    let days = (secs / 86400).cast_signed();
    let (y, m, d) = civil_from_days(days + 719_468);
    DateInfo {
        year: y,
        month: m,
        day: d,
    }
}

fn current_time() -> TimeInfo {
    use std::time::{SystemTime, UNIX_EPOCH};
    let dur = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default();
    let secs = dur.as_secs();
    #[allow(clippy::cast_possible_truncation)]
    let day_secs = (secs % 86400) as u32;
    #[allow(clippy::cast_possible_truncation)]
    let hour = (day_secs / 3600) as u8;
    #[allow(clippy::cast_possible_truncation)]
    let minute = ((day_secs % 3600) / 60) as u8;
    #[allow(clippy::cast_possible_truncation)]
    let second = (day_secs % 60) as u8;
    let micros = dur.subsec_micros();
    TimeInfo {
        hour,
        minute,
        second,
        micros,
    }
}

/// Convert days since 0000-03-01 to (year, month, day).
/// Algorithm from Howard Hinnant.
#[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
fn civil_from_days(z: i64) -> (i32, u32, u32) {
    let era = (if z >= 0 { z } else { z - 146_096 }) / 146_097;
    let doe = (z - era * 146_097) as u32;
    let yoe = (doe - doe / 1460 + doe / 36524 - doe / 146_096) / 365;
    let y = (i64::from(yoe) + era * 400) as i32;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = doy - (153 * mp + 2) / 5 + 1;
    let m = if mp < 10 { mp + 3 } else { mp - 9 };
    let y = if m <= 2 { y + 1 } else { y };
    (y, m, d)
}

fn is_leap_year(year: i32) -> bool {
    (year % 4 == 0 && year % 100 != 0) || year % 400 == 0
}

fn day_of_year(year: i32, month: u32, day: u32) -> u32 {
    let days_in_months = [31u32, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
    let mut doy: u32 = day;
    for &dm in &days_in_months[..month as usize - 1] {
        doy += dm;
    }
    if month > 2 && is_leap_year(year) {
        doy += 1;
    }
    doy
}

/// Days from 0001-01-01 (day 1) to the given date.
#[allow(clippy::cast_sign_loss)]
fn days_since_base(year: i32, month: u32, day: u32) -> i64 {
    let y = if month <= 2 { year - 1 } else { year };
    let era = (if y >= 0 { y } else { y - 399 }) / 400;
    let yoe = (y - era * 400) as u32;
    let mp = if month > 2 { month - 3 } else { month + 9 };
    let doy = (153 * mp + 2) / 5 + day - 1;
    let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    let days_from_epoch0 = i64::from(era) * 146_097 + i64::from(doe);
    // 0001-01-01 = day 306 in the Hinnant epoch (0000-03-01 = day 0)
    days_from_epoch0 - 305
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_length() {
        let result = call_builtin(
            "LENGTH",
            &[RexxValue::new("hello")],
            &NumericSettings::default(),
        );
        assert_eq!(result.unwrap().unwrap().as_str(), "5");
    }

    #[test]
    fn test_substr() {
        let result = call_builtin(
            "SUBSTR",
            &[RexxValue::new("hello world"), RexxValue::new("7")],
            &NumericSettings::default(),
        );
        assert_eq!(result.unwrap().unwrap().as_str(), "world");
    }

    #[test]
    fn test_words() {
        let result = call_builtin(
            "WORDS",
            &[RexxValue::new("one two three")],
            &NumericSettings::default(),
        );
        assert_eq!(result.unwrap().unwrap().as_str(), "3");
    }

    #[test]
    fn test_abs() {
        let result = call_builtin("ABS", &[RexxValue::new("-42")], &NumericSettings::default());
        assert_eq!(result.unwrap().unwrap().as_str(), "42");
    }

    #[test]
    fn test_not_a_bif() {
        let result = call_builtin(
            "NOTABIF",
            &[RexxValue::new("x")],
            &NumericSettings::default(),
        );
        assert!(result.is_none());
    }

    #[test]
    fn test_wrong_arg_count() {
        let result = call_builtin("LENGTH", &[], &NumericSettings::default());
        assert!(result.unwrap().is_err());
    }

    #[test]
    fn test_days_since_base() {
        assert_eq!(days_since_base(1, 1, 1), 1);
    }
}
