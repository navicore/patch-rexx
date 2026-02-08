//! REXX values — everything is a string.
//!
//! In REXX, all values are character strings. Numbers are strings that happen
//! to be valid numeric representations. This module implements the core value
//! type and REXX's decimal arithmetic model (NUMERIC DIGITS / FORM / FUZZ).

use bigdecimal::{BigDecimal, RoundingMode, Signed};
use std::fmt;
use std::num::NonZeroU64;
use std::str::FromStr;

/// Every REXX value is a string. Numeric operations interpret the string
/// content as a number when needed, and produce string results.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RexxValue {
    data: String,
}

impl RexxValue {
    pub fn new(s: impl Into<String>) -> Self {
        Self { data: s.into() }
    }

    pub fn as_str(&self) -> &str {
        &self.data
    }

    pub fn into_string(self) -> String {
        self.data
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    /// Attempt to interpret this value as a REXX number.
    /// REXX numbers can have leading/trailing blanks, optional sign,
    /// digits with an optional decimal point, and optional exponent.
    pub fn to_decimal(&self) -> Option<BigDecimal> {
        let trimmed = self.data.trim();
        if trimmed.is_empty() {
            return None;
        }
        BigDecimal::from_str(trimmed).ok()
    }

    /// Check if this value is a valid REXX number.
    pub fn is_number(&self) -> bool {
        self.to_decimal().is_some()
    }

    /// REXX whole number check — a number with no fractional part
    /// within current NUMERIC DIGITS precision.
    pub fn is_whole_number(&self, digits: u32) -> bool {
        match self.to_decimal() {
            Some(d) => {
                let rounded = d.round(0);
                let diff = (&d - &rounded).abs();
                diff < BigDecimal::from_str(&format!("1E-{digits}")).unwrap_or(BigDecimal::from(0))
            }
            None => false,
        }
    }

    /// Format a `BigDecimal` according to REXX numeric formatting rules.
    /// Respects NUMERIC DIGITS and NUMERIC FORM (SCIENTIFIC vs ENGINEERING).
    pub fn from_decimal(d: &BigDecimal, digits: u32, form: NumericForm) -> Self {
        let formatted = format_rexx_number(d, digits, form);
        Self::new(formatted)
    }
}

impl fmt::Display for RexxValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.data)
    }
}

impl From<&str> for RexxValue {
    fn from(s: &str) -> Self {
        Self::new(s)
    }
}

impl From<String> for RexxValue {
    fn from(s: String) -> Self {
        Self::new(s)
    }
}

impl From<i64> for RexxValue {
    fn from(n: i64) -> Self {
        Self::new(n.to_string())
    }
}

/// NUMERIC FORM controls exponential notation style.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum NumericForm {
    /// Exponent is a multiple of 1 (default).
    #[default]
    Scientific,
    /// Exponent is a multiple of 3.
    Engineering,
}

/// Numeric settings for the current execution context.
#[derive(Debug, Clone)]
pub struct NumericSettings {
    /// Number of significant digits (default 9).
    pub digits: u32,
    /// Exponential notation form.
    pub form: NumericForm,
    /// Digits of "fuzziness" for comparisons (default 0).
    pub fuzz: u32,
}

impl Default for NumericSettings {
    fn default() -> Self {
        Self {
            digits: 9,
            form: NumericForm::Scientific,
            fuzz: 0,
        }
    }
}

/// Format a `BigDecimal` to a REXX-compliant string representation.
/// Rounds to NUMERIC DIGITS significant digits (not decimal places),
/// then chooses plain or exponential notation based on the adjusted exponent.
#[allow(clippy::cast_possible_wrap)]
fn format_rexx_number(d: &BigDecimal, digits: u32, form: NumericForm) -> String {
    use bigdecimal::num_bigint::Sign;

    if d.sign() == Sign::NoSign {
        return "0".to_string();
    }

    let prec = NonZeroU64::new(u64::from(digits)).unwrap_or(NonZeroU64::MIN);
    let rounded = d.with_precision_round(prec, RoundingMode::HalfUp);
    let normed = rounded.normalized();
    let (coeff, scale) = normed.as_bigint_and_exponent();
    let is_negative = coeff.sign() == Sign::Minus;
    let coeff_str = coeff.abs().to_string();
    // Safe: coefficient length is bounded by NUMERIC DIGITS (≤ u32::MAX)
    let n = coeff_str.len() as i64;

    // Adjusted exponent: the power of 10 of the leading digit.
    // value = coeff × 10^(−scale), in scientific form: d.ddd × 10^adj_exp
    let adj_exp = n - 1 - scale;

    // REXX uses plain notation when the adjusted exponent is in range:
    //   non-negative and < 2×DIGITS, or negative and >= −DIGITS.
    // Beyond that range, exponential notation is used.
    let use_plain = if adj_exp >= 0 {
        adj_exp < i64::from(digits) * 2
    } else {
        adj_exp >= -i64::from(digits)
    };

    let sign = if is_negative { "-" } else { "" };

    if use_plain {
        format!("{sign}{}", format_plain(&coeff_str, adj_exp))
    } else {
        format!("{sign}{}", format_exp(&coeff_str, adj_exp, form))
    }
}

/// Format a number in plain (non-exponential) notation.
#[allow(
    clippy::cast_possible_wrap,
    clippy::cast_possible_truncation,
    clippy::cast_sign_loss
)]
fn format_plain(coeff: &str, adj_exp: i64) -> String {
    let n = coeff.len() as i64;
    if adj_exp >= n - 1 {
        // Pure integer — append trailing zeros
        let trailing = (adj_exp - n + 1) as usize;
        format!("{coeff}{}", "0".repeat(trailing))
    } else if adj_exp >= 0 {
        // Decimal point falls within the digits
        let split = (adj_exp + 1) as usize;
        format!("{}.{}", &coeff[..split], &coeff[split..])
    } else {
        // Number < 1 — prepend leading zeros after "0."
        let leading = (-adj_exp - 1) as usize;
        format!("0.{}{coeff}", "0".repeat(leading))
    }
}

/// Format a number in exponential notation (SCIENTIFIC or ENGINEERING).
#[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
fn format_exp(coeff: &str, adj_exp: i64, form: NumericForm) -> String {
    let (digits_before, exp) = match form {
        NumericForm::Scientific => (1usize, adj_exp),
        NumericForm::Engineering => {
            let e = adj_exp - adj_exp.rem_euclid(3);
            ((adj_exp - e + 1) as usize, e)
        }
    };

    let n = coeff.len();
    let mantissa = if digits_before >= n {
        let padding = digits_before - n;
        format!("{coeff}{}", "0".repeat(padding))
    } else {
        format!("{}.{}", &coeff[..digits_before], &coeff[digits_before..])
    };

    if exp >= 0 {
        format!("{mantissa}E+{exp}")
    } else {
        format!("{mantissa}E{exp}")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn string_value() {
        let v = RexxValue::new("hello");
        assert_eq!(v.as_str(), "hello");
        assert!(!v.is_number());
    }

    #[test]
    fn numeric_value() {
        let v = RexxValue::new("42");
        assert!(v.is_number());
        assert_eq!(v.to_decimal().unwrap(), BigDecimal::from(42));
    }

    #[test]
    fn numeric_with_spaces() {
        let v = RexxValue::new("  3.14  ");
        assert!(v.is_number());
    }

    #[test]
    fn non_numeric() {
        let v = RexxValue::new("hello");
        assert!(!v.is_number());
        assert!(v.to_decimal().is_none());
    }

    #[test]
    fn from_integer() {
        let v = RexxValue::from(42i64);
        assert_eq!(v.as_str(), "42");
    }

    #[test]
    fn default_numeric_settings() {
        let settings = NumericSettings::default();
        assert_eq!(settings.digits, 9);
        assert_eq!(settings.fuzz, 0);
        assert_eq!(settings.form, NumericForm::Scientific);
    }

    // ── format_rexx_number tests ──────────────────────────────────

    #[test]
    fn format_integer() {
        let d = BigDecimal::from(42);
        assert_eq!(format_rexx_number(&d, 9, NumericForm::Scientific), "42");
    }

    #[test]
    fn format_decimal() {
        let d = BigDecimal::from_str("3.14").unwrap();
        assert_eq!(format_rexx_number(&d, 9, NumericForm::Scientific), "3.14");
    }

    #[test]
    fn format_zero() {
        let d = BigDecimal::from(0);
        assert_eq!(format_rexx_number(&d, 9, NumericForm::Scientific), "0");
    }

    #[test]
    fn format_negative() {
        let d = BigDecimal::from(-42);
        assert_eq!(format_rexx_number(&d, 9, NumericForm::Scientific), "-42");
    }

    #[test]
    fn format_significant_digit_rounding() {
        // 123456789.5 rounded to 9 significant digits → 123456790
        let d = BigDecimal::from_str("123456789.5").unwrap();
        assert_eq!(
            format_rexx_number(&d, 9, NumericForm::Scientific),
            "123456790"
        );
    }

    #[test]
    fn format_large_plain() {
        // 10^17 has adjusted exponent 17, within 2×9=18 threshold → plain
        let d = BigDecimal::from_str("1E17").unwrap();
        assert_eq!(
            format_rexx_number(&d, 9, NumericForm::Scientific),
            "100000000000000000"
        );
    }

    #[test]
    fn format_large_exponential() {
        // 10^18 has adjusted exponent 18, equals 2×9 → exponential
        let d = BigDecimal::from_str("1E18").unwrap();
        assert_eq!(format_rexx_number(&d, 9, NumericForm::Scientific), "1E+18");
    }

    #[test]
    fn format_small_plain() {
        // 10^-9 has adjusted exponent -9, equals -DIGITS → plain
        let d = BigDecimal::from_str("1E-9").unwrap();
        assert_eq!(
            format_rexx_number(&d, 9, NumericForm::Scientific),
            "0.000000001"
        );
    }

    #[test]
    fn format_small_exponential() {
        // adjusted exponent -10, below -DIGITS → exponential
        let d = BigDecimal::from_str("1.23E-10").unwrap();
        assert_eq!(
            format_rexx_number(&d, 9, NumericForm::Scientific),
            "1.23E-10"
        );
    }

    #[test]
    fn format_engineering_form() {
        let d = BigDecimal::from_str("1.23E20").unwrap();
        assert_eq!(
            format_rexx_number(&d, 9, NumericForm::Engineering),
            "123E+18"
        );
    }

    #[test]
    fn format_trailing_zeros_stripped() {
        let d = BigDecimal::from_str("5.00").unwrap();
        assert_eq!(format_rexx_number(&d, 9, NumericForm::Scientific), "5");
    }

    #[test]
    fn format_negative_exponential() {
        let d = BigDecimal::from_str("-1.5E20").unwrap();
        assert_eq!(
            format_rexx_number(&d, 9, NumericForm::Scientific),
            "-1.5E+20"
        );
    }
}
