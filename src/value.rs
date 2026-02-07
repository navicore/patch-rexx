//! REXX values — everything is a string.
//!
//! In REXX, all values are character strings. Numbers are strings that happen
//! to be valid numeric representations. This module implements the core value
//! type and REXX's decimal arithmetic model (NUMERIC DIGITS / FORM / FUZZ).

use bigdecimal::BigDecimal;
use std::fmt;
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
fn format_rexx_number(d: &BigDecimal, digits: u32, form: NumericForm) -> String {
    // TODO: full REXX numeric formatting with exponential notation,
    // NUMERIC DIGITS truncation, and ENGINEERING form support.
    // For now, use a basic representation.
    let _ = form;
    let rounded = d.round(i64::from(digits));
    let s = rounded.to_string();
    // REXX strips trailing zeros after decimal point
    if s.contains('.') {
        let s = s.trim_end_matches('0');
        let s = s.trim_end_matches('.');
        s.to_string()
    } else {
        s
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
}
