//! REXX error types and error message formatting.
//!
//! REXX defines specific error numbers (e.g., Error 41 = Bad arithmetic conversion).
//! This module maps Rust error handling to REXX's error numbering system
//! while providing modern, helpful diagnostics.

use std::fmt;

/// Source location for error reporting.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceLoc {
    pub line: usize,
    pub col: usize,
    /// Original source line text for display.
    pub source_line: Option<String>,
}

impl SourceLoc {
    pub fn new(line: usize, col: usize) -> Self {
        Self {
            line,
            col,
            source_line: None,
        }
    }

    pub fn with_source(mut self, text: String) -> Self {
        self.source_line = Some(text);
        self
    }
}

/// REXX error numbers per ANSI X3.274-1996 §A.
/// Not all are used initially but the numbering must be correct.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RexxError {
    /// 4 — Program interrupted (HALT condition)
    Halt,
    /// 5 — System resources exhausted
    ResourceExhausted,
    /// 6 — Unmatched /*
    UnmatchedComment,
    /// 7 — WHEN or OTHERWISE expected
    ExpectedWhenOtherwise,
    /// 8 — Unexpected THEN or ELSE
    UnexpectedThenElse,
    /// 9 — Unexpected WHEN or OTHERWISE
    UnexpectedWhenOtherwise,
    /// 10 — Unexpected or unmatched END
    UnexpectedEnd,
    /// 13 — Invalid character in program
    InvalidCharacter,
    /// 14 — Incomplete DO/SELECT/IF
    IncompleteBlock,
    /// 15 — Invalid hexadecimal or binary string
    InvalidHexBinary,
    /// 16 — Label not found (SIGNAL target)
    LabelNotFound,
    /// 17 — Unexpected PROCEDURE
    UnexpectedProcedure,
    /// 18 — THEN expected
    ExpectedThen,
    /// 19 — String or symbol expected
    ExpectedStringOrSymbol,
    /// 20 — Symbol expected
    ExpectedSymbol,
    /// 21 — Invalid data on end of clause
    InvalidDataOnEnd,
    /// 24 — Invalid TRACE request
    InvalidTrace,
    /// 25 — Invalid sub-keyword found
    InvalidSubKeyword,
    /// 26 — Invalid whole number
    InvalidWholeNumber,
    /// 27 — Invalid DO syntax
    InvalidDoSyntax,
    /// 28 — Invalid LEAVE or ITERATE
    InvalidLeaveIterate,
    /// 29 — Environment name too long
    EnvironmentNameTooLong,
    /// 30 — Name or string too long
    NameTooLong,
    /// 31 — Name starts with number or "."
    InvalidName,
    /// 33 — Invalid expression result
    InvalidExpressionResult,
    /// 34 — Logical value not 0 or 1
    InvalidLogicalValue,
    /// 35 — Invalid expression
    InvalidExpression,
    /// 36 — Unmatched "(" in expression
    UnmatchedParen,
    /// 37 — Unexpected "," or ")"
    UnexpectedCommaOrParen,
    /// 38 — Invalid template or pattern
    InvalidTemplate,
    /// 40 — Incorrect call to routine
    IncorrectCall,
    /// 41 — Bad arithmetic conversion
    BadArithmetic,
    /// 42 — Arithmetic overflow/underflow
    ArithmeticOverflow,
    /// 43 — Routine not found
    RoutineNotFound,
    /// 44 — Function did not return data
    NoReturnData,
    /// 45 — No data specified on function RETURN
    NoReturnValue,
    /// 46 — Invalid variable reference
    InvalidVariableRef,
    /// 48 — Failure in system service
    SystemFailure,
    /// 49 — Interpretation error (INTERPRET issues)
    InterpretationError,
}

impl RexxError {
    /// The REXX error number per the ANSI spec.
    pub fn number(self) -> u32 {
        match self {
            Self::Halt => 4,
            Self::ResourceExhausted => 5,
            Self::UnmatchedComment => 6,
            Self::ExpectedWhenOtherwise => 7,
            Self::UnexpectedThenElse => 8,
            Self::UnexpectedWhenOtherwise => 9,
            Self::UnexpectedEnd => 10,
            Self::InvalidCharacter => 13,
            Self::IncompleteBlock => 14,
            Self::InvalidHexBinary => 15,
            Self::LabelNotFound => 16,
            Self::UnexpectedProcedure => 17,
            Self::ExpectedThen => 18,
            Self::ExpectedStringOrSymbol => 19,
            Self::ExpectedSymbol => 20,
            Self::InvalidDataOnEnd => 21,
            Self::InvalidTrace => 24,
            Self::InvalidSubKeyword => 25,
            Self::InvalidWholeNumber => 26,
            Self::InvalidDoSyntax => 27,
            Self::InvalidLeaveIterate => 28,
            Self::EnvironmentNameTooLong => 29,
            Self::NameTooLong => 30,
            Self::InvalidName => 31,
            Self::InvalidExpressionResult => 33,
            Self::InvalidLogicalValue => 34,
            Self::InvalidExpression => 35,
            Self::UnmatchedParen => 36,
            Self::UnexpectedCommaOrParen => 37,
            Self::InvalidTemplate => 38,
            Self::IncorrectCall => 40,
            Self::BadArithmetic => 41,
            Self::ArithmeticOverflow => 42,
            Self::RoutineNotFound => 43,
            Self::NoReturnData => 44,
            Self::NoReturnValue => 45,
            Self::InvalidVariableRef => 46,
            Self::SystemFailure => 48,
            Self::InterpretationError => 49,
        }
    }

    /// Standard REXX error message text.
    pub fn message(self) -> &'static str {
        match self {
            Self::Halt => "Program interrupted",
            Self::ResourceExhausted => "System resources exhausted",
            Self::UnmatchedComment => "Unmatched /* in source",
            Self::ExpectedWhenOtherwise => "WHEN or OTHERWISE expected",
            Self::UnexpectedThenElse => "Unexpected THEN or ELSE",
            Self::UnexpectedWhenOtherwise => "Unexpected WHEN or OTHERWISE",
            Self::UnexpectedEnd => "Unexpected or unmatched END",
            Self::InvalidCharacter => "Invalid character in program",
            Self::IncompleteBlock => "Incomplete DO/SELECT/IF",
            Self::InvalidHexBinary => "Invalid hexadecimal or binary string",
            Self::LabelNotFound => "Label not found",
            Self::UnexpectedProcedure => "Unexpected PROCEDURE",
            Self::ExpectedThen => "THEN expected",
            Self::ExpectedStringOrSymbol => "String or symbol expected",
            Self::ExpectedSymbol => "Symbol expected",
            Self::InvalidDataOnEnd => "Invalid data on end of clause",
            Self::InvalidTrace => "Invalid TRACE request",
            Self::InvalidSubKeyword => "Invalid sub-keyword found",
            Self::InvalidWholeNumber => "Invalid whole number",
            Self::InvalidDoSyntax => "Invalid DO syntax",
            Self::InvalidLeaveIterate => "Invalid LEAVE or ITERATE",
            Self::EnvironmentNameTooLong => "Environment name too long",
            Self::NameTooLong => "Name or string too long",
            Self::InvalidName => "Name starts with number or \".\"",
            Self::InvalidExpressionResult => "Invalid expression result",
            Self::InvalidLogicalValue => "Logical value not 0 or 1",
            Self::InvalidExpression => "Invalid expression",
            Self::UnmatchedParen => "Unmatched \"(\" in expression",
            Self::UnexpectedCommaOrParen => "Unexpected \",\" or \")\"",
            Self::InvalidTemplate => "Invalid template or pattern",
            Self::IncorrectCall => "Incorrect call to routine",
            Self::BadArithmetic => "Bad arithmetic conversion",
            Self::ArithmeticOverflow => "Arithmetic overflow/underflow",
            Self::RoutineNotFound => "Routine not found",
            Self::NoReturnData => "Function did not return data",
            Self::NoReturnValue => "No data specified on function RETURN",
            Self::InvalidVariableRef => "Invalid variable reference",
            Self::SystemFailure => "Failure in system service",
            Self::InterpretationError => "Interpretation error",
        }
    }
}

/// A REXX runtime/parse error with location and context.
#[derive(Debug, Clone)]
pub struct RexxDiagnostic {
    pub error: RexxError,
    pub location: Option<SourceLoc>,
    pub detail: Option<String>,
}

impl RexxDiagnostic {
    pub fn new(error: RexxError) -> Self {
        Self {
            error,
            location: None,
            detail: None,
        }
    }

    pub fn at(mut self, loc: SourceLoc) -> Self {
        self.location = Some(loc);
        self
    }

    pub fn with_detail(mut self, detail: impl Into<String>) -> Self {
        self.detail = Some(detail.into());
        self
    }
}

impl fmt::Display for RexxDiagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Error {} — {}",
            self.error.number(),
            self.error.message()
        )?;

        if let Some(ref detail) = self.detail {
            write!(f, ": {detail}")?;
        }

        if let Some(ref loc) = self.location {
            write!(f, "\n  at line {}, column {}", loc.line, loc.col)?;
            if let Some(ref source) = loc.source_line {
                write!(f, "\n  | {source}")?;
                write!(f, "\n  | {:>width$}", "^", width = loc.col)?;
            }
        }

        Ok(())
    }
}

impl std::error::Error for RexxDiagnostic {}

/// Convenience alias.
pub type RexxResult<T> = Result<T, RexxDiagnostic>;
