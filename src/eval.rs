//! REXX tree-walking evaluator — AST + Environment -> execution.
//!
//! Walks the AST produced by the parser, evaluating expressions using
//! `RexxValue` and `BigDecimal` arithmetic, and executing instructions
//! against an `Environment`.

use bigdecimal::{BigDecimal, Zero};
use std::str::FromStr;

use crate::ast::{AssignTarget, BinOp, Clause, ClauseKind, Expr, Program, TailElement, UnaryOp};
use crate::env::Environment;
use crate::error::{RexxDiagnostic, RexxError, RexxResult};
use crate::value::{NumericSettings, RexxValue};

pub struct Evaluator<'a> {
    env: &'a mut Environment,
    settings: NumericSettings,
}

impl<'a> Evaluator<'a> {
    pub fn new(env: &'a mut Environment) -> Self {
        Self {
            env,
            settings: NumericSettings::default(),
        }
    }

    pub fn exec(&mut self, program: &Program) -> RexxResult<()> {
        for clause in &program.clauses {
            self.exec_clause(clause)?;
        }
        Ok(())
    }

    fn exec_clause(&mut self, clause: &Clause) -> RexxResult<()> {
        match &clause.kind {
            ClauseKind::Say(expr) => {
                let val = self.eval_expr(expr)?;
                println!("{val}");
            }
            ClauseKind::Assignment { target, expr } => {
                let val = self.eval_expr(expr)?;
                match target {
                    AssignTarget::Simple(name) => {
                        self.env.set(name, val);
                    }
                    AssignTarget::Stem { stem, tail } => {
                        let resolved_tail = self.resolve_tail(tail);
                        self.env.set_compound(stem, &resolved_tail, val);
                    }
                }
            }
            ClauseKind::Command(expr) => {
                // Evaluate and discard — future: host command
                let _val = self.eval_expr(expr)?;
            }
            _ => {
                // Label, Nop, and other clause types not yet implemented
            }
        }
        Ok(())
    }

    fn resolve_tail(&self, tail: &[TailElement]) -> String {
        tail.iter()
            .map(|elem| match elem {
                TailElement::Const(c) => c.clone(),
                TailElement::Var(v) => self.env.get(v).into_string(),
            })
            .collect::<Vec<_>>()
            .join(".")
    }

    fn eval_expr(&mut self, expr: &Expr) -> RexxResult<RexxValue> {
        match expr {
            Expr::StringLit(s) => Ok(RexxValue::new(s.clone())),
            Expr::Number(n) => Ok(RexxValue::new(n.clone())),
            Expr::Symbol(name) => Ok(self.env.get(name)),
            Expr::Compound { stem, tail } => {
                let resolved = self.resolve_tail(tail);
                Ok(self.env.get_compound(stem, &resolved))
            }
            Expr::Paren(inner) => self.eval_expr(inner),
            Expr::UnaryOp { op, operand } => {
                let val = self.eval_expr(operand)?;
                self.eval_unary(*op, &val)
            }
            Expr::BinOp { left, op, right } => {
                let lval = self.eval_expr(left)?;
                let rval = self.eval_expr(right)?;
                self.eval_binop(*op, &lval, &rval)
            }
            Expr::FunctionCall { name, .. } => Err(RexxDiagnostic::new(RexxError::RoutineNotFound)
                .with_detail(format!("routine '{name}' not found"))),
        }
    }

    fn eval_unary(&self, op: UnaryOp, val: &RexxValue) -> RexxResult<RexxValue> {
        match op {
            UnaryOp::Plus => {
                // Force numeric validation
                let d = val.to_decimal().ok_or_else(|| {
                    RexxDiagnostic::new(RexxError::BadArithmetic)
                        .with_detail(format!("'{}' is not a number", val.as_str()))
                })?;
                Ok(RexxValue::from_decimal(
                    &d,
                    self.settings.digits,
                    self.settings.form,
                ))
            }
            UnaryOp::Minus => {
                let d = val.to_decimal().ok_or_else(|| {
                    RexxDiagnostic::new(RexxError::BadArithmetic)
                        .with_detail(format!("'{}' is not a number", val.as_str()))
                })?;
                let neg = -d;
                Ok(RexxValue::from_decimal(
                    &neg,
                    self.settings.digits,
                    self.settings.form,
                ))
            }
            UnaryOp::Not => {
                let s = val.as_str().trim();
                match s {
                    "0" => Ok(RexxValue::new("1")),
                    "1" => Ok(RexxValue::new("0")),
                    _ => Err(RexxDiagnostic::new(RexxError::InvalidLogicalValue)
                        .with_detail(format!("'{}' is not 0 or 1", val.as_str()))),
                }
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    fn eval_binop(&self, op: BinOp, left: &RexxValue, right: &RexxValue) -> RexxResult<RexxValue> {
        match op {
            // Arithmetic
            BinOp::Add => self.arithmetic(left, right, |a, b| a + b),
            BinOp::Sub => self.arithmetic(left, right, |a, b| a - b),
            BinOp::Mul => self.arithmetic(left, right, |a, b| a * b),
            BinOp::Div => {
                let b = right.to_decimal().ok_or_else(|| {
                    RexxDiagnostic::new(RexxError::BadArithmetic)
                        .with_detail(format!("'{}' is not a number", right.as_str()))
                })?;
                if b.is_zero() {
                    return Err(RexxDiagnostic::new(RexxError::ArithmeticOverflow)
                        .with_detail("division by zero"));
                }
                let a = left.to_decimal().ok_or_else(|| {
                    RexxDiagnostic::new(RexxError::BadArithmetic)
                        .with_detail(format!("'{}' is not a number", left.as_str()))
                })?;
                let result = a / b;
                Ok(RexxValue::from_decimal(
                    &result,
                    self.settings.digits,
                    self.settings.form,
                ))
            }
            BinOp::IntDiv => {
                let a = self.to_number(left)?;
                let b = self.to_number(right)?;
                if b.is_zero() {
                    return Err(RexxDiagnostic::new(RexxError::ArithmeticOverflow)
                        .with_detail("division by zero"));
                }
                // Truncate both operands to whole numbers, then divide
                let ai = a.round(0);
                let bi = b.round(0);
                let result = (ai / bi).round(0);
                Ok(RexxValue::from_decimal(
                    &result,
                    self.settings.digits,
                    self.settings.form,
                ))
            }
            BinOp::Remainder => {
                let a = self.to_number(left)?;
                let b = self.to_number(right)?;
                if b.is_zero() {
                    return Err(RexxDiagnostic::new(RexxError::ArithmeticOverflow)
                        .with_detail("division by zero"));
                }
                // REXX remainder: a - (a%b)*b
                let int_div = (&a / &b).round(0);
                let result = &a - &int_div * &b;
                Ok(RexxValue::from_decimal(
                    &result,
                    self.settings.digits,
                    self.settings.form,
                ))
            }
            BinOp::Power => {
                let base = self.to_number(left)?;
                let exp_val = self.to_number(right)?;
                // REXX requires whole-number exponent
                let exp_rounded = exp_val.round(0);
                if exp_val != exp_rounded {
                    return Err(RexxDiagnostic::new(RexxError::InvalidWholeNumber)
                        .with_detail("exponent must be a whole number"));
                }
                let exp_i64: i64 = exp_rounded.to_string().parse().map_err(|_| {
                    RexxDiagnostic::new(RexxError::ArithmeticOverflow)
                        .with_detail("exponent too large")
                })?;
                let result = pow_bigdecimal(&base, exp_i64);
                Ok(RexxValue::from_decimal(
                    &result,
                    self.settings.digits,
                    self.settings.form,
                ))
            }

            // Concatenation
            BinOp::Concat => {
                let s = format!("{}{}", left.as_str(), right.as_str());
                Ok(RexxValue::new(s))
            }
            BinOp::ConcatBlank => {
                let s = format!("{} {}", left.as_str(), right.as_str());
                Ok(RexxValue::new(s))
            }

            // Normal comparison
            BinOp::Eq => Ok(bool_val(
                normal_compare(left, right) == std::cmp::Ordering::Equal,
            )),
            BinOp::NotEq => Ok(bool_val(
                normal_compare(left, right) != std::cmp::Ordering::Equal,
            )),
            BinOp::Gt => Ok(bool_val(
                normal_compare(left, right) == std::cmp::Ordering::Greater,
            )),
            BinOp::Lt => Ok(bool_val(
                normal_compare(left, right) == std::cmp::Ordering::Less,
            )),
            BinOp::GtEq => Ok(bool_val(
                normal_compare(left, right) != std::cmp::Ordering::Less,
            )),
            BinOp::LtEq => Ok(bool_val(
                normal_compare(left, right) != std::cmp::Ordering::Greater,
            )),

            // Strict comparison
            BinOp::StrictEq => Ok(bool_val(left.as_str() == right.as_str())),
            BinOp::StrictNotEq => Ok(bool_val(left.as_str() != right.as_str())),
            BinOp::StrictGt => Ok(bool_val(left.as_str() > right.as_str())),
            BinOp::StrictLt => Ok(bool_val(left.as_str() < right.as_str())),
            BinOp::StrictGtEq => Ok(bool_val(left.as_str() >= right.as_str())),
            BinOp::StrictLtEq => Ok(bool_val(left.as_str() <= right.as_str())),

            // Logical
            BinOp::And => {
                let l = to_logical(left)?;
                let r = to_logical(right)?;
                Ok(bool_val(l && r))
            }
            BinOp::Or => {
                let l = to_logical(left)?;
                let r = to_logical(right)?;
                Ok(bool_val(l || r))
            }
            BinOp::Xor => {
                let l = to_logical(left)?;
                let r = to_logical(right)?;
                Ok(bool_val(l ^ r))
            }
        }
    }

    fn arithmetic(
        &self,
        left: &RexxValue,
        right: &RexxValue,
        f: impl FnOnce(BigDecimal, BigDecimal) -> BigDecimal,
    ) -> RexxResult<RexxValue> {
        let a = self.to_number(left)?;
        let b = self.to_number(right)?;
        let result = f(a, b);
        Ok(RexxValue::from_decimal(
            &result,
            self.settings.digits,
            self.settings.form,
        ))
    }

    #[allow(clippy::unused_self)]
    fn to_number(&self, val: &RexxValue) -> RexxResult<BigDecimal> {
        val.to_decimal().ok_or_else(|| {
            RexxDiagnostic::new(RexxError::BadArithmetic)
                .with_detail(format!("'{}' is not a number", val.as_str()))
        })
    }
}

/// Convert a `RexxValue` to a boolean, requiring it to be "0" or "1".
fn to_logical(val: &RexxValue) -> RexxResult<bool> {
    match val.as_str().trim() {
        "0" => Ok(false),
        "1" => Ok(true),
        _ => Err(RexxDiagnostic::new(RexxError::InvalidLogicalValue)
            .with_detail(format!("'{}' is not 0 or 1", val.as_str()))),
    }
}

/// Produce a REXX boolean value: "1" for true, "0" for false.
fn bool_val(b: bool) -> RexxValue {
    RexxValue::new(if b { "1" } else { "0" })
}

/// REXX normal comparison: strip leading/trailing blanks from both sides,
/// then if both are valid numbers, compare numerically;
/// otherwise pad the shorter with blanks and compare character-by-character.
fn normal_compare(left: &RexxValue, right: &RexxValue) -> std::cmp::Ordering {
    let ls = left.as_str().trim();
    let rs = right.as_str().trim();

    // Try numeric comparison first
    if let (Some(ld), Some(rd)) = (BigDecimal::from_str(ls).ok(), BigDecimal::from_str(rs).ok()) {
        return ld.cmp(&rd);
    }

    // String comparison: pad shorter with trailing blanks
    let max_len = ls.len().max(rs.len());
    let lp: String = format!("{ls:<max_len$}");
    let rp: String = format!("{rs:<max_len$}");
    lp.cmp(&rp)
}

/// Compute base ** exp for `BigDecimal` with integer exponent.
fn pow_bigdecimal(base: &BigDecimal, exp: i64) -> BigDecimal {
    if exp == 0 {
        return BigDecimal::from(1);
    }
    if exp < 0 {
        let pos_result = pow_bigdecimal(base, -exp);
        return BigDecimal::from(1) / pos_result;
    }
    let mut result = BigDecimal::from(1);
    let mut b = base.clone();
    let mut e = exp;
    // Exponentiation by squaring
    while e > 0 {
        if e & 1 == 1 {
            result *= &b;
        }
        b = &b * &b;
        e >>= 1;
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn eval_expr(src: &str) -> RexxValue {
        let mut env = Environment::new();
        let mut lexer = Lexer::new(src);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        let mut eval = Evaluator::new(&mut env);
        // Evaluate as command and extract the value
        match &program.clauses[0].kind {
            ClauseKind::Command(expr) => eval.eval_expr(expr).unwrap(),
            _ => panic!("expected command clause"),
        }
    }

    #[test]
    fn eval_addition() {
        let val = eval_expr("2 + 3");
        assert_eq!(val.as_str(), "5");
    }

    #[test]
    fn eval_subtraction() {
        let val = eval_expr("10 - 4");
        assert_eq!(val.as_str(), "6");
    }

    #[test]
    fn eval_multiplication() {
        let val = eval_expr("3 * 7");
        assert_eq!(val.as_str(), "21");
    }

    #[test]
    fn eval_division() {
        let val = eval_expr("10 / 4");
        assert_eq!(val.as_str(), "2.5");
    }

    #[test]
    fn eval_precedence() {
        let val = eval_expr("2 + 3 * 4");
        assert_eq!(val.as_str(), "14");
    }

    #[test]
    fn eval_division_by_zero() {
        let mut env = Environment::new();
        let mut lexer = Lexer::new("1 / 0");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        let mut eval = Evaluator::new(&mut env);
        match &program.clauses[0].kind {
            ClauseKind::Command(expr) => {
                let result = eval.eval_expr(expr);
                assert!(result.is_err());
                assert_eq!(result.unwrap_err().error, RexxError::ArithmeticOverflow);
            }
            _ => panic!("expected command clause"),
        }
    }

    #[test]
    fn eval_power() {
        let val = eval_expr("2 ** 10");
        assert_eq!(val.as_str(), "1024");
    }

    #[test]
    fn eval_string_concat_blank() {
        let val = eval_expr("'hello' 'world'");
        assert_eq!(val.as_str(), "hello world");
    }

    #[test]
    fn eval_string_concat_abuttal() {
        let val = eval_expr("'hello'||'world'");
        assert_eq!(val.as_str(), "helloworld");
    }

    #[test]
    fn eval_comparison_numeric() {
        let val = eval_expr("3 > 2");
        assert_eq!(val.as_str(), "1");
    }

    #[test]
    fn eval_comparison_equal() {
        let val = eval_expr("5 = 5");
        assert_eq!(val.as_str(), "1");
    }

    #[test]
    fn eval_comparison_string() {
        let val = eval_expr("'abc' = 'abc'");
        assert_eq!(val.as_str(), "1");
    }

    #[test]
    fn eval_strict_comparison() {
        let val = eval_expr("' abc' == 'abc'");
        assert_eq!(val.as_str(), "0");
    }

    #[test]
    fn eval_logical_and() {
        let val = eval_expr("1 & 1");
        assert_eq!(val.as_str(), "1");
        let val = eval_expr("1 & 0");
        assert_eq!(val.as_str(), "0");
    }

    #[test]
    fn eval_logical_or() {
        let val = eval_expr("0 | 1");
        assert_eq!(val.as_str(), "1");
    }

    #[test]
    fn eval_logical_not() {
        let val = eval_expr("\\0");
        assert_eq!(val.as_str(), "1");
    }

    #[test]
    fn eval_variable_assignment_and_lookup() {
        let mut env = Environment::new();
        let mut lexer = Lexer::new("x = 42; x + 1");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        let mut eval = Evaluator::new(&mut env);
        eval.exec(&program).unwrap();
        // After execution, x should be "42" and the command clause evaluated "43"
        assert_eq!(env.get("X").as_str(), "42");
    }

    #[test]
    fn eval_unset_variable_returns_name() {
        let val = eval_expr("foo");
        assert_eq!(val.as_str(), "FOO");
    }

    #[test]
    fn eval_say_runs() {
        // Smoke test — just ensure it doesn't panic
        let mut env = Environment::new();
        let mut lexer = Lexer::new("say 2 + 3");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        let mut eval = Evaluator::new(&mut env);
        eval.exec(&program).unwrap();
    }

    #[test]
    fn eval_negative_power() {
        let val = eval_expr("2 ** -1");
        assert_eq!(val.as_str(), "0.5");
    }

    #[test]
    fn eval_unary_minus() {
        let val = eval_expr("-5 + 3");
        assert_eq!(val.as_str(), "-2");
    }

    #[test]
    fn eval_remainder() {
        let val = eval_expr("17 // 5");
        assert_eq!(val.as_str(), "2");
    }

    #[test]
    fn eval_integer_division() {
        let val = eval_expr("17 % 5");
        assert_eq!(val.as_str(), "3");
    }
}
