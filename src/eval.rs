//! REXX tree-walking evaluator — AST + Environment -> execution.
//!
//! Walks the AST produced by the parser, evaluating expressions using
//! `RexxValue` and `BigDecimal` arithmetic, and executing instructions
//! against an `Environment`.

use bigdecimal::{BigDecimal, Zero};
use std::str::FromStr;

use crate::ast::{
    AssignTarget, BinOp, Clause, ClauseKind, ControlledLoop, DoBlock, DoKind, Expr, Program,
    TailElement, UnaryOp,
};
use crate::env::Environment;
use crate::error::{RexxDiagnostic, RexxError, RexxResult};
use crate::value::{NumericSettings, RexxValue};

/// Signal returned by clause/block execution for control flow.
pub enum ExecSignal {
    Normal,
    Leave(Option<String>),
    Iterate(Option<String>),
    Exit(Option<RexxValue>),
    Return(Option<RexxValue>),
}

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

    pub fn exec(&mut self, program: &Program) -> RexxResult<ExecSignal> {
        for clause in &program.clauses {
            let signal = self.exec_clause(clause)?;
            if !matches!(signal, ExecSignal::Normal) {
                return Ok(signal);
            }
        }
        Ok(ExecSignal::Normal)
    }

    fn exec_body(&mut self, body: &[Clause]) -> RexxResult<ExecSignal> {
        for clause in body {
            let signal = self.exec_clause(clause)?;
            if !matches!(signal, ExecSignal::Normal) {
                return Ok(signal);
            }
        }
        Ok(ExecSignal::Normal)
    }

    fn exec_clause(&mut self, clause: &Clause) -> RexxResult<ExecSignal> {
        match &clause.kind {
            ClauseKind::Say(expr) => {
                let val = self.eval_expr(expr)?;
                println!("{val}");
                Ok(ExecSignal::Normal)
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
                Ok(ExecSignal::Normal)
            }
            ClauseKind::Command(expr) => {
                // Evaluate and discard — future: host command
                let _val = self.eval_expr(expr)?;
                Ok(ExecSignal::Normal)
            }
            ClauseKind::If {
                condition,
                then_clause,
                else_clause,
            } => self.exec_if(condition, then_clause, else_clause.as_deref()),
            ClauseKind::Do(block) => self.exec_do(block),
            ClauseKind::Select {
                when_clauses,
                otherwise,
            } => self.exec_select(when_clauses, otherwise.as_ref()),
            ClauseKind::Leave(name) => Ok(ExecSignal::Leave(name.clone())),
            ClauseKind::Iterate(name) => Ok(ExecSignal::Iterate(name.clone())),
            ClauseKind::Exit(expr) => {
                let val = if let Some(e) = expr {
                    Some(self.eval_expr(e)?)
                } else {
                    None
                };
                Ok(ExecSignal::Exit(val))
            }
            ClauseKind::Return(expr) => {
                let val = if let Some(e) = expr {
                    Some(self.eval_expr(e)?)
                } else {
                    None
                };
                Ok(ExecSignal::Return(val))
            }
            _ => {
                // Label, Nop, and other clause types not yet implemented
                Ok(ExecSignal::Normal)
            }
        }
    }

    fn exec_if(
        &mut self,
        condition: &Expr,
        then_clause: &Clause,
        else_clause: Option<&Clause>,
    ) -> RexxResult<ExecSignal> {
        let cond_val = self.eval_expr(condition)?;
        let b = to_logical(&cond_val)?;
        if b {
            self.exec_clause(then_clause)
        } else if let Some(else_c) = else_clause {
            self.exec_clause(else_c)
        } else {
            Ok(ExecSignal::Normal)
        }
    }

    fn exec_do(&mut self, block: &DoBlock) -> RexxResult<ExecSignal> {
        match &block.kind {
            DoKind::Simple => {
                let signal = self.exec_body(&block.body)?;
                Ok(signal)
            }
            DoKind::Forever => self.exec_do_forever(block),
            DoKind::Count(expr) => self.exec_do_count(expr, block),
            DoKind::While(expr) => self.exec_do_while(expr, block),
            DoKind::Until(expr) => self.exec_do_until(expr, block),
            DoKind::Controlled(ctrl) => self.exec_do_controlled(ctrl, block),
        }
    }

    fn exec_do_forever(&mut self, block: &DoBlock) -> RexxResult<ExecSignal> {
        loop {
            let signal = self.exec_body(&block.body)?;
            match signal {
                ExecSignal::Normal => {}
                ExecSignal::Leave(ref name) => {
                    if Self::leave_matches(name.as_ref(), block.name.as_ref()) {
                        return Ok(ExecSignal::Normal);
                    }
                    return Ok(signal);
                }
                ExecSignal::Iterate(ref name) => {
                    if Self::iterate_matches(name.as_ref(), block.name.as_ref()) {
                        continue;
                    }
                    return Ok(signal);
                }
                ExecSignal::Exit(_) | ExecSignal::Return(_) => return Ok(signal),
            }
        }
    }

    fn exec_do_count(&mut self, count_expr: &Expr, block: &DoBlock) -> RexxResult<ExecSignal> {
        let count_val = self.eval_expr(count_expr)?;
        let count = self.to_integer(&count_val)?;
        for _ in 0..count {
            let signal = self.exec_body(&block.body)?;
            match signal {
                ExecSignal::Normal => {}
                ExecSignal::Leave(ref name) => {
                    if Self::leave_matches(name.as_ref(), block.name.as_ref()) {
                        return Ok(ExecSignal::Normal);
                    }
                    return Ok(signal);
                }
                ExecSignal::Iterate(ref name) => {
                    if Self::iterate_matches(name.as_ref(), block.name.as_ref()) {
                        continue;
                    }
                    return Ok(signal);
                }
                ExecSignal::Exit(_) | ExecSignal::Return(_) => return Ok(signal),
            }
        }
        Ok(ExecSignal::Normal)
    }

    fn exec_do_while(&mut self, cond_expr: &Expr, block: &DoBlock) -> RexxResult<ExecSignal> {
        loop {
            let cond_val = self.eval_expr(cond_expr)?;
            if !to_logical(&cond_val)? {
                break;
            }
            let signal = self.exec_body(&block.body)?;
            match signal {
                ExecSignal::Normal => {}
                ExecSignal::Leave(ref name) => {
                    if Self::leave_matches(name.as_ref(), block.name.as_ref()) {
                        return Ok(ExecSignal::Normal);
                    }
                    return Ok(signal);
                }
                ExecSignal::Iterate(ref name) => {
                    if Self::iterate_matches(name.as_ref(), block.name.as_ref()) {
                        continue;
                    }
                    return Ok(signal);
                }
                ExecSignal::Exit(_) | ExecSignal::Return(_) => return Ok(signal),
            }
        }
        Ok(ExecSignal::Normal)
    }

    fn exec_do_until(&mut self, cond_expr: &Expr, block: &DoBlock) -> RexxResult<ExecSignal> {
        loop {
            let signal = self.exec_body(&block.body)?;
            match signal {
                ExecSignal::Normal => {}
                ExecSignal::Leave(ref name) => {
                    if Self::leave_matches(name.as_ref(), block.name.as_ref()) {
                        return Ok(ExecSignal::Normal);
                    }
                    return Ok(signal);
                }
                ExecSignal::Iterate(ref name) => {
                    if Self::iterate_matches(name.as_ref(), block.name.as_ref()) {
                        // fall through to condition check
                    } else {
                        return Ok(signal);
                    }
                }
                ExecSignal::Exit(_) | ExecSignal::Return(_) => return Ok(signal),
            }
            let cond_val = self.eval_expr(cond_expr)?;
            if to_logical(&cond_val)? {
                break;
            }
        }
        Ok(ExecSignal::Normal)
    }

    #[allow(clippy::too_many_lines)]
    fn exec_do_controlled(
        &mut self,
        ctrl: &ControlledLoop,
        block: &DoBlock,
    ) -> RexxResult<ExecSignal> {
        // Evaluate start value
        let start_val = self.eval_expr(&ctrl.start)?;
        let start_num = self.to_number(&start_val)?;

        // Evaluate TO limit
        let to_num = if let Some(ref to_expr) = ctrl.to {
            let v = self.eval_expr(to_expr)?;
            Some(self.to_number(&v)?)
        } else {
            None
        };

        // Evaluate BY step (default 1)
        let by_num = if let Some(ref by_expr) = ctrl.by {
            let v = self.eval_expr(by_expr)?;
            self.to_number(&v)?
        } else {
            BigDecimal::from(1)
        };

        // REXX requires BY to be non-zero
        if by_num.is_zero() {
            return Err(RexxDiagnostic::new(RexxError::InvalidWholeNumber)
                .with_detail("BY value in DO loop must not be zero"));
        }

        // Evaluate FOR count
        let for_count = if let Some(ref for_expr) = ctrl.r#for {
            let v = self.eval_expr(for_expr)?;
            Some(self.to_integer(&v)?)
        } else {
            None
        };

        // Set the control variable
        let mut current = start_num;
        let mut iterations: i64 = 0;

        loop {
            // Check TO limit before executing body
            if let Some(ref limit) = to_num {
                #[allow(clippy::cmp_owned)]
                if by_num >= BigDecimal::from(0) {
                    if &current > limit {
                        break;
                    }
                } else if &current < limit {
                    break;
                }
            }

            // Check FOR count
            if let Some(max) = for_count
                && iterations >= max
            {
                break;
            }

            // Set control variable
            self.env.set(
                &ctrl.var,
                RexxValue::from_decimal(&current, self.settings.digits, self.settings.form),
            );

            // Check WHILE condition
            if let Some(ref while_expr) = ctrl.while_cond {
                let v = self.eval_expr(while_expr)?;
                if !to_logical(&v)? {
                    break;
                }
            }

            // Execute body
            let signal = self.exec_body(&block.body)?;
            match signal {
                ExecSignal::Normal => {}
                ExecSignal::Leave(ref name) => {
                    if Self::leave_matches(name.as_ref(), block.name.as_ref()) {
                        return Ok(ExecSignal::Normal);
                    }
                    return Ok(signal);
                }
                ExecSignal::Iterate(ref name) => {
                    if Self::iterate_matches(name.as_ref(), block.name.as_ref()) {
                        // fall through to increment
                    } else {
                        return Ok(signal);
                    }
                }
                ExecSignal::Exit(_) | ExecSignal::Return(_) => return Ok(signal),
            }

            // Check UNTIL condition (after body)
            // Per ANSI REXX, when UNTIL is satisfied the loop ends without
            // incrementing the control variable.
            if let Some(ref until_expr) = ctrl.until_cond {
                let v = self.eval_expr(until_expr)?;
                if to_logical(&v)? {
                    break;
                }
            }

            // Increment
            current += &by_num;
            iterations += 1;
        }

        // Set final value of control variable
        self.env.set(
            &ctrl.var,
            RexxValue::from_decimal(&current, self.settings.digits, self.settings.form),
        );

        Ok(ExecSignal::Normal)
    }

    fn exec_select(
        &mut self,
        when_clauses: &[(Expr, Vec<Clause>)],
        otherwise: Option<&Vec<Clause>>,
    ) -> RexxResult<ExecSignal> {
        for (condition, body) in when_clauses {
            let val = self.eval_expr(condition)?;
            if to_logical(&val)? {
                return self.exec_body(body);
            }
        }
        if let Some(body) = otherwise {
            return self.exec_body(body);
        }
        Err(RexxDiagnostic::new(RexxError::ExpectedWhenOtherwise)
            .with_detail("no WHEN matched and no OTHERWISE in SELECT"))
    }

    /// Check if a LEAVE signal matches this loop's name.
    fn leave_matches(signal_name: Option<&String>, loop_name: Option<&String>) -> bool {
        match signal_name {
            None => true, // unnamed LEAVE matches any loop
            Some(name) => loop_name.is_some_and(|ln| ln == name),
        }
    }

    /// Check if an ITERATE signal matches this loop's name.
    fn iterate_matches(signal_name: Option<&String>, loop_name: Option<&String>) -> bool {
        match signal_name {
            None => true, // unnamed ITERATE matches any loop
            Some(name) => loop_name.is_some_and(|ln| ln == name),
        }
    }

    /// Convert a value to a non-negative integer for loop counts.
    fn to_integer(&self, val: &RexxValue) -> RexxResult<i64> {
        let d = self.to_number(val)?;
        let rounded = d.round(0);
        let s = rounded.to_string();
        let n = s.parse::<i64>().map_err(|_| {
            RexxDiagnostic::new(RexxError::ArithmeticOverflow)
                .with_detail(format!("'{rounded}' is too large for a loop count"))
        })?;
        if n < 0 {
            return Err(RexxDiagnostic::new(RexxError::InvalidWholeNumber)
                .with_detail(format!("loop count must not be negative (got {n})")));
        }
        Ok(n)
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
                // REXX integer division truncates toward zero
                let result = trunc_div(&a, &b);
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
                // REXX remainder: a - (a%b)*b where % truncates toward zero
                let int_div = trunc_div(&a, &b);
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
                if exp_i64.abs() > 999_999_999 {
                    return Err(RexxDiagnostic::new(RexxError::ArithmeticOverflow)
                        .with_detail("exponent exceeds limits"));
                }
                if base.is_zero() && exp_i64 < 0 {
                    return Err(RexxDiagnostic::new(RexxError::ArithmeticOverflow)
                        .with_detail("zero raised to a negative power"));
                }
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

/// REXX integer division: divide and truncate toward zero.
fn trunc_div(a: &BigDecimal, b: &BigDecimal) -> BigDecimal {
    let quotient = a / b;
    // Truncate toward zero using with_scale_round(0, Down).
    // This avoids string-based truncation that can break on scientific notation.
    quotient.with_scale_round(0, bigdecimal::RoundingMode::Down)
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
        let signal = eval.exec(&program).unwrap();
        assert!(matches!(signal, ExecSignal::Normal));
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
        let signal = eval.exec(&program).unwrap();
        assert!(matches!(signal, ExecSignal::Normal));
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
