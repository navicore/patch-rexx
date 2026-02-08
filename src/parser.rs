//! REXX recursive descent parser — tokens to AST.
//!
//! Consumes a `Vec<Token>` from the lexer and produces an `ast::Program`.
//! REXX has no reserved words; keywords like SAY, IF, DO are just symbols
//! recognised by context at the start of a clause.

use crate::ast::{AssignTarget, BinOp, Clause, ClauseKind, Expr, Program, UnaryOp};
use crate::error::{RexxDiagnostic, RexxError, RexxResult, SourceLoc};
use crate::lexer::{Token, TokenKind};

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    pub fn parse(&mut self) -> RexxResult<Program> {
        let mut clauses = Vec::new();
        self.skip_terminators();
        while !self.at_end() {
            clauses.push(self.parse_clause()?);
            self.skip_terminators();
        }
        Ok(Program { clauses })
    }

    // ── helpers ──────────────────────────────────────────────────────

    fn peek(&self) -> &Token {
        &self.tokens[self.pos]
    }

    fn peek_kind(&self) -> &TokenKind {
        &self.tokens[self.pos].kind
    }

    fn at_end(&self) -> bool {
        matches!(self.peek_kind(), TokenKind::Eof)
    }

    fn advance(&mut self) -> &Token {
        let tok = &self.tokens[self.pos];
        if !self.at_end() {
            self.pos += 1;
        }
        tok
    }

    fn loc(&self) -> SourceLoc {
        self.peek().loc.clone()
    }

    fn expect(&mut self, kind: &TokenKind) -> RexxResult<&Token> {
        if &self.tokens[self.pos].kind == kind {
            let tok = &self.tokens[self.pos];
            self.pos += 1;
            Ok(tok)
        } else {
            Err(RexxDiagnostic::new(RexxError::InvalidExpression)
                .at(self.loc())
                .with_detail(format!("expected {kind:?}, found {:?}", self.peek_kind())))
        }
    }

    /// True if the current token is a clause terminator (`;`, `Eol`, `Eof`).
    fn is_terminator(&self) -> bool {
        matches!(
            self.peek_kind(),
            TokenKind::Semicolon | TokenKind::Eol | TokenKind::Eof
        )
    }

    fn skip_terminators(&mut self) {
        while matches!(self.peek_kind(), TokenKind::Semicolon | TokenKind::Eol) {
            self.advance();
        }
    }

    /// Case-insensitive keyword check on a Symbol token value.
    fn check_keyword(sym: &str, keyword: &str) -> bool {
        sym.eq_ignore_ascii_case(keyword)
    }

    /// Peek ahead by `n` tokens (0 = current).
    fn peek_at(&self, n: usize) -> &TokenKind {
        let idx = self.pos + n;
        if idx < self.tokens.len() {
            &self.tokens[idx].kind
        } else {
            &TokenKind::Eof
        }
    }

    // ── clause parsing ──────────────────────────────────────────────

    fn parse_clause(&mut self) -> RexxResult<Clause> {
        let loc = self.loc();

        // Look at the first token
        if let TokenKind::Symbol(ref name) = self.peek_kind().clone() {
            // Symbol + Colon -> Label
            if matches!(self.peek_at(1), TokenKind::Colon) {
                let label = name.to_uppercase();
                self.advance(); // symbol
                self.advance(); // colon
                return Ok(Clause {
                    kind: ClauseKind::Label(label),
                    loc,
                });
            }

            // Symbol + = -> Assignment
            if matches!(self.peek_at(1), TokenKind::Assign) {
                return self.parse_assignment(&loc);
            }

            // SAY instruction
            if Self::check_keyword(name, "SAY") {
                self.advance(); // consume SAY
                let expr = if self.is_terminator() {
                    // SAY with no expression outputs empty line
                    Expr::StringLit(String::new())
                } else {
                    self.parse_expression()?
                };
                return Ok(Clause {
                    kind: ClauseKind::Say(expr),
                    loc,
                });
            }

            // NOP instruction
            if Self::check_keyword(name, "NOP") {
                self.advance();
                return Ok(Clause {
                    kind: ClauseKind::Nop,
                    loc,
                });
            }
        }

        // Default: command clause (expression evaluated and discarded)
        let expr = self.parse_expression()?;
        Ok(Clause {
            kind: ClauseKind::Command(expr),
            loc,
        })
    }

    fn parse_assignment(&mut self, loc: &SourceLoc) -> RexxResult<Clause> {
        let name = if let TokenKind::Symbol(s) = self.peek_kind() {
            s.clone()
        } else {
            unreachable!()
        };
        self.advance(); // symbol
        self.advance(); // =

        let target = if name.contains('.') {
            // Compound variable: stem.tail
            let parts: Vec<&str> = name.splitn(2, '.').collect();
            let stem = parts[0].to_uppercase();
            let tail_str = parts[1];
            let tail = parse_tail_elements(tail_str);
            AssignTarget::Stem { stem, tail }
        } else {
            AssignTarget::Simple(name.to_uppercase())
        };

        let expr = self.parse_expression()?;
        Ok(Clause {
            kind: ClauseKind::Assignment { target, expr },
            loc: loc.clone(),
        })
    }

    // ── expression parsing (precedence climbing) ────────────────────
    //
    // Lowest to highest:
    //   1. OR / XOR    (| &&)
    //   2. AND         (&)
    //   3. comparison  (= \= > < >= <= == \== >> << >>= <<=)
    //   4. concat      (blank-concat, ||, abuttal)
    //   5. add / sub   (+ -)
    //   6. mul / div   (* / % //)
    //   7. power       (**)   — right associative
    //   8. unary       (+ - \)
    //   9. primary     (literals, symbols, parens, function calls)

    fn parse_expression(&mut self) -> RexxResult<Expr> {
        self.parse_or_xor()
    }

    // Level 1: OR (|) and XOR (&&)
    fn parse_or_xor(&mut self) -> RexxResult<Expr> {
        let mut left = self.parse_and()?;
        loop {
            let op = match self.peek_kind() {
                TokenKind::Or => BinOp::Or,
                TokenKind::Xor => BinOp::Xor,
                _ => break,
            };
            self.advance();
            let right = self.parse_and()?;
            left = Expr::BinOp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    // Level 2: AND (&)
    fn parse_and(&mut self) -> RexxResult<Expr> {
        let mut left = self.parse_comparison()?;
        loop {
            if !matches!(self.peek_kind(), TokenKind::And) {
                break;
            }
            self.advance();
            let right = self.parse_comparison()?;
            left = Expr::BinOp {
                left: Box::new(left),
                op: BinOp::And,
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    // Level 3: comparison operators
    fn parse_comparison(&mut self) -> RexxResult<Expr> {
        let mut left = self.parse_concat()?;
        loop {
            let op = match self.peek_kind() {
                // = at expression level is comparison, not assignment
                TokenKind::Assign | TokenKind::Equal => BinOp::Eq,
                TokenKind::NotEqual => BinOp::NotEq,
                TokenKind::Greater => BinOp::Gt,
                TokenKind::Less => BinOp::Lt,
                TokenKind::GreaterEq => BinOp::GtEq,
                TokenKind::LessEq => BinOp::LtEq,
                TokenKind::StrictEq => BinOp::StrictEq,
                TokenKind::StrictNotEq => BinOp::StrictNotEq,
                TokenKind::StrictGt => BinOp::StrictGt,
                TokenKind::StrictLt => BinOp::StrictLt,
                TokenKind::StrictGte => BinOp::StrictGtEq,
                TokenKind::StrictLte => BinOp::StrictLtEq,
                _ => break,
            };
            self.advance();
            let right = self.parse_concat()?;
            left = Expr::BinOp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    // Level 4: concatenation (||, blank-concat, abuttal)
    fn parse_concat(&mut self) -> RexxResult<Expr> {
        let mut left = self.parse_addition()?;
        loop {
            // Explicit || concat
            if matches!(self.peek_kind(), TokenKind::Concat) {
                self.advance();
                let right = self.parse_addition()?;
                left = Expr::BinOp {
                    left: Box::new(left),
                    op: BinOp::Concat,
                    right: Box::new(right),
                };
                continue;
            }

            // Implicit concatenation: the next token can start a term,
            // is not a binary operator, and there may or may not be a space.
            if self.can_start_term() && !self.is_binary_op() {
                let has_space = self.peek().space_before;
                let op = if has_space {
                    BinOp::ConcatBlank
                } else {
                    BinOp::Concat
                };
                let right = self.parse_addition()?;
                left = Expr::BinOp {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                };
                continue;
            }

            break;
        }
        Ok(left)
    }

    /// True if the current token could start a primary expression term.
    fn can_start_term(&self) -> bool {
        matches!(
            self.peek_kind(),
            TokenKind::StringLit(_)
                | TokenKind::Number(_)
                | TokenKind::Symbol(_)
                | TokenKind::LeftParen
                | TokenKind::Plus
                | TokenKind::Minus
                | TokenKind::Not
        )
    }

    /// True if the current token is a binary operator (not concat-related).
    fn is_binary_op(&self) -> bool {
        matches!(
            self.peek_kind(),
            TokenKind::Plus
                | TokenKind::Minus
                | TokenKind::Star
                | TokenKind::Slash
                | TokenKind::IntDiv
                | TokenKind::Remainder
                | TokenKind::Power
                | TokenKind::Assign
                | TokenKind::Equal
                | TokenKind::NotEqual
                | TokenKind::Greater
                | TokenKind::Less
                | TokenKind::GreaterEq
                | TokenKind::LessEq
                | TokenKind::StrictEq
                | TokenKind::StrictNotEq
                | TokenKind::StrictGt
                | TokenKind::StrictLt
                | TokenKind::StrictGte
                | TokenKind::StrictLte
                | TokenKind::And
                | TokenKind::Or
                | TokenKind::Xor
                | TokenKind::Concat
        )
    }

    // Level 5: addition / subtraction
    fn parse_addition(&mut self) -> RexxResult<Expr> {
        let mut left = self.parse_multiplication()?;
        loop {
            let op = match self.peek_kind() {
                TokenKind::Plus => BinOp::Add,
                TokenKind::Minus => BinOp::Sub,
                _ => break,
            };
            self.advance();
            let right = self.parse_multiplication()?;
            left = Expr::BinOp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    // Level 6: multiplication / division
    fn parse_multiplication(&mut self) -> RexxResult<Expr> {
        let mut left = self.parse_power()?;
        loop {
            let op = match self.peek_kind() {
                TokenKind::Star => BinOp::Mul,
                TokenKind::Slash => BinOp::Div,
                TokenKind::IntDiv => BinOp::IntDiv,
                TokenKind::Remainder => BinOp::Remainder,
                _ => break,
            };
            self.advance();
            let right = self.parse_power()?;
            left = Expr::BinOp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    // Level 7: power (**) — right associative
    fn parse_power(&mut self) -> RexxResult<Expr> {
        let base = self.parse_unary()?;
        if matches!(self.peek_kind(), TokenKind::Power) {
            self.advance();
            let exp = self.parse_power()?; // right-recursive for right-assoc
            Ok(Expr::BinOp {
                left: Box::new(base),
                op: BinOp::Power,
                right: Box::new(exp),
            })
        } else {
            Ok(base)
        }
    }

    // Level 8: unary prefix (+ - \)
    fn parse_unary(&mut self) -> RexxResult<Expr> {
        match self.peek_kind() {
            TokenKind::Plus => {
                self.advance();
                let operand = self.parse_unary()?;
                Ok(Expr::UnaryOp {
                    op: UnaryOp::Plus,
                    operand: Box::new(operand),
                })
            }
            TokenKind::Minus => {
                self.advance();
                let operand = self.parse_unary()?;
                Ok(Expr::UnaryOp {
                    op: UnaryOp::Minus,
                    operand: Box::new(operand),
                })
            }
            TokenKind::Not => {
                self.advance();
                let operand = self.parse_unary()?;
                Ok(Expr::UnaryOp {
                    op: UnaryOp::Not,
                    operand: Box::new(operand),
                })
            }
            _ => self.parse_primary(),
        }
    }

    // Level 9: primary expressions
    fn parse_primary(&mut self) -> RexxResult<Expr> {
        match self.peek_kind().clone() {
            TokenKind::StringLit(s) => {
                self.advance();
                Ok(Expr::StringLit(s))
            }
            TokenKind::Number(n) => {
                self.advance();
                Ok(Expr::Number(n))
            }
            TokenKind::Symbol(name) => {
                self.advance();
                // Function call: symbol immediately followed by '(' (no space)
                if matches!(self.peek_kind(), TokenKind::LeftParen) && !self.peek().space_before {
                    return self.parse_function_call(&name);
                }
                Ok(Expr::Symbol(name.to_uppercase()))
            }
            TokenKind::LeftParen => {
                self.advance(); // (
                let expr = self.parse_expression()?;
                let err_loc = self.loc();
                self.expect(&TokenKind::RightParen).map_err(|_| {
                    RexxDiagnostic::new(RexxError::UnmatchedParen)
                        .at(err_loc)
                        .with_detail("expected closing ')'")
                })?;
                Ok(Expr::Paren(Box::new(expr)))
            }
            _ => Err(RexxDiagnostic::new(RexxError::InvalidExpression)
                .at(self.loc())
                .with_detail(format!("unexpected token {:?}", self.peek_kind()))),
        }
    }

    fn parse_function_call(&mut self, name: &str) -> RexxResult<Expr> {
        self.advance(); // (
        let mut args = Vec::new();
        if !matches!(self.peek_kind(), TokenKind::RightParen) {
            args.push(self.parse_expression()?);
            while matches!(self.peek_kind(), TokenKind::Comma) {
                self.advance();
                args.push(self.parse_expression()?);
            }
        }
        let err_loc = self.loc();
        self.expect(&TokenKind::RightParen).map_err(|_| {
            RexxDiagnostic::new(RexxError::UnmatchedParen)
                .at(err_loc)
                .with_detail("expected ')' after function arguments")
        })?;
        Ok(Expr::FunctionCall {
            name: name.to_uppercase(),
            args,
        })
    }
}

/// Parse tail elements from the string after the first dot in a compound symbol.
/// E.g. for `arr.i.j`, after splitting on the first dot we get `"i.j"`,
/// which produces `[Var("I"), Var("J")]`.
fn parse_tail_elements(tail: &str) -> Vec<crate::ast::TailElement> {
    use crate::ast::TailElement;
    tail.split('.')
        .map(|part| {
            if part.is_empty() || part.chars().all(|c| c.is_ascii_digit()) {
                TailElement::Const(part.to_uppercase())
            } else {
                TailElement::Var(part.to_uppercase())
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    fn parse(src: &str) -> Program {
        let mut lexer = Lexer::new(src);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        parser.parse().unwrap()
    }

    fn parse_expr(src: &str) -> Expr {
        // Wrap in a command clause to extract expression
        let prog = parse(src);
        match prog.clauses.into_iter().next().unwrap().kind {
            ClauseKind::Command(e) | ClauseKind::Say(e) => e,
            other => panic!("expected expression clause, got {other:?}"),
        }
    }

    #[test]
    fn parse_number_literal() {
        let expr = parse_expr("42");
        assert!(matches!(expr, Expr::Number(n) if n == "42"));
    }

    #[test]
    fn parse_string_literal() {
        let expr = parse_expr("'hello'");
        assert!(matches!(expr, Expr::StringLit(s) if s == "hello"));
    }

    #[test]
    fn parse_addition() {
        let expr = parse_expr("2 + 3");
        assert!(matches!(expr, Expr::BinOp { op: BinOp::Add, .. }));
    }

    #[test]
    fn parse_precedence() {
        // 2 + 3 * 4 should parse as 2 + (3 * 4)
        let expr = parse_expr("2 + 3 * 4");
        match expr {
            Expr::BinOp {
                op: BinOp::Add,
                ref right,
                ..
            } => {
                assert!(matches!(**right, Expr::BinOp { op: BinOp::Mul, .. }));
            }
            _ => panic!("expected Add at top level"),
        }
    }

    #[test]
    fn parse_power_right_assoc() {
        // 2 ** 3 ** 4 should parse as 2 ** (3 ** 4)
        let expr = parse_expr("2 ** 3 ** 4");
        match expr {
            Expr::BinOp {
                op: BinOp::Power,
                ref right,
                ..
            } => {
                assert!(matches!(
                    **right,
                    Expr::BinOp {
                        op: BinOp::Power,
                        ..
                    }
                ));
            }
            _ => panic!("expected Power at top level"),
        }
    }

    #[test]
    fn parse_parens() {
        let expr = parse_expr("(2 + 3) * 4");
        assert!(matches!(expr, Expr::BinOp { op: BinOp::Mul, .. }));
    }

    #[test]
    fn parse_unary_minus() {
        let expr = parse_expr("-5");
        assert!(matches!(
            expr,
            Expr::UnaryOp {
                op: UnaryOp::Minus,
                ..
            }
        ));
    }

    #[test]
    fn parse_say_clause() {
        let prog = parse("say 'hello'");
        match &prog.clauses[0].kind {
            ClauseKind::Say(Expr::StringLit(s)) => assert_eq!(s, "hello"),
            other => panic!("expected Say, got {other:?}"),
        }
    }

    #[test]
    fn parse_assignment_clause() {
        let prog = parse("x = 42");
        match &prog.clauses[0].kind {
            ClauseKind::Assignment {
                target: AssignTarget::Simple(name),
                expr: Expr::Number(n),
            } => {
                assert_eq!(name, "X");
                assert_eq!(n, "42");
            }
            other => panic!("expected Assignment, got {other:?}"),
        }
    }

    #[test]
    fn parse_label() {
        let prog = parse("myLabel:");
        match &prog.clauses[0].kind {
            ClauseKind::Label(name) => assert_eq!(name, "MYLABEL"),
            other => panic!("expected Label, got {other:?}"),
        }
    }

    #[test]
    fn parse_concat_forms() {
        // Blank concatenation: 'a' 'b'
        let expr = parse_expr("'a' 'b'");
        assert!(matches!(
            expr,
            Expr::BinOp {
                op: BinOp::ConcatBlank,
                ..
            }
        ));
    }

    #[test]
    fn parse_explicit_concat() {
        let expr = parse_expr("'a' || 'b'");
        assert!(matches!(
            expr,
            Expr::BinOp {
                op: BinOp::Concat,
                ..
            }
        ));
    }

    #[test]
    fn parse_multiple_clauses() {
        let prog = parse("x = 10; say x + 5");
        assert_eq!(prog.clauses.len(), 2);
        assert!(matches!(
            &prog.clauses[0].kind,
            ClauseKind::Assignment { .. }
        ));
        assert!(matches!(&prog.clauses[1].kind, ClauseKind::Say(_)));
    }

    #[test]
    fn parse_unmatched_paren_error() {
        let mut lexer = Lexer::new("(2 + 3");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let result = parser.parse();
        assert!(result.is_err());
    }

    #[test]
    fn parse_comparison() {
        let expr = parse_expr("3 > 2");
        assert!(matches!(expr, Expr::BinOp { op: BinOp::Gt, .. }));
    }

    #[test]
    fn parse_function_call() {
        // Note: function call requires no space before paren
        let src = "length('hello')";
        let prog = parse(src);
        match &prog.clauses[0].kind {
            ClauseKind::Command(Expr::FunctionCall { name, args }) => {
                assert_eq!(name, "LENGTH");
                assert_eq!(args.len(), 1);
            }
            other => panic!("expected FunctionCall, got {other:?}"),
        }
    }
}
