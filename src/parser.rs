//! REXX recursive descent parser — tokens to AST.
//!
//! Consumes a `Vec<Token>` from the lexer and produces an `ast::Program`.
//! REXX has no reserved words; keywords like SAY, IF, DO are just symbols
//! recognised by context at the start of a clause.

use crate::ast::{
    AddressAction, AssignTarget, BinOp, Clause, ClauseKind, Condition, ControlledLoop, DoBlock,
    DoKind, Expr, ParseSource, ParseTemplate, Program, SignalAction, TemplateElement, UnaryOp,
};
use crate::error::{RexxDiagnostic, RexxError, RexxResult, SourceLoc};
use crate::lexer::{Token, TokenKind};

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    /// Depth counter: >0 while parsing a DO header so that symbols like
    /// TO, BY, FOR, WHILE, UNTIL are not consumed by implicit concatenation.
    do_header_depth: usize,
    /// Depth counter: >0 while parsing IF condition / WHEN condition so that
    /// THEN is not consumed by implicit concatenation.
    condition_depth: usize,
    /// Depth counter: >0 while parsing IF THEN/ELSE clauses so that
    /// ELSE is not consumed by implicit concatenation.
    if_depth: usize,
    /// Depth counter: >0 while parsing PARSE VALUE expr WITH so that
    /// WITH is not consumed by implicit concatenation.
    parse_value_depth: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            pos: 0,
            do_header_depth: 0,
            condition_depth: 0,
            if_depth: 0,
            parse_value_depth: 0,
        }
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

    /// Check if current token is a symbol matching the given keyword.
    fn is_keyword(&self, keyword: &str) -> bool {
        if let TokenKind::Symbol(name) = self.peek_kind() {
            Self::check_keyword(name, keyword)
        } else {
            false
        }
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

    /// Check if the current token is THEN (stops implicit concat in conditions).
    fn is_then_keyword(&self) -> bool {
        if let TokenKind::Symbol(name) = self.peek_kind() {
            Self::check_keyword(name, "THEN")
        } else {
            false
        }
    }

    /// Check if the current token is ELSE (stops implicit concat in IF clauses).
    fn is_else_keyword(&self) -> bool {
        if let TokenKind::Symbol(name) = self.peek_kind() {
            Self::check_keyword(name, "ELSE")
        } else {
            false
        }
    }

    /// Check if the current token is WITH (stops implicit concat in PARSE VALUE).
    fn is_with_keyword(&self) -> bool {
        if let TokenKind::Symbol(name) = self.peek_kind() {
            Self::check_keyword(name, "WITH")
        } else {
            false
        }
    }

    /// Check if the current token is a DO-header keyword that should stop
    /// implicit concatenation.
    fn is_do_header_keyword(&self) -> bool {
        if let TokenKind::Symbol(name) = self.peek_kind() {
            name.eq_ignore_ascii_case("TO")
                || name.eq_ignore_ascii_case("BY")
                || name.eq_ignore_ascii_case("FOR")
                || name.eq_ignore_ascii_case("WHILE")
                || name.eq_ignore_ascii_case("UNTIL")
        } else {
            false
        }
    }

    // ── clause parsing ──────────────────────────────────────────────

    #[allow(clippy::too_many_lines)]
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

            // IF instruction
            if Self::check_keyword(name, "IF") {
                return self.parse_if();
            }

            // DO instruction
            if Self::check_keyword(name, "DO") {
                return self.parse_do();
            }

            // SELECT instruction
            if Self::check_keyword(name, "SELECT") {
                return self.parse_select();
            }

            // LEAVE instruction
            if Self::check_keyword(name, "LEAVE") {
                return Ok(self.parse_leave());
            }

            // ITERATE instruction
            if Self::check_keyword(name, "ITERATE") {
                return Ok(self.parse_iterate());
            }

            // EXIT instruction
            if Self::check_keyword(name, "EXIT") {
                return self.parse_exit();
            }

            // RETURN instruction
            if Self::check_keyword(name, "RETURN") {
                return self.parse_return();
            }

            // CALL instruction
            if Self::check_keyword(name, "CALL") {
                return self.parse_call();
            }

            // PROCEDURE instruction
            if Self::check_keyword(name, "PROCEDURE") {
                return Ok(self.parse_procedure());
            }

            // PARSE instruction
            if Self::check_keyword(name, "PARSE") {
                return self.parse_parse();
            }

            // PULL instruction
            if Self::check_keyword(name, "PULL") {
                return self.parse_pull();
            }

            // ARG instruction
            if Self::check_keyword(name, "ARG") {
                return self.parse_arg();
            }

            // DROP instruction
            if Self::check_keyword(name, "DROP") {
                return Ok(self.parse_drop());
            }

            // SIGNAL instruction
            if Self::check_keyword(name, "SIGNAL") {
                return self.parse_signal();
            }

            // INTERPRET instruction
            if Self::check_keyword(name, "INTERPRET") {
                return self.parse_interpret();
            }

            // ADDRESS instruction
            if Self::check_keyword(name, "ADDRESS") {
                return self.parse_address();
            }

            // Stray END outside DO/SELECT
            if Self::check_keyword(name, "END") {
                return Err(RexxDiagnostic::new(RexxError::UnexpectedEnd)
                    .at(loc)
                    .with_detail("END without matching DO or SELECT"));
            }

            // Stray THEN/ELSE
            if Self::check_keyword(name, "THEN") || Self::check_keyword(name, "ELSE") {
                return Err(RexxDiagnostic::new(RexxError::UnexpectedThenElse)
                    .at(loc)
                    .with_detail(format!("unexpected {}", name.to_uppercase())));
            }

            // Stray WHEN/OTHERWISE
            if Self::check_keyword(name, "WHEN") || Self::check_keyword(name, "OTHERWISE") {
                return Err(RexxDiagnostic::new(RexxError::UnexpectedWhenOtherwise)
                    .at(loc)
                    .with_detail(format!("unexpected {}", name.to_uppercase())));
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
            unreachable!("parse_assignment called on non-symbol token")
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

    // ── control flow parsing ────────────────────────────────────────

    /// Parse: IF expr THEN clause [ELSE clause]
    fn parse_if(&mut self) -> RexxResult<Clause> {
        let loc = self.loc();
        self.advance(); // consume IF

        // Parse condition expression (with THEN suppression)
        self.condition_depth += 1;
        let condition = self.parse_expression()?;
        self.condition_depth -= 1;

        // Skip terminators before THEN
        self.skip_terminators();

        // Expect THEN keyword
        if !self.is_keyword("THEN") {
            return Err(RexxDiagnostic::new(RexxError::ExpectedThen)
                .at(self.loc())
                .with_detail("expected THEN after IF condition"));
        }
        self.advance(); // consume THEN

        // Skip terminators after THEN
        self.skip_terminators();

        // Parse one clause for THEN branch (with ELSE suppression)
        self.if_depth += 1;
        let then_clause = Box::new(self.parse_clause()?);

        // Check for ELSE: skip terminators and look for ELSE keyword
        let saved_pos = self.pos;
        self.skip_terminators();
        let else_clause = if self.is_keyword("ELSE") {
            self.advance(); // consume ELSE
            self.skip_terminators();
            let clause = self.parse_clause()?;
            Some(Box::new(clause))
        } else {
            // Restore position — those terminators might be meaningful
            self.pos = saved_pos;
            None
        };
        self.if_depth -= 1;

        Ok(Clause {
            kind: ClauseKind::If {
                condition,
                then_clause,
                else_clause,
            },
            loc,
        })
    }

    /// Parse: DO [variant]; body; END [name]
    fn parse_do(&mut self) -> RexxResult<Clause> {
        let loc = self.loc();
        self.advance(); // consume DO

        // Disambiguate variant
        // 1. DO; ... END  (simple) — next is terminator
        // 2. DO FOREVER   — next is Symbol("FOREVER")
        // 3. DO WHILE expr — next is Symbol("WHILE")
        // 4. DO UNTIL expr — next is Symbol("UNTIL")
        // 5. DO var = start [TO..BY..FOR..WHILE..UNTIL] — next is Symbol, peek(+1) is =
        // 6. DO expr — counted loop

        if self.is_terminator() {
            // Simple DO block
            self.skip_terminators();
            let body = self.parse_do_body()?;
            return Ok(Clause {
                kind: ClauseKind::Do(Box::new(DoBlock {
                    kind: DoKind::Simple,
                    body,
                    name: None,
                })),
                loc,
            });
        }

        if self.is_keyword("FOREVER") {
            self.advance(); // consume FOREVER
            self.skip_terminators();
            let body = self.parse_do_body()?;
            return Ok(Clause {
                kind: ClauseKind::Do(Box::new(DoBlock {
                    kind: DoKind::Forever,
                    body,
                    name: None,
                })),
                loc,
            });
        }

        if self.is_keyword("WHILE") {
            self.advance(); // consume WHILE
            let cond = self.parse_expression()?;
            self.skip_terminators();
            let body = self.parse_do_body()?;
            return Ok(Clause {
                kind: ClauseKind::Do(Box::new(DoBlock {
                    kind: DoKind::While(cond),
                    body,
                    name: None,
                })),
                loc,
            });
        }

        if self.is_keyword("UNTIL") {
            self.advance(); // consume UNTIL
            let cond = self.parse_expression()?;
            self.skip_terminators();
            let body = self.parse_do_body()?;
            return Ok(Clause {
                kind: ClauseKind::Do(Box::new(DoBlock {
                    kind: DoKind::Until(cond),
                    body,
                    name: None,
                })),
                loc,
            });
        }

        // Check for controlled loop: Symbol followed by =
        if let TokenKind::Symbol(_) = self.peek_kind()
            && matches!(self.peek_at(1), TokenKind::Assign)
        {
            return self.parse_controlled_do(loc);
        }

        // Counted DO: DO expr
        let count_expr = self.parse_expression()?;
        self.skip_terminators();
        let body = self.parse_do_body()?;
        Ok(Clause {
            kind: ClauseKind::Do(Box::new(DoBlock {
                kind: DoKind::Count(count_expr),
                body,
                name: None,
            })),
            loc,
        })
    }

    /// Parse controlled DO: DO var = start [TO limit] [BY step] [FOR count] [WHILE cond] [UNTIL cond]
    fn parse_controlled_do(&mut self, loc: SourceLoc) -> RexxResult<Clause> {
        let var_name = if let TokenKind::Symbol(s) = self.peek_kind() {
            s.to_uppercase()
        } else {
            unreachable!()
        };
        self.advance(); // consume variable name
        self.advance(); // consume =

        // Parse start expression with do_header_depth guard
        self.do_header_depth += 1;
        let start = self.parse_expression()?;

        let mut to: Option<Expr> = None;
        let mut by: Option<Expr> = None;
        let mut r#for: Option<Expr> = None;
        let mut while_cond: Option<Expr> = None;
        let mut until_cond: Option<Expr> = None;

        // Parse optional TO/BY/FOR/WHILE/UNTIL in any order.
        // Duplicate keywords are rejected per REXX (Error 27).
        loop {
            if self.is_keyword("TO") {
                if to.is_some() {
                    return Err(RexxDiagnostic::new(RexxError::InvalidDoSyntax)
                        .at(self.loc())
                        .with_detail("duplicate TO in DO instruction"));
                }
                self.advance();
                to = Some(self.parse_expression()?);
            } else if self.is_keyword("BY") {
                if by.is_some() {
                    return Err(RexxDiagnostic::new(RexxError::InvalidDoSyntax)
                        .at(self.loc())
                        .with_detail("duplicate BY in DO instruction"));
                }
                self.advance();
                by = Some(self.parse_expression()?);
            } else if self.is_keyword("FOR") {
                if r#for.is_some() {
                    return Err(RexxDiagnostic::new(RexxError::InvalidDoSyntax)
                        .at(self.loc())
                        .with_detail("duplicate FOR in DO instruction"));
                }
                self.advance();
                r#for = Some(self.parse_expression()?);
            } else if self.is_keyword("WHILE") {
                if while_cond.is_some() {
                    return Err(RexxDiagnostic::new(RexxError::InvalidDoSyntax)
                        .at(self.loc())
                        .with_detail("duplicate WHILE in DO instruction"));
                }
                self.advance();
                while_cond = Some(self.parse_expression()?);
            } else if self.is_keyword("UNTIL") {
                if until_cond.is_some() {
                    return Err(RexxDiagnostic::new(RexxError::InvalidDoSyntax)
                        .at(self.loc())
                        .with_detail("duplicate UNTIL in DO instruction"));
                }
                self.advance();
                until_cond = Some(self.parse_expression()?);
            } else {
                break;
            }
        }

        self.do_header_depth -= 1;

        self.skip_terminators();
        let body = self.parse_do_body()?;

        Ok(Clause {
            kind: ClauseKind::Do(Box::new(DoBlock {
                kind: DoKind::Controlled(Box::new(ControlledLoop {
                    var: var_name.clone(),
                    start,
                    to,
                    by,
                    r#for,
                    while_cond,
                    until_cond,
                })),
                body,
                name: Some(var_name),
            })),
            loc,
        })
    }

    /// Parse DO body: clauses until END [name]
    fn parse_do_body(&mut self) -> RexxResult<Vec<Clause>> {
        let mut body = Vec::new();
        self.skip_terminators();
        loop {
            if self.at_end() {
                return Err(RexxDiagnostic::new(RexxError::IncompleteBlock)
                    .at(self.loc())
                    .with_detail("expected END to close DO block"));
            }
            if self.is_keyword("END") {
                self.advance(); // consume END
                // Optionally consume a symbol after END (e.g., END i)
                if let TokenKind::Symbol(_) = self.peek_kind()
                    && !self.is_terminator()
                {
                    self.advance(); // consume the name after END
                }
                break;
            }
            body.push(self.parse_clause()?);
            self.skip_terminators();
        }
        Ok(body)
    }

    /// Parse: SELECT; WHEN expr THEN clause...; ... [OTHERWISE; clause...;] END
    fn parse_select(&mut self) -> RexxResult<Clause> {
        let loc = self.loc();
        self.advance(); // consume SELECT
        self.skip_terminators();

        let mut when_clauses: Vec<(Expr, Vec<Clause>)> = Vec::new();
        let mut otherwise: Option<Vec<Clause>> = None;

        loop {
            self.skip_terminators();

            if self.at_end() {
                return Err(RexxDiagnostic::new(RexxError::IncompleteBlock)
                    .at(self.loc())
                    .with_detail("expected END to close SELECT"));
            }

            if self.is_keyword("END") {
                self.advance(); // consume END
                break;
            }

            if self.is_keyword("WHEN") {
                self.advance(); // consume WHEN
                self.condition_depth += 1;
                let condition = self.parse_expression()?;
                self.condition_depth -= 1;
                self.skip_terminators();

                if !self.is_keyword("THEN") {
                    return Err(RexxDiagnostic::new(RexxError::ExpectedThen)
                        .at(self.loc())
                        .with_detail("expected THEN after WHEN condition"));
                }
                self.advance(); // consume THEN
                self.skip_terminators();

                // Parse one or more clauses for this WHEN
                let mut body = Vec::new();
                loop {
                    if self.at_end()
                        || self.is_keyword("WHEN")
                        || self.is_keyword("OTHERWISE")
                        || self.is_keyword("END")
                    {
                        break;
                    }
                    body.push(self.parse_clause()?);
                    self.skip_terminators();
                }
                when_clauses.push((condition, body));
                continue;
            }

            if self.is_keyword("OTHERWISE") {
                self.advance(); // consume OTHERWISE
                self.skip_terminators();

                let mut body = Vec::new();
                loop {
                    if self.at_end() || self.is_keyword("END") {
                        break;
                    }
                    body.push(self.parse_clause()?);
                    self.skip_terminators();
                }
                otherwise = Some(body);
                continue;
            }

            return Err(RexxDiagnostic::new(RexxError::ExpectedWhenOtherwise)
                .at(self.loc())
                .with_detail("expected WHEN, OTHERWISE, or END in SELECT"));
        }

        Ok(Clause {
            kind: ClauseKind::Select {
                when_clauses,
                otherwise,
            },
            loc,
        })
    }

    /// Parse: LEAVE [name]
    fn parse_leave(&mut self) -> Clause {
        let loc = self.loc();
        self.advance(); // consume LEAVE
        let name = self.try_consume_symbol_name();
        Clause {
            kind: ClauseKind::Leave(name),
            loc,
        }
    }

    /// Parse: ITERATE [name]
    fn parse_iterate(&mut self) -> Clause {
        let loc = self.loc();
        self.advance(); // consume ITERATE
        let name = self.try_consume_symbol_name();
        Clause {
            kind: ClauseKind::Iterate(name),
            loc,
        }
    }

    /// Try to consume an optional symbol name (for LEAVE/ITERATE).
    fn try_consume_symbol_name(&mut self) -> Option<String> {
        if self.is_terminator() {
            return None;
        }
        if let TokenKind::Symbol(s) = self.peek_kind() {
            let n = s.to_uppercase();
            self.advance();
            Some(n)
        } else {
            None
        }
    }

    /// Parse: EXIT [expr]
    fn parse_exit(&mut self) -> RexxResult<Clause> {
        let loc = self.loc();
        self.advance(); // consume EXIT
        let expr = if self.is_terminator() {
            None
        } else {
            Some(self.parse_expression()?)
        };
        Ok(Clause {
            kind: ClauseKind::Exit(expr),
            loc,
        })
    }

    /// Parse: RETURN [expr]
    fn parse_return(&mut self) -> RexxResult<Clause> {
        let loc = self.loc();
        self.advance(); // consume RETURN
        let expr = if self.is_terminator() {
            None
        } else {
            Some(self.parse_expression()?)
        };
        Ok(Clause {
            kind: ClauseKind::Return(expr),
            loc,
        })
    }

    /// Parse: CALL name [expr [, expr]...]
    fn parse_call(&mut self) -> RexxResult<Clause> {
        let loc = self.loc();
        self.advance(); // consume CALL

        // Read routine name
        let name = if let TokenKind::Symbol(s) = self.peek_kind() {
            let n = s.to_uppercase();
            self.advance();
            n
        } else {
            return Err(RexxDiagnostic::new(RexxError::ExpectedSymbol)
                .at(self.loc())
                .with_detail("expected routine name after CALL"));
        };

        // Parse optional comma-separated arguments
        let mut args = Vec::new();
        if !self.is_terminator() {
            args.push(self.parse_expression()?);
            while matches!(self.peek_kind(), TokenKind::Comma) {
                self.advance(); // consume comma
                args.push(self.parse_expression()?);
            }
        }

        Ok(Clause {
            kind: ClauseKind::Call { name, args },
            loc,
        })
    }

    /// Parse: PROCEDURE [EXPOSE name [name...]]
    fn parse_procedure(&mut self) -> Clause {
        let loc = self.loc();
        self.advance(); // consume PROCEDURE

        let expose = if self.is_keyword("EXPOSE") {
            self.advance(); // consume EXPOSE
            let mut names = Vec::new();
            while !self.is_terminator() {
                if let TokenKind::Symbol(s) = self.peek_kind() {
                    names.push(s.to_uppercase());
                    self.advance();
                } else {
                    break;
                }
            }
            Some(names)
        } else {
            None
        };

        Clause {
            kind: ClauseKind::Procedure(expose),
            loc,
        }
    }

    /// Parse a PARSE template: sequence of variables, dots, literal patterns,
    /// positional patterns, variable patterns, and commas.
    fn parse_template(&mut self) -> RexxResult<ParseTemplate> {
        let mut elements = Vec::new();
        while !self.is_terminator() {
            match self.peek_kind().clone() {
                TokenKind::Symbol(name) => {
                    elements.push(TemplateElement::Variable(name.to_uppercase()));
                    self.advance();
                }
                TokenKind::Dot => {
                    elements.push(TemplateElement::Dot);
                    self.advance();
                }
                TokenKind::StringLit(s) => {
                    elements.push(TemplateElement::Literal(s));
                    self.advance();
                }
                TokenKind::Number(n) => {
                    elements.push(TemplateElement::AbsolutePos(Expr::Number(n)));
                    self.advance();
                }
                TokenKind::Plus => {
                    self.advance(); // consume +
                    if let TokenKind::Number(n) = self.peek_kind().clone() {
                        self.advance();
                        let val: i32 = n.parse().map_err(|_| {
                            RexxDiagnostic::new(RexxError::InvalidTemplate)
                                .at(self.loc())
                                .with_detail(format!("invalid relative position '+{n}'"))
                        })?;
                        elements.push(TemplateElement::RelativePos(val));
                    } else {
                        return Err(RexxDiagnostic::new(RexxError::InvalidTemplate)
                            .at(self.loc())
                            .with_detail("expected number after '+' in template"));
                    }
                }
                TokenKind::Minus => {
                    self.advance(); // consume -
                    if let TokenKind::Number(n) = self.peek_kind().clone() {
                        self.advance();
                        let val: i32 = n.parse().map_err(|_| {
                            RexxDiagnostic::new(RexxError::InvalidTemplate)
                                .at(self.loc())
                                .with_detail(format!("invalid relative position '-{n}'"))
                        })?;
                        elements.push(TemplateElement::RelativePos(-val));
                    } else {
                        return Err(RexxDiagnostic::new(RexxError::InvalidTemplate)
                            .at(self.loc())
                            .with_detail("expected number after '-' in template"));
                    }
                }
                TokenKind::LeftParen => {
                    self.advance(); // consume (
                    if let TokenKind::Symbol(name) = self.peek_kind().clone() {
                        let var_name = name.to_uppercase();
                        self.advance(); // consume symbol
                        let err_loc = self.loc();
                        self.expect(&TokenKind::RightParen).map_err(|_| {
                            RexxDiagnostic::new(RexxError::InvalidTemplate)
                                .at(err_loc)
                                .with_detail("expected ')' after variable pattern name")
                        })?;
                        elements.push(TemplateElement::VariablePattern(var_name));
                    } else {
                        return Err(RexxDiagnostic::new(RexxError::InvalidTemplate)
                            .at(self.loc())
                            .with_detail("expected symbol inside '(' ')' in template"));
                    }
                }
                TokenKind::Comma => {
                    elements.push(TemplateElement::Comma);
                    self.advance();
                }
                _ => break,
            }
        }
        Ok(ParseTemplate { elements })
    }

    /// Parse: PARSE [UPPER] source template
    fn parse_parse(&mut self) -> RexxResult<Clause> {
        let loc = self.loc();
        self.advance(); // consume PARSE

        // Check for UPPER
        let upper = if self.is_keyword("UPPER") {
            self.advance();
            true
        } else {
            false
        };

        // Dispatch on source keyword
        let source = if self.is_keyword("ARG") {
            self.advance();
            ParseSource::Arg
        } else if self.is_keyword("PULL") {
            self.advance();
            ParseSource::Pull
        } else if self.is_keyword("SOURCE") {
            self.advance();
            ParseSource::Source
        } else if self.is_keyword("VERSION") {
            self.advance();
            ParseSource::Version
        } else if self.is_keyword("LINEIN") {
            self.advance();
            ParseSource::LineIn
        } else if self.is_keyword("VAR") {
            self.advance();
            if let TokenKind::Symbol(name) = self.peek_kind().clone() {
                let var_name = name.to_uppercase();
                self.advance();
                ParseSource::Var(var_name)
            } else {
                return Err(RexxDiagnostic::new(RexxError::ExpectedSymbol)
                    .at(self.loc())
                    .with_detail("expected variable name after PARSE VAR"));
            }
        } else if self.is_keyword("VALUE") {
            self.advance();
            self.parse_value_depth += 1;
            let expr = self.parse_expression();
            self.parse_value_depth -= 1;
            let expr = expr?;
            // Expect WITH keyword
            if !self.is_with_keyword() {
                return Err(RexxDiagnostic::new(RexxError::InvalidSubKeyword)
                    .at(self.loc())
                    .with_detail("expected WITH after PARSE VALUE expression"));
            }
            self.advance(); // consume WITH
            ParseSource::Value(expr)
        } else {
            return Err(RexxDiagnostic::new(RexxError::InvalidSubKeyword)
                .at(self.loc())
                .with_detail(
                    "expected ARG, PULL, SOURCE, VERSION, LINEIN, VAR, or VALUE after PARSE",
                ));
        };

        let template = if self.is_terminator() {
            ParseTemplate { elements: vec![] }
        } else {
            self.parse_template()?
        };

        Ok(Clause {
            kind: ClauseKind::Parse {
                upper,
                source,
                template,
            },
            loc,
        })
    }

    /// Parse: PULL [template]
    fn parse_pull(&mut self) -> RexxResult<Clause> {
        let loc = self.loc();
        self.advance(); // consume PULL

        let template = if self.is_terminator() {
            None
        } else {
            Some(self.parse_template()?)
        };

        Ok(Clause {
            kind: ClauseKind::Pull(template),
            loc,
        })
    }

    /// Parse: ARG [template]
    fn parse_arg(&mut self) -> RexxResult<Clause> {
        let loc = self.loc();
        self.advance(); // consume ARG
        let template = if self.is_terminator() {
            ParseTemplate { elements: vec![] }
        } else {
            self.parse_template()?
        };
        Ok(Clause {
            kind: ClauseKind::Arg(template),
            loc,
        })
    }

    /// Parse: DROP name [name...]
    fn parse_drop(&mut self) -> Clause {
        let loc = self.loc();
        self.advance(); // consume DROP

        let mut names = Vec::new();
        while !self.is_terminator() {
            if let TokenKind::Symbol(s) = self.peek_kind() {
                names.push(s.to_uppercase());
                self.advance();
            } else {
                break;
            }
        }

        Clause {
            kind: ClauseKind::Drop(names),
            loc,
        }
    }

    // ── SIGNAL parsing ───────────────────────────────────────────────

    /// Parse: SIGNAL label | SIGNAL VALUE expr | SIGNAL ON condition [NAME label] | SIGNAL OFF condition
    fn parse_signal(&mut self) -> RexxResult<Clause> {
        let loc = self.loc();
        self.advance(); // consume SIGNAL

        if self.is_keyword("ON") {
            self.advance(); // consume ON
            let condition = self.parse_condition()?;
            let name = if self.is_keyword("NAME") {
                self.advance(); // consume NAME
                if let TokenKind::Symbol(s) = self.peek_kind() {
                    let n = s.to_uppercase();
                    self.advance();
                    Some(n)
                } else {
                    return Err(RexxDiagnostic::new(RexxError::ExpectedSymbol)
                        .at(self.loc())
                        .with_detail("expected label name after SIGNAL ON condition NAME"));
                }
            } else {
                None
            };
            return Ok(Clause {
                kind: ClauseKind::Signal(SignalAction::On { condition, name }),
                loc,
            });
        }

        if self.is_keyword("OFF") {
            self.advance(); // consume OFF
            let condition = self.parse_condition()?;
            return Ok(Clause {
                kind: ClauseKind::Signal(SignalAction::Off(condition)),
                loc,
            });
        }

        if self.is_keyword("VALUE") {
            self.advance(); // consume VALUE
            let expr = self.parse_expression()?;
            return Ok(Clause {
                kind: ClauseKind::Signal(SignalAction::Value(expr)),
                loc,
            });
        }

        // SIGNAL label — must be a symbol
        if let TokenKind::Symbol(s) = self.peek_kind() {
            let label = s.to_uppercase();
            self.advance();
            return Ok(Clause {
                kind: ClauseKind::Signal(SignalAction::Label(label)),
                loc,
            });
        }

        Err(RexxDiagnostic::new(RexxError::ExpectedSymbol)
            .at(self.loc())
            .with_detail("expected label name, VALUE, ON, or OFF after SIGNAL"))
    }

    /// Parse a condition name: ERROR, FAILURE, HALT, NOVALUE, NOTREADY, SYNTAX, LOSTDIGITS
    fn parse_condition(&mut self) -> RexxResult<Condition> {
        if let TokenKind::Symbol(s) = self.peek_kind() {
            let upper = s.to_uppercase();
            let condition = match upper.as_str() {
                "ERROR" => Condition::Error,
                "FAILURE" => Condition::Failure,
                "HALT" => Condition::Halt,
                "NOVALUE" => Condition::NoValue,
                "NOTREADY" => Condition::NotReady,
                "SYNTAX" => Condition::Syntax,
                "LOSTDIGITS" => Condition::LostDigits,
                _ => {
                    return Err(RexxDiagnostic::new(RexxError::InvalidSubKeyword)
                        .at(self.loc())
                        .with_detail(format!(
                            "'{upper}' is not a valid condition name; expected ERROR, FAILURE, HALT, NOVALUE, NOTREADY, SYNTAX, or LOSTDIGITS"
                        )));
                }
            };
            self.advance();
            Ok(condition)
        } else {
            Err(RexxDiagnostic::new(RexxError::ExpectedSymbol)
                .at(self.loc())
                .with_detail("expected condition name"))
        }
    }

    // ── INTERPRET parsing ─────────────────────────────────────────────

    /// Parse: INTERPRET expr
    fn parse_interpret(&mut self) -> RexxResult<Clause> {
        let loc = self.loc();
        self.advance(); // consume INTERPRET
        let expr = if self.is_terminator() {
            Expr::StringLit(String::new())
        } else {
            self.parse_expression()?
        };
        Ok(Clause {
            kind: ClauseKind::Interpret(expr),
            loc,
        })
    }

    // ── ADDRESS parsing ───────────────────────────────────────────────

    /// Parse: ADDRESS [env [command]] | ADDRESS VALUE expr | ADDRESS (swap)
    fn parse_address(&mut self) -> RexxResult<Clause> {
        let loc = self.loc();
        self.advance(); // consume ADDRESS

        // Bare ADDRESS → swap default ↔ previous
        if self.is_terminator() {
            return Ok(Clause {
                kind: ClauseKind::Address(AddressAction::SetEnvironment(String::new())),
                loc,
            });
        }

        // ADDRESS VALUE expr → dynamic environment name
        if self.is_keyword("VALUE") {
            self.advance(); // consume VALUE
            let expr = self.parse_expression()?;
            return Ok(Clause {
                kind: ClauseKind::Address(AddressAction::Value(expr)),
                loc,
            });
        }

        // ADDRESS env [command]
        if let TokenKind::Symbol(name) = self.peek_kind().clone() {
            let env_name = name.to_uppercase();
            self.advance(); // consume environment name

            if self.is_terminator() {
                // ADDRESS env — set default
                return Ok(Clause {
                    kind: ClauseKind::Address(AddressAction::SetEnvironment(env_name)),
                    loc,
                });
            }

            // ADDRESS env command — one-shot
            let command = self.parse_expression()?;
            return Ok(Clause {
                kind: ClauseKind::Address(AddressAction::Temporary {
                    environment: env_name,
                    command,
                }),
                loc,
            });
        }

        Err(RexxDiagnostic::new(RexxError::ExpectedSymbol)
            .at(self.loc())
            .with_detail("expected environment name, VALUE, or end of clause after ADDRESS"))
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

            // When inside a DO header, suppress implicit concatenation for
            // DO-header keywords so they can be consumed by the header parser.
            if self.do_header_depth > 0 && self.is_do_header_keyword() {
                break;
            }

            // When inside a condition (IF/WHEN), suppress implicit concatenation
            // for THEN so it can be consumed by the control flow parser.
            if self.condition_depth > 0 && self.is_then_keyword() {
                break;
            }

            // When inside an IF, suppress implicit concatenation for ELSE
            // so it can be consumed by the IF parser.
            if self.if_depth > 0 && self.is_else_keyword() {
                break;
            }

            // When inside PARSE VALUE, suppress implicit concatenation for WITH
            // so it can be consumed by the PARSE parser.
            if self.parse_value_depth > 0 && self.is_with_keyword() {
                break;
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

    #[test]
    fn parse_if_then() {
        let prog = parse("if 1 then say 'yes'");
        assert!(matches!(&prog.clauses[0].kind, ClauseKind::If { .. }));
    }

    #[test]
    fn parse_if_then_else() {
        let prog = parse("if 0 then say 'no'; else say 'yes'");
        match &prog.clauses[0].kind {
            ClauseKind::If { else_clause, .. } => {
                assert!(else_clause.is_some());
            }
            other => panic!("expected If, got {other:?}"),
        }
    }

    #[test]
    fn parse_simple_do() {
        let prog = parse("do; say 'a'; end");
        match &prog.clauses[0].kind {
            ClauseKind::Do(block) => {
                assert!(matches!(block.kind, DoKind::Simple));
                assert_eq!(block.body.len(), 1);
            }
            other => panic!("expected Do, got {other:?}"),
        }
    }

    #[test]
    fn parse_do_count() {
        let prog = parse("do 3; say 'x'; end");
        match &prog.clauses[0].kind {
            ClauseKind::Do(block) => {
                assert!(matches!(block.kind, DoKind::Count(_)));
            }
            other => panic!("expected Do Count, got {other:?}"),
        }
    }

    #[test]
    fn parse_controlled_do() {
        let prog = parse("do i = 1 to 5; say i; end");
        match &prog.clauses[0].kind {
            ClauseKind::Do(block) => {
                assert!(matches!(block.kind, DoKind::Controlled(_)));
            }
            other => panic!("expected Do Controlled, got {other:?}"),
        }
    }

    #[test]
    fn parse_select() {
        let prog = parse("select; when 1 then say 'one'; otherwise say 'other'; end");
        assert!(matches!(&prog.clauses[0].kind, ClauseKind::Select { .. }));
    }

    #[test]
    fn parse_leave() {
        let prog = parse("do forever; leave; end");
        match &prog.clauses[0].kind {
            ClauseKind::Do(block) => {
                assert!(matches!(&block.body[0].kind, ClauseKind::Leave(None)));
            }
            other => panic!("expected Do, got {other:?}"),
        }
    }

    #[test]
    fn parse_exit() {
        let prog = parse("exit");
        assert!(matches!(&prog.clauses[0].kind, ClauseKind::Exit(None)));
    }

    #[test]
    fn parse_exit_with_expr() {
        let prog = parse("exit 0");
        assert!(matches!(&prog.clauses[0].kind, ClauseKind::Exit(Some(_))));
    }
}
