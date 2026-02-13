//! REXX tree-walking evaluator — AST + Environment -> execution.
//!
//! Walks the AST produced by the parser, evaluating expressions using
//! `RexxValue` and `BigDecimal` arithmetic, and executing instructions
//! against an `Environment`.

use bigdecimal::{BigDecimal, Zero};
use std::collections::{HashMap, VecDeque};
use std::fmt;
use std::path::Path;
use std::str::FromStr;

use crate::ast::{
    AddressAction, AssignTarget, BinOp, Clause, ClauseKind, Condition, ControlledLoop, DoBlock,
    DoKind, Expr, NumericFormSetting, NumericSetting, ParseSource, ParseTemplate, Program,
    SignalAction, TailElement, TemplateElement, UnaryOp,
};
use crate::env::Environment;
use crate::error::{RexxDiagnostic, RexxError, RexxResult};
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::value::{NumericSettings, RexxValue};

/// Maximum nesting depth for INTERPRET to prevent stack overflow.
const MAX_INTERPRET_DEPTH: usize = 100;

/// REXX trace levels, ordered from least to most verbose.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum TraceLevel {
    Off,
    Normal,
    Failure,
    Errors,
    Commands,
    Labels,
    Results,
    Intermediates,
    All,
}

impl TraceLevel {
    /// Parse a single-letter or full-word trace level (case-insensitive).
    fn parse(s: &str) -> Option<Self> {
        let upper = s.trim().to_uppercase();
        match upper.as_str() {
            "O" | "OFF" => Some(Self::Off),
            "N" | "NORMAL" => Some(Self::Normal),
            "F" | "FAILURE" => Some(Self::Failure),
            "E" | "ERRORS" => Some(Self::Errors),
            "C" | "COMMANDS" => Some(Self::Commands),
            "L" | "LABELS" => Some(Self::Labels),
            "R" | "RESULTS" => Some(Self::Results),
            "I" | "INTERMEDIATES" => Some(Self::Intermediates),
            "A" | "ALL" => Some(Self::All),
            _ => None,
        }
    }

    /// Return the single-letter representation.
    fn letter(self) -> char {
        match self {
            Self::Off => 'O',
            Self::Normal => 'N',
            Self::Failure => 'F',
            Self::Errors => 'E',
            Self::Commands => 'C',
            Self::Labels => 'L',
            Self::Results => 'R',
            Self::Intermediates => 'I',
            Self::All => 'A',
        }
    }
}

/// Combined trace setting: level + interactive flag.
#[derive(Debug, Clone)]
struct TraceSetting {
    level: TraceLevel,
    interactive: bool,
}

/// Result of parsing a trace setting string.
enum TraceAction {
    /// "?" alone — toggle interactive mode, keep current level.
    ToggleInteractive,
    /// A concrete setting (e.g., "R", "?R", "OFF").
    Set(TraceSetting),
}

impl TraceAction {
    /// Parse a trace setting string like "R", "?R", "?", "OFF", "?Results".
    fn parse(s: &str) -> Option<Self> {
        let trimmed = s.trim();
        if trimmed.is_empty() {
            return None;
        }
        if trimmed == "?" {
            return Some(Self::ToggleInteractive);
        }
        if let Some(rest) = trimmed.strip_prefix('?') {
            let level = TraceLevel::parse(rest)?;
            Some(Self::Set(TraceSetting {
                level,
                interactive: true,
            }))
        } else {
            let level = TraceLevel::parse(trimmed)?;
            Some(Self::Set(TraceSetting {
                level,
                interactive: false,
            }))
        }
    }
}

impl Default for TraceSetting {
    fn default() -> Self {
        Self {
            level: TraceLevel::Normal,
            interactive: false,
        }
    }
}

impl fmt::Display for TraceSetting {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.interactive {
            write!(f, "?{}", self.level.letter())
        } else {
            write!(f, "{}", self.level.letter())
        }
    }
}

/// Signal returned by clause/block execution for control flow.
pub enum ExecSignal {
    Normal,
    Leave(Option<String>),
    Iterate(Option<String>),
    Exit(Option<RexxValue>),
    Return(Option<RexxValue>),
    /// SIGNAL transfers control to a label, abandoning all blocks.
    Signal(String),
}

/// Pending EXIT state — distinguishes "no pending exit" from "exit with/without value".
enum PendingExit {
    None,
    WithValue(Option<RexxValue>),
}

impl PendingExit {
    /// If an EXIT is pending, take the value and reset to `None`.
    /// Returns the exit value wrapped in `ExecSignal::Exit` for immediate propagation.
    fn take_signal(&mut self) -> Option<ExecSignal> {
        match std::mem::replace(self, PendingExit::None) {
            PendingExit::None => Option::None,
            PendingExit::WithValue(v) => Some(ExecSignal::Exit(v)),
        }
    }

    const fn is_pending(&self) -> bool {
        matches!(self, PendingExit::WithValue(_))
    }
}

pub struct Evaluator<'a> {
    env: &'a mut Environment,
    program: &'a Program,
    settings: NumericSettings,
    labels: HashMap<String, usize>,
    arg_stack: Vec<Vec<RexxValue>>,
    pending_exit: PendingExit,
    /// Active condition traps: condition → target label name.
    traps: HashMap<Condition, String>,
    /// Pending signal from a trap (e.g., NOVALUE). Fires after clause completes.
    pending_signal: Option<String>,
    /// Current INTERPRET nesting depth (for recursion limit).
    interpret_depth: usize,
    /// Current external function call nesting depth (for recursion limit).
    external_depth: usize,
    /// Current TRACE setting (level + interactive flag).
    trace_setting: TraceSetting,
    /// External data queue for PUSH/QUEUE/PULL.
    queue: VecDeque<String>,
    /// Optional custom command handler for ADDRESS environments.
    /// Called as handler(address_env, command_string) -> return_code.
    /// If the handler returns `None`, the default shell execution is used.
    command_handler: Option<Box<dyn FnMut(&str, &str) -> Option<i32>>>,
}

impl<'a> Evaluator<'a> {
    pub fn new(env: &'a mut Environment, program: &'a Program) -> Self {
        let labels = Self::build_labels(program);
        Self {
            env,
            program,
            settings: NumericSettings::default(),
            labels,
            arg_stack: Vec::new(),
            pending_exit: PendingExit::None,
            traps: HashMap::new(),
            pending_signal: None,
            interpret_depth: 0,
            external_depth: 0,
            trace_setting: TraceSetting::default(),
            queue: VecDeque::new(),
            command_handler: None,
        }
    }

    fn build_labels(program: &Program) -> HashMap<String, usize> {
        let mut labels = HashMap::new();
        for (i, clause) in program.clauses.iter().enumerate() {
            if let ClauseKind::Label(name) = &clause.kind {
                labels.entry(name.clone()).or_insert(i);
            }
        }
        labels
    }

    pub fn exec(&mut self) -> RexxResult<ExecSignal> {
        let mut start = 0;
        loop {
            match self.exec_from(start)? {
                ExecSignal::Signal(label) => {
                    let &idx = self.labels.get(&label).ok_or_else(|| {
                        RexxDiagnostic::new(RexxError::LabelNotFound)
                            .with_detail(format!("label '{label}' not found"))
                    })?;
                    // Trace the label clause at Labels or All level per ANSI REXX
                    if matches!(
                        self.trace_setting.level,
                        TraceLevel::Labels | TraceLevel::All
                    ) {
                        self.trace_clause(&self.program.clauses[idx]);
                    }
                    start = idx + 1;
                }
                other => return Ok(other),
            }
        }
    }

    fn exec_from(&mut self, start: usize) -> RexxResult<ExecSignal> {
        for clause in &self.program.clauses[start..] {
            let signal = self.exec_clause_outer(clause)?;
            if let Some(signal) = self.pending_exit.take_signal() {
                return Ok(signal);
            }
            if let Some(label) = self.pending_signal.take() {
                return Ok(ExecSignal::Signal(label));
            }
            if !matches!(signal, ExecSignal::Normal) {
                return Ok(signal);
            }
        }
        Ok(ExecSignal::Normal)
    }

    fn exec_body(&mut self, body: &[Clause]) -> RexxResult<ExecSignal> {
        for clause in body {
            let signal = self.exec_clause_outer(clause)?;
            if let Some(signal) = self.pending_exit.take_signal() {
                return Ok(signal);
            }
            if let Some(label) = self.pending_signal.take() {
                return Ok(ExecSignal::Signal(label));
            }
            if !matches!(signal, ExecSignal::Normal) {
                return Ok(signal);
            }
        }
        Ok(ExecSignal::Normal)
    }

    /// Outer clause executor: wraps `exec_clause` with SYNTAX trap support and TRACE output.
    fn exec_clause_outer(&mut self, clause: &Clause) -> RexxResult<ExecSignal> {
        let trace_level = self.trace_setting.level;

        // Pre-execution trace: source line per ANSI REXX §8.3.36.
        // Labels: only at Labels or All level.
        // Commands: at Commands, Results, Intermediates, All.
        // Other clauses: at Results, Intermediates, All.
        let should_trace_source = match &clause.kind {
            ClauseKind::Label(_) => {
                matches!(trace_level, TraceLevel::Labels | TraceLevel::All)
            }
            ClauseKind::Command(_) => matches!(
                trace_level,
                TraceLevel::Commands
                    | TraceLevel::Results
                    | TraceLevel::Intermediates
                    | TraceLevel::All
            ),
            _ => matches!(
                trace_level,
                TraceLevel::Results | TraceLevel::Intermediates | TraceLevel::All
            ),
        };
        if should_trace_source && !matches!(clause.kind, ClauseKind::Nop) {
            self.trace_clause(clause);
        }

        match self.exec_clause(clause) {
            Ok(signal) => {
                // Post-execution trace for interactive pause
                if should_trace_source && !matches!(clause.kind, ClauseKind::Nop) {
                    self.trace_interactive_pause()?;
                }
                Ok(signal)
            }
            Err(diag) => {
                if let Some(label) = self.traps.get(&Condition::Syntax).cloned() {
                    // Set RC to the error number
                    self.env
                        .set("RC", RexxValue::new(diag.error.number().to_string()));
                    // Set condition info
                    let desc = diag.detail.unwrap_or_default();
                    self.env.set_condition_info(crate::env::ConditionInfoData {
                        condition: "SYNTAX".to_string(),
                        description: desc,
                        instruction: "SIGNAL".to_string(),
                        status: "ON".to_string(),
                    });
                    // Disable the trap (per REXX: trap fires once)
                    self.traps.remove(&Condition::Syntax);
                    Ok(ExecSignal::Signal(label))
                } else {
                    Err(diag)
                }
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    fn exec_clause(&mut self, clause: &Clause) -> RexxResult<ExecSignal> {
        match &clause.kind {
            ClauseKind::Say(expr) => {
                let val = self.eval_expr(expr)?;
                if self.pending_exit.is_pending() {
                    return Ok(ExecSignal::Normal);
                }
                if matches!(
                    self.trace_setting.level,
                    TraceLevel::Results | TraceLevel::Intermediates | TraceLevel::All
                ) {
                    self.trace_tag(">>", val.as_str());
                }
                println!("{val}");
                Ok(ExecSignal::Normal)
            }
            ClauseKind::Assignment { target, expr } => {
                let val = self.eval_expr(expr)?;
                if matches!(
                    self.trace_setting.level,
                    TraceLevel::Results | TraceLevel::Intermediates | TraceLevel::All
                ) {
                    self.trace_tag(">>", val.as_str());
                }
                match target {
                    AssignTarget::Simple(name) => {
                        self.env.set(name, val);
                    }
                    AssignTarget::Stem { stem, tail } => {
                        let resolved_tail = self.resolve_tail(tail);
                        if resolved_tail.is_empty() {
                            self.env.set_stem_default(stem, val);
                        } else {
                            self.env.set_compound(stem, &resolved_tail, val);
                        }
                    }
                }
                Ok(ExecSignal::Normal)
            }
            ClauseKind::Command(expr) => {
                let val = self.eval_expr(expr)?;
                if self.pending_exit.is_pending() || self.pending_signal.is_some() {
                    return Ok(ExecSignal::Normal);
                }
                Ok(self.exec_host_command(val.as_str()))
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
            ClauseKind::Call { name, args } => self.exec_call(name, args),
            ClauseKind::Signal(action) => self.exec_signal(action),
            ClauseKind::Label(_) | ClauseKind::Nop => Ok(ExecSignal::Normal),
            ClauseKind::Procedure(_) => Err(RexxDiagnostic::new(RexxError::UnexpectedProcedure)
                .with_detail("PROCEDURE must be the first instruction in a called routine")),
            ClauseKind::Drop(names) => {
                for name in names {
                    self.env.drop(name);
                }
                Ok(ExecSignal::Normal)
            }
            ClauseKind::Arg(template) => self.exec_arg(template),
            ClauseKind::Parse {
                upper,
                source,
                template,
            } => self.exec_parse(*upper, source, template),
            ClauseKind::Pull(template_opt) => {
                let raw = self.pull_from_queue_or_stdin()?;
                let text = raw.to_uppercase();
                if let Some(template) = template_opt {
                    self.apply_template(&text, template)?;
                }
                Ok(ExecSignal::Normal)
            }
            ClauseKind::Interpret(expr) => self.exec_interpret(expr),
            ClauseKind::Address(action) => self.exec_address(action),
            ClauseKind::Trace(expr) => self.exec_trace(expr),
            ClauseKind::Numeric(setting) => self.exec_numeric(setting),
            ClauseKind::Push(expr) => self.exec_push(expr.as_ref()),
            ClauseKind::Queue(expr) => self.exec_queue(expr.as_ref()),
        }
    }

    // ── ADDRESS / host command execution ───────────────────────────

    /// Execute an ADDRESS instruction.
    fn exec_address(&mut self, action: &AddressAction) -> RexxResult<ExecSignal> {
        match action {
            AddressAction::SetEnvironment(name) => {
                if name.is_empty() {
                    self.env.swap_address();
                } else {
                    self.env.set_address(name);
                }
                Ok(ExecSignal::Normal)
            }
            AddressAction::Value(expr) => {
                let val = self.eval_expr(expr)?;
                if self.pending_exit.is_pending() || self.pending_signal.is_some() {
                    return Ok(ExecSignal::Normal);
                }
                let name = val.as_str().to_uppercase();
                self.env.set_address(&name);
                Ok(ExecSignal::Normal)
            }
            AddressAction::Temporary {
                environment,
                command,
            } => {
                let cmd_val = self.eval_expr(command)?;
                if self.pending_exit.is_pending() || self.pending_signal.is_some() {
                    return Ok(ExecSignal::Normal);
                }
                let cmd_str = cmd_val.as_str().to_string();
                // Temporarily switch environment, run command, then restore.
                let saved_default = self.env.address().to_string();
                self.env.set_address(environment);
                let signal = self.exec_host_command(&cmd_str);
                // Restore: set_address saves current as previous, which is
                // what we want — the temporary env becomes previous per REXX.
                self.env.set_address(&saved_default);
                Ok(signal)
            }
        }
    }

    /// Execute a host command: try custom handler first, then `sh -c`.
    /// Sets RC and fires ERROR/FAILURE conditions as appropriate.
    fn exec_host_command(&mut self, command: &str) -> ExecSignal {
        // Try the custom command handler first
        let custom_rc = if let Some(ref mut handler) = self.command_handler {
            let addr = self.env.address().to_string();
            handler(&addr, command)
        } else {
            None
        };

        let rc = if let Some(rc) = custom_rc {
            // Custom handler handled it
            self.env.set("RC", RexxValue::new(rc.to_string()));
            rc
        } else {
            // Fall through to shell execution
            let result = std::process::Command::new("sh")
                .arg("-c")
                .arg(command)
                .status();
            match result {
                Ok(status) => {
                    let code = status.code().unwrap_or(-1);
                    self.env.set("RC", RexxValue::new(code.to_string()));
                    code
                }
                Err(_) => {
                    self.env.set("RC", RexxValue::new("-1"));
                    return self.fire_failure_trap(command);
                }
            }
        };

        if rc != 0 {
            if let Some(label) = self.traps.get(&Condition::Error).cloned() {
                self.env.set_condition_info(crate::env::ConditionInfoData {
                    condition: "ERROR".to_string(),
                    description: command.to_string(),
                    instruction: "SIGNAL".to_string(),
                    status: "ON".to_string(),
                });
                self.traps.remove(&Condition::Error);
                return ExecSignal::Signal(label);
            }
        }
        ExecSignal::Normal
    }

    fn fire_failure_trap(&mut self, command: &str) -> ExecSignal {
        if let Some(label) = self.traps.get(&Condition::Failure).cloned() {
            self.env.set_condition_info(crate::env::ConditionInfoData {
                condition: "FAILURE".to_string(),
                description: command.to_string(),
                instruction: "SIGNAL".to_string(),
                status: "ON".to_string(),
            });
            self.traps.remove(&Condition::Failure);
            ExecSignal::Signal(label)
        } else {
            ExecSignal::Normal
        }
    }

    // ── TRACE execution ────────────────────────────────────────────

    /// Apply a trace setting string, returning the previous setting as a string.
    /// Shared by TRACE instruction, `TRACE()` BIF, and CALL TRACE.
    fn apply_trace_setting(&mut self, s: &str) -> RexxResult<String> {
        let old = self.trace_setting.to_string();
        let action = TraceAction::parse(s).ok_or_else(|| {
            RexxDiagnostic::new(RexxError::InvalidTrace)
                .with_detail(format!("invalid trace setting '{s}'"))
        })?;
        match action {
            TraceAction::ToggleInteractive => {
                self.trace_setting.interactive = !self.trace_setting.interactive;
            }
            TraceAction::Set(new_setting) => {
                self.trace_setting = new_setting;
            }
        }
        Ok(old)
    }

    /// Execute a TRACE instruction: evaluate setting, update trace state.
    fn exec_trace(&mut self, expr: &Expr) -> RexxResult<ExecSignal> {
        let val = self.eval_expr(expr)?;
        if self.pending_exit.is_pending() || self.pending_signal.is_some() {
            return Ok(ExecSignal::Normal);
        }
        self.apply_trace_setting(val.as_str())?;
        Ok(ExecSignal::Normal)
    }

    /// Print a trace source line: "     3 *-* say 'hello'" to stderr.
    #[allow(clippy::unused_self)]
    fn trace_clause(&self, clause: &Clause) {
        let line_num = clause.loc.line;
        let source = clause.loc.source_line.as_deref().unwrap_or("(unknown)");
        eprintln!("{line_num:>6} *-* {source}");
    }

    /// Print a trace tag line: "       >>> \"value\"" to stderr.
    #[allow(clippy::unused_self)]
    fn trace_tag(&self, tag: &str, value: &str) {
        eprintln!("       >{tag}> \"{value}\"");
    }

    /// Conditionally trace an intermediate value (only at Intermediates or All level).
    fn trace_intermediates(&self, tag: &str, value: &str) {
        if matches!(
            self.trace_setting.level,
            TraceLevel::Intermediates | TraceLevel::All
        ) {
            self.trace_tag(tag, value);
        }
    }

    /// Handle interactive pause: read stdin lines and execute via INTERPRET.
    /// Per ANSI REXX, only a null line (no characters at all, not even spaces)
    /// continues execution. Any other input is executed as REXX code.
    fn trace_interactive_pause(&mut self) -> RexxResult<()> {
        if !self.trace_setting.interactive {
            return Ok(());
        }
        loop {
            let mut line = String::new();
            match std::io::stdin().read_line(&mut line) {
                Ok(0) | Err(_) => break, // EOF or I/O error
                Ok(_) => {}
            }
            // Strip trailing newline/carriage return only (preserve interior whitespace)
            let content = line.trim_end_matches(['\n', '\r']);
            // Null line (empty after stripping newline) → continue execution
            if content.is_empty() {
                break;
            }
            // Execute the input as REXX via the existing interpret machinery
            let source = content.trim().to_string();
            let mut lexer = Lexer::new(&source);
            let tokens = lexer.tokenize()?;
            let mut parser = Parser::new(tokens);
            let program = parser.parse()?;
            let labels = HashMap::new();
            self.exec_interpret_body(&program.clauses, &labels)?;
        }
        Ok(())
    }

    // ── INTERPRET execution ────────────────────────────────────────

    /// Execute an INTERPRET instruction: evaluate expr to string, lex, parse, execute.
    fn exec_interpret(&mut self, expr: &Expr) -> RexxResult<ExecSignal> {
        let val = self.eval_expr(expr)?;
        if self.pending_exit.is_pending() {
            return Ok(ExecSignal::Normal);
        }
        if self.pending_signal.is_some() {
            return Ok(ExecSignal::Normal);
        }

        let source = val.as_str().to_string();
        if source.is_empty() {
            return Ok(ExecSignal::Normal);
        }

        // Depth guard
        if self.interpret_depth >= MAX_INTERPRET_DEPTH {
            return Err(RexxDiagnostic::new(RexxError::ResourceExhausted)
                .with_detail("INTERPRET recursion depth limit exceeded"));
        }

        // Lex and parse the interpreted string
        let mut lexer = Lexer::new(&source);
        let tokens = lexer.tokenize()?;
        let mut parser = Parser::new(tokens);
        let program = parser.parse()?;

        // Build label map for the interpreted code
        let mut labels = HashMap::new();
        for (i, clause) in program.clauses.iter().enumerate() {
            if let ClauseKind::Label(name) = &clause.kind {
                labels.entry(name.clone()).or_insert(i);
            }
        }

        self.interpret_depth += 1;
        let result = self.exec_interpret_body(&program.clauses, &labels);
        self.interpret_depth -= 1;

        result
    }

    /// Execute interpreted clauses with a restart loop for local SIGNAL targets.
    fn exec_interpret_body(
        &mut self,
        clauses: &[Clause],
        labels: &HashMap<String, usize>,
    ) -> RexxResult<ExecSignal> {
        let mut start = 0;
        loop {
            match self.exec_interpret_from(clauses, start)? {
                ExecSignal::Signal(ref label) => {
                    if let Some(&idx) = labels.get(label) {
                        start = idx + 1; // restart locally
                    } else {
                        return Ok(ExecSignal::Signal(label.clone()));
                    }
                }
                other => return Ok(other),
            }
        }
    }

    /// Execute interpreted clauses from a given index (mirrors `exec_from` for interpreted code).
    fn exec_interpret_from(&mut self, clauses: &[Clause], start: usize) -> RexxResult<ExecSignal> {
        for clause in &clauses[start..] {
            let signal = self.exec_clause_outer(clause)?;
            if let Some(signal) = self.pending_exit.take_signal() {
                return Ok(signal);
            }
            if let Some(label) = self.pending_signal.take() {
                return Ok(ExecSignal::Signal(label));
            }
            if !matches!(signal, ExecSignal::Normal) {
                return Ok(signal);
            }
        }
        Ok(ExecSignal::Normal)
    }

    fn call_routine(&mut self, name: &str, args: Vec<RexxValue>) -> RexxResult<ExecSignal> {
        let &label_idx = self.labels.get(name).ok_or_else(|| {
            RexxDiagnostic::new(RexxError::RoutineNotFound)
                .with_detail(format!("routine '{name}' not found"))
        })?;

        let start_idx = label_idx + 1; // skip the label clause itself
        self.arg_stack.push(args);

        // Check if first clause after label is PROCEDURE — consume it before executing body
        let has_procedure = matches!(
            self.program.clauses.get(start_idx).map(|c| &c.kind),
            Some(ClauseKind::Procedure(_))
        );

        let exec_start = if has_procedure {
            match &self.program.clauses[start_idx].kind {
                ClauseKind::Procedure(Some(names)) => self.env.push_procedure_expose(names),
                ClauseKind::Procedure(None) => self.env.push_procedure(),
                _ => unreachable!(),
            }
            start_idx + 1
        } else {
            start_idx
        };

        let result = self.exec_from(exec_start);

        if has_procedure {
            self.env.pop_procedure();
        }
        self.arg_stack.pop();

        result
    }

    /// Try to call an external function by searching the filesystem for a `.rexx`/`.rex` file.
    /// Returns `Ok(None)` if no external file was found, `Ok(Some(signal))` if executed.
    fn try_call_external(
        &mut self,
        name: &str,
        args: Vec<RexxValue>,
    ) -> RexxResult<Option<ExecSignal>> {
        // 1. Resolve external file
        let source_dir = self.env.source_dir();
        let Some((program, path)) = crate::external::resolve_external(name, source_dir)? else {
            return Ok(None);
        };

        // 2. Recursion guard
        if self.external_depth >= 100 {
            return Err(RexxDiagnostic::new(RexxError::ResourceExhausted)
                .with_detail("external function call recursion depth limit exceeded"));
        }
        self.external_depth += 1;

        // 3. Push isolated scope, set args
        self.env.push_procedure();
        self.arg_stack.push(args);

        // 4. Update source_path so nested external calls resolve relative to this file
        let old_source_path = self.env.source_path().map(Path::to_path_buf);
        self.env.set_source_path(path);

        // 5. Build labels for external program, execute via exec_interpret_body
        let ext_labels = Self::build_labels(&program);
        let result = self.exec_interpret_body(&program.clauses, &ext_labels);

        // 6. Restore source_path, clean up scope
        match old_source_path {
            Some(old) => self.env.set_source_path(old),
            None => self.env.clear_source_path(),
        }
        self.arg_stack.pop();
        self.env.pop_procedure();
        self.external_depth -= 1;

        Ok(Some(result?))
    }

    fn exec_call(&mut self, name: &str, arg_exprs: &[Expr]) -> RexxResult<ExecSignal> {
        let mut args = Vec::with_capacity(arg_exprs.len());
        for expr in arg_exprs {
            args.push(self.eval_expr(expr)?);
            if let Some(signal) = self.pending_exit.take_signal() {
                return Ok(signal);
            }
        }

        // CALL TRACE — handle before normal dispatch
        if name.eq_ignore_ascii_case("TRACE") {
            if args.len() == 1 {
                let old = self.apply_trace_setting(args[0].as_str())?;
                self.env.set("RESULT", RexxValue::new(old));
            } else if args.is_empty() {
                let old = self.trace_setting.to_string();
                self.env.set("RESULT", RexxValue::new(old));
            } else {
                return Err(RexxDiagnostic::new(RexxError::IncorrectCall)
                    .with_detail("TRACE expects 0 or 1 arguments"));
            }
            return Ok(ExecSignal::Normal);
        }

        // Resolution order: 1) internal labels, 2) built-in functions, 3) external, 4) error
        if self.labels.contains_key(name) {
            let signal = self.call_routine(name, args)?;
            match signal {
                ExecSignal::Return(Some(val)) => {
                    self.env.set("RESULT", val);
                    Ok(ExecSignal::Normal)
                }
                ExecSignal::Return(None) | ExecSignal::Normal => {
                    self.env.drop("RESULT");
                    Ok(ExecSignal::Normal)
                }
                ExecSignal::Exit(_) | ExecSignal::Signal(_) => Ok(signal),
                ExecSignal::Leave(_) | ExecSignal::Iterate(_) => Ok(ExecSignal::Normal),
            }
        } else if let Some(result) =
            crate::builtins::call_builtin(name, &args, &self.settings, self.env, self.queue.len())
        {
            let val = result?;
            self.env.set("RESULT", val);
            Ok(ExecSignal::Normal)
        } else {
            // Step 3: external function search
            match self.try_call_external(name, args)? {
                Some(signal) => match signal {
                    ExecSignal::Return(Some(val)) => {
                        self.env.set("RESULT", val);
                        Ok(ExecSignal::Normal)
                    }
                    ExecSignal::Return(None) | ExecSignal::Normal => {
                        self.env.drop("RESULT");
                        Ok(ExecSignal::Normal)
                    }
                    ExecSignal::Exit(_) | ExecSignal::Signal(_) => Ok(signal),
                    ExecSignal::Leave(_) | ExecSignal::Iterate(_) => Ok(ExecSignal::Normal),
                },
                None => Err(RexxDiagnostic::new(RexxError::RoutineNotFound)
                    .with_detail(format!("routine '{name}' not found"))),
            }
        }
    }

    /// Execute a SIGNAL instruction.
    fn exec_signal(&mut self, action: &SignalAction) -> RexxResult<ExecSignal> {
        match action {
            SignalAction::Label(label) => Ok(ExecSignal::Signal(label.clone())),
            SignalAction::Value(expr) => {
                let val = self.eval_expr(expr)?;
                let label = val.as_str().to_uppercase();
                Ok(ExecSignal::Signal(label))
            }
            SignalAction::On { condition, name } => {
                let label = name
                    .clone()
                    .unwrap_or_else(|| Self::condition_default_label(condition));
                self.traps.insert(condition.clone(), label);
                Ok(ExecSignal::Normal)
            }
            SignalAction::Off(condition) => {
                self.traps.remove(condition);
                Ok(ExecSignal::Normal)
            }
        }
    }

    /// Default label name for a condition (the condition name itself, uppercased).
    fn condition_default_label(condition: &Condition) -> String {
        match condition {
            Condition::Error => "ERROR".to_string(),
            Condition::Failure => "FAILURE".to_string(),
            Condition::Halt => "HALT".to_string(),
            Condition::NoValue => "NOVALUE".to_string(),
            Condition::NotReady => "NOTREADY".to_string(),
            Condition::Syntax => "SYNTAX".to_string(),
            Condition::LostDigits => "LOSTDIGITS".to_string(),
        }
    }

    /// ARG is shorthand for PARSE UPPER ARG.
    fn exec_arg(&mut self, template: &ParseTemplate) -> RexxResult<ExecSignal> {
        self.exec_parse(true, &ParseSource::Arg, template)
    }

    /// Public setter so main.rs can push CLI arguments for the main program.
    pub fn set_main_args(&mut self, args: Vec<RexxValue>) {
        self.arg_stack.push(args);
    }

    /// Set a custom command handler for ADDRESS environments.
    ///
    /// The handler receives (address_environment, command_string) and returns:
    /// - `Some(rc)` if it handled the command (rc is the return code)
    /// - `None` if the command should fall through to default shell execution
    ///
    /// This allows embedding applications (like XEDIT) to intercept commands
    /// sent to custom ADDRESS environments.
    pub fn set_command_handler(&mut self, handler: Box<dyn FnMut(&str, &str) -> Option<i32>>) {
        self.command_handler = Some(handler);
    }

    // ── PARSE template engine ──────────────────────────────────────

    /// Execute a PARSE instruction: resolve source, split at commas, apply templates.
    fn exec_parse(
        &mut self,
        upper: bool,
        source: &ParseSource,
        template: &ParseTemplate,
    ) -> RexxResult<ExecSignal> {
        let sub_templates = Self::split_template_at_commas(template);

        if let ParseSource::Arg = source {
            let args = self.arg_stack.last().cloned().unwrap_or_default();
            for (i, sub_t) in sub_templates.iter().enumerate() {
                let raw = args
                    .get(i)
                    .map(|v| v.as_str().to_string())
                    .unwrap_or_default();
                let text = if upper { raw.to_uppercase() } else { raw };
                self.apply_template(&text, sub_t)?;
            }
        } else {
            let raw = match source {
                ParseSource::Var(name) => self.env.get(name).as_str().to_string(),
                ParseSource::Value(expr) => self.eval_expr(expr)?.as_str().to_string(),
                ParseSource::Pull => self.pull_from_queue_or_stdin()?,
                ParseSource::LineIn => self.read_stdin_line()?,
                ParseSource::Source => {
                    let filename = self
                        .env
                        .source_path()
                        .and_then(|p| p.file_name())
                        .map_or_else(|| "rexx".to_string(), |f| f.to_string_lossy().into_owned());
                    format!("UNIX COMMAND {filename}")
                }
                ParseSource::Version => {
                    format!("REXX-patch-rexx {} 8 Feb 2026", env!("CARGO_PKG_VERSION"))
                }
                ParseSource::Arg => unreachable!(),
            };
            let text = if upper { raw.to_uppercase() } else { raw };
            for (i, sub_t) in sub_templates.iter().enumerate() {
                let s = if i == 0 { &text } else { "" };
                self.apply_template(s, sub_t)?;
            }
        }

        Ok(ExecSignal::Normal)
    }

    /// Apply a single PARSE template to a source string.
    fn apply_template(&mut self, source: &str, template: &ParseTemplate) -> RexxResult<()> {
        let elements = &template.elements;
        let len = elements.len();
        let mut cursor: usize = 0;
        let mut i: usize = 0;

        while i < len {
            // Collect consecutive Variable/Dot targets.
            let mut targets: Vec<&TemplateElement> = Vec::new();
            while i < len {
                match &elements[i] {
                    e @ (TemplateElement::Variable(_) | TemplateElement::Dot) => {
                        targets.push(e);
                        i += 1;
                    }
                    _ => break,
                }
            }

            // Determine the section based on next element.
            if i >= len {
                // End of template: section = cursor..end
                let section = if cursor < source.len() {
                    &source[cursor..]
                } else {
                    ""
                };
                self.assign_section(section, &targets);
                break;
            }

            match &elements[i] {
                TemplateElement::Literal(pat) => {
                    cursor = self.match_pattern(source, cursor, pat, &targets);
                    i += 1;
                }
                TemplateElement::AbsolutePos(expr) => {
                    let pos_val = self.eval_expr(expr)?;
                    let pos = self.to_position_value(&pos_val)?;
                    #[allow(clippy::cast_sign_loss, clippy::cast_possible_truncation)]
                    let char_pos = if pos > 0 { (pos - 1) as usize } else { 0 };
                    let target = Self::char_pos_to_byte_offset(source, char_pos);
                    let section = if target > cursor {
                        &source[cursor..target]
                    } else {
                        ""
                    };
                    self.assign_section(section, &targets);
                    cursor = target;
                    i += 1;
                }
                TemplateElement::RelativePos(offset) => {
                    #[allow(clippy::cast_sign_loss)]
                    let target = if *offset >= 0 {
                        Self::advance_chars(source, cursor, *offset as usize)
                    } else {
                        Self::retreat_chars(source, cursor, offset.unsigned_abs() as usize)
                    };
                    let section = if target > cursor {
                        &source[cursor..target]
                    } else {
                        ""
                    };
                    self.assign_section(section, &targets);
                    cursor = target;
                    i += 1;
                }
                TemplateElement::VariablePattern(name) => {
                    let pat = self.env.get(name).as_str().to_string();
                    cursor = self.match_pattern(source, cursor, &pat, &targets);
                    i += 1;
                }
                _ => {
                    i += 1;
                }
            }
        }
        Ok(())
    }

    /// Assign a section of text to target variables using REXX word-parsing rules.
    /// Per ANSI REXX, "blanks" include space (0x20) and horizontal tab (0x09).
    fn assign_section(&mut self, section: &str, targets: &[&TemplateElement]) {
        match targets.len() {
            0 => {} // no targets — just repositioning cursor
            1 => {
                // Single target gets entire section verbatim
                self.assign_target(targets[0], section);
            }
            _ => {
                // Multiple targets: word-parse
                let mut remaining = section;
                for (j, target) in targets.iter().enumerate() {
                    if j == targets.len() - 1 {
                        // Last target: strip leading blanks, take rest
                        let trimmed = remaining.trim_start_matches([' ', '\t']);
                        self.assign_target(target, trimmed);
                    } else {
                        // Non-last: strip leading blanks, take one word
                        let trimmed = remaining.trim_start_matches([' ', '\t']);
                        if let Some(blank_pos) = trimmed.find([' ', '\t']) {
                            let word = &trimmed[..blank_pos];
                            self.assign_target(target, word);
                            remaining = &trimmed[blank_pos..];
                        } else {
                            // No more words
                            self.assign_target(target, trimmed);
                            remaining = "";
                        }
                    }
                }
            }
        }
    }

    /// Search for a literal pattern in source from cursor. Assigns the section
    /// before the match (or the rest if not found) to targets. Returns new cursor.
    /// Empty patterns are treated as not found per REXX semantics.
    fn match_pattern(
        &mut self,
        source: &str,
        cursor: usize,
        pat: &str,
        targets: &[&TemplateElement],
    ) -> usize {
        if !pat.is_empty()
            && let Some(found) = source[cursor..].find(pat)
        {
            let abs = cursor + found;
            self.assign_section(&source[cursor..abs], targets);
            abs + pat.len()
        } else {
            let section = if cursor < source.len() {
                &source[cursor..]
            } else {
                ""
            };
            self.assign_section(section, targets);
            source.len()
        }
    }

    /// Assign a value to a single template target (Variable or Dot).
    fn assign_target(&mut self, target: &TemplateElement, value: &str) {
        if let TemplateElement::Variable(name) = target {
            self.env.set(name, RexxValue::new(value));
        }
        // Dot and other elements are discarded
    }

    /// Split a template at Comma elements into sub-templates.
    /// Fast path: if no commas, return the template as-is without cloning.
    fn split_template_at_commas(template: &ParseTemplate) -> Vec<ParseTemplate> {
        if !template
            .elements
            .iter()
            .any(|e| matches!(e, TemplateElement::Comma))
        {
            return vec![template.clone()];
        }
        let mut result = Vec::new();
        let mut current = Vec::new();
        for elem in &template.elements {
            if matches!(elem, TemplateElement::Comma) {
                result.push(ParseTemplate {
                    elements: std::mem::take(&mut current),
                });
            } else {
                current.push(elem.clone());
            }
        }
        result.push(ParseTemplate { elements: current });
        result
    }

    // ── NUMERIC execution ─────────────────────────────────────────

    /// Execute a NUMERIC instruction: update self.settings.
    fn exec_numeric(&mut self, setting: &NumericSetting) -> RexxResult<ExecSignal> {
        match setting {
            NumericSetting::Digits(expr) => {
                let digits = if let Some(e) = expr {
                    let val = self.eval_expr(e)?;
                    if self.pending_exit.is_pending() || self.pending_signal.is_some() {
                        return Ok(ExecSignal::Normal);
                    }
                    let n = self.to_integer(&val)?;
                    if n < 1 {
                        return Err(RexxDiagnostic::new(RexxError::InvalidWholeNumber)
                            .with_detail("NUMERIC DIGITS value must be positive"));
                    }
                    if n > i64::from(u32::MAX) {
                        return Err(RexxDiagnostic::new(RexxError::InvalidWholeNumber)
                            .with_detail("NUMERIC DIGITS value too large"));
                    }
                    #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
                    {
                        n as u32
                    }
                } else {
                    9 // default
                };
                self.settings.digits = digits;
            }
            NumericSetting::Form(form_setting) => {
                let form = match form_setting {
                    NumericFormSetting::Scientific => crate::value::NumericForm::Scientific,
                    NumericFormSetting::Engineering => crate::value::NumericForm::Engineering,
                    NumericFormSetting::Value(expr) => {
                        let val = self.eval_expr(expr)?;
                        if self.pending_exit.is_pending() || self.pending_signal.is_some() {
                            return Ok(ExecSignal::Normal);
                        }
                        let s = val.as_str().to_uppercase();
                        match s.as_str() {
                            "SCIENTIFIC" => crate::value::NumericForm::Scientific,
                            "ENGINEERING" => crate::value::NumericForm::Engineering,
                            _ => {
                                return Err(RexxDiagnostic::new(RexxError::InvalidSubKeyword)
                                    .with_detail(format!(
                                        "NUMERIC FORM value must be SCIENTIFIC or ENGINEERING; got '{s}'"
                                    )));
                            }
                        }
                    }
                };
                self.settings.form = form;
            }
            NumericSetting::Fuzz(expr) => {
                let fuzz = if let Some(e) = expr {
                    let val = self.eval_expr(e)?;
                    if self.pending_exit.is_pending() || self.pending_signal.is_some() {
                        return Ok(ExecSignal::Normal);
                    }
                    let n = self.to_integer(&val)?;
                    if n < 0 {
                        return Err(RexxDiagnostic::new(RexxError::InvalidWholeNumber)
                            .with_detail("NUMERIC FUZZ value must not be negative"));
                    }
                    if n > i64::from(u32::MAX) {
                        return Err(RexxDiagnostic::new(RexxError::InvalidWholeNumber)
                            .with_detail("NUMERIC FUZZ value too large"));
                    }
                    #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
                    {
                        n as u32
                    }
                } else {
                    0 // default
                };
                if fuzz >= self.settings.digits {
                    return Err(RexxDiagnostic::new(RexxError::InvalidWholeNumber)
                        .with_detail("NUMERIC FUZZ must be less than NUMERIC DIGITS"));
                }
                self.settings.fuzz = fuzz;
            }
        }
        Ok(ExecSignal::Normal)
    }

    // ── PUSH / QUEUE / PULL execution ────────────────────────────────

    /// Execute PUSH: evaluate expr, add to front of queue (LIFO).
    fn exec_push(&mut self, expr: Option<&Expr>) -> RexxResult<ExecSignal> {
        let val = if let Some(e) = expr {
            let v = self.eval_expr(e)?;
            if self.pending_exit.is_pending() || self.pending_signal.is_some() {
                return Ok(ExecSignal::Normal);
            }
            v.as_str().to_string()
        } else {
            String::new()
        };
        self.queue.push_front(val);
        Ok(ExecSignal::Normal)
    }

    /// Execute QUEUE: evaluate expr, add to back of queue (FIFO).
    fn exec_queue(&mut self, expr: Option<&Expr>) -> RexxResult<ExecSignal> {
        let val = if let Some(e) = expr {
            let v = self.eval_expr(e)?;
            if self.pending_exit.is_pending() || self.pending_signal.is_some() {
                return Ok(ExecSignal::Normal);
            }
            v.as_str().to_string()
        } else {
            String::new()
        };
        self.queue.push_back(val);
        Ok(ExecSignal::Normal)
    }

    /// Pull from the external data queue; if empty, read from stdin.
    fn pull_from_queue_or_stdin(&mut self) -> RexxResult<String> {
        if let Some(line) = self.queue.pop_front() {
            Ok(line)
        } else {
            self.read_stdin_line()
        }
    }

    /// Return the current queue length (for `QUEUED()` BIF).
    pub fn queue_len(&self) -> usize {
        self.queue.len()
    }

    /// Read one line from stdin, stripping the trailing newline.
    #[allow(clippy::unused_self)]
    fn read_stdin_line(&self) -> RexxResult<String> {
        let mut line = String::new();
        std::io::stdin().read_line(&mut line).map_err(|e| {
            RexxDiagnostic::new(RexxError::SystemFailure)
                .with_detail(format!("failed to read stdin: {e}"))
        })?;
        if line.ends_with('\n') {
            line.pop();
            if line.ends_with('\r') {
                line.pop();
            }
        }
        Ok(line)
    }

    /// Convert a 0-based character position to a byte offset in a UTF-8 string.
    /// Clamps to `source.len()` if the character position exceeds the string length.
    fn char_pos_to_byte_offset(source: &str, char_pos: usize) -> usize {
        source
            .char_indices()
            .nth(char_pos)
            .map_or(source.len(), |(byte_offset, _)| byte_offset)
    }

    /// Advance `n` characters forward from `byte_cursor` and return the new byte offset.
    fn advance_chars(source: &str, byte_cursor: usize, n: usize) -> usize {
        let clamped = byte_cursor.min(source.len());
        source[clamped..]
            .char_indices()
            .nth(n)
            .map_or(source.len(), |(offset, _)| clamped + offset)
    }

    /// Retreat `n` characters backward from `byte_cursor` and return the new byte offset.
    fn retreat_chars(source: &str, byte_cursor: usize, n: usize) -> usize {
        if n == 0 {
            return byte_cursor.min(source.len());
        }
        let clamped = byte_cursor.min(source.len());
        source[..clamped]
            .char_indices()
            .map(|(i, _)| i)
            .rev()
            .nth(n - 1)
            .unwrap_or(0)
    }

    /// Convert a value to a position (integer) for PARSE template positioning.
    fn to_position_value(&self, val: &RexxValue) -> RexxResult<i64> {
        let d = self.to_number(val)?;
        let rounded = d.round(0);
        if d != rounded {
            return Err(RexxDiagnostic::new(RexxError::InvalidWholeNumber)
                .with_detail(format!("'{val}' is not a whole number")));
        }
        let s = rounded.to_string();
        s.parse::<i64>().map_err(|_| {
            RexxDiagnostic::new(RexxError::InvalidWholeNumber)
                .with_detail(format!("'{val}' is not a valid position"))
        })
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
                    if Self::signal_matches(name.as_ref(), block.name.as_ref()) {
                        return Ok(ExecSignal::Normal);
                    }
                    return Ok(signal);
                }
                ExecSignal::Iterate(ref name) => {
                    if Self::signal_matches(name.as_ref(), block.name.as_ref()) {
                        continue;
                    }
                    return Ok(signal);
                }
                ExecSignal::Exit(_) | ExecSignal::Return(_) | ExecSignal::Signal(_) => {
                    return Ok(signal);
                }
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
                    if Self::signal_matches(name.as_ref(), block.name.as_ref()) {
                        return Ok(ExecSignal::Normal);
                    }
                    return Ok(signal);
                }
                ExecSignal::Iterate(ref name) => {
                    if Self::signal_matches(name.as_ref(), block.name.as_ref()) {
                        continue;
                    }
                    return Ok(signal);
                }
                ExecSignal::Exit(_) | ExecSignal::Return(_) | ExecSignal::Signal(_) => {
                    return Ok(signal);
                }
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
                    if Self::signal_matches(name.as_ref(), block.name.as_ref()) {
                        return Ok(ExecSignal::Normal);
                    }
                    return Ok(signal);
                }
                ExecSignal::Iterate(ref name) => {
                    if Self::signal_matches(name.as_ref(), block.name.as_ref()) {
                        continue;
                    }
                    return Ok(signal);
                }
                ExecSignal::Exit(_) | ExecSignal::Return(_) | ExecSignal::Signal(_) => {
                    return Ok(signal);
                }
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
                    if Self::signal_matches(name.as_ref(), block.name.as_ref()) {
                        return Ok(ExecSignal::Normal);
                    }
                    return Ok(signal);
                }
                ExecSignal::Iterate(ref name) => {
                    if !Self::signal_matches(name.as_ref(), block.name.as_ref()) {
                        return Ok(signal);
                    }
                    // ITERATE matched: continue to UNTIL check
                }
                ExecSignal::Exit(_) | ExecSignal::Return(_) | ExecSignal::Signal(_) => {
                    return Ok(signal);
                }
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
            // Check TO limit before executing body.
            // BY is guaranteed non-zero (checked earlier).
            if let Some(ref limit) = to_num {
                let positive_step = by_num.sign() == bigdecimal::num_bigint::Sign::Plus;
                if positive_step {
                    if current > *limit {
                        break;
                    }
                } else if current < *limit {
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
                    if Self::signal_matches(name.as_ref(), block.name.as_ref()) {
                        return Ok(ExecSignal::Normal);
                    }
                    return Ok(signal);
                }
                ExecSignal::Iterate(ref name) => {
                    if !Self::signal_matches(name.as_ref(), block.name.as_ref()) {
                        return Ok(signal);
                    }
                    // ITERATE matched: fall through to increment
                }
                ExecSignal::Exit(_) | ExecSignal::Return(_) | ExecSignal::Signal(_) => {
                    return Ok(signal);
                }
            }

            // Check UNTIL condition (after body, before increment).
            // Per ANSI REXX §7.3.5, when UNTIL is satisfied the loop
            // terminates immediately — the control variable retains its
            // current value (the increment below is skipped via break).
            if let Some(ref until_expr) = ctrl.until_cond {
                let v = self.eval_expr(until_expr)?;
                if to_logical(&v)? {
                    break;
                }
            }

            // Increment (only reached if UNTIL was false or absent)
            current += &by_num;
            iterations = iterations.saturating_add(1);
        }

        // Set final value of control variable (may be one-past-limit
        // for TO termination, or last body value for UNTIL termination).
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

    /// Check if a LEAVE or ITERATE signal name matches this loop's name.
    /// Unnamed signals (None) match any loop; named signals match only
    /// if the loop has the same name.
    fn signal_matches(signal_name: Option<&String>, loop_name: Option<&String>) -> bool {
        match signal_name {
            None => true,
            Some(name) => loop_name.is_some_and(|ln| ln == name),
        }
    }

    /// Convert a value to a non-negative whole number for loop counts.
    /// Per ANSI REXX, loop counts and FOR values must be whole numbers.
    fn to_integer(&self, val: &RexxValue) -> RexxResult<i64> {
        let d = self.to_number(val)?;
        let rounded = d.round(0);
        if d != rounded {
            return Err(RexxDiagnostic::new(RexxError::InvalidWholeNumber)
                .with_detail("loop count must be a whole number"));
        }
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

    #[allow(clippy::too_many_lines)]
    fn eval_expr(&mut self, expr: &Expr) -> RexxResult<RexxValue> {
        match expr {
            Expr::StringLit(s) => {
                let val = RexxValue::new(s.clone());
                self.trace_intermediates("L", val.as_str());
                Ok(val)
            }
            Expr::Number(n) => {
                let val = RexxValue::new(n.clone());
                self.trace_intermediates("L", val.as_str());
                Ok(val)
            }
            Expr::Symbol(name) => {
                if !self.env.is_set(name)
                    && let Some(label) = self.traps.get(&Condition::NoValue).cloned()
                {
                    // Set condition info before firing
                    self.env.set_condition_info(crate::env::ConditionInfoData {
                        condition: "NOVALUE".to_string(),
                        description: name.clone(),
                        instruction: "SIGNAL".to_string(),
                        status: "ON".to_string(),
                    });
                    // Disable the trap (fires once per REXX spec)
                    self.traps.remove(&Condition::NoValue);
                    self.pending_signal = Some(label);
                }
                let val = self.env.get(name);
                self.trace_intermediates("V", val.as_str());
                Ok(val)
            }
            Expr::Compound { stem, tail } => {
                let resolved = self.resolve_tail(tail);
                if !self.env.is_compound_set(stem, &resolved)
                    && let Some(label) = self.traps.get(&Condition::NoValue).cloned()
                {
                    let compound_name = format!("{}.{}", stem.to_uppercase(), resolved);
                    self.env.set_condition_info(crate::env::ConditionInfoData {
                        condition: "NOVALUE".to_string(),
                        description: compound_name,
                        instruction: "SIGNAL".to_string(),
                        status: "ON".to_string(),
                    });
                    self.traps.remove(&Condition::NoValue);
                    self.pending_signal = Some(label);
                }
                let val = self.env.get_compound(stem, &resolved);
                self.trace_intermediates("C", val.as_str());
                Ok(val)
            }
            Expr::Paren(inner) => self.eval_expr(inner),
            Expr::UnaryOp { op, operand } => {
                let val = self.eval_expr(operand)?;
                if self.pending_signal.is_some() {
                    return Ok(val);
                }
                let result = self.eval_unary(*op, &val)?;
                self.trace_intermediates("P", result.as_str());
                Ok(result)
            }
            Expr::BinOp { left, op, right } => {
                let lval = self.eval_expr(left)?;
                if self.pending_signal.is_some() {
                    return Ok(lval);
                }
                let rval = self.eval_expr(right)?;
                if self.pending_signal.is_some() {
                    return Ok(rval);
                }
                let result = self.eval_binop(*op, &lval, &rval)?;
                self.trace_intermediates("O", result.as_str());
                Ok(result)
            }
            Expr::FunctionCall { name, args } => {
                let mut evaluated_args = Vec::with_capacity(args.len());
                for arg_expr in args {
                    evaluated_args.push(self.eval_expr(arg_expr)?);
                    if self.pending_exit.is_pending() || self.pending_signal.is_some() {
                        return Ok(RexxValue::new(""));
                    }
                }
                // TRACE() BIF — needs evaluator state, handle before normal dispatch
                if name.eq_ignore_ascii_case("TRACE") {
                    if evaluated_args.len() == 1 {
                        let old = self.apply_trace_setting(evaluated_args[0].as_str())?;
                        return Ok(RexxValue::new(old));
                    } else if !evaluated_args.is_empty() {
                        return Err(RexxDiagnostic::new(RexxError::IncorrectCall)
                            .with_detail("TRACE expects 0 or 1 arguments"));
                    }
                    return Ok(RexxValue::new(self.trace_setting.to_string()));
                }

                // Resolution order: 1) internal labels, 2) built-in functions, 3) external, 4) error
                if self.labels.contains_key(name.as_str()) {
                    let signal = self.call_routine(name, evaluated_args)?;
                    match signal {
                        ExecSignal::Return(Some(val)) => {
                            self.trace_intermediates("F", val.as_str());
                            Ok(val)
                        }
                        ExecSignal::Return(None) | ExecSignal::Normal => {
                            Err(RexxDiagnostic::new(RexxError::NoReturnData)
                                .with_detail(format!("function '{name}' did not return data")))
                        }
                        ExecSignal::Exit(val) => {
                            self.pending_exit = PendingExit::WithValue(val);
                            Ok(RexxValue::new(""))
                        }
                        ExecSignal::Signal(_) => {
                            // Propagate signal as pending — we can't return ExecSignal from eval_expr
                            if let ExecSignal::Signal(label) = signal {
                                self.pending_signal = Some(label);
                            }
                            Ok(RexxValue::new(""))
                        }
                        ExecSignal::Leave(_) | ExecSignal::Iterate(_) => Ok(RexxValue::new("")),
                    }
                } else if let Some(result) = crate::builtins::call_builtin(
                    name,
                    &evaluated_args,
                    &self.settings,
                    self.env,
                    self.queue.len(),
                ) {
                    let val = result?;
                    self.trace_intermediates("F", val.as_str());
                    Ok(val)
                } else {
                    // Step 3: external function search
                    match self.try_call_external(name, evaluated_args)? {
                        Some(signal) => match signal {
                            ExecSignal::Return(Some(val)) => {
                                self.trace_intermediates("F", val.as_str());
                                Ok(val)
                            }
                            ExecSignal::Return(None) | ExecSignal::Normal => {
                                Err(RexxDiagnostic::new(RexxError::NoReturnData)
                                    .with_detail(format!("function '{name}' did not return data")))
                            }
                            ExecSignal::Exit(val) => {
                                self.pending_exit = PendingExit::WithValue(val);
                                Ok(RexxValue::new(""))
                            }
                            ExecSignal::Signal(_) => {
                                if let ExecSignal::Signal(label) = signal {
                                    self.pending_signal = Some(label);
                                }
                                Ok(RexxValue::new(""))
                            }
                            ExecSignal::Leave(_) | ExecSignal::Iterate(_) => Ok(RexxValue::new("")),
                        },
                        None => Err(RexxDiagnostic::new(RexxError::RoutineNotFound)
                            .with_detail(format!("routine '{name}' not found"))),
                    }
                }
            }
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
                // Limit exponent to prevent OOM from massive intermediate values.
                // 1_000_000 is generous for practical REXX use cases.
                if exp_i64.abs() > 1_000_000 {
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
    // RoundingMode::Down truncates toward zero (not toward negative infinity).
    // Using with_scale_round avoids string-based truncation that breaks on
    // scientific notation from BigDecimal::to_string().
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
        let mut eval = Evaluator::new(&mut env, &program);
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
        let mut eval = Evaluator::new(&mut env, &program);
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
        let mut eval = Evaluator::new(&mut env, &program);
        let signal = eval.exec().unwrap();
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
        let mut eval = Evaluator::new(&mut env, &program);
        let signal = eval.exec().unwrap();
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
