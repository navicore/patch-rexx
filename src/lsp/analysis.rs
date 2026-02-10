use crate::ast::{AssignTarget, Clause, ClauseKind, DoBlock, DoKind, Expr, Program};
use crate::error::{RexxDiagnostic, SourceLoc};
use crate::lexer::Lexer;
use crate::parser::Parser;

#[derive(Debug, Clone)]
pub struct LabelInfo {
    pub name: String,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct SubroutineInfo {
    pub name: String,
    pub loc: SourceLoc,
    pub has_procedure: bool,
    pub exposed: Vec<String>,
    pub end_line: usize,
}

#[derive(Debug, Clone)]
pub struct VariableInfo {
    pub name: String,
    pub loc: SourceLoc,
}

#[derive(Debug, Clone)]
pub struct CallInfo {
    pub name: String,
    pub loc: SourceLoc,
    pub arg_count: usize,
}

pub struct DocumentAnalysis {
    pub errors: Vec<RexxDiagnostic>,
    pub labels: Vec<LabelInfo>,
    pub subroutines: Vec<SubroutineInfo>,
    pub assignments: Vec<VariableInfo>,
    pub references: Vec<VariableInfo>,
    pub calls: Vec<CallInfo>,
    pub stems: Vec<String>,
    pub program: Option<Program>,
    pub source_lines: Vec<String>,
}

impl DocumentAnalysis {
    pub fn analyze(source: &str) -> Self {
        let source_lines: Vec<String> = source.lines().map(String::from).collect();
        let mut errors = Vec::new();
        let empty = Self::empty(errors.clone(), source_lines.clone());

        let mut lexer = Lexer::new(source);
        let tokens = match lexer.tokenize() {
            Ok(tokens) => tokens,
            Err(e) => {
                errors.push(e);
                return Self { errors, ..empty };
            }
        };

        let mut parser = Parser::new(tokens);
        let program = match parser.parse() {
            Ok(program) => program,
            Err(e) => {
                errors.push(e);
                return Self { errors, ..empty };
            }
        };

        let mut collector = AstCollector::default();
        collector.walk_clauses(&program.clauses);

        let subroutines = detect_subroutines(&program.clauses, &collector.labels, &source_lines);

        Self {
            errors,
            labels: collector.labels,
            subroutines,
            assignments: collector.assignments,
            references: collector.references,
            calls: collector.calls,
            stems: collector.stems,
            program: Some(program),
            source_lines,
        }
    }

    fn empty(errors: Vec<RexxDiagnostic>, source_lines: Vec<String>) -> Self {
        Self {
            errors,
            labels: Vec::new(),
            subroutines: Vec::new(),
            assignments: Vec::new(),
            references: Vec::new(),
            calls: Vec::new(),
            stems: Vec::new(),
            program: None,
            source_lines,
        }
    }
}

#[derive(Default)]
struct AstCollector {
    labels: Vec<LabelInfo>,
    assignments: Vec<VariableInfo>,
    references: Vec<VariableInfo>,
    calls: Vec<CallInfo>,
    stems: Vec<String>,
}

impl AstCollector {
    fn walk_clauses(&mut self, clauses: &[Clause]) {
        for clause in clauses {
            self.walk_clause(clause);
        }
    }

    fn walk_clause(&mut self, clause: &Clause) {
        match &clause.kind {
            ClauseKind::Label(name) => {
                self.labels.push(LabelInfo {
                    name: name.clone(),
                    loc: clause.loc.clone(),
                });
            }
            ClauseKind::Assignment { target, expr } => {
                self.walk_assignment(target, &clause.loc);
                self.walk_expr(expr);
            }
            ClauseKind::Say(expr) => self.walk_expr(expr),
            ClauseKind::Call { name, args } => {
                self.calls.push(CallInfo {
                    name: name.clone(),
                    loc: clause.loc.clone(),
                    arg_count: args.len(),
                });
                for arg in args {
                    self.walk_expr(arg);
                }
            }
            ClauseKind::Do(block) => self.walk_do_block(block),
            ClauseKind::If {
                condition,
                then_clause,
                else_clause,
            } => {
                self.walk_expr(condition);
                self.walk_clause(then_clause);
                if let Some(ec) = else_clause {
                    self.walk_clause(ec);
                }
            }
            ClauseKind::Select {
                when_clauses,
                otherwise,
            } => {
                for (cond, body) in when_clauses {
                    self.walk_expr(cond);
                    self.walk_clauses(body);
                }
                if let Some(body) = otherwise {
                    self.walk_clauses(body);
                }
            }
            ClauseKind::Return(expr) | ClauseKind::Exit(expr) => {
                if let Some(e) = expr {
                    self.walk_expr(e);
                }
            }
            ClauseKind::Parse { template, .. }
            | ClauseKind::Arg(template)
            | ClauseKind::Pull(Some(template)) => {
                self.collect_template_vars(template, &clause.loc);
            }
            ClauseKind::Command(expr)
            | ClauseKind::Trace(expr)
            | ClauseKind::Interpret(expr)
            | ClauseKind::Push(Some(expr))
            | ClauseKind::Queue(Some(expr)) => {
                self.walk_expr(expr);
            }
            ClauseKind::Numeric(crate::ast::NumericSetting::Digits(Some(e))) => {
                self.walk_expr(e);
            }
            _ => {}
        }
    }

    fn walk_assignment(&mut self, target: &AssignTarget, loc: &SourceLoc) {
        match target {
            AssignTarget::Simple(name) => {
                self.assignments.push(VariableInfo {
                    name: name.clone(),
                    loc: loc.clone(),
                });
            }
            AssignTarget::Stem { stem, .. } => {
                let stem_name = format!("{stem}.");
                if !self.stems.contains(&stem_name) {
                    self.stems.push(stem_name);
                }
                self.assignments.push(VariableInfo {
                    name: stem.clone(),
                    loc: loc.clone(),
                });
            }
        }
    }

    fn collect_template_vars(&mut self, template: &crate::ast::ParseTemplate, loc: &SourceLoc) {
        for elem in &template.elements {
            if let crate::ast::TemplateElement::Variable(name) = elem {
                self.assignments.push(VariableInfo {
                    name: name.clone(),
                    loc: loc.clone(),
                });
            }
        }
    }

    fn walk_do_block(&mut self, block: &DoBlock) {
        match &block.kind {
            DoKind::Count(expr) | DoKind::While(expr) | DoKind::Until(expr) => {
                self.walk_expr(expr);
            }
            DoKind::Controlled(ctrl) => {
                self.assignments.push(VariableInfo {
                    name: ctrl.var.clone(),
                    loc: SourceLoc::new(0, 0),
                });
                self.walk_expr(&ctrl.start);
                for e in [
                    &ctrl.to,
                    &ctrl.by,
                    &ctrl.r#for,
                    &ctrl.while_cond,
                    &ctrl.until_cond,
                ]
                .into_iter()
                .flatten()
                {
                    self.walk_expr(e);
                }
            }
            DoKind::Simple | DoKind::Forever => {}
        }
        self.walk_clauses(&block.body);
    }

    fn walk_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Symbol(name) => {
                self.references.push(VariableInfo {
                    name: name.clone(),
                    loc: SourceLoc::new(0, 0),
                });
            }
            Expr::Compound { stem, .. } => {
                self.references.push(VariableInfo {
                    name: stem.clone(),
                    loc: SourceLoc::new(0, 0),
                });
            }
            Expr::FunctionCall { name, args } => {
                self.calls.push(CallInfo {
                    name: name.clone(),
                    loc: SourceLoc::new(0, 0),
                    arg_count: args.len(),
                });
                for arg in args {
                    self.walk_expr(arg);
                }
            }
            Expr::BinOp { left, right, .. } => {
                self.walk_expr(left);
                self.walk_expr(right);
            }
            Expr::UnaryOp { operand, .. } => {
                self.walk_expr(operand);
            }
            Expr::Paren(inner) => {
                self.walk_expr(inner);
            }
            Expr::StringLit(_) | Expr::Number(_) => {}
        }
    }
}

fn detect_subroutines(
    clauses: &[Clause],
    labels: &[LabelInfo],
    source_lines: &[String],
) -> Vec<SubroutineInfo> {
    let total_lines = source_lines.len();
    let mut subroutines = Vec::new();

    for (i, clause) in clauses.iter().enumerate() {
        if let ClauseKind::Label(name) = &clause.kind {
            let mut has_procedure = false;
            let mut exposed = Vec::new();

            if let Some(next_clause) = clauses.get(i + 1)
                && let ClauseKind::Procedure(expose) = &next_clause.kind
            {
                has_procedure = true;
                if let Some(names) = expose {
                    exposed.clone_from(names);
                }
            }

            let end_line = labels
                .iter()
                .filter(|l| l.loc.line > clause.loc.line)
                .map(|l| l.loc.line.saturating_sub(1))
                .min()
                .unwrap_or(total_lines);

            subroutines.push(SubroutineInfo {
                name: name.clone(),
                loc: clause.loc.clone(),
                has_procedure,
                exposed,
                end_line,
            });
        }
    }

    subroutines
}
