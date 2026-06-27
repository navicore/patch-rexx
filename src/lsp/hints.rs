//! LSP inlay hints — inline decorations for PARSE targets and NUMERIC DIGITS.
//!
//! [`inlay_hints`] walks a parsed [`DocumentAnalysis`] and emits one hint per
//! parsed template variable (": parsed") and one per `NUMERIC DIGITS n`
//! ("precision: n digits").

use tower_lsp::lsp_types::{InlayHint, InlayHintKind, InlayHintLabel, Position};

use crate::ast::{Clause, ClauseKind, Expr, NumericSetting, TemplateElement};

use super::analysis::DocumentAnalysis;

/// Build the inlay hints for a document: one per parsed template variable and
/// one per `NUMERIC DIGITS` clause. Returns an empty `Vec` when the document
/// has no parse (`analysis.program` is `None`).
pub fn inlay_hints(analysis: &DocumentAnalysis) -> Vec<InlayHint> {
    let mut hints = Vec::new();

    if let Some(ref program) = analysis.program {
        collect_hints(&program.clauses, &mut hints);
    }

    hints
}

/// Build one inlay hint anchored at `clause`'s start position. The single
/// `usize as u32` cast lives here so the truncation suppression is centralized.
#[allow(clippy::cast_possible_truncation)]
fn hint(clause: &Clause, label: String, kind: InlayHintKind) -> InlayHint {
    InlayHint {
        position: Position {
            line: clause.loc.line.saturating_sub(1) as u32,
            character: clause.loc.col.saturating_sub(1) as u32,
        },
        label: InlayHintLabel::String(label),
        kind: Some(kind),
        text_edits: None,
        tooltip: None,
        padding_left: Some(true),
        padding_right: Some(true),
        data: None,
    }
}

/// Recurse over clauses, pushing hints for PARSE/ARG template variables and
/// `NUMERIC DIGITS` clauses.
fn collect_hints(clauses: &[Clause], hints: &mut Vec<InlayHint>) {
    for clause in clauses {
        match &clause.kind {
            ClauseKind::Parse { template, .. } | ClauseKind::Arg(template) => {
                for elem in &template.elements {
                    if let TemplateElement::Variable(name) = elem {
                        hints.push(hint(clause, format!("{name}: parsed"), InlayHintKind::TYPE));
                    }
                }
            }
            ClauseKind::Numeric(NumericSetting::Digits(Some(Expr::Number(n)))) => {
                hints.push(hint(
                    clause,
                    format!("precision: {n} digits"),
                    InlayHintKind::PARAMETER,
                ));
            }
            ClauseKind::Do(block) => {
                collect_hints(&block.body, hints);
            }
            ClauseKind::If {
                then_clause,
                else_clause,
                ..
            } => {
                collect_hints(&[then_clause.as_ref().clone()], hints);
                if let Some(ec) = else_clause {
                    collect_hints(&[ec.as_ref().clone()], hints);
                }
            }
            ClauseKind::Select {
                when_clauses,
                otherwise,
            } => {
                for (_, body) in when_clauses {
                    collect_hints(body, hints);
                }
                if let Some(body) = otherwise {
                    collect_hints(body, hints);
                }
            }
            _ => {}
        }
    }
}
