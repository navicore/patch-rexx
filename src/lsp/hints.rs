use tower_lsp::lsp_types::{InlayHint, InlayHintKind, InlayHintLabel, Position};

use crate::ast::{Clause, ClauseKind, Expr, NumericSetting, TemplateElement};

use super::analysis::DocumentAnalysis;

#[allow(clippy::cast_possible_truncation)]
pub fn inlay_hints(analysis: &DocumentAnalysis) -> Vec<InlayHint> {
    let mut hints = Vec::new();

    if let Some(ref program) = analysis.program {
        collect_hints(&program.clauses, &mut hints);
    }

    hints
}

#[allow(clippy::cast_possible_truncation)]
fn collect_hints(clauses: &[Clause], hints: &mut Vec<InlayHint>) {
    for clause in clauses {
        match &clause.kind {
            ClauseKind::Parse { template, .. } | ClauseKind::Arg(template) => {
                for elem in &template.elements {
                    if let TemplateElement::Variable(name) = elem {
                        hints.push(InlayHint {
                            position: Position {
                                line: clause.loc.line.saturating_sub(1) as u32,
                                character: clause.loc.col.saturating_sub(1) as u32,
                            },
                            label: InlayHintLabel::String(format!("{name}: parsed")),
                            kind: Some(InlayHintKind::TYPE),
                            text_edits: None,
                            tooltip: None,
                            padding_left: Some(true),
                            padding_right: Some(true),
                            data: None,
                        });
                    }
                }
            }
            ClauseKind::Numeric(NumericSetting::Digits(Some(Expr::Number(n)))) => {
                hints.push(InlayHint {
                    position: Position {
                        line: clause.loc.line.saturating_sub(1) as u32,
                        character: clause.loc.col.saturating_sub(1) as u32,
                    },
                    label: InlayHintLabel::String(format!("precision: {n} digits")),
                    kind: Some(InlayHintKind::PARAMETER),
                    text_edits: None,
                    tooltip: None,
                    padding_left: Some(true),
                    padding_right: Some(true),
                    data: None,
                });
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
