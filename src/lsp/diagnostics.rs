use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, NumberOrString, Position, Range};

use crate::error::RexxDiagnostic;

#[allow(clippy::cast_possible_truncation)]
pub fn to_lsp_diagnostic(diag: &RexxDiagnostic) -> Diagnostic {
    let line = diag
        .location
        .as_ref()
        .map_or(0, |loc| loc.line.saturating_sub(1));
    let col = diag
        .location
        .as_ref()
        .map_or(0, |loc| loc.col.saturating_sub(1));

    let end_col = col + 1;

    let message = if let Some(ref detail) = diag.detail {
        format!("{}: {detail}", diag.error.message())
    } else {
        diag.error.message().to_string()
    };

    Diagnostic {
        range: Range {
            start: Position {
                line: line as u32,
                character: col as u32,
            },
            end: Position {
                line: line as u32,
                character: end_col as u32,
            },
        },
        severity: Some(DiagnosticSeverity::ERROR),
        code: Some(NumberOrString::Number(
            i32::from(diag.error.number() as u16),
        )),
        source: Some("rexx".to_string()),
        message,
        ..Diagnostic::default()
    }
}
