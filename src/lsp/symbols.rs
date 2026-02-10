use tower_lsp::lsp_types::{DocumentSymbol, Position, Range, SymbolKind};

use super::analysis::DocumentAnalysis;

#[allow(clippy::cast_possible_truncation, deprecated)]
pub fn document_symbols(analysis: &DocumentAnalysis) -> Vec<DocumentSymbol> {
    let mut symbols = Vec::new();

    for sub in &analysis.subroutines {
        let start_line = sub.loc.line.saturating_sub(1) as u32;
        let end_line = sub.end_line.saturating_sub(1).max(start_line as usize) as u32;

        let kind = if sub.has_procedure {
            SymbolKind::FUNCTION
        } else {
            SymbolKind::KEY
        };

        let detail = if sub.has_procedure {
            if sub.exposed.is_empty() {
                Some("PROCEDURE".to_string())
            } else {
                Some(format!("PROCEDURE EXPOSE {}", sub.exposed.join(" ")))
            }
        } else {
            None
        };

        symbols.push(DocumentSymbol {
            name: sub.name.clone(),
            detail,
            kind,
            tags: None,
            deprecated: None,
            range: Range {
                start: Position {
                    line: start_line,
                    character: 0,
                },
                end: Position {
                    line: end_line,
                    character: 0,
                },
            },
            selection_range: Range {
                start: Position {
                    line: start_line,
                    character: 0,
                },
                end: Position {
                    line: start_line,
                    character: sub.name.len() as u32,
                },
            },
            children: None,
        });
    }

    symbols
}
