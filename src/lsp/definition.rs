use tower_lsp::lsp_types::{GotoDefinitionResponse, Location, Position, Range, Url};

use super::analysis::DocumentAnalysis;
use super::util::word_at_position;

#[allow(clippy::cast_possible_truncation)]
pub fn goto_definition(
    analysis: &DocumentAnalysis,
    uri: &Url,
    position: Position,
) -> Option<GotoDefinitionResponse> {
    let word = word_at_position(analysis, position)?;
    let upper = word.to_uppercase();

    for label in &analysis.labels {
        if label.name == upper {
            let line = label.loc.line.saturating_sub(1) as u32;
            return Some(GotoDefinitionResponse::Scalar(Location {
                uri: uri.clone(),
                range: Range {
                    start: Position { line, character: 0 },
                    end: Position {
                        line,
                        character: label.name.len() as u32,
                    },
                },
            }));
        }
    }

    None
}
