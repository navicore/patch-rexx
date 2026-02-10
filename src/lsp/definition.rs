use tower_lsp::lsp_types::{GotoDefinitionResponse, Location, Position, Range, Url};

use super::analysis::DocumentAnalysis;

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

fn word_at_position(analysis: &DocumentAnalysis, position: Position) -> Option<String> {
    let line_idx = position.line as usize;
    let col = position.character as usize;
    let line = analysis.source_lines.get(line_idx)?;

    if col >= line.len() {
        return None;
    }

    let chars: Vec<char> = line.chars().collect();

    let mut start = col;
    while start > 0 && is_rexx_word_char(chars[start - 1]) {
        start -= 1;
    }

    let mut end = col;
    while end < chars.len() && is_rexx_word_char(chars[end]) {
        end += 1;
    }

    if start == end {
        return None;
    }

    Some(chars[start..end].iter().collect())
}

fn is_rexx_word_char(ch: char) -> bool {
    ch.is_alphanumeric() || ch == '_' || ch == '.' || ch == '!' || ch == '?' || ch == '@'
}
