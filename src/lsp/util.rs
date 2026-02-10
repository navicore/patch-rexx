use tower_lsp::lsp_types::Position;

use super::analysis::DocumentAnalysis;

/// Extract the REXX word under the cursor.
///
/// Excludes `.` so that compound variables like `ARR.1` resolve to `ARR`
/// rather than matching the whole compound token.
pub fn word_at_position(analysis: &DocumentAnalysis, position: Position) -> Option<String> {
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
    ch.is_alphanumeric() || ch == '_' || ch == '!' || ch == '?' || ch == '@'
}
