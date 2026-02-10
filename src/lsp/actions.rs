use tower_lsp::lsp_types::{
    CodeAction, CodeActionKind, Position, Range, TextEdit, Url, WorkspaceEdit,
};

use super::analysis::DocumentAnalysis;
use super::bif_docs;

pub fn code_actions(analysis: &DocumentAnalysis, uri: &Url, range: &Range) -> Vec<CodeAction> {
    let mut actions = Vec::new();

    for call in &analysis.calls {
        let call_line = call.loc.line.saturating_sub(1);
        #[allow(clippy::cast_possible_truncation)]
        if call_line as u32 >= range.start.line && call_line as u32 <= range.end.line {
            for label in &analysis.labels {
                if label.name != call.name && edit_distance(&call.name, &label.name) <= 2 {
                    actions.push(make_rename_action(uri, call, &label.name, "label"));
                }
            }

            for bif in bif_docs::all_bifs() {
                if bif.name != call.name && edit_distance(&call.name, bif.name) <= 2 {
                    actions.push(make_rename_action(uri, call, bif.name, "built-in function"));
                }
            }
        }
    }

    actions
}

fn make_rename_action(
    uri: &Url,
    call: &super::analysis::CallInfo,
    suggestion: &str,
    kind_label: &str,
) -> CodeAction {
    let line = call.loc.line.saturating_sub(1);
    let col = call.loc.col.saturating_sub(1);

    #[allow(clippy::cast_possible_truncation)]
    let range = Range {
        start: Position {
            line: line as u32,
            character: col as u32,
        },
        end: Position {
            line: line as u32,
            character: (col + call.name.len()) as u32,
        },
    };

    let mut changes = std::collections::HashMap::new();
    changes.insert(
        uri.clone(),
        vec![TextEdit {
            range,
            new_text: suggestion.to_string(),
        }],
    );

    CodeAction {
        title: format!("Did you mean '{suggestion}' ({kind_label})?"),
        kind: Some(CodeActionKind::QUICKFIX),
        edit: Some(WorkspaceEdit {
            changes: Some(changes),
            ..WorkspaceEdit::default()
        }),
        ..CodeAction::default()
    }
}

fn edit_distance(a: &str, b: &str) -> usize {
    let a_chars: Vec<char> = a.chars().collect();
    let b_chars: Vec<char> = b.chars().collect();
    let m = a_chars.len();
    let n = b_chars.len();

    let mut dp = vec![vec![0usize; n + 1]; m + 1];

    for (i, row) in dp.iter_mut().enumerate().take(m + 1) {
        row[0] = i;
    }
    for (j, val) in dp[0].iter_mut().enumerate().take(n + 1) {
        *val = j;
    }

    for i in 1..=m {
        for j in 1..=n {
            let cost = usize::from(a_chars[i - 1] != b_chars[j - 1]);
            dp[i][j] = (dp[i - 1][j] + 1)
                .min(dp[i][j - 1] + 1)
                .min(dp[i - 1][j - 1] + cost);
        }
    }

    dp[m][n]
}
