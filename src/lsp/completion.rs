use tower_lsp::lsp_types::{CompletionItem, CompletionItemKind};

use super::analysis::DocumentAnalysis;
use super::bif_docs;

static REXX_KEYWORDS: &[&str] = &[
    "SAY",
    "IF",
    "THEN",
    "ELSE",
    "DO",
    "END",
    "SELECT",
    "WHEN",
    "OTHERWISE",
    "CALL",
    "RETURN",
    "EXIT",
    "PARSE",
    "SIGNAL",
    "LEAVE",
    "ITERATE",
    "PROCEDURE",
    "EXPOSE",
    "DROP",
    "NOP",
    "INTERPRET",
    "TRACE",
    "ADDRESS",
    "NUMERIC",
    "PULL",
    "PUSH",
    "QUEUE",
    "ARG",
    "UPPER",
    "WHILE",
    "UNTIL",
    "FOREVER",
    "TO",
    "BY",
    "FOR",
    "VALUE",
    "WITH",
    "ON",
    "OFF",
    "VAR",
    "SOURCE",
    "VERSION",
    "LINEIN",
    "DIGITS",
    "FORM",
    "FUZZ",
];

pub fn completions(analysis: &DocumentAnalysis) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    // REXX keywords
    for &kw in REXX_KEYWORDS {
        items.push(CompletionItem {
            label: kw.to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some("REXX keyword".to_string()),
            ..CompletionItem::default()
        });
    }

    // BIF names
    for bif in bif_docs::all_bifs() {
        items.push(CompletionItem {
            label: bif.name.to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some(bif.signature.to_string()),
            ..CompletionItem::default()
        });
    }

    // Labels/subroutines from current document
    for sub in &analysis.subroutines {
        let kind = if sub.has_procedure {
            CompletionItemKind::FUNCTION
        } else {
            CompletionItemKind::REFERENCE
        };
        items.push(CompletionItem {
            label: sub.name.clone(),
            kind: Some(kind),
            detail: Some("label".to_string()),
            ..CompletionItem::default()
        });
    }

    // Variables from assignments
    let mut seen_vars = std::collections::HashSet::new();
    for var in &analysis.assignments {
        if seen_vars.insert(var.name.clone()) {
            items.push(CompletionItem {
                label: var.name.clone(),
                kind: Some(CompletionItemKind::VARIABLE),
                detail: Some("variable".to_string()),
                ..CompletionItem::default()
            });
        }
    }

    // Stem variables
    for stem in &analysis.stems {
        items.push(CompletionItem {
            label: stem.clone(),
            kind: Some(CompletionItemKind::VARIABLE),
            detail: Some("stem variable".to_string()),
            ..CompletionItem::default()
        });
    }

    items
}
