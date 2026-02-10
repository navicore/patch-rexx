use std::fmt::Write;

use tower_lsp::lsp_types::{Hover, HoverContents, MarkupContent, MarkupKind, Position};

use super::analysis::DocumentAnalysis;
use super::bif_docs;

static KEYWORD_DOCS: &[(&str, &str)] = &[
    ("SAY", "Writes a string to the default output stream."),
    (
        "IF",
        "Conditionally executes a clause. Syntax: IF expr THEN clause [ELSE clause]",
    ),
    (
        "DO",
        "Groups clauses or creates loops. Variants: DO, DO n, DO FOREVER, DO WHILE, DO UNTIL, DO var = start TO end.",
    ),
    (
        "SELECT",
        "Multi-way branch. Syntax: SELECT; WHEN expr THEN clause; ... OTHERWISE clause; END",
    ),
    (
        "CALL",
        "Calls an internal or external routine. Syntax: CALL name [args]",
    ),
    (
        "RETURN",
        "Returns from a subroutine, optionally with a value.",
    ),
    ("EXIT", "Exits the program, optionally with a return code."),
    (
        "PARSE",
        "Parses a string into variables using a template. Sources: ARG, PULL, VALUE, VAR, SOURCE, VERSION, LINEIN.",
    ),
    (
        "SIGNAL",
        "Transfers control to a label, or enables/disables condition traps.",
    ),
    ("LEAVE", "Exits the innermost (or named) DO loop."),
    (
        "ITERATE",
        "Skips to the next iteration of the innermost (or named) DO loop.",
    ),
    (
        "PROCEDURE",
        "Starts a new variable scope in a subroutine. EXPOSE selectively shares variables.",
    ),
    (
        "DROP",
        "Unassigns one or more variables, restoring them to their name.",
    ),
    (
        "NOP",
        "No operation. Used as a placeholder (e.g., in THEN/OTHERWISE).",
    ),
    (
        "INTERPRET",
        "Dynamically evaluates a REXX expression at runtime.",
    ),
    (
        "TRACE",
        "Controls execution tracing. Settings: A(ll), C(ommands), E(rror), I(ntermediates), L(abels), N(ormal), O(ff), R(esults).",
    ),
    (
        "ADDRESS",
        "Sets the command environment. Syntax: ADDRESS env [command] | ADDRESS VALUE expr",
    ),
    (
        "NUMERIC",
        "Controls arithmetic precision. Sub-keywords: DIGITS, FORM, FUZZ.",
    ),
    (
        "PULL",
        "Reads from the external data queue (or stdin), uppercased. Shorthand for PARSE UPPER PULL.",
    ),
    (
        "PUSH",
        "Pushes a string onto the external data queue (LIFO).",
    ),
    ("QUEUE", "Adds a string to the external data queue (FIFO)."),
    (
        "ARG",
        "Parses program/subroutine arguments (uppercased). Shorthand for PARSE UPPER ARG.",
    ),
];

pub fn hover_info(analysis: &DocumentAnalysis, position: Position) -> Option<Hover> {
    let word = word_at_position(analysis, position)?;
    let upper = word.to_uppercase();

    // Check BIFs first
    if let Some(bif) = bif_docs::lookup_bif(&upper) {
        let content = format!(
            "**{}**\n\n```\n{}\n```\n\n{}",
            bif.name, bif.signature, bif.description
        );
        return Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: content,
            }),
            range: None,
        });
    }

    // Check subroutines
    for sub in &analysis.subroutines {
        if sub.name == upper {
            let mut content = format!("**{}** — label", sub.name);
            if sub.has_procedure {
                content.push_str("\n\n`PROCEDURE");
                if !sub.exposed.is_empty() {
                    content.push_str(" EXPOSE ");
                    content.push_str(&sub.exposed.join(" "));
                }
                content.push('`');
            }
            let _ = write!(content, "\n\nDefined at line {}", sub.loc.line);
            return Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: content,
                }),
                range: None,
            });
        }
    }

    // Check keywords
    for &(kw, desc) in KEYWORD_DOCS {
        if upper == kw {
            let content = format!("**{kw}** — REXX keyword\n\n{desc}");
            return Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: content,
                }),
                range: None,
            });
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
