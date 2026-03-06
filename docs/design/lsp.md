# Language Server (LSP)

The LSP module (`src/lsp/`) provides editor integration via the Language Server Protocol. It is feature-gated behind `--features lsp` and uses `tower-lsp` with `tokio`.

## Architecture

```
Editor ←→ tower-lsp ←→ RexxLanguageServer
                              │
                        DocumentAnalysis
                         (lexer + parser + AST walk)
```

On every document change, the server re-lexes and re-parses the source, then performs a static analysis pass to collect labels, subroutines, variables, calls, and stems.

## Modules

| File | Role |
|------|------|
| `server.rs` | tower-lsp `LanguageServer` trait implementation, request dispatch |
| `analysis.rs` | Document analysis: AST walking, label/variable/call collection |
| `diagnostics.rs` | Convert `RexxDiagnostic` to LSP `Diagnostic` |
| `completion.rs` | Completion items: keywords, BIFs, labels, variables, stems |
| `hover.rs` | Hover docs: keyword syntax, BIF signatures and descriptions |
| `definition.rs` | Go-to-definition: resolve symbol to label location |
| `symbols.rs` | Document symbols: outline of labels and subroutines |
| `actions.rs` | Code actions: "Did you mean?" via Levenshtein distance |
| `hints.rs` | Inlay hints: PARSE variable annotations, NUMERIC precision |
| `bif_docs.rs` | BIF documentation database (names, signatures, descriptions) |
| `util.rs` | Helpers: word-at-cursor extraction |

## Features

### Diagnostics
Parse errors are reported as LSP diagnostics with line/column ranges. The severity maps from REXX error numbers.

### Completion
Triggered on any character (and `.` for stem completion). Sources: 53 REXX keywords, all BIF names, labels/subroutines from the current document, assigned variables, and stem names.

### Hover
Shows syntax and description for REXX keywords (SAY, IF, DO, PARSE, etc.) and BIF signatures with category and description.

### Go to Definition
Resolves a symbol under the cursor to its label definition in the current document. Case-insensitive matching.

### Document Symbols
Provides an outline of labels and subroutines. Subroutines with PROCEDURE are shown as FUNCTION symbols; plain labels as KEY symbols.

### Code Actions
When a diagnostic reports a routine-not-found error, the server suggests "Did you mean?" fixes using Levenshtein distance (threshold: 2) against known labels and BIF names.

### Inlay Hints
Annotates PARSE/ARG template variables with "parsed" hints, and NUMERIC DIGITS settings with precision information.

## Building

```bash
cargo build --release --features lsp
```

This produces the `rexx-lsp` binary.
