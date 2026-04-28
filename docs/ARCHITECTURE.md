# Architecture

A high-level map of patch-rexx. For deeper per-subsystem notes, see the other reference docs in `docs/`.

## Context & Scope

patch-rexx is a standalone REXX interpreter targeting ANSI X3.274-1996, distributed as a single static binary (`rexx`). An optional companion binary (`rexx-lsp`, behind the `lsp` Cargo feature) speaks the Language Server Protocol for editor integration.

Inputs:
- REXX source from a file argument, `-e` expression, piped stdin, or the interactive REPL.
- External `.rexx`/`.rex` files resolved via the source's directory and the `REXXPATH` env var.
- Host shell (via `ADDRESS SYSTEM`/`CMD`/`SH`) — or a pluggable command handler when embedded.

Outputs:
- `say` to stdout, errors to stderr, process exit code from `EXIT`.
- LSP messages over stdio when run as `rexx-lsp`.

Out of scope: no FFI to C extensions, no bytecode compilation, no networked services. Embedders supply their own `ADDRESS` handlers via `Evaluator::with_command_handler` / `with_env_command_handler`.

## Solution Strategy

- **Rust, edition 2024**, `unsafe_code = "forbid"`. Memory safety and a single static binary are core goals.
- **Tree-walking interpreter.** Source → tokens → AST → direct evaluation. No IR, no JIT. Matches REXX's INTERPRET-friendly semantics: re-entering the pipeline on a string is the implementation of `INTERPRET`.
- **`bigdecimal` + `num-bigint`** for arbitrary-precision decimal arithmetic, since REXX numerics are decimal with `NUMERIC DIGITS`/`FORM`/`FUZZ` knobs.
- **`regex`** powers PARSE template patterns and several BIFs.
- **`clap`** for the CLI; **`vim-line` + `crossterm`** for the REPL's vi-mode line editor with persistent history.
- **`tower-lsp` + `tokio`** are feature-gated (`lsp`); the core interpreter has zero async dependencies.

## Building Blocks

Library crate (`src/lib.rs`) exposes the pipeline modules; binaries layer CLI and LSP on top.

| Module | Role |
|--------|------|
| `lexer.rs` | Tokenizer: symbols, numbers, strings, all operators, nested `/* */` comments, hex (`'FF'x`) and binary (`'0101'b`) literals. Each token carries a `SourceLoc`. |
| `parser.rs` | Token stream → `Program` (`Vec<Clause>`). All clause types, DO variants, PARSE templates. REXX precedence: power → unary → mul → add → concat → compare → AND → OR/XOR. |
| `ast.rs` | Clause and expression types — the IR walked by the evaluator. |
| `eval.rs` | `Evaluator` walks clauses; owns numeric settings, label map, argument stack, condition trap state, data queue, TRACE state. |
| `builtins.rs` | 53 ANSI BIFs (string, word, numeric, conversion, date/time, system). |
| `env.rs` | `Environment`: scope stack, simple vars, stem variables, ADDRESS state, source path, condition info. |
| `value.rs` | `RexxValue` (always a string) plus `to_decimal()` and the NUMERIC DIGITS/FORM/FUZZ apparatus. |
| `error.rs` | ANSI REXX error numbers (4–49) wrapped in `RexxDiagnostic` with source-location detail. |
| `external.rs` | Resolves external function calls to `.rexx`/`.rex` files via source dir then `REXXPATH`. |
| `repl.rs` | Interactive line editor (vi-mode), `~/.rexx_history`. |
| `main.rs` / `bin/patch-rexx-lsp.rs` | CLI entry point and LSP entry point. |
| `lsp/` | Diagnostics, completion, hover, go-to-definition, document symbols, code actions, inlay hints. |

Core domain entities and invariants:

- **`RexxValue` (value.rs)** — owns a `String`. Invariant: a value is *always* inspectable as text; numeric form is derived on demand via `to_decimal()` and never replaces the string.
- **`Environment` (env.rs)** — owns the scope stack. Invariant: `PROCEDURE` pushes a fully isolated scope; the only way to see a parent variable is `EXPOSE`, which is also the channel by which writes propagate back on scope pop. Variables that don't exist resolve to their own uppercased name (REXX's "uninitialized symbol" rule), not an error — `NOVALUE` is a separately trappable condition.
- **`Evaluator` (eval.rs)** — the consistency boundary for a single program run. Owns numeric settings, label map, condition traps, data queue, and TRACE state; these are reachable only through it. `SIGNAL` unwinds loops by re-dispatching from the label index inside `exec()`.
- **Stem variables (env.rs `StemVar`)** — `name.` is the aggregate root: a default value plus an entry map keyed by resolved tail. All compound references go through the stem; `DROP STEM.` erases the whole aggregate atomically.

## Crosscutting Concepts

- **Errors.** Every fallible operation returns `error::RexxResult<T>` = `Result<T, RexxDiagnostic>`. Diagnostics carry an ANSI error number plus source location and human-readable detail; the CLI prints them and exits non-zero.
- **Source locations.** Threaded from the lexer through tokens, clauses, and AST nodes so diagnostics, TRACE output, and the LSP can all point at exact positions.
- **Concurrency.** The interpreter is single-threaded and synchronous. `tokio` only enters via the `lsp` feature.
- **INTERPRET.** Implemented as re-entry into lex → parse → evaluate against the *current* environment. A recursion counter caps nesting at 100.
- **TRACE.** Pervasive cross-cutting hook in the evaluator; emits `>V>` / `>O>` / `>L>` / `>P>` / `>F>` / `>C>` tagged lines for variable / operator / literal / prefix / function / compound intermediates per ANSI.
- **Feature gating.** The `lsp` Cargo feature is the only conditional compilation: it pulls in `tower-lsp`, `tokio`, `serde_json` and the `lsp/` module. The core has no async surface.
- **Lints.** `clippy::pedantic` warn-by-default with a small allow-list; `unsafe_code = "forbid"` at the crate root.
- **Testing.** Phase-by-phase integration tests (`tests/phase1.rs` … `tests/phase12a.rs`) plus an ANSI conformance suite (`tests/conformance.rs`) and external-file resolution tests (`tests/rexxfile.rs`). 500+ tests total.
