# Roadmap

The full phase-by-phase plan lives in the project-root [`ROADMAP.md`](../ROADMAP.md). This file is a short snapshot of current state and known next steps.

## Current State

- Functional end-to-end interpreter with 500+ tests; ANSI X3.274-1996 conformance suite (100+ tests) is in place.
- All thirteen development phases listed in the root roadmap are complete except for items called out below.
- LSP (`rexx-lsp`) ships with diagnostics, completion, hover, go-to-definition, document symbols, code actions, and inlay hints.
- 53 of ~70 ANSI BIFs implemented.

## Known Next Steps

Carried over from the root roadmap — items still open:

- **Built-in functions (Phase 5).** Implement: `CENTER`/`CENTRE`, `BITAND`, `BITOR`, `BITXOR`, `ERRORTEXT`, `SOURCELINE`, `SYMBOL`, `VALUE` (as a BIF).
- **TRACE (Phase 9).** Full interactive debugging — pause and accept input at trace points.
- **LSP (Phase 10).** Neovim plugin (following the `patch-seq.nvim` pattern).
- **Distribution (Phase 12).** Static binary builds (musl on Linux, native on macOS); cross-compilation to Linux amd64/arm64, macOS, Windows; WASM target for an in-browser playground.
- **Performance (Phase 12).** Profile and optimize hot paths (arithmetic, string ops, PARSE).
- **Validation (Phase 12).** Evaluate the RexxLA test suite as an additional conformance source.
- **rexxlings (Phase 14, separate repo).** A rustlings-style interactive course for REXX, using `rexx` as the runner.
