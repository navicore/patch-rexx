# Architecture Overview

patch-rexx is a tree-walking interpreter. Source code flows through a linear pipeline:

```
Source text → Lexer → Tokens → Parser → AST → Evaluator → Output
```

## Module Map

| Module | File | Role |
|--------|------|------|
| Lexer | `src/lexer.rs` | Tokenizes source into symbols, strings, numbers, operators |
| Parser | `src/parser.rs` | Builds AST from token stream with REXX operator precedence |
| AST | `src/ast.rs` | Clause and expression types — the intermediate representation |
| Evaluator | `src/eval.rs` | Walks the AST, executes clauses, evaluates expressions |
| Built-ins | `src/builtins.rs` | 53 ANSI REXX built-in functions |
| Environment | `src/env.rs` | Variable scoping: PROCEDURE/EXPOSE, stems, ADDRESS |
| Values | `src/value.rs` | RexxValue (everything-is-a-string) with BigDecimal arithmetic |
| Errors | `src/error.rs` | ANSI error numbers (4--49) with source-location diagnostics |
| External | `src/external.rs` | External function file resolution (REXXPATH) |
| REPL | `src/repl.rs` | Interactive line editor (vim-line + crossterm) |
| CLI | `src/main.rs` | Entry point: file, `-e`, REPL, pipe modes |
| LSP | `src/lsp/` | Language Server Protocol (tower-lsp, feature-gated) |

## Execution Flow

1. **Lexer** scans source text into a flat `Vec<Token>`. Each token carries a `SourceLoc` (line, column). Nested `/* */` comments are consumed here. Hex (`'FF'x`) and binary (`'0101'b`) string literals are recognized.

2. **Parser** consumes tokens and produces a `Program` (a `Vec<Clause>`). Each clause has a `ClauseKind` and a `SourceLoc`. Expression parsing follows REXX precedence: power → unary → multiplication → addition → concatenation → comparison → AND → OR/XOR.

3. **Evaluator** walks the clause list sequentially. It maintains:
   - An `Environment` for variable scoping
   - Numeric settings (DIGITS/FORM/FUZZ)
   - A label map for SIGNAL and CALL dispatch
   - An argument stack for subroutine parameters
   - Condition trap state (7 conditions)
   - An external data queue (`VecDeque<String>`)
   - TRACE settings

4. **SIGNAL** can redirect execution to any label index, unwinding all loops. The evaluator's `exec()` loop handles this via a re-dispatch mechanism.

5. **INTERPRET** re-enters the full pipeline: lex → parse → evaluate the string in the current environment. A recursion counter (max 100) prevents runaway nesting.

## Feature Gating

The LSP module is behind a Cargo feature flag (`lsp`). The core interpreter has zero async dependencies — tower-lsp and tokio are only pulled in when building `rexx-lsp`.

## Custom Command Handlers

The evaluator supports pluggable command handlers via `with_command_handler()` and `with_env_command_handler()`. This allows embedding patch-rexx in larger applications where ADDRESS commands should route to application-specific logic rather than the system shell.
