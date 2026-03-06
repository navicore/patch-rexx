# patch-rexx Roadmap

A modern REXX interpreter in Rust. Single static binary. Correct per ANSI X3.274-1996.

## Current State

Functional interpreter with 500+ tests. Phases 1--9, 11, and 13 are complete. The LSP (Phase 10) is implemented. Phase 12 conformance work is in progress.

Core modules:
- **lexer.rs** -- Full tokenizer: strings, numbers, symbols, all operators, nested `/* */` comments, hex/binary string literals
- **parser.rs** -- All REXX clause types, expression precedence, PARSE templates, DO variants
- **eval.rs** -- Full evaluator: control flow, PARSE, SIGNAL, INTERPRET, ADDRESS, TRACE, NUMERIC, queues
- **ast.rs** -- Complete AST covering all REXX clause types, DO variants, PARSE templates, expressions
- **builtins.rs** -- 53 built-in functions (string, word, numeric, conversion, date/time, system)
- **env.rs** -- Variable environments with REXX scoping: PROCEDURE isolation, EXPOSE, stem/compound variables, DROP
- **external.rs** -- External function resolution: .rexx/.rex files, REXXPATH search
- **value.rs** -- RexxValue (everything-is-a-string), BigDecimal integration, NUMERIC DIGITS/FORM/FUZZ settings
- **error.rs** -- All ANSI REXX error numbers (4--49) with source-location diagnostics
- **repl.rs** -- Interactive REPL with vim-mode editing (vim-line + crossterm), persistent history
- **main.rs** -- CLI: file execution, `-e` eval, REPL, pipe mode
- **lsp/** -- Language Server Protocol: diagnostics, completion, hover, definition, symbols, actions, hints

## Phase 1 -- SAY and Expressions

Goal: `say 2 + 3` prints `5`.

- [x] Parser: expression parsing with REXX operator precedence
- [x] Parser: SAY instruction
- [x] Parser: assignment (`x = expr`)
- [x] Evaluator: arithmetic (+, -, *, /, %, //, **)
- [x] Evaluator: string concatenation (abuttal, `||`, blank concatenation)
- [x] Evaluator: comparison operators (normal and strict)
- [x] Evaluator: logical operators (&, |, &&, \)
- [x] Wire parser and evaluator into main.rs run_source
- [x] REPL evaluates and displays results

Milestone: run classic REXX one-liners interactively.

## Phase 2 -- Control Flow

Goal: IF/THEN/ELSE, DO loops, SELECT/WHEN.

- [x] Parser: IF/THEN/ELSE
- [x] Parser: DO/END (simple, FOREVER, counted, WHILE, UNTIL, controlled)
- [x] Parser: SELECT/WHEN/OTHERWISE/END
- [x] Parser: LEAVE, ITERATE (with optional name targeting)
- [x] Parser: NOP
- [x] Evaluator: all control flow
- [x] Labels (for SIGNAL targets and CALL)

Milestone: FizzBuzz, factorial, standard interview programs.

## Phase 3 -- Subroutines and Functions

Goal: CALL, RETURN, internal routines, PROCEDURE EXPOSE.

- [x] Parser: CALL instruction
- [x] Parser: function call syntax in expressions: `name(args)`
- [x] Parser: PROCEDURE / PROCEDURE EXPOSE
- [x] Parser: RETURN / EXIT
- [x] Evaluator: internal subroutine calls (label-based)
- [x] Evaluator: PROCEDURE scoping (already modeled in env.rs)
- [x] Evaluator: function calls (must return a value)
- [x] Evaluator: CALL (may or may not return via RESULT)
- [x] ARG / PARSE ARG for parameter passing

Milestone: recursive programs, multi-routine scripts.

## Phase 4 -- PARSE

Goal: full PARSE instruction -- REXX's signature feature.

- [x] PARSE ARG
- [x] PARSE PULL
- [x] PARSE VAR name
- [x] PARSE VALUE expr WITH
- [x] PARSE SOURCE
- [x] PARSE VERSION
- [x] PARSE LINEIN
- [x] PARSE UPPER variants
- [x] Template patterns: literal string matching
- [x] Template patterns: absolute column positions
- [x] Template patterns: relative column positions (+n, -n)
- [x] Template patterns: variable patterns
- [x] Template patterns: dot placeholder (discard)
- [x] Multiple template parsing (comma-separated argument strings)

Milestone: real REXX string processing -- parsing input, extracting fields.

## Phase 5 -- Built-in Functions (~70 BIFs)

Goal: ANSI REXX built-in function library.

53 of ~70 ANSI BIFs implemented. See below for what remains.

### String
- [x] ABBREV, COPIES, COMPARE
- [ ] CENTER/CENTRE
- [x] DELSTR, DELWORD, INSERT, LASTPOS, LEFT, LENGTH
- [x] OVERLAY, POS, REVERSE, RIGHT, STRIP
- [x] SUBSTR, SUBWORD, TRANSLATE, VERIFY, WORD
- [x] WORDINDEX, WORDLENGTH, WORDPOS, WORDS
- [x] CHANGESTR, COUNTSTR, SPACE (extensions)

### Numeric
- [x] ABS, FORMAT, MAX, MIN, SIGN, TRUNC
- [x] RANDOM

### Conversion
- [x] B2X, C2D, C2X, D2C, D2X, X2B, X2C, X2D
- [x] DATATYPE

### Bit
- [ ] BITAND, BITOR, BITXOR

### Misc
- [x] ADDRESS, ARG, CONDITION, DATE, DIGITS
- [ ] ERRORTEXT
- [x] FORM, FUZZ, QUEUED
- [ ] SOURCELINE
- [ ] SYMBOL
- [x] TIME, TRACE
- [ ] VALUE (as a BIF)
- [x] XRANGE

**Remaining**: CENTER/CENTRE, BITAND, BITOR, BITXOR, ERRORTEXT, SOURCELINE, SYMBOL, VALUE (BIF).

Milestone: REXX programs that rely on the standard function library work.

## Phase 6 -- SIGNAL and Conditions

Goal: error trapping and SIGNAL flow control.

- [x] SIGNAL label (unconditional transfer)
- [x] SIGNAL VALUE expr
- [x] SIGNAL ON condition NAME label
- [x] SIGNAL OFF condition
- [x] Conditions: ERROR, FAILURE, HALT, NOVALUE, NOTREADY, SYNTAX, LOSTDIGITS
- [x] Condition information: CONDITION('C'), CONDITION('D'), etc.
- [x] RC (return code) special variable

Milestone: robust error handling in REXX programs.

## Phase 7 -- INTERPRET

Goal: dynamic code evaluation.

- [x] INTERPRET expr -- parse and execute a string as REXX code
- [x] Access to current variable environment from interpreted code
- [x] Correct scoping (interpreted code runs in caller's scope)
- [x] Recursion depth limit (100 levels)

Milestone: dynamic REXX programs, code generation patterns.

## Phase 8 -- ADDRESS and External Commands

Goal: host command environment.

- [x] ADDRESS instruction (set default, temporary, VALUE)
- [x] Shell-out to system commands
- [x] RC capture from external commands
- [x] SYSTEM/CMD/SH environment routing
- [x] Correct quoting and I/O for command strings

Milestone: REXX as a scripting/glue language -- its original purpose.

## Phase 9 -- TRACE (Interactive Debugging)

Goal: REXX's built-in tracing -- one of its best features.

- [x] TRACE OFF, TRACE NORMAL, TRACE RESULTS, TRACE INTERMEDIATES, TRACE ALL
- [x] TRACE COMMANDS, TRACE LABELS, TRACE ERRORS, TRACE FAILURE
- [x] TRACE ?prefix (interactive mode toggle)
- [x] Display clause source, results, intermediate values
- [x] Correct numbering and formatting per ANSI spec
- [x] Output tags: >V> (variable), >O> (operator), >L> (literal), >P> (prefix), >F> (function), >C> (compound)
- [ ] Full interactive debugging (pause and accept input at trace points)

Milestone: interactive debugging of REXX programs from within the interpreter.

## Phase 10 -- Language Server (LSP)

Goal: full editor integration via Language Server Protocol.

Uses tower-lsp (same stack as the Seq LSP in patch-seq). Enabled via `--features lsp`.

- [x] Diagnostics -- parse errors with source locations
- [x] Completion -- keywords, built-in functions, variables in scope, stem names
- [x] Hover -- keyword syntax, BIF signatures and descriptions
- [x] Go to Definition -- jump to labels, internal subroutine definitions
- [x] Document Symbols -- outline of labels, subroutines, PROCEDURE blocks
- [x] Code Actions -- "Did you mean?" suggestions (Levenshtein distance)
- [x] Inlay Hints -- PARSE variable annotations, NUMERIC precision info
- [ ] Neovim plugin (following patch-seq.nvim pattern)

Milestone: `patch-rexx-lsp` binary, Neovim plugin.

## Phase 11 -- External Function Libraries

Goal: extensibility.

- [x] REXX function packages (load external .rexx/.rex files)
- [x] Search order: internal -> built-in -> external
- [x] REXXPATH environment variable search path

Milestone: multi-file REXX programs.

## Phase 12 -- Polish and Distribution

- [x] NUMERIC DIGITS/FORM/FUZZ runtime control
- [x] PUSH/QUEUE/PULL data queue operations
- [x] QUEUED() BIF
- [x] Line continuation (trailing comma)
- [x] ANSI X3.274-1996 conformance test suite (100+ tests)
- [ ] Static binary builds (musl on Linux, native on macOS)
- [ ] Cross-compilation targets (Linux amd64/arm64, macOS, Windows)
- [ ] WASM target (rexx playground in browser)
- [ ] Performance: profile and optimize hot paths (arithmetic, string ops, PARSE)
- [ ] Consider RexxLA test suite for validation

## Phase 13 -- Examples Directory

Goal: a rich `examples/` directory showcasing REXX's strengths and idioms.

- [x] Classic demos: hello world, FizzBuzz, factorial, Fibonacci
- [x] String processing: PARSE templates, word manipulation, transliteration
- [x] Numeric: arbitrary-precision arithmetic, FORMAT, conversions
- [x] Control flow: DO loop variants, SELECT, SIGNAL ON error handling
- [x] Subroutines: PROCEDURE EXPOSE, recursive routines, function-style calls
- [x] INTERPRET: dynamic code generation, calculator
- [x] ADDRESS: shell scripting, command execution
- [x] Data queues: PUSH/QUEUE/PULL, stem arrays
- [x] Each example includes a header comment explaining what it demonstrates
- [x] Real-world scripts: log parser, CSV processor, file renamer
- [x] TRACE: annotated examples showing each trace setting

Milestone: newcomers can learn REXX by reading and running the examples directory.

## Phase 14 -- rexxlings (separate repo)

Goal: an interactive REXX course modeled after [rustlings](https://github.com/rust-lang/rustlings).

New repository: `rexxlings`

- [ ] CLI runner (`rexxlings` binary) that watches exercise files and re-runs on save
- [ ] Progressive exercises organized by topic (strings, control flow, PARSE, BIFs, etc.)
- [ ] Each exercise is a `.rex` file with a failing test or `TODO` comment to fix
- [ ] Hint system: `rexxlings hint <exercise>` shows a nudge without the answer
- [ ] Progress tracking: completed exercises marked, overall progress displayed
- [ ] Exercises cover REXX-specific concepts not found in other languages (PARSE templates, stem variables, INTERPRET, SIGNAL ON, TRACE)
- [ ] Introductory exercises assume no REXX background
- [ ] Advanced exercises cover idiomatic patterns and real-world scripting
- [ ] Uses `rexx` binary from patch-rexx as the interpreter

Milestone: `rexxlings` is a standalone learning tool that teaches REXX interactively.

## Design Principles

1. **Correct first.** Match ANSI REXX behavior exactly. When the spec is ambiguous, follow IBM's z/OS REXX or Regina behavior.
2. **Everything is a string.** Never optimize away the string representation. A REXX value is always inspectable as its string form.
3. **Single binary.** No shared libraries, no runtime dependencies, no installer. Copy it, run it.
4. **Good errors.** Modern diagnostics with source locations, not cryptic "Error 41.1" with no context.
5. **INTERPRET is not a hack.** It's a first-class feature. The architecture assumes it exists.
