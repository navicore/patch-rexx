# patch-rexx Roadmap

A modern REXX interpreter in Rust. Single static binary. Correct per ANSI X3.274-1996.

## Current State

Foundation modules with passing tests:
- **value.rs** — RexxValue (everything-is-a-string), BigDecimal integration, NUMERIC DIGITS/FORM/FUZZ settings
- **error.rs** — All ANSI REXX error numbers (4-49) with source-location diagnostics
- **lexer.rs** — Full tokenizer: strings, numbers, symbols, all operators, nested `/* */` comments, hex/binary string literals
- **ast.rs** — Complete AST covering all REXX clause types, DO variants, PARSE templates, expressions
- **env.rs** — Variable environments with REXX scoping: PROCEDURE isolation, EXPOSE, stem/compound variables, DROP
- **main.rs** — CLI (file execution, `-e` eval, REPL with rustyline)

## Phase 1 — SAY and Expressions

Goal: `say 2 + 3` prints `5`.

- [ ] Parser: expression parsing with REXX operator precedence
- [ ] Parser: SAY instruction
- [ ] Parser: assignment (`x = expr`)
- [ ] Evaluator: arithmetic (+, -, *, /, %, //, **)
- [ ] Evaluator: string concatenation (abuttal, `||`, blank concatenation)
- [ ] Evaluator: comparison operators (normal and strict)
- [ ] Evaluator: logical operators (&, |, &&, \)
- [ ] Wire parser and evaluator into main.rs run_source
- [ ] REPL evaluates and displays results

Milestone: run classic REXX one-liners interactively.

## Phase 2 — Control Flow

Goal: IF/THEN/ELSE, DO loops, SELECT/WHEN.

- [ ] Parser: IF/THEN/ELSE
- [ ] Parser: DO/END (simple, FOREVER, counted, WHILE, UNTIL, controlled)
- [ ] Parser: SELECT/WHEN/OTHERWISE/END
- [ ] Parser: LEAVE, ITERATE (with optional name targeting)
- [ ] Parser: NOP
- [ ] Evaluator: all control flow
- [ ] Labels (for SIGNAL targets and CALL)

Milestone: FizzBuzz, factorial, standard interview programs.

## Phase 3 — Subroutines and Functions

Goal: CALL, RETURN, internal routines, PROCEDURE EXPOSE.

- [ ] Parser: CALL instruction
- [ ] Parser: function call syntax in expressions: `name(args)`
- [ ] Parser: PROCEDURE / PROCEDURE EXPOSE
- [ ] Parser: RETURN / EXIT
- [ ] Evaluator: internal subroutine calls (label-based)
- [ ] Evaluator: PROCEDURE scoping (already modeled in env.rs)
- [ ] Evaluator: function calls (must return a value)
- [ ] Evaluator: CALL (may or may not return via RESULT)
- [ ] ARG / PARSE ARG for parameter passing

Milestone: recursive programs, multi-routine scripts.

## Phase 4 — PARSE

Goal: full PARSE instruction — REXX's signature feature.

- [ ] PARSE ARG
- [ ] PARSE PULL
- [ ] PARSE VAR name
- [ ] PARSE VALUE expr WITH
- [ ] PARSE SOURCE
- [ ] PARSE VERSION
- [ ] PARSE LINEIN
- [ ] PARSE UPPER variants
- [ ] Template patterns: literal string matching
- [ ] Template patterns: absolute column positions
- [ ] Template patterns: relative column positions (+n, -n)
- [ ] Template patterns: variable patterns
- [ ] Template patterns: dot placeholder (discard)
- [ ] Multiple template parsing (comma-separated argument strings)

Milestone: real REXX string processing — parsing input, extracting fields.

## Phase 5 — Built-in Functions (~70 BIFs)

Goal: ANSI REXX built-in function library.

### String
- [ ] ABBREV, COPIES, CENTER/CENTRE, COMPARE
- [ ] DELSTR, DELWORD, INSERT, LASTPOS, LEFT, LENGTH
- [ ] OVERLAY, POS, REVERSE, RIGHT, STRIP
- [ ] SUBSTR, SUBWORD, TRANSLATE, VERIFY, WORD
- [ ] WORDINDEX, WORDLENGTH, WORDPOS, WORDS

### Numeric
- [ ] ABS, FORMAT, MAX, MIN, SIGN, TRUNC
- [ ] RANDOM

### Conversion
- [ ] B2X, C2D, C2X, D2C, D2X, X2B, X2C, X2D
- [ ] DATATYPE

### Bit
- [ ] BITAND, BITOR, BITXOR

### Misc
- [ ] ADDRESS, ARG, CONDITION, DATE, DIGITS
- [ ] ERRORTEXT, FORM, FUZZ, QUEUED, SOURCELINE
- [ ] SYMBOL, TIME, TRACE, VALUE, XRANGE

Milestone: REXX programs that rely on the standard function library work.

## Phase 6 — SIGNAL and Conditions

Goal: error trapping and SIGNAL flow control.

- [ ] SIGNAL label (unconditional transfer)
- [ ] SIGNAL VALUE expr
- [ ] SIGNAL ON condition NAME label
- [ ] SIGNAL OFF condition
- [ ] Conditions: ERROR, FAILURE, HALT, NOVALUE, NOTREADY, SYNTAX, LOSTDIGITS
- [ ] Condition information: CONDITION('C'), CONDITION('D'), etc.
- [ ] RC (return code) special variable

Milestone: robust error handling in REXX programs.

## Phase 7 — INTERPRET

Goal: dynamic code evaluation.

- [ ] INTERPRET expr — parse and execute a string as REXX code
- [ ] Access to current variable environment from interpreted code
- [ ] Correct scoping (interpreted code runs in caller's scope)

This is trivial in an interpreter — the parser and evaluator already exist.
INTERPRET just feeds a string through the same pipeline.

Milestone: dynamic REXX programs, code generation patterns.

## Phase 8 — ADDRESS and External Commands

Goal: host command environment.

- [ ] ADDRESS instruction (set default, temporary, VALUE)
- [ ] Shell-out to system commands
- [ ] RC capture from external commands
- [ ] SYSTEM/CMD/SH environment routing
- [ ] Correct quoting and I/O for command strings

Milestone: REXX as a scripting/glue language — its original purpose.

## Phase 9 — TRACE (Interactive Debugging)

Goal: REXX's built-in tracing — one of its best features.

- [ ] TRACE OFF, TRACE NORMAL, TRACE RESULTS, TRACE INTERMEDIATES, TRACE ALL
- [ ] TRACE ?prefix (interactive mode — pause and accept input)
- [ ] Display clause source, results, intermediate values
- [ ] Correct numbering and formatting per ANSI spec

Milestone: interactive debugging of REXX programs from within the interpreter.

## Phase 10 — External Function Libraries

Goal: extensibility.

- [ ] REXX function packages (load external .rexx files)
- [ ] Search order: internal → built-in → external
- [ ] REXXPATH or similar search path mechanism

## Phase 11 — Polish and Distribution

- [ ] Static binary builds (musl on Linux, native on macOS)
- [ ] Cross-compilation targets (Linux amd64/arm64, macOS, Windows)
- [ ] WASM target (rexx playground in browser)
- [ ] ANSI X3.274-1996 conformance test suite
- [ ] Performance: profile and optimize hot paths (arithmetic, string ops, PARSE)
- [ ] Consider RexxLA test suite for validation

## Design Principles

1. **Correct first.** Match ANSI REXX behavior exactly. When the spec is ambiguous, follow IBM's z/OS REXX or Regina behavior.
2. **Everything is a string.** Never optimize away the string representation. A REXX value is always inspectable as its string form.
3. **Single binary.** No shared libraries, no runtime dependencies, no installer. Copy it, run it.
4. **Good errors.** Modern diagnostics with source locations, not cryptic "Error 41.1" with no context.
5. **INTERPRET is not a hack.** It's a first-class feature. The architecture assumes it exists.
