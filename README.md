[![CI - Linux](https://github.com/navicore/patch-rexx/actions/workflows/ci-linux.yml/badge.svg)](https://github.com/navicore/patch-rexx/actions/workflows/ci-linux.yml)
[![CI - macOS](https://github.com/navicore/patch-rexx/actions/workflows/ci-macos.yml/badge.svg)](https://github.com/navicore/patch-rexx/actions/workflows/ci-macos.yml)

# patch-rexx

A modern REXX interpreter in Rust. Single static binary. Correct per ANSI X3.274-1996.

```rexx
/* Classic REXX */
say "What is your name?"
pull name
say "Hello," name || "! The time is" time()

do i = 1 to 5
  if i // 2 = 0 then say i "is even"
  else say i "is odd"
end
```

---

## Why Another REXX?

REXX was designed by Mike Cowlishaw at IBM as the ultimate scripting language: everything is a string, INTERPRET evaluates code at runtime, PARSE does pattern matching that nothing else has matched since, and TRACE lets you debug interactively from inside the language. It was the shell of CMS, the scripting engine of OS/2, and the automation language of z/OS.

The existing open-source implementations (Regina, Brexx) are aging C codebases from the 1990s. patch-rexx aims to be:

- **Correct** -- targeting full ANSI X3.274-1996 conformance
- **Single binary** -- no shared libraries, no installer, just copy and run
- **Modern diagnostics** -- source locations, caret pointing, helpful messages instead of cryptic "Error 41.1"
- **Memory safe** -- written in Rust, no buffer overflows, no use-after-free

---

## Status

Early development. The foundation is in place:

| Module | Status |
|--------|--------|
| Lexer | Complete -- all REXX tokens, nested comments, hex/binary strings |
| AST | Complete -- all clause types, DO variants, PARSE templates, expressions |
| Variable environments | Complete -- PROCEDURE/EXPOSE scoping, stem variables, DROP |
| Value system | Complete -- everything-is-a-string with BigDecimal arithmetic backing |
| Error system | Complete -- all ANSI REXX error numbers with source-location diagnostics |
| CLI | Working -- file execution, `-e` eval, interactive REPL |
| Parser | Not yet started |
| Evaluator | Not yet started |

The roadmap includes a full Language Server (LSP) for editor integration -- diagnostics, completion, hover, go-to-definition -- using the same tower-lsp stack as [patch-seq](https://github.com/navicore/patch-seq).

See [ROADMAP.md](ROADMAP.md) for the full 12-phase development plan.

---

## Building

Requires Rust 1.93.0+.

```bash
cargo build --release
```

The release binary is statically linked with LTO and symbol stripping for minimal size.

## Usage

```bash
# Run a REXX program
patch-rexx hello.rex

# Evaluate an expression
patch-rexx -e 'say 2 + 3'

# Interactive REPL
patch-rexx
```

---

## Design Decisions

**Everything is a string.** REXX values are always strings. Numbers are strings that happen to be valid numeric representations. patch-rexx never optimizes away the string form -- a value is always inspectable as text. Arithmetic uses arbitrary-precision decimals (NUMERIC DIGITS) underneath.

**INTERPRET is first-class.** The architecture assumes `INTERPRET expr` exists. Since patch-rexx is an interpreter, this is trivial: parse the string, evaluate the AST in the current environment. No special compilation tricks, no restrictions.

**REXX scoping is opaque.** PROCEDURE creates a completely isolated variable scope. EXPOSE selectively copies variables into that scope. There is no scope chain lookup -- if a variable isn't in your scope, it returns its own uppercased name. This is correct REXX semantics and enforced by the environment implementation.

---

## Related Projects

- [patch-seq](https://github.com/navicore/patch-seq) -- Seq, a concatenative language that compiles to native code via LLVM
- [seq-lisp](https://github.com/navicore/seq-lisp) -- A Lisp interpreter written in Seq

---

## License

MIT
