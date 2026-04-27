[![CI - Linux](https://github.com/navicore/patch-rexx/actions/workflows/ci-linux.yml/badge.svg)](https://github.com/navicore/patch-rexx/actions/workflows/ci-linux.yml)
[![CI - macOS](https://github.com/navicore/patch-rexx/actions/workflows/ci-macos.yml/badge.svg)](https://github.com/navicore/patch-rexx/actions/workflows/ci-macos.yml)

# patch-rexx

A modern REXX interpreter in Rust. Single static binary. Correct per ANSI X3.274-1996.

**[Documentation](https://navicore.github.io/patch-rexx/)** -- quickstart, language status, BIF reference, and design docs.

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

The interpreter is functional end-to-end with 500+ tests covering the core language.

| Module | Status |
|--------|--------|
| Lexer | Complete -- all REXX tokens, nested comments, hex/binary strings |
| Parser | Complete -- all clause types, expression precedence, PARSE templates |
| Evaluator | Complete -- all control flow, PARSE, SIGNAL, INTERPRET, ADDRESS, TRACE |
| AST | Complete -- all clause types, DO variants, PARSE templates, expressions |
| Variable environments | Complete -- PROCEDURE/EXPOSE scoping, stem variables, DROP |
| Value system | Complete -- everything-is-a-string with BigDecimal arithmetic |
| Built-in functions | 53 of ~70 ANSI BIFs -- string, word, numeric, conversion, date/time |
| Error system | Complete -- all ANSI REXX error numbers with source-location diagnostics |
| External functions | Complete -- .rexx/.rex resolution, REXXPATH search |
| REPL | Complete -- vim-mode line editing, persistent history |
| CLI | Complete -- file execution, `-e` eval, interactive REPL, pipe mode |
| LSP | Complete -- diagnostics, completion, hover, go-to-definition, symbols, code actions, inlay hints |

See [ROADMAP.md](ROADMAP.md) for the phase-by-phase development plan and remaining work.

---

## Building

Requires Rust edition 2024 toolchain. The exact compiler version is pinned in `rust-toolchain.toml`; `rustup` will pick it up automatically when you `cd` into the repo.

```bash
cargo build --release
```

The release binary is statically linked with LTO and symbol stripping for minimal size.

To build with Language Server support:

```bash
cargo build --release --features lsp
```

## Development (`just`)

The `justfile` is the single source of truth for build, lint, and test. Both local dev and GitHub Actions call the same recipes — no drift.

```bash
just ci          # what CI runs: fmt-check, lint, test, build
just fmt         # format the workspace
just fmt-check   # verify formatting
just lint        # cargo clippy --locked --workspace --all-targets -- -D warnings
just test        # cargo test --locked --workspace --all-targets
just build       # cargo build --locked --release
just install     # cargo install --path . --locked --force  (installs `rexx` into ~/.cargo/bin)
just clean       # cargo clean
```

Run `just ci` before pushing — it runs exactly what CI runs, so a green local run means a green CI run.

## Usage

```bash
# Run a REXX program
rexx hello.rex

# Evaluate an expression
rexx -e 'say 2 + 3'

# Interactive REPL (vim-mode editing)
rexx

# Pipe mode
echo 'say "hello"' | rexx
```

The REPL supports vim key bindings (insert/normal mode), persistent history at `~/.rexx_history`, and arrow-key navigation.

---

## Examples

The `examples/` directory includes annotated programs covering:

- **Basics** -- hello world, FizzBuzz, factorial, Fibonacci
- **String processing** -- PARSE templates, word functions, transliteration
- **Numeric** -- arbitrary-precision arithmetic, conversions (D2X, C2D, etc.)
- **Control flow** -- DO loop variants, SELECT/WHEN, SIGNAL ON error handling
- **Subroutines** -- PROCEDURE EXPOSE, recursive routines
- **INTERPRET** -- dynamic code generation, calculator
- **ADDRESS** -- shell scripting, command execution
- **Data queues** -- PUSH/QUEUE/PULL, stem arrays

```bash
rexx examples/fizzbuzz.rexx
rexx examples/parse_demo.rexx
```

---

## Design Decisions

**Everything is a string.** REXX values are always strings. Numbers are strings that happen to be valid numeric representations. patch-rexx never optimizes away the string form -- a value is always inspectable as text. Arithmetic uses arbitrary-precision decimals (NUMERIC DIGITS) underneath.

**INTERPRET is first-class.** The architecture assumes `INTERPRET expr` exists. Since patch-rexx is an interpreter, this is trivial: parse the string, evaluate the AST in the current environment. No special compilation tricks, no restrictions.

**REXX scoping is opaque.** PROCEDURE creates a completely isolated variable scope. EXPOSE selectively copies variables into that scope. There is no scope chain lookup -- if a variable isn't in your scope, it returns its own uppercased name. This is correct REXX semantics and enforced by the environment implementation.

---

## Releasing

Releases are automated by `.github/workflows/release.yml`. To cut a release:

1. Create a GitHub release with a tag of the form `vX.Y.Z` (e.g. `v0.9.5`).
2. The workflow then:
   - bumps `[package].version` in `Cargo.toml` to match the tag (using a section-scoped `awk` so dependency `version = "..."` lines aren't touched),
   - runs `cargo generate-lockfile`,
   - commits the bump as `chore: bump version to X.Y.Z` (as `github-actions[bot]`) and pushes to `main`,
   - verifies `Cargo.toml` matches the tag,
   - publishes via `cargo publish --no-verify` (CI on Linux PRs and macOS main has already verified the build).

Required repo secrets (Settings → Secrets and variables → Actions):

- `PAT` — personal access token with `contents: write`, used to push the version bump back to `main`.
- `CRATES_IO_TOKEN` — API token from <https://crates.io/settings/tokens>.

A failed `cargo publish` (e.g. version already on crates.io) is downgraded to a workflow warning, not a hard failure.

---

## Related Projects

- [patch-seq](https://github.com/navicore/patch-seq) -- Seq, a concatenative language that compiles to native code via LLVM
- [seq-lisp](https://github.com/navicore/seq-lisp) -- A Lisp interpreter written in Seq

---

## License

MIT
