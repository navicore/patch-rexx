# Quickstart

## Install

Requires a Rust edition 2024 toolchain.

```bash
cargo build --release
```

The `rexx` binary is at `target/release/rexx`. Copy it anywhere on your `PATH`.

## Run a program

```bash
rexx examples/hello.rexx
```

## Evaluate an expression

```bash
rexx -e 'say 2 + 3'
```

## Interactive REPL

```bash
rexx
```

The REPL uses vim-mode editing. You start in INSERT mode; press `Esc` for NORMAL mode. History is saved to `~/.rexx_history`.

| Key | Mode | Action |
|-----|------|--------|
| `Enter` | either | Execute line |
| `Esc` | INSERT | Switch to NORMAL |
| `i` / `a` | NORMAL | Switch to INSERT |
| `k` / `j` | NORMAL | Previous / next history |
| Up / Down | either | Previous / next history |
| Ctrl-D | either | Exit |

## Pipe mode

```bash
echo 'say "hello from a pipe"' | rexx
```

## Language Server

Build with LSP support for editor integration:

```bash
cargo build --release --features lsp
```

This produces the `rexx-lsp` binary alongside `rexx`.
