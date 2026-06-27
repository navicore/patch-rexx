# API Docs (Rustdoc)

The rendered rustdoc for the **latest published crate** lives on docs.rs, which
builds the API reference straight from `src/`:

➡️ **[docs.rs/patch-rexx](https://docs.rs/patch-rexx)**

This book covers the *language* — how to write REXX, the value model, PARSE,
SIGNAL, TRACE, the LSP. The rustdoc covers the *implementation*: the public
types behind the embedding API (`Evaluator`, `Environment`, `ExecSignal`),
the trait/struct layout of the interpreter, and per-module internals.

> **Why two sites?** crates.io surfaces a single Documentation link, so the
> book (the thing most readers need) is primary there. The rustdoc is one
> click away via the link above and stays in sync with the latest release
> automatically — no version drift to manage.
