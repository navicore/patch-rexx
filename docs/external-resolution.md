# External Function Resolution

When a CALL or function invocation cannot be resolved as an internal label or built-in function, the evaluator searches for an external `.rexx` or `.rex` file.

## Search order

1. **Internal labels** — label definitions in the current program
2. **Built-in functions** — the 53 implemented BIFs
3. **External files** — filesystem search via `src/external.rs`

## File search

`resolve_external(name, source_dir)` looks for:

- `name.rexx`, `name.rex` (lowercase)
- `NAME.rexx`, `NAME.rex` (uppercase, if different from lowercase)

In these directories, in order:

1. **Source directory** — the directory containing the calling script
2. **REXXPATH entries** — colon-separated paths from the `REXXPATH` environment variable
3. **Current working directory**

## Scope isolation

External functions execute in a fresh variable environment. They cannot see the caller's variables. Arguments are passed via ARG/PARSE ARG. Return values come back via RETURN.

This matches standard REXX semantics — external routines are isolated, unlike internal subroutines which share the global scope (unless PROCEDURE is used).

## Error handling

- If an external file is found but contains parse errors, the error propagates to the caller (and can be caught by SIGNAL ON SYNTAX)
- If no file is found, Error 43 (routine not found) is raised
- If an external function is used in an expression but doesn't RETURN a value, Error 44 (no return data) is raised
- The source path is saved and restored around external calls so that PARSE SOURCE reports correctly
