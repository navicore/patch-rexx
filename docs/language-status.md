# Language Status

Current implementation status against the ANSI X3.274-1996 REXX standard.

## Implemented

| Feature | Notes |
|---------|-------|
| Expressions | Full operator precedence, all arithmetic/comparison/logical/string operators |
| SAY | Output with expression evaluation |
| Assignment | Simple and compound (stem) variables |
| IF/THEN/ELSE | Conditional execution |
| DO/END | Simple, FOREVER, counted, WHILE, UNTIL, controlled (TO/BY/FOR) |
| SELECT/WHEN/OTHERWISE | Multi-way branching |
| LEAVE / ITERATE | Loop control with optional name targeting |
| NOP | No operation |
| CALL / RETURN | Internal subroutines, label-based dispatch |
| PROCEDURE / EXPOSE | Variable scope isolation and selective exposure |
| EXIT | Program termination with optional return code |
| DROP | Unassign variables |
| PARSE | All sources (ARG, PULL, VAR, VALUE, SOURCE, VERSION, LINEIN), all template types |
| SIGNAL | Label transfer, VALUE, ON/OFF for all 7 conditions |
| INTERPRET | Dynamic code evaluation with recursion limit |
| ADDRESS | Command environment routing (default, temporary, VALUE) |
| TRACE | All levels (OFF, N, R, I, C, L, A), output tags, TRACE() BIF |
| NUMERIC | DIGITS, FORM, FUZZ runtime control |
| PUSH / QUEUE / PULL | External data queue (LIFO/FIFO) |
| External functions | .rexx/.rex resolution, REXXPATH search |
| Built-in functions | 53 of ~70 ANSI BIFs |
| LSP | Diagnostics, completion, hover, definition, symbols, actions, hints |

## Not yet implemented

| Feature | Phase |
|---------|-------|
| CENTER/CENTRE BIF | 5 |
| BITAND, BITOR, BITXOR BIFs | 5 |
| ERRORTEXT, SOURCELINE, SYMBOL, VALUE BIFs | 5 |
| Full interactive TRACE (pause + accept input) | 9 |
| Neovim plugin | 10 |
| Static musl builds | 12 |
| Cross-compilation (Linux arm64, Windows) | 12 |
| WASM target | 12 |
| RexxLA test suite integration | 12 |

See [ROADMAP.md](https://github.com/navicore/patch-rexx/blob/main/ROADMAP.md) for the full plan.
