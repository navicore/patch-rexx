# TRACE

REXX has built-in execution tracing — a debugging facility designed into the language itself.

## Trace levels

| Setting | Letter | Shows |
|---------|--------|-------|
| OFF | O | Nothing |
| NORMAL | N | Nothing (default) |
| FAILURE | F | Commands that fail (RC < 0) |
| ERRORS | E | Commands with non-zero RC |
| COMMANDS | C | All commands before execution |
| LABELS | L | Label clauses as they are reached |
| RESULTS | R | All clauses + expression results |
| INTERMEDIATES | I | All clauses + every sub-expression value |
| ALL | A | Everything including labels |

## Output format

Trace output uses a distinctive prefix system:

- `*-* ` followed by the source line — shows the clause being executed
- `>>>` — the final result of an expression (in RESULTS and above)
- `>V>` — a variable value lookup (INTERMEDIATES)
- `>L>` — a literal value (INTERMEDIATES)
- `>O>` — an operator result (INTERMEDIATES)
- `>P>` — a unary prefix result (INTERMEDIATES)
- `>F>` — a function call result (INTERMEDIATES)
- `>C>` — a compound variable lookup (INTERMEDIATES)

Line numbers are included to the left of `*-*`.

## Interactive mode

The `?` prefix toggles interactive tracing: `TRACE ?R` enables RESULTS with interactive pauses. In interactive mode, the interpreter pauses after each traced clause and accepts input. A second `?` toggle turns interactive mode off.

> **Note**: The `?` toggle is implemented, but full interactive debugging (pausing and accepting user input mid-trace) is not yet complete.

## TRACE() BIF

The `TRACE()` built-in function queries or sets the trace level:

- `TRACE()` — returns the current setting letter
- `TRACE(setting)` — sets a new level, returns the previous one
- `TRACE('?')` — toggles interactive mode flag

## Implementation

Tracing is handled in `exec_clause_outer()` which wraps every clause execution. Based on the current trace level, it emits the source line and, for RESULTS/INTERMEDIATES, captures expression evaluation output. The trace level is stored as an enum in the evaluator and can be changed mid-program via the TRACE instruction.
