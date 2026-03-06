# Error System

## ANSI error numbers

REXX defines numbered error conditions in the ANSI X3.274-1996 specification. patch-rexx maps all implemented errors to these numbers:

| Number | Condition | Example |
|--------|-----------|---------|
| 4 | HALT | External interrupt |
| 5 | Resource exhausted | INTERPRET recursion limit exceeded |
| 6 | Unmatched comment | `/* ... ` with no closing `*/` |
| 7 | WHEN/OTHERWISE expected | SELECT without WHEN |
| 8 | Unexpected THEN/ELSE | THEN without IF |
| 9 | Unexpected WHEN/OTHERWISE | WHEN outside SELECT |
| 10 | Unexpected END | END without matching DO/SELECT |
| 13 | Invalid character | Non-REXX character in source |
| 14 | Incomplete block | DO/SELECT without END |
| 15 | Invalid hex/binary string | Malformed `'...'x` or `'...'b` |
| 16 | Label not found | SIGNAL to nonexistent label |
| 17 | Unexpected PROCEDURE | PROCEDURE not immediately after label |
| 19 | Expected string or symbol | Missing operand |
| 20 | Expected symbol | Missing variable name |
| 21 | Invalid data on END | Wrong name on END clause |
| 24 | Invalid TRACE | Bad TRACE setting |
| 25 | Invalid sub-keyword | Bad keyword in context (e.g., bad SIGNAL condition) |
| 26 | Invalid whole number | Non-integer where integer required |
| 27 | Invalid DO syntax | Malformed DO clause |
| 28 | Invalid LEAVE/ITERATE | LEAVE/ITERATE outside loop or targeting wrong loop |
| 40 | Incorrect call | Wrong number or type of arguments to BIF |
| 41 | Bad arithmetic | Non-numeric operand in arithmetic |
| 42 | Arithmetic overflow | Number too large to represent |
| 43 | Routine not found | No internal, built-in, or external routine |
| 44 | No return data | Function call but routine didn't RETURN a value |

## RexxDiagnostic

Every error is wrapped in a `RexxDiagnostic` that carries:

- The error variant (enum)
- A `SourceLoc` (line number, column)
- A human-readable detail message

This gives modern diagnostic output:

```
Error 41.1 at line 5, col 12: Bad arithmetic conversion
  "abc" is not a valid number
```

## Error flow

1. An error condition is detected (evaluator, parser, or lexer)
2. A `RexxDiagnostic` is created with the source location
3. If a condition trap is active (e.g., SIGNAL ON SYNTAX), the trap fires instead of propagating the error
4. Otherwise, the error propagates up as a `Result::Err` and is reported to the user

The LSP module reuses `RexxDiagnostic` — it converts them to LSP Diagnostic objects for editor display.
