# Value Model

## Everything is a string

Every REXX value is stored as a `String`. There is no separate integer, float, or boolean type. A value like `"42"` is simultaneously the string `"42"` and the number forty-two. Arithmetic is performed by parsing the string into a `BigDecimal`, computing the result, and formatting back to a string.

This is not an optimization — it is the language specification. The string form is always the canonical representation.

## RexxValue

`src/value.rs` defines the core value type and numeric formatting.

Key operations:
- `to_decimal()` — parse a string into `BigDecimal` for arithmetic
- `is_number()` — check if a string is a valid REXX number
- `is_whole_number()` — check if a string represents an integer
- Numeric formatting — convert `BigDecimal` back to a REXX-conformant string

## NUMERIC settings

Three runtime settings control arithmetic behavior:

| Setting | Default | Controls |
|---------|---------|----------|
| NUMERIC DIGITS | 9 | Precision — number of significant digits retained |
| NUMERIC FORM | SCIENTIFIC | Exponent notation style (SCIENTIFIC or ENGINEERING) |
| NUMERIC FUZZ | 0 | Digits ignored during comparison (must be < DIGITS) |

These are tracked per-evaluator in `NumericSettings` and can be changed at runtime via the `NUMERIC` instruction.

## Arithmetic pipeline

1. Both operands are parsed from string to `BigDecimal`
2. The operation is performed at arbitrary precision
3. The result is formatted back to a string, truncated to NUMERIC DIGITS
4. Scientific or engineering notation is applied when the exponent exceeds DIGITS thresholds

## Comparison semantics

REXX has two comparison modes:

- **Normal** (`=`, `<`, `>`, etc.) — if both operands are valid numbers, compare numerically. Otherwise compare as strings with leading/trailing blank stripping.
- **Strict** (`==`, `<<`, `>>`, etc.) — always compare as strings, no blank stripping, no numeric interpretation.

NUMERIC FUZZ affects normal comparisons by temporarily reducing precision, making "close enough" values compare as equal.
