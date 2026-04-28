# PARSE Engine

PARSE is REXX's signature feature — a template-driven string decomposition system that has no direct equivalent in other languages.

## PARSE sources

| Source | Instruction | Data |
|--------|------------|------|
| ARG | `PARSE ARG` | Subroutine arguments |
| PULL | `PARSE PULL` | External data queue or stdin |
| VAR | `PARSE VAR name` | Contents of a variable |
| VALUE | `PARSE VALUE expr WITH` | Result of an expression |
| SOURCE | `PARSE SOURCE` | Program metadata (OS, invocation type, filename) |
| VERSION | `PARSE VERSION` | Interpreter name and version |
| LINEIN | `PARSE LINEIN` | One line from stdin |

`PARSE UPPER` variants uppercase the source before applying the template.

## Template elements

Templates are sequences of:

- **Variable names** — receive the next extracted piece
- **Dot (`.`)** — placeholder, discards the piece
- **Literal strings** (`'/'`, `"="`) — split the source at the next occurrence of this string
- **Absolute positions** (`5`, `1`) — move the scan cursor to this column (1-based)
- **Relative positions** (`+3`, `-2`) — move the cursor forward or backward
- **Variable patterns** (`(name)`) — use the value of a variable as a literal pattern
- **Comma** — separates templates for multiple arguments

## Word splitting

When consecutive variables appear with no pattern between them, REXX splits by words (whitespace-delimited). The last variable in a word-split group receives everything remaining.

```rexx
parse var line first second rest
/* "hello world foo bar" → first="hello", second="world", rest="foo bar" */
```

## Literal pattern matching

A literal pattern scans forward from the current position for the first occurrence of the string. The text before the match is assigned; the cursor advances past the match.

```rexx
parse var date day '/' month '/' year
/* "25/12/2024" → day="25", month="12", year="2024" */
```

If the pattern is not found, the remaining text goes to variables before the pattern, and subsequent variables get empty strings.

## Positional patterns

Absolute positions set the cursor to a column. Relative positions move it forward or backward. Backward movement allows re-scanning already-parsed portions.

```rexx
parse var record 1 name 20 dept 30 salary
/* Fixed-width field extraction */
```

## Implementation

The PARSE engine is in `eval.rs` (`exec_parse()`). It processes template elements left-to-right, maintaining a cursor position in the source string. Column positions are 1-based and UTF-8 aware — positions index characters, not bytes.
