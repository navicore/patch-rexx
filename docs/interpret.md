# INTERPRET

## What it does

`INTERPRET expr` evaluates a string as REXX source code at runtime. The string goes through the full pipeline (lex → parse → evaluate) in the current variable environment.

```rexx
code = "say 'hello from INTERPRET'"
interpret code
```

## Scope sharing

Interpreted code runs in the caller's variable scope. It can read, modify, and create variables. It can also execute control flow:

- **EXIT** propagates out and terminates the program
- **LEAVE** and **ITERATE** propagate out to enclosing DO loops
- **SIGNAL** can transfer to labels in the outer program

## Recursion limit

INTERPRET can invoke INTERPRET. To prevent runaway recursion, the evaluator enforces a depth limit of 100 nested INTERPRET calls (`MAX_INTERPRET_DEPTH`). Exceeding this raises Error 5 (resource exhausted).

## Condition trap inheritance

Interpreted code inherits the caller's active condition traps. It can also set new traps that persist after INTERPRET returns.

## Implementation

`exec_interpret()` in `eval.rs`:
1. Evaluates the expression to get a string
2. Lexes and parses the string (errors fire SYNTAX trap if active)
3. Merges any labels from the interpreted code into the label map
4. Executes the parsed clauses in the current evaluator context
5. Propagates EXIT, LEAVE, ITERATE, and SIGNAL back to the caller
