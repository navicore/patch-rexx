# SIGNAL and Conditions

## SIGNAL as control flow

`SIGNAL label` is an unconditional jump — similar to `goto`. It transfers control to a label anywhere in the program. Unlike CALL, it does not return. SIGNAL exits all active DO loops and SELECT blocks.

`SIGNAL VALUE expr` computes the label name dynamically at runtime.

## Condition traps

REXX defines 7 conditions that can be trapped:

| Condition | Fires when |
|-----------|-----------|
| ERROR | A command returns a non-zero RC |
| FAILURE | A command cannot be executed (not found) |
| HALT | External interrupt requested |
| NOVALUE | An uninitialized variable is used |
| NOTREADY | I/O operation fails |
| SYNTAX | A runtime error occurs (division by zero, bad arithmetic, etc.) |
| LOSTDIGITS | Arithmetic loses significant digits |

### Enabling traps

```rexx
signal on syntax name error_handler
signal on novalue
```

When no `NAME` is specified, the handler label defaults to the condition name (e.g., `SYNTAX:`).

### Auto-disable

A trap fires **once** and then automatically disables itself. To handle the same condition again, the handler must re-enable the trap with another `SIGNAL ON` instruction.

### Trap state

When a trap fires:
- Control transfers to the handler label via SIGNAL (exiting all loops)
- The `RC` variable is set to the REXX error number (for SYNTAX) or command return code (for ERROR/FAILURE)
- Condition metadata is stored for the `CONDITION()` BIF

### CONDITION() BIF

Queries information about the most recent trapped condition:

| Option | Returns |
|--------|---------|
| `CONDITION('C')` | Condition name (e.g., "SYNTAX") |
| `CONDITION('D')` | Description text |
| `CONDITION('I')` | Instruction that caused the condition |
| `CONDITION('S')` | Status ("SIGNAL" or "CALL") |

## Implementation

The evaluator tracks active traps in a `HashMap<Condition, String>` mapping conditions to handler label names. When a trappable error occurs, the evaluator checks for an active trap before raising the error. If a trap is found, it auto-disables the trap, stores condition info, and redirects execution via the same SIGNAL mechanism.

Label resolution uses a pre-built index (`HashMap<String, usize>`) mapping uppercased label names to clause indices, built during the first pass of `exec()`.
