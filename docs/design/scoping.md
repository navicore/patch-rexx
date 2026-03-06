# Scoping and Environments

## How REXX scoping works

REXX has no lexical scoping. By default, all variables are global within a program. The `PROCEDURE` instruction creates an opaque scope — the subroutine cannot see any caller variables unless explicitly listed in an `EXPOSE` clause.

An uninitialized variable evaluates to its own name in uppercase. This is correct REXX behavior, not an error (unless SIGNAL ON NOVALUE is active).

## Environment implementation

`src/env.rs` implements variable scoping with a stack of frames.

### Simple variables

- `get(name)` — returns the value, or the uppercased name if unset
- `set(name, value)` — assigns a value
- `is_set(name)` — checks if a variable has been assigned

### Stem (compound) variables

Stems are REXX's array-like structure. `arr.1`, `arr.2`, `arr.foo` are compound variables with stem `arr.`.

- `set("arr.", default)` — sets the default for all unset compounds under this stem
- `set_compound("arr.", "1", value)` — sets a specific tail
- `get_compound("arr.", "1")` — returns the value, or the stem default, or the uppercased compound name

Tail values are themselves derived variables — `arr.i` where `i = 3` accesses `arr.3`.

### PROCEDURE / EXPOSE

- `push_procedure()` — creates a completely empty scope. No caller variables are visible.
- `push_procedure_expose(names)` — creates a new scope, but copies the listed variables from the caller. On `pop_procedure()`, exposed variables are written back to the caller's scope.

### ADDRESS tracking

The environment also tracks the current and previous command environment names (for `ADDRESS` swapping). The default environment is `"SYSTEM"`.

### Condition information

When a condition trap fires, the environment stores condition metadata (type, description, extra info) for the `CONDITION()` BIF to query.

### EnvVars

`EnvVars` is a restricted wrapper around the environment, exposed to custom command handlers. It provides read/write access to variables but not to ADDRESS or PROCEDURE operations.
