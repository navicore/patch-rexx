# Design: validate `END` name against DO control variable

Working doc. Tracks the fix for PR #27 reviewer finding #2.

## Intent

Per ANSI X3.274-1996 §6.6.6 and Error 21 ("Invalid data on end of clause"), an `END` clause that carries a name must match the control variable of its enclosing `DO`. patch-rexx currently consumes the name and discards it without comparison, so `do i = 1 to 5; … end j` parses without complaint. We want a parse-time error.

## Constraints

- Public parser API unchanged (`Parser::parse` still returns `RexxResult<Program>`).
- Bare `END` (no name) stays accepted.
- Case-insensitive match (REXX semantics; `END i` and `END I` both valid for `do i`).
- Don't regress existing tests; add positive coverage for the new rejection paths.
- Out of scope: rejecting non-symbol garbage after `END` (e.g., `end 5`, `end "x"`); the dead `!self.is_terminator()` guard is in #3, not here.
- `RexxError::InvalidDataOnEnd` (variant for ANSI #21) already exists in `error.rs` — no new variant needed.

## Approach

The loop's control variable lives in `DoBlock::name`: `Some(var)` for controlled `DO i = …`, `None` for Simple / Forever / While / Until / Count. `parse_do_body` runs before the `DoBlock` is built, but `finish_do` already receives the name as a parameter and forwards it. Thread it one level deeper:

```rust
fn finish_do(&mut self, loc, kind, name: Option<String>) -> RexxResult<Clause> {
    self.skip_terminators();
    let body = self.parse_do_body(name.as_deref())?;   // ← pass name
    …
}

fn parse_do_body(&mut self, loop_name: Option<&str>) -> RexxResult<Vec<Clause>> {
    …
    if self.is_keyword("END") {
        self.advance();
        if let TokenKind::Symbol(s) = self.peek_kind().clone() {
            match loop_name {
                Some(loop_n) if loop_n.eq_ignore_ascii_case(&s) => self.advance(),
                Some(loop_n) => return Err(InvalidDataOnEnd … "got '{s}', expected '{loop_n}'"),
                None => return Err(InvalidDataOnEnd … "this DO has no control variable"),
            }
        }
        break;
    }
    …
}
```

Single call site to update: `finish_do`. Mechanical fan-out.

## Domain events

Produced (at parse time, never at runtime):
- `RexxError::InvalidDataOnEnd` (#21) when the END name disagrees with the loop variable, or when END names anything on a non-controlled DO.

Consumed: none. Loop variable name is already in `finish_do`'s parameter.

Triggers: parse-time only — no AST shape change, no evaluator change.

## Checkpoints

Add tests in `src/parser.rs`'s `mod tests` (and/or `tests/conformance.rs` if there's an existing END-name section):

1. `do i = 1 to 5; nop; end i` — ✓ accepts (regression check).
2. `do i = 1 to 5; nop; end I` — ✓ accepts (case-insensitive).
3. `do i = 1 to 5; nop; end j` — ✗ Error 21.
4. `do; nop; end` — ✓ accepts.
5. `do; nop; end x` — ✗ Error 21 (no control variable).
6. `do 5; nop; end x` — ✗ Error 21 (Count loop).
7. `do forever; leave; end x` — ✗ Error 21.
8. `do while x; …; end` — ✓ accepts.

Plus: existing 500+ tests still pass; `just ci` green.

## Risk / unknowns

- Whether any existing example or test mistakenly uses `END name` with a mismatched name. If so, those need their END corrected (per the user's rule: don't weaken tests; fix the code being tested, not the test). Verified before merge by running `just test`.
- Behavior of `end <var>` where `<var>` is the same uppercase form but the original case differed in DO (e.g., `do i = 1; end I`) — REXX is case-insensitive, both forms must work. The `eq_ignore_ascii_case` check covers this.
