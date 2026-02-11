/* factorial.rexx — Recursive Factorial
 *
 * Demonstrates: CALL, RETURN, PROCEDURE, ARG,
 * recursion, and large-number arithmetic.
 */
NUMERIC DIGITS 20
DO n = 0 TO 15
  SAY n'! =' factorial(n)
END
EXIT

factorial: PROCEDURE
  ARG n
  IF n <= 1 THEN RETURN 1
  RETURN n * factorial(n - 1)
