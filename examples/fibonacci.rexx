/* fibonacci.rexx — Fibonacci Sequence
 *
 * Demonstrates: DO loop with controlled iteration,
 * multiple variable tracking, and string concatenation.
 */
SAY 'First 20 Fibonacci numbers:'
SAY ''

a = 0
b = 1
DO i = 1 TO 20
  SAY 'fib('i') =' a
  c = a + b
  a = b
  b = c
END
