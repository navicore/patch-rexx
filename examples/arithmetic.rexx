/* arithmetic.rexx — Arbitrary-Precision Arithmetic
 *
 * Demonstrates: NUMERIC DIGITS, large number computation,
 * FORMAT, TRUNC, division precision, power operator.
 */

/* --- Default precision (9 digits) --- */
SAY 'Default precision (DIGITS=9):'
SAY '  1/3          =' 1/3
SAY '  2**32        =' 2**32
SAY ''

/* --- High precision --- */
NUMERIC DIGITS 50
SAY 'High precision (DIGITS=50):'
SAY '  1/3          =' 1/3
SAY '  1/7          =' 1/7
SAY '  2**128       =' 2**128
SAY ''

/* --- Large factorials --- */
NUMERIC DIGITS 100
SAY 'Large factorials (DIGITS=100):'
f = 1
DO i = 1 TO 50
  f = f * i
END
SAY '  50!  =' f
SAY '  digits in 50! =' LENGTH(f)
SAY ''

/* --- FORMAT and TRUNC --- */
NUMERIC DIGITS 20
SAY 'FORMAT and TRUNC:'
pi_approx = 355 / 113
SAY '  355/113      =' pi_approx
SAY '  TRUNC(,5)    =' TRUNC(pi_approx, 5)
SAY '  FORMAT(,2,3) :' FORMAT(pi_approx, 2, 3)
SAY ''

/* --- Power operator with big results --- */
NUMERIC DIGITS 40
SAY 'Power operator (DIGITS=40):'
SAY '  9**9         =' 9**9
SAY '  99**5        =' 99**5
SAY '  12345**3     =' 12345**3
