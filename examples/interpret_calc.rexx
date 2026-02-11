/* interpret_calc.rexx — Dynamic Code with INTERPRET
 *
 * Demonstrates: INTERPRET to evaluate dynamically constructed
 * REXX code at runtime — a simple expression calculator.
 */

SAY 'Dynamic expression evaluator using INTERPRET'
SAY COPIES('-', 45)
SAY ''

/* Evaluate a series of expressions dynamically */
expr.1 = '2 + 3 * 4'
expr.2 = '(2 + 3) * 4'
expr.3 = '2 ** 10'
expr.4 = '100 // 7'
expr.5 = "COPIES('Ha', 3)"
expr.6 = 'REVERSE("Hello")'
expr.7 = 'LENGTH("REXX is great")'
expr.0 = 7

DO i = 1 TO expr.0
  /* Build and execute: result = <expression> */
  code = 'result =' expr.i
  INTERPRET code
  SAY '  'LEFT(expr.i, 25) '=' result
END
SAY ''

/* Dynamic variable creation */
SAY 'Dynamic variable creation:'
names = 'alpha beta gamma delta'
DO i = 1 TO WORDS(names)
  varname = WORD(names, i)
  INTERPRET varname '=' i * 10
END

/* Read them back */
SAY '  alpha =' alpha
SAY '  beta  =' beta
SAY '  gamma =' gamma
SAY '  delta =' delta
