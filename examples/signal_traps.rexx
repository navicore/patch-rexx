/* signal_traps.rexx — Error Handling with SIGNAL
 *
 * Demonstrates: SIGNAL ON SYNTAX, SIGNAL ON NOVALUE,
 * CONDITION() BIF, and SIGNAL VALUE for computed jumps.
 */

/* --- SIGNAL ON SYNTAX: catch division by zero --- */
SAY 'Testing SIGNAL ON SYNTAX (division by zero):'
SIGNAL ON SYNTAX NAME catch_syntax
x = 1 / 0
SAY 'This line should not appear'

catch_syntax:
  SAY '  Caught SYNTAX condition!'
  SAY '  Condition name:' CONDITION('C')
  SAY '  Description   :' CONDITION('D')
  SAY ''

/* --- SIGNAL ON NOVALUE: catch uninitialized variable --- */
SAY 'Testing SIGNAL ON NOVALUE:'
SIGNAL ON NOVALUE NAME catch_novalue
x = undefined_variable
SAY 'This line should not appear'

catch_novalue:
  SAY '  Caught NOVALUE condition!'
  SAY '  Condition name:' CONDITION('C')
  SAY '  Description   :' CONDITION('D')
  SAY ''

/* --- SIGNAL VALUE for computed jump --- */
SAY 'Testing SIGNAL VALUE (computed jump):'
choice = 'beta'
SIGNAL VALUE choice

alpha:
  SAY '  Jumped to alpha'
  SIGNAL done
beta:
  SAY '  Jumped to beta'
  SIGNAL done
gamma:
  SAY '  Jumped to gamma'
  SIGNAL done

done:
SAY ''
SAY 'Signal handling complete.'
