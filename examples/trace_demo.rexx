/* trace_demo.rexx — Execution Tracing
 *
 * Demonstrates: TRACE RESULTS, TRACE INTERMEDIATES,
 * TRACE OFF, TRACE() BIF, and the output tag format.
 *
 * Run this to see how REXX's built-in tracing works.
 * Each section enables a different trace level and
 * shows what output it produces.
 */

/* --- TRACE RESULTS: show expression results --- */
SAY '=== TRACE RESULTS ==='
SAY '(Each expression result shown with >>> prefix)'
SAY ''
TRACE RESULTS
x = 3 + 4
SAY 'x is' x
TRACE OFF
SAY ''

/* --- TRACE INTERMEDIATES: show every sub-expression --- */
SAY '=== TRACE INTERMEDIATES ==='
SAY '(Sub-expressions tagged: >L> literal, >V> variable, >O> operator)'
SAY ''
TRACE INTERMEDIATES
a = 10
b = 20
c = a + b * 2
TRACE OFF
SAY ''
SAY 'c =' c
SAY ''

/* --- TRACE() BIF: query and set trace level --- */
SAY '=== TRACE() BIF ==='
SAY 'Current trace level:' TRACE()
old = TRACE('R')
SAY 'Was:' old '  Now:' TRACE()
TRACE OFF
SAY ''

/* --- TRACE in a loop --- */
SAY '=== TRACE in a loop ==='
SAY ''
TRACE RESULTS
DO i = 1 TO 3
  SAY 'iteration' i
END
TRACE OFF
SAY ''
SAY 'Trace demo complete.'
