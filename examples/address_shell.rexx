/* address_shell.rexx — Shell Scripting with ADDRESS
 *
 * Demonstrates: ADDRESS SYSTEM for running shell commands,
 * checking RC (return code), and temporary ADDRESS.
 */

SAY 'Shell integration with ADDRESS'
SAY COPIES('-', 35)
SAY ''

/* --- Run a simple command --- */
SAY 'Running "echo" via ADDRESS SYSTEM:'
ADDRESS SYSTEM 'echo Hello from the shell!'
SAY '  Return code:' RC
SAY ''

/* --- Capture the current date --- */
SAY 'Running "date" command:'
ADDRESS SYSTEM 'date'
SAY '  Return code:' RC
SAY ''

/* --- Check for command failure --- */
SAY 'Testing a failing command:'
ADDRESS SYSTEM 'test -f /nonexistent/file'
SAY '  Return code:' RC '(expected non-zero)'
SAY ''

/* --- Show current ADDRESS environment --- */
SAY 'Current ADDRESS environment:' ADDRESS()
