/* queues.rexx — Stack and Queue Operations
 *
 * Demonstrates: PUSH (LIFO), QUEUE (FIFO), PULL,
 * QUEUED() BIF, building and processing a work queue.
 */

SAY 'REXX Stack and Queue Operations'
SAY COPIES('-', 35)
SAY ''

/* --- PUSH (LIFO — last in, first out) --- */
SAY 'PUSH demo (LIFO):'
PUSH 'first pushed'
PUSH 'second pushed'
PUSH 'third pushed'
SAY '  Items on queue:' QUEUED()

DO WHILE QUEUED() > 0
  PULL item
  SAY '  Pulled:' item
END
SAY ''

/* --- QUEUE (FIFO — first in, first out) --- */
SAY 'QUEUE demo (FIFO):'
QUEUE 'first queued'
QUEUE 'second queued'
QUEUE 'third queued'
SAY '  Items on queue:' QUEUED()

DO WHILE QUEUED() > 0
  PULL item
  SAY '  Pulled:' item
END
SAY ''

/* --- Mixed operations — building a work queue --- */
SAY 'Work queue processing:'
tasks = 'compile link test package deploy'
DO i = 1 TO WORDS(tasks)
  QUEUE WORD(tasks, i)
END
SAY '  Queued' QUEUED() 'tasks'

step = 1
DO WHILE QUEUED() > 0
  PULL task
  SAY '  Step' step':' task
  step = step + 1
END
SAY '  Queue empty. QUEUED() =' QUEUED()
