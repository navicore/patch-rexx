/* log_parser.rexx — Log File Parser
 *
 * Demonstrates: PARSE with literal patterns, stem variables
 * as counters, DO loop, SELECT/WHEN, and string BIFs.
 *
 * Parses a set of log lines and produces a summary by level.
 */

/* Sample log data (in a real script, read from a file via LINEIN) */
log.1 = '2024-03-15 08:12:01 INFO  Server started on port 8080'
log.2 = '2024-03-15 08:12:03 INFO  Database connection established'
log.3 = '2024-03-15 08:15:22 WARN  Slow query detected: 1200ms'
log.4 = '2024-03-15 08:16:45 ERROR Connection pool exhausted'
log.5 = '2024-03-15 08:16:46 ERROR Retry failed after 3 attempts'
log.6 = '2024-03-15 08:17:00 INFO  Pool recovered, 5 connections available'
log.7 = '2024-03-15 08:20:10 WARN  Disk usage at 85%'
log.8 = '2024-03-15 08:25:33 INFO  Health check passed'
log.0 = 8

/* Counters */
count_info  = 0
count_warn  = 0
count_error = 0

/* Parse each log line */
DO i = 1 TO log.0
  line = log.i
  PARSE VAR line date time level message

  SELECT
    WHEN level = 'INFO'  THEN count_info  = count_info  + 1
    WHEN level = 'WARN'  THEN count_warn  = count_warn  + 1
    WHEN level = 'ERROR' THEN count_error = count_error + 1
    OTHERWISE NOP
  END
END

/* Summary */
SAY 'Log Summary'
SAY '==========='
SAY 'Total lines:' log.0
SAY 'INFO ......' count_info
SAY 'WARN ......' count_warn
SAY 'ERROR .....' count_error
SAY ''

/* Show only errors */
SAY 'Error details:'
DO i = 1 TO log.0
  line = log.i
  PARSE VAR line date time level message
  IF level = 'ERROR' THEN
    SAY '  ['date time']' STRIP(message)
END
