/* csv_processor.rexx — CSV Data Processing
 *
 * Demonstrates: PARSE with literal comma patterns, stem variables
 * as a table, DO loops, numeric formatting, and string BIFs.
 *
 * Processes CSV records and computes column statistics.
 */

/* CSV data */
rec.1 = 'Alice,Engineering,95000'
rec.2 = 'Bob,Marketing,72000'
rec.3 = 'Carol,Engineering,105000'
rec.4 = 'Dave,Marketing,68000'
rec.5 = 'Eve,Engineering,98000'
rec.6 = 'Frank,Sales,71000'
rec.7 = 'Grace,Sales,74000'
rec.0 = 7

/* Display table */
SAY 'Employee Records'
SAY '================'
SAY LEFT('Name', 10) LEFT('Department', 15) RIGHT('Salary', 10)
SAY COPIES('-', 37)

total = 0
max_salary = 0
min_salary = 999999999
eng_count = 0
eng_total = 0
mkt_count = 0
mkt_total = 0
sal_count = 0
sal_total = 0

DO i = 1 TO rec.0
  line = rec.i
  PARSE VAR line name ',' dept ',' salary
  SAY LEFT(name, 10) LEFT(dept, 15) RIGHT(salary, 10)

  /* Track stats */
  total = total + salary
  IF salary > max_salary THEN DO
    max_salary = salary
    max_name = name
  END
  IF salary < min_salary THEN DO
    min_salary = salary
    min_name = name
  END

  /* Count by department */
  SELECT
    WHEN dept = 'Engineering' THEN DO
      eng_count = eng_count + 1
      eng_total = eng_total + salary
    END
    WHEN dept = 'Marketing' THEN DO
      mkt_count = mkt_count + 1
      mkt_total = mkt_total + salary
    END
    WHEN dept = 'Sales' THEN DO
      sal_count = sal_count + 1
      sal_total = sal_total + salary
    END
    OTHERWISE NOP
  END
END

SAY COPIES('-', 37)
SAY ''

/* Summary statistics */
avg = TRUNC(total / rec.0, 0)
SAY 'Statistics'
SAY '=========='
SAY 'Total payroll: $'total
SAY 'Average salary: $'avg
SAY 'Highest:' max_name '($'max_salary')'
SAY 'Lowest: ' min_name '($'min_salary')'
SAY ''

/* Department breakdown */
SAY 'By Department'
SAY '============='
IF eng_count > 0 THEN
  SAY LEFT('Engineering', 15) eng_count 'employees, avg $'TRUNC(eng_total / eng_count, 0)
IF mkt_count > 0 THEN
  SAY LEFT('Marketing', 15) mkt_count 'employees, avg $'TRUNC(mkt_total / mkt_count, 0)
IF sal_count > 0 THEN
  SAY LEFT('Sales', 15) sal_count 'employees, avg $'TRUNC(sal_total / sal_count, 0)
