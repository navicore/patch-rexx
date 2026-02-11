/* fizzbuzz.rexx — FizzBuzz
 *
 * Demonstrates: DO loop, IF/THEN/ELSE, modulo (//),
 * string concatenation, and NOP.
 */
DO i = 1 TO 30
  SELECT
    WHEN i // 15 = 0 THEN SAY 'FizzBuzz'
    WHEN i // 3  = 0 THEN SAY 'Fizz'
    WHEN i // 5  = 0 THEN SAY 'Buzz'
    OTHERWISE             SAY i
  END
END
