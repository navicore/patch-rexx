/* stem_arrays.rexx — Stem Variables as Arrays/Maps
 *
 * Demonstrates: compound variables, stem defaults,
 * multi-dimensional indexing, iteration over stem "arrays".
 */

/* --- Simple indexed array --- */
SAY 'Simple array:'
color.1 = 'red'
color.2 = 'green'
color.3 = 'blue'
color.0 = 3

DO i = 1 TO color.0
  SAY '  color.'i '=' color.i
END
SAY ''

/* --- Stem default value --- */
SAY 'Stem default:'
count. = 0
count.apples = 5
count.oranges = 3
SAY '  count.apples  =' count.apples
SAY '  count.oranges =' count.oranges
SAY '  count.bananas =' count.bananas '(default)'
SAY ''

/* --- String-keyed map --- */
SAY 'String-keyed map (capital cities):'
capital. = 'unknown'
capital.France = 'Paris'
capital.Germany = 'Berlin'
capital.Japan = 'Tokyo'
capital.Brazil = 'Brasilia'

countries = 'France Germany Japan Brazil Canada'
DO i = 1 TO WORDS(countries)
  c = WORD(countries, i)
  SAY '  capital.'c '=' capital.c
END
SAY ''

/* --- Multi-dimensional stem --- */
SAY 'Multi-dimensional (3x3 identity matrix):'
matrix. = 0
DO r = 1 TO 3
  DO c = 1 TO 3
    IF r = c THEN matrix.r.c = 1
  END
END

DO r = 1 TO 3
  row = ''
  DO c = 1 TO 3
    row = row matrix.r.c
  END
  SAY ' ' row
END
