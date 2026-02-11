/* select_menu.rexx — SELECT/WHEN Decision Logic
 *
 * Demonstrates: SELECT/WHEN/OTHERWISE for menu-style dispatch,
 * nested SELECT, and DO blocks within WHEN clauses.
 */

/* --- Day of the week classifier --- */
SAY 'Day classifier:'
DO day = 1 TO 7
  SELECT
    WHEN day = 1 THEN kind = 'Monday    - start of work week'
    WHEN day = 2 THEN kind = 'Tuesday   - getting into rhythm'
    WHEN day = 3 THEN kind = 'Wednesday - midweek'
    WHEN day = 4 THEN kind = 'Thursday  - almost there'
    WHEN day = 5 THEN kind = 'Friday    - TGIF!'
    WHEN day = 6 THEN kind = 'Saturday  - weekend!'
    WHEN day = 7 THEN kind = 'Sunday    - rest day'
    OTHERWISE         kind = 'unknown'
  END
  SAY '  Day' day':' kind
END
SAY ''

/* --- Grade calculator with DO blocks --- */
SAY 'Grade calculator:'
scores = '95 82 67 45 73 88 55 91'
DO i = 1 TO WORDS(scores)
  score = WORD(scores, i)
  SELECT
    WHEN score >= 90 THEN DO
      grade = 'A'
      remark = 'Excellent'
    END
    WHEN score >= 80 THEN DO
      grade = 'B'
      remark = 'Good'
    END
    WHEN score >= 70 THEN DO
      grade = 'C'
      remark = 'Average'
    END
    WHEN score >= 60 THEN DO
      grade = 'D'
      remark = 'Below average'
    END
    OTHERWISE DO
      grade = 'F'
      remark = 'Failing'
    END
  END
  SAY '  Score' LEFT(score, 3) '-> Grade' grade '('remark')'
END
