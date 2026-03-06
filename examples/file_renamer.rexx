/* file_renamer.rexx — Batch File Rename Planner
 *
 * Demonstrates: PARSE with literal patterns, TRANSLATE,
 * string BIFs (POS, LEFT, SUBSTR, LASTPOS, STRIP),
 * DO loops, and building shell commands with ADDRESS.
 *
 * This example builds rename commands but only prints
 * them (dry-run mode) so it is safe to run.
 */

/* File list to rename: spaces and mixed case to clean up */
file.1 = 'Meeting Notes (Draft).txt'
file.2 = 'Q1 Report FINAL.pdf'
file.3 = 'Photo From Trip.JPG'
file.4 = 'MY DOCUMENT v2.doc'
file.5 = 'Budget   2024.xlsx'
file.0 = 5

SAY 'Batch Rename Plan (dry run)'
SAY '==========================='
SAY ''

DO i = 1 TO file.0
  old_name = file.i

  /* Split name and extension at last dot */
  dot_pos = LASTPOS('.', old_name)
  IF dot_pos > 0 THEN DO
    base = LEFT(old_name, dot_pos - 1)
    ext = SUBSTR(old_name, dot_pos + 1)
  END
  ELSE DO
    base = old_name
    ext = ''
  END

  /* Clean the base name:
   *   - lowercase
   *   - replace spaces with underscores
   *   - collapse multiple underscores
   *   - strip parentheses
   */
  base = TRANSLATE(base)  /* uppercase first for normalize */
  base = TRANSLATE(base, 'abcdefghijklmnopqrstuvwxyz',,
                         'ABCDEFGHIJKLMNOPQRSTUVWXYZ')

  /* Replace spaces and parens with underscores */
  new_base = ''
  DO j = 1 TO LENGTH(base)
    ch = SUBSTR(base, j, 1)
    SELECT
      WHEN ch = ' '  THEN new_base = new_base || '_'
      WHEN ch = '('  THEN new_base = new_base || '_'
      WHEN ch = ')'  THEN NOP
      OTHERWISE            new_base = new_base || ch
    END
  END

  /* Collapse runs of underscores */
  DO WHILE POS('__', new_base) > 0
    PARSE VAR new_base before '__' after
    new_base = before || '_' || after
  END
  new_base = STRIP(new_base, 'B', '_')

  /* Lowercase the extension too */
  ext = TRANSLATE(ext, 'abcdefghijklmnopqrstuvwxyz',,
                       'ABCDEFGHIJKLMNOPQRSTUVWXYZ')

  /* Build new filename */
  IF ext \= '' THEN
    new_name = new_base || '.' || ext
  ELSE
    new_name = new_base

  SAY '  'old_name
  SAY '    -> 'new_name
  SAY ''
END

SAY 'To execute, pipe these as mv commands to your shell.'
