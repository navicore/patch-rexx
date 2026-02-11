/* strings.rexx — String Processing Showcase
 *
 * Demonstrates key string BIFs: SUBSTR, LEFT, RIGHT, POS,
 * COPIES, REVERSE, STRIP, TRANSLATE, OVERLAY, INSERT,
 * DELSTR, CHANGESTR, LENGTH, LASTPOS, COMPARE.
 */
text = 'Hello, REXX World!'

SAY 'Original    :' text
SAY 'LENGTH      :' LENGTH(text)
SAY 'SUBSTR(8,4) :' SUBSTR(text, 8, 4)
SAY 'LEFT(5)     :' LEFT(text, 5)
SAY 'RIGHT(6)    :' RIGHT(text, 6)
SAY 'POS("REXX") :' POS('REXX', text)
SAY 'LASTPOS("l"):' LASTPOS('l', text)
SAY ''

SAY 'COPIES("=-",10):' COPIES('=-', 10)
SAY 'REVERSE        :' REVERSE(text)
SAY ''

padded = '   spaces everywhere   '
SAY 'Before STRIP:' '"'padded'"'
SAY 'STRIP(Both) :' '"'STRIP(padded)'"'
SAY 'STRIP(Lead) :' '"'STRIP(padded, 'L')'"'
SAY ''

SAY 'TRANSLATE(upper):' TRANSLATE('hello rexx')
SAY 'TRANSLATE(rot13):' TRANSLATE('hello', 'nopqrstuvwxyzabcdefghijklm', 'abcdefghijklmnopqrstuvwxyz')
SAY ''

base = 'XXXXXXXXXXXX'
SAY 'OVERLAY("REXX",3)  :' OVERLAY('REXX', base, 3)
SAY 'INSERT("NEW ",1)   :' INSERT('NEW ', text, 1)
SAY 'DELSTR(6,6)        :' DELSTR(text, 6, 6)
SAY 'CHANGESTR("World","REXX World!","Universe"):' CHANGESTR('World', 'REXX World!', 'Universe')
SAY ''

SAY 'COMPARE("abc","abc"):' COMPARE('abc', 'abc')
SAY 'COMPARE("abc","axc"):' COMPARE('abc', 'axc')
