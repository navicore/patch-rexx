/* words.rexx — Word Manipulation
 *
 * Demonstrates: WORDS, WORD, WORDINDEX, WORDLENGTH,
 * SUBWORD, WORDPOS, DELWORD on sample text.
 * REXX treats blank-delimited tokens as "words".
 */
text = 'The quick brown fox jumps over the lazy dog'

SAY 'Text:' text
SAY ''
SAY 'WORDS       :' WORDS(text)

DO i = 1 TO WORDS(text)
  w = WORD(text, i)
  SAY '  WORD('i')     :' LEFT(w, 8) '  index=' WORDINDEX(text, i) '  length=' WORDLENGTH(text, i)
END

SAY ''
SAY 'SUBWORD(3,3)   :' SUBWORD(text, 3, 3)
SAY 'WORDPOS("fox") :' WORDPOS('fox', text)
SAY 'WORDPOS("cat") :' WORDPOS('cat', text)
SAY 'DELWORD(4,2)   :' DELWORD(text, 4, 2)
