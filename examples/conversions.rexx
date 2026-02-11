/* conversions.rexx — Data Type Conversions
 *
 * Demonstrates: D2X, X2D, C2D, D2C, C2X, X2C,
 * B2X, X2B, DATATYPE, XRANGE.
 */

SAY 'REXX Data Type Conversions'
SAY COPIES('-', 35)
SAY ''

/* --- Decimal / Hexadecimal --- */
SAY 'Decimal <-> Hexadecimal:'
DO val = 0 TO 15
  SAY '  'd2x(val) '<- D2X('val')    X2D("'D2X(val)'") ->' X2D(D2X(val))
END
SAY ''
SAY '  D2X(255)  =' D2X(255)
SAY '  D2X(65535)=' D2X(65535)
SAY '  X2D("FF") =' X2D('FF')
SAY '  X2D("FFFF")=' X2D('FFFF')
SAY ''

/* --- Character / Decimal --- */
SAY 'Character <-> Decimal:'
chars = 'A Z a z 0 9'
DO i = 1 TO WORDS(chars)
  ch = WORD(chars, i)
  SAY '  C2D("'ch'") =' C2D(ch) '   D2C('C2D(ch)') =' D2C(C2D(ch))
END
SAY ''

/* --- Character / Hexadecimal --- */
SAY 'Character <-> Hexadecimal:'
SAY '  C2X("A")    =' C2X('A')
SAY '  C2X("REXX") =' C2X('REXX')
SAY '  X2C("41")   =' X2C('41')
SAY '  X2C("52455858") =' X2C('52455858')
SAY ''

/* --- Binary / Hexadecimal --- */
SAY 'Binary <-> Hexadecimal:'
SAY '  B2X("0001")     =' B2X('0001')
SAY '  B2X("11111111") =' B2X('11111111')
SAY '  B2X("10101010") =' B2X('10101010')
SAY '  X2B("F")        =' X2B('F')
SAY '  X2B("FF")       =' X2B('FF')
SAY '  X2B("A5")       =' X2B('A5')
SAY ''

/* --- DATATYPE checks --- */
SAY 'DATATYPE checks:'
test_values = '42 3.14 -7 abc 1E3 FF'
DO i = 1 TO WORDS(test_values)
  v = WORD(test_values, i)
  SAY '  DATATYPE("'v'")     =' DATATYPE(v)
  SAY '  DATATYPE("'v'","N") =' DATATYPE(v, 'N')
END
SAY ''

/* --- XRANGE --- */
SAY 'XRANGE examples:'
SAY '  XRANGE("a","z") =' XRANGE('a', 'z')
SAY '  XRANGE("0","9") =' XRANGE('0', '9')
SAY '  C2X(XRANGE("A","F")) =' C2X(XRANGE('A', 'F'))
