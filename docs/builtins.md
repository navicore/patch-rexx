# Built-in Functions

53 ANSI REXX built-in functions are implemented, organized by category.

## String

| Function | Signature | Description |
|----------|-----------|-------------|
| ABBREV | `ABBREV(string, abbr [,length])` | Test if abbr is a valid abbreviation of string |
| CHANGESTR | `CHANGESTR(old, string, new)` | Replace all occurrences of old with new |
| COMPARE | `COMPARE(string1, string2 [,pad])` | Return position of first difference, 0 if equal |
| COPIES | `COPIES(string, count)` | Return count concatenated copies |
| COUNTSTR | `COUNTSTR(needle, string)` | Count occurrences of needle |
| DELSTR | `DELSTR(string, start [,length])` | Delete substring |
| INDEX | `INDEX(haystack, needle [,start])` | Find position of needle (POS with swapped args) |
| INSERT | `INSERT(new, target [,position] [,length] [,pad])` | Insert string at position |
| LASTPOS | `LASTPOS(needle, haystack [,start])` | Find last occurrence |
| LEFT | `LEFT(string, length [,pad])` | Left-justify to length |
| LENGTH | `LENGTH(string)` | Return character count |
| OVERLAY | `OVERLAY(new, target [,position] [,length] [,pad])` | Overlay string at position |
| POS | `POS(needle, haystack [,start])` | Find first occurrence |
| REVERSE | `REVERSE(string)` | Reverse characters |
| RIGHT | `RIGHT(string, length [,pad])` | Right-justify to length |
| SPACE | `SPACE(string [,count] [,pad])` | Normalize inter-word spacing |
| STRIP | `STRIP(string [,option] [,char])` | Remove leading/trailing characters |
| SUBSTR | `SUBSTR(string, start [,length] [,pad])` | Extract substring |
| TRANSLATE | `TRANSLATE(string [,to] [,from] [,pad])` | Character-by-character translation |

## Word

| Function | Signature | Description |
|----------|-----------|-------------|
| DELWORD | `DELWORD(string, start [,length])` | Delete words |
| SUBWORD | `SUBWORD(string, start [,length])` | Extract word range |
| WORD | `WORD(string, n)` | Return nth word |
| WORDINDEX | `WORDINDEX(string, n)` | Return character position of nth word |
| WORDLENGTH | `WORDLENGTH(string, n)` | Return length of nth word |
| WORDPOS | `WORDPOS(phrase, string [,start])` | Find word position of phrase |
| WORDS | `WORDS(string)` | Count words |

## Numeric

| Function | Signature | Description |
|----------|-----------|-------------|
| ABS | `ABS(number)` | Absolute value |
| FORMAT | `FORMAT(number [,before] [,after] [,expp] [,expt])` | Format number |
| MAX | `MAX(number, number [,...])` | Maximum value |
| MIN | `MIN(number, number [,...])` | Minimum value |
| RANDOM | `RANDOM([min] [,max] [,seed])` | Random integer |
| SIGN | `SIGN(number)` | Sign indicator (-1, 0, 1) |
| TRUNC | `TRUNC(number [,decimals])` | Truncate to decimal places |

## Conversion

| Function | Signature | Description |
|----------|-----------|-------------|
| B2X | `B2X(binary_string)` | Binary to hexadecimal |
| C2D | `C2D(string [,length])` | Character to decimal |
| C2X | `C2X(string)` | Character to hexadecimal |
| D2C | `D2C(number [,length])` | Decimal to character |
| D2X | `D2X(number [,length])` | Decimal to hexadecimal |
| X2B | `X2B(hex_string)` | Hexadecimal to binary |
| X2C | `X2C(hex_string)` | Hexadecimal to character |
| X2D | `X2D(hex_string [,length])` | Hexadecimal to decimal |

## Type and Validation

| Function | Signature | Description |
|----------|-----------|-------------|
| DATATYPE | `DATATYPE(value [,type])` | Check data type (NUM, CHAR, A, N, X, W, etc.) |
| VERIFY | `VERIFY(string, reference [,option] [,start])` | Verify characters against reference |
| XRANGE | `XRANGE([start] [,end])` | Generate character range |

## Date and Time

| Function | Signature | Description |
|----------|-----------|-------------|
| DATE | `DATE([format])` | Current date (Normal, Sorted, Days, Weekday, etc.) |
| TIME | `TIME([format])` | Current time (Normal, Hours, Seconds, etc.) |

## System and Information

| Function | Signature | Description |
|----------|-----------|-------------|
| ADDRESS | `ADDRESS()` | Return current command environment name |
| CONDITION | `CONDITION([option])` | Return condition trap information |
| DIGITS | `DIGITS()` | Return current NUMERIC DIGITS setting |
| FORM | `FORM()` | Return current NUMERIC FORM setting |
| FUZZ | `FUZZ()` | Return current NUMERIC FUZZ setting |
| QUEUED | `QUEUED()` | Return number of lines in external data queue |
| TRACE | `TRACE([setting])` | Query or set trace level |

## Not yet implemented

ARG, CENTER/CENTRE, BITAND, BITOR, BITXOR, ERRORTEXT, SOURCELINE, SYMBOL, VALUE (as a BIF).
