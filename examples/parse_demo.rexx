/* parse_demo.rexx — PARSE Templates
 *
 * Demonstrates: PARSE VAR with word splitting, literal patterns,
 * positional patterns (absolute and relative), variable patterns,
 * dot placeholder, and PARSE VALUE ... WITH.
 */

/* --- Simple word splitting --- */
line = 'John Smith 42 London'
PARSE VAR line first last age city
SAY 'Word splitting:'
SAY '  first=' first '  last=' last '  age=' age '  city=' city
SAY ''

/* --- Literal pattern --- */
data = 'name=Alice;city=Paris;score=99'
PARSE VAR data 'name=' name ';city=' city ';score=' score
SAY 'Literal pattern:'
SAY '  name=' name '  city=' city '  score=' score
SAY ''

/* --- Absolute positional --- */
fixed = 'SMITH     JOHN      30NYC'
PARSE VAR fixed surname 11 firstname 21 age 23 city
SAY 'Absolute positional (fixed-width fields):'
SAY '  surname="'STRIP(surname)'"  firstname="'STRIP(firstname)'"  age=' age '  city=' city
SAY ''

/* --- Relative positional --- */
rec = 'ABCDEFGHIJ'
PARSE VAR rec first3 +3 next4 +4 rest
SAY 'Relative positional:'
SAY '  first3=' first3 '  next4=' next4 '  rest=' rest
SAY ''

/* --- Dot placeholder (skip a token) --- */
sentence = 'Error 42: file not found'
PARSE VAR sentence . code ':' message
SAY 'Dot placeholder (skip first word):'
SAY '  code=' code '  message=' STRIP(message)
SAY ''

/* --- PARSE VALUE ... WITH --- */
expr = 3 + 4
PARSE VALUE 'Result:' expr WITH label ':' val
SAY 'PARSE VALUE ... WITH:'
SAY '  label=' label '  val=' STRIP(val)
