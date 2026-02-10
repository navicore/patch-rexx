pub struct BifInfo {
    pub name: &'static str,
    pub signature: &'static str,
    pub description: &'static str,
}

static BIFS: &[BifInfo] = &[
    // String functions
    BifInfo {
        name: "LENGTH",
        signature: "LENGTH(string)",
        description: "Returns the length of string.",
    },
    BifInfo {
        name: "SUBSTR",
        signature: "SUBSTR(string, start [, length [, pad]])",
        description: "Returns a substring of string starting at position start.",
    },
    BifInfo {
        name: "LEFT",
        signature: "LEFT(string, length [, pad])",
        description: "Returns the leftmost length characters of string.",
    },
    BifInfo {
        name: "RIGHT",
        signature: "RIGHT(string, length [, pad])",
        description: "Returns the rightmost length characters of string.",
    },
    BifInfo {
        name: "POS",
        signature: "POS(needle, haystack [, start])",
        description: "Returns the position of needle in haystack, or 0 if not found.",
    },
    BifInfo {
        name: "LASTPOS",
        signature: "LASTPOS(needle, haystack [, start])",
        description: "Returns the last position of needle in haystack, or 0 if not found.",
    },
    BifInfo {
        name: "INDEX",
        signature: "INDEX(haystack, needle [, start])",
        description: "Returns the position of needle in haystack (like POS with swapped args).",
    },
    BifInfo {
        name: "COPIES",
        signature: "COPIES(string, n)",
        description: "Returns n copies of string concatenated together.",
    },
    BifInfo {
        name: "REVERSE",
        signature: "REVERSE(string)",
        description: "Returns string with characters in reverse order.",
    },
    BifInfo {
        name: "STRIP",
        signature: "STRIP(string [, option [, char]])",
        description: "Strips leading/trailing characters from string. Option: B(oth), L(eading), T(railing).",
    },
    BifInfo {
        name: "SPACE",
        signature: "SPACE(string [, n [, pad]])",
        description: "Formats blank-delimited words with n pad characters between each word.",
    },
    BifInfo {
        name: "OVERLAY",
        signature: "OVERLAY(new, target [, start [, length [, pad]]])",
        description: "Overlays new onto target starting at position start.",
    },
    BifInfo {
        name: "INSERT",
        signature: "INSERT(new, target [, start [, length [, pad]]])",
        description: "Inserts new into target after position start.",
    },
    BifInfo {
        name: "DELSTR",
        signature: "DELSTR(string, start [, length])",
        description: "Deletes a substring from string starting at position start.",
    },
    BifInfo {
        name: "TRANSLATE",
        signature: "TRANSLATE(string [, tableo [, tablei [, pad]]])",
        description: "Translates characters in string using translation tables.",
    },
    BifInfo {
        name: "CHANGESTR",
        signature: "CHANGESTR(needle, haystack, replacement)",
        description: "Replaces all occurrences of needle in haystack with replacement.",
    },
    BifInfo {
        name: "COUNTSTR",
        signature: "COUNTSTR(needle, haystack)",
        description: "Counts non-overlapping occurrences of needle in haystack.",
    },
    BifInfo {
        name: "COMPARE",
        signature: "COMPARE(string1, string2 [, pad])",
        description: "Returns 0 if strings are equal, otherwise the position of first difference.",
    },
    BifInfo {
        name: "ABBREV",
        signature: "ABBREV(information, info [, length])",
        description: "Returns 1 if info is an abbreviation of information.",
    },
    // Word functions
    BifInfo {
        name: "WORDS",
        signature: "WORDS(string)",
        description: "Returns the number of blank-delimited words in string.",
    },
    BifInfo {
        name: "WORD",
        signature: "WORD(string, n)",
        description: "Returns the nth blank-delimited word in string.",
    },
    BifInfo {
        name: "WORDINDEX",
        signature: "WORDINDEX(string, n)",
        description: "Returns the character position of the nth word in string.",
    },
    BifInfo {
        name: "WORDLENGTH",
        signature: "WORDLENGTH(string, n)",
        description: "Returns the length of the nth word in string.",
    },
    BifInfo {
        name: "SUBWORD",
        signature: "SUBWORD(string, n [, length])",
        description: "Returns a substring of string starting at the nth word.",
    },
    BifInfo {
        name: "WORDPOS",
        signature: "WORDPOS(phrase, string [, start])",
        description: "Returns the word position of phrase in string, or 0.",
    },
    BifInfo {
        name: "DELWORD",
        signature: "DELWORD(string, n [, length])",
        description: "Deletes words from string starting at word n.",
    },
    // Numeric functions
    BifInfo {
        name: "ABS",
        signature: "ABS(number)",
        description: "Returns the absolute value of number.",
    },
    BifInfo {
        name: "SIGN",
        signature: "SIGN(number)",
        description: "Returns -1, 0, or 1 indicating the sign of number.",
    },
    BifInfo {
        name: "MAX",
        signature: "MAX(number [, number]...)",
        description: "Returns the largest of the given numbers.",
    },
    BifInfo {
        name: "MIN",
        signature: "MIN(number [, number]...)",
        description: "Returns the smallest of the given numbers.",
    },
    BifInfo {
        name: "TRUNC",
        signature: "TRUNC(number [, n])",
        description: "Truncates number to n decimal places (default 0).",
    },
    BifInfo {
        name: "FORMAT",
        signature: "FORMAT(number [, before [, after [, expp [, expt]]]])",
        description: "Formats number with specified integer and decimal places.",
    },
    BifInfo {
        name: "RANDOM",
        signature: "RANDOM([min] [, max] [, seed])",
        description: "Returns a pseudo-random whole number in the range min to max.",
    },
    // Conversion functions
    BifInfo {
        name: "D2C",
        signature: "D2C(wholenumber [, length])",
        description: "Converts a decimal number to its character representation.",
    },
    BifInfo {
        name: "C2D",
        signature: "C2D(string [, length])",
        description: "Converts a character string to its decimal representation.",
    },
    BifInfo {
        name: "D2X",
        signature: "D2X(wholenumber [, length])",
        description: "Converts a decimal number to its hexadecimal representation.",
    },
    BifInfo {
        name: "X2D",
        signature: "X2D(hexstring [, length])",
        description: "Converts a hexadecimal string to its decimal representation.",
    },
    BifInfo {
        name: "C2X",
        signature: "C2X(string)",
        description: "Converts a character string to its hexadecimal representation.",
    },
    BifInfo {
        name: "X2C",
        signature: "X2C(hexstring)",
        description: "Converts a hexadecimal string to its character representation.",
    },
    BifInfo {
        name: "B2X",
        signature: "B2X(binarystring)",
        description: "Converts a binary string to its hexadecimal representation.",
    },
    BifInfo {
        name: "X2B",
        signature: "X2B(hexstring)",
        description: "Converts a hexadecimal string to its binary representation.",
    },
    // Informational functions
    BifInfo {
        name: "DATATYPE",
        signature: "DATATYPE(string [, type])",
        description: "Returns 'NUM' or 'CHAR', or tests if string matches a specific type.",
    },
    BifInfo {
        name: "VERIFY",
        signature: "VERIFY(string, reference [, option [, start]])",
        description: "Returns position of first character in string not in reference, or 0.",
    },
    BifInfo {
        name: "XRANGE",
        signature: "XRANGE([start [, end]])",
        description: "Returns a string of all characters from start to end inclusive.",
    },
    // Date/Time
    BifInfo {
        name: "DATE",
        signature: "DATE([option])",
        description: "Returns the current date in the format specified by option.",
    },
    BifInfo {
        name: "TIME",
        signature: "TIME([option])",
        description: "Returns the current time in the format specified by option.",
    },
    // Condition/Address
    BifInfo {
        name: "CONDITION",
        signature: "CONDITION([option])",
        description: "Returns information about the current trapped condition.",
    },
    BifInfo {
        name: "ADDRESS",
        signature: "ADDRESS()",
        description: "Returns the name of the current default environment.",
    },
];

pub fn lookup_bif(name: &str) -> Option<&'static BifInfo> {
    let upper = name.to_uppercase();
    BIFS.iter().find(|b| b.name == upper)
}

pub fn all_bifs() -> &'static [BifInfo] {
    BIFS
}
