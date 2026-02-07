//! REXX lexer — tokenizes source text into a stream of tokens.
//!
//! REXX tokenization is straightforward: the language has no reserved words
//! (keywords are context-sensitive), so the lexer produces generic symbols
//! and lets the parser decide meaning.

use crate::error::{RexxDiagnostic, RexxError, RexxResult, SourceLoc};

/// Token types produced by the lexer.
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Literals
    StringLit(String),
    Number(String),
    Symbol(String),

    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    IntDiv,    // %
    Remainder, // //
    Power,     // **
    Concat,    // ||
    Assign,    // =

    // Comparison
    Equal,       // = (context-dependent, same char as Assign)
    NotEqual,    // \= or <>
    Greater,     // >
    Less,        // <
    GreaterEq,   // >= or \<
    LessEq,      // <= or \>
    StrictEq,    // ==
    StrictNotEq, // \==
    StrictGt,    // >>
    StrictLt,    // <<
    StrictGte,   // >>=
    StrictLte,   // <<=

    // Logical
    And, // &
    Or,  // |
    Xor, // &&
    Not, // \ or ¬

    // Delimiters
    LeftParen,
    RightParen,
    Comma,
    Semicolon,
    Colon,
    Dot,

    // Special
    Eol, // End of logical line (clause terminator)
    Eof,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub loc: SourceLoc,
}

impl Token {
    pub fn new(kind: TokenKind, loc: SourceLoc) -> Self {
        Self { kind, loc }
    }
}

pub struct Lexer {
    source: Vec<char>,
    pos: usize,
    line: usize,
    col: usize,
    lines: Vec<String>,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        let lines: Vec<String> = source.lines().map(String::from).collect();
        Self {
            source: source.chars().collect(),
            pos: 0,
            line: 1,
            col: 1,
            lines,
        }
    }

    pub fn tokenize(&mut self) -> RexxResult<Vec<Token>> {
        let mut tokens = Vec::new();

        loop {
            self.skip_whitespace_and_comments()?;

            if self.at_end() {
                tokens.push(Token::new(TokenKind::Eof, self.loc()));
                break;
            }

            let token = self.next_token()?;
            tokens.push(token);
        }

        Ok(tokens)
    }

    fn loc(&self) -> SourceLoc {
        let mut loc = SourceLoc::new(self.line, self.col);
        if self.line > 0 && self.line <= self.lines.len() {
            loc = loc.with_source(self.lines[self.line - 1].clone());
        }
        loc
    }

    fn at_end(&self) -> bool {
        self.pos >= self.source.len()
    }

    fn peek(&self) -> Option<char> {
        self.source.get(self.pos).copied()
    }

    fn peek_ahead(&self, n: usize) -> Option<char> {
        self.source.get(self.pos + n).copied()
    }

    fn advance(&mut self) -> Option<char> {
        let ch = self.source.get(self.pos).copied()?;
        self.pos += 1;
        if ch == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        Some(ch)
    }

    fn skip_whitespace_and_comments(&mut self) -> RexxResult<()> {
        loop {
            // Skip whitespace (but not newlines — they can be clause terminators)
            while let Some(ch) = self.peek() {
                if ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n' {
                    self.advance();
                } else {
                    break;
                }
            }

            // Skip block comments /* ... */ (can nest)
            if self.peek() == Some('/') && self.peek_ahead(1) == Some('*') {
                let loc = self.loc();
                self.advance(); // /
                self.advance(); // *
                let mut depth = 1u32;
                while depth > 0 {
                    if self.at_end() {
                        return Err(RexxDiagnostic::new(RexxError::UnmatchedComment).at(loc));
                    }
                    if self.peek() == Some('/') && self.peek_ahead(1) == Some('*') {
                        self.advance();
                        self.advance();
                        depth += 1;
                    } else if self.peek() == Some('*') && self.peek_ahead(1) == Some('/') {
                        self.advance();
                        self.advance();
                        depth -= 1;
                    } else {
                        self.advance();
                    }
                }
                continue;
            }

            // Skip line comments -- (ANSI REXX extension)
            if self.peek() == Some('-') && self.peek_ahead(1) == Some('-') {
                while let Some(ch) = self.peek() {
                    if ch == '\n' {
                        break;
                    }
                    self.advance();
                }
                continue;
            }

            break;
        }
        Ok(())
    }

    #[allow(clippy::too_many_lines)]
    fn next_token(&mut self) -> RexxResult<Token> {
        let loc = self.loc();
        let ch = self.peek().unwrap();

        match ch {
            // String literals: 'single' or "double" quoted
            '\'' | '"' => self.lex_string(ch),

            // Numbers
            '0'..='9' => Ok(self.lex_number()),

            // Symbols (identifiers, keywords — REXX has no reserved words)
            'a'..='z' | 'A'..='Z' | '_' | '!' | '?' | '@' | '#' | '$' => Ok(self.lex_symbol()),

            // Dot can start a symbol or be standalone
            '.' => {
                if self
                    .peek_ahead(1)
                    .is_some_and(|c| c.is_alphanumeric() || c == '_')
                {
                    Ok(self.lex_symbol())
                } else {
                    self.advance();
                    Ok(Token::new(TokenKind::Dot, loc))
                }
            }

            // Operators and delimiters
            '+' => {
                self.advance();
                Ok(Token::new(TokenKind::Plus, loc))
            }
            '-' => {
                self.advance();
                Ok(Token::new(TokenKind::Minus, loc))
            }
            '*' => {
                self.advance();
                if self.peek() == Some('*') {
                    self.advance();
                    Ok(Token::new(TokenKind::Power, loc))
                } else {
                    Ok(Token::new(TokenKind::Star, loc))
                }
            }
            '/' => {
                self.advance();
                if self.peek() == Some('/') {
                    self.advance();
                    Ok(Token::new(TokenKind::Remainder, loc))
                } else {
                    Ok(Token::new(TokenKind::Slash, loc))
                }
            }
            '%' => {
                self.advance();
                Ok(Token::new(TokenKind::IntDiv, loc))
            }
            '|' => {
                self.advance();
                if self.peek() == Some('|') {
                    self.advance();
                    Ok(Token::new(TokenKind::Concat, loc))
                } else {
                    Ok(Token::new(TokenKind::Or, loc))
                }
            }
            '&' => {
                self.advance();
                if self.peek() == Some('&') {
                    self.advance();
                    Ok(Token::new(TokenKind::Xor, loc))
                } else {
                    Ok(Token::new(TokenKind::And, loc))
                }
            }
            '\\' | '¬' => {
                self.advance();
                if self.peek() == Some('=') {
                    self.advance();
                    if self.peek() == Some('=') {
                        self.advance();
                        Ok(Token::new(TokenKind::StrictNotEq, loc))
                    } else {
                        Ok(Token::new(TokenKind::NotEqual, loc))
                    }
                } else if self.peek() == Some('<') {
                    self.advance();
                    Ok(Token::new(TokenKind::GreaterEq, loc))
                } else if self.peek() == Some('>') {
                    self.advance();
                    Ok(Token::new(TokenKind::LessEq, loc))
                } else {
                    Ok(Token::new(TokenKind::Not, loc))
                }
            }
            '=' => {
                self.advance();
                if self.peek() == Some('=') {
                    self.advance();
                    Ok(Token::new(TokenKind::StrictEq, loc))
                } else {
                    // Parser disambiguates assignment vs comparison
                    Ok(Token::new(TokenKind::Assign, loc))
                }
            }
            '>' => {
                self.advance();
                if self.peek() == Some('>') {
                    self.advance();
                    if self.peek() == Some('=') {
                        self.advance();
                        Ok(Token::new(TokenKind::StrictGte, loc))
                    } else {
                        Ok(Token::new(TokenKind::StrictGt, loc))
                    }
                } else if self.peek() == Some('=') {
                    self.advance();
                    Ok(Token::new(TokenKind::GreaterEq, loc))
                } else {
                    Ok(Token::new(TokenKind::Greater, loc))
                }
            }
            '<' => {
                self.advance();
                if self.peek() == Some('<') {
                    self.advance();
                    if self.peek() == Some('=') {
                        self.advance();
                        Ok(Token::new(TokenKind::StrictLte, loc))
                    } else {
                        Ok(Token::new(TokenKind::StrictLt, loc))
                    }
                } else if self.peek() == Some('=') {
                    self.advance();
                    Ok(Token::new(TokenKind::LessEq, loc))
                } else if self.peek() == Some('>') {
                    self.advance();
                    Ok(Token::new(TokenKind::NotEqual, loc))
                } else {
                    Ok(Token::new(TokenKind::Less, loc))
                }
            }
            '(' => {
                self.advance();
                Ok(Token::new(TokenKind::LeftParen, loc))
            }
            ')' => {
                self.advance();
                Ok(Token::new(TokenKind::RightParen, loc))
            }
            ',' => {
                self.advance();
                Ok(Token::new(TokenKind::Comma, loc))
            }
            ';' => {
                self.advance();
                Ok(Token::new(TokenKind::Semicolon, loc))
            }
            ':' => {
                self.advance();
                Ok(Token::new(TokenKind::Colon, loc))
            }
            _ => Err(RexxDiagnostic::new(RexxError::InvalidCharacter)
                .at(loc)
                .with_detail(format!("unexpected character '{ch}'"))),
        }
    }

    fn lex_string(&mut self, quote: char) -> RexxResult<Token> {
        let loc = self.loc();
        self.advance(); // opening quote
        let mut value = String::new();

        loop {
            if self.at_end() {
                return Err(RexxDiagnostic::new(RexxError::InvalidExpression)
                    .at(loc)
                    .with_detail("unterminated string literal"));
            }
            let ch = self.advance().unwrap();
            if ch == quote {
                // Doubled quote is an escape: '' inside '...' means literal '
                if self.peek() == Some(quote) {
                    self.advance();
                    value.push(quote);
                } else {
                    break;
                }
            } else {
                value.push(ch);
            }
        }

        // Check for hex/binary string suffix: '...'X or "..."X or B
        if let Some(suffix) = self.peek() {
            match suffix.to_ascii_uppercase() {
                'X' => {
                    self.advance();
                    let decoded = hex_string_to_chars(&value).map_err(|e| {
                        RexxDiagnostic::new(RexxError::InvalidHexBinary)
                            .at(loc.clone())
                            .with_detail(e)
                    })?;
                    return Ok(Token::new(TokenKind::StringLit(decoded), loc));
                }
                'B' => {
                    self.advance();
                    let decoded = bin_string_to_chars(&value).map_err(|e| {
                        RexxDiagnostic::new(RexxError::InvalidHexBinary)
                            .at(loc.clone())
                            .with_detail(e)
                    })?;
                    return Ok(Token::new(TokenKind::StringLit(decoded), loc));
                }
                _ => {}
            }
        }

        Ok(Token::new(TokenKind::StringLit(value), loc))
    }

    fn lex_number(&mut self) -> Token {
        let loc = self.loc();
        let mut num = String::new();

        while let Some(ch) = self.peek() {
            if ch.is_ascii_digit() || ch == '.' {
                num.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        // Exponent part
        if self.peek().is_some_and(|c| c == 'e' || c == 'E') {
            num.push(self.advance().unwrap());
            if self.peek().is_some_and(|c| c == '+' || c == '-') {
                num.push(self.advance().unwrap());
            }
            while let Some(ch) = self.peek() {
                if ch.is_ascii_digit() {
                    num.push(ch);
                    self.advance();
                } else {
                    break;
                }
            }
        }

        Token::new(TokenKind::Number(num), loc)
    }

    fn lex_symbol(&mut self) -> Token {
        let loc = self.loc();
        let mut name = String::new();

        while let Some(ch) = self.peek() {
            if ch.is_alphanumeric()
                || ch == '_'
                || ch == '.'
                || ch == '!'
                || ch == '?'
                || ch == '@'
                || ch == '#'
                || ch == '$'
            {
                name.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        Token::new(TokenKind::Symbol(name), loc)
    }
}

/// Convert a hex string like "48 65 6C" to characters.
fn hex_string_to_chars(s: &str) -> Result<String, String> {
    let hex: String = s.chars().filter(|c| !c.is_whitespace()).collect();
    if !hex.len().is_multiple_of(2) {
        return Err("odd number of hex digits".into());
    }
    let mut result = String::new();
    for i in (0..hex.len()).step_by(2) {
        let byte = u8::from_str_radix(&hex[i..i + 2], 16)
            .map_err(|_| format!("invalid hex digit at position {i}"))?;
        result.push(byte as char);
    }
    Ok(result)
}

/// Convert a binary string like "0100 1000" to characters.
fn bin_string_to_chars(s: &str) -> Result<String, String> {
    let bits: String = s.chars().filter(|c| !c.is_whitespace()).collect();
    if !bits.len().is_multiple_of(8) {
        return Err("binary string length must be a multiple of 8".into());
    }
    let mut result = String::new();
    for i in (0..bits.len()).step_by(8) {
        let byte = u8::from_str_radix(&bits[i..i + 8], 2)
            .map_err(|_| format!("invalid binary digit at position {i}"))?;
        result.push(byte as char);
    }
    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_say() {
        let mut lexer = Lexer::new("say 'Hello, World!'");
        let tokens = lexer.tokenize().unwrap();
        assert!(matches!(&tokens[0].kind, TokenKind::Symbol(s) if s == "say"));
        assert!(matches!(&tokens[1].kind, TokenKind::StringLit(s) if s == "Hello, World!"));
        assert!(matches!(&tokens[2].kind, TokenKind::Eof));
    }

    #[test]
    fn arithmetic_tokens() {
        let mut lexer = Lexer::new("3 + 4 * 2");
        let tokens = lexer.tokenize().unwrap();
        assert!(matches!(&tokens[0].kind, TokenKind::Number(n) if n == "3"));
        assert!(matches!(&tokens[1].kind, TokenKind::Plus));
        assert!(matches!(&tokens[2].kind, TokenKind::Number(n) if n == "4"));
        assert!(matches!(&tokens[3].kind, TokenKind::Star));
        assert!(matches!(&tokens[4].kind, TokenKind::Number(n) if n == "2"));
    }

    #[test]
    fn nested_comments() {
        let mut lexer = Lexer::new("/* outer /* inner */ still comment */ say 'hi'");
        let tokens = lexer.tokenize().unwrap();
        assert!(matches!(&tokens[0].kind, TokenKind::Symbol(s) if s == "say"));
    }

    #[test]
    fn hex_string() {
        let mut lexer = Lexer::new("'48656C6C6F'x");
        let tokens = lexer.tokenize().unwrap();
        assert!(matches!(&tokens[0].kind, TokenKind::StringLit(s) if s == "Hello"));
    }

    #[test]
    fn doubled_quote_escape() {
        let mut lexer = Lexer::new("'it''s'");
        let tokens = lexer.tokenize().unwrap();
        assert!(matches!(&tokens[0].kind, TokenKind::StringLit(s) if s == "it's"));
    }

    #[test]
    fn comparison_operators() {
        let mut lexer = Lexer::new("a == b \\= c >> d");
        let tokens = lexer.tokenize().unwrap();
        assert!(matches!(&tokens[1].kind, TokenKind::StrictEq));
        assert!(matches!(&tokens[3].kind, TokenKind::NotEqual));
        assert!(matches!(&tokens[5].kind, TokenKind::StrictGt));
    }
}
