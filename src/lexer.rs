//! Lexer (tokenizer) for Tiny ML
//!
//! Converts source code into a stream of tokens for the parser.
//! Handles:
//! - Keywords: `let`, `rec`, `in`, `if`, `then`, `else`, `match`, `with`, `fun`, etc.
//! - Literals: integers, decimals, booleans, strings
//! - Operators: arithmetic, comparison, logical
//! - Comments: OCaml-style `(* ... *)` with nesting
//!
//! # Usage
//!
//! ```ignore
//! let mut lexer = Lexer::new("let x = 42");
//! let tokens = lexer.tokenize();
//! ```

use crate::token::{Token, Span, SpannedToken};

/// Lexer for tokenizing Tiny ML source code.
///
/// Scans the input string and produces a sequence of tokens with
/// source location information (spans).
pub struct Lexer<'a> {
    /// The complete source input
    #[allow(dead_code)]
    input: &'a str,
    /// Character iterator with byte positions
    chars: std::iter::Peekable<std::str::CharIndices<'a>>,
    /// Current byte position in input
    pos: usize,
    /// Current line number (1-indexed)
    line: usize,
    /// Current column number (1-indexed)
    column: usize,
}

impl<'a> Lexer<'a> {
    /// Create a new lexer for the given source code
    pub fn new(input: &'a str) -> Self {
        Lexer {
            input,
            chars: input.char_indices().peekable(),
            pos: 0,
            line: 1,
            column: 1,
        }
    }

    fn peek_char(&mut self) -> Option<char> {
        self.chars.peek().map(|(_, c)| *c)
    }

    fn next_char(&mut self) -> Option<char> {
        if let Some((pos, c)) = self.chars.next() {
            self.pos = pos + c.len_utf8();
            if c == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
            Some(c)
        } else {
            None
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek_char() {
            if c.is_whitespace() {
                self.next_char();
            } else if c == '(' {
                // Check for comment (* ... *)
                // Peek ahead without consuming
                let mut peek_iter = self.chars.clone();
                peek_iter.next(); // skip the '('
                if peek_iter.peek().map(|(_, c)| *c) == Some('*') {
                    // It's a comment, consume '(' and '*' and skip the comment
                    self.next_char(); // consume '('
                    self.next_char(); // consume '*'
                    self.skip_comment();
                } else {
                    // Not a comment, stop skipping whitespace
                    break;
                }
            } else {
                break;
            }
        }
    }

    fn skip_comment(&mut self) {
        // Skip until we find *)
        let mut depth = 1;
        while depth > 0 {
            match self.next_char() {
                Some('(') if self.peek_char() == Some('*') => {
                    self.next_char();
                    depth += 1;
                }
                Some('*') if self.peek_char() == Some(')') => {
                    self.next_char();
                    depth -= 1;
                }
                None => break,
                _ => {}
            }
        }
    }

    fn read_number(&mut self, first: char) -> Token {
        let mut s = String::new();
        s.push(first);
        let mut has_dot = false;

        while let Some(c) = self.peek_char() {
            if c.is_ascii_digit() {
                s.push(c);
                self.next_char();
            } else if c == '.' && !has_dot {
                // Check if next char is also a digit (to distinguish from 1.method)
                // Peek ahead without consuming
                let mut peek_iter = self.chars.clone();
                peek_iter.next(); // skip the '.'
                if let Some((_, next)) = peek_iter.peek() {
                    if next.is_ascii_digit() {
                        // It's a decimal number, consume the '.'
                        self.next_char();
                        s.push('.');
                        has_dot = true;
                        continue;
                    }
                }
                // Not a decimal, stop reading
                break;
            } else {
                break;
            }
        }

        if has_dot {
            Token::Decimal(s)
        } else {
            Token::Int(s.parse().unwrap_or(0))
        }
    }

    fn read_ident(&mut self, first: char) -> Token {
        let mut s = String::new();
        s.push(first);

        while let Some(c) = self.peek_char() {
            if c.is_alphanumeric() || c == '_' || c == '\'' {
                s.push(c);
                self.next_char();
            } else {
                break;
            }
        }

        Token::keyword_or_ident(&s)
    }

    fn read_string(&mut self) -> Token {
        let mut s = String::new();

        loop {
            match self.next_char() {
                None => break, // Unterminated string
                Some('"') => break, // End of string
                Some('\\') => {
                    // Escape sequence
                    match self.next_char() {
                        Some('n') => s.push('\n'),
                        Some('t') => s.push('\t'),
                        Some('r') => s.push('\r'),
                        Some('\\') => s.push('\\'),
                        Some('"') => s.push('"'),
                        Some('0') => s.push('\0'),
                        Some(c) => s.push(c), // Unknown escape, keep literal
                        None => break,
                    }
                }
                Some(c) => s.push(c),
            }
        }

        Token::String(s)
    }

    pub fn next_token(&mut self) -> SpannedToken {
        self.skip_whitespace();

        let start = self.pos;
        let line = self.line;
        let column = self.column;

        let token = match self.next_char() {
            None => Token::Eof,
            Some(c) => match c {
                '+' => Token::Plus,
                '*' => Token::Star,
                '/' => Token::Slash,
                '%' => Token::Percent,
                '(' => Token::LParen,
                ')' => Token::RParen,
                '|' => {
                    if self.peek_char() == Some('|') {
                        self.next_char();
                        Token::Or
                    } else {
                        Token::Pipe
                    }
                }
                ':' => Token::Colon,
                ',' => Token::Comma,
                '_' => {
                    if self.peek_char().map_or(true, |c| !c.is_alphanumeric()) {
                        Token::Underscore
                    } else {
                        self.read_ident('_')
                    }
                }
                '-' => {
                    if self.peek_char() == Some('>') {
                        self.next_char();
                        Token::Arrow
                    } else {
                        Token::Minus
                    }
                }
                '=' => {
                    if self.peek_char() == Some('=') {
                        self.next_char();
                        Token::EqEq
                    } else {
                        Token::Eq
                    }
                }
                '<' => {
                    match self.peek_char() {
                        Some('>') => {
                            self.next_char();
                            Token::NotEq
                        }
                        Some('=') => {
                            self.next_char();
                            Token::Le
                        }
                        _ => Token::Lt,
                    }
                }
                '>' => {
                    if self.peek_char() == Some('=') {
                        self.next_char();
                        Token::Ge
                    } else {
                        Token::Gt
                    }
                }
                '&' => {
                    if self.peek_char() == Some('&') {
                        self.next_char();
                        Token::And
                    } else {
                        // Single & not used, treat as error or ident
                        Token::Ident("&".to_string())
                    }
                }
                '"' => self.read_string(),
                '.' => {
                    if self.peek_char() == Some('.') {
                        self.next_char();
                        Token::DotDot
                    } else {
                        // Single dot not used, treat as unknown
                        Token::Ident(".".to_string())
                    }
                }
                c if c.is_ascii_digit() => self.read_number(c),
                c if c.is_alphabetic() || c == '_' => self.read_ident(c),
                c => {
                    // Unknown character
                    Token::Ident(c.to_string())
                }
            },
        };

        SpannedToken {
            token,
            span: Span {
                start,
                end: self.pos,
                line,
                column,
            },
        }
    }

    pub fn tokenize(&mut self) -> Vec<SpannedToken> {
        let mut tokens = Vec::new();
        loop {
            let tok = self.next_token();
            let is_eof = tok.token == Token::Eof;
            tokens.push(tok);
            if is_eof {
                break;
            }
        }
        tokens
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_tokens() {
        let mut lexer = Lexer::new("let x = 42");
        assert!(matches!(lexer.next_token().token, Token::Let));
        assert!(matches!(lexer.next_token().token, Token::Ident(s) if s == "x"));
        assert!(matches!(lexer.next_token().token, Token::Eq));
        assert!(matches!(lexer.next_token().token, Token::Int(42)));
    }

    #[test]
    fn test_decimal_literal() {
        let mut lexer = Lexer::new("3.14");
        assert!(matches!(lexer.next_token().token, Token::Decimal(s) if s == "3.14"));
    }

    #[test]
    fn test_keywords() {
        let mut lexer = Lexer::new("let rec if then else match with");
        assert!(matches!(lexer.next_token().token, Token::Let));
        assert!(matches!(lexer.next_token().token, Token::Rec));
        assert!(matches!(lexer.next_token().token, Token::If));
        assert!(matches!(lexer.next_token().token, Token::Then));
        assert!(matches!(lexer.next_token().token, Token::Else));
        assert!(matches!(lexer.next_token().token, Token::Match));
        assert!(matches!(lexer.next_token().token, Token::With));
    }

    #[test]
    fn test_comment() {
        let mut lexer = Lexer::new("let (* this is a comment *) x = 5");
        assert!(matches!(lexer.next_token().token, Token::Let));
        assert!(matches!(lexer.next_token().token, Token::Ident(s) if s == "x"));
    }
}
