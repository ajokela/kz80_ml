//! Token definitions for Tiny ML
//!
//! Defines the token types produced by the lexer and consumed by the parser.
//! Each token carries source location information via [`Span`].

/// Token types in the Tiny ML language.
///
/// Tokens are the atomic units of syntax produced by the lexer.
#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
pub enum Token {
    // Literals
    /// Integer literal (e.g., `42`, `-7`)
    Int(i32),
    /// Decimal literal stored as string (e.g., `"3.14"`)
    Decimal(String),
    /// Boolean literal (`true` or `false`)
    Bool(bool),
    /// String literal (e.g., `"hello"`)
    String(String),
    /// Identifier (variable or function name)
    Ident(String),

    // Keywords
    /// `let` keyword for bindings
    Let,
    /// `rec` keyword for recursive bindings
    Rec,
    /// `in` keyword for let expressions
    In,
    /// `if` keyword
    If,
    /// `then` keyword
    Then,
    /// `else` keyword
    Else,
    /// `match` keyword for pattern matching
    Match,
    /// `with` keyword in match expressions
    With,
    /// `when` keyword for pattern guards
    When,
    /// `fun` keyword for lambda expressions
    Fun,
    /// `true` boolean literal
    True,
    /// `false` boolean literal
    False,
    /// `None` option constructor
    None,
    /// `Some` option constructor
    Some,

    // Type keywords
    /// `int` type annotation
    IntType,
    /// `bool` type annotation
    BoolType,
    /// `decimal` type annotation
    DecimalType,

    // Operators
    /// `+` addition
    Plus,
    /// `-` subtraction or negation
    Minus,
    /// `*` multiplication
    Star,
    /// `/` division
    Slash,
    /// `%` modulo
    Percent,
    /// `=` assignment/binding
    Eq,
    /// `==` equality comparison
    EqEq,
    /// `<>` inequality
    NotEq,
    /// `<` less than
    Lt,
    /// `>` greater than
    Gt,
    /// `<=` less than or equal
    Le,
    /// `>=` greater than or equal
    Ge,
    /// `&&` logical and
    And,
    /// `||` logical or
    Or,
    /// `not` logical negation
    Not,

    // Delimiters
    /// `(` left parenthesis
    LParen,
    /// `)` right parenthesis
    RParen,
    /// `->` function arrow
    Arrow,
    /// `|` pattern separator
    Pipe,
    /// `:` type annotation separator
    Colon,
    /// `,` tuple element separator
    Comma,
    /// `..` range pattern separator
    DotDot,
    /// `_` wildcard pattern
    Underscore,

    // Special
    /// End of file marker
    Eof,
}

impl Token {
    pub fn keyword_or_ident(s: &str) -> Token {
        match s {
            "let" => Token::Let,
            "rec" => Token::Rec,
            "in" => Token::In,
            "if" => Token::If,
            "then" => Token::Then,
            "else" => Token::Else,
            "match" => Token::Match,
            "with" => Token::With,
            "when" => Token::When,
            "fun" => Token::Fun,
            "true" => Token::Bool(true),
            "false" => Token::Bool(false),
            "not" => Token::Not,
            "None" => Token::None,
            "Some" => Token::Some,
            "int" => Token::IntType,
            "bool" => Token::BoolType,
            "decimal" => Token::DecimalType,
            _ => Token::Ident(s.to_string()),
        }
    }
}

/// Source location span for error reporting.
///
/// Tracks the byte range and line/column of a token in the source code.
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct Span {
    /// Start byte offset in source
    pub start: usize,
    /// End byte offset in source (exclusive)
    pub end: usize,
    /// Line number (1-indexed)
    pub line: usize,
    /// Column number (1-indexed)
    pub column: usize,
}

/// A token paired with its source location.
///
/// Used throughout the parser to provide accurate error messages.
#[derive(Debug, Clone)]
pub struct SpannedToken {
    /// The token value
    pub token: Token,
    /// Source location of the token
    pub span: Span,
}
