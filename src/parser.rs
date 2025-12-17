//! Recursive descent parser for Tiny ML
//!
//! Converts a token stream into an abstract syntax tree (AST).
//! The parser handles:
//! - Top-level bindings (`let`, `let rec`)
//! - Expressions (arithmetic, conditionals, match, lambdas)
//! - Patterns for match expressions
//! - Type annotations on parameters
//!
//! # Usage
//!
//! ```ignore
//! let mut lexer = Lexer::new(source);
//! let tokens = lexer.tokenize();
//! let mut parser = Parser::new(tokens);
//! let program = parser.parse_program()?;
//! ```

use crate::token::{Token, SpannedToken};
use crate::ast::*;

/// Recursive descent parser for Tiny ML source code.
///
/// The parser consumes a sequence of tokens and produces an AST.
/// It uses recursive descent with operator precedence parsing for expressions.
pub struct Parser {
    /// Token stream from lexer
    tokens: Vec<SpannedToken>,
    /// Current position in token stream
    pos: usize,
}

/// Error returned when parsing fails.
///
/// Contains the error message and source location for diagnostics.
#[derive(Debug)]
pub struct ParseError {
    /// Description of the parse error
    pub message: String,
    /// Line number (1-indexed)
    pub line: usize,
    /// Column number (1-indexed)
    pub column: usize,
}

impl ParseError {
    /// Create a new parse error with location information
    pub fn new(message: impl Into<String>, line: usize, column: usize) -> Self {
        ParseError {
            message: message.into(),
            line,
            column,
        }
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Parse error at {}:{}: {}", self.line, self.column, self.message)
    }
}

impl Parser {
    /// Create a new parser from a token stream
    pub fn new(tokens: Vec<SpannedToken>) -> Self {
        Parser { tokens, pos: 0 }
    }

    fn peek(&self) -> &Token {
        self.tokens.get(self.pos).map(|t| &t.token).unwrap_or(&Token::Eof)
    }

    fn peek_span(&self) -> (usize, usize) {
        self.tokens
            .get(self.pos)
            .map(|t| (t.span.line, t.span.column))
            .unwrap_or((0, 0))
    }

    fn advance(&mut self) -> &Token {
        let tok = self.peek();
        if !matches!(tok, Token::Eof) {
            self.pos += 1;
        }
        self.tokens.get(self.pos - 1).map(|t| &t.token).unwrap_or(&Token::Eof)
    }

    fn expect(&mut self, expected: Token) -> Result<(), ParseError> {
        let (line, col) = self.peek_span();
        if self.peek() == &expected {
            self.advance();
            Ok(())
        } else {
            Err(ParseError::new(
                format!("Expected {:?}, got {:?}", expected, self.peek()),
                line,
                col,
            ))
        }
    }

    fn check(&self, tok: &Token) -> bool {
        self.peek() == tok
    }

    fn match_token(&mut self, tok: &Token) -> bool {
        if self.check(tok) {
            self.advance();
            true
        } else {
            false
        }
    }

    /// Parse a complete program
    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut bindings = Vec::new();

        while !self.check(&Token::Eof) {
            bindings.push(self.parse_binding()?);
        }

        Ok(Program { bindings })
    }

    /// Parse a top-level binding: let [rec] name [params] = expr
    fn parse_binding(&mut self) -> Result<Binding, ParseError> {
        self.expect(Token::Let)?;

        let recursive = self.match_token(&Token::Rec);

        let (line, col) = self.peek_span();
        let name = match self.advance() {
            Token::Ident(s) => s.clone(),
            _ => return Err(ParseError::new("Expected identifier", line, col)),
        };

        let params = self.parse_params()?;

        self.expect(Token::Eq)?;

        let body = self.parse_expr()?;

        Ok(Binding {
            name,
            params,
            body,
            recursive,
            ty: None,
        })
    }

    /// Parse function parameters
    fn parse_params(&mut self) -> Result<Vec<Param>, ParseError> {
        let mut params = Vec::new();

        while let Token::Ident(name) = self.peek().clone() {
            self.advance();

            // Check for type annotation: (name : type)
            let ty = if self.match_token(&Token::Colon) {
                Some(self.parse_type_annotation()?)
            } else {
                None
            };

            params.push(Param {
                name,
                ty,
            });
        }

        // Also handle parenthesized params: (x : int)
        while self.check(&Token::LParen) {
            self.advance();
            let (line, col) = self.peek_span();
            let name = match self.advance() {
                Token::Ident(s) => s.clone(),
                _ => return Err(ParseError::new("Expected parameter name", line, col)),
            };

            let ty = if self.match_token(&Token::Colon) {
                Some(self.parse_type_annotation()?)
            } else {
                None
            };

            self.expect(Token::RParen)?;
            params.push(Param { name, ty });
        }

        Ok(params)
    }

    /// Parse type annotation
    fn parse_type_annotation(&mut self) -> Result<TypeAnnotation, ParseError> {
        let (line, col) = self.peek_span();
        let base = match self.peek() {
            Token::IntType => {
                self.advance();
                TypeAnnotation::Int
            }
            Token::BoolType => {
                self.advance();
                TypeAnnotation::Bool
            }
            Token::DecimalType => {
                self.advance();
                TypeAnnotation::Decimal
            }
            Token::LParen => {
                self.advance();
                let inner = self.parse_type_annotation()?;
                self.expect(Token::RParen)?;
                inner
            }
            _ => return Err(ParseError::new("Expected type", line, col)),
        };

        // Check for function type: t1 -> t2
        if self.match_token(&Token::Arrow) {
            let ret = self.parse_type_annotation()?;
            Ok(TypeAnnotation::Fun(Box::new(base), Box::new(ret)))
        } else {
            Ok(base)
        }
    }

    /// Parse expression (entry point for expressions)
    pub fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_let_expr()
    }

    /// Parse let expression or pass to next level
    fn parse_let_expr(&mut self) -> Result<Expr, ParseError> {
        if self.check(&Token::Let) {
            self.advance();
            let recursive = self.match_token(&Token::Rec);

            let (line, col) = self.peek_span();
            let name = match self.advance() {
                Token::Ident(s) => s.clone(),
                _ => return Err(ParseError::new("Expected identifier", line, col)),
            };

            let params = self.parse_params()?;
            self.expect(Token::Eq)?;
            let value = self.parse_expr()?;
            self.expect(Token::In)?;
            let body = self.parse_expr()?;

            Ok(Expr::Let {
                name,
                recursive,
                params,
                value: Box::new(value),
                body: Box::new(body),
            })
        } else if self.check(&Token::Fun) {
            self.parse_lambda()
        } else if self.check(&Token::If) {
            self.parse_if_expr()
        } else if self.check(&Token::Match) {
            self.parse_match_expr()
        } else {
            self.parse_or_expr()
        }
    }

    /// Parse lambda expression: fun x y -> expr
    fn parse_lambda(&mut self) -> Result<Expr, ParseError> {
        self.expect(Token::Fun)?;

        let params = self.parse_lambda_params()?;

        if params.is_empty() {
            let (line, col) = self.peek_span();
            return Err(ParseError::new("Lambda requires at least one parameter", line, col));
        }

        self.expect(Token::Arrow)?;

        let body = self.parse_expr()?;

        Ok(Expr::Lambda {
            params,
            body: Box::new(body),
        })
    }

    /// Parse lambda parameters (identifiers until ->)
    fn parse_lambda_params(&mut self) -> Result<Vec<Param>, ParseError> {
        let mut params = Vec::new();

        // Parse simple identifiers until we hit ->
        while let Token::Ident(name) = self.peek().clone() {
            self.advance();
            params.push(Param { name, ty: None });
        }

        // Also handle parenthesized params with type annotations: (x : int)
        while self.check(&Token::LParen) {
            // Peek ahead to see if this is a type-annotated param or just a grouped expr
            let mut peek_iter = self.tokens[self.pos..].iter();
            peek_iter.next(); // skip LParen
            if let Some(t) = peek_iter.next() {
                if let Token::Ident(_) = t.token {
                    if let Some(t2) = peek_iter.next() {
                        if t2.token == Token::Colon {
                            // This is a type-annotated parameter
                            self.advance(); // consume LParen
                            let (line, col) = self.peek_span();
                            let name = match self.advance() {
                                Token::Ident(s) => s.clone(),
                                _ => return Err(ParseError::new("Expected parameter name", line, col)),
                            };
                            self.expect(Token::Colon)?;
                            let ty = Some(self.parse_type_annotation()?);
                            self.expect(Token::RParen)?;
                            params.push(Param { name, ty });
                            continue;
                        }
                    }
                }
            }
            // Not a type-annotated param, stop parsing params
            break;
        }

        Ok(params)
    }

    /// Parse if expression
    fn parse_if_expr(&mut self) -> Result<Expr, ParseError> {
        self.expect(Token::If)?;
        let cond = self.parse_expr()?;
        self.expect(Token::Then)?;
        let then_branch = self.parse_expr()?;
        self.expect(Token::Else)?;
        let else_branch = self.parse_expr()?;

        Ok(Expr::If(
            Box::new(cond),
            Box::new(then_branch),
            Box::new(else_branch),
        ))
    }

    /// Parse match expression
    fn parse_match_expr(&mut self) -> Result<Expr, ParseError> {
        self.expect(Token::Match)?;
        let scrutinee = self.parse_expr()?;
        self.expect(Token::With)?;

        let mut arms = Vec::new();

        // Optional leading pipe
        self.match_token(&Token::Pipe);

        loop {
            let pattern = self.parse_pattern()?;

            // Parse optional guard: when <expr>
            let guard = if self.match_token(&Token::When) {
                Some(Box::new(self.parse_comparison()?))
            } else {
                None
            };

            self.expect(Token::Arrow)?;
            let body = self.parse_expr()?;
            arms.push(MatchArm { pattern, guard, body });

            if !self.match_token(&Token::Pipe) {
                break;
            }
        }

        Ok(Expr::Match {
            scrutinee: Box::new(scrutinee),
            arms,
        })
    }

    /// Parse pattern
    fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
        let (line, col) = self.peek_span();
        match self.peek().clone() {
            Token::Int(n) => {
                self.advance();
                // Check for range pattern: start..end
                if self.match_token(&Token::DotDot) {
                    let (line2, col2) = self.peek_span();
                    if let Token::Int(end) = self.peek().clone() {
                        self.advance();
                        Ok(Pattern::Range(n, end))
                    } else {
                        Err(ParseError::new("Expected integer after '..'", line2, col2))
                    }
                } else {
                    Ok(Pattern::Int(n))
                }
            }
            Token::Ident(name) => {
                self.advance();
                Ok(Pattern::Var(name))
            }
            Token::Underscore => {
                self.advance();
                Ok(Pattern::Wildcard)
            }
            Token::None => {
                self.advance();
                Ok(Pattern::OptionNone)
            }
            Token::Some => {
                self.advance();
                // Some(x) pattern - requires a variable binding
                self.expect(Token::LParen)?;
                let (line2, col2) = self.peek_span();
                if let Token::Ident(name) = self.peek().clone() {
                    self.advance();
                    self.expect(Token::RParen)?;
                    Ok(Pattern::OptionSome(name))
                } else {
                    Err(ParseError::new("Expected variable name in Some pattern", line2, col2))
                }
            }
            Token::LParen => {
                self.advance();
                // Tuple pattern: (p1, p2, ...)
                let first = self.parse_pattern()?;
                if self.check(&Token::Comma) {
                    let mut patterns = vec![first];
                    while self.match_token(&Token::Comma) {
                        patterns.push(self.parse_pattern()?);
                    }
                    self.expect(Token::RParen)?;
                    Ok(Pattern::Tuple(patterns))
                } else {
                    // Just a parenthesized pattern
                    self.expect(Token::RParen)?;
                    Ok(first)
                }
            }
            _ => Err(ParseError::new("Expected pattern", line, col)),
        }
    }

    /// Parse logical or: e1 || e2
    fn parse_or_expr(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_and_expr()?;

        while self.match_token(&Token::Or) {
            let right = self.parse_and_expr()?;
            left = Expr::BinOp(BinOp::Or, Box::new(left), Box::new(right));
        }

        Ok(left)
    }

    /// Parse logical and: e1 && e2
    fn parse_and_expr(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_comparison()?;

        while self.match_token(&Token::And) {
            let right = self.parse_comparison()?;
            left = Expr::BinOp(BinOp::And, Box::new(left), Box::new(right));
        }

        Ok(left)
    }

    /// Parse comparison: e1 < e2, e1 == e2, etc.
    fn parse_comparison(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_additive()?;

        loop {
            let op = match self.peek() {
                Token::EqEq => BinOp::Eq,
                Token::NotEq => BinOp::NotEq,
                Token::Lt => BinOp::Lt,
                Token::Gt => BinOp::Gt,
                Token::Le => BinOp::Le,
                Token::Ge => BinOp::Ge,
                _ => break,
            };
            self.advance();
            let right = self.parse_additive()?;
            left = Expr::BinOp(op, Box::new(left), Box::new(right));
        }

        Ok(left)
    }

    /// Parse additive: e1 + e2, e1 - e2
    fn parse_additive(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_multiplicative()?;

        loop {
            let op = match self.peek() {
                Token::Plus => BinOp::Add,
                Token::Minus => BinOp::Sub,
                _ => break,
            };
            self.advance();
            let right = self.parse_multiplicative()?;
            left = Expr::BinOp(op, Box::new(left), Box::new(right));
        }

        Ok(left)
    }

    /// Parse multiplicative: e1 * e2, e1 / e2, e1 % e2
    fn parse_multiplicative(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_unary()?;

        loop {
            let op = match self.peek() {
                Token::Star => BinOp::Mul,
                Token::Slash => BinOp::Div,
                Token::Percent => BinOp::Mod,
                _ => break,
            };
            self.advance();
            let right = self.parse_unary()?;
            left = Expr::BinOp(op, Box::new(left), Box::new(right));
        }

        Ok(left)
    }

    /// Parse unary: -e, not e
    fn parse_unary(&mut self) -> Result<Expr, ParseError> {
        if self.match_token(&Token::Minus) {
            let expr = self.parse_unary()?;
            return Ok(Expr::UnaryOp(UnaryOp::Neg, Box::new(expr)));
        }

        if self.match_token(&Token::Not) {
            let expr = self.parse_unary()?;
            return Ok(Expr::UnaryOp(UnaryOp::Not, Box::new(expr)));
        }

        // Type conversions: int e, decimal e
        if self.match_token(&Token::IntType) {
            let expr = self.parse_application()?;
            return Ok(Expr::ToInt(Box::new(expr)));
        }

        if self.match_token(&Token::DecimalType) {
            let expr = self.parse_application()?;
            return Ok(Expr::ToDecimal(Box::new(expr)));
        }

        self.parse_application()
    }

    /// Parse function application: f x y
    fn parse_application(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_primary()?;

        // Keep applying arguments as long as we see primary expressions
        while self.is_primary_start() {
            let arg = self.parse_primary()?;
            expr = Expr::App(Box::new(expr), Box::new(arg));
        }

        Ok(expr)
    }

    /// Check if current token can start a primary expression
    fn is_primary_start(&self) -> bool {
        matches!(
            self.peek(),
            Token::Int(_)
                | Token::Decimal(_)
                | Token::Bool(_)
                | Token::String(_)
                | Token::Ident(_)
                | Token::LParen
        )
    }

    /// Parse primary expression: literal, variable, parenthesized
    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        let (line, col) = self.peek_span();

        match self.peek().clone() {
            Token::Int(n) => {
                self.advance();
                Ok(Expr::Int(n))
            }
            Token::Decimal(s) => {
                self.advance();
                Ok(Expr::Decimal(s))
            }
            Token::Bool(b) => {
                self.advance();
                Ok(Expr::Bool(b))
            }
            Token::String(s) => {
                self.advance();
                Ok(Expr::String(s))
            }
            Token::Ident(name) => {
                self.advance();
                Ok(Expr::Var(name))
            }
            Token::None => {
                self.advance();
                Ok(Expr::None)
            }
            Token::Some => {
                self.advance();
                // Some requires a value: Some(expr) or Some expr
                if self.check(&Token::LParen) {
                    self.advance();
                    let value = self.parse_expr()?;
                    self.expect(Token::RParen)?;
                    Ok(Expr::Some(Box::new(value)))
                } else {
                    // Allow Some applied to a simple expression without parens
                    let value = self.parse_primary()?;
                    Ok(Expr::Some(Box::new(value)))
                }
            }
            Token::LParen => {
                self.advance();

                // Check for empty parens or tuple
                if self.match_token(&Token::RParen) {
                    // Unit value - represent as Int 0 for now
                    return Ok(Expr::Int(0));
                }

                let expr = self.parse_expr()?;

                // Check for tuple: (e1, e2, ...)
                if self.check(&Token::Comma) {
                    let mut exprs = vec![expr];
                    while self.match_token(&Token::Comma) {
                        exprs.push(self.parse_expr()?);
                    }
                    self.expect(Token::RParen)?;
                    return Ok(Expr::Tuple(exprs));
                }

                self.expect(Token::RParen)?;
                Ok(expr)
            }
            _ => Err(ParseError::new(
                format!("Unexpected token: {:?}", self.peek()),
                line,
                col,
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    fn parse(input: &str) -> Result<Program, ParseError> {
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        parser.parse_program()
    }

    #[test]
    fn test_parse_simple_binding() {
        let program = parse("let x = 42").unwrap();
        assert_eq!(program.bindings.len(), 1);
        assert_eq!(program.bindings[0].name, "x");
    }

    #[test]
    fn test_parse_function() {
        let program = parse("let square x = x * x").unwrap();
        assert_eq!(program.bindings[0].name, "square");
        assert_eq!(program.bindings[0].params.len(), 1);
    }

    #[test]
    fn test_parse_recursive() {
        let program = parse("let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1)").unwrap();
        assert!(program.bindings[0].recursive);
    }

    #[test]
    fn test_parse_if() {
        let program = parse("let x = if true then 1 else 2").unwrap();
        assert!(matches!(program.bindings[0].body, Expr::If(_, _, _)));
    }

    #[test]
    fn test_parse_decimal() {
        let program = parse("let pi = 3.14159").unwrap();
        assert!(matches!(program.bindings[0].body, Expr::Decimal(_)));
    }

    #[test]
    fn test_parse_lambda() {
        let program = parse("let f = fun x -> x + 1").unwrap();
        assert!(matches!(program.bindings[0].body, Expr::Lambda { .. }));
    }

    #[test]
    fn test_parse_lambda_two_params() {
        let program = parse("let f = fun a b -> a + b").unwrap();
        if let Expr::Lambda { params, .. } = &program.bindings[0].body {
            assert_eq!(params.len(), 2);
            assert_eq!(params[0].name, "a");
            assert_eq!(params[1].name, "b");
        } else {
            panic!("Expected Lambda");
        }
    }

    #[test]
    fn test_parse_lambda_applied() {
        let program = parse("let x = (fun x -> x + 1) 5").unwrap();
        assert!(matches!(program.bindings[0].body, Expr::App(_, _)));
    }
}
