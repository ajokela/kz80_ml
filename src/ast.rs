//! Abstract Syntax Tree for Tiny ML
//!
//! This module defines the core data structures representing parsed Tiny ML programs.
//! The AST is produced by the parser and consumed by the type checker and code generator.
//!
//! # Structure
//!
//! - [`Program`] - A complete program (sequence of bindings)
//! - [`Binding`] - Top-level value or function definition
//! - [`Expr`] - Expression nodes (literals, operations, control flow, etc.)
//! - [`Pattern`] - Patterns for match expressions
//! - [`BinOp`] / [`UnaryOp`] - Operators

use crate::types::Type;

/// A complete Tiny ML program consisting of top-level bindings.
///
/// Programs are executed by evaluating bindings in order. The last binding
/// named `main` is typically the entry point.
///
/// # Example
///
/// ```text
/// let square x = x * x
/// let main = print_int (square 5)
/// ```
#[derive(Debug, Clone)]
pub struct Program {
    /// Top-level bindings in declaration order
    pub bindings: Vec<Binding>,
}

/// A top-level binding: `let x = e` or `let rec f x = e`
///
/// Bindings can be simple values or functions (with parameters).
/// Recursive bindings (`let rec`) allow the bound name to be used
/// within its own definition.
#[derive(Debug, Clone)]
pub struct Binding {
    /// Name being bound
    pub name: String,
    /// Function parameters (empty for simple value bindings)
    pub params: Vec<Param>,
    /// The expression being bound
    pub body: Expr,
    /// Whether this is a recursive binding (`let rec`)
    pub recursive: bool,
    /// Inferred type (filled in by type inference)
    #[allow(dead_code)]
    pub ty: Option<Type>,
}

/// Function parameter with optional type annotation.
///
/// Parameters can have explicit type annotations: `(x : int)`
#[derive(Debug, Clone)]
pub struct Param {
    /// Parameter name
    pub name: String,
    /// Optional type annotation
    pub ty: Option<TypeAnnotation>,
}

/// User-provided type annotation in source code.
///
/// Used for explicit parameter types: `let f (x : int) = ...`
#[derive(Debug, Clone)]
pub enum TypeAnnotation {
    /// Integer type
    Int,
    /// Boolean type
    Bool,
    /// Decimal (BCD) type
    Decimal,
    /// Function type: `a -> b`
    Fun(Box<TypeAnnotation>, Box<TypeAnnotation>),
}

/// Expression nodes in the AST.
///
/// Expressions are the core computational elements of the language.
/// Every expression evaluates to a value.
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum Expr {
    // Literals
    Int(i32),
    Decimal(String),
    Bool(bool),
    String(String),

    // Variable reference
    Var(String),

    // Binary operations
    BinOp(BinOp, Box<Expr>, Box<Expr>),

    // Unary operations
    UnaryOp(UnaryOp, Box<Expr>),

    // Function application: f x
    App(Box<Expr>, Box<Expr>),

    // Conditional: if e1 then e2 else e3
    If(Box<Expr>, Box<Expr>, Box<Expr>),

    // Local binding: let x = e1 in e2
    Let {
        name: String,
        recursive: bool,
        params: Vec<Param>,
        value: Box<Expr>,
        body: Box<Expr>,
    },

    // Pattern match on integers
    Match {
        scrutinee: Box<Expr>,
        arms: Vec<MatchArm>,
    },

    // Type conversion: int e (decimal -> int) or decimal e (int -> decimal)
    ToInt(Box<Expr>),
    ToDecimal(Box<Expr>),

    // Sequence: e1; e2 (evaluate e1 for side effect, return e2)
    Seq(Box<Expr>, Box<Expr>),

    // Tuple: (e1, e2, ...)
    Tuple(Vec<Expr>),

    // Annotated expression with inferred type (filled in by type checker)
    Typed(Box<Expr>, Type),

    // Lambda expression: fun x y -> expr
    Lambda {
        params: Vec<Param>,
        body: Box<Expr>,
    },

    // Option type constructors
    None,                    // None value
    Some(Box<Expr>),         // Some(value)
}

/// Binary operators for arithmetic, comparison, and logical operations.
///
/// These operators work on two operands and produce a result value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    // Arithmetic (work on Int or Decimal)
    /// Addition: `a + b`
    Add,
    /// Subtraction: `a - b`
    Sub,
    /// Multiplication: `a * b`
    Mul,
    /// Division: `a / b`
    Div,
    /// Modulo: `a % b`
    Mod,

    // Comparison (produce Bool)
    /// Equality: `a == b`
    Eq,
    /// Inequality: `a <> b`
    NotEq,
    /// Less than: `a < b`
    Lt,
    /// Greater than: `a > b`
    Gt,
    /// Less than or equal: `a <= b`
    Le,
    /// Greater than or equal: `a >= b`
    Ge,

    // Logical (work on Bool)
    /// Logical and: `a && b`
    And,
    /// Logical or: `a || b`
    Or,
}

impl BinOp {
    /// Returns true if this is an arithmetic operator (+, -, *, /, %)
    pub fn is_arithmetic(&self) -> bool {
        matches!(self, BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod)
    }

    /// Returns true if this is a comparison operator (==, <>, <, >, <=, >=)
    pub fn is_comparison(&self) -> bool {
        matches!(self, BinOp::Eq | BinOp::NotEq | BinOp::Lt | BinOp::Gt | BinOp::Le | BinOp::Ge)
    }

    /// Returns true if this is a logical operator (&&, ||)
    pub fn is_logical(&self) -> bool {
        matches!(self, BinOp::And | BinOp::Or)
    }
}

/// Unary operators for negation and logical not.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    /// Numeric negation: `-x`
    Neg,
    /// Logical negation: `not x`
    Not,
}

/// A single arm in a match expression.
///
/// Each arm consists of a pattern, an optional guard condition,
/// and a body expression to evaluate if the pattern matches.
///
/// # Syntax
///
/// ```text
/// | pattern [when guard] -> body
/// ```
#[derive(Debug, Clone)]
pub struct MatchArm {
    /// Pattern to match against the scrutinee
    pub pattern: Pattern,
    /// Optional guard condition (must evaluate to true for arm to match)
    pub guard: Option<Box<Expr>>,
    /// Expression to evaluate if pattern (and guard) match
    pub body: Expr,
}

/// Patterns for destructuring values in match expressions.
///
/// Patterns can match literal values, bind variables, or destructure
/// compound values like tuples and options.
#[derive(Debug, Clone)]
pub enum Pattern {
    /// Match a specific integer literal
    Int(i32),
    /// Match any value and bind it to a name
    Var(String),
    /// Wildcard: matches anything, no binding (`_`)
    Wildcard,
    /// Match the `None` option value
    OptionNone,
    /// Match `Some(x)` and bind the inner value to the name
    OptionSome(String),
    /// Match a tuple and destructure its elements: `(p1, p2, ...)`
    Tuple(Vec<Pattern>),
    /// Match an integer range: `start..end` (inclusive start, exclusive end)
    Range(i32, i32),
}

#[allow(dead_code)]
impl Expr {
    /// Create a simple integer expression
    pub fn int(n: i32) -> Self {
        Expr::Int(n)
    }

    /// Create a decimal expression
    pub fn decimal(s: &str) -> Self {
        Expr::Decimal(s.to_string())
    }

    /// Create a variable reference
    pub fn var(name: &str) -> Self {
        Expr::Var(name.to_string())
    }

    /// Create a binary operation
    pub fn binop(op: BinOp, left: Expr, right: Expr) -> Self {
        Expr::BinOp(op, Box::new(left), Box::new(right))
    }

    /// Create an if expression
    pub fn if_then_else(cond: Expr, then_branch: Expr, else_branch: Expr) -> Self {
        Expr::If(Box::new(cond), Box::new(then_branch), Box::new(else_branch))
    }

    /// Create a function application
    pub fn app(func: Expr, arg: Expr) -> Self {
        Expr::App(Box::new(func), Box::new(arg))
    }

    /// Compute free variables in an expression
    /// Returns variable names that are used but not bound in this expression
    pub fn free_vars(&self, bound: &std::collections::HashSet<String>) -> std::collections::HashSet<String> {
        use std::collections::HashSet;

        match self {
            Expr::Int(_) | Expr::Bool(_) | Expr::Decimal(_) | Expr::String(_) | Expr::None => HashSet::new(),

            Expr::Var(name) => {
                if bound.contains(name) {
                    HashSet::new()
                } else {
                    let mut set = HashSet::new();
                    set.insert(name.clone());
                    set
                }
            }

            Expr::BinOp(_, left, right) => {
                let mut fv = left.free_vars(bound);
                fv.extend(right.free_vars(bound));
                fv
            }

            Expr::UnaryOp(_, inner) => inner.free_vars(bound),

            Expr::App(func, arg) => {
                let mut fv = func.free_vars(bound);
                fv.extend(arg.free_vars(bound));
                fv
            }

            Expr::If(cond, then_br, else_br) => {
                let mut fv = cond.free_vars(bound);
                fv.extend(then_br.free_vars(bound));
                fv.extend(else_br.free_vars(bound));
                fv
            }

            Expr::Let { name, params, value, body, .. } => {
                // Value is evaluated in current scope (with params bound for the value)
                let mut value_bound = bound.clone();
                for p in params {
                    value_bound.insert(p.name.clone());
                }
                let mut fv = value.free_vars(&value_bound);

                // Body has the let-bound name in scope
                let mut body_bound = bound.clone();
                body_bound.insert(name.clone());
                fv.extend(body.free_vars(&body_bound));
                fv
            }

            Expr::Lambda { params, body } => {
                let mut inner_bound = bound.clone();
                for p in params {
                    inner_bound.insert(p.name.clone());
                }
                body.free_vars(&inner_bound)
            }

            Expr::Match { scrutinee, arms } => {
                let mut fv = scrutinee.free_vars(bound);
                for arm in arms {
                    let mut arm_bound = bound.clone();
                    arm.pattern.bound_vars(&mut arm_bound);
                    fv.extend(arm.body.free_vars(&arm_bound));
                }
                fv
            }

            Expr::Seq(first, second) => {
                let mut fv = first.free_vars(bound);
                fv.extend(second.free_vars(bound));
                fv
            }

            Expr::Tuple(exprs) => {
                let mut fv = HashSet::new();
                for e in exprs {
                    fv.extend(e.free_vars(bound));
                }
                fv
            }

            Expr::Some(inner) => inner.free_vars(bound),
            Expr::ToInt(inner) => inner.free_vars(bound),
            Expr::ToDecimal(inner) => inner.free_vars(bound),
            Expr::Typed(inner, _) => inner.free_vars(bound),
        }
    }
}

impl Pattern {
    /// Add variables bound by this pattern to the set
    pub fn bound_vars(&self, bound: &mut std::collections::HashSet<String>) {
        match self {
            Pattern::Var(name) => { bound.insert(name.clone()); }
            Pattern::OptionSome(name) => { bound.insert(name.clone()); }
            Pattern::Tuple(patterns) => {
                for p in patterns {
                    p.bound_vars(bound);
                }
            }
            Pattern::Int(_) | Pattern::Wildcard | Pattern::OptionNone | Pattern::Range(_, _) => {}
        }
    }
}
