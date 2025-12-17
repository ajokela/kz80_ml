//! Hindley-Milner type inference for Tiny ML
//!
//! This module implements a simplified HM type system with:
//! - Base types: Int, Bool, Decimal, String
//! - Function types with curried application
//! - Compound types: Tuple, Option
//! - Type variables for inference
//! - Automatic int→decimal coercion for mixed arithmetic
//!
//! # Usage
//!
//! ```ignore
//! let mut inference = TypeInference::new();
//! inference.check_program(&program)?;
//! // Type environment now contains inferred types for all bindings
//! ```

use std::collections::HashMap;
use crate::ast::{Expr, BinOp, UnaryOp, Program, Pattern, TypeAnnotation};

/// Types in the Tiny ML type system.
///
/// The type system supports several categories of types:
/// - **Base types**: `Int`, `Bool`, `Decimal`, `String`
/// - **Compound types**: `Fun` (functions), `Tuple`, `Option`
/// - **Type variables**: `Var` (general), `Num` (numeric constraint)
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    /// 16-bit signed integer
    Int,
    /// Boolean (true/false)
    Bool,
    /// 4-byte packed BCD decimal
    Decimal,
    /// Null-terminated string
    String,
    /// Function type: `from -> to`
    Fun(Box<Type>, Box<Type>),
    /// Unconstrained type variable (used during inference)
    Var(usize),
    /// Numeric type variable - resolves to Int or Decimal
    Num(usize),
    /// Option type: `None | Some(inner)`
    Option(Box<Type>),
    /// Tuple type: `(T1, T2, ...)`
    Tuple(Vec<Type>),
}

#[allow(dead_code)]
impl Type {
    /// Returns true if this type is numeric (Int, Decimal, or Num variable)
    pub fn is_numeric(&self) -> bool {
        matches!(self, Type::Int | Type::Decimal | Type::Num(_))
    }

    /// Convenience constructor for function types
    pub fn fun(from: Type, to: Type) -> Type {
        Type::Fun(Box::new(from), Box::new(to))
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Bool => write!(f, "bool"),
            Type::Decimal => write!(f, "decimal"),
            Type::String => write!(f, "string"),
            Type::Fun(a, b) => write!(f, "({} -> {})", a, b),
            Type::Var(n) => write!(f, "?{}", n),
            Type::Num(n) => write!(f, "num?{}", n),
            Type::Option(inner) => write!(f, "{} option", inner),
            Type::Tuple(types) => {
                write!(f, "(")?;
                for (i, t) in types.iter().enumerate() {
                    if i > 0 { write!(f, " * ")?; }
                    write!(f, "{}", t)?;
                }
                write!(f, ")")
            }
        }
    }
}

/// Type error information
#[derive(Debug, Clone)]
pub struct TypeError {
    pub message: String,
}

impl TypeError {
    pub fn new(msg: impl Into<String>) -> Self {
        TypeError { message: msg.into() }
    }
}

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Type error: {}", self.message)
    }
}

/// Substitution: mapping from type variables to types
#[derive(Debug, Clone, Default)]
pub struct Substitution {
    map: HashMap<usize, Type>,
}

#[allow(dead_code)]
impl Substitution {
    pub fn new() -> Self {
        Substitution { map: HashMap::new() }
    }

    pub fn insert(&mut self, var: usize, ty: Type) {
        self.map.insert(var, ty);
    }

    pub fn get(&self, var: usize) -> Option<&Type> {
        self.map.get(&var)
    }

    /// Apply substitution to a type
    pub fn apply(&self, ty: &Type) -> Type {
        match ty {
            Type::Int | Type::Bool | Type::Decimal | Type::String => ty.clone(),
            Type::Var(n) | Type::Num(n) => {
                if let Some(t) = self.map.get(n) {
                    self.apply(t)
                } else {
                    ty.clone()
                }
            }
            Type::Fun(a, b) => Type::Fun(
                Box::new(self.apply(a)),
                Box::new(self.apply(b)),
            ),
            Type::Option(inner) => Type::Option(Box::new(self.apply(inner))),
            Type::Tuple(types) => Type::Tuple(types.iter().map(|t| self.apply(t)).collect()),
        }
    }

    /// Compose two substitutions: self then other
    pub fn compose(&self, other: &Substitution) -> Substitution {
        let mut result = Substitution::new();
        for (k, v) in &self.map {
            result.insert(*k, other.apply(v));
        }
        for (k, v) in &other.map {
            if !result.map.contains_key(k) {
                result.insert(*k, v.clone());
            }
        }
        result
    }
}

/// Type environment: mapping from variable names to their types.
///
/// The environment is used during type inference to track the types
/// of bound variables in the current scope.
#[derive(Debug, Clone, Default)]
pub struct TypeEnv {
    bindings: HashMap<String, Type>,
}

#[allow(dead_code)]
impl TypeEnv {
    /// Create an empty type environment
    pub fn new() -> Self {
        TypeEnv { bindings: HashMap::new() }
    }

    /// Add a binding to this environment (mutates in place)
    pub fn insert(&mut self, name: String, ty: Type) {
        self.bindings.insert(name, ty);
    }

    /// Look up a variable's type
    pub fn get(&self, name: &str) -> Option<&Type> {
        self.bindings.get(name)
    }

    /// Create a new environment with an additional binding
    pub fn extend(&self, name: String, ty: Type) -> TypeEnv {
        let mut new_env = self.clone();
        new_env.insert(name, ty);
        new_env
    }

    /// Apply a substitution to all types in the environment
    pub fn apply_subst(&self, subst: &Substitution) -> TypeEnv {
        let mut new_env = TypeEnv::new();
        for (name, ty) in &self.bindings {
            new_env.insert(name.clone(), subst.apply(ty));
        }
        new_env
    }
}

/// Type inference engine using Hindley-Milner algorithm.
///
/// Performs type inference by generating type variables for unknown types,
/// then unifying constraints to solve for concrete types.
///
/// # Example
///
/// ```ignore
/// let mut ti = TypeInference::new();
/// let env = TypeEnv::new();
/// let ty = ti.infer(&env, &expr)?;
/// ```
pub struct TypeInference {
    /// Counter for generating fresh type variables
    next_var: usize,
    /// Current substitution (solved type variable mappings)
    substitution: Substitution,
}

impl TypeInference {
    /// Create a new type inference engine
    pub fn new() -> Self {
        TypeInference {
            next_var: 0,
            substitution: Substitution::new(),
        }
    }

    /// Generate a fresh type variable
    fn fresh_var(&mut self) -> Type {
        let var = self.next_var;
        self.next_var += 1;
        Type::Var(var)
    }

    /// Generate a fresh numeric type variable
    fn fresh_num(&mut self) -> Type {
        let var = self.next_var;
        self.next_var += 1;
        Type::Num(var)
    }

    /// Occurs check: does var occur in ty?
    fn occurs(&self, var: usize, ty: &Type) -> bool {
        match ty {
            Type::Int | Type::Bool | Type::Decimal | Type::String => false,
            Type::Var(n) | Type::Num(n) => *n == var,
            Type::Fun(a, b) => self.occurs(var, a) || self.occurs(var, b),
            Type::Option(inner) => self.occurs(var, inner),
            Type::Tuple(types) => types.iter().any(|t| self.occurs(var, t)),
        }
    }

    /// Unify two types
    pub fn unify(&mut self, t1: &Type, t2: &Type) -> Result<(), TypeError> {
        let t1 = self.substitution.apply(t1);
        let t2 = self.substitution.apply(t2);

        match (&t1, &t2) {
            // Same types unify trivially
            (Type::Int, Type::Int) => Ok(()),
            (Type::Bool, Type::Bool) => Ok(()),
            (Type::Decimal, Type::Decimal) => Ok(()),
            (Type::String, Type::String) => Ok(()),

            // Type variable unifies with anything (with occurs check)
            (Type::Var(n), t) | (t, Type::Var(n)) => {
                if let Type::Var(m) = t {
                    if n == m {
                        return Ok(());
                    }
                }
                if self.occurs(*n, t) {
                    return Err(TypeError::new(format!("Infinite type: ?{} = {}", n, t)));
                }
                self.substitution.insert(*n, t.clone());
                Ok(())
            }

            // Numeric type variable unifies with Int or Decimal
            (Type::Num(n), Type::Int) | (Type::Int, Type::Num(n)) => {
                self.substitution.insert(*n, Type::Int);
                Ok(())
            }
            (Type::Num(n), Type::Decimal) | (Type::Decimal, Type::Num(n)) => {
                self.substitution.insert(*n, Type::Decimal);
                Ok(())
            }
            (Type::Num(n), Type::Num(m)) => {
                if n != m {
                    self.substitution.insert(*n, Type::Num(*m));
                }
                Ok(())
            }

            // Function types unify component-wise
            (Type::Fun(a1, b1), Type::Fun(a2, b2)) => {
                self.unify(a1, a2)?;
                self.unify(b1, b2)
            }

            // Option types unify component-wise
            (Type::Option(a), Type::Option(b)) => {
                self.unify(a, b)
            }

            // Tuple types unify component-wise if same length
            (Type::Tuple(ts1), Type::Tuple(ts2)) => {
                if ts1.len() != ts2.len() {
                    return Err(TypeError::new(format!(
                        "Tuple length mismatch: {} vs {}",
                        ts1.len(), ts2.len()
                    )));
                }
                for (t1, t2) in ts1.iter().zip(ts2.iter()) {
                    self.unify(t1, t2)?;
                }
                Ok(())
            }

            // Mismatched types
            _ => Err(TypeError::new(format!("Cannot unify {} with {}", t1, t2))),
        }
    }

    /// Infer the type of an expression
    pub fn infer(&mut self, env: &TypeEnv, expr: &Expr) -> Result<Type, TypeError> {
        match expr {
            Expr::Int(_) => Ok(Type::Int),
            Expr::Decimal(_) => Ok(Type::Decimal),
            Expr::Bool(_) => Ok(Type::Bool),
            Expr::String(_) => Ok(Type::String),

            Expr::Var(name) => {
                env.get(name)
                    .cloned()
                    .ok_or_else(|| TypeError::new(format!("Unbound variable: {}", name)))
            }

            Expr::BinOp(op, left, right) => {
                let left_ty = self.infer(env, left)?;
                let right_ty = self.infer(env, right)?;
                self.infer_binop(*op, &left_ty, &right_ty)
            }

            Expr::UnaryOp(op, expr) => {
                let ty = self.infer(env, expr)?;
                match op {
                    UnaryOp::Neg => {
                        // Negation works on numeric types
                        let num = self.fresh_num();
                        self.unify(&ty, &num)?;
                        Ok(self.substitution.apply(&num))
                    }
                    UnaryOp::Not => {
                        self.unify(&ty, &Type::Bool)?;
                        Ok(Type::Bool)
                    }
                }
            }

            Expr::If(cond, then_branch, else_branch) => {
                let cond_ty = self.infer(env, cond)?;
                self.unify(&cond_ty, &Type::Bool)?;

                let then_ty = self.infer(env, then_branch)?;
                let else_ty = self.infer(env, else_branch)?;
                self.unify(&then_ty, &else_ty)?;

                Ok(self.substitution.apply(&then_ty))
            }

            Expr::Let { name, recursive, params, value, body } => {
                let value_ty = if *recursive {
                    // For recursive bindings, add a placeholder type first
                    let placeholder = self.fresh_var();
                    let rec_env = env.extend(name.clone(), placeholder.clone());
                    let inferred = self.infer_function(&rec_env, params, value)?;
                    self.unify(&placeholder, &inferred)?;
                    self.substitution.apply(&inferred)
                } else if params.is_empty() {
                    self.infer(env, value)?
                } else {
                    self.infer_function(env, params, value)?
                };

                let body_env = env.extend(name.clone(), value_ty);
                self.infer(&body_env, body)
            }

            Expr::App(func, arg) => {
                let func_ty = self.infer(env, func)?;
                let arg_ty = self.infer(env, arg)?;
                let result_ty = self.fresh_var();

                self.unify(&func_ty, &Type::fun(arg_ty, result_ty.clone()))?;
                Ok(self.substitution.apply(&result_ty))
            }

            Expr::Match { scrutinee, arms } => {
                let scrut_ty = self.infer(env, scrutinee)?;

                // Determine if we're matching on Option, Tuple, or Int based on patterns
                let is_option_match = arms.iter().any(|arm| {
                    matches!(arm.pattern, Pattern::OptionNone | Pattern::OptionSome(_))
                });
                let is_tuple_match = arms.iter().any(|arm| {
                    matches!(arm.pattern, Pattern::Tuple(_))
                });

                let inner_ty = self.fresh_var();
                let mut tuple_elem_types: Vec<Type> = Vec::new();

                if is_option_match {
                    // Option match: scrutinee must be T option
                    self.unify(&scrut_ty, &Type::Option(Box::new(inner_ty.clone())))?;
                } else if is_tuple_match {
                    // Tuple match: infer element types from patterns
                    // For now, just verify that scrutinee is compatible
                } else {
                    // Integer match (original behavior)
                    self.unify(&scrut_ty, &Type::Int)?;
                }

                let result_ty = self.fresh_var();
                for arm in arms {
                    let arm_env = match &arm.pattern {
                        Pattern::Int(_) => env.clone(),
                        Pattern::Range(_, _) => env.clone(),  // Range doesn't bind variables
                        Pattern::Var(name) => {
                            // When matching on integers, bind to Int
                            // When matching on Options, bind to the resolved scrutinee type
                            let resolved = self.substitution.apply(&scrut_ty);
                            env.extend(name.clone(), resolved)
                        }
                        Pattern::Wildcard => env.clone(),
                        Pattern::OptionNone => env.clone(),
                        Pattern::OptionSome(name) => {
                            // Bind the inner value
                            let resolved_inner = self.substitution.apply(&inner_ty);
                            env.extend(name.clone(), resolved_inner)
                        }
                        Pattern::Tuple(patterns) => {
                            // Create type variables for each element
                            if tuple_elem_types.is_empty() {
                                for _ in patterns {
                                    tuple_elem_types.push(self.fresh_var());
                                }
                                // Unify scrutinee with tuple type
                                let expected_tuple = Type::Tuple(tuple_elem_types.clone());
                                self.unify(&scrut_ty, &expected_tuple)?;
                            }

                            // Build environment with bindings for each element
                            let mut new_env = env.clone();
                            for (i, pat) in patterns.iter().enumerate() {
                                if let Pattern::Var(name) = pat {
                                    let elem_ty = self.substitution.apply(&tuple_elem_types[i]);
                                    new_env = new_env.extend(name.clone(), elem_ty);
                                }
                                // Wildcards don't need binding
                            }
                            new_env
                        }
                    };

                    // Type check guard if present (must be Bool)
                    if let Some(guard) = &arm.guard {
                        let guard_ty = self.infer(&arm_env, guard)?;
                        self.unify(&guard_ty, &Type::Bool)?;
                    }

                    let arm_ty = self.infer(&arm_env, &arm.body)?;
                    self.unify(&result_ty, &arm_ty)?;
                }

                Ok(self.substitution.apply(&result_ty))
            }

            Expr::ToInt(inner) => {
                let inner_ty = self.infer(env, inner)?;
                self.unify(&inner_ty, &Type::Decimal)?;
                Ok(Type::Int)
            }

            Expr::ToDecimal(inner) => {
                let inner_ty = self.infer(env, inner)?;
                self.unify(&inner_ty, &Type::Int)?;
                Ok(Type::Decimal)
            }

            Expr::Seq(first, second) => {
                self.infer(env, first)?;  // Ignore result type
                self.infer(env, second)
            }

            Expr::Tuple(exprs) => {
                let mut types = Vec::new();
                for e in exprs {
                    types.push(self.infer(env, e)?);
                }
                Ok(Type::Tuple(types))
            }

            Expr::Typed(inner, ty) => {
                let inferred = self.infer(env, inner)?;
                self.unify(&inferred, ty)?;
                Ok(ty.clone())
            }

            Expr::Lambda { params, body } => {
                self.infer_function(env, params, body)
            }

            Expr::None => {
                // None has type 'a option where 'a is a fresh type variable
                let inner_ty = self.fresh_var();
                Ok(Type::Option(Box::new(inner_ty)))
            }

            Expr::Some(inner) => {
                // Some(x) has type T option where T is the type of x
                let inner_ty = self.infer(env, inner)?;
                Ok(Type::Option(Box::new(inner_ty)))
            }
        }
    }

    /// Infer type of a binary operation with auto-coercion
    fn infer_binop(&mut self, op: BinOp, left: &Type, right: &Type) -> Result<Type, TypeError> {
        let left = self.substitution.apply(left);
        let right = self.substitution.apply(right);

        if op.is_arithmetic() {
            // Arithmetic: both operands must be numeric, result is numeric
            // Auto-coerce int to decimal if mixed
            match (&left, &right) {
                (Type::Int, Type::Int) => Ok(Type::Int),
                (Type::Decimal, Type::Decimal) => Ok(Type::Decimal),
                (Type::Int, Type::Decimal) | (Type::Decimal, Type::Int) => {
                    // Mixed: result is decimal (int gets coerced)
                    Ok(Type::Decimal)
                }
                (Type::Num(n), Type::Int) | (Type::Int, Type::Num(n)) => {
                    self.substitution.insert(*n, Type::Int);
                    Ok(Type::Int)
                }
                (Type::Num(n), Type::Decimal) | (Type::Decimal, Type::Num(n)) => {
                    self.substitution.insert(*n, Type::Decimal);
                    Ok(Type::Decimal)
                }
                (Type::Num(n), Type::Num(m)) => {
                    // Both unknown, keep as numeric
                    if n != m {
                        self.substitution.insert(*n, Type::Num(*m));
                    }
                    Ok(Type::Num(*m))
                }
                // Type::Var used in arithmetic - constrain to numeric (default to Int)
                (Type::Var(n), Type::Int) | (Type::Int, Type::Var(n)) => {
                    self.substitution.insert(*n, Type::Int);
                    Ok(Type::Int)
                }
                (Type::Var(n), Type::Decimal) | (Type::Decimal, Type::Var(n)) => {
                    self.substitution.insert(*n, Type::Decimal);
                    Ok(Type::Decimal)
                }
                (Type::Var(n), Type::Num(m)) | (Type::Num(m), Type::Var(n)) => {
                    self.substitution.insert(*n, Type::Num(*m));
                    Ok(Type::Num(*m))
                }
                (Type::Var(n), Type::Var(m)) => {
                    // Both unknown - unify them and constrain to numeric (Int as default)
                    if n != m {
                        self.substitution.insert(*n, Type::Var(*m));
                    }
                    self.substitution.insert(*m, Type::Int);
                    Ok(Type::Int)
                }
                _ => Err(TypeError::new(format!(
                    "Arithmetic operator requires numeric types, got {} and {}",
                    left, right
                ))),
            }
        } else if op.is_comparison() {
            // Comparison: both operands must be same type, result is bool
            // Allow comparing int to decimal (coerce int)
            match (&left, &right) {
                (Type::Int, Type::Int) => Ok(Type::Bool),
                (Type::Decimal, Type::Decimal) => Ok(Type::Bool),
                (Type::Bool, Type::Bool) if op == BinOp::Eq || op == BinOp::NotEq => Ok(Type::Bool),
                (Type::Int, Type::Decimal) | (Type::Decimal, Type::Int) => Ok(Type::Bool),
                (Type::Num(_), _) | (_, Type::Num(_)) => {
                    self.unify(&left, &right)?;
                    Ok(Type::Bool)
                }
                // Type::Var in comparison - try to unify
                (Type::Var(_), _) | (_, Type::Var(_)) => {
                    self.unify(&left, &right)?;
                    Ok(Type::Bool)
                }
                _ => Err(TypeError::new(format!(
                    "Cannot compare {} with {}",
                    left, right
                ))),
            }
        } else if op.is_logical() {
            // Logical: both operands must be bool
            self.unify(&left, &Type::Bool)?;
            self.unify(&right, &Type::Bool)?;
            Ok(Type::Bool)
        } else {
            Err(TypeError::new("Unknown operator"))
        }
    }

    /// Infer type of a function with parameters
    fn infer_function(&mut self, env: &TypeEnv, params: &[crate::ast::Param], body: &Expr) -> Result<Type, TypeError> {
        if params.is_empty() {
            return self.infer(env, body);
        }

        let mut current_env = env.clone();
        let mut param_types = Vec::new();

        for param in params {
            let param_ty = if let Some(ann) = &param.ty {
                self.annotation_to_type(ann)
            } else {
                self.fresh_var()
            };
            current_env = current_env.extend(param.name.clone(), param_ty.clone());
            param_types.push(param_ty);
        }

        let body_ty = self.infer(&current_env, body)?;

        // Build function type right to left: a -> b -> c -> result
        let mut result = body_ty;
        for param_ty in param_types.into_iter().rev() {
            result = Type::fun(param_ty, result);
        }

        Ok(self.substitution.apply(&result))
    }

    /// Convert type annotation to Type
    fn annotation_to_type(&self, ann: &TypeAnnotation) -> Type {
        match ann {
            TypeAnnotation::Int => Type::Int,
            TypeAnnotation::Bool => Type::Bool,
            TypeAnnotation::Decimal => Type::Decimal,
            TypeAnnotation::Fun(a, b) => {
                Type::fun(self.annotation_to_type(a), self.annotation_to_type(b))
            }
        }
    }

    /// Type check a whole program
    pub fn check_program(&mut self, program: &Program) -> Result<TypeEnv, TypeError> {
        let mut env = TypeEnv::new();

        // Add built-in functions
        env.insert("print_int".to_string(), Type::fun(Type::Int, Type::Int));
        env.insert("print_decimal".to_string(), Type::fun(Type::Decimal, Type::Decimal));
        env.insert("print_bool".to_string(), Type::fun(Type::Bool, Type::Bool));
        env.insert("print_string".to_string(), Type::fun(Type::String, Type::Int));
        env.insert("print_newline".to_string(), Type::fun(Type::Int, Type::Int));

        for binding in &program.bindings {
            let ty = if binding.recursive {
                let placeholder = self.fresh_var();
                let rec_env = env.extend(binding.name.clone(), placeholder.clone());
                let inferred = self.infer_function(&rec_env, &binding.params, &binding.body)?;
                self.unify(&placeholder, &inferred)?;
                self.substitution.apply(&inferred)
            } else if binding.params.is_empty() {
                self.infer(&env, &binding.body)?
            } else {
                self.infer_function(&env, &binding.params, &binding.body)?
            };

            env.insert(binding.name.clone(), ty);
        }

        Ok(env)
    }

    /// Get the final substitution
    #[allow(dead_code)]
    pub fn get_substitution(&self) -> &Substitution {
        &self.substitution
    }

    /// Resolve all type variables to concrete types, defaulting Num to Int
    pub fn resolve(&self, ty: &Type) -> Type {
        let resolved = self.substitution.apply(ty);
        match resolved {
            Type::Num(_) => Type::Int,  // Default numeric to int
            Type::Var(_) => Type::Int,  // Default unknown to int
            Type::Fun(a, b) => Type::fun(self.resolve(&a), self.resolve(&b)),
            Type::Option(inner) => Type::Option(Box::new(self.resolve(&inner))),
            Type::Tuple(types) => Type::Tuple(types.iter().map(|t| self.resolve(t)).collect()),
            other => other,
        }
    }
}

impl Default for TypeInference {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_unify_same() {
        let mut ti = TypeInference::new();
        assert!(ti.unify(&Type::Int, &Type::Int).is_ok());
        assert!(ti.unify(&Type::Bool, &Type::Bool).is_ok());
    }

    #[test]
    fn test_unify_var() {
        let mut ti = TypeInference::new();
        let var = ti.fresh_var();
        assert!(ti.unify(&var, &Type::Int).is_ok());
        assert_eq!(ti.substitution.apply(&var), Type::Int);
    }

    #[test]
    fn test_unify_mismatch() {
        let mut ti = TypeInference::new();
        assert!(ti.unify(&Type::Int, &Type::Bool).is_err());
    }

    #[test]
    fn test_infer_literal() {
        let mut ti = TypeInference::new();
        let env = TypeEnv::new();
        assert_eq!(ti.infer(&env, &Expr::Int(42)).unwrap(), Type::Int);
        assert_eq!(ti.infer(&env, &Expr::decimal("3.14")).unwrap(), Type::Decimal);
    }

    #[test]
    fn test_infer_binop() {
        let mut ti = TypeInference::new();
        let env = TypeEnv::new();
        let expr = Expr::binop(BinOp::Add, Expr::int(1), Expr::int(2));
        assert_eq!(ti.infer(&env, &expr).unwrap(), Type::Int);
    }

    #[test]
    fn test_infer_mixed_arithmetic() {
        let mut ti = TypeInference::new();
        let env = TypeEnv::new();
        // 1 + 2.0 should be decimal (auto-coerce)
        let expr = Expr::binop(BinOp::Add, Expr::int(1), Expr::decimal("2.0"));
        assert_eq!(ti.infer(&env, &expr).unwrap(), Type::Decimal);
    }
}
