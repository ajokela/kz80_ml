//! kz80_ml - Tiny ML compiler for the Z80
//!
//! A statically-typed functional language with Hindley-Milner type inference,
//! targeting the Z80 processor via the RetroShield platform.

pub mod token;
pub mod lexer;
pub mod ast;
pub mod parser;
pub mod types;
pub mod codegen;
pub mod repl;
