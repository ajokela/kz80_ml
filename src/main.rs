//! kz80_ml - Tiny ML compiler for the Z80
//!
//! A statically-typed functional language with Hindley-Milner type inference,
//! targeting the Z80 processor via the RetroShield platform.
//!
//! Features:
//! - ML-style syntax with let bindings and recursion
//! - Type inference (int, bool, decimal)
//! - Auto-coercion from int to decimal in mixed expressions
//! - BCD decimal arithmetic using the DAA instruction
//! - Pattern matching on integers

use std::fs;
use std::io::{self, Read};
use std::path::PathBuf;

use kz80_ml::lexer::Lexer;
use kz80_ml::parser::Parser;
use kz80_ml::codegen::CodeGenerator;
use kz80_ml::types::TypeInference;
use kz80_ml::repl;

const VERSION: &str = "0.1.0";

fn print_usage() {
    eprintln!("kz80_ml {} - Tiny ML compiler for Z80", VERSION);
    eprintln!();
    eprintln!("Usage: kz80_ml [OPTIONS] <input.ml>");
    eprintln!("       kz80_ml --repl -o repl.bin");
    eprintln!();
    eprintln!("Options:");
    eprintln!("  -o <file>     Output file (default: out.bin)");
    eprintln!("  --repl        Generate on-target REPL binary");
    eprintln!("  --ast         Print AST and exit");
    eprintln!("  --tokens      Print tokens and exit");
    eprintln!("  --types       Print inferred types and exit");
    eprintln!("  -h, --help    Show this help");
    eprintln!("  -v, --version Show version");
    eprintln!();
    eprintln!("Examples:");
    eprintln!("  kz80_ml factorial.ml -o factorial.bin");
    eprintln!("  kz80_ml --types program.ml");
    eprintln!("  kz80_ml --repl -o repl.bin  # Generate on-target REPL");
}

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() < 2 {
        print_usage();
        std::process::exit(1);
    }

    let mut input_file: Option<String> = None;
    let mut output_file = "out.bin".to_string();
    let mut print_ast = false;
    let mut print_tokens = false;
    let mut print_types = false;
    let mut gen_repl = false;

    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "-h" | "--help" => {
                print_usage();
                return;
            }
            "-v" | "--version" => {
                println!("kz80_ml {}", VERSION);
                return;
            }
            "-o" => {
                i += 1;
                if i >= args.len() {
                    eprintln!("Error: -o requires an argument");
                    std::process::exit(1);
                }
                output_file = args[i].clone();
            }
            "--repl" => gen_repl = true,
            "--ast" => print_ast = true,
            "--tokens" => print_tokens = true,
            "--types" => print_types = true,
            arg if arg.starts_with('-') => {
                eprintln!("Unknown option: {}", arg);
                std::process::exit(1);
            }
            _ => {
                input_file = Some(args[i].clone());
            }
        }
        i += 1;
    }

    // Handle REPL generation
    if gen_repl {
        let binary = repl::generate_repl();
        let output_path = PathBuf::from(&output_file);
        fs::write(&output_path, &binary).unwrap_or_else(|e| {
            eprintln!("Error writing {}: {}", output_file, e);
            std::process::exit(1);
        });
        println!("Generated REPL -> {} ({} bytes)", output_file, binary.len());
        return;
    }

    let input_path = match input_file {
        Some(p) => p,
        None => {
            eprintln!("Error: no input file specified");
            print_usage();
            std::process::exit(1);
        }
    };

    // Read input
    let source = if input_path == "/dev/stdin" || input_path == "-" {
        let mut buf = String::new();
        io::stdin().read_to_string(&mut buf).expect("Failed to read stdin");
        buf
    } else {
        fs::read_to_string(&input_path).unwrap_or_else(|e| {
            eprintln!("Error reading {}: {}", input_path, e);
            std::process::exit(1);
        })
    };

    // Tokenize
    let mut lexer = Lexer::new(&source);
    let tokens = lexer.tokenize();

    if print_tokens {
        println!("Tokens:");
        for tok in &tokens {
            println!("  {:?} at {}:{}", tok.token, tok.span.line, tok.span.column);
        }
        return;
    }

    // Parse
    let mut parser = Parser::new(tokens);
    let program = match parser.parse_program() {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Parse error: {}", e);
            std::process::exit(1);
        }
    };

    if print_ast {
        println!("AST:");
        for binding in &program.bindings {
            println!("  let{} {} = {:?}",
                if binding.recursive { " rec" } else { "" },
                binding.name,
                binding.body
            );
        }
        return;
    }

    // Type check
    let mut type_inference = TypeInference::new();
    let type_env = match type_inference.check_program(&program) {
        Ok(env) => env,
        Err(e) => {
            eprintln!("Type error: {}", e);
            std::process::exit(1);
        }
    };

    if print_types {
        println!("Inferred types:");
        for binding in &program.bindings {
            if let Some(ty) = type_env.get(&binding.name) {
                let resolved = type_inference.resolve(ty);
                println!("  {} : {}", binding.name, resolved);
            }
        }
        return;
    }

    // Generate code
    let mut codegen = CodeGenerator::new();
    let binary = match codegen.generate(&program) {
        Ok(b) => b,
        Err(e) => {
            eprintln!("Code generation error: {}", e);
            std::process::exit(1);
        }
    };

    // Write output
    let output_path = PathBuf::from(&output_file);
    fs::write(&output_path, &binary).unwrap_or_else(|e| {
        eprintln!("Error writing {}: {}", output_file, e);
        std::process::exit(1);
    });

    println!("Compiled {} -> {} ({} bytes)", input_path, output_file, binary.len());
}
