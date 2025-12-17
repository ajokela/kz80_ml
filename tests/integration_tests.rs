//! Integration tests for kz80_ml REPL
//!
//! These tests generate REPL binaries and run them through the emulator
//! to verify end-to-end functionality.
//!
//! Note: These tests require the emulator to be available at ../emulator/retroshield

use std::io::Write;
use std::process::{Command, Stdio};

/// Path to the emulator (relative to project root)
const EMULATOR_PATH: &str = "../emulator/retroshield";

/// Check if emulator is available
fn emulator_available() -> bool {
    std::path::Path::new(EMULATOR_PATH).exists()
}

/// Run a REPL session with the given input and return the output
/// Uses the `timeout` command to prevent hanging
fn run_repl(input: &str) -> Option<String> {
    if !emulator_available() {
        return None;
    }

    // Generate the REPL binary
    let binary = kz80_ml::repl::generate_repl();

    // Write to temp file
    let temp_path = "/tmp/kz80_ml_test.bin";
    let mut file = std::fs::File::create(temp_path).ok()?;
    file.write_all(&binary).ok()?;

    // Run emulator with timeout using the shell
    let output = Command::new("sh")
        .arg("-c")
        .arg(format!(
            "echo '{}' | timeout 2 {} -l {} 2>&1",
            input.replace("'", "'\\''"),
            EMULATOR_PATH,
            temp_path
        ))
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .ok()?;

    String::from_utf8(output.stdout).ok()
}

/// Extract the numeric result from REPL output
/// Output format is like "> INPUT123\n> " where INPUT is echoed (uppercase) and 123 is the result
/// We need to skip past the echoed input to find just the result
fn extract_result_with_input(output: &str, input: &str) -> Option<i32> {
    // The REPL echoes input in uppercase, then prints the result
    // Find the line containing the echoed input
    let input_upper = input.to_uppercase();

    for line in output.lines() {
        if line.starts_with("> ") {
            let content = &line[2..]; // Skip "> "

            // Check if this line contains our echoed input
            if content.starts_with(&input_upper) {
                // The result follows the echoed input
                let result_str = &content[input_upper.len()..];

                // Parse the result (may start with - for negative)
                let trimmed = result_str.trim();
                if let Ok(n) = trimmed.parse::<i32>() {
                    return Some(n);
                }

                // Try to extract leading number if there's trailing content
                let mut digits = String::new();
                let mut chars = trimmed.chars().peekable();

                // Handle optional leading minus
                if chars.peek() == Some(&'-') {
                    digits.push(chars.next().unwrap());
                }

                // Collect digits
                while let Some(&c) = chars.peek() {
                    if c.is_ascii_digit() {
                        digits.push(chars.next().unwrap());
                    } else {
                        break;
                    }
                }

                if !digits.is_empty() && digits != "-" {
                    if let Ok(n) = digits.parse::<i32>() {
                        return Some(n);
                    }
                }
            }
        }
    }
    None
}

/// Extract the last numeric result from REPL output for multi-line input
/// last_input is the last line of input (the expression whose result we want)
/// Note: REPL output doesn't have newlines between prompts, so "> " can appear mid-line
fn extract_last_result_with_input(output: &str, last_input: &str) -> Option<i32> {
    let input_upper = last_input.to_uppercase();
    let pattern = format!("> {}", input_upper);

    // Search for the pattern anywhere in the output (not just at line start)
    if let Some(pos) = output.find(&pattern) {
        let after_pattern = &output[pos + pattern.len()..];

        // Extract the number that follows
        let mut digits = String::new();
        let mut chars = after_pattern.chars().peekable();

        // Handle optional leading minus
        if chars.peek() == Some(&'-') {
            digits.push(chars.next().unwrap());
        }

        // Collect digits
        while let Some(&c) = chars.peek() {
            if c.is_ascii_digit() {
                digits.push(chars.next().unwrap());
            } else {
                break;
            }
        }

        if !digits.is_empty() && digits != "-" {
            if let Ok(n) = digits.parse::<i32>() {
                return Some(n);
            }
        }
    }
    None
}

#[test]
fn test_simple_addition() {
    let input = "1 + 2";
    if let Some(output) = run_repl(input) {
        let result = extract_result_with_input(&output, input);
        assert_eq!(result, Some(3), "1 + 2 should equal 3, got output: {}", output);
    } else {
        eprintln!("Skipping test_simple_addition: emulator not available");
    }
}

#[test]
fn test_multiplication() {
    let input = "6 * 7";
    if let Some(output) = run_repl(input) {
        let result = extract_result_with_input(&output, input);
        assert_eq!(result, Some(42), "6 * 7 should equal 42, got output: {}", output);
    } else {
        eprintln!("Skipping test_multiplication: emulator not available");
    }
}

#[test]
fn test_operator_precedence() {
    let input = "2 + 3 * 4";
    if let Some(output) = run_repl(input) {
        let result = extract_result_with_input(&output, input);
        assert_eq!(result, Some(14), "2 + 3 * 4 should equal 14, got output: {}", output);
    } else {
        eprintln!("Skipping test_operator_precedence: emulator not available");
    }
}

#[test]
fn test_parentheses() {
    let input = "(2 + 3) * 4";
    if let Some(output) = run_repl(input) {
        let result = extract_result_with_input(&output, input);
        assert_eq!(result, Some(20), "(2 + 3) * 4 should equal 20, got output: {}", output);
    } else {
        eprintln!("Skipping test_parentheses: emulator not available");
    }
}

#[test]
fn test_variable_assignment() {
    let input = "let x = 10\nx + 5";
    if let Some(output) = run_repl(input) {
        let result = extract_last_result_with_input(&output, "x + 5");
        assert_eq!(result, Some(15), "x + 5 with x=10 should equal 15, got output: {}", output);
    } else {
        eprintln!("Skipping test_variable_assignment: emulator not available");
    }
}

#[test]
fn test_if_then_else_true() {
    let input = "if 1 < 2 then 100 else 200";
    if let Some(output) = run_repl(input) {
        let result = extract_result_with_input(&output, input);
        assert_eq!(result, Some(100), "if 1 < 2 then 100 else 200 should equal 100, got output: {}", output);
    } else {
        eprintln!("Skipping test_if_then_else_true: emulator not available");
    }
}

#[test]
fn test_if_then_else_false() {
    let input = "if 2 < 1 then 100 else 200";
    if let Some(output) = run_repl(input) {
        let result = extract_result_with_input(&output, input);
        assert_eq!(result, Some(200), "if 2 < 1 then 100 else 200 should equal 200, got output: {}", output);
    } else {
        eprintln!("Skipping test_if_then_else_false: emulator not available");
    }
}

#[test]
fn test_comparison_eq() {
    let input = "if 5 == 5 then 1 else 0";
    if let Some(output) = run_repl(input) {
        let result = extract_result_with_input(&output, input);
        assert_eq!(result, Some(1), "5 == 5 should be true, got output: {}", output);
    } else {
        eprintln!("Skipping test_comparison_eq: emulator not available");
    }
}

#[test]
fn test_bitwise_and() {
    let input = "12 & 10";
    if let Some(output) = run_repl(input) {
        let result = extract_result_with_input(&output, input);
        assert_eq!(result, Some(8), "12 & 10 should equal 8, got output: {}", output);
    } else {
        eprintln!("Skipping test_bitwise_and: emulator not available");
    }
}

#[test]
fn test_bitwise_or() {
    let input = "12 | 10";
    if let Some(output) = run_repl(input) {
        let result = extract_result_with_input(&output, input);
        assert_eq!(result, Some(14), "12 | 10 should equal 14, got output: {}", output);
    } else {
        eprintln!("Skipping test_bitwise_or: emulator not available");
    }
}

#[test]
fn test_shift_left() {
    let input = "1 << 4";
    if let Some(output) = run_repl(input) {
        let result = extract_result_with_input(&output, input);
        assert_eq!(result, Some(16), "1 << 4 should equal 16, got output: {}", output);
    } else {
        eprintln!("Skipping test_shift_left: emulator not available");
    }
}

#[test]
fn test_function_definition_and_call() {
    let input = "fun f x = x * 2\nf 5";
    if let Some(output) = run_repl(input) {
        let result = extract_last_result_with_input(&output, "f 5");
        assert_eq!(result, Some(10), "f(5) with f(x) = x * 2 should equal 10, got output: {}", output);
    } else {
        eprintln!("Skipping test_function_definition_and_call: emulator not available");
    }
}

#[test]
fn test_decimal_literal() {
    let input = "3.14";
    if let Some(output) = run_repl(input) {
        let result = extract_result_with_input(&output, input);
        assert_eq!(result, Some(314), "3.14 should be represented as 314, got output: {}", output);
    } else {
        eprintln!("Skipping test_decimal_literal: emulator not available");
    }
}

#[test]
fn test_decimal_function_arg() {
    let input = "fun f x = x + 100\nf 1.5";
    if let Some(output) = run_repl(input) {
        let result = extract_last_result_with_input(&output, "f 1.5");
        assert_eq!(result, Some(250), "f(1.5) with f(x) = x + 100 should equal 250 (150+100), got output: {}", output);
    } else {
        eprintln!("Skipping test_decimal_function_arg: emulator not available");
    }
}

#[test]
fn test_hex_literal() {
    let input = "0xFF";
    if let Some(output) = run_repl(input) {
        let result = extract_result_with_input(&output, input);
        assert_eq!(result, Some(255), "0xFF should equal 255, got output: {}", output);
    } else {
        eprintln!("Skipping test_hex_literal: emulator not available");
    }
}

#[test]
fn test_negative_number() {
    let input = "-5";
    if let Some(output) = run_repl(input) {
        let result = extract_result_with_input(&output, input);
        assert_eq!(result, Some(-5), "-5 should equal -5, got output: {}", output);
    } else {
        eprintln!("Skipping test_negative_number: emulator not available");
    }
}

#[test]
fn test_modulo() {
    let input = "17 % 5";
    if let Some(output) = run_repl(input) {
        let result = extract_result_with_input(&output, input);
        assert_eq!(result, Some(2), "17 % 5 should equal 2, got output: {}", output);
    } else {
        eprintln!("Skipping test_modulo: emulator not available");
    }
}

#[test]
fn test_semicolon_multiple_statements() {
    let input = "1; 2; 3";
    if let Some(output) = run_repl(input) {
        let result = extract_result_with_input(&output, input);
        assert_eq!(result, Some(3), "1; 2; 3 should return 3, got output: {}", output);
    } else {
        eprintln!("Skipping test_semicolon_multiple_statements: emulator not available");
    }
}

#[test]
fn test_logical_and() {
    let input = "1 and 1";
    if let Some(output) = run_repl(input) {
        let result = extract_result_with_input(&output, input);
        assert_eq!(result, Some(1), "1 and 1 should equal 1, got output: {}", output);
    } else {
        eprintln!("Skipping test_logical_and: emulator not available");
    }
}

#[test]
fn test_logical_or() {
    let input = "0 or 1";
    if let Some(output) = run_repl(input) {
        let result = extract_result_with_input(&output, input);
        assert_eq!(result, Some(1), "0 or 1 should equal 1, got output: {}", output);
    } else {
        eprintln!("Skipping test_logical_or: emulator not available");
    }
}

#[test]
fn test_logical_not() {
    let input = "not 0";
    if let Some(output) = run_repl(input) {
        let result = extract_result_with_input(&output, input);
        assert_eq!(result, Some(1), "not 0 should equal 1, got output: {}", output);
    } else {
        eprintln!("Skipping test_logical_not: emulator not available");
    }
}
