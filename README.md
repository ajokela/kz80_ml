# kz80_ml

A Tiny ML compiler for the Z80 processor with Hindley-Milner type inference.

## Features

- **ML-like syntax** with let bindings, pattern matching, and recursion
- **Hindley-Milner type inference** for automatic type checking
- **Two numeric types**: 16-bit integers and 4-byte packed BCD decimals (2 decimal places)
- **Auto-coercion** from int to decimal in mixed arithmetic
- **Pattern matching** on integers with wildcard support
- **Recursive functions** for algorithms like factorial and fibonacci
- **Generates native Z80 machine code** that runs directly on the CPU

## Building

```bash
cargo build --release
```

## Usage

```bash
kz80_ml input.ml -o output.bin
```

Options:
- `-o <file>`: Output binary file (default: `a.out`)
- `-`: Read source from stdin

## Language Reference

### Types

- `int` - 16-bit signed integer
- `decimal` - Fixed-point decimal with 2 decimal places (stored as 4-byte BCD)
- `bool` - Boolean (true/false)

### Literals

```ml
42        (* integer *)
3.14      (* decimal *)
true      (* boolean *)
false
```

### Operators

Arithmetic: `+`, `-`, `*`, `/`, `mod`
Comparison: `==`, `!=`, `<`, `<=`, `>`, `>=`
Logical: `not`

### Let Bindings

```ml
let x = 42
let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1)
```

### Functions

Functions are curried:

```ml
let add a b = a + b
let main = print_int (add 3 4)  (* prints 7 *)
```

### Conditionals

```ml
if condition then expr1 else expr2
```

### Pattern Matching

```ml
match n with
| 0 -> 1
| 1 -> 1
| _ -> fib (n - 1) + fib (n - 2)
```

### Built-in Functions

- `print_int e` - Print an integer
- `print_decimal e` - Print a decimal (displayed as integer, e.g., 3.14 shows as 314)

## Examples

### Factorial

```ml
let rec factorial n =
  if n <= 1 then 1
  else n * factorial (n - 1)

let main = print_int (factorial 6)  (* prints 720 *)
```

### Fibonacci

```ml
let rec fib n =
  match n with
  | 0 -> 0
  | 1 -> 1
  | _ -> fib (n - 1) + fib (n - 2)

let main = print_int (fib 10)  (* prints 55 *)
```

### Mixed Decimal Arithmetic

```ml
let price = 19.99
let quantity = 5

(* quantity (int) is auto-coerced to decimal *)
let subtotal = price * quantity

let main = print_decimal subtotal  (* prints 9995, meaning 99.95 *)
```

### Collatz Conjecture

```ml
let rec collatz n =
  match n with
  | 1 -> 0
  | _ -> if n / 2 * 2 == n
         then 1 + collatz (n / 2)
         else 1 + collatz (3 * n + 1)

let main = print_int (collatz 27)  (* prints 111 *)
```

## Memory Layout

- ROM: 0x0000 - 0x1FFF (8KB)
- RAM: 0x2000 - 0x37FF (6KB)
- Stack: 0x37FF (grows downward)
- I/O: 0x01 (character output)

## Target Platform

Designed for the RetroShield Z80, but should work on any Z80 system with the above memory layout and I/O port 0x01 for character output.

## Testing

The project includes a comprehensive test suite with three levels of testing:

### Rust Unit Tests

Unit tests for the lexer, parser, type inference, and code generator:

```bash
cargo test
```

This runs 36 unit tests covering tokenization, parsing, type checking, and REPL code generation.

### Rust Integration Tests

End-to-end tests that generate REPL binaries and run them through the Z80 emulator:

```bash
cargo test --test integration_tests
```

These 21 tests verify the REPL works correctly for arithmetic, variables, functions, conditionals, and more. Requires the emulator at `../emulator/retroshield`.

### ML Test Suite

Test programs written in the ML language itself:

```bash
./tests/ml/run_tests.sh
```

This runs 29 ML programs through the batch compiler and emulator, covering:

| Category | Tests |
|----------|-------|
| Arithmetic | `+`, `-`, `*`, `/`, `%`, negation |
| Precedence | operator precedence, parentheses |
| Comparisons | `<`, `>`, `<=`, `>=`, `==` |
| Control Flow | nested if-then-else |
| Let Bindings | simple, multiple, let-in |
| Functions | single/multi-arg, composition |
| Recursion | factorial, fibonacci |

Each test file contains an expected result comment that the runner verifies:

```ml
(* Test: Addition *)
(* Expected: 8 *)

let main = print_int (3 + 5)
```

### REPL vs Batch Compiler Features

The on-target REPL supports additional features not available in the batch compiler:

| Feature | Batch Compiler | REPL |
|---------|---------------|------|
| Arithmetic | Yes | Yes |
| Comparisons | Yes | Yes |
| Functions | Yes | Yes |
| Recursion | Yes | Yes |
| Bitwise (`&`, `|`, `^`) | No | Yes |
| Shifts (`<<`, `>>`) | No | Yes |
| Logical (`and`, `or`, `not`) | No | Yes |
| Hex literals (`0xFF`) | No | Yes |
| Not equal (`!=`) | No | Yes |

## License

BSD-3-Clause
