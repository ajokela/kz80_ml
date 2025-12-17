# kz80_ml TODO

## Runtime Implementation
- [x] Port BCD multiplication from kz80_c/kz80_smalltalk
- [x] Port BCD division from kz80_c/kz80_smalltalk
- [x] Implement `int_to_bcd` conversion routine
- [x] Implement `bcd_to_int` conversion routine
- [x] Implement `print_decimal` with proper decimal point placement
- [x] Add 16-bit multiply routine (`rt_mul16`)
- [x] Add 16-bit divide routine (`rt_div16_full`)
- [x] Add 16-bit modulo routine (`rt_mod16`)
- [x] Add signed comparison routine (`rt_lt16`)

## Codegen
- [x] Handle decimal literals properly (parse and encode as BCD)
- [x] Implement auto-coercion insertion in codegen (int→decimal)
- [x] Fix stack frame handling for function parameters
- [x] Add proper function call convention

## Language Features
- [x] Tuple support with pattern matching
- [x] Option types (Some/None) with pattern matching
- [x] Lambda expressions (anonymous functions)
- [x] First-class functions (pass functions as arguments)
- [x] Closures (lambdas capturing enclosing scope variables)
- [x] Type annotations on function parameters: `let f (x: int) = ...`
- [x] String literals and `print_string`
- [x] Pattern matching guards (when keyword)
- [x] Pattern matching ranges (1..10)
- [x] Tail-call optimization for recursive functions

## Testing
- [x] Test compilation with Z80 emulator
- [x] Add integration tests that run through emulator (tests/ml/run_tests.sh - 49 tests)
- [x] Test mixed int/decimal arithmetic
- [x] Test recursive functions (factorial, fibonacci)

## Cleanup
- [x] Remove unused imports and dead code warnings
- [x] Add documentation comments to public APIs
- [x] Add README.md with usage examples
- [x] Add LICENSE file (BSD-3-Clause)
