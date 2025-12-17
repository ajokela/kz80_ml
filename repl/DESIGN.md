# kz80_ml On-Target REPL Design

## Overview

A stack-based bytecode interpreter that runs on the Z80, providing an
interactive ML-like REPL experience.

## Memory Layout

```
0x0000 - 0x1FFF  ROM (REPL code + interpreter)
0x2000 - 0x27FF  Bytecode buffer (2KB max program)
0x2800 - 0x2FFF  Symbol table (2KB)
0x3000 - 0x33FF  Input line buffer (1KB)
0x3400 - 0x36FF  Evaluation stack (768 bytes = 384 16-bit values)
0x3700 - 0x37FF  Scratch/temp (256 bytes)
0x3800 - 0x3FFF  CPU stack (2KB)
```

## Bytecode Instructions

### Stack Operations (0x00-0x0F)
```
0x00  NOP           No operation
0x01  PUSH_I16 lo hi  Push 16-bit integer
0x02  PUSH_DEC b0 b1 b2 b3  Push 4-byte BCD decimal
0x03  DUP           Duplicate top of stack
0x04  DROP          Pop and discard top
0x05  SWAP          Swap top two values
0x06  OVER          Copy second item to top
```

### Arithmetic (0x10-0x1F) - Integer
```
0x10  ADD_I         Pop two, push sum (integer)
0x11  SUB_I         Pop two, push difference
0x12  MUL_I         Pop two, push product
0x13  DIV_I         Pop two, push quotient
0x14  MOD_I         Pop two, push remainder
0x15  NEG_I         Negate top of stack
```

### Arithmetic (0x20-0x2F) - Decimal (BCD)
```
0x20  ADD_D         BCD addition
0x21  SUB_D         BCD subtraction
0x22  MUL_D         BCD multiplication
0x23  DIV_D         BCD division
```

### Comparison (0x30-0x3F)
```
0x30  EQ_I          Equal (integer), push 1 or 0
0x31  NE_I          Not equal
0x32  LT_I          Less than
0x33  LE_I          Less or equal
0x34  GT_I          Greater than
0x35  GE_I          Greater or equal
```

### Control Flow (0x40-0x4F)
```
0x40  JMP addr_lo addr_hi    Unconditional jump
0x41  JZ  addr_lo addr_hi    Jump if zero
0x42  JNZ addr_lo addr_hi    Jump if not zero
0x43  CALL addr_lo addr_hi   Call subroutine
0x44  RET                    Return from subroutine
```

### Variables (0x50-0x5F)
```
0x50  LOAD idx      Push value of variable[idx]
0x51  STORE idx     Pop and store to variable[idx]
0x52  LOAD_NAME len chars...  Load by name (for REPL)
0x53  STORE_NAME len chars... Store by name
```

### I/O (0x60-0x6F)
```
0x60  PRINT_I       Pop and print as integer
0x61  PRINT_D       Pop and print as decimal
0x62  PRINT_NL      Print newline
0x63  PRINT_CH      Pop and print as ASCII char
```

### Type Conversion (0x70-0x7F)
```
0x70  I2D           Convert int to decimal (multiply by 100, to BCD)
0x71  D2I           Convert decimal to int (BCD to int, divide by 100)
```

### Special (0xF0-0xFF)
```
0xF0  HALT          Stop execution
0xF1  DEBUG         Print debug info
0xFF  END           End of bytecode
```

## Symbol Table Format

Each entry:
```
[1 byte]  name_length (0 = empty slot)
[8 bytes] name (padded with 0)
[1 byte]  type (0=int, 1=decimal, 2=function)
[2 bytes] value (int) or address (decimal/function)
---
12 bytes per entry, ~170 entries in 2KB
```

## Parser Strategy

Simple recursive descent for expressions:
1. Read tokens one at a time
2. Shunting-yard algorithm for operator precedence
3. Emit bytecode directly to buffer
4. Execute when newline received

## Grammar (Simplified)

```
line     ::= expr | 'let' IDENT '=' expr
expr     ::= term (('+' | '-') term)*
term     ::= factor (('*' | '/') factor)*
factor   ::= NUMBER | DECIMAL | IDENT | '(' expr ')' | '-' factor
```

## REPL Loop

```
1. Print prompt "> "
2. Read line into buffer
3. If empty, goto 1
4. Tokenize line
5. Parse to bytecode
6. If parse error, print error, goto 1
7. Execute bytecode
8. Print result (if expression)
9. Goto 1
```

## Token Types

```
0x00  EOF
0x01  INT      (followed by 2-byte value)
0x02  DEC      (followed by 4-byte BCD)
0x03  IDENT    (followed by length + chars)
0x04  PLUS
0x05  MINUS
0x06  STAR
0x07  SLASH
0x08  LPAREN
0x09  RPAREN
0x0A  EQ
0x0B  LET
0x0C  IF
0x0D  THEN
0x0E  ELSE
0x0F  NEWLINE
```
