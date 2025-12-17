//! On-target REPL generator for Z80
//!
//! Generates a standalone REPL binary that runs on the Z80,
//! including a bytecode interpreter, lexer, and parser.

/// Memory layout constants
const BYTECODE_BUF: u16 = 0x2000;   // Bytecode buffer start
const SYMBOL_TABLE: u16 = 0x2800;   // Symbol table (26 vars * 2 bytes = 52 bytes)
const FUNC_TABLE: u16 = 0x2900;     // Function table (26 funcs * 4 bytes = 104 bytes)
                                    // [param_idx, body_len, body_lo, body_hi]
const FUNC_CODE: u16 = 0x2A00;      // Function bytecode storage
const FUNC_CODE_PTR: u16 = 0x2F00;  // Pointer to next free byte in FUNC_CODE
const INPUT_BUF: u16 = 0x3000;      // Line input buffer
const EVAL_STACK: u16 = 0x3400;     // Evaluation stack
const SCRATCH: u16 = 0x3700;        // Scratch area
const CPU_STACK: u16 = 0x3FFF;      // CPU stack top

/// Bytecode opcodes
#[allow(dead_code)]
mod op {
    pub const NOP: u8 = 0x00;
    pub const PUSH_I16: u8 = 0x01;
    pub const PUSH_DEC: u8 = 0x02;
    pub const DUP: u8 = 0x03;
    pub const DROP: u8 = 0x04;
    pub const SWAP: u8 = 0x05;

    pub const ADD_I: u8 = 0x10;
    pub const SUB_I: u8 = 0x11;
    pub const MUL_I: u8 = 0x12;
    pub const DIV_I: u8 = 0x13;
    pub const MOD_I: u8 = 0x14;
    pub const NEG_I: u8 = 0x15;
    pub const BAND_I: u8 = 0x16;   // bitwise AND
    pub const BOR_I: u8 = 0x17;    // bitwise OR
    pub const BXOR_I: u8 = 0x18;   // bitwise XOR
    pub const BNOT_I: u8 = 0x19;   // bitwise NOT (unary)
    pub const SHL_I: u8 = 0x1A;    // shift left
    pub const SHR_I: u8 = 0x1B;    // shift right
    pub const LAND_I: u8 = 0x1C;   // logical AND
    pub const LOR_I: u8 = 0x1D;    // logical OR
    pub const LNOT_I: u8 = 0x1E;   // logical NOT (unary)

    pub const ADD_D: u8 = 0x20;
    pub const SUB_D: u8 = 0x21;
    pub const MUL_D: u8 = 0x22;
    pub const DIV_D: u8 = 0x23;

    pub const EQ_I: u8 = 0x30;
    pub const NE_I: u8 = 0x31;
    pub const LT_I: u8 = 0x32;
    pub const LE_I: u8 = 0x33;
    pub const GT_I: u8 = 0x34;
    pub const GE_I: u8 = 0x35;

    pub const JMP: u8 = 0x40;
    pub const JZ: u8 = 0x41;
    pub const JNZ: u8 = 0x42;

    pub const LOAD: u8 = 0x50;
    pub const STORE: u8 = 0x51;

    pub const PRINT_I: u8 = 0x60;
    pub const PRINT_D: u8 = 0x61;
    pub const PRINT_NL: u8 = 0x62;
    pub const POP_I: u8 = 0x63;   // Pop and discard top of stack
    pub const FCALL: u8 = 0x64;   // Call function: FCALL func_idx param_addr
    pub const FRET: u8 = 0x65;    // Return from function

    pub const I2D: u8 = 0x70;
    pub const D2I: u8 = 0x71;

    pub const HALT: u8 = 0xF0;
    pub const END: u8 = 0xFF;
}

/// Token types for lexer
#[allow(dead_code)]
mod tok {
    pub const EOF: u8 = 0x00;
    pub const INT: u8 = 0x01;
    pub const DEC: u8 = 0x02;
    pub const IDENT: u8 = 0x03;
    pub const PLUS: u8 = 0x04;
    pub const MINUS: u8 = 0x05;
    pub const STAR: u8 = 0x06;
    pub const SLASH: u8 = 0x07;
    pub const LPAREN: u8 = 0x08;
    pub const RPAREN: u8 = 0x09;
    pub const EQ: u8 = 0x0A;
    pub const LET: u8 = 0x0B;
    pub const EQEQ: u8 = 0x0C;
    pub const NE: u8 = 0x0D;
    pub const LT: u8 = 0x0E;
    pub const LE: u8 = 0x0F;
    pub const GT: u8 = 0x10;
    pub const GE: u8 = 0x11;
    pub const MOD: u8 = 0x12;
    pub const BAND: u8 = 0x13;    // &
    pub const BOR: u8 = 0x14;     // |
    pub const BXOR: u8 = 0x15;    // ^
    pub const BNOT: u8 = 0x16;    // ~
    pub const SHL: u8 = 0x17;     // <<
    pub const SHR: u8 = 0x18;     // >>
    pub const AND_KW: u8 = 0x19;  // and (keyword)
    pub const OR_KW: u8 = 0x1A;   // or (keyword)
    pub const NOT_KW: u8 = 0x1B;  // not (keyword)
    pub const IF_KW: u8 = 0x1C;   // if (keyword)
    pub const THEN_KW: u8 = 0x1D; // then (keyword)
    pub const ELSE_KW: u8 = 0x1E; // else (keyword)
    pub const NEWLINE: u8 = 0x1F;
    pub const ASSIGN: u8 = 0x20;  // := (assignment)
    pub const SEMI: u8 = 0x21;    // ; (semicolon)
    pub const WHILE_KW: u8 = 0x22; // while (keyword)
    pub const DO_KW: u8 = 0x23;   // do (keyword)
    pub const FUN_KW: u8 = 0x24;  // fun (keyword)
}

pub struct ReplGenerator {
    code: Vec<u8>,
    labels: std::collections::HashMap<String, u16>,
    fixups: Vec<(usize, String)>,
    current_addr: u16,
}

impl ReplGenerator {
    pub fn new() -> Self {
        ReplGenerator {
            code: Vec::new(),
            labels: std::collections::HashMap::new(),
            fixups: Vec::new(),
            current_addr: 0x0000,
        }
    }

    fn emit(&mut self, bytes: &[u8]) {
        self.code.extend_from_slice(bytes);
        self.current_addr += bytes.len() as u16;
    }

    fn emit_byte(&mut self, b: u8) {
        self.code.push(b);
        self.current_addr += 1;
    }

    fn emit_word(&mut self, w: u16) {
        self.code.push((w & 0xFF) as u8);
        self.code.push((w >> 8) as u8);
        self.current_addr += 2;
    }

    fn label(&mut self, name: &str) {
        self.labels.insert(name.to_string(), self.current_addr);
    }

    fn fixup(&mut self, name: &str) {
        self.fixups.push((self.code.len(), name.to_string()));
        self.emit_word(0x0000); // placeholder
    }

    fn resolve_fixups(&mut self) {
        for (pos, name) in &self.fixups {
            if let Some(&addr) = self.labels.get(name) {
                self.code[*pos] = (addr & 0xFF) as u8;
                self.code[*pos + 1] = (addr >> 8) as u8;
            } else {
                panic!("Unresolved label: {}", name);
            }
        }
    }

    pub fn generate(&mut self) -> Vec<u8> {
        // Entry point
        self.emit(&[0x31]); // LD SP, CPU_STACK
        self.emit_word(CPU_STACK);

        // Initialize eval stack pointer (use IY)
        self.emit(&[0xFD, 0x21]); // LD IY, EVAL_STACK
        self.emit_word(EVAL_STACK);

        // Initialize function code pointer
        self.emit(&[0x21]);       // LD HL, FUNC_CODE
        self.emit_word(FUNC_CODE);
        self.emit(&[0x22]);       // LD (FUNC_CODE_PTR), HL
        self.emit_word(FUNC_CODE_PTR);

        // Jump to main REPL loop
        self.emit(&[0xC3]); // JP repl_loop
        self.fixup("repl_loop");

        // Generate all the runtime routines
        self.emit_print_int();
        self.emit_print_decimal();
        self.emit_print_string();
        self.emit_print_char();
        self.emit_print_newline();
        self.emit_print_hex_byte();
        self.emit_read_line();
        self.emit_mul16();
        self.emit_div16();
        self.emit_lexer();
        self.emit_parser();
        self.emit_interpreter();
        self.emit_repl_loop();

        // String constants
        self.emit_strings();

        self.resolve_fixups();

        // Pad to 8KB
        while self.code.len() < 0x2000 {
            self.code.push(0x00);
        }

        self.code.clone()
    }

    fn emit_print_char(&mut self) {
        // Print character in A to output port 0x00 (Intel 8251 USART)
        self.label("print_char");
        self.emit(&[0xD3, 0x00]); // OUT (0x00), A
        self.emit(&[0xC9]);       // RET
    }

    fn emit_print_newline(&mut self) {
        self.label("print_newline");
        self.emit(&[0x3E, 0x0D]); // LD A, '\r'
        self.emit(&[0xD3, 0x00]); // OUT (0x00), A
        self.emit(&[0x3E, 0x0A]); // LD A, '\n'
        self.emit(&[0xD3, 0x00]); // OUT (0x00), A
        self.emit(&[0xC9]);       // RET
    }

    fn emit_print_hex_byte(&mut self) {
        // Print byte in A as 2 hex digits
        self.label("print_hex_byte");
        self.emit(&[0xF5]);       // PUSH AF (save value)
        self.emit(&[0x0F]);       // RRCA
        self.emit(&[0x0F]);       // RRCA
        self.emit(&[0x0F]);       // RRCA
        self.emit(&[0x0F]);       // RRCA (high nibble now in low bits)
        self.emit(&[0xCD]);       // CALL print_nibble
        self.fixup("print_nibble");
        self.emit(&[0xF1]);       // POP AF
        // Fall through to print low nibble

        self.label("print_nibble");
        self.emit(&[0xE6, 0x0F]); // AND 0x0F
        self.emit(&[0xC6, 0x30]); // ADD A, '0'
        self.emit(&[0xFE, 0x3A]); // CP '9'+1
        self.emit(&[0x38, 0x02]); // JR C, +2 (skip if <= '9')
        self.emit(&[0xC6, 0x07]); // ADD A, 7 (convert to 'A'-'F')
        self.emit(&[0xD3, 0x00]); // OUT (0x00), A
        self.emit(&[0xC9]);       // RET
    }

    fn emit_print_string(&mut self) {
        // Print null-terminated string at HL
        self.label("print_string");
        self.label("print_string_loop");
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xB7]);       // OR A
        self.emit(&[0xC8]);       // RET Z
        self.emit(&[0xD3, 0x00]); // OUT (0x00), A
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0xC3]);       // JP print_string_loop
        self.fixup("print_string_loop");
    }

    fn emit_print_int(&mut self) {
        // Print 16-bit signed integer in HL
        self.label("print_int");

        // Check if negative
        self.emit(&[0x7C]);       // LD A, H
        self.emit(&[0xB7]);       // OR A
        self.emit(&[0xF2]);       // JP P, print_int_pos
        self.fixup("print_int_pos");

        // Negative: print '-' and negate
        self.emit(&[0x3E, 0x2D]); // LD A, '-'
        self.emit(&[0xD3, 0x00]); // OUT (0x00), A
        self.emit(&[0x7C]);       // LD A, H
        self.emit(&[0x2F]);       // CPL
        self.emit(&[0x67]);       // LD H, A
        self.emit(&[0x7D]);       // LD A, L
        self.emit(&[0x2F]);       // CPL
        self.emit(&[0x6F]);       // LD L, A
        self.emit(&[0x23]);       // INC HL

        self.label("print_int_pos");
        // Convert to decimal and print
        // Use repeated division by 10

        // We'll push digits onto CPU stack, then pop and print
        self.emit(&[0x01]);       // LD BC, 0 (digit count)
        self.emit_word(0);

        self.label("print_int_div_loop");
        // Divide HL by 10, remainder in A
        self.emit(&[0x11, 10, 0]); // LD DE, 10
        self.emit(&[0xCD]);       // CALL div16
        self.fixup("div16");
        // HL = quotient, DE had divisor, A = remainder (from div16_full)
        // Actually need to use div16_full which returns remainder

        // Push remainder (digit)
        self.emit(&[0xF5]);       // PUSH AF (save digit)
        self.emit(&[0x03]);       // INC BC (digit count)

        // Check if quotient is 0
        self.emit(&[0x7C]);       // LD A, H
        self.emit(&[0xB5]);       // OR L
        self.emit(&[0xC2]);       // JP NZ, print_int_div_loop
        self.fixup("print_int_div_loop");

        // Pop and print digits
        self.label("print_int_print_loop");
        self.emit(&[0xF1]);       // POP AF
        self.emit(&[0xC6, 0x30]); // ADD A, '0'
        self.emit(&[0xD3, 0x00]); // OUT (0x00), A
        self.emit(&[0x0B]);       // DEC BC
        self.emit(&[0x78]);       // LD A, B
        self.emit(&[0xB1]);       // OR C
        self.emit(&[0xC2]);       // JP NZ, print_int_print_loop
        self.fixup("print_int_print_loop");

        self.emit(&[0xC9]);       // RET
    }

    fn emit_print_decimal(&mut self) {
        // Print 4-byte BCD at HL (2 decimal places)
        // Format: prints as integer * 100 (e.g., 3.14 prints as 314)
        self.label("print_decimal");

        // For simplicity, just print the BCD digits
        self.emit(&[0x06, 4]);    // LD B, 4 (4 bytes)
        self.emit(&[0x0E, 0]);    // LD C, 0 (leading zero flag)

        self.label("print_dec_loop");
        self.emit(&[0x7E]);       // LD A, (HL)

        // High nibble
        self.emit(&[0xF5]);       // PUSH AF
        self.emit(&[0xCB, 0x3F]); // SRL A (x4)
        self.emit(&[0xCB, 0x3F]);
        self.emit(&[0xCB, 0x3F]);
        self.emit(&[0xCB, 0x3F]);
        self.emit(&[0xB1]);       // OR C (check leading zero)
        self.emit(&[0xCA]);       // JP Z, skip_hi_dec
        self.fixup("skip_hi_dec");
        self.emit(&[0x0E, 1]);    // LD C, 1 (no more leading zeros)
        self.emit(&[0xC6, 0x30]); // ADD A, '0'
        self.emit(&[0xD3, 0x00]); // OUT (0x00), A
        self.label("skip_hi_dec");

        // Low nibble
        self.emit(&[0xF1]);       // POP AF
        self.emit(&[0xE6, 0x0F]); // AND 0x0F
        self.emit(&[0xB1]);       // OR C
        self.emit(&[0xCA]);       // JP Z, skip_lo_dec
        self.fixup("skip_lo_dec");
        self.emit(&[0x0E, 1]);    // LD C, 1
        self.emit(&[0xC6, 0x30]); // ADD A, '0'
        self.emit(&[0xD3, 0x00]); // OUT (0x00), A
        self.label("skip_lo_dec");

        self.emit(&[0x23]);       // INC HL
        // Decrement B and loop if not zero (manually since DJNZ uses relative)
        self.emit(&[0x05]);       // DEC B
        self.emit(&[0xC2]);       // JP NZ, print_dec_loop
        self.fixup("print_dec_loop");

        // If nothing printed, print 0
        self.emit(&[0x79]);       // LD A, C
        self.emit(&[0xB7]);       // OR A
        self.emit(&[0xC0]);       // RET NZ
        self.emit(&[0x3E, 0x30]); // LD A, '0'
        self.emit(&[0xD3, 0x00]); // OUT (0x00), A
        self.emit(&[0xC9]);       // RET
    }

    fn emit_read_line(&mut self) {
        // Read a line from input port 0x00 into buffer at INPUT_BUF
        // Returns length in A, buffer pointer in HL
        self.label("read_line");

        self.emit(&[0x21]);       // LD HL, INPUT_BUF
        self.emit_word(INPUT_BUF);
        self.emit(&[0x06, 0]);    // LD B, 0 (length counter)

        self.label("read_line_loop");
        // Read character
        self.emit(&[0xDB, 0x00]); // IN A, (0x00)
        self.emit(&[0xB7]);       // OR A (check if 0 = no char)
        self.emit(&[0xCA]);       // JP Z, read_line_loop (poll until char)
        self.fixup("read_line_loop");

        // Check for backspace (0x08 or 0x7F)
        self.emit(&[0xFE, 0x08]); // CP 0x08
        self.emit(&[0xCA]);       // JP Z, read_line_backspace
        self.fixup("read_line_backspace");
        self.emit(&[0xFE, 0x7F]); // CP 0x7F
        self.emit(&[0xCA]);       // JP Z, read_line_backspace
        self.fixup("read_line_backspace");

        // Check for Enter (0x0D or 0x0A)
        self.emit(&[0xFE, 0x0D]); // CP 0x0D
        self.emit(&[0xCA]);       // JP Z, read_line_done
        self.fixup("read_line_done");
        self.emit(&[0xFE, 0x0A]); // CP 0x0A
        self.emit(&[0xCA]);       // JP Z, read_line_done
        self.fixup("read_line_done");

        // Store character and echo
        self.emit(&[0x77]);       // LD (HL), A
        self.emit(&[0xD3, 0x00]); // OUT (0x00), A (echo)
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0x04]);       // INC B
        self.emit(&[0xC3]);       // JP read_line_loop
        self.fixup("read_line_loop");

        // Backspace handling
        self.label("read_line_backspace");
        self.emit(&[0x78]);       // LD A, B
        self.emit(&[0xB7]);       // OR A
        self.emit(&[0xCA]);       // JP Z, read_line_loop (nothing to delete)
        self.fixup("read_line_loop");
        self.emit(&[0x2B]);       // DEC HL
        self.emit(&[0x05]);       // DEC B
        self.emit(&[0x3E, 0x08]); // LD A, 0x08 (backspace)
        self.emit(&[0xD3, 0x00]); // OUT
        self.emit(&[0x3E, 0x20]); // LD A, ' '
        self.emit(&[0xD3, 0x00]); // OUT
        self.emit(&[0x3E, 0x08]); // LD A, 0x08
        self.emit(&[0xD3, 0x00]); // OUT
        self.emit(&[0xC3]);       // JP read_line_loop
        self.fixup("read_line_loop");

        // Done - null terminate
        self.label("read_line_done");
        self.emit(&[0x36, 0x00]); // LD (HL), 0
        self.emit(&[0x78]);       // LD A, B (return length)
        self.emit(&[0x21]);       // LD HL, INPUT_BUF
        self.emit_word(INPUT_BUF);
        self.emit(&[0xC9]);       // RET
    }

    fn emit_mul16(&mut self) {
        // 16-bit multiply: HL = HL * DE
        self.label("mul16");
        self.emit(&[0xC5]);       // PUSH BC
        self.emit(&[0x44]);       // LD B, H
        self.emit(&[0x4D]);       // LD C, L
        self.emit(&[0x21, 0, 0]); // LD HL, 0

        self.label("mul16_loop");
        self.emit(&[0xCB, 0x38]); // SRL B
        self.emit(&[0xCB, 0x19]); // RR C
        self.emit(&[0xD2]);       // JP NC, mul16_no_add
        self.fixup("mul16_no_add");
        self.emit(&[0x19]);       // ADD HL, DE
        self.label("mul16_no_add");
        self.emit(&[0x78]);       // LD A, B
        self.emit(&[0xB1]);       // OR C
        self.emit(&[0xCA]);       // JP Z, mul16_done
        self.fixup("mul16_done");
        self.emit(&[0xCB, 0x23]); // SLA E
        self.emit(&[0xCB, 0x12]); // RL D
        self.emit(&[0xC3]);       // JP mul16_loop
        self.fixup("mul16_loop");

        self.label("mul16_done");
        self.emit(&[0xC1]);       // POP BC
        self.emit(&[0xC9]);       // RET
    }

    fn emit_div16(&mut self) {
        // 16-bit divide: HL = HL / DE, remainder in A
        self.label("div16");
        self.emit(&[0xC5]);       // PUSH BC

        // Check for divide by zero
        self.emit(&[0x7A]);       // LD A, D
        self.emit(&[0xB3]);       // OR E
        self.emit(&[0xCA]);       // JP Z, div16_zero
        self.fixup("div16_zero");

        self.emit(&[0x01, 0, 0]); // LD BC, 0 (quotient)
        self.emit(&[0xF5]);       // PUSH AF (save for later)

        // Shift-subtract algorithm
        self.emit(&[0x3E, 16]);   // LD A, 16 (bit counter)

        self.label("div16_loop");
        self.emit(&[0xF5]);       // PUSH AF
        // Shift HL left, carry into BC
        self.emit(&[0x29]);       // ADD HL, HL
        self.emit(&[0xCB, 0x11]); // RL C
        self.emit(&[0xCB, 0x10]); // RL B

        // Try subtract DE from BC
        self.emit(&[0x79]);       // LD A, C
        self.emit(&[0x93]);       // SUB E
        self.emit(&[0x4F]);       // LD C, A
        self.emit(&[0x78]);       // LD A, B
        self.emit(&[0x9A]);       // SBC D
        self.emit(&[0x47]);       // LD B, A
        self.emit(&[0xD2]);       // JP NC, div16_ok
        self.fixup("div16_ok");

        // Restore BC
        self.emit(&[0x79]);       // LD A, C
        self.emit(&[0x83]);       // ADD E
        self.emit(&[0x4F]);       // LD C, A
        self.emit(&[0x78]);       // LD A, B
        self.emit(&[0x8A]);       // ADC D
        self.emit(&[0x47]);       // LD B, A
        self.emit(&[0xF1]);       // POP AF
        self.emit(&[0x3D]);       // DEC A
        self.emit(&[0xC2]);       // JP NZ, div16_loop
        self.fixup("div16_loop");
        self.emit(&[0xC3]);       // JP div16_result
        self.fixup("div16_result");

        self.label("div16_ok");
        self.emit(&[0xCB, 0xC5]); // SET 0, L (set quotient bit)
        self.emit(&[0xF1]);       // POP AF
        self.emit(&[0x3D]);       // DEC A
        self.emit(&[0xC2]);       // JP NZ, div16_loop
        self.fixup("div16_loop");

        self.label("div16_result");
        // HL has quotient, BC has remainder
        // First pop and discard the saved AF, then get remainder
        self.emit(&[0xF1]);       // POP AF (discard saved)
        self.emit(&[0x79]);       // LD A, C (remainder low byte)
        self.emit(&[0xC1]);       // POP BC
        self.emit(&[0xC9]);       // RET

        self.label("div16_zero");
        self.emit(&[0x21, 0, 0]); // LD HL, 0
        self.emit(&[0xAF]);       // XOR A
        self.emit(&[0xC1]);       // POP BC
        self.emit(&[0xC9]);       // RET
    }

    fn emit_lexer(&mut self) {
        // Lexer: tokenize input at HL
        // Returns token type in A, value in DE (if applicable)
        // Advances HL past consumed characters
        self.label("lex_next");

        // Skip whitespace
        // Byte positions: 0:LD, 1:CP, 3:JR, 5:CP, 7:JR, 9:INC, 10:JR, 12:lex_check_char
        self.label("lex_skip_ws");
        self.emit(&[0x7E]);       // LD A, (HL) - pos 0
        self.emit(&[0xFE, 0x20]); // CP ' ' - pos 1-2
        self.emit(&[0x28, 0x04]); // JR Z, lex_skip_ws_inc (+4: from pos 5 to pos 9)
        self.emit(&[0xFE, 0x09]); // CP '\t' - pos 5-6
        self.emit(&[0x20, 0x03]); // JR NZ, lex_check_char (+3: from pos 9 to pos 12)
        self.label("lex_skip_ws_inc");
        self.emit(&[0x23]);       // INC HL - pos 9
        self.emit(&[0x18, 0xF4]); // JR lex_skip_ws (-12: from pos 12 back to pos 0) - pos 10-11

        self.label("lex_check_char");
        self.emit(&[0x7E]);       // LD A, (HL)

        // Check for end of input
        self.emit(&[0xB7]);       // OR A
        self.emit(&[0xC8]);       // RET Z (A=0=EOF)

        // Check for line comment (--)
        self.emit(&[0xFE, 0x2D]); // CP '-'
        self.emit(&[0xC2]);       // JP NZ, lex_not_line_comment
        self.fixup("lex_not_line_comment");
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xFE, 0x2D]); // CP '-'
        self.emit(&[0xCA]);       // JP Z, lex_line_comment
        self.fixup("lex_line_comment");
        self.emit(&[0x2B]);       // DEC HL (not a comment, put back)
        self.emit(&[0x7E]);       // LD A, (HL) (reload char)
        self.label("lex_not_line_comment");

        // Check for block comment ((*)
        self.emit(&[0xFE, 0x28]); // CP '('
        self.emit(&[0xC2]);       // JP NZ, lex_not_block_comment
        self.fixup("lex_not_block_comment");
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xFE, 0x2A]); // CP '*'
        self.emit(&[0xCA]);       // JP Z, lex_block_comment
        self.fixup("lex_block_comment");
        self.emit(&[0x2B]);       // DEC HL (not a comment, put back)
        self.emit(&[0x7E]);       // LD A, (HL) (reload char)
        self.label("lex_not_block_comment");

        // Check for newline
        self.emit(&[0xFE, 0x0D]); // CP '\r'
        self.emit(&[0x28, 0x04]); // JR Z, lex_newline
        self.emit(&[0xFE, 0x0A]); // CP '\n'
        self.emit(&[0x20, 0x04]); // JR NZ, lex_not_newline
        self.label("lex_newline");
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0x3E, tok::NEWLINE]); // LD A, NEWLINE
        self.emit(&[0xC9]);       // RET

        self.label("lex_not_newline");
        // Check for digit (number)
        self.emit(&[0xFE, 0x30]); // CP '0'
        self.emit(&[0x38, 0x07]); // JR C, lex_not_number (+7: 2+2+3)
        self.emit(&[0xFE, 0x3A]); // CP '9'+1
        self.emit(&[0x30, 0x03]); // JR NC, lex_not_number (+3: JP is 3 bytes)
        self.emit(&[0xC3]);       // JP lex_number (too far for JR)
        self.fixup("lex_number");

        self.label("lex_not_number");
        // Check for letter (identifier or keyword)
        // Must handle both uppercase (A-Z) and lowercase (a-z) due to emulator case conversion
        // Byte positions: 0:CP, 2:JR, 4:CP, 6:JR, 8:CP, 10:JR, 12:CP, 14:JR, 16:JP(3), 19:not_ident
        self.emit(&[0xFE, 0x41]); // CP 'A'
        self.emit(&[0x38, 0x0F]); // JR C, lex_not_ident (+15: skip to pos 19)
        self.emit(&[0xFE, 0x5B]); // CP 'Z'+1
        self.emit(&[0x38, 0x08]); // JR C, lex_ident_upper (+8: jump to pos 16)
        self.emit(&[0xFE, 0x61]); // CP 'a'
        self.emit(&[0x38, 0x07]); // JR C, lex_not_ident (+7: skip to pos 19)
        self.emit(&[0xFE, 0x7B]); // CP 'z'+1
        self.emit(&[0x30, 0x03]); // JR NC, lex_not_ident (+3: skip to pos 19)
        self.label("lex_ident_upper");
        self.emit(&[0xC3]);       // JP lex_ident
        self.fixup("lex_ident");

        self.label("lex_not_ident");
        // Single character tokens
        self.emit(&[0x23]);       // INC HL (consume char)

        self.emit(&[0xFE, 0x2B]); // CP '+'
        self.emit(&[0x20, 0x03]); // JR NZ, +
        self.emit(&[0x3E, tok::PLUS]);
        self.emit(&[0xC9]);

        self.emit(&[0xFE, 0x2D]); // CP '-'
        self.emit(&[0x20, 0x03]); // JR NZ, +
        self.emit(&[0x3E, tok::MINUS]);
        self.emit(&[0xC9]);

        self.emit(&[0xFE, 0x2A]); // CP '*'
        self.emit(&[0x20, 0x03]); // JR NZ, +
        self.emit(&[0x3E, tok::STAR]);
        self.emit(&[0xC9]);

        self.emit(&[0xFE, 0x2F]); // CP '/'
        self.emit(&[0x20, 0x03]); // JR NZ, +
        self.emit(&[0x3E, tok::SLASH]);
        self.emit(&[0xC9]);

        self.emit(&[0xFE, 0x25]); // CP '%'
        self.emit(&[0x20, 0x03]); // JR NZ, +
        self.emit(&[0x3E, tok::MOD]);
        self.emit(&[0xC9]);

        self.emit(&[0xFE, 0x28]); // CP '('
        self.emit(&[0x20, 0x03]); // JR NZ, +
        self.emit(&[0x3E, tok::LPAREN]);
        self.emit(&[0xC9]);

        self.emit(&[0xFE, 0x29]); // CP ')'
        self.emit(&[0x20, 0x03]); // JR NZ, +
        self.emit(&[0x3E, tok::RPAREN]);
        self.emit(&[0xC9]);

        self.emit(&[0xFE, 0x3B]); // CP ';'
        self.emit(&[0x20, 0x03]); // JR NZ, +
        self.emit(&[0x3E, tok::SEMI]);
        self.emit(&[0xC9]);

        self.emit(&[0xFE, 0x21]); // CP '!'
        self.emit(&[0x20, 0x09]); // JR NZ, +9 (skip !=  handling, 9 bytes)
        // Check for !=
        self.emit(&[0x7E]);       // LD A, (HL) - 1 byte
        self.emit(&[0xFE, 0x3D]); // CP '=' - 2 bytes
        self.emit(&[0x20, 0x04]); // JR NZ, +4 (skip to next check) - 2 bytes
        self.emit(&[0x23]);       // INC HL - 1 byte
        self.emit(&[0x3E, tok::NE]); // LD A, NE - 2 bytes
        self.emit(&[0xC9]);       // RET - 1 byte (total 9 bytes)
        // Lone '!' is not a valid token, fall through to next check

        self.emit(&[0xFE, 0x3D]); // CP '='
        self.emit(&[0x20, 0x0C]); // JR NZ, +12 (skip = handling: 12 bytes)
        // Check for ==
        self.emit(&[0x7E]);       // LD A, (HL) - 1
        self.emit(&[0xFE, 0x3D]); // CP '=' - 2
        self.emit(&[0x20, 0x04]); // JR NZ, +4 (skip to EQ) - 2
        self.emit(&[0x23]);       // INC HL - 1
        self.emit(&[0x3E, tok::EQEQ]); // - 2
        self.emit(&[0xC9]);       // - 1
        self.emit(&[0x3E, tok::EQ]); // - 2
        self.emit(&[0xC9]);       // - 1 (total 12)

        self.emit(&[0xFE, 0x3C]); // CP '<'
        self.emit(&[0xC2]);       // JP NZ, lex_not_lt
        self.fixup("lex_not_lt");
        // Check for <= or <<
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xFE, 0x3D]); // CP '='
        self.emit(&[0x20, 0x04]); // JR NZ, +4
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0x3E, tok::LE]);
        self.emit(&[0xC9]);
        self.emit(&[0xFE, 0x3C]); // CP '<'
        self.emit(&[0x20, 0x04]); // JR NZ, +4
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0x3E, tok::SHL]);
        self.emit(&[0xC9]);
        self.emit(&[0x3E, tok::LT]);
        self.emit(&[0xC9]);
        self.label("lex_not_lt");

        self.emit(&[0xFE, 0x3E]); // CP '>'
        self.emit(&[0xC2]);       // JP NZ, lex_not_gt
        self.fixup("lex_not_gt");
        // Check for >= or >>
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xFE, 0x3D]); // CP '='
        self.emit(&[0x20, 0x04]); // JR NZ, +4
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0x3E, tok::GE]);
        self.emit(&[0xC9]);
        self.emit(&[0xFE, 0x3E]); // CP '>'
        self.emit(&[0x20, 0x04]); // JR NZ, +4
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0x3E, tok::SHR]);
        self.emit(&[0xC9]);
        self.emit(&[0x3E, tok::GT]);
        self.emit(&[0xC9]);
        self.label("lex_not_gt");

        // Bitwise AND: &
        self.emit(&[0xFE, 0x26]); // CP '&'
        self.emit(&[0x20, 0x03]); // JR NZ, +3
        self.emit(&[0x3E, tok::BAND]);
        self.emit(&[0xC9]);

        // Bitwise OR: |
        self.emit(&[0xFE, 0x7C]); // CP '|'
        self.emit(&[0x20, 0x03]); // JR NZ, +3
        self.emit(&[0x3E, tok::BOR]);
        self.emit(&[0xC9]);

        // Bitwise XOR: ^
        self.emit(&[0xFE, 0x5E]); // CP '^'
        self.emit(&[0x20, 0x03]); // JR NZ, +3
        self.emit(&[0x3E, tok::BXOR]);
        self.emit(&[0xC9]);

        // Bitwise NOT: ~
        self.emit(&[0xFE, 0x7E]); // CP '~'
        self.emit(&[0x20, 0x03]); // JR NZ, +3
        self.emit(&[0x3E, tok::BNOT]);
        self.emit(&[0xC9]);

        // Assignment: :=
        self.emit(&[0xFE, 0x3A]); // CP ':'
        self.emit(&[0x20, 0x09]); // JR NZ, +9 (skip := handling)
        self.emit(&[0x7E]);       // LD A, (HL) - look at next char
        self.emit(&[0xFE, 0x3D]); // CP '='
        self.emit(&[0x20, 0x04]); // JR NZ, +4 (not :=, skip)
        self.emit(&[0x23]);       // INC HL - consume '='
        self.emit(&[0x3E, tok::ASSIGN]);
        self.emit(&[0xC9]);       // RET
        // Lone ':' falls through to unknown

        // Unknown character - return EOF
        self.emit(&[0xAF]);       // XOR A
        self.emit(&[0xC9]);       // RET

        // Line comment: skip until newline or end of input
        self.label("lex_line_comment");
        self.emit(&[0x23]);       // INC HL (skip second '-')
        self.label("lex_line_comment_loop");
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xB7]);       // OR A
        self.emit(&[0xCA]);       // JP Z, lex_next (end of input, get next token)
        self.fixup("lex_next");
        self.emit(&[0xFE, 0x0D]); // CP '\r'
        self.emit(&[0xCA]);       // JP Z, lex_next (hit newline, get next token)
        self.fixup("lex_next");
        self.emit(&[0xFE, 0x0A]); // CP '\n'
        self.emit(&[0xCA]);       // JP Z, lex_next (hit newline, get next token)
        self.fixup("lex_next");
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0xC3]);       // JP lex_line_comment_loop
        self.fixup("lex_line_comment_loop");

        // Block comment: skip until *)
        self.label("lex_block_comment");
        self.emit(&[0x23]);       // INC HL (skip '*')
        self.label("lex_block_comment_loop");
        // Byte positions: 0:LD, 1:OR, 2:JP(3), 5:CP(2), 7:JR(2), 9:INC, 10:LD, 11:CP(2), 13:JR(2), 15:INC, 16:JP(3), 19:next
        self.emit(&[0x7E]);       // LD A, (HL) - pos 0
        self.emit(&[0xB7]);       // OR A - pos 1
        self.emit(&[0xCA]);       // JP Z, lex_next - pos 2
        self.fixup("lex_next");
        self.emit(&[0xFE, 0x2A]); // CP '*' - pos 5
        self.emit(&[0x20, 0x0A]); // JR NZ, lex_block_comment_next (+10: 9+10=19) - pos 7
        // Found '*', check for ')'
        self.emit(&[0x23]);       // INC HL - pos 9
        self.emit(&[0x7E]);       // LD A, (HL) - pos 10
        self.emit(&[0xFE, 0x29]); // CP ')' - pos 11
        self.emit(&[0x20, 0x04]); // JR NZ, lex_block_comment_next (+4: 15+4=19) - pos 13
        // Found '*)', end of comment
        self.emit(&[0x23]);       // INC HL (skip ')') - pos 15
        self.emit(&[0xC3]);       // JP lex_next - pos 16
        self.fixup("lex_next");
        self.label("lex_block_comment_next"); // pos 19
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0xC3]);       // JP lex_block_comment_loop
        self.fixup("lex_block_comment_loop");

        // Number lexing - uses IX to preserve input pointer
        self.label("lex_number");
        self.emit(&[0xDD, 0xE5]); // PUSH IX (save IX)
        self.emit(&[0xE5]);       // PUSH HL (save input pointer)
        self.emit(&[0xDD, 0xE1]); // POP IX (IX = input pointer)
        self.emit(&[0x21, 0, 0]); // LD HL, 0 (accumulator)

        // Check for hex prefix 0x or 0X
        self.emit(&[0xDD, 0x7E, 0]); // LD A, (IX+0) - first char
        self.emit(&[0xFE, 0x30]); // CP '0'
        self.emit(&[0xC2]);       // JP NZ, lex_num_loop (not '0', parse decimal)
        self.fixup("lex_num_loop");
        self.emit(&[0xDD, 0x7E, 1]); // LD A, (IX+1) - second char
        self.emit(&[0xFE, 0x58]); // CP 'X' (emulator converts to uppercase)
        self.emit(&[0xCA]);       // JP Z, lex_hex
        self.fixup("lex_hex");
        self.emit(&[0xFE, 0x78]); // CP 'x' (lowercase just in case)
        self.emit(&[0xCA]);       // JP Z, lex_hex
        self.fixup("lex_hex");
        // Not hex, fall through to decimal

        self.label("lex_num_loop");
        self.emit(&[0xDD, 0x7E, 0]); // LD A, (IX+0)
        self.emit(&[0xFE, 0x30]); // CP '0'
        self.emit(&[0xDA]);       // JP C, lex_num_done
        self.fixup("lex_num_done");
        self.emit(&[0xFE, 0x3A]); // CP '9'+1
        self.emit(&[0xD2]);       // JP NC, lex_num_done
        self.fixup("lex_num_done");

        // digit = A - '0'
        self.emit(&[0xD6, 0x30]); // SUB '0'
        self.emit(&[0x4F]);       // LD C, A (save digit)
        self.emit(&[0x06, 0]);    // LD B, 0

        // HL = HL * 10 + digit
        self.emit(&[0x29]);       // ADD HL, HL (x2)
        self.emit(&[0x54]);       // LD D, H
        self.emit(&[0x5D]);       // LD E, L (DE = x2)
        self.emit(&[0x29]);       // ADD HL, HL (x4)
        self.emit(&[0x29]);       // ADD HL, HL (x8)
        self.emit(&[0x19]);       // ADD HL, DE (x8 + x2 = x10)
        self.emit(&[0x09]);       // ADD HL, BC (+ digit)

        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xC3]);       // JP lex_num_loop
        self.fixup("lex_num_loop");

        self.label("lex_num_done");
        // Check if next char is '.' for decimal number
        self.emit(&[0xDD, 0x7E, 0]); // LD A, (IX+0)
        self.emit(&[0xFE, 0x2E]); // CP '.'
        self.emit(&[0xC2]);       // JP NZ, lex_num_return_int (not a decimal)
        self.fixup("lex_num_return_int");

        // It's a decimal! Parse fractional part
        // HL has integer part, skip the '.'
        self.emit(&[0xDD, 0x23]); // INC IX (skip '.')

        // Multiply integer part by 100 for 2 decimal places
        // HL = HL * 100
        self.emit(&[0xD5]);       // PUSH DE (save)
        self.emit(&[0x54]);       // LD D, H
        self.emit(&[0x5D]);       // LD E, L (DE = HL)
        self.emit(&[0x29]);       // ADD HL, HL (x2)
        self.emit(&[0x29]);       // ADD HL, HL (x4)
        self.emit(&[0x19]);       // ADD HL, DE (x5)
        self.emit(&[0x29]);       // ADD HL, HL (x10)
        self.emit(&[0x29]);       // ADD HL, HL (x20)
        self.emit(&[0x54]);       // LD D, H
        self.emit(&[0x5D]);       // LD E, L (DE = x20)
        self.emit(&[0x29]);       // ADD HL, HL (x40)
        self.emit(&[0x29]);       // ADD HL, HL (x80)
        self.emit(&[0x19]);       // ADD HL, DE (x80 + x20 = x100)
        self.emit(&[0xD1]);       // POP DE (restore)

        // Now parse up to 2 fractional digits
        // HL = integer_part * 100
        // First digit (tenths place): multiply by 10 and add
        self.emit(&[0xDD, 0x7E, 0]); // LD A, (IX+0)
        self.emit(&[0xFE, 0x30]); // CP '0'
        self.emit(&[0xDA]);       // JP C, lex_dec_done (not a digit)
        self.fixup("lex_dec_done");
        self.emit(&[0xFE, 0x3A]); // CP '9'+1
        self.emit(&[0xD2]);       // JP NC, lex_dec_done
        self.fixup("lex_dec_done");
        // digit value = A - '0'
        self.emit(&[0xD6, 0x30]); // SUB '0'
        self.emit(&[0x4F]);       // LD C, A (save digit)
        self.emit(&[0x06, 0]);    // LD B, 0
        // BC = digit, need to add digit*10 to HL
        // digit*10 = digit*8 + digit*2
        self.emit(&[0xC5]);       // PUSH BC (save digit)
        self.emit(&[0xCB, 0x21]); // SLA C (x2)
        self.emit(&[0xCB, 0x10]); // RL B
        self.emit(&[0xD5]);       // PUSH DE (save)
        self.emit(&[0x50]);       // LD D, B
        self.emit(&[0x59]);       // LD E, C (DE = digit*2)
        self.emit(&[0xCB, 0x21]); // SLA C (x4)
        self.emit(&[0xCB, 0x10]); // RL B
        self.emit(&[0xCB, 0x21]); // SLA C (x8)
        self.emit(&[0xCB, 0x10]); // RL B
        self.emit(&[0x09]);       // ADD HL, BC (+ digit*8)
        self.emit(&[0x19]);       // ADD HL, DE (+ digit*2 = digit*10 total)
        self.emit(&[0xD1]);       // POP DE
        self.emit(&[0xC1]);       // POP BC (discard saved digit)
        self.emit(&[0xDD, 0x23]); // INC IX

        // Second digit (hundredths place): just add
        self.emit(&[0xDD, 0x7E, 0]); // LD A, (IX+0)
        self.emit(&[0xFE, 0x30]); // CP '0'
        self.emit(&[0xDA]);       // JP C, lex_dec_done
        self.fixup("lex_dec_done");
        self.emit(&[0xFE, 0x3A]); // CP '9'+1
        self.emit(&[0xD2]);       // JP NC, lex_dec_done
        self.fixup("lex_dec_done");
        self.emit(&[0xD6, 0x30]); // SUB '0'
        self.emit(&[0x4F]);       // LD C, A
        self.emit(&[0x06, 0]);    // LD B, 0
        self.emit(&[0x09]);       // ADD HL, BC
        self.emit(&[0xDD, 0x23]); // INC IX

        self.label("lex_dec_done");
        // Return with tok::DEC, value in DE
        self.emit(&[0x54]);       // LD D, H
        self.emit(&[0x5D]);       // LD E, L (DE = result)
        self.emit(&[0xDD, 0xE5]); // PUSH IX
        self.emit(&[0xE1]);       // POP HL (HL = updated input pointer)
        self.emit(&[0xDD, 0xE1]); // POP IX (restore IX)
        self.emit(&[0x3E, tok::DEC]);
        self.emit(&[0xC9]);       // RET

        self.label("lex_num_return_int");
        // Move result to DE, restore HL from IX
        self.emit(&[0x54]);       // LD D, H
        self.emit(&[0x5D]);       // LD E, L (DE = result)
        self.emit(&[0xDD, 0xE5]); // PUSH IX
        self.emit(&[0xE1]);       // POP HL (HL = updated input pointer)
        self.emit(&[0xDD, 0xE1]); // POP IX (restore IX)
        self.emit(&[0x3E, tok::INT]);
        self.emit(&[0xC9]);       // RET (DE has value, HL = updated pointer)

        // Hex number parsing
        self.label("lex_hex");
        self.emit(&[0xDD, 0x23]); // INC IX (skip '0')
        self.emit(&[0xDD, 0x23]); // INC IX (skip 'x')

        self.label("lex_hex_loop");
        self.emit(&[0xDD, 0x7E, 0]); // LD A, (IX+0)
        // Check if digit 0-9
        self.emit(&[0xFE, 0x30]); // CP '0'
        self.emit(&[0xDA]);       // JP C, lex_hex_check_letter
        self.fixup("lex_hex_check_letter");
        self.emit(&[0xFE, 0x3A]); // CP '9'+1
        self.emit(&[0xD2]);       // JP NC, lex_hex_check_letter
        self.fixup("lex_hex_check_letter");
        // It's 0-9
        self.emit(&[0xD6, 0x30]); // SUB '0'
        self.emit(&[0xC3]);       // JP lex_hex_add
        self.fixup("lex_hex_add");

        self.label("lex_hex_check_letter");
        // Check if A-F (uppercase from emulator)
        self.emit(&[0xFE, 0x41]); // CP 'A'
        self.emit(&[0xDA]);       // JP C, lex_num_done (not a hex digit)
        self.fixup("lex_num_done");
        self.emit(&[0xFE, 0x47]); // CP 'G' (F+1)
        self.emit(&[0xD2]);       // JP NC, lex_num_done
        self.fixup("lex_num_done");
        // It's A-F
        self.emit(&[0xD6, 0x37]); // SUB 'A'-10 = 55 = 0x37

        self.label("lex_hex_add");
        // A has digit value (0-15), add to HL*16
        self.emit(&[0x4F]);       // LD C, A
        self.emit(&[0x06, 0]);    // LD B, 0
        self.emit(&[0x29]);       // ADD HL, HL (x2)
        self.emit(&[0x29]);       // ADD HL, HL (x4)
        self.emit(&[0x29]);       // ADD HL, HL (x8)
        self.emit(&[0x29]);       // ADD HL, HL (x16)
        self.emit(&[0x09]);       // ADD HL, BC (+ digit)
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xC3]);       // JP lex_hex_loop
        self.fixup("lex_hex_loop");

        // Identifier lexing
        self.label("lex_ident");
        // Store start pointer in DE (preserve HL for loop)
        self.emit(&[0xD5]);       // PUSH DE (save original DE)
        self.emit(&[0x54]);       // LD D, H
        self.emit(&[0x5D]);       // LD E, L (DE = HL = start)

        self.label("lex_ident_loop");
        self.emit(&[0x7E]);       // LD A, (HL)
        // Check if alphanumeric (A-Z, a-z, 0-9)
        // Check uppercase A-Z
        self.emit(&[0xFE, 0x41]); // CP 'A'
        self.emit(&[0xDA]);       // JP C, lex_ident_check_lower
        self.fixup("lex_ident_check_lower");
        self.emit(&[0xFE, 0x5B]); // CP 'Z'+1
        self.emit(&[0xDA]);       // JP C, lex_ident_cont
        self.fixup("lex_ident_cont");

        self.label("lex_ident_check_lower");
        // Check lowercase a-z
        self.emit(&[0xFE, 0x61]); // CP 'a'
        self.emit(&[0xDA]);       // JP C, lex_ident_check_num
        self.fixup("lex_ident_check_num");
        self.emit(&[0xFE, 0x7B]); // CP 'z'+1
        self.emit(&[0xDA]);       // JP C, lex_ident_cont
        self.fixup("lex_ident_cont");

        self.label("lex_ident_check_num");
        self.emit(&[0xFE, 0x30]); // CP '0'
        self.emit(&[0xDA]);       // JP C, lex_ident_done
        self.fixup("lex_ident_done");
        self.emit(&[0xFE, 0x3A]); // CP '9'+1
        self.emit(&[0xD2]);       // JP NC, lex_ident_done
        self.fixup("lex_ident_done");

        self.label("lex_ident_cont");
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0xC3]);       // JP lex_ident_loop
        self.fixup("lex_ident_loop");

        self.label("lex_ident_done");
        // Stack: [original DE], DE = start, HL = end
        // Save first char of identifier for variable lookup
        self.emit(&[0x1A]);       // LD A, (DE) - first char
        self.emit(&[0x32]);       // LD (SCRATCH), A
        self.emit_word(SCRATCH);
        // Save end pointer
        self.emit(&[0xE5]);       // PUSH HL (end)
        // HL = start for keyword check
        self.emit(&[0x62]);       // LD H, D
        self.emit(&[0x6B]);       // LD L, E

        // Simple keyword check for "LET" (uppercase due to emulator conversion)
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xFE, 0x4C]); // CP 'L'
        self.emit(&[0xC2]);       // JP NZ, lex_ident_not_let
        self.fixup("lex_ident_not_let");
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xFE, 0x45]); // CP 'E'
        self.emit(&[0xC2]);       // JP NZ, lex_ident_not_let
        self.fixup("lex_ident_not_let");
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xFE, 0x54]); // CP 'T'
        self.emit(&[0xC2]);       // JP NZ, lex_ident_not_let
        self.fixup("lex_ident_not_let");
        self.emit(&[0x23]);       // INC HL (past 'T')
        // Check next char is not alphanumeric (end of "LET")
        // Positions from LD A: 0:LD, 1:CP, 3:JR, 5:CP, 7:JR, 9:CP, 11:JR, 13:CP, 15:JR, 17:is_let(5 bytes), 22:not_let
        self.emit(&[0x7E]);       // LD A, (HL)
        // Check if not A-Z
        self.emit(&[0xFE, 0x41]); // CP 'A'
        self.emit(&[0x38, 0x0C]); // JR C, is_let (+12: jump to pos 17)
        self.emit(&[0xFE, 0x5B]); // CP 'Z'+1
        self.emit(&[0x38, 0x0D]); // JR C, lex_ident_not_let (+13: it's A-Z, longer word)
        // Check if not a-z
        self.emit(&[0xFE, 0x61]); // CP 'a'
        self.emit(&[0x38, 0x04]); // JR C, is_let (+4: it's <'a' and >='Z'+1)
        self.emit(&[0xFE, 0x7B]); // CP 'z'+1
        self.emit(&[0x38, 0x05]); // JR C, lex_ident_not_let (+5: it's a-z, longer word)
        self.label("is_let");
        // It's "let"! HL is past "let"
        self.emit(&[0xF1]);       // POP AF (discard saved end)
        self.emit(&[0xD1]);       // POP DE (restore original DE)
        self.emit(&[0x3E, tok::LET]);
        self.emit(&[0xC9]);

        self.label("lex_ident_not_let");
        // Stack: [orig DE, end], HL = somewhere in keyword check
        // Restore HL to start for next keyword check
        self.emit(&[0xE1]);       // POP HL (end) - save for later
        self.emit(&[0xD5]);       // PUSH DE (keep start on stack)
        self.emit(&[0xE5]);       // PUSH HL (put end back)
        // Now stack: [orig DE, start, end]
        // Get start into HL
        self.emit(&[0xD1]);       // POP DE (end into DE temporarily)
        self.emit(&[0xE1]);       // POP HL (start)
        self.emit(&[0xD5]);       // PUSH DE (put end back)
        self.emit(&[0xE5]);       // PUSH HL (put start back for next check)
        // Now: HL = start, stack: [orig DE, end, start]

        // Check for "AND" keyword (uppercase)
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xFE, 0x41]); // CP 'A'
        self.emit(&[0xC2]);       // JP NZ, lex_ident_not_and
        self.fixup("lex_ident_not_and");
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xFE, 0x4E]); // CP 'N'
        self.emit(&[0xC2]);       // JP NZ, lex_ident_not_and
        self.fixup("lex_ident_not_and");
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xFE, 0x44]); // CP 'D'
        self.emit(&[0xC2]);       // JP NZ, lex_ident_not_and
        self.fixup("lex_ident_not_and");
        self.emit(&[0x23]);       // INC HL
        // Check next char is not alphanumeric
        // Positions: 0:LD, 1-2:CP, 3-4:JR, 5-6:CP, 7-8:JR, 9-10:CP, 11-12:JR, 13-14:CP, 15-16:JR, 17:is_and(6 bytes), 23:not_and
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xFE, 0x41]); // CP 'A'
        self.emit(&[0x38, 0x0C]); // JR C, is_and (+12: jump from PC=5 to pos 17)
        self.emit(&[0xFE, 0x5B]); // CP 'Z'+1
        self.emit(&[0x38, 0x0E]); // JR C, lex_ident_not_and (+14: jump from PC=9 to pos 23)
        self.emit(&[0xFE, 0x61]); // CP 'a'
        self.emit(&[0x38, 0x04]); // JR C, is_and (+4: jump from PC=13 to pos 17)
        self.emit(&[0xFE, 0x7B]); // CP 'z'+1
        self.emit(&[0x38, 0x06]); // JR C, lex_ident_not_and (+6: jump from PC=17 to pos 23)
        self.label("is_and");
        self.emit(&[0xE1]);       // POP HL (discard start)
        self.emit(&[0xE1]);       // POP HL (discard end)
        self.emit(&[0xD1]);       // POP DE (restore original DE)
        self.emit(&[0x3E, tok::AND_KW]);
        self.emit(&[0xC9]);

        self.label("lex_ident_not_and");
        // Stack: [orig DE, end, start]
        // Restore HL to start for next check
        self.emit(&[0xE1]);       // POP HL (start)
        self.emit(&[0xE5]);       // PUSH HL (put start back)

        // Check for "OR" keyword (uppercase)
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xFE, 0x4F]); // CP 'O'
        self.emit(&[0xC2]);       // JP NZ, lex_ident_not_or
        self.fixup("lex_ident_not_or");
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xFE, 0x52]); // CP 'R'
        self.emit(&[0xC2]);       // JP NZ, lex_ident_not_or
        self.fixup("lex_ident_not_or");
        self.emit(&[0x23]);       // INC HL
        // Check next char is not alphanumeric
        // Positions: 0:LD, 1-2:CP, 3-4:JR, 5-6:CP, 7-8:JR, 9-10:CP, 11-12:JR, 13-14:CP, 15-16:JR, 17:is_or(6 bytes), 23:not_or
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xFE, 0x41]); // CP 'A'
        self.emit(&[0x38, 0x0C]); // JR C, is_or (+12: jump from PC=5 to pos 17)
        self.emit(&[0xFE, 0x5B]); // CP 'Z'+1
        self.emit(&[0x38, 0x0E]); // JR C, lex_ident_not_or (+14: jump from PC=9 to pos 23)
        self.emit(&[0xFE, 0x61]); // CP 'a'
        self.emit(&[0x38, 0x04]); // JR C, is_or (+4: jump from PC=13 to pos 17)
        self.emit(&[0xFE, 0x7B]); // CP 'z'+1
        self.emit(&[0x38, 0x06]); // JR C, lex_ident_not_or (+6: jump from PC=17 to pos 23)
        self.label("is_or");
        self.emit(&[0xE1]);       // POP HL (discard start)
        self.emit(&[0xE1]);       // POP HL (discard end)
        self.emit(&[0xD1]);       // POP DE (restore original DE)
        self.emit(&[0x3E, tok::OR_KW]);
        self.emit(&[0xC9]);

        self.label("lex_ident_not_or");
        // Stack: [orig DE, end, start]
        // Restore HL to start for next check
        self.emit(&[0xE1]);       // POP HL (start)
        self.emit(&[0xE5]);       // PUSH HL (put start back)

        // Check for "NOT" keyword (uppercase)
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xFE, 0x4E]); // CP 'N'
        self.emit(&[0xC2]);       // JP NZ, lex_ident_not_not
        self.fixup("lex_ident_not_not");
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xFE, 0x4F]); // CP 'O'
        self.emit(&[0xC2]);       // JP NZ, lex_ident_not_not
        self.fixup("lex_ident_not_not");
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xFE, 0x54]); // CP 'T'
        self.emit(&[0xC2]);       // JP NZ, lex_ident_not_not
        self.fixup("lex_ident_not_not");
        self.emit(&[0x23]);       // INC HL
        // Check next char is not alphanumeric
        // Positions: 0:LD, 1-2:CP, 3-4:JR, 5-6:CP, 7-8:JR, 9-10:CP, 11-12:JR, 13-14:CP, 15-16:JR, 17:is_not(6 bytes), 23:not_not
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xFE, 0x41]); // CP 'A'
        self.emit(&[0x38, 0x0C]); // JR C, is_not (+12: jump from PC=5 to pos 17)
        self.emit(&[0xFE, 0x5B]); // CP 'Z'+1
        self.emit(&[0x38, 0x0E]); // JR C, lex_ident_not_not (+14: jump from PC=9 to pos 23)
        self.emit(&[0xFE, 0x61]); // CP 'a'
        self.emit(&[0x38, 0x04]); // JR C, is_not (+4: jump from PC=13 to pos 17)
        self.emit(&[0xFE, 0x7B]); // CP 'z'+1
        self.emit(&[0x38, 0x06]); // JR C, lex_ident_not_not (+6: jump from PC=17 to pos 23)
        self.label("is_not");
        self.emit(&[0xE1]);       // POP HL (discard start)
        self.emit(&[0xE1]);       // POP HL (discard end)
        self.emit(&[0xD1]);       // POP DE (restore original DE)
        self.emit(&[0x3E, tok::NOT_KW]);
        self.emit(&[0xC9]);

        self.label("lex_ident_not_not");
        // Stack: [orig DE, end, start]
        // Restore HL to start for next check
        self.emit(&[0xE1]);       // POP HL (start)
        self.emit(&[0xE5]);       // PUSH HL (put start back)

        // Check for "IF" keyword (uppercase)
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xFE, 0x49]); // CP 'I'
        self.emit(&[0xC2]);       // JP NZ, lex_ident_not_if
        self.fixup("lex_ident_not_if");
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xFE, 0x46]); // CP 'F'
        self.emit(&[0xC2]);       // JP NZ, lex_ident_not_if
        self.fixup("lex_ident_not_if");
        self.emit(&[0x23]);       // INC HL
        // Check next char is not alphanumeric
        // Positions: 0:LD, 1-2:CP, 3-4:JR, 5-6:CP, 7-8:JR, 9-10:CP, 11-12:JR, 13-14:CP, 15-16:JR, 17:is_if(6 bytes), 23:not_if
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xFE, 0x41]); // CP 'A'
        self.emit(&[0x38, 0x0C]); // JR C, is_if (+12)
        self.emit(&[0xFE, 0x5B]); // CP 'Z'+1
        self.emit(&[0x38, 0x0E]); // JR C, lex_ident_not_if (+14)
        self.emit(&[0xFE, 0x61]); // CP 'a'
        self.emit(&[0x38, 0x04]); // JR C, is_if (+4)
        self.emit(&[0xFE, 0x7B]); // CP 'z'+1
        self.emit(&[0x38, 0x06]); // JR C, lex_ident_not_if (+6)
        self.label("is_if");
        self.emit(&[0xE1]);       // POP HL (discard start)
        self.emit(&[0xE1]);       // POP HL (discard end)
        self.emit(&[0xD1]);       // POP DE (restore original DE)
        self.emit(&[0x3E, tok::IF_KW]);
        self.emit(&[0xC9]);

        self.label("lex_ident_not_if");
        // Stack: [orig DE, end, start]
        // Restore HL to start for next check
        self.emit(&[0xE1]);       // POP HL (start)
        self.emit(&[0xE5]);       // PUSH HL (put start back)

        // Check for "THEN" keyword (uppercase)
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xFE, 0x54]); // CP 'T'
        self.emit(&[0xC2]);       // JP NZ, lex_ident_not_then
        self.fixup("lex_ident_not_then");
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xFE, 0x48]); // CP 'H'
        self.emit(&[0xC2]);       // JP NZ, lex_ident_not_then
        self.fixup("lex_ident_not_then");
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xFE, 0x45]); // CP 'E'
        self.emit(&[0xC2]);       // JP NZ, lex_ident_not_then
        self.fixup("lex_ident_not_then");
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xFE, 0x4E]); // CP 'N'
        self.emit(&[0xC2]);       // JP NZ, lex_ident_not_then
        self.fixup("lex_ident_not_then");
        self.emit(&[0x23]);       // INC HL
        // Check next char is not alphanumeric
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xFE, 0x41]); // CP 'A'
        self.emit(&[0x38, 0x0C]); // JR C, is_then (+12)
        self.emit(&[0xFE, 0x5B]); // CP 'Z'+1
        self.emit(&[0x38, 0x0E]); // JR C, lex_ident_not_then (+14)
        self.emit(&[0xFE, 0x61]); // CP 'a'
        self.emit(&[0x38, 0x04]); // JR C, is_then (+4)
        self.emit(&[0xFE, 0x7B]); // CP 'z'+1
        self.emit(&[0x38, 0x06]); // JR C, lex_ident_not_then (+6)
        self.label("is_then");
        self.emit(&[0xE1]);       // POP HL (discard start)
        self.emit(&[0xE1]);       // POP HL (discard end)
        self.emit(&[0xD1]);       // POP DE (restore original DE)
        self.emit(&[0x3E, tok::THEN_KW]);
        self.emit(&[0xC9]);

        self.label("lex_ident_not_then");
        // Stack: [orig DE, end, start]
        // Restore HL to start for next check
        self.emit(&[0xE1]);       // POP HL (start)
        self.emit(&[0xE5]);       // PUSH HL (put start back)

        // Check for "ELSE" keyword (uppercase)
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xFE, 0x45]); // CP 'E'
        self.emit(&[0xC2]);       // JP NZ, lex_ident_not_else
        self.fixup("lex_ident_not_else");
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xFE, 0x4C]); // CP 'L'
        self.emit(&[0xC2]);       // JP NZ, lex_ident_not_else
        self.fixup("lex_ident_not_else");
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xFE, 0x53]); // CP 'S'
        self.emit(&[0xC2]);       // JP NZ, lex_ident_not_else
        self.fixup("lex_ident_not_else");
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xFE, 0x45]); // CP 'E'
        self.emit(&[0xC2]);       // JP NZ, lex_ident_not_else
        self.fixup("lex_ident_not_else");
        self.emit(&[0x23]);       // INC HL
        // Check next char is not alphanumeric
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xFE, 0x41]); // CP 'A'
        self.emit(&[0x38, 0x0C]); // JR C, is_else (+12)
        self.emit(&[0xFE, 0x5B]); // CP 'Z'+1
        self.emit(&[0x38, 0x0E]); // JR C, lex_ident_not_else (+14)
        self.emit(&[0xFE, 0x61]); // CP 'a'
        self.emit(&[0x38, 0x04]); // JR C, is_else (+4)
        self.emit(&[0xFE, 0x7B]); // CP 'z'+1
        self.emit(&[0x38, 0x06]); // JR C, lex_ident_not_else (+6)
        self.label("is_else");
        self.emit(&[0xE1]);       // POP HL (discard start)
        self.emit(&[0xE1]);       // POP HL (discard end)
        self.emit(&[0xD1]);       // POP DE (restore original DE)
        self.emit(&[0x3E, tok::ELSE_KW]);
        self.emit(&[0xC9]);

        self.label("lex_ident_not_else");
        // Stack: [orig DE, end, start]
        // Restore HL to start for next check
        self.emit(&[0xE1]);       // POP HL (start)
        self.emit(&[0xE5]);       // PUSH HL (put start back)

        // Check for "WHILE" keyword (uppercase)
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xFE, 0x57]); // CP 'W'
        self.emit(&[0xC2]);       // JP NZ, lex_ident_not_while
        self.fixup("lex_ident_not_while");
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xFE, 0x48]); // CP 'H'
        self.emit(&[0xC2]);       // JP NZ, lex_ident_not_while
        self.fixup("lex_ident_not_while");
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xFE, 0x49]); // CP 'I'
        self.emit(&[0xC2]);       // JP NZ, lex_ident_not_while
        self.fixup("lex_ident_not_while");
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xFE, 0x4C]); // CP 'L'
        self.emit(&[0xC2]);       // JP NZ, lex_ident_not_while
        self.fixup("lex_ident_not_while");
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xFE, 0x45]); // CP 'E'
        self.emit(&[0xC2]);       // JP NZ, lex_ident_not_while
        self.fixup("lex_ident_not_while");
        self.emit(&[0x23]);       // INC HL
        // Check next char is not alphanumeric
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xFE, 0x41]); // CP 'A'
        self.emit(&[0x38, 0x0C]); // JR C, is_while (+12)
        self.emit(&[0xFE, 0x5B]); // CP 'Z'+1
        self.emit(&[0x38, 0x0E]); // JR C, lex_ident_not_while (+14)
        self.emit(&[0xFE, 0x61]); // CP 'a'
        self.emit(&[0x38, 0x04]); // JR C, is_while (+4)
        self.emit(&[0xFE, 0x7B]); // CP 'z'+1
        self.emit(&[0x38, 0x06]); // JR C, lex_ident_not_while (+6)
        self.label("is_while");
        self.emit(&[0xE1]);       // POP HL (discard start)
        self.emit(&[0xE1]);       // POP HL (discard end)
        self.emit(&[0xD1]);       // POP DE (restore original DE)
        self.emit(&[0x3E, tok::WHILE_KW]);
        self.emit(&[0xC9]);

        self.label("lex_ident_not_while");
        // Stack: [orig DE, end, start]
        // Restore HL to start for next check
        self.emit(&[0xE1]);       // POP HL (start)
        self.emit(&[0xE5]);       // PUSH HL (put start back)

        // Check for "DO" keyword (uppercase)
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xFE, 0x44]); // CP 'D'
        self.emit(&[0xC2]);       // JP NZ, lex_ident_not_do
        self.fixup("lex_ident_not_do");
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xFE, 0x4F]); // CP 'O'
        self.emit(&[0xC2]);       // JP NZ, lex_ident_not_do
        self.fixup("lex_ident_not_do");
        self.emit(&[0x23]);       // INC HL
        // Check next char is not alphanumeric
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xFE, 0x41]); // CP 'A'
        self.emit(&[0x38, 0x0C]); // JR C, is_do (+12)
        self.emit(&[0xFE, 0x5B]); // CP 'Z'+1
        self.emit(&[0x38, 0x0E]); // JR C, lex_ident_not_do (+14)
        self.emit(&[0xFE, 0x61]); // CP 'a'
        self.emit(&[0x38, 0x04]); // JR C, is_do (+4)
        self.emit(&[0xFE, 0x7B]); // CP 'z'+1
        self.emit(&[0x38, 0x06]); // JR C, lex_ident_not_do (+6)
        self.label("is_do");
        self.emit(&[0xE1]);       // POP HL (discard start)
        self.emit(&[0xE1]);       // POP HL (discard end)
        self.emit(&[0xD1]);       // POP DE (restore original DE)
        self.emit(&[0x3E, tok::DO_KW]);
        self.emit(&[0xC9]);

        self.label("lex_ident_not_do");
        // Stack: [orig DE, end, start]
        // Restore HL to start for next check
        self.emit(&[0xE1]);       // POP HL (start)
        self.emit(&[0xE5]);       // PUSH HL (put start back)

        // Check for "FUN" keyword (uppercase)
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xFE, 0x46]); // CP 'F'
        self.emit(&[0xC2]);       // JP NZ, lex_ident_not_kw
        self.fixup("lex_ident_not_kw");
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xFE, 0x55]); // CP 'U'
        self.emit(&[0xC2]);       // JP NZ, lex_ident_not_kw
        self.fixup("lex_ident_not_kw");
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xFE, 0x4E]); // CP 'N'
        self.emit(&[0xC2]);       // JP NZ, lex_ident_not_kw
        self.fixup("lex_ident_not_kw");
        self.emit(&[0x23]);       // INC HL
        // Check next char is not alphanumeric
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0xFE, 0x41]); // CP 'A'
        self.emit(&[0x38, 0x0C]); // JR C, is_fun (+12)
        self.emit(&[0xFE, 0x5B]); // CP 'Z'+1
        self.emit(&[0x38, 0x0E]); // JR C, lex_ident_not_kw (+14)
        self.emit(&[0xFE, 0x61]); // CP 'a'
        self.emit(&[0x38, 0x04]); // JR C, is_fun (+4)
        self.emit(&[0xFE, 0x7B]); // CP 'z'+1
        self.emit(&[0x38, 0x06]); // JR C, lex_ident_not_kw (+6)
        self.label("is_fun");
        self.emit(&[0xE1]);       // POP HL (discard start)
        self.emit(&[0xE1]);       // POP HL (discard end)
        self.emit(&[0xD1]);       // POP DE (restore original DE)
        self.emit(&[0x3E, tok::FUN_KW]);
        self.emit(&[0xC9]);

        self.label("lex_ident_not_kw");
        // Not a keyword, return IDENT
        // Stack: [orig DE, end, start]
        self.emit(&[0xE1]);       // POP HL (discard start)
        self.emit(&[0xE1]);       // POP HL (end)
        self.emit(&[0xD1]);       // POP DE (restore original DE)
        self.emit(&[0x3E, tok::IDENT]);
        self.emit(&[0xC9]);
    }

    fn emit_parser(&mut self) {
        // Parser: parse expression and emit bytecode
        // Input: HL = input buffer
        // Output: bytecode at BYTECODE_BUF
        // NOTE: IX must be initialized to BYTECODE_BUF before calling!
        //       (done in emit_repl_loop, NOT here - to allow recursion)

        // Precedence (lowest to highest):
        // 1. or (parse_expr)
        // 2. and (parse_and_expr)
        // 3. comparison (parse_cmp_expr)
        // 4. bitwise (parse_bitwise_expr)
        // 5. additive (parse_add_expr)
        // 6. multiplicative (parse_term)
        // 7. unary (parse_factor) - includes not, ~, -

        // Top level: parse_expr handles 'or' (lowest precedence)
        self.label("parse_expr");
        self.emit(&[0xCD]);       // CALL parse_and_expr
        self.fixup("parse_and_expr");

        self.label("parse_expr_loop");
        self.emit(&[0xE5]);       // PUSH HL
        self.emit(&[0xCD]);       // CALL lex_next
        self.fixup("lex_next");

        self.emit(&[0xFE, tok::OR_KW]);
        self.emit(&[0xCA]);       // JP Z, parse_lor
        self.fixup("parse_lor");

        // No 'or', return
        self.emit(&[0xE1]);       // POP HL
        self.emit(&[0xC9]);       // RET

        self.label("parse_lor");
        self.emit(&[0xD1]);       // POP DE
        self.emit(&[0xCD]);       // CALL parse_and_expr
        self.fixup("parse_and_expr");
        self.emit(&[0xDD, 0x36, 0, op::LOR_I]); // LD (IX+0), LOR_I
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xC3]);       // JP parse_expr_loop
        self.fixup("parse_expr_loop");

        // parse_and_expr: handles 'and' operator
        self.label("parse_and_expr");
        self.emit(&[0xCD]);       // CALL parse_cmp_expr
        self.fixup("parse_cmp_expr");

        self.label("parse_and_loop");
        self.emit(&[0xE5]);       // PUSH HL
        self.emit(&[0xCD]);       // CALL lex_next
        self.fixup("lex_next");

        self.emit(&[0xFE, tok::AND_KW]);
        self.emit(&[0xCA]);       // JP Z, parse_land
        self.fixup("parse_land");

        // No 'and', return
        self.emit(&[0xE1]);       // POP HL
        self.emit(&[0xC9]);       // RET

        self.label("parse_land");
        self.emit(&[0xD1]);       // POP DE
        self.emit(&[0xCD]);       // CALL parse_cmp_expr
        self.fixup("parse_cmp_expr");
        self.emit(&[0xDD, 0x36, 0, op::LAND_I]); // LD (IX+0), LAND_I
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xC3]);       // JP parse_and_loop
        self.fixup("parse_and_loop");

        // parse_cmp_expr: handles comparison operators (==, !=, <, <=, >, >=)
        self.label("parse_cmp_expr");
        self.emit(&[0xCD]);       // CALL parse_bitwise_expr
        self.fixup("parse_bitwise_expr");

        self.label("parse_cmp_loop");
        self.emit(&[0xE5]);       // PUSH HL
        self.emit(&[0xCD]);       // CALL lex_next
        self.fixup("lex_next");

        self.emit(&[0xFE, tok::EQEQ]);
        self.emit(&[0xCA]);       // JP Z, parse_eq
        self.fixup("parse_eq");

        self.emit(&[0xFE, tok::NE]);
        self.emit(&[0xCA]);       // JP Z, parse_ne
        self.fixup("parse_ne");

        self.emit(&[0xFE, tok::LT]);
        self.emit(&[0xCA]);       // JP Z, parse_lt
        self.fixup("parse_lt");

        self.emit(&[0xFE, tok::LE]);
        self.emit(&[0xCA]);       // JP Z, parse_le
        self.fixup("parse_le");

        self.emit(&[0xFE, tok::GT]);
        self.emit(&[0xCA]);       // JP Z, parse_gt
        self.fixup("parse_gt");

        self.emit(&[0xFE, tok::GE]);
        self.emit(&[0xCA]);       // JP Z, parse_ge
        self.fixup("parse_ge");

        // No comparison operator, return
        self.emit(&[0xE1]);       // POP HL
        self.emit(&[0xC9]);       // RET

        self.label("parse_eq");
        self.emit(&[0xD1]);       // POP DE
        self.emit(&[0xCD]);       // CALL parse_bitwise_expr
        self.fixup("parse_bitwise_expr");
        self.emit(&[0xDD, 0x36, 0, op::EQ_I]); // LD (IX+0), EQ_I
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xC3]);       // JP parse_cmp_loop
        self.fixup("parse_cmp_loop");

        self.label("parse_ne");
        self.emit(&[0xD1]);       // POP DE
        self.emit(&[0xCD]);       // CALL parse_bitwise_expr
        self.fixup("parse_bitwise_expr");
        self.emit(&[0xDD, 0x36, 0, op::NE_I]); // LD (IX+0), NE_I
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xC3]);       // JP parse_cmp_loop
        self.fixup("parse_cmp_loop");

        self.label("parse_lt");
        self.emit(&[0xD1]);       // POP DE
        self.emit(&[0xCD]);       // CALL parse_bitwise_expr
        self.fixup("parse_bitwise_expr");
        self.emit(&[0xDD, 0x36, 0, op::LT_I]); // LD (IX+0), LT_I
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xC3]);       // JP parse_cmp_loop
        self.fixup("parse_cmp_loop");

        self.label("parse_le");
        self.emit(&[0xD1]);       // POP DE
        self.emit(&[0xCD]);       // CALL parse_bitwise_expr
        self.fixup("parse_bitwise_expr");
        self.emit(&[0xDD, 0x36, 0, op::LE_I]); // LD (IX+0), LE_I
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xC3]);       // JP parse_cmp_loop
        self.fixup("parse_cmp_loop");

        self.label("parse_gt");
        self.emit(&[0xD1]);       // POP DE
        self.emit(&[0xCD]);       // CALL parse_bitwise_expr
        self.fixup("parse_bitwise_expr");
        self.emit(&[0xDD, 0x36, 0, op::GT_I]); // LD (IX+0), GT_I
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xC3]);       // JP parse_cmp_loop
        self.fixup("parse_cmp_loop");

        self.label("parse_ge");
        self.emit(&[0xD1]);       // POP DE
        self.emit(&[0xCD]);       // CALL parse_bitwise_expr
        self.fixup("parse_bitwise_expr");
        self.emit(&[0xDD, 0x36, 0, op::GE_I]); // LD (IX+0), GE_I
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xC3]);       // JP parse_cmp_loop
        self.fixup("parse_cmp_loop");

        // parse_bitwise_expr: handles bitwise operators &, |, ^, <<, >>
        self.label("parse_bitwise_expr");
        self.emit(&[0xCD]);       // CALL parse_add_expr
        self.fixup("parse_add_expr");

        self.label("parse_bitwise_loop");
        self.emit(&[0xE5]);       // PUSH HL
        self.emit(&[0xCD]);       // CALL lex_next
        self.fixup("lex_next");

        self.emit(&[0xFE, tok::BAND]);
        self.emit(&[0xCA]);       // JP Z, parse_band
        self.fixup("parse_band");

        self.emit(&[0xFE, tok::BOR]);
        self.emit(&[0xCA]);       // JP Z, parse_bor
        self.fixup("parse_bor");

        self.emit(&[0xFE, tok::BXOR]);
        self.emit(&[0xCA]);       // JP Z, parse_bxor
        self.fixup("parse_bxor");

        self.emit(&[0xFE, tok::SHL]);
        self.emit(&[0xCA]);       // JP Z, parse_shl
        self.fixup("parse_shl");

        self.emit(&[0xFE, tok::SHR]);
        self.emit(&[0xCA]);       // JP Z, parse_shr
        self.fixup("parse_shr");

        // No bitwise operator, return
        self.emit(&[0xE1]);       // POP HL
        self.emit(&[0xC9]);       // RET

        self.label("parse_band");
        self.emit(&[0xD1]);       // POP DE
        self.emit(&[0xCD]);       // CALL parse_add_expr
        self.fixup("parse_add_expr");
        self.emit(&[0xDD, 0x36, 0, op::BAND_I]); // LD (IX+0), BAND_I
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xC3]);       // JP parse_bitwise_loop
        self.fixup("parse_bitwise_loop");

        self.label("parse_bor");
        self.emit(&[0xD1]);       // POP DE
        self.emit(&[0xCD]);       // CALL parse_add_expr
        self.fixup("parse_add_expr");
        self.emit(&[0xDD, 0x36, 0, op::BOR_I]); // LD (IX+0), BOR_I
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xC3]);       // JP parse_bitwise_loop
        self.fixup("parse_bitwise_loop");

        self.label("parse_bxor");
        self.emit(&[0xD1]);       // POP DE
        self.emit(&[0xCD]);       // CALL parse_add_expr
        self.fixup("parse_add_expr");
        self.emit(&[0xDD, 0x36, 0, op::BXOR_I]); // LD (IX+0), BXOR_I
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xC3]);       // JP parse_bitwise_loop
        self.fixup("parse_bitwise_loop");

        self.label("parse_shl");
        self.emit(&[0xD1]);       // POP DE
        self.emit(&[0xCD]);       // CALL parse_add_expr
        self.fixup("parse_add_expr");
        self.emit(&[0xDD, 0x36, 0, op::SHL_I]); // LD (IX+0), SHL_I
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xC3]);       // JP parse_bitwise_loop
        self.fixup("parse_bitwise_loop");

        self.label("parse_shr");
        self.emit(&[0xD1]);       // POP DE
        self.emit(&[0xCD]);       // CALL parse_add_expr
        self.fixup("parse_add_expr");
        self.emit(&[0xDD, 0x36, 0, op::SHR_I]); // LD (IX+0), SHR_I
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xC3]);       // JP parse_bitwise_loop
        self.fixup("parse_bitwise_loop");

        // parse_add_expr: handles + and - operators
        self.label("parse_add_expr");
        self.emit(&[0xCD]);       // CALL parse_term
        self.fixup("parse_term");

        self.label("parse_add_expr_loop");
        self.emit(&[0xE5]);       // PUSH HL
        self.emit(&[0xCD]);       // CALL lex_next
        self.fixup("lex_next");

        self.emit(&[0xFE, tok::PLUS]);
        self.emit(&[0xCA]);       // JP Z, parse_add
        self.fixup("parse_add");

        self.emit(&[0xFE, tok::MINUS]);
        self.emit(&[0xCA]);       // JP Z, parse_sub
        self.fixup("parse_sub");

        // Not + or -, restore HL and return
        self.emit(&[0xE1]);       // POP HL
        self.emit(&[0xC9]);       // RET

        self.label("parse_add");
        self.emit(&[0xD1]);       // POP DE (discard saved HL, use new one)
        self.emit(&[0xCD]);       // CALL parse_term
        self.fixup("parse_term");
        self.emit(&[0xDD, 0x36, 0, op::ADD_I]); // LD (IX+0), ADD_I
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xC3]);       // JP parse_add_expr_loop
        self.fixup("parse_add_expr_loop");

        self.label("parse_sub");
        self.emit(&[0xD1]);       // POP DE
        self.emit(&[0xCD]);       // CALL parse_term
        self.fixup("parse_term");
        self.emit(&[0xDD, 0x36, 0, op::SUB_I]); // LD (IX+0), SUB_I
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xC3]);       // JP parse_add_expr_loop
        self.fixup("parse_add_expr_loop");

        // Parse term: handles * and /
        self.label("parse_term");
        self.emit(&[0xCD]);       // CALL parse_factor
        self.fixup("parse_factor");

        self.label("parse_term_loop");
        self.emit(&[0xE5]);       // PUSH HL
        self.emit(&[0xCD]);       // CALL lex_next
        self.fixup("lex_next");

        self.emit(&[0xFE, tok::STAR]);
        self.emit(&[0xCA]);       // JP Z, parse_mul
        self.fixup("parse_mul");

        self.emit(&[0xFE, tok::SLASH]);
        self.emit(&[0xCA]);       // JP Z, parse_div
        self.fixup("parse_div");

        self.emit(&[0xFE, tok::MOD]);
        self.emit(&[0xCA]);       // JP Z, parse_mod
        self.fixup("parse_mod");

        // Not *, /, or %, restore HL and return
        self.emit(&[0xE1]);       // POP HL
        self.emit(&[0xC9]);       // RET

        self.label("parse_mul");
        self.emit(&[0xD1]);       // POP DE
        self.emit(&[0xCD]);       // CALL parse_factor
        self.fixup("parse_factor");
        self.emit(&[0xDD, 0x36, 0, op::MUL_I]); // LD (IX+0), MUL_I
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xC3]);       // JP parse_term_loop
        self.fixup("parse_term_loop");

        self.label("parse_div");
        self.emit(&[0xD1]);       // POP DE
        self.emit(&[0xCD]);       // CALL parse_factor
        self.fixup("parse_factor");
        self.emit(&[0xDD, 0x36, 0, op::DIV_I]); // LD (IX+0), DIV_I
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xC3]);       // JP parse_term_loop
        self.fixup("parse_term_loop");

        self.label("parse_mod");
        self.emit(&[0xD1]);       // POP DE
        self.emit(&[0xCD]);       // CALL parse_factor
        self.fixup("parse_factor");
        self.emit(&[0xDD, 0x36, 0, op::MOD_I]); // LD (IX+0), MOD_I
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xC3]);       // JP parse_term_loop
        self.fixup("parse_term_loop");

        // Parse factor: number or (expr)
        self.label("parse_factor");
        self.emit(&[0xCD]);       // CALL lex_next
        self.fixup("lex_next");

        self.emit(&[0xFE, tok::INT]);
        self.emit(&[0xCA]);       // JP Z, parse_int
        self.fixup("parse_int");

        self.emit(&[0xFE, tok::DEC]);
        self.emit(&[0xCA]);       // JP Z, parse_int (same handler, DE has value)
        self.fixup("parse_int");

        self.emit(&[0xFE, tok::LPAREN]);
        self.emit(&[0xCA]);       // JP Z, parse_paren
        self.fixup("parse_paren");

        self.emit(&[0xFE, tok::MINUS]);
        self.emit(&[0xCA]);       // JP Z, parse_neg
        self.fixup("parse_neg");

        self.emit(&[0xFE, tok::BNOT]);
        self.emit(&[0xCA]);       // JP Z, parse_bnot
        self.fixup("parse_bnot");

        self.emit(&[0xFE, tok::NOT_KW]);
        self.emit(&[0xCA]);       // JP Z, parse_lnot
        self.fixup("parse_lnot");

        self.emit(&[0xFE, tok::IF_KW]);
        self.emit(&[0xCA]);       // JP Z, parse_if
        self.fixup("parse_if");

        self.emit(&[0xFE, tok::WHILE_KW]);
        self.emit(&[0xCA]);       // JP Z, parse_while
        self.fixup("parse_while");

        self.emit(&[0xFE, tok::IDENT]);
        self.emit(&[0xCA]);       // JP Z, parse_var
        self.fixup("parse_var");

        // Error - unexpected token
        self.emit(&[0xC9]);       // RET (for now)

        self.label("parse_int");
        // Emit PUSH_I16 with value from DE
        self.emit(&[0xDD, 0x36, 0, op::PUSH_I16]); // LD (IX+0), PUSH_I16
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xDD, 0x73, 0]); // LD (IX+0), E
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xDD, 0x72, 0]); // LD (IX+0), D
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xC9]);       // RET

        self.label("parse_paren");
        self.emit(&[0xCD]);       // CALL parse_expr (full expression inside parens)
        self.fixup("parse_expr");
        // Expect )
        self.emit(&[0xCD]);       // CALL lex_next
        self.fixup("lex_next");
        self.emit(&[0xFE, tok::RPAREN]);
        // If not ), error (ignore for now)
        self.emit(&[0xC9]);       // RET

        self.label("parse_neg");
        // Unary minus: parse factor then emit NEG_I
        self.emit(&[0xCD]);       // CALL parse_factor (recursive)
        self.fixup("parse_factor");
        self.emit(&[0xDD, 0x36, 0, op::NEG_I]); // LD (IX+0), NEG_I
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xC9]);       // RET

        self.label("parse_bnot");
        // Bitwise NOT: parse factor then emit BNOT_I
        self.emit(&[0xCD]);       // CALL parse_factor (recursive)
        self.fixup("parse_factor");
        self.emit(&[0xDD, 0x36, 0, op::BNOT_I]); // LD (IX+0), BNOT_I
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xC9]);       // RET

        self.label("parse_lnot");
        // Logical NOT: parse factor then emit LNOT_I
        self.emit(&[0xCD]);       // CALL parse_factor (recursive)
        self.fixup("parse_factor");
        self.emit(&[0xDD, 0x36, 0, op::LNOT_I]); // LD (IX+0), LNOT_I
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xC9]);       // RET

        self.label("parse_if");
        // If-then-else expression: if cond then expr1 else expr2
        // Bytecode: [cond] JZ addr_else [then] JMP addr_end [else]

        // Parse condition
        self.emit(&[0xCD]);       // CALL parse_expr
        self.fixup("parse_expr");

        // Emit JZ with placeholder
        self.emit(&[0xDD, 0x36, 0, op::JZ]); // LD (IX+0), JZ
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xDD, 0xE5]); // PUSH IX (save JZ addr location)
        self.emit(&[0xDD, 0x36, 0, 0]); // LD (IX+0), 0 (placeholder low)
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xDD, 0x36, 0, 0]); // LD (IX+0), 0 (placeholder high)
        self.emit(&[0xDD, 0x23]); // INC IX

        // Expect THEN token
        self.emit(&[0xCD]);       // CALL lex_next
        self.fixup("lex_next");
        // (could verify it's THEN_KW)

        // Parse then expression
        self.emit(&[0xCD]);       // CALL parse_expr
        self.fixup("parse_expr");

        // Emit JMP with placeholder
        self.emit(&[0xDD, 0x36, 0, op::JMP]); // LD (IX+0), JMP
        self.emit(&[0xDD, 0x23]); // INC IX

        // Save JMP addr location in DE
        self.emit(&[0xDD, 0xE5]); // PUSH IX
        self.emit(&[0xD1]);       // POP DE (DE = JMP addr location)

        self.emit(&[0xDD, 0x36, 0, 0]); // LD (IX+0), 0 (placeholder low)
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xDD, 0x36, 0, 0]); // LD (IX+0), 0 (placeholder high)
        self.emit(&[0xDD, 0x23]); // INC IX

        // Now: IX = start of else code, DE = JMP addr loc, Stack: [JZ addr loc]
        // HL = input pointer (past "then" expr) - MUST PRESERVE!

        // Save both DE (JMP addr) and HL (input ptr) before backpatch
        self.emit(&[0xD5]);       // PUSH DE (JMP addr loc)
        self.emit(&[0xE5]);       // PUSH HL (input pointer)
        // Stack: [JZ addr loc, JMP addr loc, input ptr]

        // Backpatch JZ to current IX
        self.emit(&[0xDD, 0xE5]); // PUSH IX
        self.emit(&[0xC1]);       // POP BC (BC = current IX = else start)
        // Get JZ addr loc from bottom of our saves
        self.emit(&[0xE1]);       // POP HL (input ptr)
        self.emit(&[0xD1]);       // POP DE (JMP addr loc)
        self.emit(&[0xE3]);       // EX (SP), HL - HL = JZ addr, Stack: [input ptr]
        self.emit(&[0x71]);       // LD (HL), C (write low byte of else addr)
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0x70]);       // LD (HL), B (write high byte of else addr)

        // Restore input pointer, save JMP addr for later backpatch
        self.emit(&[0xE1]);       // POP HL (restore input pointer)
        self.emit(&[0xD5]);       // PUSH DE (save JMP addr loc for later)

        // Expect ELSE token
        self.emit(&[0xCD]);       // CALL lex_next
        self.fixup("lex_next");
        // (could verify it's ELSE_KW)

        // Parse else expression
        self.emit(&[0xCD]);       // CALL parse_expr
        self.fixup("parse_expr");

        // Backpatch JMP to current IX
        self.emit(&[0xE1]);       // POP HL (JMP addr loc)
        self.emit(&[0xDD, 0xE5]); // PUSH IX
        self.emit(&[0xC1]);       // POP BC (BC = current IX = end)
        self.emit(&[0x71]);       // LD (HL), C (write low byte)
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0x70]);       // LD (HL), B (write high byte)

        self.emit(&[0xC9]);       // RET

        self.label("parse_while");
        // While loop: while cond do body
        // Bytecode: PUSH_I16 0 [loop: cond JZ end POP_I body JMP loop] end:
        // Returns last body value, or 0 if body never executed

        // Emit PUSH_I16 0 as default result
        self.emit(&[0xDD, 0x36, 0, op::PUSH_I16]); // LD (IX+0), PUSH_I16
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xDD, 0x36, 0, 0]); // LD (IX+0), 0 (low byte)
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xDD, 0x36, 0, 0]); // LD (IX+0), 0 (high byte)
        self.emit(&[0xDD, 0x23]); // INC IX

        // Save loop_start address (current IX)
        self.emit(&[0xDD, 0xE5]); // PUSH IX (loop_start address)

        // Parse condition
        self.emit(&[0xCD]);       // CALL parse_expr
        self.fixup("parse_expr");

        // Emit JZ with placeholder
        self.emit(&[0xDD, 0x36, 0, op::JZ]); // LD (IX+0), JZ
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xDD, 0xE5]); // PUSH IX (save JZ addr location)
        self.emit(&[0xDD, 0x36, 0, 0]); // LD (IX+0), 0 (placeholder low)
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xDD, 0x36, 0, 0]); // LD (IX+0), 0 (placeholder high)
        self.emit(&[0xDD, 0x23]); // INC IX

        // Expect DO token
        self.emit(&[0xCD]);       // CALL lex_next
        self.fixup("lex_next");
        // (could verify it's DO_KW)

        // Emit POP_I to discard previous result before body
        self.emit(&[0xDD, 0x36, 0, op::POP_I]); // LD (IX+0), POP_I
        self.emit(&[0xDD, 0x23]); // INC IX

        // Parse body
        self.emit(&[0xCD]);       // CALL parse_expr
        self.fixup("parse_expr");

        // Emit JMP back to loop_start
        self.emit(&[0xDD, 0x36, 0, op::JMP]); // LD (IX+0), JMP
        self.emit(&[0xDD, 0x23]); // INC IX
        // Get loop_start from stack (it's under JZ addr)
        // Stack: [loop_start, JZ addr]
        self.emit(&[0xD1]);       // POP DE (JZ addr loc)
        self.emit(&[0xC1]);       // POP BC (loop_start addr)
        self.emit(&[0xDD, 0x71, 0]); // LD (IX+0), C (loop_start low)
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xDD, 0x70, 0]); // LD (IX+0), B (loop_start high)
        self.emit(&[0xDD, 0x23]); // INC IX

        // Backpatch JZ to current IX (end of loop)
        // DE = JZ addr loc, need to write current IX there
        self.emit(&[0xDD, 0xE5]); // PUSH IX
        self.emit(&[0xC1]);       // POP BC (BC = current IX = end)
        self.emit(&[0xEB]);       // EX DE, HL (HL = JZ addr loc, DE = input ptr)
        self.emit(&[0x71]);       // LD (HL), C (write low byte)
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0x70]);       // LD (HL), B (write high byte)
        self.emit(&[0xEB]);       // EX DE, HL (restore HL = input ptr)

        self.emit(&[0xC9]);       // RET

        self.label("parse_var");
        // Variable reference or assignment expression
        // First char of identifier is in SCRATCH memory
        // Save HL (input pointer) - we need to preserve it
        self.emit(&[0xE5]);       // PUSH HL
        self.emit(&[0x3A]);       // LD A, (SCRATCH)
        self.emit_word(SCRATCH);
        // Convert uppercase to lowercase if needed (emulator sends uppercase)
        self.emit(&[0xFE, 0x5B]); // CP 'Z'+1
        self.emit(&[0x30, 0x02]); // JR NC, +2 (skip if >= '[', already lowercase or other)
        self.emit(&[0xC6, 0x20]); // ADD A, 0x20 (convert 'A'-'Z' to 'a'-'z')
        // Calculate address: SYMBOL_TABLE + 2 * (char - 'a')
        self.emit(&[0xD6, 0x61]); // SUB 'a'
        self.emit(&[0x87]);       // ADD A, A (multiply by 2)
        self.emit(&[0x5F]);       // LD E, A
        self.emit(&[0x16, 0x00]); // LD D, 0
        self.emit(&[0x21]);       // LD HL, SYMBOL_TABLE
        self.emit_word(SYMBOL_TABLE);
        self.emit(&[0x19]);       // ADD HL, DE (HL = address of variable)
        // Save variable address in BC
        self.emit(&[0x44]);       // LD B, H
        self.emit(&[0x4D]);       // LD C, L (BC = var address)

        // Restore input pointer and check for :=
        self.emit(&[0xE1]);       // POP HL (restore input pointer)
        self.emit(&[0xE5]);       // PUSH HL (save it again for lex_next)
        self.emit(&[0xC5]);       // PUSH BC (save var address)
        self.emit(&[0xCD]);       // CALL lex_next (to check for :=)
        self.fixup("lex_next");
        self.emit(&[0xFE, tok::ASSIGN]);
        self.emit(&[0x20, 0x18]); // JR NZ, parse_var_load (+24: not assignment, just load)

        // It's an assignment: var := expr
        // Pop and discard old input pointer (we'll use HL from lex which is past :=)
        self.emit(&[0xC1]);       // POP BC (var address)
        self.emit(&[0xD1]);       // POP DE (discard old input pointer)
        // HL now points past :=, BC has var address
        self.emit(&[0xC5]);       // PUSH BC (save var address)
        self.emit(&[0xCD]);       // CALL parse_expr (parse RHS)
        self.fixup("parse_expr");
        self.emit(&[0xC1]);       // POP BC (restore var address)
        // Emit STORE
        self.emit(&[0xDD, 0x36, 0, op::STORE]);
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xDD, 0x71, 0]); // LD (IX+0), C
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xDD, 0x70, 0]); // LD (IX+0), B
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xC9]);       // RET

        self.label("parse_var_load");
        // Not an assignment. Check if it's a function call (ident followed by arg)
        // Token from lex_next is in A, HL points past it
        // Stack: [var_addr, original_input]
        // Check if token starts an expression (INT, DEC, LPAREN, IDENT, MINUS)
        // Byte offsets: 5 checks * 4 bytes + JP(3) = 23 bytes total
        // parse_func_call is at byte 23 from start of parse_var_load
        self.emit(&[0xFE, tok::INT]);
        self.emit(&[0x28, 0x13]); // JR Z, parse_func_call (+19)
        self.emit(&[0xFE, tok::DEC]);
        self.emit(&[0x28, 0x0F]); // JR Z, parse_func_call (+15)
        self.emit(&[0xFE, tok::LPAREN]);
        self.emit(&[0x28, 0x0B]); // JR Z, parse_func_call (+11)
        self.emit(&[0xFE, tok::IDENT]);
        self.emit(&[0x28, 0x07]); // JR Z, parse_func_call (+7)
        self.emit(&[0xFE, tok::MINUS]);
        self.emit(&[0x28, 0x03]); // JR Z, parse_func_call (+3)
        // Not a function call, emit LOAD
        self.emit(&[0xC3]);       // JP parse_var_just_load
        self.fixup("parse_var_just_load");

        self.label("parse_func_call");
        // It's a function call: ident arg
        // Stack: [var_addr, original_input], A = token, DE = arg value (if INT), HL = past token
        // We need func_idx from var_addr. var_addr = SYMBOL_TABLE + 2 * func_idx
        // So func_idx = (var_addr - SYMBOL_TABLE) / 2

        // Save DE (arg value) to SCRATCH area
        self.emit(&[0xED, 0x53]); // LD (SCRATCH+4), DE
        self.emit_word(SCRATCH + 4);

        // Pop var_addr and original_input from stack
        self.emit(&[0xC1]);       // POP BC (var address)
        self.emit(&[0xF1]);       // POP AF (discard original input)

        // Calculate func_idx from BC: (BC - SYMBOL_TABLE) / 2
        self.emit(&[0x79]);       // LD A, C
        self.emit(&[0xD6]);       // SUB low(SYMBOL_TABLE)
        self.emit_byte((SYMBOL_TABLE & 0xFF) as u8);
        self.emit(&[0xCB, 0x3F]); // SRL A (divide by 2)
        // A = func_idx, save it in C
        self.emit(&[0x4F]);       // LD C, A (save func_idx in C)

        // Restore DE (arg value) from SCRATCH
        self.emit(&[0xED, 0x5B]); // LD DE, (SCRATCH+4)
        self.emit_word(SCRATCH + 4);

        // Emit PUSH_I16 with arg value from DE
        self.emit(&[0xDD, 0x36, 0, op::PUSH_I16]); // emit PUSH_I16
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xDD, 0x73, 0]); // LD (IX+0), E
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xDD, 0x72, 0]); // LD (IX+0), D
        self.emit(&[0xDD, 0x23]); // INC IX

        // Emit FCALL func_idx (C has func_idx)
        self.emit(&[0xDD, 0x36, 0, op::FCALL]);
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xDD, 0x71, 0]); // LD (IX+0), C (func_idx)
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xC9]);       // RET

        self.label("parse_var_just_load");
        // Not an assignment and not a function call, just a variable load
        self.emit(&[0xC1]);       // POP BC (var address)
        self.emit(&[0xE1]);       // POP HL (restore original input pointer)
        // Emit LOAD opcode with address
        self.emit(&[0xDD, 0x36, 0, op::LOAD]); // LD (IX+0), LOAD
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xDD, 0x71, 0]); // LD (IX+0), C (low byte of address)
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xDD, 0x70, 0]); // LD (IX+0), B (high byte of address)
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xC9]);       // RET
    }

    fn emit_interpreter(&mut self) {
        // Bytecode interpreter
        // Executes bytecode at BYTECODE_BUF
        // Uses IY as evaluation stack pointer
        self.label("interpret");

        self.emit(&[0x21]);       // LD HL, BYTECODE_BUF
        self.emit_word(BYTECODE_BUF);
        self.emit(&[0xFD, 0x21]); // LD IY, EVAL_STACK
        self.emit_word(EVAL_STACK);

        self.label("interp_loop");
        self.emit(&[0x7E]);       // LD A, (HL)
        self.emit(&[0x23]);       // INC HL

        // Dispatch on opcode using JP Z (absolute conditional jump)
        self.emit(&[0xFE, op::END]);
        self.emit(&[0xC8]);       // RET Z

        self.emit(&[0xFE, op::PUSH_I16]);
        self.emit(&[0xCA]);       // JP Z, interp_push_i16
        self.fixup("interp_push_i16");

        self.emit(&[0xFE, op::ADD_I]);
        self.emit(&[0xCA]);       // JP Z, interp_add_i
        self.fixup("interp_add_i");

        self.emit(&[0xFE, op::SUB_I]);
        self.emit(&[0xCA]);       // JP Z, interp_sub_i
        self.fixup("interp_sub_i");

        self.emit(&[0xFE, op::MUL_I]);
        self.emit(&[0xCA]);       // JP Z, interp_mul_i
        self.fixup("interp_mul_i");

        self.emit(&[0xFE, op::DIV_I]);
        self.emit(&[0xCA]);       // JP Z, interp_div_i
        self.fixup("interp_div_i");

        self.emit(&[0xFE, op::MOD_I]);
        self.emit(&[0xCA]);       // JP Z, interp_mod_i
        self.fixup("interp_mod_i");

        self.emit(&[0xFE, op::NEG_I]);
        self.emit(&[0xCA]);       // JP Z, interp_neg_i
        self.fixup("interp_neg_i");

        self.emit(&[0xFE, op::EQ_I]);
        self.emit(&[0xCA]);       // JP Z, interp_eq_i
        self.fixup("interp_eq_i");

        self.emit(&[0xFE, op::NE_I]);
        self.emit(&[0xCA]);       // JP Z, interp_ne_i
        self.fixup("interp_ne_i");

        self.emit(&[0xFE, op::LT_I]);
        self.emit(&[0xCA]);       // JP Z, interp_lt_i
        self.fixup("interp_lt_i");

        self.emit(&[0xFE, op::LE_I]);
        self.emit(&[0xCA]);       // JP Z, interp_le_i
        self.fixup("interp_le_i");

        self.emit(&[0xFE, op::GT_I]);
        self.emit(&[0xCA]);       // JP Z, interp_gt_i
        self.fixup("interp_gt_i");

        self.emit(&[0xFE, op::GE_I]);
        self.emit(&[0xCA]);       // JP Z, interp_ge_i
        self.fixup("interp_ge_i");

        self.emit(&[0xFE, op::PRINT_I]);
        self.emit(&[0xCA]);       // JP Z, interp_print_i
        self.fixup("interp_print_i");

        self.emit(&[0xFE, op::POP_I]);
        self.emit(&[0xCA]);       // JP Z, interp_pop_i
        self.fixup("interp_pop_i");

        self.emit(&[0xFE, op::LOAD]);
        self.emit(&[0xCA]);       // JP Z, interp_load
        self.fixup("interp_load");

        self.emit(&[0xFE, op::STORE]);
        self.emit(&[0xCA]);       // JP Z, interp_store
        self.fixup("interp_store");

        self.emit(&[0xFE, op::FCALL]);
        self.emit(&[0xCA]);       // JP Z, interp_fcall
        self.fixup("interp_fcall");

        self.emit(&[0xFE, op::FRET]);
        self.emit(&[0xCA]);       // JP Z, interp_fret
        self.fixup("interp_fret");

        self.emit(&[0xFE, op::BAND_I]);
        self.emit(&[0xCA]);       // JP Z, interp_band_i
        self.fixup("interp_band_i");

        self.emit(&[0xFE, op::BOR_I]);
        self.emit(&[0xCA]);       // JP Z, interp_bor_i
        self.fixup("interp_bor_i");

        self.emit(&[0xFE, op::BXOR_I]);
        self.emit(&[0xCA]);       // JP Z, interp_bxor_i
        self.fixup("interp_bxor_i");

        self.emit(&[0xFE, op::BNOT_I]);
        self.emit(&[0xCA]);       // JP Z, interp_bnot_i
        self.fixup("interp_bnot_i");

        self.emit(&[0xFE, op::SHL_I]);
        self.emit(&[0xCA]);       // JP Z, interp_shl_i
        self.fixup("interp_shl_i");

        self.emit(&[0xFE, op::SHR_I]);
        self.emit(&[0xCA]);       // JP Z, interp_shr_i
        self.fixup("interp_shr_i");

        self.emit(&[0xFE, op::LAND_I]);
        self.emit(&[0xCA]);       // JP Z, interp_land_i
        self.fixup("interp_land_i");

        self.emit(&[0xFE, op::LOR_I]);
        self.emit(&[0xCA]);       // JP Z, interp_lor_i
        self.fixup("interp_lor_i");

        self.emit(&[0xFE, op::LNOT_I]);
        self.emit(&[0xCA]);       // JP Z, interp_lnot_i
        self.fixup("interp_lnot_i");

        self.emit(&[0xFE, op::JMP]);
        self.emit(&[0xCA]);       // JP Z, interp_jmp
        self.fixup("interp_jmp");

        self.emit(&[0xFE, op::JZ]);
        self.emit(&[0xCA]);       // JP Z, interp_jz
        self.fixup("interp_jz");

        // Unknown opcode - halt
        self.emit(&[0x76]);       // HALT

        // PUSH_I16: push 16-bit integer
        self.label("interp_push_i16");
        self.emit(&[0x5E]);       // LD E, (HL)
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0x56]);       // LD D, (HL)
        self.emit(&[0x23]);       // INC HL
        // Push DE to eval stack (IY)
        self.emit(&[0xFD, 0x73, 0]); // LD (IY+0), E
        self.emit(&[0xFD, 0x72, 1]); // LD (IY+1), D
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xC3]);       // JP interp_loop
        self.fixup("interp_loop");

        // ADD_I: pop two, push sum
        self.label("interp_add_i");
        self.emit(&[0xE5]);       // PUSH HL (save bytecode ptr)
        // Pop first operand
        self.emit(&[0xFD, 0x2B]); // DEC IY
        self.emit(&[0xFD, 0x2B]); // DEC IY
        self.emit(&[0xFD, 0x5E, 0]); // LD E, (IY+0)
        self.emit(&[0xFD, 0x56, 1]); // LD D, (IY+1)
        // Pop second operand
        self.emit(&[0xFD, 0x2B]); // DEC IY
        self.emit(&[0xFD, 0x2B]); // DEC IY
        self.emit(&[0xFD, 0x6E, 0]); // LD L, (IY+0)
        self.emit(&[0xFD, 0x66, 1]); // LD H, (IY+1)
        // Add
        self.emit(&[0x19]);       // ADD HL, DE
        // Push result
        self.emit(&[0xFD, 0x75, 0]); // LD (IY+0), L
        self.emit(&[0xFD, 0x74, 1]); // LD (IY+1), H
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xE1]);       // POP HL (restore bytecode ptr)
        self.emit(&[0xC3]);       // JP interp_loop
        self.fixup("interp_loop");

        // SUB_I: pop two, push difference
        self.label("interp_sub_i");
        self.emit(&[0xE5]);       // PUSH HL (save bytecode ptr)
        self.emit(&[0xFD, 0x2B]); // DEC IY
        self.emit(&[0xFD, 0x2B]); // DEC IY
        self.emit(&[0xFD, 0x5E, 0]); // LD E, (IY+0)
        self.emit(&[0xFD, 0x56, 1]); // LD D, (IY+1)
        self.emit(&[0xFD, 0x2B]); // DEC IY
        self.emit(&[0xFD, 0x2B]); // DEC IY
        self.emit(&[0xFD, 0x6E, 0]); // LD L, (IY+0)
        self.emit(&[0xFD, 0x66, 1]); // LD H, (IY+1)
        // Subtract: HL = HL - DE
        self.emit(&[0xB7]);       // OR A (clear carry)
        self.emit(&[0xED, 0x52]); // SBC HL, DE
        self.emit(&[0xFD, 0x75, 0]); // LD (IY+0), L
        self.emit(&[0xFD, 0x74, 1]); // LD (IY+1), H
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xE1]);       // POP HL (restore bytecode ptr)
        self.emit(&[0xC3]);       // JP interp_loop
        self.fixup("interp_loop");

        // MUL_I: pop two, push product
        self.label("interp_mul_i");
        self.emit(&[0xE5]);       // PUSH HL (save bytecode ptr)
        self.emit(&[0xFD, 0x2B]); // DEC IY (x4)
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x5E, 0]); // LD E, (IY+0)
        self.emit(&[0xFD, 0x56, 1]); // LD D, (IY+1)
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x6E, 0]); // LD L, (IY+0)
        self.emit(&[0xFD, 0x66, 1]); // LD H, (IY+1)
        self.emit(&[0xCD]);       // CALL mul16
        self.fixup("mul16");
        self.emit(&[0xFD, 0x75, 0]); // LD (IY+0), L
        self.emit(&[0xFD, 0x74, 1]); // LD (IY+1), H
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xE1]);       // POP HL (restore bytecode ptr)
        self.emit(&[0xC3]);       // JP interp_loop
        self.fixup("interp_loop");

        // DIV_I: pop two, push quotient
        self.label("interp_div_i");
        self.emit(&[0xE5]);       // PUSH HL (save bytecode ptr)
        self.emit(&[0xFD, 0x2B]); // DEC IY (x4)
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x5E, 0]); // LD E, (IY+0) - divisor
        self.emit(&[0xFD, 0x56, 1]); // LD D, (IY+1)
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x6E, 0]); // LD L, (IY+0) - dividend
        self.emit(&[0xFD, 0x66, 1]); // LD H, (IY+1)
        self.emit(&[0xCD]);       // CALL div16
        self.fixup("div16");
        self.emit(&[0xFD, 0x75, 0]); // LD (IY+0), L
        self.emit(&[0xFD, 0x74, 1]); // LD (IY+1), H
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xE1]);       // POP HL (restore bytecode ptr)
        self.emit(&[0xC3]);       // JP interp_loop
        self.fixup("interp_loop");

        // MOD_I: pop two, push remainder
        self.label("interp_mod_i");
        self.emit(&[0xE5]);       // PUSH HL (save bytecode ptr)
        self.emit(&[0xFD, 0x2B]); // DEC IY (x4)
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x5E, 0]); // LD E, (IY+0) - divisor
        self.emit(&[0xFD, 0x56, 1]); // LD D, (IY+1)
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x6E, 0]); // LD L, (IY+0) - dividend
        self.emit(&[0xFD, 0x66, 1]); // LD H, (IY+1)
        self.emit(&[0xCD]);       // CALL div16 (remainder in A)
        self.fixup("div16");
        // A has remainder, extend to 16-bit
        self.emit(&[0x6F]);       // LD L, A
        self.emit(&[0x26, 0x00]); // LD H, 0
        self.emit(&[0xFD, 0x75, 0]); // LD (IY+0), L
        self.emit(&[0xFD, 0x74, 1]); // LD (IY+1), H
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xE1]);       // POP HL (restore bytecode ptr)
        self.emit(&[0xC3]);       // JP interp_loop
        self.fixup("interp_loop");

        // NEG_I: pop one, push negated value
        self.label("interp_neg_i");
        self.emit(&[0xE5]);       // PUSH HL (save bytecode ptr)
        // Pop value into HL
        self.emit(&[0xFD, 0x2B]); // DEC IY
        self.emit(&[0xFD, 0x2B]); // DEC IY
        self.emit(&[0xFD, 0x6E, 0]); // LD L, (IY+0)
        self.emit(&[0xFD, 0x66, 1]); // LD H, (IY+1)
        // Negate HL: complement and increment
        self.emit(&[0x7D]);       // LD A, L
        self.emit(&[0x2F]);       // CPL
        self.emit(&[0x6F]);       // LD L, A
        self.emit(&[0x7C]);       // LD A, H
        self.emit(&[0x2F]);       // CPL
        self.emit(&[0x67]);       // LD H, A
        self.emit(&[0x23]);       // INC HL
        // Push result
        self.emit(&[0xFD, 0x75, 0]); // LD (IY+0), L
        self.emit(&[0xFD, 0x74, 1]); // LD (IY+1), H
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xE1]);       // POP HL (restore bytecode ptr)
        self.emit(&[0xC3]);       // JP interp_loop
        self.fixup("interp_loop");

        // PRINT_I: pop and print
        self.label("interp_print_i");
        self.emit(&[0xE5]);       // PUSH HL (save bytecode ptr)
        self.emit(&[0xFD, 0x2B]); // DEC IY (x2)
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x6E, 0]); // LD L, (IY+0)
        self.emit(&[0xFD, 0x66, 1]); // LD H, (IY+1)
        self.emit(&[0xCD]);       // CALL print_int
        self.fixup("print_int");
        self.emit(&[0xE1]);       // POP HL (restore bytecode ptr)
        self.emit(&[0xC3]);       // JP interp_loop
        self.fixup("interp_loop");

        // POP_I: pop and discard top of stack
        self.label("interp_pop_i");
        self.emit(&[0xFD, 0x2B]); // DEC IY (x2) - just move stack pointer down
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xC3]);       // JP interp_loop
        self.fixup("interp_loop");

        // EQ_I: pop two, push 1 if equal, 0 otherwise
        self.label("interp_eq_i");
        self.emit(&[0xE5]);       // PUSH HL (save bytecode ptr)
        self.emit(&[0xFD, 0x2B]); // DEC IY (x4)
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x5E, 0]); // LD E, (IY+0) - second operand
        self.emit(&[0xFD, 0x56, 1]); // LD D, (IY+1)
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x6E, 0]); // LD L, (IY+0) - first operand
        self.emit(&[0xFD, 0x66, 1]); // LD H, (IY+1)
        // Compare HL with DE: check if HL == DE
        self.emit(&[0xB7]);       // OR A (clear carry)
        self.emit(&[0xED, 0x52]); // SBC HL, DE
        self.emit(&[0x21, 0, 0]); // LD HL, 0 (assume false)
        self.emit(&[0xC2]);       // JP NZ, interp_cmp_done
        self.fixup("interp_cmp_done");
        self.emit(&[0x2C]);       // INC L (HL = 1, true)
        self.label("interp_cmp_done");
        self.emit(&[0xFD, 0x75, 0]); // LD (IY+0), L
        self.emit(&[0xFD, 0x74, 1]); // LD (IY+1), H
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xE1]);       // POP HL (restore bytecode ptr)
        self.emit(&[0xC3]);       // JP interp_loop
        self.fixup("interp_loop");

        // NE_I: pop two, push 1 if not equal, 0 otherwise
        self.label("interp_ne_i");
        self.emit(&[0xE5]);       // PUSH HL
        self.emit(&[0xFD, 0x2B]); // DEC IY (x4)
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x5E, 0]); // LD E, (IY+0)
        self.emit(&[0xFD, 0x56, 1]); // LD D, (IY+1)
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x6E, 0]); // LD L, (IY+0)
        self.emit(&[0xFD, 0x66, 1]); // LD H, (IY+1)
        self.emit(&[0xB7]);       // OR A
        self.emit(&[0xED, 0x52]); // SBC HL, DE
        self.emit(&[0x21, 0, 0]); // LD HL, 0
        self.emit(&[0xCA]);       // JP Z, interp_ne_done
        self.fixup("interp_ne_done");
        self.emit(&[0x2C]);       // INC L (HL = 1)
        self.label("interp_ne_done");
        self.emit(&[0xFD, 0x75, 0]); // LD (IY+0), L
        self.emit(&[0xFD, 0x74, 1]); // LD (IY+1), H
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xE1]);       // POP HL
        self.emit(&[0xC3]);       // JP interp_loop
        self.fixup("interp_loop");

        // LT_I: pop two, push 1 if first < second (signed), 0 otherwise
        self.label("interp_lt_i");
        self.emit(&[0xE5]);       // PUSH HL
        self.emit(&[0xFD, 0x2B]); // DEC IY (x4)
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x5E, 0]); // LD E, (IY+0) - second (right operand)
        self.emit(&[0xFD, 0x56, 1]); // LD D, (IY+1)
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x6E, 0]); // LD L, (IY+0) - first (left operand)
        self.emit(&[0xFD, 0x66, 1]); // LD H, (IY+1)
        // Signed compare: HL < DE?
        // Subtract DE from HL, check sign and overflow
        self.emit(&[0xB7]);       // OR A (clear carry)
        self.emit(&[0xED, 0x52]); // SBC HL, DE  (HL = HL - DE)
        // After SBC: S flag = sign of result, V flag = overflow
        // For signed less-than: S xor V
        self.emit(&[0x21, 0, 0]); // LD HL, 0 (assume false)
        self.emit(&[0xF2]);       // JP P, interp_lt_pos (if positive/S=0)
        self.fixup("interp_lt_pos");
        // S=1 (negative): true if V=0
        self.emit(&[0xE2]);       // JP PO, interp_lt_true (if V=0, parity odd)
        self.fixup("interp_lt_true");
        self.emit(&[0xC3]);       // JP interp_lt_done
        self.fixup("interp_lt_done");
        self.label("interp_lt_pos");
        // S=0 (positive): true if V=1
        self.emit(&[0xEA]);       // JP PE, interp_lt_true (if V=1, parity even)
        self.fixup("interp_lt_true");
        self.emit(&[0xC3]);       // JP interp_lt_done
        self.fixup("interp_lt_done");
        self.label("interp_lt_true");
        self.emit(&[0x2C]);       // INC L (HL = 1)
        self.label("interp_lt_done");
        self.emit(&[0xFD, 0x75, 0]); // LD (IY+0), L
        self.emit(&[0xFD, 0x74, 1]); // LD (IY+1), H
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xE1]);       // POP HL
        self.emit(&[0xC3]);       // JP interp_loop
        self.fixup("interp_loop");

        // GE_I: pop two, push 1 if first >= second (signed)
        // This is NOT (first < second)
        self.label("interp_ge_i");
        self.emit(&[0xE5]);       // PUSH HL
        self.emit(&[0xFD, 0x2B]); // DEC IY (x4)
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x5E, 0]); // LD E, (IY+0)
        self.emit(&[0xFD, 0x56, 1]); // LD D, (IY+1)
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x6E, 0]); // LD L, (IY+0)
        self.emit(&[0xFD, 0x66, 1]); // LD H, (IY+1)
        self.emit(&[0xB7]);       // OR A
        self.emit(&[0xED, 0x52]); // SBC HL, DE
        self.emit(&[0x21, 1, 0]); // LD HL, 1 (assume true/>=)
        self.emit(&[0xF2]);       // JP P, interp_ge_pos
        self.fixup("interp_ge_pos");
        // S=1: true if V=1 (overflow means not really negative)
        self.emit(&[0xEA]);       // JP PE, interp_ge_done
        self.fixup("interp_ge_done");
        self.emit(&[0x2D]);       // DEC L (HL = 0, false)
        self.emit(&[0xC3]);       // JP interp_ge_done
        self.fixup("interp_ge_done");
        self.label("interp_ge_pos");
        // S=0: true if V=0
        self.emit(&[0xE2]);       // JP PO, interp_ge_done
        self.fixup("interp_ge_done");
        self.emit(&[0x2D]);       // DEC L (HL = 0, false)
        self.label("interp_ge_done");
        self.emit(&[0xFD, 0x75, 0]); // LD (IY+0), L
        self.emit(&[0xFD, 0x74, 1]); // LD (IY+1), H
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xE1]);       // POP HL
        self.emit(&[0xC3]);       // JP interp_loop
        self.fixup("interp_loop");

        // GT_I: first > second is same as second < first
        self.label("interp_gt_i");
        self.emit(&[0xE5]);       // PUSH HL
        self.emit(&[0xFD, 0x2B]); // DEC IY (x4)
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x6E, 0]); // LD L, (IY+0) - swap: get second into HL
        self.emit(&[0xFD, 0x66, 1]); // LD H, (IY+1)
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x5E, 0]); // LD E, (IY+0) - swap: get first into DE
        self.emit(&[0xFD, 0x56, 1]); // LD D, (IY+1)
        // Now compare HL < DE (second < first, which is first > second)
        self.emit(&[0xB7]);       // OR A
        self.emit(&[0xED, 0x52]); // SBC HL, DE
        self.emit(&[0x21, 0, 0]); // LD HL, 0
        self.emit(&[0xF2]);       // JP P, interp_gt_pos
        self.fixup("interp_gt_pos");
        self.emit(&[0xE2]);       // JP PO, interp_gt_true
        self.fixup("interp_gt_true");
        self.emit(&[0xC3]);       // JP interp_gt_done
        self.fixup("interp_gt_done");
        self.label("interp_gt_pos");
        self.emit(&[0xEA]);       // JP PE, interp_gt_true
        self.fixup("interp_gt_true");
        self.emit(&[0xC3]);       // JP interp_gt_done
        self.fixup("interp_gt_done");
        self.label("interp_gt_true");
        self.emit(&[0x2C]);       // INC L
        self.label("interp_gt_done");
        self.emit(&[0xFD, 0x75, 0]); // LD (IY+0), L
        self.emit(&[0xFD, 0x74, 1]); // LD (IY+1), H
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xE1]);       // POP HL
        self.emit(&[0xC3]);       // JP interp_loop
        self.fixup("interp_loop");

        // LE_I: first <= second is NOT (first > second)
        self.label("interp_le_i");
        self.emit(&[0xE5]);       // PUSH HL
        self.emit(&[0xFD, 0x2B]); // DEC IY (x4)
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x6E, 0]); // LD L, (IY+0) - get second
        self.emit(&[0xFD, 0x66, 1]); // LD H, (IY+1)
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x5E, 0]); // LD E, (IY+0) - get first
        self.emit(&[0xFD, 0x56, 1]); // LD D, (IY+1)
        // Check second < first (which means first > second)
        self.emit(&[0xB7]);       // OR A
        self.emit(&[0xED, 0x52]); // SBC HL, DE (second - first)
        self.emit(&[0x21, 1, 0]); // LD HL, 1 (assume true/<=)
        self.emit(&[0xF2]);       // JP P, interp_le_pos
        self.fixup("interp_le_pos");
        // S=1: if V=0, second < first, so first > second, so NOT <=
        self.emit(&[0xEA]);       // JP PE, interp_le_done (V=1 means it's <=)
        self.fixup("interp_le_done");
        self.emit(&[0x2D]);       // DEC L (HL = 0, false)
        self.emit(&[0xC3]);       // JP interp_le_done
        self.fixup("interp_le_done");
        self.label("interp_le_pos");
        // S=0: if V=0, result is correct (not <), so <=
        self.emit(&[0xE2]);       // JP PO, interp_le_done (V=0 means it's <=)
        self.fixup("interp_le_done");
        self.emit(&[0x2D]);       // DEC L (HL = 0, false)
        self.label("interp_le_done");
        self.emit(&[0xFD, 0x75, 0]); // LD (IY+0), L
        self.emit(&[0xFD, 0x74, 1]); // LD (IY+1), H
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xE1]);       // POP HL
        self.emit(&[0xC3]);       // JP interp_loop
        self.fixup("interp_loop");

        // LOAD: load value from variable address onto stack
        self.label("interp_load");
        self.emit(&[0xE5]);       // PUSH HL (save bytecode ptr)
        // Read address from bytecode
        self.emit(&[0x5E]);       // LD E, (HL)
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0x56]);       // LD D, (HL)
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0xE3]);       // EX (SP), HL (save updated bytecode ptr, get old one)
        // DE has variable address, load value from (DE)
        self.emit(&[0x1A]);       // LD A, (DE)
        self.emit(&[0x6F]);       // LD L, A
        self.emit(&[0x13]);       // INC DE
        self.emit(&[0x1A]);       // LD A, (DE)
        self.emit(&[0x67]);       // LD H, A
        // Push value onto eval stack
        self.emit(&[0xFD, 0x75, 0]); // LD (IY+0), L
        self.emit(&[0xFD, 0x74, 1]); // LD (IY+1), H
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xE1]);       // POP HL (restore bytecode ptr)
        self.emit(&[0xC3]);       // JP interp_loop
        self.fixup("interp_loop");

        // STORE: pop value from stack and store to variable address
        self.label("interp_store");
        self.emit(&[0xE5]);       // PUSH HL (save bytecode ptr)
        // Read address from bytecode
        self.emit(&[0x5E]);       // LD E, (HL)
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0x56]);       // LD D, (HL)
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0xE3]);       // EX (SP), HL (save updated bytecode ptr)
        // Pop value from eval stack
        self.emit(&[0xFD, 0x2B]); // DEC IY
        self.emit(&[0xFD, 0x2B]); // DEC IY
        self.emit(&[0xFD, 0x6E, 0]); // LD L, (IY+0)
        self.emit(&[0xFD, 0x66, 1]); // LD H, (IY+1)
        // Store value to (DE)
        self.emit(&[0x7D]);       // LD A, L
        self.emit(&[0x12]);       // LD (DE), A
        self.emit(&[0x13]);       // INC DE
        self.emit(&[0x7C]);       // LD A, H
        self.emit(&[0x12]);       // LD (DE), A
        self.emit(&[0xE1]);       // POP HL (restore bytecode ptr)
        self.emit(&[0xC3]);       // JP interp_loop
        self.fixup("interp_loop");

        // BAND_I: pop two, push bitwise AND
        self.label("interp_band_i");
        self.emit(&[0xE5]);       // PUSH HL (save bytecode ptr)
        self.emit(&[0xFD, 0x2B]); // DEC IY (x4)
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x5E, 0]); // LD E, (IY+0) - right operand low
        self.emit(&[0xFD, 0x56, 1]); // LD D, (IY+1) - right operand high
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x6E, 0]); // LD L, (IY+0) - left operand low
        self.emit(&[0xFD, 0x66, 1]); // LD H, (IY+1) - left operand high
        // HL AND DE
        self.emit(&[0x7C]);       // LD A, H
        self.emit(&[0xA2]);       // AND D
        self.emit(&[0x67]);       // LD H, A
        self.emit(&[0x7D]);       // LD A, L
        self.emit(&[0xA3]);       // AND E
        self.emit(&[0x6F]);       // LD L, A
        // Push result
        self.emit(&[0xFD, 0x75, 0]); // LD (IY+0), L
        self.emit(&[0xFD, 0x74, 1]); // LD (IY+1), H
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xE1]);       // POP HL
        self.emit(&[0xC3]);       // JP interp_loop
        self.fixup("interp_loop");

        // BOR_I: pop two, push bitwise OR
        self.label("interp_bor_i");
        self.emit(&[0xE5]);       // PUSH HL
        self.emit(&[0xFD, 0x2B]); // DEC IY (x4)
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x5E, 0]); // LD E, (IY+0)
        self.emit(&[0xFD, 0x56, 1]); // LD D, (IY+1)
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x6E, 0]); // LD L, (IY+0)
        self.emit(&[0xFD, 0x66, 1]); // LD H, (IY+1)
        // HL OR DE
        self.emit(&[0x7C]);       // LD A, H
        self.emit(&[0xB2]);       // OR D
        self.emit(&[0x67]);       // LD H, A
        self.emit(&[0x7D]);       // LD A, L
        self.emit(&[0xB3]);       // OR E
        self.emit(&[0x6F]);       // LD L, A
        // Push result
        self.emit(&[0xFD, 0x75, 0]); // LD (IY+0), L
        self.emit(&[0xFD, 0x74, 1]); // LD (IY+1), H
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xE1]);       // POP HL
        self.emit(&[0xC3]);       // JP interp_loop
        self.fixup("interp_loop");

        // BXOR_I: pop two, push bitwise XOR
        self.label("interp_bxor_i");
        self.emit(&[0xE5]);       // PUSH HL
        self.emit(&[0xFD, 0x2B]); // DEC IY (x4)
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x5E, 0]); // LD E, (IY+0)
        self.emit(&[0xFD, 0x56, 1]); // LD D, (IY+1)
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x6E, 0]); // LD L, (IY+0)
        self.emit(&[0xFD, 0x66, 1]); // LD H, (IY+1)
        // HL XOR DE
        self.emit(&[0x7C]);       // LD A, H
        self.emit(&[0xAA]);       // XOR D
        self.emit(&[0x67]);       // LD H, A
        self.emit(&[0x7D]);       // LD A, L
        self.emit(&[0xAB]);       // XOR E
        self.emit(&[0x6F]);       // LD L, A
        // Push result
        self.emit(&[0xFD, 0x75, 0]); // LD (IY+0), L
        self.emit(&[0xFD, 0x74, 1]); // LD (IY+1), H
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xE1]);       // POP HL
        self.emit(&[0xC3]);       // JP interp_loop
        self.fixup("interp_loop");

        // BNOT_I: pop one, push bitwise NOT (unary)
        self.label("interp_bnot_i");
        self.emit(&[0xE5]);       // PUSH HL
        self.emit(&[0xFD, 0x2B]); // DEC IY
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x6E, 0]); // LD L, (IY+0)
        self.emit(&[0xFD, 0x66, 1]); // LD H, (IY+1)
        // NOT HL (complement)
        self.emit(&[0x7C]);       // LD A, H
        self.emit(&[0x2F]);       // CPL
        self.emit(&[0x67]);       // LD H, A
        self.emit(&[0x7D]);       // LD A, L
        self.emit(&[0x2F]);       // CPL
        self.emit(&[0x6F]);       // LD L, A
        // Push result
        self.emit(&[0xFD, 0x75, 0]); // LD (IY+0), L
        self.emit(&[0xFD, 0x74, 1]); // LD (IY+1), H
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xE1]);       // POP HL
        self.emit(&[0xC3]);       // JP interp_loop
        self.fixup("interp_loop");

        // SHL_I: pop two, shift left (left << right)
        self.label("interp_shl_i");
        self.emit(&[0xE5]);       // PUSH HL
        self.emit(&[0xFD, 0x2B]); // DEC IY (x4)
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x5E, 0]); // LD E, (IY+0) - shift count (low byte only)
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x6E, 0]); // LD L, (IY+0) - value to shift
        self.emit(&[0xFD, 0x66, 1]); // LD H, (IY+1)
        // Shift HL left E times
        self.emit(&[0x7B]);       // LD A, E
        self.emit(&[0xB7]);       // OR A (check if 0)
        self.emit(&[0x28, 0x04]); // JR Z, +4 (skip 4-byte loop if 0)
        self.label("interp_shl_loop");
        self.emit(&[0x29]);       // ADD HL, HL (shift left)
        self.emit(&[0x3D]);       // DEC A
        self.emit(&[0x20, 0xFC]); // JR NZ, interp_shl_loop (-4)
        // Push result
        self.emit(&[0xFD, 0x75, 0]); // LD (IY+0), L
        self.emit(&[0xFD, 0x74, 1]); // LD (IY+1), H
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xE1]);       // POP HL
        self.emit(&[0xC3]);       // JP interp_loop
        self.fixup("interp_loop");

        // SHR_I: pop two, shift right (left >> right) - logical shift
        self.label("interp_shr_i");
        self.emit(&[0xE5]);       // PUSH HL
        self.emit(&[0xFD, 0x2B]); // DEC IY (x4)
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x5E, 0]); // LD E, (IY+0) - shift count
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x6E, 0]); // LD L, (IY+0) - value to shift
        self.emit(&[0xFD, 0x66, 1]); // LD H, (IY+1)
        // Shift HL right E times (logical)
        self.emit(&[0x7B]);       // LD A, E
        self.emit(&[0xB7]);       // OR A (check if 0)
        self.emit(&[0x28, 0x07]); // JR Z, +7 (skip 7-byte loop if 0)
        self.label("interp_shr_loop");
        self.emit(&[0xCB, 0x3C]); // SRL H (shift right logical) - 2 bytes
        self.emit(&[0xCB, 0x1D]); // RR L (rotate right through carry) - 2 bytes
        self.emit(&[0x3D]);       // DEC A - 1 byte
        self.emit(&[0x20, 0xF9]); // JR NZ, interp_shr_loop (-7: from pos 7 back to pos 0)
        // Push result
        self.emit(&[0xFD, 0x75, 0]); // LD (IY+0), L
        self.emit(&[0xFD, 0x74, 1]); // LD (IY+1), H
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xE1]);       // POP HL
        self.emit(&[0xC3]);       // JP interp_loop
        self.fixup("interp_loop");

        // LAND_I: pop two, push logical AND (both non-zero -> 1, else 0)
        self.label("interp_land_i");
        self.emit(&[0xE5]);       // PUSH HL
        self.emit(&[0xFD, 0x2B]); // DEC IY (x4)
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x5E, 0]); // LD E, (IY+0)
        self.emit(&[0xFD, 0x56, 1]); // LD D, (IY+1)
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x6E, 0]); // LD L, (IY+0)
        self.emit(&[0xFD, 0x66, 1]); // LD H, (IY+1)
        // Check if HL is non-zero
        // Positions: 0:LD A,H, 1:OR L, 2-3:JR Z, 4:LD A,D, 5:OR E, 6-7:JR Z, 8-10:LD HL,1, 11-12:JR, 13:false label, 13-15:LD HL,0, 16:done
        self.emit(&[0x7C]);       // LD A, H
        self.emit(&[0xB5]);       // OR L
        self.emit(&[0x28, 0x09]); // JR Z, land_false (+9: PC=4, target=13)
        // Check if DE is non-zero
        self.emit(&[0x7A]);       // LD A, D
        self.emit(&[0xB3]);       // OR E
        self.emit(&[0x28, 0x05]); // JR Z, land_false (+5: PC=8, target=13)
        // Both non-zero, result is 1
        self.emit(&[0x21, 1, 0]); // LD HL, 1
        self.emit(&[0x18, 0x03]); // JR land_done (+3: PC=13, target=16)
        self.label("interp_land_false");
        self.emit(&[0x21, 0, 0]); // LD HL, 0
        self.label("interp_land_done");
        self.emit(&[0xFD, 0x75, 0]); // LD (IY+0), L
        self.emit(&[0xFD, 0x74, 1]); // LD (IY+1), H
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xE1]);       // POP HL
        self.emit(&[0xC3]);       // JP interp_loop
        self.fixup("interp_loop");

        // LOR_I: pop two, push logical OR (either non-zero -> 1, else 0)
        self.label("interp_lor_i");
        self.emit(&[0xE5]);       // PUSH HL
        self.emit(&[0xFD, 0x2B]); // DEC IY (x4)
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x5E, 0]); // LD E, (IY+0)
        self.emit(&[0xFD, 0x56, 1]); // LD D, (IY+1)
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x6E, 0]); // LD L, (IY+0)
        self.emit(&[0xFD, 0x66, 1]); // LD H, (IY+1)
        // Check if HL is non-zero
        // Positions: 0:LD A,H, 1:OR L, 2-3:JR NZ, 4:LD A,D, 5:OR E, 6-7:JR Z, 8:true label, 8-10:LD HL,1, 11-12:JR, 13:false label, 13-15:LD HL,0, 16:done
        self.emit(&[0x7C]);       // LD A, H
        self.emit(&[0xB5]);       // OR L
        self.emit(&[0x20, 0x04]); // JR NZ, lor_true (+4: PC=4, target=8)
        // Check if DE is non-zero
        self.emit(&[0x7A]);       // LD A, D
        self.emit(&[0xB3]);       // OR E
        self.emit(&[0x28, 0x05]); // JR Z, lor_false (+5: PC=8, target=13)
        self.label("interp_lor_true");
        self.emit(&[0x21, 1, 0]); // LD HL, 1
        self.emit(&[0x18, 0x03]); // JR lor_done (+3: PC=13, target=16)
        self.label("interp_lor_false");
        self.emit(&[0x21, 0, 0]); // LD HL, 0
        self.label("interp_lor_done");
        self.emit(&[0xFD, 0x75, 0]); // LD (IY+0), L
        self.emit(&[0xFD, 0x74, 1]); // LD (IY+1), H
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xE1]);       // POP HL
        self.emit(&[0xC3]);       // JP interp_loop
        self.fixup("interp_loop");

        // LNOT_I: pop one, push logical NOT (non-zero -> 0, zero -> 1)
        self.label("interp_lnot_i");
        self.emit(&[0xE5]);       // PUSH HL
        self.emit(&[0xFD, 0x2B]); // DEC IY
        self.emit(&[0xFD, 0x2B]);
        self.emit(&[0xFD, 0x6E, 0]); // LD L, (IY+0)
        self.emit(&[0xFD, 0x66, 1]); // LD H, (IY+1)
        // Check if HL is zero
        self.emit(&[0x7C]);       // LD A, H
        self.emit(&[0xB5]);       // OR L
        self.emit(&[0x21, 1, 0]); // LD HL, 1 (assume zero, result = 1)
        self.emit(&[0x28, 0x01]); // JR Z, lnot_done (+1: skip 1-byte DEC L)
        self.emit(&[0x2D]);       // DEC L (HL = 0)
        self.label("interp_lnot_done");
        self.emit(&[0xFD, 0x75, 0]); // LD (IY+0), L
        self.emit(&[0xFD, 0x74, 1]); // LD (IY+1), H
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xE1]);       // POP HL
        self.emit(&[0xC3]);       // JP interp_loop
        self.fixup("interp_loop");

        // JMP: unconditional jump to address
        // Bytecode: JMP, addr_low, addr_high
        self.label("interp_jmp");
        self.emit(&[0x5E]);       // LD E, (HL) - get low byte of address
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0x56]);       // LD D, (HL) - get high byte of address
        self.emit(&[0xEB]);       // EX DE, HL - HL = target address
        self.emit(&[0xC3]);       // JP interp_loop
        self.fixup("interp_loop");

        // JZ: jump to address if top of stack is zero
        // Bytecode: JZ, addr_low, addr_high
        // Pops the condition value from stack
        self.label("interp_jz");
        // DEBUG: uncomment to verify JZ is being dispatched
        // self.emit(&[0x76]);       // HALT - for debugging
        // Pop condition from stack
        self.emit(&[0xFD, 0x2B]); // DEC IY
        self.emit(&[0xFD, 0x2B]); // DEC IY
        self.emit(&[0xFD, 0x5E, 0]); // LD E, (IY+0)
        self.emit(&[0xFD, 0x56, 1]); // LD D, (IY+1)
        // Check if DE is zero
        self.emit(&[0x7A]);       // LD A, D
        self.emit(&[0xB3]);       // OR E
        self.emit(&[0x20, 0x08]); // JR NZ, interp_jz_skip (+8: skip LD E, INC HL, LD D, PUSH, POP, JP = 1+1+1+1+1+3=8)
        // Zero - take the jump
        self.emit(&[0x5E]);       // LD E, (HL) - get low byte of address
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0x56]);       // LD D, (HL) - get high byte of address
        self.emit(&[0xD5]);       // PUSH DE - save target address
        self.emit(&[0xE1]);       // POP HL - HL = target address
        self.emit(&[0xC3]);       // JP interp_loop
        self.fixup("interp_loop");
        // Non-zero - skip address bytes and continue
        self.label("interp_jz_skip");
        self.emit(&[0x23]);       // INC HL - skip low byte
        self.emit(&[0x23]);       // INC HL - skip high byte
        self.emit(&[0xC3]);       // JP interp_loop
        self.fixup("interp_loop");

        // FCALL: call function
        // Bytecode: FCALL, func_idx
        // - Pop argument from eval stack
        // - Look up function in FUNC_TABLE
        // - Store argument in param variable
        // - Push return address (current HL)
        // - Jump to function body
        self.label("interp_fcall");
        self.emit(&[0x5E]);       // LD E, (HL) - func_idx
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0xE5]);       // PUSH HL (save return address)

        // Pop argument from eval stack into BC
        self.emit(&[0xFD, 0x2B]); // DEC IY
        self.emit(&[0xFD, 0x2B]); // DEC IY
        self.emit(&[0xFD, 0x4E, 0]); // LD C, (IY+0) - arg low
        self.emit(&[0xFD, 0x46, 1]); // LD B, (IY+1) - arg high
        // BC = argument value

        // Calculate function table entry: FUNC_TABLE + func_idx * 4
        self.emit(&[0x7B]);       // LD A, E (func_idx)
        self.emit(&[0x87]);       // ADD A, A (x2)
        self.emit(&[0x87]);       // ADD A, A (x4)
        self.emit(&[0x6F]);       // LD L, A
        self.emit(&[0x26, 0x00]); // LD H, 0
        self.emit(&[0x11]);       // LD DE, FUNC_TABLE
        self.emit_word(FUNC_TABLE);
        self.emit(&[0x19]);       // ADD HL, DE (HL = &table[func_idx])

        // Read table entry: [param_idx, defined, body_lo, body_hi]
        // Save param_idx to A first
        self.emit(&[0x7E]);       // LD A, (HL) - param_idx
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0xF5]);       // PUSH AF (save param_idx in A)
        self.emit(&[0x7E]);       // LD A, (HL) - defined flag
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0xB7]);       // OR A
        self.emit(&[0xCA]);       // JP Z, interp_fcall_undef (if not defined)
        self.fixup("interp_fcall_undef");

        // Get body address into DE
        self.emit(&[0x5E]);       // LD E, (HL) - body_lo
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0x56]);       // LD D, (HL) - body_hi
        // DE = body address

        // Store argument (BC) in param variable
        // param_idx is on stack, pop it to A
        self.emit(&[0xF1]);       // POP AF (param_idx in A)
        // Calculate param variable address: SYMBOL_TABLE + param_idx * 2
        self.emit(&[0x87]);       // ADD A, A (x2)
        self.emit(&[0x6F]);       // LD L, A
        self.emit(&[0x26, 0x00]); // LD H, 0
        self.emit(&[0xD5]);       // PUSH DE (save body addr)
        self.emit(&[0x11]);       // LD DE, SYMBOL_TABLE
        self.emit_word(SYMBOL_TABLE);
        self.emit(&[0x19]);       // ADD HL, DE (HL = &var[param_idx])
        self.emit(&[0xD1]);       // POP DE (restore body addr)
        // Store BC (argument) to (HL)
        self.emit(&[0x71]);       // LD (HL), C
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0x70]);       // LD (HL), B

        // Jump to function body (DE has body address)
        self.emit(&[0xEB]);       // EX DE, HL (HL = body address)
        self.emit(&[0xC3]);       // JP interp_loop
        self.fixup("interp_loop");

        self.label("interp_fcall_undef");
        // Function not defined - pop param_idx, return addr, and push 0 as result
        self.emit(&[0xF1]);       // POP AF (discard param_idx)
        self.emit(&[0xE1]);       // POP HL (restore return addr)
        self.emit(&[0xFD, 0x36, 0, 0]); // LD (IY+0), 0
        self.emit(&[0xFD, 0x36, 1, 0]); // LD (IY+1), 0
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xFD, 0x23]); // INC IY
        self.emit(&[0xC3]);       // JP interp_loop
        self.fixup("interp_loop");

        // FRET: return from function
        // Pop return address from stack and continue there
        self.label("interp_fret");
        self.emit(&[0xE1]);       // POP HL (return address)
        self.emit(&[0xC3]);       // JP interp_loop
        self.fixup("interp_loop");
    }

    fn emit_repl_loop(&mut self) {
        // Main REPL loop
        self.label("repl_loop");

        // Print prompt
        self.emit(&[0x21]);       // LD HL, prompt_str
        self.fixup("prompt_str");
        self.emit(&[0xCD]);       // CALL print_string
        self.fixup("print_string");

        // Read line
        self.emit(&[0xCD]);       // CALL read_line
        self.fixup("read_line");

        // Check if empty (A = length)
        self.emit(&[0xB7]);       // OR A
        self.emit(&[0xCA]);       // JP Z, repl_loop (if empty, loop)
        self.fixup("repl_loop");

        // Initialize IX to bytecode buffer before parsing
        self.emit(&[0xDD, 0x21]); // LD IX, BYTECODE_BUF
        self.emit_word(BYTECODE_BUF);

        // Statement dispatch - we jump back here after semicolons
        self.label("repl_stmt_dispatch");

        // Check if this is a let statement
        self.emit(&[0xE5]);       // PUSH HL (save input pointer)
        self.emit(&[0xCD]);       // CALL lex_next
        self.fixup("lex_next");
        self.emit(&[0xFE, tok::LET]);
        self.emit(&[0xCA]);       // JP Z, repl_let
        self.fixup("repl_let");

        // Check if this is a fun definition
        self.emit(&[0xFE, tok::FUN_KW]);
        self.emit(&[0xCA]);       // JP Z, repl_fun
        self.fixup("repl_fun");

        // Check if this is an assignment: ident := expr
        self.emit(&[0xFE, tok::IDENT]);
        self.emit(&[0xC2]);       // JP NZ, repl_not_assign
        self.fixup("repl_not_assign");
        // Got IDENT - save identifier char (in SCRATCH) and check for :=
        // HL is past identifier, peek at next token
        self.emit(&[0xE5]);       // PUSH HL (save position after ident)
        self.emit(&[0xCD]);       // CALL lex_next
        self.fixup("lex_next");
        self.emit(&[0xFE, tok::ASSIGN]);
        self.emit(&[0xC2]);       // JP NZ, repl_not_assign_restore
        self.fixup("repl_not_assign_restore");
        // It's an assignment! Clean up stack and handle it
        self.emit(&[0xD1]);       // POP DE (discard saved HL after ident)
        self.emit(&[0xD1]);       // POP DE (discard original saved input)
        self.emit(&[0xC3]);       // JP repl_assign
        self.fixup("repl_assign");

        self.label("repl_not_assign_restore");
        // Not an assignment, need to restore original input and parse as expression
        // Stack: [original input, after ident]
        self.emit(&[0xE1]);       // POP HL (discard position after ident)
        self.emit(&[0xE1]);       // POP HL (restore original input pointer)
        self.emit(&[0xC3]);       // JP repl_parse_expr
        self.fixup("repl_parse_expr");

        self.label("repl_not_assign");
        // Not IDENT token, just restore and parse
        self.emit(&[0xE1]);       // POP HL (restore original input pointer)

        self.label("repl_parse_expr");
        self.emit(&[0xCD]);       // CALL parse_expr
        self.fixup("parse_expr");

        // Check for semicolon - if found, emit POP_I and parse another statement
        self.emit(&[0xCD]);       // CALL lex_next
        self.fixup("lex_next");
        self.emit(&[0xFE, tok::SEMI]); // CP SEMI
        self.emit(&[0x20, 0x09]); // JR NZ, +9 (not semi, go to finalize)
        // It's a semicolon - emit POP_I to discard this result, parse more
        self.emit(&[0xDD, 0x36, 0, op::POP_I]); // LD (IX+0), POP_I
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xC3]);       // JP repl_stmt_dispatch (loop back for more statements)
        self.fixup("repl_stmt_dispatch");

        // Not a semicolon - finalize with PRINT_I and END
        // Auto-add PRINT_I at end before END
        self.emit(&[0xDD, 0x36, 0, op::PRINT_I]); // LD (IX+0), PRINT_I
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xDD, 0x36, 0, op::END]); // LD (IX+0), END

        // Execute bytecode
        self.emit(&[0xCD]);       // CALL interpret
        self.fixup("interpret");

        // Print newline
        self.emit(&[0xCD]);       // CALL print_newline
        self.fixup("print_newline");

        // Loop
        self.emit(&[0xC3]);       // JP repl_loop
        self.fixup("repl_loop");

        // Handle let statement: let <ident> = <expr>
        self.label("repl_let");
        self.emit(&[0xF1]);       // POP AF (discard saved input pointer, we used HL from lex)
        // HL now points past "let", get identifier
        self.emit(&[0xCD]);       // CALL lex_next
        self.fixup("lex_next");
        self.emit(&[0xFE, tok::IDENT]);
        self.emit(&[0xC2]);       // JP NZ, repl_loop (error: expected identifier)
        self.fixup("repl_loop");

        // Variable char is in SCRATCH, calculate address
        // Save HL (input pointer) first
        self.emit(&[0xE5]);       // PUSH HL (save input pointer)
        self.emit(&[0x3A]);       // LD A, (SCRATCH)
        self.emit_word(SCRATCH);
        // Convert uppercase to lowercase if needed (emulator sends uppercase)
        self.emit(&[0xFE, 0x5B]); // CP 'Z'+1
        self.emit(&[0x30, 0x02]); // JR NC, +2 (skip if >= '[')
        self.emit(&[0xC6, 0x20]); // ADD A, 0x20 (convert 'A'-'Z' to 'a'-'z')
        self.emit(&[0xD6, 0x61]); // SUB 'a'
        self.emit(&[0x87]);       // ADD A, A (multiply by 2)
        self.emit(&[0x5F]);       // LD E, A
        self.emit(&[0x16, 0x00]); // LD D, 0
        // Calculate variable address
        self.emit(&[0x21]);       // LD HL, SYMBOL_TABLE
        self.emit_word(SYMBOL_TABLE);
        self.emit(&[0x19]);       // ADD HL, DE
        self.emit(&[0x44]);       // LD B, H
        self.emit(&[0x4D]);       // LD C, L (BC = variable address)
        // Restore HL (input pointer)
        self.emit(&[0xE1]);       // POP HL
        self.emit(&[0xC5]);       // PUSH BC (save variable address)

        // Expect '='
        self.emit(&[0xCD]);       // CALL lex_next
        self.fixup("lex_next");
        self.emit(&[0xFE, tok::EQ]);
        self.emit(&[0xC2]);       // JP NZ, repl_let_error
        self.fixup("repl_let_error");

        // Parse expression (HL already past '=')
        self.emit(&[0xCD]);       // CALL parse_expr
        self.fixup("parse_expr");

        // Emit STORE opcode with address from BC
        self.emit(&[0xC1]);       // POP BC (restore variable address)
        self.emit(&[0xDD, 0x36, 0, op::STORE]); // LD (IX+0), STORE
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xDD, 0x71, 0]); // LD (IX+0), C (low byte of address)
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xDD, 0x70, 0]); // LD (IX+0), B (high byte of address)
        self.emit(&[0xDD, 0x23]); // INC IX

        // Check for semicolon - if found, emit POP_I and continue with more statements
        self.emit(&[0xCD]);       // CALL lex_next
        self.fixup("lex_next");
        self.emit(&[0xFE, tok::SEMI]); // CP SEMI
        self.emit(&[0x20, 0x09]); // JR NZ, +9 (not semi, go to finalize)
        // It's a semicolon - emit POP_I to discard store result, parse more
        self.emit(&[0xDD, 0x36, 0, op::POP_I]); // LD (IX+0), POP_I
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xC3]);       // JP repl_stmt_dispatch (loop back for more)
        self.fixup("repl_stmt_dispatch");

        // Not a semicolon - finalize let (no output for standalone let)
        self.emit(&[0xDD, 0x36, 0, op::END]); // LD (IX+0), END

        // Execute bytecode
        self.emit(&[0xCD]);       // CALL interpret
        self.fixup("interpret");

        // Loop (no output for let)
        self.emit(&[0xC3]);       // JP repl_loop
        self.fixup("repl_loop");

        // Error in let statement
        self.label("repl_let_error");
        self.emit(&[0xC1]);       // POP BC (clean up stack)
        self.emit(&[0xC3]);       // JP repl_loop
        self.fixup("repl_loop");

        // Handle assignment: ident := expr
        // Identifier char is already in SCRATCH, HL is past ':='
        self.label("repl_assign");
        // Save HL (input pointer past ':=') before calculating address
        self.emit(&[0xE5]);       // PUSH HL (save input pointer)

        // Calculate variable address from SCRATCH
        self.emit(&[0x3A]);       // LD A, (SCRATCH)
        self.emit_word(SCRATCH);
        // Convert uppercase to lowercase if needed
        self.emit(&[0xFE, 0x5B]); // CP 'Z'+1
        self.emit(&[0x30, 0x02]); // JR NC, +2 (skip if >= '[')
        self.emit(&[0xC6, 0x20]); // ADD A, 0x20 (convert to lowercase)
        self.emit(&[0xD6, 0x61]); // SUB 'a'
        self.emit(&[0x87]);       // ADD A, A (multiply by 2)
        self.emit(&[0x5F]);       // LD E, A
        self.emit(&[0x16, 0x00]); // LD D, 0
        self.emit(&[0x21]);       // LD HL, SYMBOL_TABLE
        self.emit_word(SYMBOL_TABLE);
        self.emit(&[0x19]);       // ADD HL, DE
        self.emit(&[0x44]);       // LD B, H
        self.emit(&[0x4D]);       // LD C, L (BC = variable address)

        // Restore HL (input pointer) and save BC (variable address)
        self.emit(&[0xE1]);       // POP HL (restore input pointer)
        self.emit(&[0xC5]);       // PUSH BC (save variable address)

        // Parse expression
        self.emit(&[0xCD]);       // CALL parse_expr
        self.fixup("parse_expr");

        // Emit STORE with address from BC
        self.emit(&[0xC1]);       // POP BC (restore variable address)
        self.emit(&[0xDD, 0x36, 0, op::STORE]); // LD (IX+0), STORE
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xDD, 0x71, 0]); // LD (IX+0), C
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xDD, 0x70, 0]); // LD (IX+0), B
        self.emit(&[0xDD, 0x23]); // INC IX

        // Check for semicolon - if found, emit POP_I and continue with more statements
        self.emit(&[0xCD]);       // CALL lex_next
        self.fixup("lex_next");
        self.emit(&[0xFE, tok::SEMI]); // CP SEMI
        self.emit(&[0x20, 0x09]); // JR NZ, +9 (not semi, go to finalize)
        // It's a semicolon - emit POP_I to discard store result, parse more
        self.emit(&[0xDD, 0x36, 0, op::POP_I]); // LD (IX+0), POP_I
        self.emit(&[0xDD, 0x23]); // INC IX
        self.emit(&[0xC3]);       // JP repl_stmt_dispatch (loop back for more)
        self.fixup("repl_stmt_dispatch");

        // Not a semicolon - finalize assignment (no output for standalone assignment)
        self.emit(&[0xDD, 0x36, 0, op::END]); // LD (IX+0), END

        // Execute bytecode
        self.emit(&[0xCD]);       // CALL interpret
        self.fixup("interpret");

        // Loop (no output for assignment)
        self.emit(&[0xC3]);       // JP repl_loop
        self.fixup("repl_loop");

        // Handle function definition: fun f x = body
        // Syntax: fun <func_name> <param_name> = <body>
        // FUNC_TABLE entry: [param_idx, defined_flag, body_lo, body_hi]
        // Use SCRATCH+1 for func_idx, SCRATCH+2 for param_idx
        self.label("repl_fun");
        self.emit(&[0xF1]);       // POP AF (discard saved input pointer)
        // HL now points past "fun"

        // Get function name (must be single letter a-z)
        self.emit(&[0xCD]);       // CALL lex_next
        self.fixup("lex_next");
        self.emit(&[0xFE, tok::IDENT]);
        self.emit(&[0xC2]);       // JP NZ, repl_loop (error: expected identifier)
        self.fixup("repl_loop");
        // Function name char is in SCRATCH, convert to index and save
        self.emit(&[0x3A]);       // LD A, (SCRATCH)
        self.emit_word(SCRATCH);
        self.emit(&[0xFE, 0x5B]); // CP 'Z'+1
        self.emit(&[0x30, 0x02]); // JR NC, +2
        self.emit(&[0xC6, 0x20]); // ADD A, 0x20 (to lowercase)
        self.emit(&[0xD6, 0x61]); // SUB 'a' (A = 0-25)
        self.emit(&[0x32]);       // LD (SCRATCH+1), A (save func_idx)
        self.emit_word(SCRATCH + 1);

        // Get parameter name
        self.emit(&[0xCD]);       // CALL lex_next
        self.fixup("lex_next");
        self.emit(&[0xFE, tok::IDENT]);
        self.emit(&[0xC2]);       // JP NZ, repl_loop (error)
        self.fixup("repl_loop");
        self.emit(&[0x3A]);       // LD A, (SCRATCH)
        self.emit_word(SCRATCH);
        self.emit(&[0xFE, 0x5B]); // CP 'Z'+1
        self.emit(&[0x30, 0x02]); // JR NC, +2
        self.emit(&[0xC6, 0x20]); // ADD A, 0x20
        self.emit(&[0xD6, 0x61]); // SUB 'a'
        self.emit(&[0x32]);       // LD (SCRATCH+2), A (save param_idx)
        self.emit_word(SCRATCH + 2);

        // Expect '='
        self.emit(&[0xCD]);       // CALL lex_next
        self.fixup("lex_next");
        self.emit(&[0xFE, tok::EQ]);
        self.emit(&[0xC2]);       // JP NZ, repl_loop (error)
        self.fixup("repl_loop");

        // Now HL = input pointer past '='
        // Save HL, set up IX for function code parsing
        self.emit(&[0xE5]);       // PUSH HL (save input pointer)
        self.emit(&[0x2A]);       // LD HL, (FUNC_CODE_PTR)
        self.emit_word(FUNC_CODE_PTR);
        self.emit(&[0xE5]);       // PUSH HL (save body_start)
        self.emit(&[0xDD, 0xE1]); // POP IX (IX = body_start for code gen)
        self.emit(&[0xE1]);       // POP HL (restore input pointer)

        // Parse body expression (emit to function code area)
        self.emit(&[0xCD]);       // CALL parse_expr
        self.fixup("parse_expr");

        // Emit FRET at end of function body
        self.emit(&[0xDD, 0x36, 0, op::FRET]); // LD (IX+0), FRET
        self.emit(&[0xDD, 0x23]); // INC IX

        // Save body_start from FUNC_CODE_PTR (before updating)
        self.emit(&[0x2A]);       // LD HL, (FUNC_CODE_PTR)
        self.emit_word(FUNC_CODE_PTR);
        self.emit(&[0xD5]);       // PUSH DE (save DE)
        self.emit(&[0xEB]);       // EX DE, HL (DE = body_start)

        // Update FUNC_CODE_PTR to new position (end of body = IX)
        self.emit(&[0xDD, 0xE5]); // PUSH IX
        self.emit(&[0xE1]);       // POP HL
        self.emit(&[0x22]);       // LD (FUNC_CODE_PTR), HL
        self.emit_word(FUNC_CODE_PTR);

        // Calculate function table entry address
        // FUNC_TABLE + func_idx * 4
        self.emit(&[0x3A]);       // LD A, (SCRATCH+1) (func_idx)
        self.emit_word(SCRATCH + 1);
        self.emit(&[0x87]);       // ADD A, A (x2)
        self.emit(&[0x87]);       // ADD A, A (x4)
        self.emit(&[0x6F]);       // LD L, A
        self.emit(&[0x26, 0x00]); // LD H, 0
        self.emit(&[0x01]);       // LD BC, FUNC_TABLE
        self.emit_word(FUNC_TABLE);
        self.emit(&[0x09]);       // ADD HL, BC (HL = &table[func_idx])

        // Store entry: [param_idx, 1 (defined), body_lo, body_hi]
        self.emit(&[0x3A]);       // LD A, (SCRATCH+2) (param_idx)
        self.emit_word(SCRATCH + 2);
        self.emit(&[0x77]);       // LD (HL), A (param_idx)
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0x36, 0x01]); // LD (HL), 1 (defined flag)
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0x73]);       // LD (HL), E (body_lo)
        self.emit(&[0x23]);       // INC HL
        self.emit(&[0x72]);       // LD (HL), D (body_hi)

        // Restore DE and loop back
        self.emit(&[0xD1]);       // POP DE
        self.emit(&[0xC3]);       // JP repl_loop
        self.fixup("repl_loop");
    }

    fn emit_strings(&mut self) {
        // Prompt string
        self.label("prompt_str");
        for b in b"> " {
            self.emit_byte(*b);
        }
        self.emit_byte(0);
    }
}

/// Generate a standalone REPL binary for Z80
pub fn generate_repl() -> Vec<u8> {
    let mut gen = ReplGenerator::new();
    gen.generate()
}

#[cfg(test)]
mod tests {
    use super::*;

    // Helper to find a byte sequence in the binary
    fn find_sequence(binary: &[u8], pattern: &[u8]) -> Option<usize> {
        binary.windows(pattern.len()).position(|w| w == pattern)
    }

    // Helper to check if a sequence exists
    fn has_sequence(binary: &[u8], pattern: &[u8]) -> bool {
        find_sequence(binary, pattern).is_some()
    }

    #[test]
    fn test_repl_generation() {
        let binary = generate_repl();
        assert_eq!(binary.len(), 0x2000); // 8KB
        // Check entry point starts with LD SP
        assert_eq!(binary[0], 0x31);
    }

    #[test]
    fn test_memory_layout_constants() {
        // Verify memory regions don't overlap
        assert!(BYTECODE_BUF < SYMBOL_TABLE);
        assert!(SYMBOL_TABLE < FUNC_TABLE);
        assert!(FUNC_TABLE < FUNC_CODE);
        assert!(FUNC_CODE < FUNC_CODE_PTR);
        assert!(FUNC_CODE_PTR < INPUT_BUF);
        assert!(INPUT_BUF < EVAL_STACK);
        assert!(EVAL_STACK < SCRATCH);
        assert!(SCRATCH < CPU_STACK);
    }

    #[test]
    fn test_stack_pointer_init() {
        let binary = generate_repl();
        // Entry point: LD SP, CPU_STACK (0x3FFF)
        // LD SP, nn = 0x31 nn_lo nn_hi
        assert_eq!(binary[0], 0x31);
        assert_eq!(binary[1], (CPU_STACK & 0xFF) as u8);
        assert_eq!(binary[2], (CPU_STACK >> 8) as u8);
    }

    #[test]
    fn test_eval_stack_init() {
        let binary = generate_repl();
        // LD IY, EVAL_STACK = FD 21 lo hi
        let pattern = [0xFD, 0x21, (EVAL_STACK & 0xFF) as u8, (EVAL_STACK >> 8) as u8];
        assert!(has_sequence(&binary, &pattern), "IY initialization not found");
    }

    #[test]
    fn test_func_code_ptr_init() {
        let binary = generate_repl();
        // Should have: LD HL, FUNC_CODE followed by LD (FUNC_CODE_PTR), HL
        // LD HL, nn = 21 lo hi
        let ld_hl = [0x21, (FUNC_CODE & 0xFF) as u8, (FUNC_CODE >> 8) as u8];
        assert!(has_sequence(&binary, &ld_hl), "LD HL, FUNC_CODE not found");

        // LD (nn), HL = 22 lo hi
        let ld_indirect = [0x22, (FUNC_CODE_PTR & 0xFF) as u8, (FUNC_CODE_PTR >> 8) as u8];
        assert!(has_sequence(&binary, &ld_indirect), "LD (FUNC_CODE_PTR), HL not found");
    }

    #[test]
    fn test_opcode_values() {
        // Verify opcodes are unique and within expected range
        let opcodes = [
            op::NOP, op::PUSH_I16, op::PUSH_DEC, op::DUP, op::DROP, op::SWAP,
            op::ADD_I, op::SUB_I, op::MUL_I, op::DIV_I, op::MOD_I, op::NEG_I,
            op::BAND_I, op::BOR_I, op::BXOR_I, op::BNOT_I, op::SHL_I, op::SHR_I,
            op::LAND_I, op::LOR_I, op::LNOT_I,
            op::EQ_I, op::NE_I, op::LT_I, op::LE_I, op::GT_I, op::GE_I,
            op::JMP, op::JZ, op::JNZ,
            op::LOAD, op::STORE,
            op::PRINT_I, op::PRINT_D, op::PRINT_NL, op::POP_I, op::FCALL, op::FRET,
            op::I2D, op::D2I,
            op::HALT, op::END,
        ];

        // Check no duplicates
        let mut sorted = opcodes.to_vec();
        sorted.sort();
        sorted.dedup();
        assert_eq!(sorted.len(), opcodes.len(), "Duplicate opcode values detected");
    }

    #[test]
    fn test_token_values() {
        // Verify token types are unique
        let tokens = [
            tok::EOF, tok::INT, tok::DEC, tok::IDENT,
            tok::PLUS, tok::MINUS, tok::STAR, tok::SLASH,
            tok::LPAREN, tok::RPAREN, tok::EQ, tok::LET,
            tok::EQEQ, tok::NE, tok::LT, tok::LE, tok::GT, tok::GE, tok::MOD,
            tok::BAND, tok::BOR, tok::BXOR, tok::BNOT, tok::SHL, tok::SHR,
            tok::AND_KW, tok::OR_KW, tok::NOT_KW,
            tok::IF_KW, tok::THEN_KW, tok::ELSE_KW,
            tok::NEWLINE, tok::ASSIGN, tok::SEMI,
            tok::WHILE_KW, tok::DO_KW, tok::FUN_KW,
        ];

        let mut sorted = tokens.to_vec();
        sorted.sort();
        sorted.dedup();
        assert_eq!(sorted.len(), tokens.len(), "Duplicate token values detected");
    }

    #[test]
    fn test_binary_has_ret_instruction() {
        let binary = generate_repl();
        // Should have RET (0xC9) instructions for subroutines
        assert!(binary.contains(&0xC9), "No RET instructions found");
    }

    #[test]
    fn test_binary_has_halt() {
        let binary = generate_repl();
        // Should have HALT (0x76) for unknown opcodes
        assert!(binary.contains(&0x76), "No HALT instruction found");
    }

    #[test]
    fn test_interpreter_dispatch_opcodes() {
        let binary = generate_repl();

        // Check that key opcodes are referenced in the dispatch table
        // CP op followed by JP Z = FE op CA lo hi

        // PUSH_I16 dispatch
        let push_dispatch = [0xFE, op::PUSH_I16, 0xCA];
        assert!(has_sequence(&binary, &push_dispatch), "PUSH_I16 dispatch not found");

        // ADD_I dispatch
        let add_dispatch = [0xFE, op::ADD_I, 0xCA];
        assert!(has_sequence(&binary, &add_dispatch), "ADD_I dispatch not found");

        // PRINT_I dispatch
        let print_dispatch = [0xFE, op::PRINT_I, 0xCA];
        assert!(has_sequence(&binary, &print_dispatch), "PRINT_I dispatch not found");

        // END dispatch (RET Z = C8)
        let end_dispatch = [0xFE, op::END, 0xC8];
        assert!(has_sequence(&binary, &end_dispatch), "END dispatch not found");
    }

    #[test]
    fn test_lexer_token_dispatch() {
        let binary = generate_repl();

        // Lexer should check for various characters
        // CP '+' (0x2B)
        assert!(has_sequence(&binary, &[0xFE, 0x2B]), "Lexer '+' check not found");
        // CP '-' (0x2D)
        assert!(has_sequence(&binary, &[0xFE, 0x2D]), "Lexer '-' check not found");
        // CP '*' (0x2A)
        assert!(has_sequence(&binary, &[0xFE, 0x2A]), "Lexer '*' check not found");
        // CP '(' (0x28)
        assert!(has_sequence(&binary, &[0xFE, 0x28]), "Lexer '(' check not found");
        // CP ')' (0x29)
        assert!(has_sequence(&binary, &[0xFE, 0x29]), "Lexer ')' check not found");
    }

    #[test]
    fn test_io_instructions_present() {
        let binary = generate_repl();

        // OUT (0x00), A = D3 00 (for character output)
        assert!(has_sequence(&binary, &[0xD3, 0x00]), "OUT instruction not found");

        // IN A, (0x00) = DB 00 (for character input)
        assert!(has_sequence(&binary, &[0xDB, 0x00]), "IN instruction not found");
    }

    #[test]
    fn test_mul_div_routines_present() {
        let binary = generate_repl();

        // Multiplication uses ADD HL, HL (0x29) repeatedly
        // Should have multiple ADD HL, HL for multiply by 10, 16, etc.
        let add_hl_count = binary.iter().filter(|&&b| b == 0x29).count();
        assert!(add_hl_count > 5, "Not enough ADD HL,HL for mul routines");

        // Division uses SRL, RR, SBC instructions
        // SRL B = CB 38
        assert!(has_sequence(&binary, &[0xCB, 0x38]), "SRL B not found (mul16)");
    }

    #[test]
    fn test_fcall_fret_opcodes() {
        let binary = generate_repl();

        // FCALL dispatch
        let fcall_dispatch = [0xFE, op::FCALL, 0xCA];
        assert!(has_sequence(&binary, &fcall_dispatch), "FCALL dispatch not found");

        // FRET dispatch
        let fret_dispatch = [0xFE, op::FRET, 0xCA];
        assert!(has_sequence(&binary, &fret_dispatch), "FRET dispatch not found");
    }

    #[test]
    fn test_comparison_opcodes() {
        let binary = generate_repl();

        // Should have dispatches for all comparison ops
        assert!(has_sequence(&binary, &[0xFE, op::EQ_I, 0xCA]), "EQ_I dispatch not found");
        assert!(has_sequence(&binary, &[0xFE, op::NE_I, 0xCA]), "NE_I dispatch not found");
        assert!(has_sequence(&binary, &[0xFE, op::LT_I, 0xCA]), "LT_I dispatch not found");
        assert!(has_sequence(&binary, &[0xFE, op::LE_I, 0xCA]), "LE_I dispatch not found");
        assert!(has_sequence(&binary, &[0xFE, op::GT_I, 0xCA]), "GT_I dispatch not found");
        assert!(has_sequence(&binary, &[0xFE, op::GE_I, 0xCA]), "GE_I dispatch not found");
    }

    #[test]
    fn test_bitwise_opcodes() {
        let binary = generate_repl();

        assert!(has_sequence(&binary, &[0xFE, op::BAND_I, 0xCA]), "BAND_I dispatch not found");
        assert!(has_sequence(&binary, &[0xFE, op::BOR_I, 0xCA]), "BOR_I dispatch not found");
        assert!(has_sequence(&binary, &[0xFE, op::BXOR_I, 0xCA]), "BXOR_I dispatch not found");
        assert!(has_sequence(&binary, &[0xFE, op::BNOT_I, 0xCA]), "BNOT_I dispatch not found");
        assert!(has_sequence(&binary, &[0xFE, op::SHL_I, 0xCA]), "SHL_I dispatch not found");
        assert!(has_sequence(&binary, &[0xFE, op::SHR_I, 0xCA]), "SHR_I dispatch not found");
    }

    #[test]
    fn test_logical_opcodes() {
        let binary = generate_repl();

        assert!(has_sequence(&binary, &[0xFE, op::LAND_I, 0xCA]), "LAND_I dispatch not found");
        assert!(has_sequence(&binary, &[0xFE, op::LOR_I, 0xCA]), "LOR_I dispatch not found");
        assert!(has_sequence(&binary, &[0xFE, op::LNOT_I, 0xCA]), "LNOT_I dispatch not found");
    }

    #[test]
    fn test_jump_opcodes() {
        let binary = generate_repl();

        assert!(has_sequence(&binary, &[0xFE, op::JMP, 0xCA]), "JMP dispatch not found");
        assert!(has_sequence(&binary, &[0xFE, op::JZ, 0xCA]), "JZ dispatch not found");
    }

    #[test]
    fn test_prompt_string() {
        let binary = generate_repl();

        // Prompt is "> " followed by null terminator
        assert!(has_sequence(&binary, &[0x3E, 0x20, 0x00]), "Prompt string '> \\0' not found");
    }

    #[test]
    fn test_no_uninitialized_memory_in_code() {
        let binary = generate_repl();

        // The code section should not be all zeros
        // Check first 1KB has meaningful code
        let first_kb = &binary[0..1024];
        let non_zero_count = first_kb.iter().filter(|&&b| b != 0).count();
        assert!(non_zero_count > 500, "First 1KB seems mostly empty");
    }

    #[test]
    fn test_symbol_table_not_in_code() {
        // SYMBOL_TABLE address should not appear as code
        // (it's a data address, not a jump target in the first few bytes)
        let binary = generate_repl();

        // The binary is 8KB, SYMBOL_TABLE is at 0x2800 which is outside
        assert!(SYMBOL_TABLE >= binary.len() as u16, "SYMBOL_TABLE should be outside binary");
    }
}
