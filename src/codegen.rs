//! Z80 code generator for Tiny ML
//!
//! Generates Z80 machine code from a typed AST, producing a ROM image
//! that runs on Z80-based retrocomputing hardware.
//!
//! # Architecture
//!
//! - **Evaluation model**: Stack-based with IX as frame pointer
//! - **Integers**: 16-bit signed (HL register)
//! - **Decimals**: 4-byte packed BCD in memory
//! - **Functions**: Closure calling convention with captured variables
//! - **TCO**: Tail-call optimization for recursive functions
//!
//! # Memory Layout
//!
//! - `0x0000-0x1FFF`: ROM (8KB) - code and runtime routines
//! - `0x3000-0x30FF`: Data section (BCD constants, strings)
//! - `0x3100-0x36FF`: Global variables and heap
//! - `0x3700-0x37FF`: Stack (grows downward)
//!
//! # Usage
//!
//! ```ignore
//! let mut codegen = CodeGenerator::new();
//! let binary = codegen.generate(&program)?;
//! ```

use crate::ast::*;
use crate::types::{TypeInference, TypeEnv};
use std::collections::HashMap;

// Memory layout constants
const ROM_START: u16 = 0x0000;
const ROM_SIZE: usize = 0x2000;  // 8KB ROM
const STACK_TOP: u16 = 0x37FF;
const RUNTIME_BASE: u16 = 0x0100;
const DATA_BASE: u16 = 0x3000;

/// Z80 code generator for Tiny ML programs.
///
/// Compiles a typed AST into Z80 machine code. The generator handles:
/// - Expression evaluation (arithmetic, comparisons, function calls)
/// - Control flow (if/else, match expressions)
/// - Function definitions with closures
/// - Runtime library for I/O and BCD arithmetic
/// - Tail-call optimization for recursive functions
pub struct CodeGenerator {
    code: Vec<u8>,
    origin: u16,
    labels: HashMap<String, u16>,
    fixups: Vec<(usize, String)>,  // (offset, label_name)
    next_label: usize,

    // Variable environment: name -> stack offset from frame pointer (IX)
    // Positive offsets = function parameters (above IX)
    // Negative offsets = local let bindings (below IX)
    locals: HashMap<String, i16>,
    param_offset: i16,   // Next param offset (grows positive)
    local_offset: i16,   // Next local offset (grows negative)

    // Global value storage: name -> RAM address
    globals: HashMap<String, u16>,
    global_offset: u16,  // Current offset for global allocation

    // Function labels: name -> label for getting function addresses
    functions: HashMap<String, String>,

    // Deferred lambda bodies: (label, params, body, captures) to emit after main code
    // captures is a list of variable names that were captured from the enclosing scope
    deferred_lambdas: Vec<(String, Vec<Param>, Box<Expr>, Vec<String>)>,

    // Type information from inference
    type_env: TypeEnv,
    type_inference: TypeInference,

    // Data section for constants (BCD values, etc.)
    data: Vec<u8>,
    data_offset: u16,  // Current offset within data section

    // Runtime addresses
    rt_print_int: u16,
    rt_print_decimal: u16,
    rt_print_string: u16,
    rt_print_newline: u16,
    rt_bcd_add: u16,
    rt_bcd_sub: u16,
    rt_bcd_mul: u16,
    rt_bcd_div: u16,
    rt_int_to_bcd: u16,
    rt_bcd_to_int: u16,
    rt_bcd_cmp: u16,

    // Tail-call optimization support
    current_fn_name: Option<String>,        // Name of function currently being generated
    current_fn_body_label: Option<String>,  // Label after prologue for tail jumps
    current_fn_param_count: usize,          // Number of parameters
}

impl CodeGenerator {
    pub fn new() -> Self {
        CodeGenerator {
            code: Vec::new(),
            origin: ROM_START,
            labels: HashMap::new(),
            fixups: Vec::new(),
            next_label: 0,
            locals: HashMap::new(),
            param_offset: 4,   // First param at IX+4 (after saved IX and return addr)
            local_offset: -2,  // First local at IX-2 (below frame pointer)
            globals: HashMap::new(),
            global_offset: 0x3100,  // Start globals after data section (0x3000-0x30FF for data)
            functions: HashMap::new(),
            deferred_lambdas: Vec::new(),
            type_env: TypeEnv::new(),
            type_inference: TypeInference::new(),
            data: Vec::new(),
            data_offset: 0,
            rt_print_int: 0,
            rt_print_decimal: 0,
            rt_print_string: 0,
            rt_print_newline: 0,
            rt_bcd_add: 0,
            rt_bcd_sub: 0,
            rt_bcd_mul: 0,
            rt_bcd_div: 0,
            rt_int_to_bcd: 0,
            rt_bcd_to_int: 0,
            rt_bcd_cmp: 0,
            current_fn_name: None,
            current_fn_body_label: None,
            current_fn_param_count: 0,
        }
    }

    /// Allocate a BCD constant in the data section and return its address
    fn alloc_bcd_constant(&mut self, bcd: [u8; 4]) -> u16 {
        let addr = DATA_BASE + self.data_offset;
        self.data.extend_from_slice(&bcd);
        self.data_offset += 4;
        addr
    }

    /// Allocate a string constant in the data section and return its address
    fn alloc_string_constant(&mut self, s: &str) -> u16 {
        let addr = DATA_BASE + self.data_offset;
        // Store as null-terminated string
        self.data.extend_from_slice(s.as_bytes());
        self.data.push(0);  // null terminator
        self.data_offset += s.len() as u16 + 1;
        addr
    }

    /// Parse a decimal string and convert to 4-byte packed BCD
    /// Uses fixed-point with 2 decimal places: "3.14" -> 314 stored as BCD
    fn parse_decimal_to_bcd(&self, s: &str) -> [u8; 4] {
        // Split on decimal point
        let parts: Vec<&str> = s.split('.').collect();
        let int_part: u32 = parts[0].parse().unwrap_or(0);
        let frac_part: u32 = if parts.len() > 1 {
            // Take first 2 digits of fractional part, pad with zeros if needed
            let frac_str = parts[1];
            let padded = format!("{:0<2}", &frac_str[..frac_str.len().min(2)]);
            padded.parse().unwrap_or(0)
        } else {
            0
        };

        // Combine: value = int_part * 100 + frac_part
        let value = int_part * 100 + frac_part;

        // Convert to 8-digit BCD (4 bytes)
        self.int_to_bcd(value)
    }

    /// Convert an integer to 4-byte packed BCD
    fn int_to_bcd(&self, mut value: u32) -> [u8; 4] {
        let mut bcd = [0u8; 4];

        // Extract digits from right to left (LSB first)
        // byte3 (LSB): 10s and 1s
        bcd[3] = ((value % 10) | ((value / 10 % 10) << 4)) as u8;
        value /= 100;

        // byte2: 1000s and 100s
        bcd[2] = ((value % 10) | ((value / 10 % 10) << 4)) as u8;
        value /= 100;

        // byte1: 100000s and 10000s
        bcd[1] = ((value % 10) | ((value / 10 % 10) << 4)) as u8;
        value /= 100;

        // byte0: highest digits (10000000s and 1000000s) - usually 0
        bcd[0] = ((value % 10) | ((value / 10 % 10) << 4)) as u8;

        bcd
    }

    /// Get the type of an expression (simplified type inference)
    fn get_expr_type(&self, expr: &Expr) -> crate::types::Type {
        use crate::types::Type;
        match expr {
            Expr::Int(_) => Type::Int,
            Expr::Decimal(_) => Type::Decimal,
            Expr::Bool(_) => Type::Bool,
            Expr::Var(name) => {
                self.type_env.get(name).cloned().unwrap_or(Type::Int)
            }
            Expr::BinOp(op, left, right) => {
                let lt = self.get_expr_type(left);
                let rt = self.get_expr_type(right);
                match op {
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => {
                        // If either is decimal, result is decimal
                        if matches!(lt, Type::Decimal) || matches!(rt, Type::Decimal) {
                            Type::Decimal
                        } else {
                            Type::Int
                        }
                    }
                    _ => Type::Bool, // Comparisons return bool
                }
            }
            Expr::UnaryOp(_, inner) => self.get_expr_type(inner),
            Expr::If(_, then_branch, _) => self.get_expr_type(then_branch),
            Expr::Let { body, .. } => self.get_expr_type(body),
            Expr::App(func, _) => {
                // For function calls, we'd need full type inference
                // For now, assume int unless we know better
                if let Expr::Var(name) = func.as_ref() {
                    self.type_env.get(name).cloned().unwrap_or(Type::Int)
                } else {
                    Type::Int
                }
            }
            Expr::ToInt(_) => Type::Int,
            Expr::ToDecimal(_) => Type::Decimal,
            _ => Type::Int, // Default to int for other cases
        }
    }

    /// Collect all arguments from a curried application chain.
    /// `f a b c` is represented as `App(App(App(f, a), b), c)`.
    /// Returns (function_name, [arg1, arg2, arg3, ...]) with args in application order.
    #[allow(dead_code)]
    fn collect_app_args<'a>(&self, expr: &'a Expr) -> (Option<&'a str>, Vec<&'a Expr>) {
        let mut args = Vec::new();
        let mut current = expr;

        while let Expr::App(func, arg) = current {
            args.push(arg.as_ref());
            current = func.as_ref();
        }

        args.reverse(); // Collected in reverse order

        if let Expr::Var(name) = current {
            (Some(name.as_str()), args)
        } else {
            (None, args)
        }
    }

    /// Collect all arguments from a curried application chain, returning the function expression.
    /// Returns (function_expr, [arg1, arg2, arg3, ...]) with args in application order.
    fn collect_app_args_with_expr<'a>(&self, expr: &'a Expr) -> (&'a Expr, Vec<&'a Expr>) {
        let mut args = Vec::new();
        let mut current = expr;

        while let Expr::App(func, arg) = current {
            args.push(arg.as_ref());
            current = func.as_ref();
        }

        args.reverse(); // Collected in reverse order
        (current, args)
    }

    fn emit(&mut self, bytes: &[u8]) {
        self.code.extend_from_slice(bytes);
    }

    fn emit_byte(&mut self, b: u8) {
        self.code.push(b);
    }

    fn emit_word(&mut self, w: u16) {
        self.code.push((w & 0xFF) as u8);
        self.code.push((w >> 8) as u8);
    }

    fn current_addr(&self) -> u16 {
        self.origin + self.code.len() as u16
    }

    fn gen_label(&mut self) -> String {
        let label = format!("L{}", self.next_label);
        self.next_label += 1;
        label
    }

    fn define_label(&mut self, name: &str) {
        self.labels.insert(name.to_string(), self.current_addr());
    }

    fn emit_call(&mut self, label: &str) {
        self.emit_byte(0xCD);  // CALL
        self.fixups.push((self.code.len(), label.to_string()));
        self.emit_word(0x0000);  // Placeholder
    }

    fn emit_jp(&mut self, label: &str) {
        self.emit_byte(0xC3);  // JP
        self.fixups.push((self.code.len(), label.to_string()));
        self.emit_word(0x0000);
    }

    fn emit_jp_z(&mut self, label: &str) {
        self.emit_byte(0xCA);  // JP Z
        self.fixups.push((self.code.len(), label.to_string()));
        self.emit_word(0x0000);
    }

    fn emit_jp_nz(&mut self, label: &str) {
        self.emit_byte(0xC2);  // JP NZ
        self.fixups.push((self.code.len(), label.to_string()));
        self.emit_word(0x0000);
    }

    fn emit_jp_m(&mut self, label: &str) {
        self.emit_byte(0xFA);  // JP M (sign flag set, negative)
        self.fixups.push((self.code.len(), label.to_string()));
        self.emit_word(0x0000);
    }

    fn emit_jp_p(&mut self, label: &str) {
        self.emit_byte(0xF2);  // JP P (sign flag clear, positive/zero)
        self.fixups.push((self.code.len(), label.to_string()));
        self.emit_word(0x0000);
    }

    fn emit_jr(&mut self, label: &str) {
        self.emit_byte(0x18);  // JR
        self.fixups.push((self.code.len(), label.to_string()));
        self.emit_byte(0x00);  // Placeholder (relative)
    }

    fn emit_jr_z(&mut self, label: &str) {
        self.emit_byte(0x28);  // JR Z
        self.fixups.push((self.code.len(), label.to_string()));
        self.emit_byte(0x00);
    }

    fn emit_jr_nz(&mut self, label: &str) {
        self.emit_byte(0x20);  // JR NZ
        self.fixups.push((self.code.len(), label.to_string()));
        self.emit_byte(0x00);
    }

    fn resolve_fixups(&mut self) {
        for (offset, label) in &self.fixups {
            if let Some(&addr) = self.labels.get(label) {
                // Check if this is a relative jump (1-byte offset)
                let opcode = self.code[offset - 1];
                // JR (0x18, 0x20, 0x28, 0x30, 0x38) and DJNZ (0x10) are relative jumps
                if opcode == 0x10 || opcode == 0x18 || opcode == 0x20 || opcode == 0x28 || opcode == 0x30 || opcode == 0x38 {
                    // Relative jump
                    let from = self.origin as i32 + *offset as i32 + 1;
                    let to = addr as i32;
                    let rel = to - from;
                    if rel < -128 || rel > 127 {
                        panic!("Relative jump out of range: {} -> {}", from, to);
                    }
                    self.code[*offset] = rel as i8 as u8;
                } else {
                    // Absolute address
                    self.code[*offset] = (addr & 0xFF) as u8;
                    self.code[*offset + 1] = (addr >> 8) as u8;
                }
            } else {
                panic!("Undefined label: {}", label);
            }
        }
    }

    /// Generate code for the entire program
    pub fn generate(&mut self, program: &Program) -> Result<Vec<u8>, String> {
        // Type check first
        self.type_env = self.type_inference.check_program(program)
            .map_err(|e| e.message)?;

        // Emit header and initialization
        self.emit_header();

        // Emit runtime library
        self.emit_runtime();

        // Define entry point
        self.define_label("main");

        // Initialize stack pointer
        self.emit(&[0x31]);  // LD SP, imm16
        self.emit_word(STACK_TOP);

        // Initialize closure heap pointer at 0x3700 to point to 0x3710
        // (First 16 bytes reserved for heap pointer itself and metadata)
        self.emit(&[0x21]);  // LD HL, 0x3710
        self.emit_word(0x3710);
        self.emit(&[0x22]);  // LD (0x3700), HL
        self.emit_word(0x3700);

        // Set up IX as frame pointer for top-level code
        // This allows match expressions with variable bindings to work
        self.emit(&[0xDD, 0x21]);    // LD IX, 0
        self.emit_word(0);
        self.emit(&[0xDD, 0x39]);    // ADD IX, SP (IX = SP)

        // Call data initialization (will be a no-op if no data)
        self.emit_call("data_init");

        // Separate functions from value bindings
        let mut functions = Vec::new();
        let mut values = Vec::new();
        for binding in &program.bindings {
            if binding.params.is_empty() {
                values.push(binding);
            } else {
                functions.push(binding);
            }
        }

        // Pre-register all function labels so they can be referenced before definition
        for binding in &functions {
            let fn_label = format!("fn_{}", binding.name);
            self.functions.insert(binding.name.clone(), fn_label);
        }

        // Generate code for value bindings first (these form the main program)
        for binding in &values {
            self.gen_binding(binding)?;
        }

        // Halt at end of main program
        self.emit(&[0x76]);  // HALT

        // Now generate function code (after HALT, these are only reached via CALL)
        for binding in &functions {
            self.gen_binding(binding)?;
        }

        // Emit deferred lambda bodies (lambdas stored as values)
        self.emit_deferred_lambdas()?;

        // Emit data initialization routine
        self.emit_data_init_routine();

        // Resolve label fixups
        self.resolve_fixups();

        // Append data section to code (will be in ROM, copied to RAM at init)
        let data_rom_addr = self.code.len() as u16 + self.origin;
        self.define_label("data_section");
        self.code.extend_from_slice(&self.data);

        // Patch data init routine with actual ROM address
        self.patch_data_init(data_rom_addr);

        // Pad to ROM size
        while self.code.len() < ROM_SIZE {
            self.code.push(0x00);
        }

        Ok(self.code.clone())
    }

    fn emit_data_init_routine(&mut self) {
        // This routine copies data from ROM to RAM at startup
        // Called automatically before user code
        // HL = source (ROM), DE = dest (RAM), BC = count
        self.define_label("data_init");

        // Skip LDIR if no data (emulator bug with LDIR when BC=0)
        if self.data.is_empty() {
            self.emit(&[0xC9]);   // RET immediately if no data
            return;
        }

        self.emit(&[0x21]);       // LD HL, data_rom_addr (will be patched)
        self.emit_word(0x0000);   // Placeholder for ROM source address
        self.emit(&[0x11]);       // LD DE, DATA_BASE
        self.emit_word(DATA_BASE);
        self.emit(&[0x01]);       // LD BC, data_size
        self.emit_word(self.data.len() as u16);
        // LDIR: copy BC bytes from (HL) to (DE)
        self.emit(&[0xED, 0xB0]); // LDIR
        self.emit(&[0xC9]);       // RET
    }

    fn patch_data_init(&mut self, rom_addr: u16) {
        // Find and patch the data init routine's source address
        // Skip patching if there's no data (routine is just RET)
        if self.data.is_empty() {
            return;
        }
        if let Some(&init_addr) = self.labels.get("data_init") {
            let patch_offset = (init_addr - self.origin) as usize + 1; // +1 to skip LD HL opcode
            self.code[patch_offset] = (rom_addr & 0xFF) as u8;
            self.code[patch_offset + 1] = ((rom_addr >> 8) & 0xFF) as u8;
        }
    }

    fn emit_header(&mut self) {
        // Jump to main
        self.emit_byte(0xC3);  // JP main
        self.fixups.push((self.code.len(), "main".to_string()));
        self.emit_word(0x0000);
    }

    fn emit_runtime(&mut self) {
        // Set runtime base
        while self.code.len() < RUNTIME_BASE as usize {
            self.code.push(0x00);
        }

        // Print integer (HL = value)
        self.define_label("rt_print_int");
        self.rt_print_int = self.current_addr();
        self.emit_print_int_routine();

        // Print newline
        self.define_label("rt_print_newline");
        self.rt_print_newline = self.current_addr();
        self.emit_print_newline_routine();

        // BCD routines
        self.define_label("rt_bcd_add");
        self.rt_bcd_add = self.current_addr();
        self.emit_bcd_add_routine();

        self.define_label("rt_bcd_sub");
        self.rt_bcd_sub = self.current_addr();
        self.emit_bcd_sub_routine();

        self.define_label("rt_bcd_mul");
        self.rt_bcd_mul = self.current_addr();
        self.emit_bcd_mul_routine();

        self.define_label("rt_bcd_div");
        self.rt_bcd_div = self.current_addr();
        self.emit_bcd_div_routine();

        self.define_label("rt_int_to_bcd");
        self.rt_int_to_bcd = self.current_addr();
        self.emit_int_to_bcd_routine();

        self.define_label("rt_bcd_to_int");
        self.rt_bcd_to_int = self.current_addr();
        self.emit_bcd_to_int_routine();

        self.define_label("rt_bcd_cmp");
        self.rt_bcd_cmp = self.current_addr();
        self.emit_bcd_cmp_routine();

        self.define_label("rt_print_decimal");
        self.rt_print_decimal = self.current_addr();
        self.emit_print_decimal_routine();

        self.define_label("rt_print_string");
        self.rt_print_string = self.current_addr();
        self.emit_print_string_routine();

        // 16-bit integer arithmetic routines
        self.define_label("rt_mul16");
        self.emit_mul16_routine();

        self.define_label("rt_div16_full");
        self.emit_div16_full_routine();

        self.define_label("rt_mod16");
        self.emit_mod16_routine();

        self.define_label("rt_lt16");
        self.emit_lt16_routine();
    }

    fn emit_print_int_routine(&mut self) {
        // Print 16-bit signed integer in HL
        // Uses serial output via MC6850 ACIA at ports 0x80/0x81

        // Check for negative
        self.emit(&[0x7C]);        // LD A, H
        self.emit(&[0xB7]);        // OR A
        self.emit(&[0xF2]);        // JP P, positive
        let pos_label = self.gen_label();
        self.fixups.push((self.code.len(), pos_label.clone()));
        self.emit_word(0x0000);

        // Negative: print minus and negate
        self.emit(&[0x3E, 0x2D]);  // LD A, '-'
        self.emit(&[0xD3, 0x81]); // OUT (0x81), A

        // Negate HL: HL = 0 - HL
        self.emit(&[0xAF]);        // XOR A
        self.emit(&[0x95]);        // SUB L
        self.emit(&[0x6F]);        // LD L, A
        self.emit(&[0x9F]);        // SBC A, A
        self.emit(&[0x94]);        // SUB H
        self.emit(&[0x67]);        // LD H, A

        self.define_label(&pos_label);

        // Convert and print using repeated division by 10
        // Simple implementation: divide repeatedly, push digits, then print
        self.emit(&[0x01]);        // LD BC, 10
        self.emit_word(10);
        self.emit(&[0x11]);        // LD DE, 0 (digit count)
        self.emit_word(0);

        let div_loop = self.gen_label();
        self.define_label(&div_loop);

        // Divide HL by BC (10), remainder in A
        self.emit_call("rt_div16");
        self.emit(&[0xF5]);        // PUSH AF (save remainder/digit)
        self.emit(&[0x13]);        // INC DE (count digits)

        // Check if HL is zero
        self.emit(&[0x7C]);        // LD A, H
        self.emit(&[0xB5]);        // OR L
        self.emit_jr_nz(&div_loop);

        // Print digits (they're on stack in reverse order)
        let print_loop = self.gen_label();
        self.define_label(&print_loop);
        self.emit(&[0xF1]);        // POP AF
        self.emit(&[0xC6, 0x30]); // ADD A, '0'
        self.emit(&[0xD3, 0x81]); // OUT (0x81), A
        self.emit(&[0x1B]);        // DEC DE
        self.emit(&[0x7A]);        // LD A, D
        self.emit(&[0xB3]);        // OR E
        self.emit_jr_nz(&print_loop);

        self.emit(&[0xC9]);        // RET

        // Helper: 16-bit division HL / BC -> HL quotient, A remainder
        self.define_label("rt_div16");
        self.emit(&[0xAF]);        // XOR A (clear remainder)
        self.emit(&[0x06, 16]);    // LD B, 16 (bit count)
        let div16_loop = self.gen_label();
        self.define_label(&div16_loop);
        self.emit(&[0x29]);        // ADD HL, HL (shift left)
        self.emit(&[0x17]);        // RLA (shift into A)
        self.emit(&[0xFE, 10]);    // CP 10
        self.emit(&[0x38, 3]);     // JR C, +3 (skip SUB and INC if A < 10)
        self.emit(&[0xD6, 10]);    // SUB 10
        self.emit(&[0x2C]);        // INC L (set quotient bit)
        self.emit(&[0x10]);        // DJNZ
        let offset = div16_loop.clone();
        self.fixups.push((self.code.len(), offset));
        self.emit_byte(0x00);
        self.emit(&[0xC9]);        // RET
    }

    fn emit_print_newline_routine(&mut self) {
        self.emit(&[0x3E, 0x0D]);  // LD A, CR
        self.emit(&[0xD3, 0x81]); // OUT (0x81), A
        self.emit(&[0x3E, 0x0A]);  // LD A, LF
        self.emit(&[0xD3, 0x81]); // OUT (0x81), A
        self.emit(&[0xC9]);        // RET
    }

    fn emit_bcd_add_routine(&mut self) {
        // Add two 4-byte BCD numbers using DAA
        // DE points to first operand (result), HL points to second
        // Result stored at DE: (DE) = (DE) + (HL)
        // Must start from LSB (byte3) and work backwards to propagate carry

        // Move pointers to byte3 (LSB)
        self.emit(&[0x23]);        // INC HL
        self.emit(&[0x23]);        // INC HL
        self.emit(&[0x23]);        // INC HL
        self.emit(&[0x13]);        // INC DE
        self.emit(&[0x13]);        // INC DE
        self.emit(&[0x13]);        // INC DE

        self.emit(&[0x06, 4]);     // LD B, 4 (4 bytes)
        self.emit(&[0xB7]);        // OR A (clear carry)

        let loop_label = self.gen_label();
        let done_label = self.gen_label();
        self.define_label(&loop_label);
        self.emit(&[0x1A]);        // LD A, (DE)
        self.emit(&[0x8E]);        // ADC A, (HL)
        self.emit(&[0x27]);        // DAA
        self.emit(&[0x12]);        // LD (DE), A
        self.emit(&[0x2B]);        // DEC HL (move to next higher byte)
        self.emit(&[0x1B]);        // DEC DE
        // Preserve carry across loop
        self.emit(&[0xF5]);        // PUSH AF
        self.emit(&[0x05]);        // DEC B
        self.emit_jr_z(&done_label);
        self.emit(&[0xF1]);        // POP AF
        self.emit_jr(&loop_label);
        self.define_label(&done_label);
        self.emit(&[0xF1]);        // POP AF (clean stack)
        self.emit(&[0xC9]);        // RET
    }

    fn emit_bcd_sub_routine(&mut self) {
        // Subtract two 4-byte BCD numbers using DAA
        // (DE) = (DE) - (HL)
        // Must start from LSB (byte3) and work backwards to propagate borrow

        // Move pointers to byte3 (LSB)
        self.emit(&[0x23]);        // INC HL
        self.emit(&[0x23]);        // INC HL
        self.emit(&[0x23]);        // INC HL
        self.emit(&[0x13]);        // INC DE
        self.emit(&[0x13]);        // INC DE
        self.emit(&[0x13]);        // INC DE

        self.emit(&[0x06, 4]);     // LD B, 4
        self.emit(&[0xB7]);        // OR A (clear carry/borrow)

        let loop_label = self.gen_label();
        let done_label = self.gen_label();
        self.define_label(&loop_label);
        self.emit(&[0x1A]);        // LD A, (DE)
        self.emit(&[0x9E]);        // SBC A, (HL)
        self.emit(&[0x27]);        // DAA
        self.emit(&[0x12]);        // LD (DE), A
        self.emit(&[0x2B]);        // DEC HL
        self.emit(&[0x1B]);        // DEC DE
        self.emit(&[0xF5]);        // PUSH AF
        self.emit(&[0x05]);        // DEC B
        self.emit_jr_z(&done_label);
        self.emit(&[0xF1]);        // POP AF
        self.emit_jr(&loop_label);
        self.define_label(&done_label);
        self.emit(&[0xF1]);        // POP AF
        self.emit(&[0xC9]);        // RET
    }

    fn emit_bcd_mul_routine(&mut self) {
        // BCD multiplication via repeated addition
        // DE points to multiplicand, HL points to multiplier
        // Result stored at DE (in-place)
        // Uses BCD_TEMP at 0x3700 to save original multiplicand
        // Supports multipliers up to 9999 (uses byte2 and byte3)

        const BCD_TEMP: u16 = 0x3700;

        // Stack layout after setup:
        //   [result_ptr (DE)]  <- SP+2
        //   [counter (BC)]     <- SP

        // 1. Save result pointer (= multiplicand ptr)
        self.emit(&[0xD5]);        // PUSH DE (save result ptr)

        // 2. Copy multiplicand to BCD_TEMP
        //    DE = source, need to copy to BCD_TEMP
        self.emit(&[0xE5]);        // PUSH HL (save multiplier ptr)
        self.emit(&[0x21]);        // LD HL, BCD_TEMP
        self.emit_word(BCD_TEMP);
        self.emit(&[0xEB]);        // EX DE, HL (HL = multiplicand, DE = BCD_TEMP)
        self.emit(&[0x06, 4]);     // LD B, 4
        // Copy loop
        let copy_loop = self.gen_label();
        self.define_label(&copy_loop);
        self.emit(&[0x7E]);        // LD A, (HL)
        self.emit(&[0x12]);        // LD (DE), A
        self.emit(&[0x23]);        // INC HL
        self.emit(&[0x13]);        // INC DE
        self.emit(&[0x10]);        // DJNZ
        self.emit(&[0xFA]);        // -6 (back to copy loop)

        // 3. Get multiplier ptr back and convert to binary counter
        self.emit(&[0xE1]);        // POP HL (multiplier ptr)
        // Get byte2 (100s/1000s) and byte3 (10s/1s)
        self.emit(&[0x23]);        // INC HL (skip byte0)
        self.emit(&[0x23]);        // INC HL (skip byte1)
        self.emit(&[0x5E]);        // LD E, (HL) - byte2: 1000s(high)/100s(low)
        self.emit(&[0x23]);        // INC HL
        self.emit(&[0x7E]);        // LD A, (HL) - byte3: 10s(high)/1s(low)

        // Convert byte3 packed BCD in A to binary
        // A = 0xTU where T=tens, U=units, value = T*10 + U
        self.emit(&[0x4F]);        // LD C, A (save full byte3)
        self.emit(&[0xE6, 0x0F]);  // AND 0x0F (units)
        self.emit(&[0x47]);        // LD B, A (B = units)
        self.emit(&[0x79]);        // LD A, C (restore byte3)
        self.emit(&[0xCB, 0x3F]);  // SRL A (shift right 4x)
        self.emit(&[0xCB, 0x3F]);
        self.emit(&[0xCB, 0x3F]);
        self.emit(&[0xCB, 0x3F]);  // A = tens digit
        // Multiply tens by 10: A * 10
        self.emit(&[0x4F]);        // LD C, A (save tens)
        self.emit(&[0x87]);        // ADD A, A (A*2)
        self.emit(&[0x87]);        // ADD A, A (A*4)
        self.emit(&[0x81]);        // ADD A, C (A*5)
        self.emit(&[0x87]);        // ADD A, A (A*10)
        self.emit(&[0x80]);        // ADD A, B (A*10 + units)
        self.emit(&[0x4F]);        // LD C, A (C = byte3 value: 0-99)

        // Now process byte2 (100s digit) using 16-bit arithmetic
        // C has byte3 value (0-99), E has byte2
        self.emit(&[0x7B]);        // LD A, E (byte2)
        self.emit(&[0xE6, 0x0F]);  // AND 0x0F (100s digit, 0-9)
        // Compute 100s_digit * 100 in HL using repeated addition
        self.emit(&[0x21, 0, 0]);  // LD HL, 0
        self.emit(&[0xB7]);        // OR A (check if 0)
        let skip_100s = self.gen_label();
        self.emit(&[0x28, 12]);    // JR Z, skip_100s (skip if digit is 0)
        self.emit(&[0x47]);        // LD B, A (counter = digit, 1-9)
        self.emit(&[0x11]);        // LD DE, 100
        self.emit_word(100);
        let add_100s_loop = self.gen_label();
        self.define_label(&add_100s_loop);
        self.emit(&[0x19]);        // ADD HL, DE (HL += 100)
        self.emit(&[0x10, 0xFD]);  // DJNZ add_100s_loop (-3)
        self.define_label(&skip_100s);

        // HL now has 100s_digit * 100 (0-900)
        // Add byte3 value (in C) to get total multiplier (0-999)
        self.emit(&[0x06, 0]);     // LD B, 0
        self.emit(&[0x09]);        // ADD HL, BC (HL += byte3_value)

        // BC = HL (the 16-bit multiplier, 0-999)
        self.emit(&[0x44]);        // LD B, H
        self.emit(&[0x4D]);        // LD C, L

        // Stack currently has [result_ptr] at bottom
        // Save counter in BC, need to access result_ptr from stack

        // 4. Clear the result area (original multiplicand location)
        // Get result ptr from stack into IX (BC has counter)
        self.emit(&[0xDD, 0xE1]); // POP IX (get result ptr into IX)
        self.emit(&[0xDD, 0xE5]); // PUSH IX (save result ptr back)
        self.emit(&[0xC5]);        // PUSH BC (save counter on top)
        self.emit(&[0xAF]);        // XOR A
        self.emit(&[0xDD, 0x77, 0]); // LD (IX+0), A
        self.emit(&[0xDD, 0x77, 1]); // LD (IX+1), A
        self.emit(&[0xDD, 0x77, 2]); // LD (IX+2), A
        self.emit(&[0xDD, 0x77, 3]); // LD (IX+3), A

        // 5. Multiply loop: add BCD_TEMP to result BC times
        self.emit(&[0xC1]);        // POP BC (get counter)

        let mul_loop = self.gen_label();
        let mul_done = self.gen_label();

        self.define_label(&mul_loop);
        // Check if counter is zero
        self.emit(&[0x78]);        // LD A, B
        self.emit(&[0xB1]);        // OR C
        self.emit(&[0xCA]);        // JP Z, mul_done
        self.fixups.push((self.code.len(), mul_done.clone()));
        self.emit_word(0x0000);

        // Save counter
        self.emit(&[0xC5]);        // PUSH BC

        // Load pointers for bcd_add: HL = BCD_TEMP, DE = result
        self.emit(&[0x21]);        // LD HL, BCD_TEMP
        self.emit_word(BCD_TEMP);
        self.emit(&[0xDD, 0xE5]); // PUSH IX
        self.emit(&[0xD1]);        // POP DE (DE = result ptr from IX)

        // Call BCD add
        self.emit_call("rt_bcd_add");

        // Restore counter and decrement
        self.emit(&[0xC1]);        // POP BC
        self.emit(&[0x0B]);        // DEC BC
        self.emit(&[0xC3]);        // JP mul_loop
        self.fixups.push((self.code.len(), mul_loop.clone()));
        self.emit_word(0x0000);

        self.define_label(&mul_done);
        // Clean up - result ptr is in IX (from loop), also on stack
        // Need to pop the result ptr from stack
        self.emit(&[0xD1]);        // POP DE (discard stack copy of result ptr)

        // Fixed-point adjustment: divide by 100 (shift BCD right 2 digits = 1 byte)
        // This corrects for 2 decimal places in each operand
        // IX still points to result
        // Shift: byte3 = byte2, byte2 = byte1, byte1 = byte0, byte0 = 0
        self.emit(&[0xDD, 0x7E, 2]);  // LD A, (IX+2)
        self.emit(&[0xDD, 0x77, 3]);  // LD (IX+3), A
        self.emit(&[0xDD, 0x7E, 1]);  // LD A, (IX+1)
        self.emit(&[0xDD, 0x77, 2]);  // LD (IX+2), A
        self.emit(&[0xDD, 0x7E, 0]);  // LD A, (IX+0)
        self.emit(&[0xDD, 0x77, 1]);  // LD (IX+1), A
        self.emit(&[0xAF]);           // XOR A
        self.emit(&[0xDD, 0x77, 0]);  // LD (IX+0), A

        self.emit(&[0xC9]);        // RET
    }

    fn emit_bcd_div_routine(&mut self) {
        // BCD division via repeated subtraction
        // DE points to dividend, HL points to divisor
        // Result (quotient) stored at DE (in-place)
        // Uses BCD_TEMP2 at 0x3704 for divisor copy
        // Uses BCD_TEMP3 at 0x3708 for quotient counter

        const BCD_TEMP2: u16 = 0x3704;  // Divisor copy
        const BCD_TEMP3: u16 = 0x3708;  // Quotient counter

        // 1. Save dividend pointer (this becomes result location)
        self.emit(&[0xD5]);        // PUSH DE (save dividend ptr)
        self.emit(&[0xE5]);        // PUSH HL (save divisor ptr)

        // 2. Copy divisor to BCD_TEMP2
        self.emit(&[0x11]);        // LD DE, BCD_TEMP2
        self.emit_word(BCD_TEMP2);
        // HL = divisor (already set)
        self.emit(&[0x06, 4]);     // LD B, 4
        let copy_loop1 = self.gen_label();
        self.define_label(&copy_loop1);
        self.emit(&[0x7E]);        // LD A, (HL)
        self.emit(&[0x12]);        // LD (DE), A
        self.emit(&[0x23]);        // INC HL
        self.emit(&[0x13]);        // INC DE
        self.emit(&[0x10, 0xFA]); // DJNZ -6

        // 3. Clear quotient counter at BCD_TEMP3
        self.emit(&[0x21]);        // LD HL, BCD_TEMP3
        self.emit_word(BCD_TEMP3);
        self.emit(&[0xAF]);        // XOR A
        self.emit(&[0x77]);        // LD (HL), A
        self.emit(&[0x23]);        // INC HL
        self.emit(&[0x77]);        // LD (HL), A
        self.emit(&[0x23]);        // INC HL
        self.emit(&[0x77]);        // LD (HL), A
        self.emit(&[0x23]);        // INC HL
        self.emit(&[0x77]);        // LD (HL), A

        // Restore pointers
        self.emit(&[0xE1]);        // POP HL (divisor, but we'll use BCD_TEMP2)
        self.emit(&[0xE1]);        // POP DE (dividend)
        self.emit(&[0xD5]);        // PUSH DE (save dividend ptr again for result)

        // 4. Division loop: while dividend >= divisor, subtract and count
        let div_loop = self.gen_label();
        let div_done = self.gen_label();

        self.define_label(&div_loop);

        // Compare dividend with divisor
        // First, save current dividend in case we need to restore
        self.emit(&[0xE5]);        // PUSH HL (save HL - it will be trashed)
        self.emit(&[0xD5]);        // PUSH DE (save dividend ptr)

        // Call BCD compare: dividend (DE) vs BCD_TEMP2 (divisor)
        self.emit(&[0x21]);        // LD HL, BCD_TEMP2
        self.emit_word(BCD_TEMP2);
        // DE already points to dividend
        self.emit_call("rt_bcd_cmp");
        // Returns: flags set based on (DE) - (HL), i.e., dividend - divisor
        // If carry, dividend < divisor, we're done

        self.emit(&[0xD1]);        // POP DE (restore dividend ptr)
        self.emit(&[0xE1]);        // POP HL (restore HL)

        // Check result - if carry set, dividend < divisor, exit loop
        self.emit(&[0xDA]);        // JP C, div_done
        self.fixups.push((self.code.len(), div_done.clone()));
        self.emit_word(0x0000);

        // Subtract divisor from dividend: dividend = dividend - BCD_TEMP2
        self.emit(&[0xD5]);        // PUSH DE (save dividend ptr)
        self.emit(&[0x21]);        // LD HL, BCD_TEMP2
        self.emit_word(BCD_TEMP2);
        // DE = dividend ptr
        self.emit_call("rt_bcd_sub");
        self.emit(&[0xD1]);        // POP DE (restore dividend ptr)

        // Increment quotient counter (4-byte BCD at BCD_TEMP3)
        // Add 1 to the quotient: we do this by adding BCD 00000001 to it
        // Simpler: just increment byte3 and handle overflow
        self.emit(&[0x21]);        // LD HL, BCD_TEMP3+3 (LSB of quotient)
        self.emit_word(BCD_TEMP3 + 3);
        self.emit(&[0x7E]);        // LD A, (HL)
        self.emit(&[0xC6, 0x01]);  // ADD A, 1
        self.emit(&[0x27]);        // DAA
        self.emit(&[0x77]);        // LD (HL), A
        // Handle carry to higher bytes
        self.emit(&[0x30, 12]);    // JR NC, +12 (skip overflow handling)
        self.emit(&[0x2B]);        // DEC HL (byte2)
        self.emit(&[0x7E]);        // LD A, (HL)
        self.emit(&[0xC6, 0x01]);  // ADD A, 1
        self.emit(&[0x27]);        // DAA
        self.emit(&[0x77]);        // LD (HL), A
        self.emit(&[0x30, 5]);     // JR NC, +5
        self.emit(&[0x2B]);        // DEC HL (byte1)
        self.emit(&[0x7E]);        // LD A, (HL)
        self.emit(&[0xC6, 0x01]);  // ADD A, 1
        self.emit(&[0x77]);        // LD (HL), A
        // byte0 overflow very unlikely for typical values

        // Continue loop
        self.emit(&[0xC3]);        // JP div_loop
        self.fixups.push((self.code.len(), div_loop.clone()));
        self.emit_word(0x0000);

        self.define_label(&div_done);

        // 5. Copy quotient (BCD_TEMP3) to result location (original dividend)
        self.emit(&[0xE1]);        // POP DE (result ptr = original dividend location)
        self.emit(&[0x21]);        // LD HL, BCD_TEMP3
        self.emit_word(BCD_TEMP3);
        self.emit(&[0x06, 4]);     // LD B, 4
        let copy_loop2 = self.gen_label();
        self.define_label(&copy_loop2);
        self.emit(&[0x7E]);        // LD A, (HL)
        self.emit(&[0x12]);        // LD (DE), A
        self.emit(&[0x23]);        // INC HL
        self.emit(&[0x13]);        // INC DE
        self.emit(&[0x10, 0xFA]); // DJNZ -6

        self.emit(&[0xC9]);        // RET
    }

    fn emit_int_to_bcd_routine(&mut self) {
        // Convert 16-bit unsigned int in HL to 4-byte BCD at DE
        // Layout: byte0 (MSB) to byte3 (LSB)
        // For 65535: 0x00, 0x06, 0x55, 0x35
        // For 42:    0x00, 0x00, 0x00, 0x42

        self.emit(&[0xD5]);        // PUSH DE (save result ptr)

        // Clear result buffer (4 bytes)
        self.emit(&[0xAF]);        // XOR A
        self.emit(&[0x12]);        // LD (DE), A - byte0
        self.emit(&[0x13]);        // INC DE
        self.emit(&[0x12]);        // LD (DE), A - byte1
        self.emit(&[0x13]);        // INC DE
        self.emit(&[0x12]);        // LD (DE), A - byte2
        self.emit(&[0x13]);        // INC DE
        self.emit(&[0x12]);        // LD (DE), A - byte3
        self.emit(&[0xD1]);        // POP DE (restore result ptr)
        self.emit(&[0xD5]);        // PUSH DE (save again for return)

        // Process 10000s digit (low nibble of byte1)
        self.emit(&[0x01]);        // LD BC, 10000
        self.emit_word(10000);
        self.emit_call("rt_bcd_div_digit");
        // A = 10000s digit (0-6 for 16-bit values)
        self.emit(&[0x13]);        // INC DE (point to byte1)
        self.emit(&[0x12]);        // LD (DE), A (store in byte1)

        // Process 1000s digit (high nibble of byte2)
        self.emit(&[0x01]);        // LD BC, 1000
        self.emit_word(1000);
        self.emit_call("rt_bcd_div_digit");
        self.emit(&[0xCB, 0x27]);  // SLA A (shift to high nibble)
        self.emit(&[0xCB, 0x27]);
        self.emit(&[0xCB, 0x27]);
        self.emit(&[0xCB, 0x27]);
        self.emit(&[0xF5]);        // PUSH AF (save 1000s in high nibble)

        // Process 100s digit (low nibble of byte2)
        self.emit(&[0x01]);        // LD BC, 100
        self.emit_word(100);
        self.emit_call("rt_bcd_div_digit");
        self.emit(&[0xC1]);        // POP BC (B = 1000s shifted)
        self.emit(&[0xB0]);        // OR B (combine)
        self.emit(&[0x13]);        // INC DE (point to byte2)
        self.emit(&[0x12]);        // LD (DE), A

        // Process 10s digit (high nibble of byte3)
        self.emit(&[0x01]);        // LD BC, 10
        self.emit_word(10);
        self.emit_call("rt_bcd_div_digit");
        self.emit(&[0xCB, 0x27]);  // SLA A x4
        self.emit(&[0xCB, 0x27]);
        self.emit(&[0xCB, 0x27]);
        self.emit(&[0xCB, 0x27]);
        self.emit(&[0xF5]);        // PUSH AF (save 10s in high nibble)

        // Units digit (low nibble of byte3)
        self.emit(&[0x7D]);        // LD A, L (remainder = units)
        self.emit(&[0xC1]);        // POP BC (B = 10s shifted)
        self.emit(&[0xB0]);        // OR B (combine)
        self.emit(&[0x13]);        // INC DE (point to byte3)
        self.emit(&[0x12]);        // LD (DE), A

        self.emit(&[0xD1]);        // POP DE (restore original result ptr)
        self.emit(&[0xC9]);        // RET

        // Helper: divide HL by BC, return quotient digit in A, remainder in HL
        self.define_label("rt_bcd_div_digit");
        self.emit(&[0xAF]);        // XOR A (digit counter = 0)
        let loop_label = self.gen_label();
        let done_label = self.gen_label();
        self.define_label(&loop_label);
        self.emit(&[0xB7]);        // OR A (clear carry for SBC)
        self.emit(&[0xED, 0x42]);  // SBC HL, BC
        self.emit_jr_c(&done_label);  // JR C, done (if borrow, over-subtracted)
        self.emit(&[0x3C]);        // INC A (count successful subtraction)
        self.emit_jr(&loop_label);
        self.define_label(&done_label);
        self.emit(&[0x09]);        // ADD HL, BC (restore after over-subtraction)
        self.emit(&[0xC9]);        // RET
    }

    fn emit_bcd_to_int_routine(&mut self) {
        // Convert 4-byte BCD at HL to 16-bit int in HL
        // Only handles values up to 65535
        // BCD layout: byte0 (MSB, usually 0), byte1 (10000s), byte2 (1000s/100s), byte3 (10s/1s)
        // Algorithm: result = sum of (digit * place_value) for all digits

        // Save BCD pointer in IX for indexed access
        self.emit(&[0xDD, 0xE5]); // PUSH IX (save caller's IX)
        self.emit(&[0xDD, 0x21]); // LD IX, HL... wait, need PUSH HL; POP IX
        // Actually: LD IX from HL
        self.emit(&[0xE5]);        // PUSH HL
        self.emit(&[0xDD, 0xE1]); // POP IX (IX = BCD ptr)

        // Initialize result to 0 in HL
        self.emit(&[0x21]);        // LD HL, 0
        self.emit_word(0);

        // Process 10000s digit (byte1)
        self.emit(&[0xDD, 0x7E, 1]); // LD A, (IX+1)
        self.emit(&[0xE6, 0x0F]);  // AND 0x0F
        self.emit(&[0xB7]);        // OR A (check if zero)
        let skip_10k = self.gen_label();
        self.emit_jr_z(&skip_10k);
        self.emit(&[0x47]);        // LD B, A
        self.emit(&[0x11]);        // LD DE, 10000
        self.emit_word(10000);
        let loop_10k = self.gen_label();
        self.define_label(&loop_10k);
        self.emit(&[0x19]);        // ADD HL, DE
        self.emit(&[0x10]);        // DJNZ
        self.emit_byte(0xFD);      // -3 (back to ADD HL, DE)
        self.define_label(&skip_10k);

        // Process 1000s digit (byte2 high nibble)
        self.emit(&[0xDD, 0x7E, 2]); // LD A, (IX+2)
        self.emit(&[0xCB, 0x3F]);  // SRL A x4
        self.emit(&[0xCB, 0x3F]);
        self.emit(&[0xCB, 0x3F]);
        self.emit(&[0xCB, 0x3F]);
        self.emit(&[0xB7]);        // OR A
        let skip_1k = self.gen_label();
        self.emit_jr_z(&skip_1k);
        self.emit(&[0x47]);        // LD B, A
        self.emit(&[0x11]);        // LD DE, 1000
        self.emit_word(1000);
        let loop_1k = self.gen_label();
        self.define_label(&loop_1k);
        self.emit(&[0x19]);        // ADD HL, DE
        self.emit(&[0x10]);        // DJNZ
        self.emit_byte(0xFD);      // -3
        self.define_label(&skip_1k);

        // Process 100s digit (byte2 low nibble)
        self.emit(&[0xDD, 0x7E, 2]); // LD A, (IX+2)
        self.emit(&[0xE6, 0x0F]);  // AND 0x0F
        self.emit(&[0xB7]);        // OR A
        let skip_100 = self.gen_label();
        self.emit_jr_z(&skip_100);
        self.emit(&[0x47]);        // LD B, A
        self.emit(&[0x11]);        // LD DE, 100
        self.emit_word(100);
        let loop_100 = self.gen_label();
        self.define_label(&loop_100);
        self.emit(&[0x19]);        // ADD HL, DE
        self.emit(&[0x10]);        // DJNZ
        self.emit_byte(0xFD);      // -3
        self.define_label(&skip_100);

        // Process 10s digit (byte3 high nibble)
        self.emit(&[0xDD, 0x7E, 3]); // LD A, (IX+3)
        self.emit(&[0xCB, 0x3F]);  // SRL A x4
        self.emit(&[0xCB, 0x3F]);
        self.emit(&[0xCB, 0x3F]);
        self.emit(&[0xCB, 0x3F]);
        self.emit(&[0xB7]);        // OR A
        let skip_10 = self.gen_label();
        self.emit_jr_z(&skip_10);
        self.emit(&[0x47]);        // LD B, A
        self.emit(&[0x11]);        // LD DE, 10
        self.emit_word(10);
        let loop_10 = self.gen_label();
        self.define_label(&loop_10);
        self.emit(&[0x19]);        // ADD HL, DE
        self.emit(&[0x10]);        // DJNZ
        self.emit_byte(0xFD);      // -3
        self.define_label(&skip_10);

        // Process 1s digit (byte3 low nibble)
        self.emit(&[0xDD, 0x7E, 3]); // LD A, (IX+3)
        self.emit(&[0xE6, 0x0F]);  // AND 0x0F
        // Just add A to L (with carry to H)
        self.emit(&[0x85]);        // ADD A, L
        self.emit(&[0x6F]);        // LD L, A
        self.emit(&[0x30, 1]);     // JR NC, +1
        self.emit(&[0x24]);        // INC H

        // Restore IX and return
        self.emit(&[0xDD, 0xE1]); // POP IX
        self.emit(&[0xC9]);        // RET
    }

    fn emit_bcd_cmp_routine(&mut self) {
        // Compare two 4-byte BCD numbers
        // DE points to first, HL points to second
        // Returns: Z flag if equal, A<0 (sign set) if DE<HL, A>0 if DE>HL
        // Compares from MSB to LSB

        self.emit(&[0x06, 4]);     // LD B, 4 (4 bytes)
        let loop_label = self.gen_label();
        let done_label = self.gen_label();
        self.define_label(&loop_label);
        self.emit(&[0x1A]);        // LD A, (DE) - get DE byte
        self.emit(&[0x96]);        // SUB (HL)   - subtract HL byte
        self.emit_jr_nz(&done_label);  // If not equal, done
        self.emit(&[0x23]);        // INC HL
        self.emit(&[0x13]);        // INC DE
        self.emit(&[0x10]);        // DJNZ loop
        self.fixups.push((self.code.len(), loop_label.clone()));
        self.emit_byte(0x00);
        // All bytes equal, A=0, Z=1
        self.define_label(&done_label);
        self.emit(&[0xC9]);        // RET
    }

    fn emit_print_decimal_routine(&mut self) {
        // Print 4-byte BCD number at HL
        // Skips leading zeros, prints decimal point after 6 digits (8-2 decimal places)

        self.emit(&[0x06, 0]);     // LD B, 0 (printed flag)
        self.emit(&[0x0E, 4]);     // LD C, 4 (4 bytes = 8 digits)

        let loop_label = self.gen_label();
        let skip_hi = self.gen_label();
        let skip_lo = self.gen_label();
        let done_label = self.gen_label();

        self.define_label(&loop_label);
        self.emit(&[0x7E]);        // LD A, (HL)

        // High nibble
        self.emit(&[0xF5]);        // PUSH AF (save byte)
        self.emit(&[0xCB, 0x3F]);  // SRL A x4
        self.emit(&[0xCB, 0x3F]);
        self.emit(&[0xCB, 0x3F]);
        self.emit(&[0xCB, 0x3F]);
        self.emit(&[0xB0]);        // OR B (check if printed yet or digit non-zero)
        self.emit_jr_z(&skip_hi);
        self.emit(&[0x06, 1]);     // LD B, 1 (mark printed)
        self.emit(&[0xF1]);        // POP AF
        self.emit(&[0xF5]);        // PUSH AF
        self.emit(&[0xCB, 0x3F]);  // SRL A x4
        self.emit(&[0xCB, 0x3F]);
        self.emit(&[0xCB, 0x3F]);
        self.emit(&[0xCB, 0x3F]);
        self.emit(&[0xC6, 0x30]);  // ADD A, '0'
        self.emit(&[0xD3, 0x81]);  // OUT (0x81), A
        self.define_label(&skip_hi);
        self.emit(&[0xF1]);        // POP AF

        // Low nibble
        self.emit(&[0xE6, 0x0F]);  // AND 0x0F
        self.emit(&[0xB0]);        // OR B
        self.emit_jr_z(&skip_lo);
        self.emit(&[0x06, 1]);     // LD B, 1
        self.emit(&[0x7E]);        // LD A, (HL)
        self.emit(&[0xE6, 0x0F]);  // AND 0x0F
        self.emit(&[0xC6, 0x30]);  // ADD A, '0'
        self.emit(&[0xD3, 0x81]);  // OUT (0x81), A
        self.define_label(&skip_lo);

        self.emit(&[0x23]);        // INC HL
        self.emit(&[0x0D]);        // DEC C
        self.emit_jr_nz(&loop_label);

        // If nothing printed, print 0
        self.emit(&[0x78]);        // LD A, B
        self.emit(&[0xB7]);        // OR A
        self.emit_jr_nz(&done_label);
        self.emit(&[0x3E, 0x30]);  // LD A, '0'
        self.emit(&[0xD3, 0x81]);  // OUT (0x81), A
        self.define_label(&done_label);
        self.emit(&[0xC9]);        // RET
    }

    fn emit_print_string_routine(&mut self) {
        // Print null-terminated string at HL
        // Input: HL = pointer to null-terminated string
        // Output: nothing (destroys A, HL)

        let loop_label = self.gen_label();
        let done_label = self.gen_label();

        self.define_label(&loop_label);
        self.emit(&[0x7E]);        // LD A, (HL)
        self.emit(&[0xB7]);        // OR A (check if zero)
        self.emit_jr_z(&done_label);
        self.emit(&[0xD3, 0x81]);  // OUT (0x81), A
        self.emit(&[0x23]);        // INC HL
        self.emit_jr(&loop_label);
        self.define_label(&done_label);
        self.emit(&[0xC9]);        // RET
    }

    fn emit_mul16_routine(&mut self) {
        // 16-bit unsigned multiply: HL * DE -> HL (low 16 bits of result)
        // Uses shift-and-add algorithm
        // Input: HL = multiplier, DE = multiplicand
        // Output: HL = HL * DE (low 16 bits)

        self.emit(&[0xC5]);        // PUSH BC (save BC)

        // BC = multiplier, HL will be result, DE = multiplicand (unchanged)
        self.emit(&[0x44]);        // LD B, H (save multiplier high)
        self.emit(&[0x4D]);        // LD C, L (save multiplier low)
        self.emit(&[0x21]);        // LD HL, 0 (result)
        self.emit_word(0);
        self.emit(&[0x3E, 16]);    // LD A, 16 (bit counter)

        let loop_label = self.gen_label();
        let skip_label = self.gen_label();

        self.define_label(&loop_label);
        // Shift BC (multiplier) right, check LSB
        self.emit(&[0xCB, 0x38]);  // SRL B
        self.emit(&[0xCB, 0x19]);  // RR C (rotate with carry from B)
        self.emit_jr_nc(&skip_label);
        // Bit was 1: add DE (multiplicand) to result
        self.emit(&[0x19]);        // ADD HL, DE
        self.define_label(&skip_label);
        // Shift DE (multiplicand) left for next bit
        self.emit(&[0xCB, 0x23]);  // SLA E
        self.emit(&[0xCB, 0x12]);  // RL D
        // Decrement counter
        self.emit(&[0x3D]);        // DEC A
        self.emit_jr_nz(&loop_label);

        self.emit(&[0xC1]);        // POP BC (restore BC)
        self.emit(&[0xC9]);        // RET
    }

    fn emit_div16_full_routine(&mut self) {
        // 16-bit unsigned divide: HL / DE -> HL (quotient)
        // Uses shift-and-subtract algorithm
        // Input: HL = dividend, DE = divisor
        // Output: HL = quotient

        self.emit(&[0xC5]);        // PUSH BC (save BC)
        self.emit(&[0xD5]);        // PUSH DE (save divisor for final restore)

        // Move dividend to BC (will become quotient), clear HL (remainder)
        self.emit(&[0x44]);        // LD B, H
        self.emit(&[0x4D]);        // LD C, L
        self.emit(&[0x21]);        // LD HL, 0 (remainder)
        self.emit_word(0);

        self.emit(&[0x3E, 16]);    // LD A, 16 (bit counter)

        let loop_label = self.gen_label();
        let no_sub_label = self.gen_label();

        self.define_label(&loop_label);
        // Shift BC left, high bit goes into HL via carry
        self.emit(&[0xCB, 0x21]);  // SLA C
        self.emit(&[0xCB, 0x10]);  // RL B
        self.emit(&[0xED, 0x6A]);  // ADC HL, HL (shift HL left with carry)

        // Try to subtract divisor from remainder
        self.emit(&[0xB7]);        // OR A (clear carry for SBC)
        self.emit(&[0xED, 0x52]);  // SBC HL, DE
        self.emit(&[0x30, 2]);     // JR NC, +2 (if no borrow, HL >= DE)
        // Borrow occurred (HL < DE): restore HL by adding DE back
        self.emit(&[0x19]);        // ADD HL, DE (restore remainder)
        self.emit_jr(&no_sub_label);
        // No borrow (HL >= DE): keep result and set quotient bit
        self.emit(&[0xCB, 0xC1]);  // SET 0, C (set LSB of quotient)
        self.define_label(&no_sub_label);

        // Next bit
        self.emit(&[0x3D]);        // DEC A
        self.emit_jr_nz(&loop_label);

        // Result: BC = quotient, HL = remainder
        self.emit(&[0x60]);        // LD H, B
        self.emit(&[0x69]);        // LD L, C

        self.emit(&[0xD1]);        // POP DE (restore divisor)
        self.emit(&[0xC1]);        // POP BC (restore BC)
        self.emit(&[0xC9]);        // RET
    }

    fn emit_mod16_routine(&mut self) {
        // 16-bit unsigned modulo: HL % DE -> HL (remainder)
        // Same algorithm as division but returns remainder instead of quotient

        self.emit(&[0xC5]);        // PUSH BC
        self.emit(&[0xD5]);        // PUSH DE (save divisor for final restore)

        // Move dividend to BC, clear HL (remainder)
        self.emit(&[0x44]);        // LD B, H
        self.emit(&[0x4D]);        // LD C, L
        self.emit(&[0x21]);        // LD HL, 0 (remainder)
        self.emit_word(0);

        self.emit(&[0x3E, 16]);    // LD A, 16 (bit counter)

        let loop_label = self.gen_label();
        let no_sub_label = self.gen_label();

        self.define_label(&loop_label);
        // Shift BC left, high bit into HL
        self.emit(&[0xCB, 0x21]);  // SLA C
        self.emit(&[0xCB, 0x10]);  // RL B
        self.emit(&[0xED, 0x6A]);  // ADC HL, HL (shift HL left with carry)

        // Try to subtract divisor from remainder
        self.emit(&[0xB7]);        // OR A (clear carry for SBC)
        self.emit(&[0xED, 0x52]);  // SBC HL, DE
        self.emit(&[0x30, 2]);     // JR NC, +2 (if no borrow, HL >= DE)
        // Borrow occurred (HL < DE): restore HL by adding DE back
        self.emit(&[0x19]);        // ADD HL, DE (restore remainder)
        self.emit_jr(&no_sub_label);
        // No borrow (HL >= DE): keep subtracted result
        self.define_label(&no_sub_label);

        self.emit(&[0x3D]);        // DEC A
        self.emit_jr_nz(&loop_label);

        // HL = remainder (already in HL)
        self.emit(&[0xD1]);        // POP DE (restore divisor)
        self.emit(&[0xC1]);        // POP BC
        self.emit(&[0xC9]);        // RET
    }

    fn emit_lt16_routine(&mut self) {
        // Signed 16-bit less-than comparison: HL < DE?
        // Returns: HL = 1 if true, HL = 0 if false

        // For signed comparison, we need to handle the sign bit
        // HL < DE (signed) is true when:
        // - HL negative, DE positive: always true
        // - Both same sign: unsigned comparison works
        // - HL positive, DE negative: always false

        // XOR the sign bits to check if signs differ
        self.emit(&[0x7C]);        // LD A, H
        self.emit(&[0xAA]);        // XOR D
        self.emit(&[0xF2]);        // JP P, same_sign (bit 7 = 0 means same sign)
        let same_sign = self.gen_label();
        self.fixups.push((self.code.len(), same_sign.clone()));
        self.emit_word(0x0000);

        // Signs differ: if HL is negative (H bit 7 = 1), return true
        self.emit(&[0x7C]);        // LD A, H
        self.emit(&[0xB7]);        // OR A (test bit 7)
        self.emit(&[0xF2]);        // JP P, return_false (H positive, so DE is negative, HL >= DE)
        let ret_false = self.gen_label();
        self.fixups.push((self.code.len(), ret_false.clone()));
        self.emit_word(0x0000);

        // H is negative, D is positive: HL < DE is true
        let ret_true = self.gen_label();
        self.emit(&[0xC3]);        // JP ret_true
        self.fixups.push((self.code.len(), ret_true.clone()));
        self.emit_word(0x0000);

        self.define_label(&same_sign);
        // Same sign: use unsigned comparison
        // HL < DE unsigned
        self.emit(&[0xB7]);        // OR A (clear carry)
        self.emit(&[0xED, 0x52]);  // SBC HL, DE
        self.emit(&[0x38]);        // JR C, ret_true (if carry, HL was < DE)
        self.fixups.push((self.code.len(), ret_true.clone()));
        self.emit_byte(0x00);

        self.define_label(&ret_false);
        self.emit(&[0x21]);        // LD HL, 0 (false)
        self.emit_word(0);
        self.emit(&[0xC9]);        // RET

        self.define_label(&ret_true);
        self.emit(&[0x21]);        // LD HL, 1 (true)
        self.emit_word(1);
        self.emit(&[0xC9]);        // RET
    }

    fn emit_jr_c(&mut self, label: &str) {
        self.emit_byte(0x38);  // JR C
        self.fixups.push((self.code.len(), label.to_string()));
        self.emit_byte(0x00);
    }

    fn emit_jr_nc(&mut self, label: &str) {
        self.emit_byte(0x30);  // JR NC
        self.fixups.push((self.code.len(), label.to_string()));
        self.emit_byte(0x00);
    }

    fn gen_binding(&mut self, binding: &Binding) -> Result<(), String> {
        if binding.params.is_empty() {
            // Simple value binding - evaluate and store in RAM
            self.gen_expr(&binding.body)?;

            // Determine storage size based on type
            let ty = self.type_env.get(&binding.name);
            let size = match ty {
                Some(crate::types::Type::Decimal) => 4,  // BCD is 4 bytes
                _ => 2,  // int, bool are 2 bytes
            };

            // Allocate global storage
            let addr = self.global_offset;
            self.globals.insert(binding.name.clone(), addr);
            self.global_offset += size;

            // Store result at the allocated address
            if size == 4 {
                // Decimal: HL points to BCD data, copy 4 bytes to global storage
                self.emit(&[0x11]);       // LD DE, addr
                self.emit_word(addr);
                self.emit(&[0x01, 4, 0]); // LD BC, 4
                self.emit(&[0xED, 0xB0]); // LDIR (copy from HL to DE)
            } else {
                // Int/Bool: value in HL, store at addr
                self.emit(&[0x22]);       // LD (addr), HL
                self.emit_word(addr);
            }
        } else {
            // Function binding
            // Calling convention:
            //   Caller pushes arguments right-to-left (last arg first, first arg last)
            //   Then CALL pushes return address
            //   Callee saves IX (old frame pointer) and sets IX = SP
            //
            // Stack layout after prologue:
            //   [arg_n]     <- IX + 4 + 2*(n-1)  (first pushed, highest addr)
            //   ...
            //   [arg_1]     <- IX + 4            (last pushed, lowest of args)
            //   [ret_addr]  <- IX + 2
            //   [saved_IX]  <- IX                (frame pointer points here)
            //   [locals...] <- IX - 2, IX - 4, ... (pushed during body evaluation)

            let fn_label = format!("fn_{}", binding.name);
            self.functions.insert(binding.name.clone(), fn_label.clone());
            self.define_label(&fn_label);

            // Function prologue: set up frame pointer using IX
            self.emit(&[0xDD, 0xE5]);    // PUSH IX (save caller's frame pointer)
            self.emit(&[0xDD, 0x21]);    // LD IX, 0
            self.emit_word(0);
            self.emit(&[0xDD, 0x39]);    // ADD IX, SP (IX = current SP = frame pointer)

            // Bind parameters to stack locations (positive offsets from IX)
            // Use closure calling convention: IX+4 = closure_ptr (ignored for named functions)
            // User parameters start at IX+6
            self.locals.clear();
            self.param_offset = 6;  // Skip saved IX (2), return addr (2), closure_ptr (2)
            self.local_offset = -2; // Locals start below frame pointer

            // Arguments are pushed right-to-left by caller, so first param is at lowest address
            // For `f a b c` called as: PUSH c, PUSH b, PUSH a, PUSH closure_ptr, CALL f
            // After prologue: closure_ptr at IX+4, a at IX+6, b at IX+8, c at IX+10
            for param in &binding.params {
                self.locals.insert(param.name.clone(), self.param_offset);
                self.param_offset += 2;
            }

            // Set up tail-call optimization context for recursive functions
            let body_label = if binding.recursive {
                let label = self.gen_label();
                self.define_label(&label);
                self.current_fn_name = Some(binding.name.clone());
                self.current_fn_body_label = Some(label.clone());
                self.current_fn_param_count = binding.params.len();
                Some(label)
            } else {
                None
            };

            // Generate body with tail-call awareness
            self.gen_expr_tail(&binding.body)?;

            // Clear TCO context
            self.current_fn_name = None;
            self.current_fn_body_label = None;
            self.current_fn_param_count = 0;

            // Function epilogue: restore frame pointer and return
            // Result is in HL
            self.emit(&[0xDD, 0xE1]);    // POP IX (restore caller's frame pointer)
            self.emit(&[0xC9]);          // RET
            let _ = body_label;  // Suppress unused warning
        }
        Ok(())
    }

    /// Emit deferred lambda bodies (lambdas that were stored as values)
    fn emit_deferred_lambdas(&mut self) -> Result<(), String> {
        // Take ownership of deferred lambdas to avoid borrow issues
        let lambdas = std::mem::take(&mut self.deferred_lambdas);

        for (label, params, body, captures) in lambdas {
            self.define_label(&label);

            // Function prologue: set up frame pointer using IX
            self.emit(&[0xDD, 0xE5]);    // PUSH IX (save caller's frame pointer)
            self.emit(&[0xDD, 0x21]);    // LD IX, 0
            self.emit_word(0);
            self.emit(&[0xDD, 0x39]);    // ADD IX, SP (IX = current SP = frame pointer)

            // Bind parameters to stack locations (positive offsets from IX)
            // Stack layout after prologue:
            //   IX+4 = closure_ptr (always passed as hidden first arg)
            //   IX+6 = first user arg
            //   IX+8 = second user arg
            //   ...
            self.locals.clear();
            self.param_offset = 6;  // Skip saved IX (2), return addr (2), closure_ptr (2)
            self.local_offset = -2;

            for param in &params {
                self.locals.insert(param.name.clone(), self.param_offset);
                self.param_offset += 2;
            }

            // If there are captures, load them from the closure and push as locals
            // Closure layout: [fn_addr (2), cap1 (2), cap2 (2), ...]
            if !captures.is_empty() {
                // Load closure_ptr from IX+4
                self.emit(&[0xDD, 0x6E, 0x04]);  // LD L, (IX+4)
                self.emit(&[0xDD, 0x66, 0x05]);  // LD H, (IX+5)
                // HL now = closure_ptr

                // Save closure_ptr in BC for repeated access
                self.emit(&[0x44]);  // LD B, H
                self.emit(&[0x4D]);  // LD C, L

                for (i, cap_name) in captures.iter().enumerate() {
                    // Restore closure_ptr to HL
                    self.emit(&[0x60]);  // LD H, B
                    self.emit(&[0x69]);  // LD L, C

                    // Add offset: 2 + i*2 to get to capture i
                    let cap_offset = 2 + (i * 2) as u16;
                    if cap_offset > 0 {
                        self.emit(&[0x11]);  // LD DE, offset
                        self.emit_word(cap_offset);
                        self.emit(&[0x19]);  // ADD HL, DE
                    }

                    // Load capture value from (HL)
                    self.emit(&[0x5E]);  // LD E, (HL)
                    self.emit(&[0x23]);  // INC HL
                    self.emit(&[0x56]);  // LD D, (HL)
                    self.emit(&[0xEB]);  // EX DE, HL (HL = capture value)

                    // Push capture value as a local
                    self.emit(&[0xE5]);  // PUSH HL
                    self.locals.insert(cap_name.clone(), self.local_offset);
                    self.local_offset -= 2;
                }
            }

            // Generate body
            self.gen_expr(&body)?;

            // Clean up captured locals from stack
            for _ in &captures {
                self.emit(&[0xD1]);  // POP DE (discard capture)
            }

            // Function epilogue: restore frame pointer and return
            self.emit(&[0xDD, 0xE1]);    // POP IX (restore caller's frame pointer)
            self.emit(&[0xC9]);          // RET
        }

        Ok(())
    }

    /// Generate expression in tail position (enables tail-call optimization)
    fn gen_expr_tail(&mut self, expr: &Expr) -> Result<(), String> {
        match expr {
            // If expression: propagate tail position to branches
            Expr::If(cond, then_branch, else_branch) => {
                self.gen_expr(cond)?;

                let else_label = self.gen_label();
                let end_label = self.gen_label();

                self.emit(&[0x7C]);  // LD A, H
                self.emit(&[0xB5]);  // OR L
                self.emit_jp_z(&else_label);

                self.gen_expr_tail(then_branch)?;
                self.emit_jp(&end_label);

                self.define_label(&else_label);
                self.gen_expr_tail(else_branch)?;

                self.define_label(&end_label);
            }

            // Let expression: propagate tail position to body
            Expr::Let { name, recursive, params, value, body } => {
                if !params.is_empty() {
                    return Err("Local functions not yet supported".to_string());
                }

                // Evaluate the value
                if *recursive {
                    let saved_offset = self.local_offset;
                    self.locals.insert(name.clone(), self.local_offset);
                    self.local_offset -= 2;
                    self.gen_expr(value)?;
                    self.emit(&[0xE5]);  // PUSH HL
                    self.gen_expr_tail(body)?;
                    self.emit(&[0xD1]);  // POP DE
                    self.locals.remove(name);
                    self.local_offset = saved_offset;
                } else {
                    self.gen_expr(value)?;
                    self.emit(&[0xE5]);  // PUSH HL
                    let saved_offset = self.local_offset;
                    self.locals.insert(name.clone(), self.local_offset);
                    self.local_offset -= 2;
                    self.gen_expr_tail(body)?;
                    self.emit(&[0xD1]);  // POP DE
                    self.locals.remove(name);
                    self.local_offset = saved_offset;
                }
            }

            // Function application: check for self-recursive tail call
            Expr::App(_, _) => {
                // Check if this is a tail-recursive call to the current function
                let (func_expr, args) = self.collect_app_args_with_expr(expr);

                if let Some(ref current_fn) = self.current_fn_name.clone() {
                    if let Expr::Var(name) = func_expr {
                        if name == current_fn && args.len() == self.current_fn_param_count {
                            // This is a tail-recursive call!
                            return self.gen_tail_call(&args);
                        }
                    }
                }

                // Not a tail call, generate normally
                self.gen_expr(expr)?;
            }

            // Match expression: propagate tail position to arms
            Expr::Match { scrutinee, arms } => {
                self.gen_expr(scrutinee)?;

                let end_label = self.gen_label();

                for (i, arm) in arms.iter().enumerate() {
                    let next_arm = if i < arms.len() - 1 {
                        self.gen_label()
                    } else {
                        end_label.clone()
                    };

                    // Generate pattern matching code (same as non-tail version)
                    self.gen_match_pattern_tail(arm, &next_arm, &end_label, i, arms.len())?;

                    if i < arms.len() - 1 {
                        self.define_label(&next_arm);
                    }
                }

                self.define_label(&end_label);
            }

            // For all other expressions, no tail call optimization needed
            _ => {
                self.gen_expr(expr)?;
            }
        }
        Ok(())
    }

    /// Generate a tail-recursive call (overwrites args and jumps back to body)
    fn gen_tail_call(&mut self, args: &[&Expr]) -> Result<(), String> {
        // Evaluate all new arguments and push onto stack
        // We push them in forward order, so we can pop them in reverse order into the param slots
        for arg in args.iter() {
            self.gen_expr(arg)?;
            self.emit(&[0xE5]);  // PUSH HL
        }

        // Pop new values into the parameter slots (in reverse order)
        // Parameters are at IX+6, IX+8, IX+10, etc.
        for i in (0..args.len()).rev() {
            let offset = (6 + i * 2) as i8;
            let offset_byte = offset as u8;
            let offset_plus1 = (offset + 1) as u8;
            self.emit(&[0xE1]);  // POP HL
            self.emit(&[0xDD, 0x75, offset_byte]);      // LD (IX+offset), L
            self.emit(&[0xDD, 0x74, offset_plus1]);     // LD (IX+offset+1), H
        }

        // Jump back to the body label (after prologue)
        if let Some(ref body_label) = self.current_fn_body_label.clone() {
            self.emit_jp(body_label);
        }

        Ok(())
    }

    /// Generate pattern matching for a single arm in tail position
    fn gen_match_pattern_tail(&mut self, arm: &MatchArm, next_arm: &str, end_label: &str, i: usize, total_arms: usize) -> Result<(), String> {
        match &arm.pattern {
            Pattern::Int(n) => {
                self.emit(&[0x11]);  // LD DE, n
                self.emit_word(*n as u16);
                self.emit(&[0xA7]);  // AND A
                self.emit(&[0xED, 0x52]);  // SBC HL, DE
                self.emit_jp_nz(next_arm);
                self.emit(&[0x21]);
                self.emit_word(*n as u16);

                // Check guard if present
                if let Some(guard) = &arm.guard {
                    self.gen_expr(guard)?;
                    self.emit(&[0x7C]);  // LD A, H
                    self.emit(&[0xB5]);  // OR L
                    self.emit_jp_z(next_arm);
                }

                self.gen_expr_tail(&arm.body)?;
                self.emit_jp(end_label);
            }
            Pattern::Range(start, end) => {
                let range_fail = self.gen_label();
                let range_ok = self.gen_label();

                self.emit(&[0xE5]);  // PUSH HL
                self.emit(&[0x11]);  // LD DE, start
                self.emit_word(*start as u16);
                self.emit(&[0xA7]);  // AND A
                self.emit(&[0xED, 0x52]);  // SBC HL, DE
                self.emit_jp_m(&range_fail);

                self.emit(&[0xE1]);  // POP HL
                self.emit(&[0xE5]);  // PUSH HL
                self.emit(&[0x11]);  // LD DE, end
                self.emit_word(*end as u16);
                self.emit(&[0xA7]);  // AND A
                self.emit(&[0xED, 0x52]);  // SBC HL, DE
                self.emit_jp_p(&range_fail);

                self.emit(&[0xE1]);  // POP HL
                self.emit_jp(&range_ok);

                self.define_label(&range_fail);
                self.emit(&[0xE1]);  // POP HL
                self.emit_jp(next_arm);

                self.define_label(&range_ok);

                // Check guard if present
                if let Some(guard) = &arm.guard {
                    self.gen_expr(guard)?;
                    self.emit(&[0x7C]);  // LD A, H
                    self.emit(&[0xB5]);  // OR L
                    self.emit_jp_z(next_arm);
                }

                self.gen_expr_tail(&arm.body)?;
                self.emit_jp(end_label);
            }
            Pattern::Var(name) => {
                let has_guard = arm.guard.is_some();
                let saved_offset = self.local_offset;

                if has_guard {
                    self.emit(&[0xE5]);  // PUSH HL (backup)
                }

                self.emit(&[0xE5]);  // PUSH HL (binding)
                self.locals.insert(name.clone(), self.local_offset);
                self.local_offset -= 2;

                if let Some(guard) = &arm.guard {
                    self.gen_expr(guard)?;
                    self.emit(&[0x7C]);  // LD A, H
                    self.emit(&[0xB5]);  // OR L
                    let guard_ok = self.gen_label();
                    self.emit_jp_nz(&guard_ok);
                    self.emit(&[0xD1]);  // POP DE (discard binding)
                    self.emit(&[0xE1]);  // POP HL (restore scrutinee)
                    self.local_offset = saved_offset;
                    self.locals.remove(name);
                    self.emit_jp(next_arm);
                    self.define_label(&guard_ok);
                }

                self.gen_expr_tail(&arm.body)?;

                self.emit(&[0xD1]);  // POP DE (discard binding)
                if has_guard {
                    self.emit(&[0xD1]);  // POP DE (discard backup)
                }
                self.local_offset = saved_offset;
                self.locals.remove(name);
                self.emit_jp(end_label);

                if i < total_arms - 1 {
                    // Note: next_arm is defined by caller
                }
            }
            Pattern::Wildcard => {
                // Check guard if present
                if let Some(guard) = &arm.guard {
                    self.gen_expr(guard)?;
                    self.emit(&[0x7C]);  // LD A, H
                    self.emit(&[0xB5]);  // OR L
                    self.emit_jp_z(next_arm);
                }

                self.gen_expr_tail(&arm.body)?;
                self.emit_jp(end_label);
            }
            Pattern::OptionNone => {
                self.emit(&[0xE5]);  // PUSH HL
                self.emit(&[0x5E]);  // LD E, (HL)
                self.emit(&[0x23]);  // INC HL
                self.emit(&[0x56]);  // LD D, (HL)
                self.emit(&[0x7A]);  // LD A, D
                self.emit(&[0xB3]);  // OR E
                self.emit(&[0xE1]);  // POP HL
                self.emit_jp_nz(next_arm);

                // Check guard if present
                if let Some(guard) = &arm.guard {
                    self.gen_expr(guard)?;
                    self.emit(&[0x7C]);  // LD A, H
                    self.emit(&[0xB5]);  // OR L
                    self.emit_jp_z(next_arm);
                }

                self.gen_expr_tail(&arm.body)?;
                self.emit_jp(end_label);
            }
            Pattern::OptionSome(name) => {
                self.emit(&[0xE5]);  // PUSH HL
                self.emit(&[0x5E]);  // LD E, (HL)
                self.emit(&[0x23]);  // INC HL
                self.emit(&[0x56]);  // LD D, (HL)
                self.emit(&[0x7A]);  // LD A, D
                self.emit(&[0xB3]);  // OR E
                self.emit(&[0xE1]);  // POP HL
                self.emit_jp_z(next_arm);

                self.emit(&[0x23]);  // INC HL
                self.emit(&[0x23]);  // INC HL
                self.emit(&[0x5E]);  // LD E, (HL)
                self.emit(&[0x23]);  // INC HL
                self.emit(&[0x56]);  // LD D, (HL)
                self.emit(&[0xEB]);  // EX DE, HL

                self.emit(&[0xE5]);  // PUSH HL
                let saved_offset = self.local_offset;
                self.locals.insert(name.clone(), self.local_offset);
                self.local_offset -= 2;

                if let Some(guard) = &arm.guard {
                    self.gen_expr(guard)?;
                    self.emit(&[0x7C]);  // LD A, H
                    self.emit(&[0xB5]);  // OR L
                    let guard_ok = self.gen_label();
                    self.emit_jp_nz(&guard_ok);
                    self.emit(&[0xD1]);  // POP DE
                    self.local_offset = saved_offset;
                    self.locals.remove(name);
                    self.emit_jp(next_arm);
                    self.define_label(&guard_ok);
                }

                self.gen_expr_tail(&arm.body)?;

                self.emit(&[0xD1]);  // POP DE
                self.local_offset = saved_offset;
                self.locals.remove(name);
                self.emit_jp(end_label);

                if i < total_arms - 1 {
                    // Note: next_arm is defined by caller
                }
            }
            Pattern::Tuple(patterns) => {
                self.emit(&[0x44]);  // LD B, H
                self.emit(&[0x4D]);  // LD C, L

                let saved_offset = self.local_offset;
                let mut bound_names: Vec<String> = Vec::new();

                for (idx, pat) in patterns.iter().enumerate() {
                    self.emit(&[0x60]);  // LD H, B
                    self.emit(&[0x69]);  // LD L, C

                    if idx > 0 {
                        self.emit(&[0x11]);  // LD DE, idx*2
                        self.emit_word((idx * 2) as u16);
                        self.emit(&[0x19]);  // ADD HL, DE
                    }

                    self.emit(&[0x5E]);  // LD E, (HL)
                    self.emit(&[0x23]);  // INC HL
                    self.emit(&[0x56]);  // LD D, (HL)
                    self.emit(&[0xEB]);  // EX DE, HL

                    match pat {
                        Pattern::Var(name) => {
                            self.emit(&[0xE5]);  // PUSH HL
                            self.locals.insert(name.clone(), self.local_offset);
                            self.local_offset -= 2;
                            bound_names.push(name.clone());
                        }
                        Pattern::Wildcard => {}
                        _ => {
                            return Err("Only variable and wildcard patterns supported in tuples".to_string());
                        }
                    }
                }

                if let Some(guard) = &arm.guard {
                    self.gen_expr(guard)?;
                    self.emit(&[0x7C]);  // LD A, H
                    self.emit(&[0xB5]);  // OR L
                    let guard_ok = self.gen_label();
                    self.emit_jp_nz(&guard_ok);
                    for name in bound_names.iter().rev() {
                        self.emit(&[0xD1]);  // POP DE
                        self.locals.remove(name);
                    }
                    self.local_offset = saved_offset;
                    self.emit_jp(next_arm);
                    self.define_label(&guard_ok);
                }

                self.gen_expr_tail(&arm.body)?;

                for name in bound_names.iter().rev() {
                    self.emit(&[0xD1]);  // POP DE
                    self.locals.remove(name);
                }
                self.local_offset = saved_offset;

                self.emit_jp(end_label);

                if i < total_arms - 1 {
                    // Note: next_arm is defined by caller
                }
            }
        }
        Ok(())
    }

    fn gen_expr(&mut self, expr: &Expr) -> Result<(), String> {
        match expr {
            Expr::Int(n) => {
                // Load immediate into HL
                self.emit(&[0x21]);  // LD HL, imm16
                self.emit_word(*n as u16);
            }

            Expr::Decimal(s) => {
                // Parse decimal and encode as BCD
                let bcd = self.parse_decimal_to_bcd(s);
                let addr = self.alloc_bcd_constant(bcd);
                // Load address of BCD constant into HL
                self.emit(&[0x21]);  // LD HL, addr
                self.emit_word(addr);
            }

            Expr::Bool(b) => {
                self.emit(&[0x21]);
                self.emit_word(if *b { 1 } else { 0 });
            }

            Expr::String(s) => {
                // Allocate string constant and load its address
                let addr = self.alloc_string_constant(s);
                self.emit(&[0x21]);  // LD HL, addr
                self.emit_word(addr);
            }

            Expr::Var(name) => {
                if let Some(&offset) = self.locals.get(name) {
                    // Load from stack frame using IX + signed offset
                    // Offset can be positive (params) or negative (locals)
                    // Z80 (IX+d) uses signed 8-bit displacement
                    let offset_byte = offset as i8 as u8;
                    let offset_plus1 = (offset + 1) as i8 as u8;
                    self.emit(&[0xDD, 0x6E, offset_byte]);      // LD L, (IX+offset)
                    self.emit(&[0xDD, 0x66, offset_plus1]);     // LD H, (IX+offset+1)
                } else if let Some(&addr) = self.globals.get(name) {
                    // Load from global storage
                    let ty = self.type_env.get(name);
                    if matches!(ty, Some(crate::types::Type::Decimal)) {
                        // Decimal: load address into HL
                        self.emit(&[0x21]);       // LD HL, addr
                        self.emit_word(addr);
                    } else {
                        // Int/Bool/Function pointer: load value into HL
                        self.emit(&[0x2A]);       // LD HL, (addr)
                        self.emit_word(addr);
                    }
                } else if self.functions.contains_key(name) {
                    // Named function used as a value - wrap in a closure
                    // Since all functions use the closure calling convention, we can
                    // directly use the function address (no trampoline needed)
                    let fn_label = self.functions.get(name).cloned().unwrap();

                    // Load current heap pointer
                    self.emit(&[0x2A]);  // LD HL, (CLOSURE_HEAP_PTR)
                    self.emit_word(0x3700);
                    self.emit(&[0xEB]);  // EX DE, HL (DE = closure_addr)

                    // Increment heap pointer by 2 (just fn_addr, no captures)
                    self.emit(&[0x21]);  // LD HL, 2
                    self.emit_word(2);
                    self.emit(&[0x19]);  // ADD HL, DE (HL = next closure addr)
                    self.emit(&[0x22]);  // LD (CLOSURE_HEAP_PTR), HL
                    self.emit_word(0x3700);

                    // DE = closure_addr
                    // Store function address at closure[0]
                    self.emit(&[0x21]);  // LD HL, fn_addr
                    self.fixups.push((self.code.len(), fn_label));
                    self.emit_word(0x0000);
                    // Store HL at (DE)
                    self.emit(&[0xEB]);  // EX DE, HL (HL = closure_addr, DE = fn_addr)
                    self.emit(&[0x73]);  // LD (HL), E
                    self.emit(&[0x23]);  // INC HL
                    self.emit(&[0x72]);  // LD (HL), D
                    self.emit(&[0x2B]);  // DEC HL (back to closure start)
                    // HL = closure_addr
                } else {
                    return Err(format!("Unknown variable: {}", name));
                }
            }

            Expr::BinOp(op, left, right) => {
                // Determine operand types for choosing int vs decimal operations
                let left_type = self.get_expr_type(left);
                let right_type = self.get_expr_type(right);
                let use_decimal = matches!(left_type, crate::types::Type::Decimal)
                    || matches!(right_type, crate::types::Type::Decimal);

                if use_decimal && matches!(op, BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div) {
                    // Decimal (BCD) arithmetic
                    // For BCD ops: DE = left ptr, HL = right ptr

                    // Evaluate left operand
                    self.gen_expr(left)?;
                    if !matches!(left_type, crate::types::Type::Decimal) {
                        // Need to convert int to decimal
                        // HL has int value, multiply by 100 for 2 decimal places, then convert to BCD
                        self.emit(&[0x11]);       // LD DE, 100
                        self.emit_word(100);
                        self.emit_call("rt_mul16");  // HL = HL * 100
                        let temp_addr = self.global_offset;
                        self.global_offset += 4;
                        self.emit(&[0x11]);       // LD DE, temp_addr
                        self.emit_word(temp_addr);
                        self.emit_call("rt_int_to_bcd");
                        self.emit(&[0x21]);       // LD HL, temp_addr (now points to BCD)
                        self.emit_word(temp_addr);
                    }
                    self.emit(&[0xE5]);  // PUSH HL (left BCD ptr)

                    // Evaluate right operand
                    self.gen_expr(right)?;
                    if !matches!(right_type, crate::types::Type::Decimal) {
                        // Convert int to decimal (multiply by 100 for 2 decimal places)
                        self.emit(&[0x11]);       // LD DE, 100
                        self.emit_word(100);
                        self.emit_call("rt_mul16");  // HL = HL * 100
                        let temp_addr = self.global_offset;
                        self.global_offset += 4;
                        self.emit(&[0x11]);       // LD DE, temp_addr
                        self.emit_word(temp_addr);
                        self.emit_call("rt_int_to_bcd");
                        self.emit(&[0x21]);       // LD HL, temp_addr
                        self.emit_word(temp_addr);
                    }
                    // HL = right BCD ptr

                    self.emit(&[0xD1]);  // POP DE (DE = left BCD ptr)

                    // Allocate result storage
                    let result_addr = self.global_offset;
                    self.global_offset += 4;

                    // Copy left to result (most BCD ops are in-place on first operand)
                    self.emit(&[0xE5]);        // PUSH HL (save right ptr)
                    self.emit(&[0x21]);        // LD HL, result_addr
                    self.emit_word(result_addr);
                    self.emit(&[0xEB]);        // EX DE, HL (HL = left, DE = result)
                    self.emit(&[0x01, 4, 0]);  // LD BC, 4
                    self.emit(&[0xED, 0xB0]);  // LDIR (copy left to result)
                    self.emit(&[0xE1]);        // POP HL (restore right ptr)

                    // Now: DE points past result, HL = right ptr
                    // Set up: DE = result ptr, HL = right ptr
                    self.emit(&[0x11]);        // LD DE, result_addr
                    self.emit_word(result_addr);

                    match op {
                        BinOp::Add => self.emit_call("rt_bcd_add"),
                        BinOp::Sub => self.emit_call("rt_bcd_sub"),
                        BinOp::Mul => self.emit_call("rt_bcd_mul"),
                        BinOp::Div => self.emit_call("rt_bcd_div"),
                        _ => {}
                    }

                    // Result is at result_addr, load address into HL
                    self.emit(&[0x21]);        // LD HL, result_addr
                    self.emit_word(result_addr);
                } else {
                    // Integer arithmetic (original code)
                    self.gen_expr(left)?;
                    self.emit(&[0xE5]);  // PUSH HL
                    self.gen_expr(right)?;
                    self.emit(&[0xD1]); // POP DE (left in DE, right in HL)

                    // Exchange so left is in HL for most ops
                    self.emit(&[0xEB]); // EX DE, HL (now left in HL, right in DE)

                    match op {
                        BinOp::Add => {
                            self.emit(&[0x19]);  // ADD HL, DE
                        }
                        BinOp::Sub => {
                            // HL = HL - DE
                            self.emit(&[0xA7]);  // AND A (clear carry)
                            self.emit(&[0xED, 0x52]);  // SBC HL, DE
                        }
                        BinOp::Mul => {
                            // 16-bit multiply: HL * DE -> HL
                            self.emit_call("rt_mul16");
                        }
                        BinOp::Div => {
                            // 16-bit divide: HL / DE -> HL
                            self.emit_call("rt_div16_full");
                        }
                        BinOp::Mod => {
                            // HL % DE -> HL (remainder)
                            self.emit_call("rt_mod16");
                        }
                        BinOp::Eq => {
                            // HL == DE
                            self.emit(&[0xA7]);       // AND A
                            self.emit(&[0xED, 0x52]); // SBC HL, DE
                            let true_label = self.gen_label();
                            let end_label = self.gen_label();
                            self.emit_jr_z(&true_label);
                            self.emit(&[0x21]); self.emit_word(0);  // LD HL, 0 (false)
                            self.emit_jr(&end_label);
                            self.define_label(&true_label);
                            self.emit(&[0x21]); self.emit_word(1);  // LD HL, 1 (true)
                            self.define_label(&end_label);
                        }
                        BinOp::NotEq => {
                            self.emit(&[0xA7]);
                            self.emit(&[0xED, 0x52]);
                            let true_label = self.gen_label();
                            let end_label = self.gen_label();
                            self.emit_jr_nz(&true_label);
                            self.emit(&[0x21]); self.emit_word(0);
                            self.emit_jr(&end_label);
                            self.define_label(&true_label);
                            self.emit(&[0x21]); self.emit_word(1);
                            self.define_label(&end_label);
                        }
                        BinOp::Lt => {
                            // HL < DE (signed)
                            self.emit_call("rt_lt16");
                        }
                        BinOp::Gt => {
                            // HL > DE (swap and use lt)
                            self.emit(&[0xEB]);  // EX DE, HL
                            self.emit_call("rt_lt16");
                        }
                        BinOp::Le => {
                            // HL <= DE: not (HL > DE)
                            self.emit(&[0xEB]);
                            self.emit_call("rt_lt16");
                            // Invert result
                            self.emit(&[0x7D]);       // LD A, L
                            self.emit(&[0xEE, 0x01]); // XOR 1
                            self.emit(&[0x6F]);       // LD L, A
                        }
                        BinOp::Ge => {
                            // HL >= DE: not (HL < DE)
                            self.emit_call("rt_lt16");
                            self.emit(&[0x7D]);
                            self.emit(&[0xEE, 0x01]);
                            self.emit(&[0x6F]);
                        }
                        BinOp::And => {
                            // Logical AND
                            self.emit(&[0x7C]);  // LD A, H
                            self.emit(&[0xB5]);  // OR L
                            let false_label = self.gen_label();
                            let end_label = self.gen_label();
                            self.emit_jr_z(&false_label);
                            self.emit(&[0x7A]);  // LD A, D
                            self.emit(&[0xB3]);  // OR E
                            self.emit_jr_z(&false_label);
                            self.emit(&[0x21]); self.emit_word(1);
                            self.emit_jr(&end_label);
                            self.define_label(&false_label);
                            self.emit(&[0x21]); self.emit_word(0);
                            self.define_label(&end_label);
                        }
                        BinOp::Or => {
                            // Logical OR
                            self.emit(&[0x7C]);
                            self.emit(&[0xB5]);
                            let true_label = self.gen_label();
                            let end_label = self.gen_label();
                            self.emit_jr_nz(&true_label);
                            self.emit(&[0x7A]);
                            self.emit(&[0xB3]);
                            self.emit_jr_nz(&true_label);
                            self.emit(&[0x21]); self.emit_word(0);
                            self.emit_jr(&end_label);
                            self.define_label(&true_label);
                            self.emit(&[0x21]); self.emit_word(1);
                            self.define_label(&end_label);
                        }
                    }
                }
            }

            Expr::UnaryOp(op, inner) => {
                self.gen_expr(inner)?;
                match op {
                    UnaryOp::Neg => {
                        // Negate HL
                        self.emit(&[0xAF]);  // XOR A
                        self.emit(&[0x95]);  // SUB L
                        self.emit(&[0x6F]);  // LD L, A
                        self.emit(&[0x9F]);  // SBC A, A
                        self.emit(&[0x94]);  // SUB H
                        self.emit(&[0x67]);  // LD H, A
                    }
                    UnaryOp::Not => {
                        // Logical NOT: 0 -> 1, nonzero -> 0
                        self.emit(&[0x7C]);  // LD A, H
                        self.emit(&[0xB5]);  // OR L
                        let zero_label = self.gen_label();
                        let end_label = self.gen_label();
                        self.emit_jr_z(&zero_label);
                        self.emit(&[0x21]); self.emit_word(0);
                        self.emit_jr(&end_label);
                        self.define_label(&zero_label);
                        self.emit(&[0x21]); self.emit_word(1);
                        self.define_label(&end_label);
                    }
                }
            }

            Expr::If(cond, then_branch, else_branch) => {
                self.gen_expr(cond)?;
                // Test if HL is zero
                self.emit(&[0x7C]);  // LD A, H
                self.emit(&[0xB5]);  // OR L

                let else_label = self.gen_label();
                let end_label = self.gen_label();

                self.emit_jp_z(&else_label);
                self.gen_expr(then_branch)?;
                self.emit_jp(&end_label);
                self.define_label(&else_label);
                self.gen_expr(else_branch)?;
                self.define_label(&end_label);
            }

            Expr::Let { name, recursive: _, params, value, body } => {
                if params.is_empty() {
                    // Simple let: evaluate value, push to stack, bind with negative IX offset
                    self.gen_expr(value)?;
                    self.emit(&[0xE5]);  // PUSH HL (value now on stack below IX)

                    // Bind to current local offset (negative, below frame pointer)
                    let saved_offset = self.local_offset;
                    self.locals.insert(name.clone(), self.local_offset);
                    self.local_offset -= 2;  // Next local goes further below

                    self.gen_expr(body)?;

                    self.emit(&[0xD1]);  // POP DE (discard let binding, restore stack)
                    self.local_offset = saved_offset;  // Restore offset tracker
                    self.locals.remove(name);
                } else {
                    // Local function - not fully supported yet
                    return Err("Local functions not yet supported".to_string());
                }
            }

            Expr::App(_, _) => {
                // Collect all arguments from curried application
                let (func_expr, args) = self.collect_app_args_with_expr(expr);

                // Check if function is a lambda for immediate application
                if let Expr::Lambda { params, body } = func_expr {
                    return self.gen_lambda_app(params, body, &args);
                }

                // Check if this is a direct function call or an indirect call through a pointer
                if let Expr::Var(name) = func_expr {
                    // Handle built-in functions
                    match name.as_str() {
                        "print_int" => {
                            if args.len() != 1 {
                                return Err("print_int takes exactly one argument".to_string());
                            }
                            self.gen_expr(args[0])?;
                            self.emit_call("rt_print_int");
                            self.emit(&[0x21]);  // LD HL, 0
                            self.emit_word(0);
                            return Ok(());
                        }
                        "print_decimal" => {
                            if args.len() != 1 {
                                return Err("print_decimal takes exactly one argument".to_string());
                            }
                            self.gen_expr(args[0])?;
                            self.emit_call("rt_print_decimal");
                            self.emit(&[0x21]);
                            self.emit_word(0);
                            return Ok(());
                        }
                        "print_string" => {
                            if args.len() != 1 {
                                return Err("print_string takes exactly one argument".to_string());
                            }
                            self.gen_expr(args[0])?;
                            self.emit_call("rt_print_string");
                            self.emit(&[0x21]);
                            self.emit_word(0);
                            return Ok(());
                        }
                        "print_newline" => {
                            self.emit_call("rt_print_newline");
                            self.emit(&[0x21]);
                            self.emit_word(0);
                            return Ok(());
                        }
                        _ => {}
                    }

                    // Check if it's a direct function (defined with let f x = ...)
                    // vs a function pointer (let f = fun x -> ... or passed as arg)
                    let is_direct_function = self.functions.contains_key(name)
                        && !self.locals.contains_key(name)
                        && !self.globals.contains_key(name);

                    if is_direct_function {
                        // Direct function call with closure calling convention
                        // Push user args in reverse order
                        for arg in args.iter().rev() {
                            self.gen_expr(arg)?;
                            self.emit(&[0xE5]);  // PUSH HL
                        }

                        // Push dummy closure_ptr (0)
                        self.emit(&[0x21]);  // LD HL, 0
                        self.emit_word(0);
                        self.emit(&[0xE5]);  // PUSH HL (dummy closure_ptr)

                        let label = format!("fn_{}", name);
                        self.emit_call(&label);

                        // Clean up: closure_ptr + user args
                        let total_to_pop = args.len() + 1;
                        self.emit(&[0xEB]);  // EX DE, HL (save result)
                        for _ in 0..total_to_pop {
                            self.emit(&[0xE1]);  // POP HL (discard)
                        }
                        self.emit(&[0xEB]);  // EX DE, HL (restore result)
                        return Ok(());
                    }
                }

                // Indirect function call through a closure pointer
                // Closure format: [fn_addr (2 bytes), captured_values...]
                // Call convention: push closure_ptr as hidden first arg, then user args

                // First, evaluate the closure expression to get the closure pointer
                self.gen_expr(func_expr)?;
                // HL = closure_ptr, save it for later
                self.emit(&[0xE5]);  // PUSH HL (save closure_ptr)

                // Push user arguments in reverse order
                for arg in args.iter().rev() {
                    self.gen_expr(arg)?;
                    self.emit(&[0xE5]);  // PUSH HL
                }

                // Now push the closure_ptr as the hidden first argument
                // (it's below the user args on stack, need to reload from saved position)
                // Stack: [saved_closure_ptr, arg_n, ..., arg_1]
                // We need to push closure_ptr again at the top
                // Load from saved position: SP + (num_args * 2)
                let stack_offset = (args.len() * 2) as u16;
                self.emit(&[0x21]);  // LD HL, stack_offset
                self.emit_word(stack_offset);
                self.emit(&[0x39]);  // ADD HL, SP (HL = address of saved closure_ptr)
                self.emit(&[0x5E]);  // LD E, (HL)
                self.emit(&[0x23]);  // INC HL
                self.emit(&[0x56]);  // LD D, (HL)
                self.emit(&[0xD5]);  // PUSH DE (closure_ptr as hidden first arg)
                self.emit(&[0xEB]);  // EX DE, HL (HL = closure_ptr)

                // Load function address from closure[0]
                self.emit(&[0x5E]);  // LD E, (HL)
                self.emit(&[0x23]);  // INC HL
                self.emit(&[0x56]);  // LD D, (HL)
                self.emit(&[0xEB]);  // EX DE, HL (HL = fn_addr)

                // Indirect call: push return address, then JP (HL)
                let return_label = self.gen_label();
                self.emit(&[0x11]);  // LD DE, return_addr
                self.fixups.push((self.code.len(), return_label.clone()));
                self.emit_word(0x0000);
                self.emit(&[0xD5]);  // PUSH DE (return address)
                self.emit(&[0xE9]);  // JP (HL) - jump to function
                self.define_label(&return_label);

                // Clean up: closure_ptr + user args from stack
                // Stack: [saved_closure_ptr, arg_n, ..., arg_1, closure_ptr_copy]
                let total_to_pop = args.len() + 2;  // user args + closure_ptr + saved_closure_ptr
                self.emit(&[0xEB]);  // EX DE, HL (save result)
                for _ in 0..total_to_pop {
                    self.emit(&[0xE1]);  // POP HL (discard)
                }
                self.emit(&[0xEB]);  // EX DE, HL (restore result)
            }

            Expr::Match { scrutinee, arms } => {
                self.gen_expr(scrutinee)?;

                let end_label = self.gen_label();

                for (i, arm) in arms.iter().enumerate() {
                    let next_arm = if i < arms.len() - 1 {
                        self.gen_label()
                    } else {
                        end_label.clone()
                    };

                    match &arm.pattern {
                        Pattern::Int(n) => {
                            // Compare HL with n
                            self.emit(&[0x11]);  // LD DE, n
                            self.emit_word(*n as u16);
                            self.emit(&[0xA7]);  // AND A
                            self.emit(&[0xED, 0x52]);  // SBC HL, DE
                            self.emit_jp_nz(&next_arm);
                            // Restore HL for body
                            self.emit(&[0x21]);
                            self.emit_word(*n as u16);
                        }
                        Pattern::Range(start, end) => {
                            // Range pattern: start..end (inclusive start, exclusive end)
                            // Match if start <= HL < end
                            let range_fail = self.gen_label();
                            let range_ok = self.gen_label();

                            // Save scrutinee for second check and body
                            self.emit(&[0xE5]);  // PUSH HL

                            // Check HL >= start (signed comparison)
                            self.emit(&[0x11]);  // LD DE, start
                            self.emit_word(*start as u16);
                            self.emit(&[0xA7]);  // AND A (clear carry)
                            self.emit(&[0xED, 0x52]);  // SBC HL, DE
                            self.emit_jp_m(&range_fail);  // if HL - start < 0, HL < start

                            // Restore HL for second check
                            self.emit(&[0xE1]);  // POP HL
                            self.emit(&[0xE5]);  // PUSH HL (save again)

                            // Check HL < end (signed comparison)
                            self.emit(&[0x11]);  // LD DE, end
                            self.emit_word(*end as u16);
                            self.emit(&[0xA7]);  // AND A (clear carry)
                            self.emit(&[0xED, 0x52]);  // SBC HL, DE
                            self.emit_jp_p(&range_fail);  // if HL - end >= 0, HL >= end

                            // Success - restore HL and continue to guard/body
                            self.emit(&[0xE1]);  // POP HL
                            self.emit_jp(&range_ok);

                            // Failure path - restore HL and try next arm
                            self.define_label(&range_fail);
                            self.emit(&[0xE1]);  // POP HL
                            self.emit_jp(&next_arm);

                            self.define_label(&range_ok);
                        }
                        Pattern::Var(name) => {
                            let has_guard = arm.guard.is_some();
                            let saved_offset = self.local_offset;

                            // If there's a guard, save scrutinee as backup
                            if has_guard {
                                self.emit(&[0xE5]);  // PUSH HL (backup for guard failure)
                            }

                            // Bind variable with negative offset (below frame pointer)
                            self.emit(&[0xE5]);  // PUSH HL (binding)
                            self.locals.insert(name.clone(), self.local_offset);
                            self.local_offset -= 2;

                            // Check guard if present
                            if let Some(guard) = &arm.guard {
                                self.gen_expr(guard)?;
                                // Guard result in HL, check if false (0)
                                self.emit(&[0x7C]);  // LD A, H
                                self.emit(&[0xB5]);  // OR L
                                // If guard fails, need to clean up binding and try next arm
                                let guard_ok = self.gen_label();
                                self.emit_jp_nz(&guard_ok);
                                // Guard failed - clean up and restore scrutinee
                                self.emit(&[0xD1]);  // POP DE (discard binding)
                                self.emit(&[0xE1]);  // POP HL (restore scrutinee from backup)
                                self.local_offset = saved_offset;
                                self.locals.remove(name);
                                self.emit_jp(&next_arm);
                                self.define_label(&guard_ok);
                            }

                            self.gen_expr(&arm.body)?;

                            self.emit(&[0xD1]);  // POP DE (discard binding)
                            if has_guard {
                                self.emit(&[0xD1]);  // POP DE (discard backup)
                            }
                            self.local_offset = saved_offset;
                            self.locals.remove(name);
                            self.emit_jp(&end_label);

                            if i < arms.len() - 1 {
                                self.define_label(&next_arm);
                            }
                            continue;
                        }
                        Pattern::Wildcard => {
                            // Always matches
                        }
                        Pattern::OptionNone => {
                            // HL = address of option struct [tag, value]
                            // Match if tag == 0
                            self.emit(&[0xE5]);  // PUSH HL (save option addr)
                            // Load tag from (HL)
                            self.emit(&[0x5E]);  // LD E, (HL)
                            self.emit(&[0x23]);  // INC HL
                            self.emit(&[0x56]);  // LD D, (HL)
                            // DE = tag, check if DE == 0
                            self.emit(&[0x7A]);  // LD A, D
                            self.emit(&[0xB3]);  // OR E
                            self.emit(&[0xE1]);  // POP HL (restore option addr)
                            self.emit_jp_nz(&next_arm);  // If tag != 0, try next pattern
                        }
                        Pattern::OptionSome(name) => {
                            // HL = address of option struct [tag, value]
                            // Match if tag == 1, then bind value
                            self.emit(&[0xE5]);  // PUSH HL (save option addr)
                            // Load tag from (HL)
                            self.emit(&[0x5E]);  // LD E, (HL)
                            self.emit(&[0x23]);  // INC HL
                            self.emit(&[0x56]);  // LD D, (HL)
                            // DE = tag, check if DE == 1
                            self.emit(&[0x7A]);  // LD A, D
                            self.emit(&[0xB3]);  // OR E
                            self.emit(&[0xE1]);  // POP HL (restore option addr)
                            self.emit_jp_z(&next_arm);  // If tag == 0 (None), try next pattern

                            // Tag is non-zero, it's Some - load value from (HL+2)
                            self.emit(&[0x23]);  // INC HL
                            self.emit(&[0x23]);  // INC HL (now at value)
                            self.emit(&[0x5E]);  // LD E, (HL)
                            self.emit(&[0x23]);  // INC HL
                            self.emit(&[0x56]);  // LD D, (HL)
                            self.emit(&[0xEB]);  // EX DE, HL (HL = value)

                            // Bind value to name
                            self.emit(&[0xE5]);  // PUSH HL (value on stack)
                            let saved_offset = self.local_offset;
                            self.locals.insert(name.clone(), self.local_offset);
                            self.local_offset -= 2;

                            // Check guard if present
                            if let Some(guard) = &arm.guard {
                                self.gen_expr(guard)?;
                                // Guard result in HL, check if false (0)
                                self.emit(&[0x7C]);  // LD A, H
                                self.emit(&[0xB5]);  // OR L
                                // If guard fails, need to clean up binding and try next arm
                                let guard_ok = self.gen_label();
                                self.emit_jp_nz(&guard_ok);
                                // Guard failed - clean up and try next
                                self.emit(&[0xD1]);  // POP DE (discard binding)
                                self.local_offset = saved_offset;
                                self.locals.remove(name);
                                self.emit_jp(&next_arm);
                                self.define_label(&guard_ok);
                            }

                            self.gen_expr(&arm.body)?;

                            self.emit(&[0xD1]);  // POP DE (discard binding)
                            self.local_offset = saved_offset;
                            self.locals.remove(name);
                            self.emit_jp(&end_label);

                            if i < arms.len() - 1 {
                                self.define_label(&next_arm);
                            }
                            continue;
                        }
                        Pattern::Tuple(patterns) => {
                            // HL = address of tuple (consecutive 16-bit values)
                            // Save tuple address in BC register to preserve across element loading
                            self.emit(&[0x44]);  // LD B, H
                            self.emit(&[0x4D]);  // LD C, L (BC = tuple addr)

                            let saved_offset = self.local_offset;
                            let mut bound_names: Vec<String> = Vec::new();

                            // Load each element and bind if it's a variable pattern
                            for (idx, pat) in patterns.iter().enumerate() {
                                // Restore tuple address from BC
                                self.emit(&[0x60]);  // LD H, B
                                self.emit(&[0x69]);  // LD L, C (HL = tuple addr)

                                // Add idx*2 to HL for this element
                                if idx > 0 {
                                    self.emit(&[0x11]);  // LD DE, idx*2
                                    self.emit_word((idx * 2) as u16);
                                    self.emit(&[0x19]);  // ADD HL, DE
                                }

                                // Now HL points to element, load it
                                self.emit(&[0x5E]);  // LD E, (HL)
                                self.emit(&[0x23]);  // INC HL
                                self.emit(&[0x56]);  // LD D, (HL)
                                self.emit(&[0xEB]);  // EX DE, HL (HL = element value)

                                match pat {
                                    Pattern::Var(name) => {
                                        // Bind this value to the variable
                                        self.emit(&[0xE5]);  // PUSH HL
                                        self.locals.insert(name.clone(), self.local_offset);
                                        self.local_offset -= 2;
                                        bound_names.push(name.clone());
                                    }
                                    Pattern::Wildcard => {
                                        // Do nothing, value is discarded
                                    }
                                    _ => {
                                        return Err("Only variable and wildcard patterns supported in tuples".to_string());
                                    }
                                }
                            }

                            // Check guard if present
                            if let Some(guard) = &arm.guard {
                                self.gen_expr(guard)?;
                                // Guard result in HL, check if false (0)
                                self.emit(&[0x7C]);  // LD A, H
                                self.emit(&[0xB5]);  // OR L
                                // If guard fails, need to clean up bindings and try next arm
                                let guard_ok = self.gen_label();
                                self.emit_jp_nz(&guard_ok);
                                // Guard failed - clean up bindings and try next
                                for name in bound_names.iter().rev() {
                                    self.emit(&[0xD1]);  // POP DE (discard binding)
                                    self.locals.remove(name);
                                }
                                self.local_offset = saved_offset;
                                self.emit_jp(&next_arm);
                                self.define_label(&guard_ok);
                            }

                            // Generate body with bindings in scope
                            self.gen_expr(&arm.body)?;

                            // Clean up bindings
                            for name in bound_names.iter().rev() {
                                self.emit(&[0xD1]);  // POP DE (discard binding)
                                self.locals.remove(name);
                            }
                            self.local_offset = saved_offset;

                            self.emit_jp(&end_label);

                            if i < arms.len() - 1 {
                                self.define_label(&next_arm);
                            }
                            continue;
                        }
                    }

                    // Check guard if present (for patterns without bindings)
                    if let Some(guard) = &arm.guard {
                        self.gen_expr(guard)?;
                        // Guard result in HL, check if false (0)
                        self.emit(&[0x7C]);  // LD A, H
                        self.emit(&[0xB5]);  // OR L
                        self.emit_jp_z(&next_arm);  // If HL == 0, try next arm
                    }

                    self.gen_expr(&arm.body)?;
                    self.emit_jp(&end_label);

                    if i < arms.len() - 1 {
                        self.define_label(&next_arm);
                    }
                }

                self.define_label(&end_label);
            }

            Expr::ToInt(inner) => {
                self.gen_expr(inner)?;
                self.emit_call("rt_bcd_to_int");
            }

            Expr::ToDecimal(inner) => {
                self.gen_expr(inner)?;
                self.emit_call("rt_int_to_bcd");
            }

            Expr::Seq(first, second) => {
                self.gen_expr(first)?;
                self.gen_expr(second)?;
            }

            Expr::Tuple(exprs) => {
                // Tuple is stored as consecutive 16-bit values in global memory
                // Returns address of first element in HL
                let tuple_addr = self.global_offset;
                let tuple_size = exprs.len() * 2;
                self.global_offset += tuple_size as u16;

                // Generate code for each element and store at the appropriate offset
                for (i, expr) in exprs.iter().enumerate() {
                    self.gen_expr(expr)?;
                    // HL = value of this element
                    // Store at tuple_addr + i*2
                    let elem_addr = tuple_addr + (i * 2) as u16;
                    self.emit(&[0x22]);  // LD (addr), HL
                    self.emit_word(elem_addr);
                }

                // Return address of tuple
                self.emit(&[0x21]);  // LD HL, tuple_addr
                self.emit_word(tuple_addr);
            }

            Expr::Typed(inner, _ty) => {
                self.gen_expr(inner)?;
            }

            Expr::Lambda { params, body } => {
                // Lambda as first-class value: generate code for the lambda body later,
                // and return its closure address now
                let lambda_label = format!("lambda_{}", self.next_label);
                self.next_label += 1;

                // Compute free variables (captures) for this lambda
                let mut bound: std::collections::HashSet<String> = std::collections::HashSet::new();
                for p in params {
                    bound.insert(p.name.clone());
                }
                let free = body.free_vars(&bound);

                // Filter out globals and known functions - only keep local variables
                let captures: Vec<String> = free.into_iter()
                    .filter(|name| self.locals.contains_key(name))
                    .collect();

                // Closure struct: [fn_addr (2 bytes), cap1, cap2, ...]
                // Allocate at RUNTIME using a closure heap pointer at CLOSURE_HEAP_PTR (0x3700)
                // The heap starts at 0x3710 and grows upward
                let closure_size = (2 + captures.len() * 2) as u16;

                // Load current heap pointer
                self.emit(&[0x2A]);  // LD HL, (CLOSURE_HEAP_PTR)
                self.emit_word(0x3700);
                // HL = current closure address, save it in DE
                self.emit(&[0xEB]);  // EX DE, HL (DE = closure_addr)

                // Increment heap pointer by closure_size
                self.emit(&[0x21]);  // LD HL, closure_size
                self.emit_word(closure_size);
                self.emit(&[0x19]);  // ADD HL, DE (HL = next closure addr)
                self.emit(&[0x22]);  // LD (CLOSURE_HEAP_PTR), HL
                self.emit_word(0x3700);

                // DE = our closure address, save in BC for later
                self.emit(&[0xD5]);  // PUSH DE (save closure_addr on stack)

                // Now DE = closure_addr
                // Store function address at closure[0]
                self.emit(&[0x21]);  // LD HL, fn_addr (placeholder)
                self.fixups.push((self.code.len(), lambda_label.clone()));
                self.emit_word(0x0000);
                // Store HL at (DE)
                self.emit(&[0xEB]);  // EX DE, HL (HL = closure_addr, DE = fn_addr)
                self.emit(&[0x73]);  // LD (HL), E
                self.emit(&[0x23]);  // INC HL
                self.emit(&[0x72]);  // LD (HL), D
                self.emit(&[0x23]);  // INC HL
                // HL now points to closure[1] (first capture slot)

                // Store captured values at closure[1], closure[2], ...
                // Save current closure write pointer in BC
                self.emit(&[0x44]);  // LD B, H
                self.emit(&[0x4D]);  // LD C, L

                for (_i, cap_name) in captures.iter().enumerate() {
                    // Load the captured variable's current value
                    if let Some(&offset) = self.locals.get(cap_name) {
                        if offset >= 0 {
                            // Parameter: positive offset from IX
                            self.emit(&[0xDD, 0x6E, offset as u8]);  // LD L, (IX+offset)
                            self.emit(&[0xDD, 0x66, (offset + 1) as u8]);  // LD H, (IX+offset+1)
                        } else {
                            // Local: negative offset from IX
                            let neg_offset = (offset as i8) as u8;
                            self.emit(&[0xDD, 0x6E, neg_offset]);  // LD L, (IX+offset)
                            self.emit(&[0xDD, 0x66, neg_offset.wrapping_add(1)]);  // LD H, (IX+offset+1)
                        }
                    }
                    // Store HL at (BC), then increment BC by 2
                    self.emit(&[0xEB]);  // EX DE, HL (DE = captured value)
                    self.emit(&[0x60]);  // LD H, B
                    self.emit(&[0x69]);  // LD L, C (HL = write pointer)
                    self.emit(&[0x73]);  // LD (HL), E
                    self.emit(&[0x23]);  // INC HL
                    self.emit(&[0x72]);  // LD (HL), D
                    self.emit(&[0x23]);  // INC HL
                    self.emit(&[0x44]);  // LD B, H
                    self.emit(&[0x4D]);  // LD C, L
                }

                // Defer the lambda body to be emitted after main code
                self.deferred_lambdas.push((lambda_label, params.clone(), body.clone(), captures));

                // Return closure address in HL (pop from stack)
                self.emit(&[0xE1]);  // POP HL (closure_addr)
            }

            Expr::None => {
                // None is stored as a 4-byte struct: [tag=0, value=0]
                // Allocate in global memory and return address
                let addr = self.global_offset;
                self.global_offset += 4;

                // Initialize tag to 0
                self.emit(&[0x21, 0, 0]);  // LD HL, 0
                self.emit(&[0x22]);        // LD (addr), HL
                self.emit_word(addr);
                // Also zero the value part
                self.emit(&[0x22]);        // LD (addr+2), HL
                self.emit_word(addr + 2);

                // Return address of option struct
                self.emit(&[0x21]);  // LD HL, addr
                self.emit_word(addr);
            }

            Expr::Some(inner) => {
                // Some(x) is stored as a 4-byte struct: [tag=1, value=x]
                // First evaluate the inner expression
                self.gen_expr(inner)?;
                // HL now contains the value

                // Allocate space for the option
                let addr = self.global_offset;
                self.global_offset += 4;

                // Store tag=1 at addr
                self.emit(&[0xE5]);        // PUSH HL (save value)
                self.emit(&[0x21, 1, 0]);  // LD HL, 1 (tag)
                self.emit(&[0x22]);        // LD (addr), HL
                self.emit_word(addr);

                // Store value at addr+2
                self.emit(&[0xE1]);        // POP HL (restore value)
                self.emit(&[0x22]);        // LD (addr+2), HL
                self.emit_word(addr + 2);

                // Return address of option struct
                self.emit(&[0x21]);        // LD HL, addr
                self.emit_word(addr);
            }
        }

        Ok(())
    }

    /// Generate code for an immediately-applied lambda: (fun x -> body) arg
    /// Sets up a temporary frame with IX for the lambda parameters.
    fn gen_lambda_app(&mut self, params: &[Param], body: &Expr, args: &[&Expr]) -> Result<(), String> {
        if args.len() != params.len() {
            return Err(format!(
                "Lambda expects {} arguments, got {}",
                params.len(),
                args.len()
            ));
        }

        // Save existing bindings for parameter names (in case of shadowing)
        let saved_offsets: Vec<_> = params.iter().map(|p| {
            (p.name.clone(), self.locals.get(&p.name).cloned())
        }).collect();

        let saved_local_offset = self.local_offset;

        // Set up a temporary frame for the lambda
        // Save IX, push args, set IX to point to the frame
        self.emit(&[0xDD, 0xE5]);  // PUSH IX (save caller's IX)

        // Push arguments onto stack (in order, so first arg is at lowest address)
        for arg in args.iter() {
            self.gen_expr(arg)?;
            self.emit(&[0xE5]); // PUSH HL
        }

        // Set up IX to point to the current stack position (after pushed args)
        // IX = SP: LD IX, 0; ADD IX, SP
        self.emit(&[0xDD, 0x21]);  // LD IX, 0
        self.emit_word(0);
        self.emit(&[0xDD, 0x39]);  // ADD IX, SP

        // Now stack layout (growing downward):
        //   [arg_n]     <- IX + 2*(n-1)  (highest address, first pushed if we had reversed)
        //   ...
        //   [arg_1]     <- IX + 0        (lowest address, last pushed)
        //   <- SP = IX points here

        // Wait, we pushed in forward order, so:
        //   [saved IX]
        //   [arg_0]     <- top of args
        //   [arg_1]
        //   ...
        //   [arg_n-1]   <- SP points here after pushes
        // After ADD IX, SP: IX = SP

        // Bind parameters: arg_i is at IX + 2*i (since we pushed in order, first arg is deepest)
        // Actually, stack grows down, so:
        //   After PUSH arg_0: stack has arg_0 at SP
        //   After PUSH arg_1: stack has arg_1 at SP, arg_0 at SP+2
        //   After PUSH arg_n-1: stack has arg_n-1 at SP, ..., arg_0 at SP + 2*(n-1)
        // IX = SP, so:
        //   arg_0 is at IX + 2*(n-1)  (deepest, first pushed)
        //   arg_n-1 is at IX + 0     (shallowest, last pushed)

        // We want param[0] (first parameter) to be arg[0] (first argument)
        // So param[i] maps to arg[i] which is at IX + 2*(n-1-i)
        let n = params.len();
        self.local_offset = 0; // Reset for this frame
        for (i, param) in params.iter().enumerate() {
            let offset = 2 * (n - 1 - i) as i16;
            self.locals.insert(param.name.clone(), offset);
        }

        // Generate body (parameters are now in scope)
        self.gen_expr(body)?;

        // Result is in HL. Clean up:
        // Pop args and restore IX
        self.emit(&[0xEB]); // EX DE, HL (save result in DE)
        for _ in 0..params.len() {
            self.emit(&[0xE1]); // POP HL (discard arg)
        }
        self.emit(&[0xDD, 0xE1]); // POP IX (restore caller's IX)
        self.emit(&[0xEB]); // EX DE, HL (restore result to HL)

        // Restore parameter bindings and local offset
        for (name, old_offset) in saved_offsets {
            if let Some(offset) = old_offset {
                self.locals.insert(name, offset);
            } else {
                self.locals.remove(&name);
            }
        }
        self.local_offset = saved_local_offset;

        Ok(())
    }
}

impl Default for CodeGenerator {
    fn default() -> Self {
        Self::new()
    }
}
