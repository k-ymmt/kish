//! Typed IR to packed bytecode encode pass and decode utility.

use crate::ir::bytecode::{
    ArithCompoundOp, ArithProgramOp, CommandDispatchHint, EncodedCodeObject, EncodedModule,
    Instruction, OpenMode, RedirectProgramOp, WordProgramOp,
};
use crate::ir::error::IrError;
use crate::ir::program::{ArithProgram, CodeObject, IrModule, RedirectProgram, WordProgram};

// ---------------------------------------------------------------------------
// VM Instruction opcodes (0x00..=0x22)
// ---------------------------------------------------------------------------
mod vm_op {
    pub const NOP: u8 = 0x00;
    pub const PUSH_CONST: u8 = 0x01;
    pub const PUSH_INT: u8 = 0x02;
    pub const PUSH_STRING: u8 = 0x03;
    pub const PUSH_SYMBOL: u8 = 0x04;
    pub const DROP: u8 = 0x05;
    pub const DUP: u8 = 0x06;
    pub const LOCAL_GET: u8 = 0x07;
    pub const LOCAL_SET: u8 = 0x08;
    pub const JMP: u8 = 0x09;
    pub const JMP_IF_ZERO: u8 = 0x0A;
    pub const JMP_IF_NON_ZERO: u8 = 0x0B;
    pub const CALL: u8 = 0x0C;
    pub const RET: u8 = 0x0D;
    pub const BEGIN_SIMPLE: u8 = 0x0E;
    pub const ADD_ARG: u8 = 0x0F;
    pub const ADD_ASSIGN: u8 = 0x10;
    pub const ADD_REDIR: u8 = 0x11;
    pub const END_SIMPLE: u8 = 0x12;
    pub const EXEC_SIMPLE: u8 = 0x13;
    pub const NEGATE_STATUS: u8 = 0x14;
    pub const EXEC_SUBSHELL: u8 = 0x15;
    pub const DEFINE_FUNCTION: u8 = 0x16;
    pub const EXEC_BACKGROUND: u8 = 0x17;
    pub const BEGIN_PIPELINE: u8 = 0x18;
    pub const ADD_PIPELINE_STAGE: u8 = 0x19;
    pub const EXEC_PIPELINE: u8 = 0x1A;
    pub const FOR_SETUP: u8 = 0x1B;
    pub const FOR_ADD_WORD: u8 = 0x1C;
    pub const FOR_NEXT: u8 = 0x1D;
    pub const CASE_SET_SUBJECT: u8 = 0x1E;
    pub const CASE_TEST_PATTERN: u8 = 0x1F;
    pub const CASE_CLEAR: u8 = 0x20;
    pub const PUSH_REDIRECT_SCOPE: u8 = 0x21;
    pub const POP_REDIRECT_SCOPE: u8 = 0x22;
}

// ---------------------------------------------------------------------------
// WordProgramOp opcodes (0x00..=0x07)
// ---------------------------------------------------------------------------
mod word_op {
    pub const PUSH_LITERAL: u8 = 0x00;
    pub const EXPAND_TILDE: u8 = 0x01;
    pub const EXPAND_PARAMETER: u8 = 0x02;
    pub const EXPAND_COMMAND_SUBSTITUTION: u8 = 0x03;
    pub const EXPAND_ARITHMETIC: u8 = 0x04;
    pub const FIELD_SPLIT: u8 = 0x05;
    pub const GLOB: u8 = 0x06;
    pub const QUOTE_REMOVAL: u8 = 0x07;
}

// ---------------------------------------------------------------------------
// RedirectProgramOp opcodes (0x00..=0x03)
// ---------------------------------------------------------------------------
mod redir_op {
    pub const OPEN: u8 = 0x00;
    pub const DUP: u8 = 0x01;
    pub const CLOSE: u8 = 0x02;
    pub const HERE_DOC: u8 = 0x03;
}

// ---------------------------------------------------------------------------
// ArithProgramOp opcodes (0x00..=0x21)
// ---------------------------------------------------------------------------
mod arith_op {
    pub const PUSH_LITERAL: u8 = 0x00;
    pub const LOAD_VARIABLE: u8 = 0x01;
    pub const UNARY_PLUS: u8 = 0x02;
    pub const UNARY_MINUS: u8 = 0x03;
    pub const BITWISE_NOT: u8 = 0x04;
    pub const LOGICAL_NOT: u8 = 0x05;
    pub const ADD: u8 = 0x06;
    pub const SUBTRACT: u8 = 0x07;
    pub const MULTIPLY: u8 = 0x08;
    pub const DIVIDE: u8 = 0x09;
    pub const MODULO: u8 = 0x0A;
    pub const BITWISE_AND: u8 = 0x0B;
    pub const BITWISE_OR: u8 = 0x0C;
    pub const BITWISE_XOR: u8 = 0x0D;
    pub const SHIFT_LEFT: u8 = 0x0E;
    pub const SHIFT_RIGHT: u8 = 0x0F;
    pub const LESS_THAN: u8 = 0x10;
    pub const GREATER_THAN: u8 = 0x11;
    pub const LESS_EQUAL: u8 = 0x12;
    pub const GREATER_EQUAL: u8 = 0x13;
    pub const EQUAL: u8 = 0x14;
    pub const NOT_EQUAL: u8 = 0x15;
    pub const LOGICAL_AND: u8 = 0x16;
    pub const LOGICAL_OR: u8 = 0x17;
    pub const ASSIGN: u8 = 0x18;
    pub const COMPOUND_ASSIGN: u8 = 0x19;
    pub const PRE_INCREMENT: u8 = 0x1A;
    pub const PRE_DECREMENT: u8 = 0x1B;
    pub const POST_INCREMENT: u8 = 0x1C;
    pub const POST_DECREMENT: u8 = 0x1D;
    pub const JMP_IF_ZERO: u8 = 0x1E;
    pub const JMP_IF_NON_ZERO: u8 = 0x1F;
    pub const JMP: u8 = 0x20;
    pub const POP: u8 = 0x21;
}

// ---------------------------------------------------------------------------
// Wide sentinel
// ---------------------------------------------------------------------------
const WIDE_SENTINEL: u32 = 0x00FF_FFFF;

// ---------------------------------------------------------------------------
// Packing helpers
// ---------------------------------------------------------------------------

/// Packs an 8-bit opcode and 24-bit operand into a single u32 word.
fn pack_word(opcode: u8, operand_24: u32) -> u32 {
    ((opcode as u32) << 24) | (operand_24 & 0x00FF_FFFF)
}

/// Emits a single-operand instruction. If the operand fits in 24 bits,
/// emits one word; otherwise emits a wide (2-word) encoding.
fn emit_single_operand(buf: &mut Vec<u32>, opcode: u8, operand: u32) {
    if operand >= WIDE_SENTINEL {
        buf.push(pack_word(opcode, WIDE_SENTINEL));
        buf.push(operand);
    } else {
        buf.push(pack_word(opcode, operand));
    }
}

/// Emits PushInt / PushLiteral(i64): always 3 words.
fn emit_i64(buf: &mut Vec<u32>, opcode: u8, value: i64) {
    let bits = value as u64;
    buf.push(pack_word(opcode, 0));
    buf.push((bits >> 32) as u32);
    buf.push(bits as u32);
}

/// Emits a two-operand instruction:
///   word 0: [opcode | operand1_24]  (potentially wide → word 0 + word 1)
///   ext:    operand2_u32
fn emit_two_operands(buf: &mut Vec<u32>, opcode: u8, operand1: u32, operand2: u32) {
    emit_single_operand(buf, opcode, operand1);
    buf.push(operand2);
}

// ---------------------------------------------------------------------------
// Encode helpers for CommandDispatchHint
// ---------------------------------------------------------------------------

fn hint_to_u24(hint: &CommandDispatchHint) -> u32 {
    match hint {
        CommandDispatchHint::NoCommand => 0,
        CommandDispatchHint::SpecialBuiltin => 1,
        CommandDispatchHint::Standard => 2,
    }
}

// ---------------------------------------------------------------------------
// Encode helpers for OpenMode
// ---------------------------------------------------------------------------

fn open_mode_to_u8(mode: &OpenMode) -> u8 {
    match mode {
        OpenMode::ReadOnly => 0,
        OpenMode::WriteCreate => 1,
        OpenMode::Append => 2,
        OpenMode::ReadWrite => 3,
        OpenMode::Clobber => 4,
    }
}

// ---------------------------------------------------------------------------
// Encode helpers for ArithCompoundOp
// ---------------------------------------------------------------------------

fn arith_compound_op_to_u32(op: &ArithCompoundOp) -> u32 {
    match op {
        ArithCompoundOp::Add => 0,
        ArithCompoundOp::Subtract => 1,
        ArithCompoundOp::Multiply => 2,
        ArithCompoundOp::Divide => 3,
        ArithCompoundOp::Modulo => 4,
        ArithCompoundOp::ShiftLeft => 5,
        ArithCompoundOp::ShiftRight => 6,
        ArithCompoundOp::BitwiseAnd => 7,
        ArithCompoundOp::BitwiseOr => 8,
        ArithCompoundOp::BitwiseXor => 9,
    }
}

// ---------------------------------------------------------------------------
// Code object encoding
// ---------------------------------------------------------------------------

/// Encodes a single code object into packed u32 words.
pub fn encode_code_object(co: &CodeObject) -> Result<EncodedCodeObject, IrError> {
    let mut words = Vec::new();

    for instr in &co.instructions {
        encode_instruction(&mut words, instr)?;
    }

    Ok(EncodedCodeObject {
        id: co.id,
        words,
        source_map: Vec::new(),
    })
}

fn encode_instruction(buf: &mut Vec<u32>, instr: &Instruction) -> Result<(), IrError> {
    match instr {
        Instruction::Nop => buf.push(pack_word(vm_op::NOP, 0)),
        Instruction::PushConst(id) => emit_single_operand(buf, vm_op::PUSH_CONST, id.value()),
        Instruction::PushInt(v) => emit_i64(buf, vm_op::PUSH_INT, *v),
        Instruction::PushString(id) => emit_single_operand(buf, vm_op::PUSH_STRING, id.value()),
        Instruction::PushSymbol(id) => emit_single_operand(buf, vm_op::PUSH_SYMBOL, id.value()),
        Instruction::Drop => buf.push(pack_word(vm_op::DROP, 0)),
        Instruction::Dup => buf.push(pack_word(vm_op::DUP, 0)),
        Instruction::LocalGet(id) => emit_single_operand(buf, vm_op::LOCAL_GET, id.value()),
        Instruction::LocalSet(id) => emit_single_operand(buf, vm_op::LOCAL_SET, id.value()),
        Instruction::Jmp(t) => emit_single_operand(buf, vm_op::JMP, t.index()),
        Instruction::JmpIfZero(t) => emit_single_operand(buf, vm_op::JMP_IF_ZERO, t.index()),
        Instruction::JmpIfNonZero(t) => {
            emit_single_operand(buf, vm_op::JMP_IF_NON_ZERO, t.index());
        }
        Instruction::Call(id) => emit_single_operand(buf, vm_op::CALL, id.value()),
        Instruction::Ret => buf.push(pack_word(vm_op::RET, 0)),
        Instruction::BeginSimple => buf.push(pack_word(vm_op::BEGIN_SIMPLE, 0)),
        Instruction::AddArg(id) => emit_single_operand(buf, vm_op::ADD_ARG, id.value()),
        Instruction::AddAssign(sym, wp) => {
            emit_two_operands(buf, vm_op::ADD_ASSIGN, sym.value(), wp.value());
        }
        Instruction::AddRedir(id) => emit_single_operand(buf, vm_op::ADD_REDIR, id.value()),
        Instruction::EndSimple => buf.push(pack_word(vm_op::END_SIMPLE, 0)),
        Instruction::ExecSimple(hint) => {
            buf.push(pack_word(vm_op::EXEC_SIMPLE, hint_to_u24(hint)));
        }
        Instruction::NegateStatus => buf.push(pack_word(vm_op::NEGATE_STATUS, 0)),
        Instruction::ExecSubshell(id) => {
            emit_single_operand(buf, vm_op::EXEC_SUBSHELL, id.value());
        }
        Instruction::DefineFunction { name, body } => {
            emit_two_operands(buf, vm_op::DEFINE_FUNCTION, name.value(), body.value());
        }
        Instruction::ExecBackground(id) => {
            emit_single_operand(buf, vm_op::EXEC_BACKGROUND, id.value());
        }
        Instruction::BeginPipeline(n) => emit_single_operand(buf, vm_op::BEGIN_PIPELINE, *n),
        Instruction::AddPipelineStage(id) => {
            emit_single_operand(buf, vm_op::ADD_PIPELINE_STAGE, id.value());
        }
        Instruction::ExecPipeline => buf.push(pack_word(vm_op::EXEC_PIPELINE, 0)),
        Instruction::ForSetup { var, word_count } => {
            emit_two_operands(buf, vm_op::FOR_SETUP, var.value(), *word_count);
        }
        Instruction::ForAddWord(id) => emit_single_operand(buf, vm_op::FOR_ADD_WORD, id.value()),
        Instruction::ForNext => buf.push(pack_word(vm_op::FOR_NEXT, 0)),
        Instruction::CaseSetSubject(id) => {
            emit_single_operand(buf, vm_op::CASE_SET_SUBJECT, id.value());
        }
        Instruction::CaseTestPattern(id) => {
            emit_single_operand(buf, vm_op::CASE_TEST_PATTERN, id.value());
        }
        Instruction::CaseClear => buf.push(pack_word(vm_op::CASE_CLEAR, 0)),
        Instruction::PushRedirectScope => buf.push(pack_word(vm_op::PUSH_REDIRECT_SCOPE, 0)),
        Instruction::PopRedirectScope => buf.push(pack_word(vm_op::POP_REDIRECT_SCOPE, 0)),
    }
    Ok(())
}

// ---------------------------------------------------------------------------
// Word program encoding
// ---------------------------------------------------------------------------

/// Encodes a word-expansion subprogram into packed u32 words.
pub fn encode_word_program(program: &WordProgram) -> Result<Vec<u32>, IrError> {
    let mut words = Vec::new();
    for op in &program.ops {
        encode_word_op(&mut words, op)?;
    }
    Ok(words)
}

fn encode_word_op(buf: &mut Vec<u32>, op: &WordProgramOp) -> Result<(), IrError> {
    match op {
        WordProgramOp::PushLiteral(id) => {
            emit_single_operand(buf, word_op::PUSH_LITERAL, id.value());
        }
        WordProgramOp::ExpandTilde(id) => {
            emit_single_operand(buf, word_op::EXPAND_TILDE, id.value());
        }
        WordProgramOp::ExpandParameter(id) => {
            emit_single_operand(buf, word_op::EXPAND_PARAMETER, id.value());
        }
        WordProgramOp::ExpandCommandSubstitution(id) => {
            emit_single_operand(buf, word_op::EXPAND_COMMAND_SUBSTITUTION, id.value());
        }
        WordProgramOp::ExpandArithmetic(id) => {
            emit_single_operand(buf, word_op::EXPAND_ARITHMETIC, id.value());
        }
        WordProgramOp::FieldSplit => buf.push(pack_word(word_op::FIELD_SPLIT, 0)),
        WordProgramOp::Glob => buf.push(pack_word(word_op::GLOB, 0)),
        WordProgramOp::QuoteRemoval => buf.push(pack_word(word_op::QUOTE_REMOVAL, 0)),
    }
    Ok(())
}

// ---------------------------------------------------------------------------
// Redirect program encoding
// ---------------------------------------------------------------------------

/// Encodes a redirect subprogram into packed u32 words.
pub fn encode_redirect_program(program: &RedirectProgram) -> Result<Vec<u32>, IrError> {
    let mut words = Vec::new();
    for op in &program.ops {
        encode_redirect_op(&mut words, op)?;
    }
    Ok(words)
}

fn encode_redirect_op(buf: &mut Vec<u32>, op: &RedirectProgramOp) -> Result<(), IrError> {
    match op {
        RedirectProgramOp::Open { fd, target, mode } => {
            // operand_24 = fd(16b) | mode(8b)
            let operand_24 = ((*fd as u32) << 8) | (open_mode_to_u8(mode) as u32);
            emit_single_operand(buf, redir_op::OPEN, operand_24);
            buf.push(target.value());
        }
        RedirectProgramOp::Dup { from, to } => {
            // operand_24 = from_fd(16b), ext = to_fd
            let operand_24 = *from as u32;
            buf.push(pack_word(redir_op::DUP, operand_24));
            buf.push(*to as u32);
        }
        RedirectProgramOp::Close { fd } => {
            buf.push(pack_word(redir_op::CLOSE, *fd as u32));
        }
        RedirectProgramOp::HereDoc { fd, body, expand } => {
            // operand_24 = fd(16b) | expand(1b) | reserved(7b)
            let expand_bit = if *expand { 1u32 } else { 0u32 };
            let operand_24 = ((*fd as u32) << 8) | (expand_bit << 7);
            emit_single_operand(buf, redir_op::HERE_DOC, operand_24);
            buf.push(body.value());
        }
    }
    Ok(())
}

// ---------------------------------------------------------------------------
// Arith program encoding
// ---------------------------------------------------------------------------

/// Encodes an arithmetic subprogram into packed u32 words.
pub fn encode_arith_program(program: &ArithProgram) -> Result<Vec<u32>, IrError> {
    let mut words = Vec::new();
    for op in &program.ops {
        encode_arith_op(&mut words, op)?;
    }
    Ok(words)
}

fn encode_arith_op(buf: &mut Vec<u32>, op: &ArithProgramOp) -> Result<(), IrError> {
    match op {
        ArithProgramOp::PushLiteral(v) => emit_i64(buf, arith_op::PUSH_LITERAL, *v),
        ArithProgramOp::LoadVariable(id) => {
            emit_single_operand(buf, arith_op::LOAD_VARIABLE, id.value());
        }
        ArithProgramOp::UnaryPlus => buf.push(pack_word(arith_op::UNARY_PLUS, 0)),
        ArithProgramOp::UnaryMinus => buf.push(pack_word(arith_op::UNARY_MINUS, 0)),
        ArithProgramOp::BitwiseNot => buf.push(pack_word(arith_op::BITWISE_NOT, 0)),
        ArithProgramOp::LogicalNot => buf.push(pack_word(arith_op::LOGICAL_NOT, 0)),
        ArithProgramOp::Add => buf.push(pack_word(arith_op::ADD, 0)),
        ArithProgramOp::Subtract => buf.push(pack_word(arith_op::SUBTRACT, 0)),
        ArithProgramOp::Multiply => buf.push(pack_word(arith_op::MULTIPLY, 0)),
        ArithProgramOp::Divide => buf.push(pack_word(arith_op::DIVIDE, 0)),
        ArithProgramOp::Modulo => buf.push(pack_word(arith_op::MODULO, 0)),
        ArithProgramOp::BitwiseAnd => buf.push(pack_word(arith_op::BITWISE_AND, 0)),
        ArithProgramOp::BitwiseOr => buf.push(pack_word(arith_op::BITWISE_OR, 0)),
        ArithProgramOp::BitwiseXor => buf.push(pack_word(arith_op::BITWISE_XOR, 0)),
        ArithProgramOp::ShiftLeft => buf.push(pack_word(arith_op::SHIFT_LEFT, 0)),
        ArithProgramOp::ShiftRight => buf.push(pack_word(arith_op::SHIFT_RIGHT, 0)),
        ArithProgramOp::LessThan => buf.push(pack_word(arith_op::LESS_THAN, 0)),
        ArithProgramOp::GreaterThan => buf.push(pack_word(arith_op::GREATER_THAN, 0)),
        ArithProgramOp::LessEqual => buf.push(pack_word(arith_op::LESS_EQUAL, 0)),
        ArithProgramOp::GreaterEqual => buf.push(pack_word(arith_op::GREATER_EQUAL, 0)),
        ArithProgramOp::Equal => buf.push(pack_word(arith_op::EQUAL, 0)),
        ArithProgramOp::NotEqual => buf.push(pack_word(arith_op::NOT_EQUAL, 0)),
        ArithProgramOp::LogicalAnd => buf.push(pack_word(arith_op::LOGICAL_AND, 0)),
        ArithProgramOp::LogicalOr => buf.push(pack_word(arith_op::LOGICAL_OR, 0)),
        ArithProgramOp::Assign(id) => emit_single_operand(buf, arith_op::ASSIGN, id.value()),
        ArithProgramOp::CompoundAssign(id, op_kind) => {
            emit_two_operands(
                buf,
                arith_op::COMPOUND_ASSIGN,
                id.value(),
                arith_compound_op_to_u32(op_kind),
            );
        }
        ArithProgramOp::PreIncrement(id) => {
            emit_single_operand(buf, arith_op::PRE_INCREMENT, id.value());
        }
        ArithProgramOp::PreDecrement(id) => {
            emit_single_operand(buf, arith_op::PRE_DECREMENT, id.value());
        }
        ArithProgramOp::PostIncrement(id) => {
            emit_single_operand(buf, arith_op::POST_INCREMENT, id.value());
        }
        ArithProgramOp::PostDecrement(id) => {
            emit_single_operand(buf, arith_op::POST_DECREMENT, id.value());
        }
        ArithProgramOp::JmpIfZero(target) => {
            emit_single_operand(buf, arith_op::JMP_IF_ZERO, *target);
        }
        ArithProgramOp::JmpIfNonZero(target) => {
            emit_single_operand(buf, arith_op::JMP_IF_NON_ZERO, *target);
        }
        ArithProgramOp::Jmp(target) => emit_single_operand(buf, arith_op::JMP, *target),
        ArithProgramOp::Pop => buf.push(pack_word(arith_op::POP, 0)),
    }
    Ok(())
}

// ---------------------------------------------------------------------------
// Module encoding (public entry point)
// ---------------------------------------------------------------------------

/// Encodes a typed IR module to a packed bytecode module.
pub fn encode_module(module: &IrModule) -> Result<EncodedModule, IrError> {
    let mut code_objects = Vec::with_capacity(module.code_objects.len());
    for co in &module.code_objects {
        code_objects.push(encode_code_object(co)?);
    }

    let mut word_programs = Vec::with_capacity(module.word_programs.len());
    for wp in &module.word_programs {
        word_programs.push(encode_word_program(wp)?);
    }

    let mut redirect_programs = Vec::with_capacity(module.redirect_programs.len());
    for rp in &module.redirect_programs {
        redirect_programs.push(encode_redirect_program(rp)?);
    }

    let mut arith_programs = Vec::with_capacity(module.arith_programs.len());
    for ap in &module.arith_programs {
        arith_programs.push(encode_arith_program(ap)?);
    }

    Ok(EncodedModule {
        string_pool: module.string_pool.clone(),
        symbol_pool: module.symbol_pool.clone(),
        const_pool: module.const_pool.clone(),
        code_objects,
        word_programs,
        redirect_programs,
        arith_programs,
    })
}

// ===========================================================================
// Decode utility
// ===========================================================================

/// One decoded instruction for debug dumps.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DecodedInstruction {
    /// Word offset in the encoded stream.
    pub offset: u32,
    /// Number of u32 words consumed.
    pub word_count: u32,
    /// Human-readable disassembly.
    pub display: String,
}

// ---------------------------------------------------------------------------
// Decode helpers
// ---------------------------------------------------------------------------

fn read_u32(words: &[u32], pos: usize) -> Result<u32, IrError> {
    words.get(pos).copied().ok_or_else(|| {
        IrError::encoding_overflow(
            None,
            "unexpected end of encoded stream",
            format!("expected word at offset {pos}"),
        )
    })
}

fn decode_opcode_operand(word: u32) -> (u8, u32) {
    let opcode = (word >> 24) as u8;
    let operand = word & 0x00FF_FFFF;
    (opcode, operand)
}

/// Reads a single operand that may be wide. Returns (value, words_consumed).
fn read_single_operand(words: &[u32], pos: usize, operand_24: u32) -> Result<(u32, u32), IrError> {
    if operand_24 >= WIDE_SENTINEL {
        let full = read_u32(words, pos + 1)?;
        Ok((full, 2))
    } else {
        Ok((operand_24, 1))
    }
}

/// Reads an i64 from two extension words. Returns (value, 3).
fn read_i64(words: &[u32], pos: usize) -> Result<(i64, u32), IrError> {
    let hi = read_u32(words, pos + 1)?;
    let lo = read_u32(words, pos + 2)?;
    let bits = ((hi as u64) << 32) | (lo as u64);
    Ok((bits as i64, 3))
}

/// Reads two operands. The first may be wide, the second is always an extension word.
fn read_two_operands(
    words: &[u32],
    pos: usize,
    operand_24: u32,
) -> Result<(u32, u32, u32), IrError> {
    let (val1, consumed) = read_single_operand(words, pos, operand_24)?;
    let val2 = read_u32(words, pos + consumed as usize)?;
    Ok((val1, val2, consumed + 1))
}

// ---------------------------------------------------------------------------
// VM instruction decode
// ---------------------------------------------------------------------------

fn vm_opcode_name(opcode: u8) -> Option<&'static str> {
    Some(match opcode {
        vm_op::NOP => "Nop",
        vm_op::PUSH_CONST => "PushConst",
        vm_op::PUSH_INT => "PushInt",
        vm_op::PUSH_STRING => "PushString",
        vm_op::PUSH_SYMBOL => "PushSymbol",
        vm_op::DROP => "Drop",
        vm_op::DUP => "Dup",
        vm_op::LOCAL_GET => "LocalGet",
        vm_op::LOCAL_SET => "LocalSet",
        vm_op::JMP => "Jmp",
        vm_op::JMP_IF_ZERO => "JmpIfZero",
        vm_op::JMP_IF_NON_ZERO => "JmpIfNonZero",
        vm_op::CALL => "Call",
        vm_op::RET => "Ret",
        vm_op::BEGIN_SIMPLE => "BeginSimple",
        vm_op::ADD_ARG => "AddArg",
        vm_op::ADD_ASSIGN => "AddAssign",
        vm_op::ADD_REDIR => "AddRedir",
        vm_op::END_SIMPLE => "EndSimple",
        vm_op::EXEC_SIMPLE => "ExecSimple",
        vm_op::NEGATE_STATUS => "NegateStatus",
        vm_op::EXEC_SUBSHELL => "ExecSubshell",
        vm_op::DEFINE_FUNCTION => "DefineFunction",
        vm_op::EXEC_BACKGROUND => "ExecBackground",
        vm_op::BEGIN_PIPELINE => "BeginPipeline",
        vm_op::ADD_PIPELINE_STAGE => "AddPipelineStage",
        vm_op::EXEC_PIPELINE => "ExecPipeline",
        vm_op::FOR_SETUP => "ForSetup",
        vm_op::FOR_ADD_WORD => "ForAddWord",
        vm_op::FOR_NEXT => "ForNext",
        vm_op::CASE_SET_SUBJECT => "CaseSetSubject",
        vm_op::CASE_TEST_PATTERN => "CaseTestPattern",
        vm_op::CASE_CLEAR => "CaseClear",
        vm_op::PUSH_REDIRECT_SCOPE => "PushRedirectScope",
        vm_op::POP_REDIRECT_SCOPE => "PopRedirectScope",
        _ => return None,
    })
}

/// Decodes a packed VM instruction stream into human-readable form.
pub fn decode_code_object_words(words: &[u32]) -> Result<Vec<DecodedInstruction>, IrError> {
    let mut result = Vec::new();
    let mut pos = 0;

    while pos < words.len() {
        let offset = pos as u32;
        let word = read_u32(words, pos)?;
        let (opcode, operand_24) = decode_opcode_operand(word);

        let name = vm_opcode_name(opcode).ok_or_else(|| {
            IrError::encoding_overflow(
                None,
                "unknown VM opcode during decode",
                format!("opcode 0x{opcode:02X} at offset {offset}"),
            )
        })?;

        let (display, consumed) = match opcode {
            // Zero-operand instructions
            vm_op::NOP | vm_op::DROP | vm_op::DUP | vm_op::RET | vm_op::BEGIN_SIMPLE
            | vm_op::END_SIMPLE | vm_op::NEGATE_STATUS | vm_op::EXEC_PIPELINE
            | vm_op::FOR_NEXT | vm_op::CASE_CLEAR | vm_op::PUSH_REDIRECT_SCOPE
            | vm_op::POP_REDIRECT_SCOPE => (name.to_string(), 1),

            // PushInt: always 3 words
            vm_op::PUSH_INT => {
                let (val, consumed) = read_i64(words, pos)?;
                (format!("{name} {val}"), consumed)
            }

            // Two-operand instructions
            vm_op::ADD_ASSIGN => {
                let (v1, v2, consumed) = read_two_operands(words, pos, operand_24)?;
                (format!("{name} sym={v1} wp={v2}"), consumed)
            }
            vm_op::DEFINE_FUNCTION => {
                let (v1, v2, consumed) = read_two_operands(words, pos, operand_24)?;
                (format!("{name} name={v1} body={v2}"), consumed)
            }
            vm_op::FOR_SETUP => {
                let (v1, v2, consumed) = read_two_operands(words, pos, operand_24)?;
                (format!("{name} var={v1} word_count={v2}"), consumed)
            }

            // ExecSimple with hint
            vm_op::EXEC_SIMPLE => {
                let hint_name = match operand_24 {
                    0 => "NoCommand",
                    1 => "SpecialBuiltin",
                    2 => "Standard",
                    _ => "Unknown",
                };
                (format!("{name} {hint_name}"), 1)
            }

            // Single-operand instructions (all the rest)
            _ => {
                let (val, consumed) = read_single_operand(words, pos, operand_24)?;
                (format!("{name} {val}"), consumed)
            }
        };

        result.push(DecodedInstruction {
            offset,
            word_count: consumed,
            display,
        });
        pos += consumed as usize;
    }

    Ok(result)
}

// ---------------------------------------------------------------------------
// Word program decode
// ---------------------------------------------------------------------------

fn word_opcode_name(opcode: u8) -> Option<&'static str> {
    Some(match opcode {
        word_op::PUSH_LITERAL => "PushLiteral",
        word_op::EXPAND_TILDE => "ExpandTilde",
        word_op::EXPAND_PARAMETER => "ExpandParameter",
        word_op::EXPAND_COMMAND_SUBSTITUTION => "ExpandCommandSubstitution",
        word_op::EXPAND_ARITHMETIC => "ExpandArithmetic",
        word_op::FIELD_SPLIT => "FieldSplit",
        word_op::GLOB => "Glob",
        word_op::QUOTE_REMOVAL => "QuoteRemoval",
        _ => return None,
    })
}

/// Decodes a packed word-program stream into human-readable form.
pub fn decode_word_program_words(words: &[u32]) -> Result<Vec<DecodedInstruction>, IrError> {
    let mut result = Vec::new();
    let mut pos = 0;

    while pos < words.len() {
        let offset = pos as u32;
        let word = read_u32(words, pos)?;
        let (opcode, operand_24) = decode_opcode_operand(word);

        let name = word_opcode_name(opcode).ok_or_else(|| {
            IrError::encoding_overflow(
                None,
                "unknown word-program opcode during decode",
                format!("opcode 0x{opcode:02X} at offset {offset}"),
            )
        })?;

        let (display, consumed) = match opcode {
            word_op::FIELD_SPLIT | word_op::GLOB | word_op::QUOTE_REMOVAL => {
                (name.to_string(), 1)
            }
            _ => {
                let (val, consumed) = read_single_operand(words, pos, operand_24)?;
                (format!("{name} {val}"), consumed)
            }
        };

        result.push(DecodedInstruction {
            offset,
            word_count: consumed,
            display,
        });
        pos += consumed as usize;
    }

    Ok(result)
}

// ---------------------------------------------------------------------------
// Redirect program decode
// ---------------------------------------------------------------------------

fn redir_opcode_name(opcode: u8) -> Option<&'static str> {
    Some(match opcode {
        redir_op::OPEN => "Open",
        redir_op::DUP => "Dup",
        redir_op::CLOSE => "Close",
        redir_op::HERE_DOC => "HereDoc",
        _ => return None,
    })
}

fn open_mode_name(val: u8) -> &'static str {
    match val {
        0 => "ReadOnly",
        1 => "WriteCreate",
        2 => "Append",
        3 => "ReadWrite",
        4 => "Clobber",
        _ => "Unknown",
    }
}

/// Decodes a packed redirect-program stream into human-readable form.
pub fn decode_redirect_program_words(words: &[u32]) -> Result<Vec<DecodedInstruction>, IrError> {
    let mut result = Vec::new();
    let mut pos = 0;

    while pos < words.len() {
        let offset = pos as u32;
        let word = read_u32(words, pos)?;
        let (opcode, operand_24) = decode_opcode_operand(word);

        let name = redir_opcode_name(opcode).ok_or_else(|| {
            IrError::encoding_overflow(
                None,
                "unknown redirect-program opcode during decode",
                format!("opcode 0x{opcode:02X} at offset {offset}"),
            )
        })?;

        let (display, consumed) = match opcode {
            redir_op::OPEN => {
                // Check if wide
                let (actual_operand, head_consumed) =
                    read_single_operand(words, pos, operand_24)?;
                let fd = (actual_operand >> 8) as u16;
                let mode = (actual_operand & 0xFF) as u8;
                let target = read_u32(words, pos + head_consumed as usize)?;
                (
                    format!("{name} fd={fd} mode={} target={target}", open_mode_name(mode)),
                    head_consumed + 1,
                )
            }
            redir_op::DUP => {
                let from = operand_24 as u16;
                let to = read_u32(words, pos + 1)?;
                (format!("{name} from={from} to={to}"), 2)
            }
            redir_op::CLOSE => {
                let fd = operand_24 as u16;
                (format!("{name} fd={fd}"), 1)
            }
            redir_op::HERE_DOC => {
                let (actual_operand, head_consumed) =
                    read_single_operand(words, pos, operand_24)?;
                let fd = (actual_operand >> 8) as u16;
                let expand = (actual_operand >> 7) & 1 != 0;
                let body = read_u32(words, pos + head_consumed as usize)?;
                (
                    format!("{name} fd={fd} expand={expand} body={body}"),
                    head_consumed + 1,
                )
            }
            _ => unreachable!(),
        };

        result.push(DecodedInstruction {
            offset,
            word_count: consumed,
            display,
        });
        pos += consumed as usize;
    }

    Ok(result)
}

// ---------------------------------------------------------------------------
// Arith program decode
// ---------------------------------------------------------------------------

fn arith_opcode_name(opcode: u8) -> Option<&'static str> {
    Some(match opcode {
        arith_op::PUSH_LITERAL => "PushLiteral",
        arith_op::LOAD_VARIABLE => "LoadVariable",
        arith_op::UNARY_PLUS => "UnaryPlus",
        arith_op::UNARY_MINUS => "UnaryMinus",
        arith_op::BITWISE_NOT => "BitwiseNot",
        arith_op::LOGICAL_NOT => "LogicalNot",
        arith_op::ADD => "Add",
        arith_op::SUBTRACT => "Subtract",
        arith_op::MULTIPLY => "Multiply",
        arith_op::DIVIDE => "Divide",
        arith_op::MODULO => "Modulo",
        arith_op::BITWISE_AND => "BitwiseAnd",
        arith_op::BITWISE_OR => "BitwiseOr",
        arith_op::BITWISE_XOR => "BitwiseXor",
        arith_op::SHIFT_LEFT => "ShiftLeft",
        arith_op::SHIFT_RIGHT => "ShiftRight",
        arith_op::LESS_THAN => "LessThan",
        arith_op::GREATER_THAN => "GreaterThan",
        arith_op::LESS_EQUAL => "LessEqual",
        arith_op::GREATER_EQUAL => "GreaterEqual",
        arith_op::EQUAL => "Equal",
        arith_op::NOT_EQUAL => "NotEqual",
        arith_op::LOGICAL_AND => "LogicalAnd",
        arith_op::LOGICAL_OR => "LogicalOr",
        arith_op::ASSIGN => "Assign",
        arith_op::COMPOUND_ASSIGN => "CompoundAssign",
        arith_op::PRE_INCREMENT => "PreIncrement",
        arith_op::PRE_DECREMENT => "PreDecrement",
        arith_op::POST_INCREMENT => "PostIncrement",
        arith_op::POST_DECREMENT => "PostDecrement",
        arith_op::JMP_IF_ZERO => "JmpIfZero",
        arith_op::JMP_IF_NON_ZERO => "JmpIfNonZero",
        arith_op::JMP => "Jmp",
        arith_op::POP => "Pop",
        _ => return None,
    })
}

fn arith_compound_op_name(val: u32) -> &'static str {
    match val {
        0 => "Add",
        1 => "Subtract",
        2 => "Multiply",
        3 => "Divide",
        4 => "Modulo",
        5 => "ShiftLeft",
        6 => "ShiftRight",
        7 => "BitwiseAnd",
        8 => "BitwiseOr",
        9 => "BitwiseXor",
        _ => "Unknown",
    }
}

/// Decodes a packed arith-program stream into human-readable form.
pub fn decode_arith_program_words(words: &[u32]) -> Result<Vec<DecodedInstruction>, IrError> {
    let mut result = Vec::new();
    let mut pos = 0;

    while pos < words.len() {
        let offset = pos as u32;
        let word = read_u32(words, pos)?;
        let (opcode, operand_24) = decode_opcode_operand(word);

        let name = arith_opcode_name(opcode).ok_or_else(|| {
            IrError::encoding_overflow(
                None,
                "unknown arith-program opcode during decode",
                format!("opcode 0x{opcode:02X} at offset {offset}"),
            )
        })?;

        let (display, consumed) = match opcode {
            // Zero-operand
            arith_op::UNARY_PLUS
            | arith_op::UNARY_MINUS
            | arith_op::BITWISE_NOT
            | arith_op::LOGICAL_NOT
            | arith_op::ADD
            | arith_op::SUBTRACT
            | arith_op::MULTIPLY
            | arith_op::DIVIDE
            | arith_op::MODULO
            | arith_op::BITWISE_AND
            | arith_op::BITWISE_OR
            | arith_op::BITWISE_XOR
            | arith_op::SHIFT_LEFT
            | arith_op::SHIFT_RIGHT
            | arith_op::LESS_THAN
            | arith_op::GREATER_THAN
            | arith_op::LESS_EQUAL
            | arith_op::GREATER_EQUAL
            | arith_op::EQUAL
            | arith_op::NOT_EQUAL
            | arith_op::LOGICAL_AND
            | arith_op::LOGICAL_OR
            | arith_op::POP => (name.to_string(), 1),

            // PushLiteral(i64): always 3 words
            arith_op::PUSH_LITERAL => {
                let (val, consumed) = read_i64(words, pos)?;
                (format!("{name} {val}"), consumed)
            }

            // CompoundAssign: two operands
            arith_op::COMPOUND_ASSIGN => {
                let (sym, op_kind, consumed) = read_two_operands(words, pos, operand_24)?;
                (
                    format!("{name} sym={sym} op={}", arith_compound_op_name(op_kind)),
                    consumed,
                )
            }

            // Single-operand (all remaining)
            _ => {
                let (val, consumed) = read_single_operand(words, pos, operand_24)?;
                (format!("{name} {val}"), consumed)
            }
        };

        result.push(DecodedInstruction {
            offset,
            word_count: consumed,
            display,
        });
        pos += consumed as usize;
    }

    Ok(result)
}
