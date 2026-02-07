use kish::ir::{
    ArithCompoundOp, ArithProgramOp, BranchTarget, CommandDispatchHint, Instruction, IrModule,
    IrOptions, LoweringContext, OpenMode, RedirectProgramOp, SourceMapEntry, WordProgramOp,
    decode_arith_program_words, decode_code_object_words, decode_redirect_program_words,
    decode_word_program_words, encode_module,
};
use kish::ir::{
    ArithProgramId, CodeObjectId, ConstId, LocalId, RedirectProgramId, StringId, SymbolId,
    WordProgramId,
};
use kish::ir::{ArithProgram, CodeObject, RedirectProgram, WordProgram};
use kish::lexer::{Lexer, LexerMode, SourceId};
use kish::parser::{ParseOptions, ParseStep, Parser, TokenStream};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn make_module_with_code(instructions: Vec<Instruction>) -> IrModule {
    IrModule {
        code_objects: vec![CodeObject {
            id: CodeObjectId::new(0),
            instructions,
            locals_count: 0,
            max_stack_depth: 0,
        }],
        ..Default::default()
    }
}

fn make_module_with_word_program(ops: Vec<WordProgramOp>) -> IrModule {
    IrModule {
        word_programs: vec![WordProgram {
            id: WordProgramId::new(0),
            ops,
        }],
        ..Default::default()
    }
}

fn make_module_with_redirect_program(ops: Vec<RedirectProgramOp>) -> IrModule {
    IrModule {
        redirect_programs: vec![RedirectProgram {
            id: RedirectProgramId::new(0),
            ops,
        }],
        ..Default::default()
    }
}

fn make_module_with_arith_program(ops: Vec<ArithProgramOp>) -> IrModule {
    IrModule {
        arith_programs: vec![ArithProgram {
            id: ArithProgramId::new(0),
            ops,
        }],
        ..Default::default()
    }
}

fn encode_instructions(instructions: Vec<Instruction>) -> Vec<u32> {
    let module = make_module_with_code(instructions);
    let encoded = encode_module(&module).expect("encoding should succeed");
    encoded.code_objects[0].words.clone()
}

fn encode_word_ops(ops: Vec<WordProgramOp>) -> Vec<u32> {
    let module = make_module_with_word_program(ops);
    let encoded = encode_module(&module).expect("encoding should succeed");
    encoded.word_programs[0].clone()
}

fn encode_redirect_ops(ops: Vec<RedirectProgramOp>) -> Vec<u32> {
    let module = make_module_with_redirect_program(ops);
    let encoded = encode_module(&module).expect("encoding should succeed");
    encoded.redirect_programs[0].clone()
}

fn encode_arith_ops(ops: Vec<ArithProgramOp>) -> Vec<u32> {
    let module = make_module_with_arith_program(ops);
    let encoded = encode_module(&module).expect("encoding should succeed");
    encoded.arith_programs[0].clone()
}

fn pack(opcode: u8, operand_24: u32) -> u32 {
    ((opcode as u32) << 24) | (operand_24 & 0x00FF_FFFF)
}

const WIDE: u32 = 0x00FF_FFFF;

// ===========================================================================
// 1. Empty module encoding
// ===========================================================================

#[test]
fn empty_module_encodes_to_empty() {
    let module = IrModule::default();
    let encoded = encode_module(&module).expect("empty module should encode");
    assert!(encoded.code_objects.is_empty());
    assert!(encoded.word_programs.is_empty());
    assert!(encoded.redirect_programs.is_empty());
    assert!(encoded.arith_programs.is_empty());
    assert!(encoded.string_pool.is_empty());
    assert!(encoded.symbol_pool.is_empty());
    assert!(encoded.const_pool.is_empty());
}

// ===========================================================================
// 2. Zero-operand VM instructions
// ===========================================================================

#[test]
fn encode_zero_operand_instructions() {
    let instructions = vec![
        Instruction::Nop,
        Instruction::Drop,
        Instruction::Dup,
        Instruction::Ret,
        Instruction::BeginSimple,
        Instruction::EndSimple,
        Instruction::NegateStatus,
        Instruction::ExecPipeline,
        Instruction::ForNext,
        Instruction::CaseClear,
        Instruction::PushRedirectScope,
        Instruction::PopRedirectScope,
    ];
    let words = encode_instructions(instructions);
    assert_eq!(words.len(), 12);
    assert_eq!(words[0], pack(0x00, 0)); // Nop
    assert_eq!(words[1], pack(0x05, 0)); // Drop
    assert_eq!(words[2], pack(0x06, 0)); // Dup
    assert_eq!(words[3], pack(0x0D, 0)); // Ret
    assert_eq!(words[4], pack(0x0E, 0)); // BeginSimple
    assert_eq!(words[5], pack(0x12, 0)); // EndSimple
    assert_eq!(words[6], pack(0x14, 0)); // NegateStatus
    assert_eq!(words[7], pack(0x1A, 0)); // ExecPipeline
    assert_eq!(words[8], pack(0x1D, 0)); // ForNext
    assert_eq!(words[9], pack(0x20, 0)); // CaseClear
    assert_eq!(words[10], pack(0x21, 0)); // PushRedirectScope
    assert_eq!(words[11], pack(0x22, 0)); // PopRedirectScope
}

// ===========================================================================
// 3. Single-operand instructions with small IDs
// ===========================================================================

#[test]
fn encode_single_operand_small() {
    let words = encode_instructions(vec![Instruction::PushConst(ConstId::new(42))]);
    assert_eq!(words, vec![pack(0x01, 42)]);
}

#[test]
fn encode_push_string_small() {
    let words = encode_instructions(vec![Instruction::PushString(StringId::new(7))]);
    assert_eq!(words, vec![pack(0x03, 7)]);
}

#[test]
fn encode_push_symbol_small() {
    let words = encode_instructions(vec![Instruction::PushSymbol(SymbolId::new(100))]);
    assert_eq!(words, vec![pack(0x04, 100)]);
}

#[test]
fn encode_local_get_set() {
    let words = encode_instructions(vec![
        Instruction::LocalGet(LocalId::new(3)),
        Instruction::LocalSet(LocalId::new(5)),
    ]);
    assert_eq!(words, vec![pack(0x07, 3), pack(0x08, 5)]);
}

#[test]
fn encode_branch_small() {
    let words = encode_instructions(vec![
        Instruction::Jmp(BranchTarget::new(10)),
        Instruction::JmpIfZero(BranchTarget::new(20)),
        Instruction::JmpIfNonZero(BranchTarget::new(30)),
    ]);
    assert_eq!(words, vec![pack(0x09, 10), pack(0x0A, 20), pack(0x0B, 30)]);
}

#[test]
fn encode_call_small() {
    let words = encode_instructions(vec![Instruction::Call(CodeObjectId::new(1))]);
    assert_eq!(words, vec![pack(0x0C, 1)]);
}

#[test]
fn encode_add_arg_small() {
    let words = encode_instructions(vec![Instruction::AddArg(WordProgramId::new(2))]);
    assert_eq!(words, vec![pack(0x0F, 2)]);
}

#[test]
fn encode_add_redir_small() {
    let words = encode_instructions(vec![Instruction::AddRedir(RedirectProgramId::new(4))]);
    assert_eq!(words, vec![pack(0x11, 4)]);
}

#[test]
fn encode_exec_subshell_small() {
    let words = encode_instructions(vec![Instruction::ExecSubshell(CodeObjectId::new(3))]);
    assert_eq!(words, vec![pack(0x15, 3)]);
}

#[test]
fn encode_exec_background_small() {
    let words = encode_instructions(vec![Instruction::ExecBackground(CodeObjectId::new(5))]);
    assert_eq!(words, vec![pack(0x17, 5)]);
}

#[test]
fn encode_begin_pipeline_small() {
    let words = encode_instructions(vec![Instruction::BeginPipeline(3)]);
    assert_eq!(words, vec![pack(0x18, 3)]);
}

#[test]
fn encode_add_pipeline_stage_small() {
    let words = encode_instructions(vec![Instruction::AddPipelineStage(CodeObjectId::new(2))]);
    assert_eq!(words, vec![pack(0x19, 2)]);
}

#[test]
fn encode_for_add_word_small() {
    let words = encode_instructions(vec![Instruction::ForAddWord(WordProgramId::new(1))]);
    assert_eq!(words, vec![pack(0x1C, 1)]);
}

#[test]
fn encode_case_set_subject_small() {
    let words = encode_instructions(vec![Instruction::CaseSetSubject(WordProgramId::new(0))]);
    assert_eq!(words, vec![pack(0x1E, 0)]);
}

#[test]
fn encode_case_test_pattern_small() {
    let words = encode_instructions(vec![Instruction::CaseTestPattern(WordProgramId::new(3))]);
    assert_eq!(words, vec![pack(0x1F, 3)]);
}

// ===========================================================================
// 4. Wide operand encoding
// ===========================================================================

#[test]
fn encode_wide_operand() {
    let big = WIDE; // 0x00FF_FFFF - triggers wide encoding
    let words = encode_instructions(vec![Instruction::PushConst(ConstId::new(big))]);
    assert_eq!(words.len(), 2);
    assert_eq!(words[0], pack(0x01, WIDE));
    assert_eq!(words[1], big);
}

#[test]
fn encode_wide_operand_large_value() {
    let big = 0x01FF_FFFF;
    let words = encode_instructions(vec![Instruction::PushString(StringId::new(big))]);
    assert_eq!(words.len(), 2);
    assert_eq!(words[0], pack(0x03, WIDE));
    assert_eq!(words[1], big);
}

#[test]
fn encode_wide_branch_target() {
    let big = WIDE + 1;
    let words = encode_instructions(vec![Instruction::Jmp(BranchTarget::new(big))]);
    assert_eq!(words.len(), 2);
    assert_eq!(words[0], pack(0x09, WIDE));
    assert_eq!(words[1], big);
}

// ===========================================================================
// 5. PushInt encoding
// ===========================================================================

#[test]
fn encode_push_int_zero() {
    let words = encode_instructions(vec![Instruction::PushInt(0)]);
    assert_eq!(words.len(), 3);
    assert_eq!(words[0], pack(0x02, 0));
    assert_eq!(words[1], 0);
    assert_eq!(words[2], 0);
}

#[test]
fn encode_push_int_positive() {
    let val: i64 = 42;
    let bits = val as u64;
    let words = encode_instructions(vec![Instruction::PushInt(val)]);
    assert_eq!(words.len(), 3);
    assert_eq!(words[0], pack(0x02, 0));
    assert_eq!(words[1], (bits >> 32) as u32);
    assert_eq!(words[2], bits as u32);
}

#[test]
fn encode_push_int_negative() {
    let val: i64 = -1;
    let bits = val as u64;
    let words = encode_instructions(vec![Instruction::PushInt(val)]);
    assert_eq!(words.len(), 3);
    assert_eq!(words[0], pack(0x02, 0));
    assert_eq!(words[1], (bits >> 32) as u32); // 0xFFFF_FFFF
    assert_eq!(words[2], bits as u32); // 0xFFFF_FFFF
}

#[test]
fn encode_push_int_i64_max() {
    let val = i64::MAX;
    let bits = val as u64;
    let words = encode_instructions(vec![Instruction::PushInt(val)]);
    assert_eq!(words.len(), 3);
    assert_eq!(words[1], (bits >> 32) as u32);
    assert_eq!(words[2], bits as u32);
}

#[test]
fn encode_push_int_i64_min() {
    let val = i64::MIN;
    let bits = val as u64;
    let words = encode_instructions(vec![Instruction::PushInt(val)]);
    assert_eq!(words.len(), 3);
    assert_eq!(words[1], (bits >> 32) as u32);
    assert_eq!(words[2], bits as u32);
}

// ===========================================================================
// 6. Two-operand instructions
// ===========================================================================

#[test]
fn encode_add_assign() {
    let words = encode_instructions(vec![Instruction::AddAssign(
        SymbolId::new(10),
        WordProgramId::new(20),
    )]);
    // 2 words: [opcode|sym_10] [wp_20]
    assert_eq!(words.len(), 2);
    assert_eq!(words[0], pack(0x10, 10));
    assert_eq!(words[1], 20);
}

#[test]
fn encode_add_assign_wide_first_operand() {
    let big = WIDE + 5;
    let words = encode_instructions(vec![Instruction::AddAssign(
        SymbolId::new(big),
        WordProgramId::new(3),
    )]);
    // 3 words: [opcode|WIDE] [big] [3]
    assert_eq!(words.len(), 3);
    assert_eq!(words[0], pack(0x10, WIDE));
    assert_eq!(words[1], big);
    assert_eq!(words[2], 3);
}

#[test]
fn encode_define_function() {
    let words = encode_instructions(vec![Instruction::DefineFunction {
        name: SymbolId::new(5),
        body: CodeObjectId::new(2),
    }]);
    assert_eq!(words.len(), 2);
    assert_eq!(words[0], pack(0x16, 5));
    assert_eq!(words[1], 2);
}

#[test]
fn encode_for_setup() {
    let words = encode_instructions(vec![Instruction::ForSetup {
        var: SymbolId::new(7),
        word_count: 3,
    }]);
    assert_eq!(words.len(), 2);
    assert_eq!(words[0], pack(0x1B, 7));
    assert_eq!(words[1], 3);
}

// ===========================================================================
// 7. ExecSimple with each CommandDispatchHint
// ===========================================================================

#[test]
fn encode_exec_simple_no_command() {
    let words =
        encode_instructions(vec![Instruction::ExecSimple(CommandDispatchHint::NoCommand)]);
    assert_eq!(words, vec![pack(0x13, 0)]);
}

#[test]
fn encode_exec_simple_special_builtin() {
    let words = encode_instructions(vec![Instruction::ExecSimple(
        CommandDispatchHint::SpecialBuiltin,
    )]);
    assert_eq!(words, vec![pack(0x13, 1)]);
}

#[test]
fn encode_exec_simple_standard() {
    let words =
        encode_instructions(vec![Instruction::ExecSimple(CommandDispatchHint::Standard)]);
    assert_eq!(words, vec![pack(0x13, 2)]);
}

// ===========================================================================
// 8. WordProgramOp encoding
// ===========================================================================

#[test]
fn encode_word_push_literal() {
    let words = encode_word_ops(vec![WordProgramOp::PushLiteral(StringId::new(5))]);
    assert_eq!(words, vec![pack(0x00, 5)]);
}

#[test]
fn encode_word_expand_tilde() {
    let words = encode_word_ops(vec![WordProgramOp::ExpandTilde(StringId::new(0))]);
    assert_eq!(words, vec![pack(0x01, 0)]);
}

#[test]
fn encode_word_expand_parameter() {
    let words = encode_word_ops(vec![WordProgramOp::ExpandParameter(SymbolId::new(3))]);
    assert_eq!(words, vec![pack(0x02, 3)]);
}

#[test]
fn encode_word_expand_command_substitution() {
    let words = encode_word_ops(vec![WordProgramOp::ExpandCommandSubstitution(
        CodeObjectId::new(1),
    )]);
    assert_eq!(words, vec![pack(0x03, 1)]);
}

#[test]
fn encode_word_expand_arithmetic() {
    let words = encode_word_ops(vec![WordProgramOp::ExpandArithmetic(ArithProgramId::new(2))]);
    assert_eq!(words, vec![pack(0x04, 2)]);
}

#[test]
fn encode_word_zero_operand_ops() {
    let words = encode_word_ops(vec![
        WordProgramOp::FieldSplit,
        WordProgramOp::Glob,
        WordProgramOp::QuoteRemoval,
    ]);
    assert_eq!(words, vec![pack(0x05, 0), pack(0x06, 0), pack(0x07, 0)]);
}

#[test]
fn encode_word_wide_operand() {
    let big = WIDE + 10;
    let words = encode_word_ops(vec![WordProgramOp::PushLiteral(StringId::new(big))]);
    assert_eq!(words.len(), 2);
    assert_eq!(words[0], pack(0x00, WIDE));
    assert_eq!(words[1], big);
}

// ===========================================================================
// 9. RedirectProgramOp encoding
// ===========================================================================

#[test]
fn encode_redirect_open() {
    let words = encode_redirect_ops(vec![RedirectProgramOp::Open {
        fd: 1,
        target: WordProgramId::new(0),
        mode: OpenMode::WriteCreate,
    }]);
    // operand_24 = (1 << 8) | 1 = 0x0101
    assert_eq!(words.len(), 2);
    assert_eq!(words[0], pack(0x00, 0x0101));
    assert_eq!(words[1], 0); // target wp id
}

#[test]
fn encode_redirect_open_readonly() {
    let words = encode_redirect_ops(vec![RedirectProgramOp::Open {
        fd: 0,
        target: WordProgramId::new(5),
        mode: OpenMode::ReadOnly,
    }]);
    assert_eq!(words[0], pack(0x00, 0x0000)); // fd=0, mode=0
    assert_eq!(words[1], 5);
}

#[test]
fn encode_redirect_open_append() {
    let words = encode_redirect_ops(vec![RedirectProgramOp::Open {
        fd: 2,
        target: WordProgramId::new(1),
        mode: OpenMode::Append,
    }]);
    // (2 << 8) | 2 = 0x0202
    assert_eq!(words[0], pack(0x00, 0x0202));
    assert_eq!(words[1], 1);
}

#[test]
fn encode_redirect_open_read_write() {
    let words = encode_redirect_ops(vec![RedirectProgramOp::Open {
        fd: 3,
        target: WordProgramId::new(0),
        mode: OpenMode::ReadWrite,
    }]);
    // (3 << 8) | 3 = 0x0303
    assert_eq!(words[0], pack(0x00, 0x0303));
}

#[test]
fn encode_redirect_open_clobber() {
    let words = encode_redirect_ops(vec![RedirectProgramOp::Open {
        fd: 1,
        target: WordProgramId::new(0),
        mode: OpenMode::Clobber,
    }]);
    // (1 << 8) | 4 = 0x0104
    assert_eq!(words[0], pack(0x00, 0x0104));
}

#[test]
fn encode_redirect_dup() {
    let words = encode_redirect_ops(vec![RedirectProgramOp::Dup { from: 1, to: 2 }]);
    assert_eq!(words.len(), 2);
    assert_eq!(words[0], pack(0x01, 1));
    assert_eq!(words[1], 2);
}

#[test]
fn encode_redirect_close() {
    let words = encode_redirect_ops(vec![RedirectProgramOp::Close { fd: 3 }]);
    assert_eq!(words, vec![pack(0x02, 3)]);
}

#[test]
fn encode_redirect_heredoc_expand() {
    let words = encode_redirect_ops(vec![RedirectProgramOp::HereDoc {
        fd: 0,
        body: StringId::new(10),
        expand: true,
    }]);
    // fd=0, expand=true => (0 << 8) | (1 << 7) = 0x80
    assert_eq!(words.len(), 2);
    assert_eq!(words[0], pack(0x03, 0x80));
    assert_eq!(words[1], 10);
}

#[test]
fn encode_redirect_heredoc_no_expand() {
    let words = encode_redirect_ops(vec![RedirectProgramOp::HereDoc {
        fd: 0,
        body: StringId::new(5),
        expand: false,
    }]);
    // fd=0, expand=false => 0x00
    assert_eq!(words[0], pack(0x03, 0x00));
    assert_eq!(words[1], 5);
}

// ===========================================================================
// 10. ArithProgramOp encoding
// ===========================================================================

#[test]
fn encode_arith_push_literal() {
    let words = encode_arith_ops(vec![ArithProgramOp::PushLiteral(42)]);
    assert_eq!(words.len(), 3);
    assert_eq!(words[0], pack(0x00, 0));
    let bits = 42u64;
    assert_eq!(words[1], (bits >> 32) as u32);
    assert_eq!(words[2], bits as u32);
}

#[test]
fn encode_arith_push_literal_negative() {
    let val: i64 = -100;
    let bits = val as u64;
    let words = encode_arith_ops(vec![ArithProgramOp::PushLiteral(val)]);
    assert_eq!(words.len(), 3);
    assert_eq!(words[1], (bits >> 32) as u32);
    assert_eq!(words[2], bits as u32);
}

#[test]
fn encode_arith_load_variable() {
    let words = encode_arith_ops(vec![ArithProgramOp::LoadVariable(SymbolId::new(3))]);
    assert_eq!(words, vec![pack(0x01, 3)]);
}

#[test]
fn encode_arith_zero_operand_ops() {
    let ops = vec![
        ArithProgramOp::UnaryPlus,
        ArithProgramOp::UnaryMinus,
        ArithProgramOp::BitwiseNot,
        ArithProgramOp::LogicalNot,
        ArithProgramOp::Add,
        ArithProgramOp::Subtract,
        ArithProgramOp::Multiply,
        ArithProgramOp::Divide,
        ArithProgramOp::Modulo,
        ArithProgramOp::BitwiseAnd,
        ArithProgramOp::BitwiseOr,
        ArithProgramOp::BitwiseXor,
        ArithProgramOp::ShiftLeft,
        ArithProgramOp::ShiftRight,
        ArithProgramOp::LessThan,
        ArithProgramOp::GreaterThan,
        ArithProgramOp::LessEqual,
        ArithProgramOp::GreaterEqual,
        ArithProgramOp::Equal,
        ArithProgramOp::NotEqual,
        ArithProgramOp::LogicalAnd,
        ArithProgramOp::LogicalOr,
        ArithProgramOp::Pop,
    ];
    let expected_opcodes: Vec<u8> = vec![
        0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F,
        0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x21,
    ];
    let words = encode_arith_ops(ops);
    assert_eq!(words.len(), expected_opcodes.len());
    for (i, &expected_opcode) in expected_opcodes.iter().enumerate() {
        assert_eq!(words[i], pack(expected_opcode, 0), "mismatch at index {i}");
    }
}

#[test]
fn encode_arith_assign() {
    let words = encode_arith_ops(vec![ArithProgramOp::Assign(SymbolId::new(7))]);
    assert_eq!(words, vec![pack(0x18, 7)]);
}

#[test]
fn encode_arith_compound_assign() {
    let words = encode_arith_ops(vec![ArithProgramOp::CompoundAssign(
        SymbolId::new(4),
        ArithCompoundOp::Add,
    )]);
    // 2 words: [opcode|sym_4] [op_kind=0]
    assert_eq!(words.len(), 2);
    assert_eq!(words[0], pack(0x19, 4));
    assert_eq!(words[1], 0); // Add = 0
}

#[test]
fn encode_arith_compound_assign_all_ops() {
    let all_ops = vec![
        (ArithCompoundOp::Add, 0u32),
        (ArithCompoundOp::Subtract, 1),
        (ArithCompoundOp::Multiply, 2),
        (ArithCompoundOp::Divide, 3),
        (ArithCompoundOp::Modulo, 4),
        (ArithCompoundOp::ShiftLeft, 5),
        (ArithCompoundOp::ShiftRight, 6),
        (ArithCompoundOp::BitwiseAnd, 7),
        (ArithCompoundOp::BitwiseOr, 8),
        (ArithCompoundOp::BitwiseXor, 9),
    ];
    for (op, expected_kind) in all_ops {
        let words = encode_arith_ops(vec![ArithProgramOp::CompoundAssign(
            SymbolId::new(0),
            op.clone(),
        )]);
        assert_eq!(words[1], expected_kind, "mismatch for {op:?}");
    }
}

#[test]
fn encode_arith_inc_dec() {
    let words = encode_arith_ops(vec![
        ArithProgramOp::PreIncrement(SymbolId::new(1)),
        ArithProgramOp::PreDecrement(SymbolId::new(2)),
        ArithProgramOp::PostIncrement(SymbolId::new(3)),
        ArithProgramOp::PostDecrement(SymbolId::new(4)),
    ]);
    assert_eq!(
        words,
        vec![pack(0x1A, 1), pack(0x1B, 2), pack(0x1C, 3), pack(0x1D, 4)]
    );
}

#[test]
fn encode_arith_jumps() {
    let words = encode_arith_ops(vec![
        ArithProgramOp::JmpIfZero(10),
        ArithProgramOp::JmpIfNonZero(20),
        ArithProgramOp::Jmp(30),
    ]);
    assert_eq!(words, vec![pack(0x1E, 10), pack(0x1F, 20), pack(0x20, 30)]);
}

#[test]
fn encode_arith_wide_operand() {
    let big = WIDE + 100;
    let words = encode_arith_ops(vec![ArithProgramOp::LoadVariable(SymbolId::new(big))]);
    assert_eq!(words.len(), 2);
    assert_eq!(words[0], pack(0x01, WIDE));
    assert_eq!(words[1], big);
}

// ===========================================================================
// 11. Encode-then-decode round-trip tests
// ===========================================================================

#[test]
fn round_trip_vm_zero_operand() {
    let instructions = vec![
        Instruction::Nop,
        Instruction::Drop,
        Instruction::Dup,
        Instruction::Ret,
    ];
    let words = encode_instructions(instructions);
    let decoded = decode_code_object_words(&words).expect("decode should succeed");
    assert_eq!(decoded.len(), 4);
    assert_eq!(decoded[0].display, "Nop");
    assert_eq!(decoded[1].display, "Drop");
    assert_eq!(decoded[2].display, "Dup");
    assert_eq!(decoded[3].display, "Ret");
}

#[test]
fn round_trip_vm_single_operand() {
    let words = encode_instructions(vec![
        Instruction::PushConst(ConstId::new(42)),
        Instruction::Jmp(BranchTarget::new(100)),
    ]);
    let decoded = decode_code_object_words(&words).expect("decode should succeed");
    assert_eq!(decoded.len(), 2);
    assert_eq!(decoded[0].display, "PushConst 42");
    assert_eq!(decoded[1].display, "Jmp 100");
}

#[test]
fn round_trip_vm_push_int() {
    let words = encode_instructions(vec![Instruction::PushInt(-42)]);
    let decoded = decode_code_object_words(&words).expect("decode should succeed");
    assert_eq!(decoded.len(), 1);
    assert_eq!(decoded[0].display, "PushInt -42");
    assert_eq!(decoded[0].word_count, 3);
}

#[test]
fn round_trip_vm_wide_operand() {
    let big = WIDE + 50;
    let words = encode_instructions(vec![Instruction::PushConst(ConstId::new(big))]);
    let decoded = decode_code_object_words(&words).expect("decode should succeed");
    assert_eq!(decoded.len(), 1);
    assert_eq!(decoded[0].display, format!("PushConst {big}"));
    assert_eq!(decoded[0].word_count, 2);
}

#[test]
fn round_trip_vm_two_operand() {
    let words = encode_instructions(vec![Instruction::AddAssign(
        SymbolId::new(5),
        WordProgramId::new(10),
    )]);
    let decoded = decode_code_object_words(&words).expect("decode should succeed");
    assert_eq!(decoded.len(), 1);
    assert_eq!(decoded[0].display, "AddAssign sym=5 wp=10");
}

#[test]
fn round_trip_vm_define_function() {
    let words = encode_instructions(vec![Instruction::DefineFunction {
        name: SymbolId::new(1),
        body: CodeObjectId::new(2),
    }]);
    let decoded = decode_code_object_words(&words).expect("decode should succeed");
    assert_eq!(decoded[0].display, "DefineFunction name=1 body=2");
}

#[test]
fn round_trip_vm_for_setup() {
    let words = encode_instructions(vec![Instruction::ForSetup {
        var: SymbolId::new(3),
        word_count: 5,
    }]);
    let decoded = decode_code_object_words(&words).expect("decode should succeed");
    assert_eq!(decoded[0].display, "ForSetup var=3 word_count=5");
}

#[test]
fn round_trip_vm_exec_simple_hints() {
    let words = encode_instructions(vec![
        Instruction::ExecSimple(CommandDispatchHint::NoCommand),
        Instruction::ExecSimple(CommandDispatchHint::SpecialBuiltin),
        Instruction::ExecSimple(CommandDispatchHint::Standard),
    ]);
    let decoded = decode_code_object_words(&words).expect("decode should succeed");
    assert_eq!(decoded[0].display, "ExecSimple NoCommand");
    assert_eq!(decoded[1].display, "ExecSimple SpecialBuiltin");
    assert_eq!(decoded[2].display, "ExecSimple Standard");
}

#[test]
fn round_trip_word_program() {
    let words = encode_word_ops(vec![
        WordProgramOp::PushLiteral(StringId::new(3)),
        WordProgramOp::FieldSplit,
        WordProgramOp::Glob,
        WordProgramOp::QuoteRemoval,
    ]);
    let decoded = decode_word_program_words(&words).expect("decode should succeed");
    assert_eq!(decoded.len(), 4);
    assert_eq!(decoded[0].display, "PushLiteral 3");
    assert_eq!(decoded[1].display, "FieldSplit");
    assert_eq!(decoded[2].display, "Glob");
    assert_eq!(decoded[3].display, "QuoteRemoval");
}

#[test]
fn round_trip_redirect_program() {
    let words = encode_redirect_ops(vec![
        RedirectProgramOp::Open {
            fd: 1,
            target: WordProgramId::new(0),
            mode: OpenMode::WriteCreate,
        },
        RedirectProgramOp::Close { fd: 2 },
    ]);
    let decoded = decode_redirect_program_words(&words).expect("decode should succeed");
    assert_eq!(decoded.len(), 2);
    assert!(decoded[0].display.contains("Open"));
    assert!(decoded[0].display.contains("fd=1"));
    assert!(decoded[0].display.contains("WriteCreate"));
    assert_eq!(decoded[1].display, "Close fd=2");
}

#[test]
fn round_trip_redirect_dup() {
    let words = encode_redirect_ops(vec![RedirectProgramOp::Dup { from: 1, to: 2 }]);
    let decoded = decode_redirect_program_words(&words).expect("decode should succeed");
    assert_eq!(decoded[0].display, "Dup from=1 to=2");
}

#[test]
fn round_trip_redirect_heredoc() {
    let words = encode_redirect_ops(vec![RedirectProgramOp::HereDoc {
        fd: 0,
        body: StringId::new(7),
        expand: true,
    }]);
    let decoded = decode_redirect_program_words(&words).expect("decode should succeed");
    assert!(decoded[0].display.contains("HereDoc"));
    assert!(decoded[0].display.contains("fd=0"));
    assert!(decoded[0].display.contains("expand=true"));
    assert!(decoded[0].display.contains("body=7"));
}

#[test]
fn round_trip_arith_program() {
    let words = encode_arith_ops(vec![
        ArithProgramOp::PushLiteral(100),
        ArithProgramOp::LoadVariable(SymbolId::new(1)),
        ArithProgramOp::Add,
        ArithProgramOp::Pop,
    ]);
    let decoded = decode_arith_program_words(&words).expect("decode should succeed");
    assert_eq!(decoded.len(), 4);
    assert_eq!(decoded[0].display, "PushLiteral 100");
    assert_eq!(decoded[1].display, "LoadVariable 1");
    assert_eq!(decoded[2].display, "Add");
    assert_eq!(decoded[3].display, "Pop");
}

#[test]
fn round_trip_arith_compound_assign() {
    let words = encode_arith_ops(vec![ArithProgramOp::CompoundAssign(
        SymbolId::new(2),
        ArithCompoundOp::Multiply,
    )]);
    let decoded = decode_arith_program_words(&words).expect("decode should succeed");
    assert_eq!(decoded[0].display, "CompoundAssign sym=2 op=Multiply");
}

// ===========================================================================
// 12. Full pipeline test (parse -> lower -> encode)
// ===========================================================================

fn parser_for(input: &str) -> Parser<'_> {
    let lexer = Lexer::new(input, LexerMode::Normal);
    let stream = TokenStream::new(lexer);
    Parser::new(SourceId::new(0), ParseOptions::default(), stream)
}

#[test]
fn full_pipeline_echo_hello() {
    let mut parser = parser_for("echo hello\n");
    let step = parser
        .parse_complete_command()
        .expect("parse should succeed");
    let ParseStep::Complete(command) = step else {
        panic!("expected complete command");
    };

    let mut context = LoweringContext::new(IrOptions::default());
    let module = context
        .lower_complete_command(&command)
        .expect("lowering should succeed");

    let encoded = encode_module(&module).expect("encoding should succeed");
    assert!(!encoded.code_objects.is_empty());
    assert!(!encoded.code_objects[0].words.is_empty());

    // Decode should also succeed
    let decoded =
        decode_code_object_words(&encoded.code_objects[0].words).expect("decode should succeed");
    assert!(!decoded.is_empty());
}

#[test]
fn full_pipeline_program() {
    let mut parser = parser_for("echo a\necho b\n");
    let program = parser
        .parse_program()
        .expect("program fixture should parse");

    let mut context = LoweringContext::new(IrOptions::default());
    let module = context
        .lower_program(&program)
        .expect("lowering should succeed");

    let encoded = encode_module(&module).expect("encoding should succeed");
    assert!(!encoded.code_objects.is_empty());

    // All code objects should decode
    for eco in &encoded.code_objects {
        let decoded = decode_code_object_words(&eco.words).expect("decode should succeed");
        assert!(!decoded.is_empty() || eco.words.is_empty());
    }
}

// ===========================================================================
// 13. Determinism test
// ===========================================================================

#[test]
fn encoding_is_deterministic() {
    let mut parser1 = parser_for("echo hello\n");
    let step1 = parser1
        .parse_complete_command()
        .expect("parse should succeed");
    let ParseStep::Complete(cmd1) = step1 else {
        panic!("expected complete command");
    };
    let mut ctx1 = LoweringContext::new(IrOptions::default());
    let mod1 = ctx1
        .lower_complete_command(&cmd1)
        .expect("lowering should succeed");
    let enc1 = encode_module(&mod1).expect("encoding should succeed");

    let mut parser2 = parser_for("echo hello\n");
    let step2 = parser2
        .parse_complete_command()
        .expect("parse should succeed");
    let ParseStep::Complete(cmd2) = step2 else {
        panic!("expected complete command");
    };
    let mut ctx2 = LoweringContext::new(IrOptions::default());
    let mod2 = ctx2
        .lower_complete_command(&cmd2)
        .expect("lowering should succeed");
    let enc2 = encode_module(&mod2).expect("encoding should succeed");

    assert_eq!(enc1, enc2, "encoding the same input twice must be identical");
}

// ===========================================================================
// 14. SourceMapEntry default
// ===========================================================================

#[test]
fn source_map_entry_default() {
    let entry = SourceMapEntry::default();
    assert_eq!(entry.word_offset, 0);
    assert_eq!(entry.source_start, 0);
    assert_eq!(entry.source_end, 0);
}

#[test]
fn encoded_code_object_source_map_is_empty() {
    let module = make_module_with_code(vec![Instruction::Nop]);
    let encoded = encode_module(&module).expect("encoding should succeed");
    assert!(encoded.code_objects[0].source_map.is_empty());
}

// ===========================================================================
// 15. Pools are cloned correctly
// ===========================================================================

#[test]
fn pools_are_preserved() {
    let module = IrModule {
        string_pool: vec!["hello".to_string(), "world".to_string()],
        symbol_pool: vec!["x".to_string()],
        const_pool: vec![kish::ir::ConstValue::Integer(42)],
        ..Default::default()
    };
    let encoded = encode_module(&module).expect("encoding should succeed");
    assert_eq!(encoded.string_pool, module.string_pool);
    assert_eq!(encoded.symbol_pool, module.symbol_pool);
    assert_eq!(encoded.const_pool, module.const_pool);
}

// ===========================================================================
// 16. Multiple instructions sequence word offsets
// ===========================================================================

#[test]
fn decode_tracks_offsets_correctly() {
    let words = encode_instructions(vec![
        Instruction::Nop,                          // 1 word, offset 0
        Instruction::PushInt(42),                   // 3 words, offset 1
        Instruction::PushConst(ConstId::new(0)),    // 1 word, offset 4
    ]);
    let decoded = decode_code_object_words(&words).expect("decode should succeed");
    assert_eq!(decoded.len(), 3);
    assert_eq!(decoded[0].offset, 0);
    assert_eq!(decoded[0].word_count, 1);
    assert_eq!(decoded[1].offset, 1);
    assert_eq!(decoded[1].word_count, 3);
    assert_eq!(decoded[2].offset, 4);
    assert_eq!(decoded[2].word_count, 1);
}
