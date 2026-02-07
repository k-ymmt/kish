//! Tests for the IR verification pass.

use kish::ir::bytecode::{
    ArithCompoundOp, ArithProgramOp, BranchTarget, CommandDispatchHint, Instruction, OpenMode,
    RedirectProgramOp, WordProgramOp,
};
use kish::ir::ids::{
    ArithProgramId, CodeObjectId, ConstId, LocalId, RedirectProgramId, StringId, SymbolId,
    WordProgramId,
};
use kish::ir::program::{
    ArithProgram, CodeObject, ConstValue, IrModule, RedirectProgram, WordProgram,
};
use kish::ir::{
    IrErrorKind, IrOptions, LoweringContext, VerifyWarningKind, encode_module, verify_module,
    verify_module_debug,
};
use kish::lexer::{Lexer, LexerMode, SourceId};
use kish::parser::{ParseOptions, ParseStep, Parser, TokenStream};

// ===========================================================================
// Helpers
// ===========================================================================

fn parser_for(input: &str) -> Parser<'_> {
    let lexer = Lexer::new(input, LexerMode::Normal);
    let stream = TokenStream::new(lexer);
    Parser::new(SourceId::new(0), ParseOptions::default(), stream)
}

fn lower_input(input: &str) -> IrModule {
    let mut parser = parser_for(input);
    let step = parser
        .parse_complete_command()
        .expect("should parse");
    let ParseStep::Complete(command) = step else {
        panic!("expected complete command");
    };
    let mut ctx = LoweringContext::new(IrOptions::default());
    ctx.lower_complete_command(&command)
        .expect("lowering should succeed")
}

fn lower_program_input(input: &str) -> IrModule {
    let mut parser = parser_for(input);
    let program = parser.parse_program().expect("should parse program");
    let mut ctx = LoweringContext::new(IrOptions::default());
    ctx.lower_program(&program)
        .expect("lowering should succeed")
}

fn assert_invariant_violation(result: Result<(), kish::ir::IrError>, expected_msg: &str) {
    let err = result.expect_err("should fail verification");
    assert_eq!(err.kind, IrErrorKind::InvariantViolation);
    assert_eq!(err.message, expected_msg, "message mismatch");
}

fn assert_invariant_violation_detail(
    result: Result<(), kish::ir::IrError>,
    expected_msg: &str,
    expected_detail_contains: &str,
) {
    let err = result.expect_err("should fail verification");
    assert_eq!(err.kind, IrErrorKind::InvariantViolation);
    assert_eq!(err.message, expected_msg, "message mismatch");
    let detail = err.detail.as_deref().unwrap_or("");
    assert!(
        detail.contains(expected_detail_contains),
        "detail {detail:?} should contain {expected_detail_contains:?}"
    );
}

// ===========================================================================
// Positive tests
// ===========================================================================

#[test]
fn empty_module_verifies() {
    let module = IrModule::default();
    verify_module(&module).expect("empty module should verify");
}

#[test]
fn valid_simple_command_module_verifies() {
    let module = lower_input("echo hello\n");
    verify_module(&module).expect("simple command module should verify");
}

#[test]
fn valid_pipeline_module_verifies() {
    let module = lower_input("echo a | cat\n");
    verify_module(&module).expect("pipeline module should verify");
}

#[test]
fn valid_control_flow_module_verifies() {
    let module = lower_input("if true; then echo ok; fi\n");
    verify_module(&module).expect("control flow module should verify");
}

#[test]
fn valid_for_loop_module_verifies() {
    let module = lower_input("for x in a b c; do echo $x; done\n");
    verify_module(&module).expect("for loop module should verify");
}

#[test]
fn valid_case_module_verifies() {
    let module = lower_input("case x in a) echo a;; esac\n");
    verify_module(&module).expect("case module should verify");
}

#[test]
fn valid_module_with_all_pool_types_verifies() {
    let mut module = IrModule::default();

    // String and symbol pools
    module.string_pool.push("hello".to_string());
    module.symbol_pool.push("var".to_string());

    // Const pool referencing pools
    module.const_pool.push(ConstValue::String(StringId::new(0)));
    module.const_pool.push(ConstValue::Symbol(SymbolId::new(0)));
    module.const_pool.push(ConstValue::Integer(42));

    // Word program
    module.word_programs.push(WordProgram {
        id: WordProgramId::new(0),
        ops: vec![
            WordProgramOp::PushLiteral(StringId::new(0)),
            WordProgramOp::ExpandParameter(SymbolId::new(0)),
            WordProgramOp::FieldSplit,
            WordProgramOp::QuoteRemoval,
        ],
    });

    // Redirect program
    module.redirect_programs.push(RedirectProgram {
        id: RedirectProgramId::new(0),
        ops: vec![
            RedirectProgramOp::Open {
                fd: 1,
                target: WordProgramId::new(0),
                mode: OpenMode::WriteCreate,
            },
            RedirectProgramOp::HereDoc {
                fd: 0,
                body: StringId::new(0),
                expand: true,
            },
            RedirectProgramOp::Close { fd: 2 },
        ],
    });

    // Arith program
    module.arith_programs.push(ArithProgram {
        id: ArithProgramId::new(0),
        ops: vec![
            ArithProgramOp::LoadVariable(SymbolId::new(0)),
            ArithProgramOp::PushLiteral(1),
            ArithProgramOp::Add,
        ],
    });

    // Code object using all the above
    module.code_objects.push(CodeObject {
        id: CodeObjectId::new(0),
        instructions: vec![
            Instruction::PushConst(ConstId::new(0)),
            Instruction::Drop,
            Instruction::PushString(StringId::new(0)),
            Instruction::Drop,
            Instruction::PushSymbol(SymbolId::new(0)),
            Instruction::Drop,
            Instruction::BeginSimple,
            Instruction::AddArg(WordProgramId::new(0)),
            Instruction::AddRedir(RedirectProgramId::new(0)),
            Instruction::EndSimple,
            Instruction::ExecSimple(CommandDispatchHint::Standard),
            Instruction::Ret,
        ],
        locals_count: 0,
        max_stack_depth: 1,
    });

    verify_module(&module).expect("module with all pool types should verify");
}

// ===========================================================================
// Negative tests: const pool
// ===========================================================================

#[test]
fn const_pool_invalid_string_ref() {
    let mut module = IrModule::default();
    module.const_pool.push(ConstValue::String(StringId::new(0)));

    assert_invariant_violation(verify_module(&module), "invalid string pool reference");
}

#[test]
fn const_pool_invalid_symbol_ref() {
    let mut module = IrModule::default();
    module.const_pool.push(ConstValue::Symbol(SymbolId::new(0)));

    assert_invariant_violation(verify_module(&module), "invalid symbol pool reference");
}

// ===========================================================================
// Negative tests: code object identity
// ===========================================================================

#[test]
fn code_object_id_mismatch() {
    let mut module = IrModule::default();
    module.code_objects.push(CodeObject {
        id: CodeObjectId::new(5),
        instructions: vec![],
        locals_count: 0,
        max_stack_depth: 0,
    });

    assert_invariant_violation_detail(
        verify_module(&module),
        "code object id mismatch",
        "has id=5, expected 0",
    );
}

// ===========================================================================
// Negative tests: instruction pool refs
// ===========================================================================

#[test]
fn code_object_out_of_bounds_push_const() {
    let mut module = IrModule::default();
    module.code_objects.push(CodeObject {
        id: CodeObjectId::new(0),
        instructions: vec![Instruction::PushConst(ConstId::new(99))],
        locals_count: 0,
        max_stack_depth: 0,
    });

    assert_invariant_violation(verify_module(&module), "invalid const pool reference");
}

#[test]
fn code_object_out_of_bounds_push_string() {
    let mut module = IrModule::default();
    module.code_objects.push(CodeObject {
        id: CodeObjectId::new(0),
        instructions: vec![Instruction::PushString(StringId::new(5))],
        locals_count: 0,
        max_stack_depth: 0,
    });

    assert_invariant_violation_detail(
        verify_module(&module),
        "invalid string pool reference",
        "string_id=5",
    );
}

#[test]
fn code_object_out_of_bounds_push_symbol() {
    let mut module = IrModule::default();
    module.code_objects.push(CodeObject {
        id: CodeObjectId::new(0),
        instructions: vec![Instruction::PushSymbol(SymbolId::new(3))],
        locals_count: 0,
        max_stack_depth: 0,
    });

    assert_invariant_violation(verify_module(&module), "invalid symbol pool reference");
}

#[test]
fn code_object_out_of_bounds_call() {
    let mut module = IrModule::default();
    module.code_objects.push(CodeObject {
        id: CodeObjectId::new(0),
        instructions: vec![Instruction::Call(CodeObjectId::new(10))],
        locals_count: 0,
        max_stack_depth: 0,
    });

    assert_invariant_violation(verify_module(&module), "invalid code object reference");
}

#[test]
fn code_object_out_of_bounds_add_arg() {
    let mut module = IrModule::default();
    module.code_objects.push(CodeObject {
        id: CodeObjectId::new(0),
        instructions: vec![
            Instruction::BeginSimple,
            Instruction::AddArg(WordProgramId::new(0)),
            Instruction::EndSimple,
            Instruction::ExecSimple(CommandDispatchHint::Standard),
        ],
        locals_count: 0,
        max_stack_depth: 0,
    });

    assert_invariant_violation(verify_module(&module), "invalid word program reference");
}

#[test]
fn code_object_out_of_bounds_add_redir() {
    let mut module = IrModule::default();
    module.code_objects.push(CodeObject {
        id: CodeObjectId::new(0),
        instructions: vec![
            Instruction::BeginSimple,
            Instruction::AddRedir(RedirectProgramId::new(7)),
            Instruction::EndSimple,
            Instruction::ExecSimple(CommandDispatchHint::Standard),
        ],
        locals_count: 0,
        max_stack_depth: 0,
    });

    assert_invariant_violation(verify_module(&module), "invalid redirect program reference");
}

#[test]
fn code_object_branch_target_overflow() {
    let mut module = IrModule::default();
    module.code_objects.push(CodeObject {
        id: CodeObjectId::new(0),
        instructions: vec![
            Instruction::Jmp(BranchTarget::new(100)),
        ],
        locals_count: 0,
        max_stack_depth: 0,
    });

    assert_invariant_violation_detail(
        verify_module(&module),
        "invalid branch target",
        "branch_target=100",
    );
}

#[test]
fn code_object_branch_target_one_past_end_is_valid() {
    let mut module = IrModule::default();
    // 2 instructions, so target=2 (one-past-end) is valid
    module.code_objects.push(CodeObject {
        id: CodeObjectId::new(0),
        instructions: vec![
            Instruction::Nop,
            Instruction::Jmp(BranchTarget::new(2)),
        ],
        locals_count: 0,
        max_stack_depth: 0,
    });

    verify_module(&module).expect("one-past-end branch target should be valid");
}

#[test]
fn code_object_local_id_overflow() {
    let mut module = IrModule::default();
    module.code_objects.push(CodeObject {
        id: CodeObjectId::new(0),
        instructions: vec![Instruction::LocalGet(LocalId::new(5))],
        locals_count: 2,
        max_stack_depth: 0,
    });

    assert_invariant_violation_detail(
        verify_module(&module),
        "invalid local variable reference",
        "local_id=5",
    );
}

#[test]
fn code_object_define_function_invalid_refs() {
    let mut module = IrModule::default();
    module.code_objects.push(CodeObject {
        id: CodeObjectId::new(0),
        instructions: vec![Instruction::DefineFunction {
            name: SymbolId::new(0),
            body: CodeObjectId::new(0),
        }],
        locals_count: 0,
        max_stack_depth: 0,
    });

    // No symbols exist
    assert_invariant_violation(verify_module(&module), "invalid symbol pool reference");
}

#[test]
fn code_object_exec_subshell_invalid() {
    let mut module = IrModule::default();
    module.code_objects.push(CodeObject {
        id: CodeObjectId::new(0),
        instructions: vec![Instruction::ExecSubshell(CodeObjectId::new(5))],
        locals_count: 0,
        max_stack_depth: 0,
    });

    assert_invariant_violation(verify_module(&module), "invalid code object reference");
}

#[test]
fn code_object_add_assign_invalid_symbol() {
    let mut module = IrModule::default();
    module.word_programs.push(WordProgram {
        id: WordProgramId::new(0),
        ops: vec![],
    });
    module.code_objects.push(CodeObject {
        id: CodeObjectId::new(0),
        instructions: vec![
            Instruction::BeginSimple,
            Instruction::AddAssign(SymbolId::new(99), WordProgramId::new(0)),
            Instruction::EndSimple,
            Instruction::ExecSimple(CommandDispatchHint::NoCommand),
        ],
        locals_count: 0,
        max_stack_depth: 0,
    });

    assert_invariant_violation(verify_module(&module), "invalid symbol pool reference");
}

// ===========================================================================
// Negative tests: structural balancing
// ===========================================================================

#[test]
fn unbalanced_end_simple_without_begin() {
    let mut module = IrModule::default();
    module.code_objects.push(CodeObject {
        id: CodeObjectId::new(0),
        instructions: vec![Instruction::EndSimple],
        locals_count: 0,
        max_stack_depth: 0,
    });

    assert_invariant_violation_detail(
        verify_module(&module),
        "unbalanced simple command assembly",
        "EndSimple without matching BeginSimple",
    );
}

#[test]
fn unbalanced_begin_simple_unclosed() {
    let mut module = IrModule::default();
    module.code_objects.push(CodeObject {
        id: CodeObjectId::new(0),
        instructions: vec![Instruction::BeginSimple],
        locals_count: 0,
        max_stack_depth: 0,
    });

    assert_invariant_violation_detail(
        verify_module(&module),
        "unbalanced simple command assembly",
        "unclosed BeginSimple",
    );
}

#[test]
fn unbalanced_pop_redirect_scope() {
    let mut module = IrModule::default();
    module.code_objects.push(CodeObject {
        id: CodeObjectId::new(0),
        instructions: vec![Instruction::PopRedirectScope],
        locals_count: 0,
        max_stack_depth: 0,
    });

    assert_invariant_violation_detail(
        verify_module(&module),
        "unbalanced redirect scope",
        "PopRedirectScope without matching PushRedirectScope",
    );
}

#[test]
fn unbalanced_push_redirect_scope_unclosed() {
    let mut module = IrModule::default();
    module.code_objects.push(CodeObject {
        id: CodeObjectId::new(0),
        instructions: vec![Instruction::PushRedirectScope],
        locals_count: 0,
        max_stack_depth: 0,
    });

    assert_invariant_violation_detail(
        verify_module(&module),
        "unbalanced redirect scope",
        "unclosed PushRedirectScope",
    );
}

#[test]
fn pipeline_stage_count_mismatch() {
    let mut module = IrModule::default();
    module.code_objects.push(CodeObject {
        id: CodeObjectId::new(0),
        instructions: vec![
            Instruction::BeginPipeline(2),
            Instruction::AddPipelineStage(CodeObjectId::new(0)),
            // Only 1 stage instead of 2
            Instruction::ExecPipeline,
        ],
        locals_count: 0,
        max_stack_depth: 0,
    });

    assert_invariant_violation_detail(
        verify_module(&module),
        "pipeline stage count mismatch",
        "expected 2 stages, got 1",
    );
}

#[test]
fn orphan_add_pipeline_stage() {
    let mut module = IrModule::default();
    module.code_objects.push(CodeObject {
        id: CodeObjectId::new(0),
        instructions: vec![Instruction::AddPipelineStage(CodeObjectId::new(0))],
        locals_count: 0,
        max_stack_depth: 0,
    });

    assert_invariant_violation_detail(
        verify_module(&module),
        "orphan pipeline stage",
        "AddPipelineStage without BeginPipeline",
    );
}

#[test]
fn orphan_exec_pipeline() {
    let mut module = IrModule::default();
    module.code_objects.push(CodeObject {
        id: CodeObjectId::new(0),
        instructions: vec![Instruction::ExecPipeline],
        locals_count: 0,
        max_stack_depth: 0,
    });

    assert_invariant_violation_detail(
        verify_module(&module),
        "orphan pipeline exec",
        "ExecPipeline without BeginPipeline",
    );
}

#[test]
fn orphan_for_add_word() {
    let mut module = IrModule::default();
    module.word_programs.push(WordProgram {
        id: WordProgramId::new(0),
        ops: vec![],
    });
    module.code_objects.push(CodeObject {
        id: CodeObjectId::new(0),
        instructions: vec![Instruction::ForAddWord(WordProgramId::new(0))],
        locals_count: 0,
        max_stack_depth: 0,
    });

    assert_invariant_violation_detail(
        verify_module(&module),
        "orphan for-add-word",
        "ForAddWord without ForSetup",
    );
}

#[test]
fn incomplete_for_setup() {
    let mut module = IrModule::default();
    module.symbol_pool.push("x".to_string());
    module.word_programs.push(WordProgram {
        id: WordProgramId::new(0),
        ops: vec![],
    });
    module.code_objects.push(CodeObject {
        id: CodeObjectId::new(0),
        instructions: vec![
            Instruction::ForSetup {
                var: SymbolId::new(0),
                word_count: 3,
            },
            Instruction::ForAddWord(WordProgramId::new(0)),
            // Only 1 of 3 words added
        ],
        locals_count: 0,
        max_stack_depth: 0,
    });

    assert_invariant_violation_detail(
        verify_module(&module),
        "incomplete for-loop setup",
        "ForSetup expected 3 words, only 1 added",
    );
}

// ===========================================================================
// Negative tests: word program
// ===========================================================================

#[test]
fn word_program_id_mismatch() {
    let mut module = IrModule::default();
    module.word_programs.push(WordProgram {
        id: WordProgramId::new(7),
        ops: vec![],
    });

    assert_invariant_violation_detail(
        verify_module(&module),
        "word program id mismatch",
        "has id=7, expected 0",
    );
}

#[test]
fn word_program_out_of_bounds_string() {
    let mut module = IrModule::default();
    module.word_programs.push(WordProgram {
        id: WordProgramId::new(0),
        ops: vec![WordProgramOp::PushLiteral(StringId::new(0))],
    });

    assert_invariant_violation(verify_module(&module), "invalid string pool reference");
}

#[test]
fn word_program_out_of_bounds_code_object() {
    let mut module = IrModule::default();
    module.word_programs.push(WordProgram {
        id: WordProgramId::new(0),
        ops: vec![WordProgramOp::ExpandCommandSubstitution(CodeObjectId::new(
            0,
        ))],
    });

    assert_invariant_violation(verify_module(&module), "invalid code object reference");
}

#[test]
fn word_program_out_of_bounds_arith() {
    let mut module = IrModule::default();
    module.word_programs.push(WordProgram {
        id: WordProgramId::new(0),
        ops: vec![WordProgramOp::ExpandArithmetic(ArithProgramId::new(0))],
    });

    assert_invariant_violation(verify_module(&module), "invalid arith program reference");
}

#[test]
fn word_program_out_of_bounds_symbol() {
    let mut module = IrModule::default();
    module.word_programs.push(WordProgram {
        id: WordProgramId::new(0),
        ops: vec![WordProgramOp::ExpandParameter(SymbolId::new(0))],
    });

    assert_invariant_violation(verify_module(&module), "invalid symbol pool reference");
}

// ===========================================================================
// Negative tests: redirect program
// ===========================================================================

#[test]
fn redirect_program_id_mismatch() {
    let mut module = IrModule::default();
    module.redirect_programs.push(RedirectProgram {
        id: RedirectProgramId::new(3),
        ops: vec![],
    });

    assert_invariant_violation_detail(
        verify_module(&module),
        "redirect program id mismatch",
        "has id=3, expected 0",
    );
}

#[test]
fn redirect_program_out_of_bounds_word_program() {
    let mut module = IrModule::default();
    module.redirect_programs.push(RedirectProgram {
        id: RedirectProgramId::new(0),
        ops: vec![RedirectProgramOp::Open {
            fd: 1,
            target: WordProgramId::new(0),
            mode: OpenMode::WriteCreate,
        }],
    });

    assert_invariant_violation(verify_module(&module), "invalid word program reference");
}

#[test]
fn redirect_program_out_of_bounds_string() {
    let mut module = IrModule::default();
    module.redirect_programs.push(RedirectProgram {
        id: RedirectProgramId::new(0),
        ops: vec![RedirectProgramOp::HereDoc {
            fd: 0,
            body: StringId::new(0),
            expand: false,
        }],
    });

    assert_invariant_violation(verify_module(&module), "invalid string pool reference");
}

// ===========================================================================
// Negative tests: arith program
// ===========================================================================

#[test]
fn arith_program_id_mismatch() {
    let mut module = IrModule::default();
    module.arith_programs.push(ArithProgram {
        id: ArithProgramId::new(2),
        ops: vec![],
    });

    assert_invariant_violation_detail(
        verify_module(&module),
        "arith program id mismatch",
        "has id=2, expected 0",
    );
}

#[test]
fn arith_program_out_of_bounds_symbol_load() {
    let mut module = IrModule::default();
    module.arith_programs.push(ArithProgram {
        id: ArithProgramId::new(0),
        ops: vec![ArithProgramOp::LoadVariable(SymbolId::new(0))],
    });

    assert_invariant_violation(verify_module(&module), "invalid symbol pool reference");
}

#[test]
fn arith_program_out_of_bounds_symbol_assign() {
    let mut module = IrModule::default();
    module.arith_programs.push(ArithProgram {
        id: ArithProgramId::new(0),
        ops: vec![
            ArithProgramOp::PushLiteral(1),
            ArithProgramOp::Assign(SymbolId::new(0)),
        ],
    });

    assert_invariant_violation(verify_module(&module), "invalid symbol pool reference");
}

#[test]
fn arith_program_out_of_bounds_compound_assign() {
    let mut module = IrModule::default();
    module.arith_programs.push(ArithProgram {
        id: ArithProgramId::new(0),
        ops: vec![
            ArithProgramOp::PushLiteral(1),
            ArithProgramOp::CompoundAssign(SymbolId::new(0), ArithCompoundOp::Add),
        ],
    });

    assert_invariant_violation(verify_module(&module), "invalid symbol pool reference");
}

#[test]
fn arith_program_out_of_bounds_pre_increment() {
    let mut module = IrModule::default();
    module.arith_programs.push(ArithProgram {
        id: ArithProgramId::new(0),
        ops: vec![ArithProgramOp::PreIncrement(SymbolId::new(0))],
    });

    assert_invariant_violation(verify_module(&module), "invalid symbol pool reference");
}

#[test]
fn arith_program_out_of_bounds_post_decrement() {
    let mut module = IrModule::default();
    module.arith_programs.push(ArithProgram {
        id: ArithProgramId::new(0),
        ops: vec![ArithProgramOp::PostDecrement(SymbolId::new(0))],
    });

    assert_invariant_violation(verify_module(&module), "invalid symbol pool reference");
}

#[test]
fn arith_program_jump_target_overflow() {
    let mut module = IrModule::default();
    module.arith_programs.push(ArithProgram {
        id: ArithProgramId::new(0),
        ops: vec![
            ArithProgramOp::PushLiteral(1),
            ArithProgramOp::Jmp(99),
        ],
    });

    assert_invariant_violation_detail(
        verify_module(&module),
        "invalid arith jump target",
        "target=99",
    );
}

#[test]
fn arith_program_jmp_if_zero_target_overflow() {
    let mut module = IrModule::default();
    module.arith_programs.push(ArithProgram {
        id: ArithProgramId::new(0),
        ops: vec![
            ArithProgramOp::PushLiteral(1),
            ArithProgramOp::JmpIfZero(50),
        ],
    });

    assert_invariant_violation_detail(
        verify_module(&module),
        "invalid arith jump target",
        "target=50",
    );
}

#[test]
fn arith_program_jmp_if_non_zero_target_overflow() {
    let mut module = IrModule::default();
    module.arith_programs.push(ArithProgram {
        id: ArithProgramId::new(0),
        ops: vec![
            ArithProgramOp::PushLiteral(1),
            ArithProgramOp::JmpIfNonZero(5),
        ],
    });

    assert_invariant_violation_detail(
        verify_module(&module),
        "invalid arith jump target",
        "target=5",
    );
}

// ===========================================================================
// Integration test: full pipeline roundtrip
// ===========================================================================

#[test]
fn full_pipeline_parse_lower_verify_encode() {
    let module = lower_program_input("echo hello\necho world\n");
    verify_module(&module).expect("lowered module should verify");
    encode_module(&module).expect("verified module should encode");
}

#[test]
fn full_pipeline_complex_program() {
    let module = lower_program_input(
        "if true; then echo ok; else echo no; fi\nfor x in a b; do echo $x; done\n",
    );
    verify_module(&module).expect("complex module should verify");
    encode_module(&module).expect("complex module should encode");
}

// ===========================================================================
// Debug verifier tests
// ===========================================================================

#[test]
fn debug_verifier_detects_unreachable_code() {
    let mut module = IrModule::default();
    module.code_objects.push(CodeObject {
        id: CodeObjectId::new(0),
        instructions: vec![
            Instruction::Ret,
            Instruction::Nop,
            Instruction::Nop,
        ],
        locals_count: 0,
        max_stack_depth: 0,
    });

    let warnings = verify_module_debug(&module);
    assert!(!warnings.is_empty());
    assert!(warnings
        .iter()
        .any(|w| w.kind == VerifyWarningKind::UnreachableCode));
}

#[test]
fn debug_verifier_detects_dead_code_object() {
    let mut module = IrModule::default();
    // Entry code object
    module.code_objects.push(CodeObject {
        id: CodeObjectId::new(0),
        instructions: vec![Instruction::Ret],
        locals_count: 0,
        max_stack_depth: 0,
    });
    // Unreferenced code object
    module.code_objects.push(CodeObject {
        id: CodeObjectId::new(1),
        instructions: vec![Instruction::Ret],
        locals_count: 0,
        max_stack_depth: 0,
    });

    let warnings = verify_module_debug(&module);
    assert!(warnings
        .iter()
        .any(|w| w.kind == VerifyWarningKind::DeadCodeObject));
}

#[test]
fn debug_verifier_no_warnings_for_valid_lowered_module() {
    let module = lower_input("echo hello\n");
    verify_module(&module).expect("should verify");
    let warnings = verify_module_debug(&module);
    // A properly lowered simple command should not have dead code objects or unreachable code
    let dead = warnings
        .iter()
        .filter(|w| w.kind == VerifyWarningKind::DeadCodeObject)
        .count();
    assert_eq!(dead, 0, "no dead code objects expected");
}

#[test]
fn debug_verifier_empty_module_no_warnings() {
    let module = IrModule::default();
    let warnings = verify_module_debug(&module);
    assert!(warnings.is_empty());
}
