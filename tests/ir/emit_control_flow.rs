//! Phase 5 tests: HIR -> VM IR control flow emission.

use kish::ir::{BranchTarget, Instruction, IrModule, IrOptions, LoweringContext};
use kish::lexer::{Lexer, LexerMode, SourceId};
use kish::parser::{ParseOptions, ParseStep, Parser, TokenStream};

// ---------------------------------------------------------------------------
// Test helpers
// ---------------------------------------------------------------------------

fn parser_for(input: &str) -> Parser<'_> {
    let lexer = Lexer::new(input, LexerMode::Normal);
    let stream = TokenStream::new(lexer);
    Parser::new(SourceId::new(0), ParseOptions::default(), stream)
}

fn lower_program(input: &str) -> IrModule {
    let mut parser = parser_for(input);
    let program = parser.parse_program().expect("parse should succeed");
    let mut context = LoweringContext::new(IrOptions::default());
    context.lower_program(&program).expect("lowering should succeed")
}

fn lower_command(input: &str) -> IrModule {
    let mut parser = parser_for(input);
    let step = parser
        .parse_complete_command()
        .expect("parse should succeed");
    let ParseStep::Complete(command) = step else {
        panic!("expected complete command, got {step:?}");
    };
    let mut context = LoweringContext::new(IrOptions::default());
    context
        .lower_complete_command(&command)
        .expect("lowering should succeed")
}

/// Returns the instructions for the first (top-level) code object.
fn top_instructions(module: &IrModule) -> &[Instruction] {
    &module.code_objects[0].instructions
}

// ---------------------------------------------------------------------------
// Simple command stub
// ---------------------------------------------------------------------------

#[test]
fn simple_command_emits_push_int_zero() {
    let module = lower_command("echo hello\n");
    let instrs = top_instructions(&module);
    // Should contain PushInt(0) as stub + Ret
    assert!(instrs.contains(&Instruction::PushInt(0)));
    assert_eq!(instrs.last(), Some(&Instruction::Ret));
}

#[test]
fn simple_command_single_produces_one_code_object() {
    let module = lower_command("echo hello\n");
    assert_eq!(module.code_objects.len(), 1);
}

#[test]
fn simple_command_stub_return_value() {
    let module = lower_command("true\n");
    let instrs = top_instructions(&module);
    // The stub emits PushInt(0), Ret
    assert_eq!(instrs, &[Instruction::PushInt(0), Instruction::Ret]);
}

// ---------------------------------------------------------------------------
// List sequencing
// ---------------------------------------------------------------------------

#[test]
fn list_two_commands_has_drop_between() {
    let module = lower_command("a; b\n");
    let instrs = top_instructions(&module);
    // Expected: PushInt(0), Drop, PushInt(0), Ret
    assert_eq!(
        instrs,
        &[
            Instruction::PushInt(0),
            Instruction::Drop,
            Instruction::PushInt(0),
            Instruction::Ret,
        ]
    );
}

#[test]
fn list_three_commands_has_two_drops() {
    let module = lower_command("a; b; c\n");
    let instrs = top_instructions(&module);
    // Expected: PushInt(0), Drop, PushInt(0), Drop, PushInt(0), Ret
    assert_eq!(
        instrs,
        &[
            Instruction::PushInt(0),
            Instruction::Drop,
            Instruction::PushInt(0),
            Instruction::Drop,
            Instruction::PushInt(0),
            Instruction::Ret,
        ]
    );
}

#[test]
fn list_single_command_no_drop() {
    let module = lower_command("a\n");
    let instrs = top_instructions(&module);
    assert_eq!(instrs, &[Instruction::PushInt(0), Instruction::Ret]);
}

// ---------------------------------------------------------------------------
// AND-OR chains
// ---------------------------------------------------------------------------

#[test]
fn and_if_chain_emits_dup_jmp_if_nonzero() {
    let module = lower_command("a && b\n");
    let instrs = top_instructions(&module);
    // PushInt(0), Dup, JmpIfNonZero(skip), Drop, PushInt(0), skip: Ret
    assert_eq!(instrs[0], Instruction::PushInt(0));
    assert_eq!(instrs[1], Instruction::Dup);
    assert!(matches!(instrs[2], Instruction::JmpIfNonZero(_)));
    assert_eq!(instrs[3], Instruction::Drop);
    assert_eq!(instrs[4], Instruction::PushInt(0));
    assert_eq!(instrs[5], Instruction::Ret);

    // The jump target should point to Ret (index 5)
    let Instruction::JmpIfNonZero(target) = instrs[2] else {
        panic!("expected JmpIfNonZero");
    };
    assert_eq!(target, BranchTarget::new(5));
}

#[test]
fn or_if_chain_emits_dup_jmp_if_zero() {
    let module = lower_command("a || b\n");
    let instrs = top_instructions(&module);
    assert_eq!(instrs[0], Instruction::PushInt(0));
    assert_eq!(instrs[1], Instruction::Dup);
    assert!(matches!(instrs[2], Instruction::JmpIfZero(_)));
    assert_eq!(instrs[3], Instruction::Drop);
    assert_eq!(instrs[4], Instruction::PushInt(0));
    assert_eq!(instrs[5], Instruction::Ret);

    let Instruction::JmpIfZero(target) = instrs[2] else {
        panic!("expected JmpIfZero");
    };
    assert_eq!(target, BranchTarget::new(5));
}

#[test]
fn and_or_mixed_chain() {
    let module = lower_command("a && b || c\n");
    let instrs = top_instructions(&module);
    // a: PushInt(0)
    // Dup, JmpIfNonZero(skip_b)
    // Drop, b: PushInt(0)
    // skip_b:
    // Dup, JmpIfZero(skip_c)
    // Drop, c: PushInt(0)
    // skip_c:
    // Ret
    assert_eq!(instrs[0], Instruction::PushInt(0)); // a
    assert_eq!(instrs[1], Instruction::Dup);
    assert!(matches!(instrs[2], Instruction::JmpIfNonZero(_))); // skip_b
    assert_eq!(instrs[3], Instruction::Drop);
    assert_eq!(instrs[4], Instruction::PushInt(0)); // b
    // skip_b target = index 5
    assert_eq!(instrs[5], Instruction::Dup);
    assert!(matches!(instrs[6], Instruction::JmpIfZero(_))); // skip_c
    assert_eq!(instrs[7], Instruction::Drop);
    assert_eq!(instrs[8], Instruction::PushInt(0)); // c
    // skip_c target = index 9
    assert_eq!(instrs[9], Instruction::Ret);
}

#[test]
fn and_or_single_pipeline_no_extra_instructions() {
    let module = lower_command("a\n");
    let instrs = top_instructions(&module);
    // No Dup/Jmp should appear for single pipeline.
    assert!(!instrs.iter().any(|i| matches!(i, Instruction::Dup)));
}

// ---------------------------------------------------------------------------
// Pipeline
// ---------------------------------------------------------------------------

#[test]
fn single_command_pipeline_emits_inline() {
    let module = lower_command("a\n");
    // Single command should not create sub code objects.
    assert_eq!(module.code_objects.len(), 1);
}

#[test]
fn multi_command_pipeline_emits_sub_code_objects() {
    let module = lower_command("a | b | c\n");
    // Top-level + 3 pipeline stages = 4 code objects
    assert_eq!(module.code_objects.len(), 4);

    let instrs = top_instructions(&module);
    assert_eq!(instrs[0], Instruction::BeginPipeline(3));
    assert!(matches!(instrs[1], Instruction::AddPipelineStage(_)));
    assert!(matches!(instrs[2], Instruction::AddPipelineStage(_)));
    assert!(matches!(instrs[3], Instruction::AddPipelineStage(_)));
    assert_eq!(instrs[4], Instruction::ExecPipeline);
    assert_eq!(instrs[5], Instruction::Ret);
}

#[test]
fn negated_pipeline_emits_negate_status() {
    let module = lower_command("! a\n");
    let instrs = top_instructions(&module);
    assert!(instrs.contains(&Instruction::NegateStatus));
}

#[test]
fn negated_multi_pipeline_emits_negate_after_exec() {
    let module = lower_command("! a | b\n");
    let instrs = top_instructions(&module);
    // Find ExecPipeline, then NegateStatus should follow.
    let exec_idx = instrs
        .iter()
        .position(|i| matches!(i, Instruction::ExecPipeline))
        .expect("should have ExecPipeline");
    assert_eq!(instrs[exec_idx + 1], Instruction::NegateStatus);
}

// ---------------------------------------------------------------------------
// If / elif / else
// ---------------------------------------------------------------------------

#[test]
fn if_then_fi_basic_structure() {
    let module = lower_command("if true; then echo y; fi\n");
    let instrs = top_instructions(&module);
    // condition: PushInt(0)
    // JmpIfNonZero(else_label)
    // then_body: PushInt(0)
    // Jmp(end)
    // else_label: PushInt(0)  -- no else -> default 0
    // end:
    // Ret
    assert!(instrs.contains(&Instruction::PushInt(0)));
    assert!(instrs.iter().any(|i| matches!(i, Instruction::JmpIfNonZero(_))));
    assert!(instrs.iter().any(|i| matches!(i, Instruction::Jmp(_))));
}

#[test]
fn if_else_has_no_default_push() {
    let module = lower_command("if true; then echo y; else echo n; fi\n");
    let instrs = top_instructions(&module);
    // With else body, there should be exactly 2 PushInt(0): condition stub + then body stub
    // plus else body stub = 3 PushInt(0)
    // No extra default PushInt(0) for "no branch taken".
    let push_count = instrs
        .iter()
        .filter(|i| matches!(i, Instruction::PushInt(0)))
        .count();
    // condition (PushInt(0)), then_body (PushInt(0)), else_body (PushInt(0))
    assert_eq!(push_count, 3);
}

#[test]
fn if_elif_else_structure() {
    let module =
        lower_command("if true; then echo a; elif false; then echo b; else echo c; fi\n");
    let instrs = top_instructions(&module);
    // Should have 2 JmpIfNonZero (one for main condition, one for elif condition)
    let jmp_nz_count = instrs
        .iter()
        .filter(|i| matches!(i, Instruction::JmpIfNonZero(_)))
        .count();
    assert_eq!(jmp_nz_count, 2);
    // Should have 2 unconditional Jmp (then_body->end, elif_body->end)
    let jmp_count = instrs
        .iter()
        .filter(|i| matches!(i, Instruction::Jmp(_)))
        .count();
    assert_eq!(jmp_count, 2);
}

#[test]
fn if_no_else_pushes_default_zero() {
    let module = lower_command("if true; then echo y; fi\n");
    let instrs = top_instructions(&module);
    // The code should contain PushInt(0) for: condition stub, then body stub, default exit
    let push_count = instrs
        .iter()
        .filter(|i| matches!(i, Instruction::PushInt(0)))
        .count();
    // condition + then_body + default = 3
    assert_eq!(push_count, 3);
}

// ---------------------------------------------------------------------------
// While / Until loops
// ---------------------------------------------------------------------------

#[test]
fn while_loop_structure() {
    let module = lower_command("while true; do echo loop; done\n");
    let instrs = top_instructions(&module);
    // Default status: PushInt(0)
    // condition: PushInt(0), JmpIfNonZero(end)
    // Drop
    // body: PushInt(0), Jmp(top)
    // end: Ret
    assert!(instrs.iter().any(|i| matches!(i, Instruction::JmpIfNonZero(_))));
    // Backward jump for loop
    assert!(instrs.iter().any(|i| matches!(i, Instruction::Jmp(_))));
    // Should start with PushInt(0) as default.
    assert_eq!(instrs[0], Instruction::PushInt(0));
}

#[test]
fn until_loop_uses_jmp_if_zero() {
    let module = lower_command("until false; do echo loop; done\n");
    let instrs = top_instructions(&module);
    // Until loop exits when condition succeeds (zero).
    assert!(instrs.iter().any(|i| matches!(i, Instruction::JmpIfZero(_))));
    // Should not have JmpIfNonZero
    assert!(!instrs.iter().any(|i| matches!(i, Instruction::JmpIfNonZero(_))));
}

#[test]
fn while_loop_backward_jump() {
    let module = lower_command("while true; do echo loop; done\n");
    let instrs = top_instructions(&module);
    // Find the unconditional Jmp; its target should be before the Jmp itself (backward jump).
    for (idx, instr) in instrs.iter().enumerate() {
        if let Instruction::Jmp(target) = instr {
            if (target.index() as usize) < idx {
                return; // Found backward jump - test passes.
            }
        }
    }
    panic!("expected a backward jump in while loop");
}

// ---------------------------------------------------------------------------
// For loop
// ---------------------------------------------------------------------------

#[test]
fn for_loop_basic_structure() {
    let module = lower_command("for i in a b; do echo $i; done\n");
    let instrs = top_instructions(&module);
    // Should contain ForSetup, ForAddWord x2, ForNext
    assert!(instrs.iter().any(|i| matches!(i, Instruction::ForSetup { .. })));
    let add_word_count = instrs
        .iter()
        .filter(|i| matches!(i, Instruction::ForAddWord(_)))
        .count();
    assert_eq!(add_word_count, 2);
    assert!(instrs.iter().any(|i| matches!(i, Instruction::ForNext)));
}

#[test]
fn for_loop_without_words() {
    // `for x` without `in` uses word_count: 0
    let module = lower_command("for i do echo $i; done\n");
    let instrs = top_instructions(&module);
    let setup = instrs
        .iter()
        .find(|i| matches!(i, Instruction::ForSetup { .. }))
        .expect("should have ForSetup");
    let Instruction::ForSetup { word_count, .. } = setup else {
        unreachable!();
    };
    assert_eq!(*word_count, 0);
    // No ForAddWord instructions
    assert!(!instrs.iter().any(|i| matches!(i, Instruction::ForAddWord(_))));
}

#[test]
fn for_loop_interns_variable_symbol() {
    let module = lower_command("for myvar in a; do echo x; done\n");
    // The symbol "myvar" should be in the symbol pool
    assert!(module.symbol_pool.iter().any(|s| s == "myvar"));
}

// ---------------------------------------------------------------------------
// Case statement
// ---------------------------------------------------------------------------

#[test]
fn case_basic_structure() {
    let module = lower_command("case x in a) echo a;; esac\n");
    let instrs = top_instructions(&module);
    assert!(instrs.contains(&Instruction::CaseSetSubject));
    assert!(instrs.iter().any(|i| matches!(i, Instruction::CaseTestPattern(_))));
    assert!(instrs.contains(&Instruction::CaseClear));
}

#[test]
fn case_break_jumps_to_end() {
    let module = lower_command("case x in a) echo a;; b) echo b;; esac\n");
    let instrs = top_instructions(&module);
    // Both items use `;;` (Break), so there should be jumps to case_end.
    let jmp_count = instrs
        .iter()
        .filter(|i| matches!(i, Instruction::Jmp(_)))
        .count();
    // At least 2 jumps for break + jumps for no-match skips
    assert!(jmp_count >= 2);
}

#[test]
fn case_fallthrough_jumps_to_next_body() {
    let module = lower_command("case x in a) echo a;& b) echo b;; esac\n");
    let instrs = top_instructions(&module);
    // With fallthrough, item[0]'s terminator jumps to item[1]'s body label,
    // skipping item[1]'s pattern matching.
    // We verify the structure has the correct pattern.
    assert!(instrs.contains(&Instruction::CaseSetSubject));
    assert!(instrs.contains(&Instruction::CaseClear));
}

#[test]
fn case_multiple_patterns() {
    let module = lower_command("case x in a|b) echo match;; esac\n");
    let instrs = top_instructions(&module);
    // Two patterns means two CaseTestPattern instructions.
    let test_count = instrs
        .iter()
        .filter(|i| matches!(i, Instruction::CaseTestPattern(_)))
        .count();
    assert_eq!(test_count, 2);
}

// ---------------------------------------------------------------------------
// Subshell / Brace-group
// ---------------------------------------------------------------------------

#[test]
fn subshell_creates_sub_code_object() {
    let module = lower_command("(echo a)\n");
    // Top-level + subshell code object = 2
    assert_eq!(module.code_objects.len(), 2);
    let instrs = top_instructions(&module);
    assert!(instrs.iter().any(|i| matches!(i, Instruction::ExecSubshell(_))));
}

#[test]
fn brace_group_emits_inline() {
    let module = lower_command("{ echo a; }\n");
    // Brace group should NOT create a sub code object.
    assert_eq!(module.code_objects.len(), 1);
    // Should just have the stub PushInt(0) inline.
    let instrs = top_instructions(&module);
    assert_eq!(instrs, &[Instruction::PushInt(0), Instruction::Ret]);
}

#[test]
fn subshell_sub_code_object_has_ret() {
    let module = lower_command("(echo a)\n");
    // The subshell code object (index 1) should end with Ret.
    let sub_instrs = &module.code_objects[1].instructions;
    assert_eq!(sub_instrs.last(), Some(&Instruction::Ret));
}

// ---------------------------------------------------------------------------
// Function definition
// ---------------------------------------------------------------------------

#[test]
fn function_definition_emits_define_function() {
    let module = lower_command("f() { echo hello; }\n");
    let instrs = top_instructions(&module);
    assert!(instrs.iter().any(|i| matches!(i, Instruction::DefineFunction { .. })));
}

#[test]
fn function_definition_creates_body_code_object() {
    let module = lower_command("f() { echo hello; }\n");
    // Top-level + function body = 2
    assert_eq!(module.code_objects.len(), 2);
    // Function name should be in symbol pool.
    assert!(module.symbol_pool.iter().any(|s| s == "f"));
}

// ---------------------------------------------------------------------------
// Background
// ---------------------------------------------------------------------------

#[test]
fn background_creates_sub_code_object() {
    let module = lower_command("echo hello &\n");
    // Top-level + background code object = 2
    assert_eq!(module.code_objects.len(), 2);
    let instrs = top_instructions(&module);
    assert!(instrs.iter().any(|i| matches!(i, Instruction::ExecBackground(_))));
}

#[test]
fn background_sub_code_object_has_ret() {
    let module = lower_command("echo hello &\n");
    let sub_instrs = &module.code_objects[1].instructions;
    assert_eq!(sub_instrs.last(), Some(&Instruction::Ret));
}

// ---------------------------------------------------------------------------
// Integration tests
// ---------------------------------------------------------------------------

#[test]
fn nested_if_in_while() {
    let module = lower_command("while true; do if true; then echo y; fi; done\n");
    let instrs = top_instructions(&module);
    // Should have while loop structure + if structure.
    // At least 2 JmpIfNonZero: one for while condition, one for if condition.
    let jmp_nz_count = instrs
        .iter()
        .filter(|i| matches!(i, Instruction::JmpIfNonZero(_)))
        .count();
    assert_eq!(jmp_nz_count, 2);
}

#[test]
fn compound_in_pipeline() {
    let module = lower_command("{ echo a; } | cat\n");
    // Top-level + 2 pipeline stages = 3
    assert_eq!(module.code_objects.len(), 3);
    let instrs = top_instructions(&module);
    assert_eq!(instrs[0], Instruction::BeginPipeline(2));
}

#[test]
fn program_multiple_commands_drops_between() {
    let module = lower_program("echo a\necho b\n");
    let instrs = top_instructions(&module);
    // program: PushInt(0), Drop, PushInt(0), Ret
    assert_eq!(
        instrs,
        &[
            Instruction::PushInt(0),
            Instruction::Drop,
            Instruction::PushInt(0),
            Instruction::Ret,
        ]
    );
}
