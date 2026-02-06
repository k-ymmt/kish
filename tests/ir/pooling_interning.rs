use kish::ir::{
    CodeObject, CodeObjectId, ConstValue, Instruction, IrErrorKind, IrModuleBuilder, IrOptions,
    LoweringContext, RedirectProgram, RedirectProgramOp, StringId, SymbolId, WordProgram,
    WordProgramOp,
};

fn code_object_with_nops(id: u32, count: usize) -> CodeObject {
    CodeObject {
        id: CodeObjectId::new(id),
        instructions: vec![Instruction::Nop; count],
        locals_count: 0,
        max_stack_depth: 0,
    }
}

#[test]
fn string_and_symbol_interning_are_deduplicated_with_stable_ids() {
    let mut builder = IrModuleBuilder::new(IrOptions::default());

    let s0 = builder.intern_string("echo").expect("string should intern");
    let s1 = builder
        .intern_string("printf")
        .expect("string should intern");
    let s0_repeat = builder.intern_string("echo").expect("string should dedup");

    let sym0 = builder.intern_symbol("PATH").expect("symbol should intern");
    let sym0_repeat = builder.intern_symbol("PATH").expect("symbol should dedup");
    let sym1 = builder.intern_symbol("HOME").expect("symbol should intern");

    assert_eq!(s0.value(), 0);
    assert_eq!(s1.value(), 1);
    assert_eq!(s0, s0_repeat);
    assert_eq!(sym0.value(), 0);
    assert_eq!(sym1.value(), 1);
    assert_eq!(sym0, sym0_repeat);

    let module = builder.finish();
    assert_eq!(
        module.string_pool,
        vec!["echo".to_string(), "printf".to_string()]
    );
    assert_eq!(
        module.symbol_pool,
        vec!["PATH".to_string(), "HOME".to_string()]
    );
    assert_eq!(module.string_at(s0), Some("echo"));
    assert_eq!(module.symbol_at(sym1), Some("HOME"));
}

#[test]
fn constant_interning_deduplicates_integer_string_and_symbol_entries() {
    let mut builder = IrModuleBuilder::new(IrOptions::default());

    let int0 = builder
        .intern_const_integer(42)
        .expect("integer const should intern");
    let int0_repeat = builder
        .intern_const_integer(42)
        .expect("integer const should dedup");
    let str0 = builder
        .intern_const_string("hello")
        .expect("string const should intern");
    let str0_repeat = builder
        .intern_const(ConstValue::String(StringId::new(0)))
        .expect("same string const should dedup");
    let sym0 = builder
        .intern_const_symbol("PATH")
        .expect("symbol const should intern");
    let sym0_repeat = builder
        .intern_const(ConstValue::Symbol(SymbolId::new(0)))
        .expect("same symbol const should dedup");

    assert_eq!(int0, int0_repeat);
    assert_eq!(str0, str0_repeat);
    assert_eq!(sym0, sym0_repeat);

    let module = builder.finish();
    assert_eq!(module.string_pool, vec!["hello".to_string()]);
    assert_eq!(module.symbol_pool, vec!["PATH".to_string()]);
    assert_eq!(
        module.const_pool,
        vec![
            ConstValue::Integer(42),
            ConstValue::String(StringId::new(0)),
            ConstValue::Symbol(SymbolId::new(0)),
        ]
    );
}

#[test]
fn max_consts_limit_is_enforced_only_for_unique_constants() {
    let options = IrOptions {
        max_consts: 1,
        ..IrOptions::default()
    };
    let mut builder = IrModuleBuilder::new(options);

    let first = builder
        .intern_const_integer(1)
        .expect("first const should fit");
    let duplicate = builder
        .intern_const_integer(1)
        .expect("duplicate const should reuse existing entry");
    assert_eq!(first, duplicate);

    let error = builder
        .intern_const_integer(2)
        .expect_err("second unique const must exceed limit");
    assert_eq!(error.kind, IrErrorKind::LimitExceeded);
    assert_eq!(error.message, "constant pool limit exceeded");
    assert_eq!(
        error.detail.as_deref(),
        Some("max_consts=1, attempted_count=2")
    );
}

#[test]
fn invalid_constant_reference_handles_are_rejected() {
    let mut builder = IrModuleBuilder::new(IrOptions::default());

    let string_error = builder
        .intern_const(ConstValue::String(StringId::new(0)))
        .expect_err("string const with unknown id must fail");
    assert_eq!(string_error.kind, IrErrorKind::InvariantViolation);
    assert_eq!(
        string_error.message,
        "constant references unknown string id"
    );

    builder
        .intern_string("ok")
        .expect("string interning should succeed");
    let symbol_error = builder
        .intern_const(ConstValue::Symbol(SymbolId::new(1)))
        .expect_err("symbol const with unknown id must fail");
    assert_eq!(symbol_error.kind, IrErrorKind::InvariantViolation);
    assert_eq!(
        symbol_error.message,
        "constant references unknown symbol id"
    );
}

#[test]
fn code_object_count_limit_is_enforced_and_ids_are_reassigned() {
    let options = IrOptions {
        max_code_objects: 1,
        max_instructions: 100,
        ..IrOptions::default()
    };
    let mut builder = IrModuleBuilder::new(options);

    let id = builder
        .add_code_object(code_object_with_nops(42, 1))
        .expect("first code object should fit");
    assert_eq!(id.value(), 0);

    let error = builder
        .add_code_object(code_object_with_nops(99, 1))
        .expect_err("second code object must exceed limit");
    assert_eq!(error.kind, IrErrorKind::LimitExceeded);
    assert_eq!(error.message, "code object limit exceeded");
    assert_eq!(
        error.detail.as_deref(),
        Some("max_code_objects=1, attempted_count=2")
    );

    let module = builder.finish();
    assert_eq!(module.code_objects.len(), 1);
    assert_eq!(module.code_objects[0].id.value(), 0);
}

#[test]
fn module_instruction_limit_is_enforced_cumulatively() {
    let options = IrOptions {
        max_instructions: 3,
        max_code_objects: 8,
        ..IrOptions::default()
    };
    let mut builder = IrModuleBuilder::new(options);

    builder
        .add_code_object(code_object_with_nops(0, 2))
        .expect("first object should fit");
    let error = builder
        .add_code_object(code_object_with_nops(1, 2))
        .expect_err("cumulative instruction count must exceed limit");

    assert_eq!(error.kind, IrErrorKind::LimitExceeded);
    assert_eq!(error.message, "module instruction limit exceeded");
    assert_eq!(
        error.detail.as_deref(),
        Some("max_instructions=3, attempted_total=4")
    );
}

#[test]
fn word_and_redirect_program_op_limits_are_enforced_per_program() {
    let options = IrOptions {
        max_word_program_ops: 1,
        max_redirect_ops: 1,
        ..IrOptions::default()
    };
    let mut builder = IrModuleBuilder::new(options);

    let word_id = builder
        .add_word_program(WordProgram {
            ops: vec![WordProgramOp::FieldSplit],
            ..WordProgram::default()
        })
        .expect("word program should fit");
    assert_eq!(word_id.value(), 0);

    let word_error = builder
        .add_word_program(WordProgram {
            ops: vec![WordProgramOp::FieldSplit, WordProgramOp::Glob],
            ..WordProgram::default()
        })
        .expect_err("word program must exceed op limit");
    assert_eq!(word_error.kind, IrErrorKind::LimitExceeded);
    assert_eq!(word_error.message, "word program op limit exceeded");
    assert_eq!(
        word_error.detail.as_deref(),
        Some("max_word_program_ops=1, attempted_ops=2")
    );

    let redirect_id = builder
        .add_redirect_program(RedirectProgram {
            ops: vec![RedirectProgramOp::Close { fd: 1 }],
            ..RedirectProgram::default()
        })
        .expect("redirect program should fit");
    assert_eq!(redirect_id.value(), 0);

    let redirect_error = builder
        .add_redirect_program(RedirectProgram {
            ops: vec![
                RedirectProgramOp::Close { fd: 1 },
                RedirectProgramOp::Close { fd: 2 },
            ],
            ..RedirectProgram::default()
        })
        .expect_err("redirect program must exceed op limit");
    assert_eq!(redirect_error.kind, IrErrorKind::LimitExceeded);
    assert_eq!(redirect_error.message, "redirect program op limit exceeded");
    assert_eq!(
        redirect_error.detail.as_deref(),
        Some("max_redirect_ops=1, attempted_ops=2")
    );
}

#[test]
fn lowering_context_can_create_module_builder_with_same_options() {
    let options = IrOptions {
        max_instructions: 10,
        max_consts: 11,
        max_code_objects: 12,
        max_word_program_ops: 13,
        max_redirect_ops: 14,
        max_arith_program_ops: 16,
        max_arity: 15,
    };
    let context = LoweringContext::new(options);
    let builder = context.module_builder();

    assert_eq!(builder.options(), options);
}
