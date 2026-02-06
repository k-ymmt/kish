use kish::ir::{CodeObjectBuilder, CodeObjectId, Instruction, IrErrorKind, LabelId};

#[test]
fn duplicate_label_bind_returns_invariant_violation() {
    let mut builder = CodeObjectBuilder::new(CodeObjectId::new(0));
    let label = builder.new_label();

    builder
        .bind_label(label)
        .expect("first bind should succeed");
    let error = builder
        .bind_label(label)
        .expect_err("duplicate bind must fail");

    assert_eq!(error.kind, IrErrorKind::InvariantViolation);
}

#[test]
fn unresolved_label_at_finalize_returns_invariant_violation() {
    let mut builder = CodeObjectBuilder::new(CodeObjectId::new(1));
    let label = builder.new_label();

    builder.emit_jmp(label).expect("jump emit should succeed");
    let error = builder
        .finalize()
        .expect_err("finalize should fail for unresolved target");

    assert_eq!(error.kind, IrErrorKind::InvariantViolation);
}

#[test]
fn unknown_label_usage_returns_invariant_violation() {
    let mut builder = CodeObjectBuilder::new(CodeObjectId::new(2));

    let error = builder
        .emit_jmp(LabelId::new(999))
        .expect_err("unknown label should fail");

    assert_eq!(error.kind, IrErrorKind::InvariantViolation);
}

#[test]
fn instruction_limit_exceeded_returns_limit_exceeded() {
    let mut builder = CodeObjectBuilder::with_limits(CodeObjectId::new(3), 1);

    builder
        .emit(Instruction::PushInt(1))
        .expect("first instruction should fit in limit");
    let error = builder
        .emit(Instruction::PushInt(2))
        .expect_err("second instruction should exceed limit");

    assert_eq!(error.kind, IrErrorKind::LimitExceeded);
}
