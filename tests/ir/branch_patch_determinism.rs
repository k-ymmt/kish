use kish::ir::{CodeObjectBuilder, CodeObjectId, Instruction};

fn build_fixture_code_object() -> Vec<Instruction> {
    let mut builder = CodeObjectBuilder::new(CodeObjectId::new(10));

    let entry = builder.new_label();
    let then_label = builder.new_label();
    let end_label = builder.new_label();

    builder
        .bind_label(entry)
        .expect("entry bind should succeed");
    builder
        .emit(Instruction::PushInt(0))
        .expect("emit should succeed");
    builder
        .emit_jmp_if_zero(then_label)
        .expect("emit should succeed");
    builder
        .emit(Instruction::PushInt(100))
        .expect("emit should succeed");
    builder.emit_jmp(end_label).expect("emit should succeed");

    builder
        .bind_label(then_label)
        .expect("then bind should succeed");
    builder
        .emit(Instruction::PushInt(200))
        .expect("emit should succeed");

    builder
        .bind_label(end_label)
        .expect("end bind should succeed");
    builder.emit(Instruction::Ret).expect("emit should succeed");

    builder
        .finalize()
        .expect("finalize should succeed")
        .instructions
}

#[test]
fn branch_fixup_is_deterministic() {
    let first = build_fixture_code_object();
    let second = build_fixture_code_object();
    assert_eq!(first, second);
}
