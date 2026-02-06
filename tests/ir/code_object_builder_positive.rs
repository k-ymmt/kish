use kish::ir::{BranchTarget, CodeObjectBuilder, CodeObjectId, Instruction};

#[test]
fn forward_jump_is_patched_to_absolute_target() {
    let mut builder = CodeObjectBuilder::new(CodeObjectId::new(0));

    let target = builder.new_label();
    let jmp_index = builder.emit_jmp(target).expect("jump emit should succeed");
    assert_eq!(jmp_index, 0);

    builder
        .emit(Instruction::PushInt(1))
        .expect("emit should succeed");
    builder
        .bind_label(target)
        .expect("label bind should succeed");
    builder.emit(Instruction::Ret).expect("emit should succeed");

    let code = builder.finalize().expect("finalize should succeed");
    assert_eq!(code.instructions.len(), 3);
    assert_eq!(code.instructions[0], Instruction::Jmp(BranchTarget::new(2)));
}

#[test]
fn backward_jump_is_patched_to_absolute_target() {
    let mut builder = CodeObjectBuilder::new(CodeObjectId::new(1));

    let loop_start = builder.new_label();
    builder
        .bind_label(loop_start)
        .expect("label bind should succeed");
    builder
        .emit(Instruction::PushInt(1))
        .expect("emit should succeed");
    builder
        .emit_jmp_if_non_zero(loop_start)
        .expect("jump emit should succeed");

    let code = builder.finalize().expect("finalize should succeed");
    assert_eq!(
        code.instructions[1],
        Instruction::JmpIfNonZero(BranchTarget::new(0))
    );
}

#[test]
fn mixed_control_flow_patches_all_branches() {
    let mut builder = CodeObjectBuilder::new(CodeObjectId::new(2));

    let then_label = builder.new_label();
    let end_label = builder.new_label();

    builder
        .emit(Instruction::PushInt(0))
        .expect("emit should succeed");
    builder
        .emit_jmp_if_zero(then_label)
        .expect("emit should succeed");
    builder
        .emit(Instruction::PushInt(10))
        .expect("emit should succeed");
    builder.emit_jmp(end_label).expect("emit should succeed");

    builder
        .bind_label(then_label)
        .expect("label bind should succeed");
    builder
        .emit(Instruction::PushInt(20))
        .expect("emit should succeed");

    builder
        .bind_label(end_label)
        .expect("label bind should succeed");
    builder.emit(Instruction::Ret).expect("emit should succeed");

    builder.set_locals_count(3);
    builder.note_stack_depth(5);
    builder.note_stack_depth(2);

    let code = builder.finalize().expect("finalize should succeed");
    assert_eq!(code.locals_count, 3);
    assert_eq!(code.max_stack_depth, 5);
    assert_eq!(
        code.instructions,
        vec![
            Instruction::PushInt(0),
            Instruction::JmpIfZero(BranchTarget::new(4)),
            Instruction::PushInt(10),
            Instruction::Jmp(BranchTarget::new(5)),
            Instruction::PushInt(20),
            Instruction::Ret,
        ]
    );
}
