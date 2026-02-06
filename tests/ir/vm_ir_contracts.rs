use kish::ir::{
    BranchTarget, CodeObjectId, ConstId, Instruction, LocalId, RedirectProgramId, StringId,
    SymbolId, WordProgramId,
};

#[test]
fn branch_target_accessors_are_stable() {
    let target = BranchTarget::new(42);
    assert_eq!(target.index(), 42);
}

#[test]
fn instruction_variants_cover_phase2_contract() {
    let _instructions = vec![
        Instruction::Nop,
        Instruction::PushConst(ConstId::new(0)),
        Instruction::PushInt(123),
        Instruction::PushString(StringId::new(1)),
        Instruction::PushSymbol(SymbolId::new(2)),
        Instruction::Drop,
        Instruction::Dup,
        Instruction::LocalGet(LocalId::new(0)),
        Instruction::LocalSet(LocalId::new(0)),
        Instruction::Jmp(BranchTarget::new(7)),
        Instruction::JmpIfZero(BranchTarget::new(8)),
        Instruction::JmpIfNonZero(BranchTarget::new(9)),
        Instruction::Call(CodeObjectId::new(3)),
        Instruction::Ret,
        Instruction::BeginSimple,
        Instruction::AddArg(WordProgramId::new(1)),
        Instruction::AddAssign(SymbolId::new(4), WordProgramId::new(5)),
        Instruction::AddRedir(RedirectProgramId::new(6)),
        Instruction::EndSimple,
        Instruction::ExecSimple,
    ];
}
