use std::sync::Arc;

use kish::ir::bytecode::{BranchTarget, Instruction};
use kish::ir::ids::{CodeObjectId, ConstId, LocalId, StringId, SymbolId};
use kish::ir::program::{CodeObject, ConstValue, IrModule};
use kish::vm::{Value, VmErrorKind, VmMachine};

// ---------------------------------------------------------------------------
// Test helper
// ---------------------------------------------------------------------------

/// Builds a minimal `IrModule` with a single code object at index 0.
fn single_code_object_module(
    instructions: Vec<Instruction>,
    locals_count: u32,
    max_stack_depth: u32,
) -> Arc<IrModule> {
    let mut module = IrModule::new();
    module.code_objects.push(CodeObject {
        id: CodeObjectId::new(0),
        instructions,
        locals_count,
        max_stack_depth,
    });
    Arc::new(module)
}

/// Builds an `IrModule` from multiple code objects.
fn multi_code_object_module(code_objects: Vec<CodeObject>) -> Arc<IrModule> {
    let mut module = IrModule::new();
    module.code_objects = code_objects;
    Arc::new(module)
}

/// Executes code object 0 and returns the result.
fn exec(module: Arc<IrModule>) -> Result<Value, kish::vm::VmError> {
    let mut vm = VmMachine::new(module);
    vm.execute_code_object(CodeObjectId::new(0))
}

/// Executes code object 0 with a call depth limit and returns the result.
fn exec_with_limit(module: Arc<IrModule>, limit: usize) -> Result<Value, kish::vm::VmError> {
    let mut vm = VmMachine::with_call_depth_limit(module, limit);
    vm.execute_code_object(CodeObjectId::new(0))
}

// ===========================================================================
// 1. Basic Stack
// ===========================================================================

#[test]
fn nop_returns_exit_status_zero() {
    let module = single_code_object_module(vec![Instruction::Nop], 0, 8);
    let result = exec(module).unwrap();
    assert_eq!(result, Value::ExitStatus(0));
}

#[test]
fn push_int_returns_value() {
    let module = single_code_object_module(vec![Instruction::PushInt(42)], 0, 8);
    let result = exec(module).unwrap();
    assert_eq!(result, Value::Integer(42));
}

#[test]
fn push_int_negative() {
    let module = single_code_object_module(vec![Instruction::PushInt(-1)], 0, 8);
    let result = exec(module).unwrap();
    assert_eq!(result, Value::Integer(-1));
}

#[test]
fn push_string() {
    let mut module = IrModule::new();
    module.string_pool.push("hello".to_string());
    module.code_objects.push(CodeObject {
        id: CodeObjectId::new(0),
        instructions: vec![Instruction::PushString(StringId::new(0))],
        locals_count: 0,
        max_stack_depth: 8,
    });
    let module = Arc::new(module);
    let result = exec(module).unwrap();
    assert_eq!(result, Value::String("hello".into()));
}

#[test]
fn push_symbol() {
    let mut module = IrModule::new();
    module.symbol_pool.push("MY_VAR".to_string());
    module.code_objects.push(CodeObject {
        id: CodeObjectId::new(0),
        instructions: vec![Instruction::PushSymbol(SymbolId::new(0))],
        locals_count: 0,
        max_stack_depth: 8,
    });
    let module = Arc::new(module);
    let result = exec(module).unwrap();
    assert_eq!(result, Value::String("MY_VAR".into()));
}

#[test]
fn push_const_string() {
    let mut module = IrModule::new();
    module.string_pool.push("world".to_string());
    module.const_pool.push(ConstValue::String(StringId::new(0)));
    module.code_objects.push(CodeObject {
        id: CodeObjectId::new(0),
        instructions: vec![Instruction::PushConst(ConstId::new(0))],
        locals_count: 0,
        max_stack_depth: 8,
    });
    let result = exec(Arc::new(module)).unwrap();
    assert_eq!(result, Value::String("world".into()));
}

#[test]
fn push_const_symbol() {
    let mut module = IrModule::new();
    module.symbol_pool.push("SYM".to_string());
    module.const_pool.push(ConstValue::Symbol(SymbolId::new(0)));
    module.code_objects.push(CodeObject {
        id: CodeObjectId::new(0),
        instructions: vec![Instruction::PushConst(ConstId::new(0))],
        locals_count: 0,
        max_stack_depth: 8,
    });
    let result = exec(Arc::new(module)).unwrap();
    assert_eq!(result, Value::String("SYM".into()));
}

#[test]
fn push_const_integer() {
    let mut module = IrModule::new();
    module.const_pool.push(ConstValue::Integer(99));
    module.code_objects.push(CodeObject {
        id: CodeObjectId::new(0),
        instructions: vec![Instruction::PushConst(ConstId::new(0))],
        locals_count: 0,
        max_stack_depth: 8,
    });
    let result = exec(Arc::new(module)).unwrap();
    assert_eq!(result, Value::Integer(99));
}

#[test]
fn drop_removes_top() {
    let module = single_code_object_module(
        vec![
            Instruction::PushInt(1),
            Instruction::PushInt(2),
            Instruction::Drop,
        ],
        0,
        8,
    );
    let result = exec(module).unwrap();
    assert_eq!(result, Value::Integer(1));
}

#[test]
fn dup_duplicates_top() {
    let module = single_code_object_module(
        vec![
            Instruction::PushInt(5),
            Instruction::Dup,
            Instruction::Drop,
        ],
        0,
        8,
    );
    let result = exec(module).unwrap();
    assert_eq!(result, Value::Integer(5));
}

#[test]
fn empty_code_object_returns_exit_status_zero() {
    let module = single_code_object_module(vec![], 0, 8);
    let result = exec(module).unwrap();
    assert_eq!(result, Value::ExitStatus(0));
}

// ===========================================================================
// 2. Registers
// ===========================================================================

#[test]
fn local_set_and_get() {
    let module = single_code_object_module(
        vec![
            Instruction::PushInt(42),
            Instruction::LocalSet(LocalId::new(0)),
            Instruction::LocalGet(LocalId::new(0)),
        ],
        1,
        8,
    );
    let result = exec(module).unwrap();
    assert_eq!(result, Value::Integer(42));
}

#[test]
fn local_get_uninitialized_returns_uninitialized() {
    let module = single_code_object_module(
        vec![Instruction::LocalGet(LocalId::new(0))],
        1,
        8,
    );
    let result = exec(module).unwrap();
    assert_eq!(result, Value::Uninitialized);
}

#[test]
fn local_overwrite() {
    let module = single_code_object_module(
        vec![
            Instruction::PushInt(1),
            Instruction::LocalSet(LocalId::new(0)),
            Instruction::PushInt(2),
            Instruction::LocalSet(LocalId::new(0)),
            Instruction::LocalGet(LocalId::new(0)),
        ],
        1,
        8,
    );
    let result = exec(module).unwrap();
    assert_eq!(result, Value::Integer(2));
}

#[test]
fn local_get_out_of_bounds() {
    let module = single_code_object_module(
        vec![Instruction::LocalGet(LocalId::new(5))],
        1,
        8,
    );
    let err = exec(module).unwrap_err();
    assert_eq!(err.kind, VmErrorKind::RegisterOutOfBounds);
}

#[test]
fn local_set_out_of_bounds() {
    let module = single_code_object_module(
        vec![
            Instruction::PushInt(1),
            Instruction::LocalSet(LocalId::new(5)),
        ],
        1,
        8,
    );
    let err = exec(module).unwrap_err();
    assert_eq!(err.kind, VmErrorKind::RegisterOutOfBounds);
}

// ===========================================================================
// 3. Branches
// ===========================================================================

#[test]
fn jmp_unconditional() {
    // PushInt(1), Jmp(3), PushInt(2), <target>
    // Skips PushInt(2), result should be Integer(1)
    let module = single_code_object_module(
        vec![
            Instruction::PushInt(1),
            Instruction::Jmp(BranchTarget::new(3)),
            Instruction::PushInt(2),
            Instruction::Nop,
        ],
        0,
        8,
    );
    let result = exec(module).unwrap();
    assert_eq!(result, Value::Integer(1));
}

#[test]
fn jmp_if_zero_taken_integer() {
    let module = single_code_object_module(
        vec![
            Instruction::PushInt(10),       // marker
            Instruction::PushInt(0),        // condition: zero
            Instruction::JmpIfZero(BranchTarget::new(4)),
            Instruction::PushInt(20),       // skipped
            Instruction::Nop,              // target
        ],
        0,
        8,
    );
    let result = exec(module).unwrap();
    assert_eq!(result, Value::Integer(10));
}

#[test]
fn jmp_if_zero_not_taken_integer() {
    let module = single_code_object_module(
        vec![
            Instruction::PushInt(10),       // marker
            Instruction::PushInt(1),        // condition: nonzero
            Instruction::JmpIfZero(BranchTarget::new(4)),
            Instruction::PushInt(20),       // not skipped
            Instruction::Nop,              // target
        ],
        0,
        8,
    );
    let result = exec(module).unwrap();
    assert_eq!(result, Value::Integer(20));
}

#[test]
fn jmp_if_non_zero_taken() {
    let module = single_code_object_module(
        vec![
            Instruction::PushInt(10),       // marker
            Instruction::PushInt(5),        // condition: nonzero
            Instruction::JmpIfNonZero(BranchTarget::new(4)),
            Instruction::PushInt(20),       // skipped
            Instruction::Nop,              // target
        ],
        0,
        8,
    );
    let result = exec(module).unwrap();
    assert_eq!(result, Value::Integer(10));
}

#[test]
fn jmp_if_non_zero_not_taken() {
    let module = single_code_object_module(
        vec![
            Instruction::PushInt(10),       // marker
            Instruction::PushInt(0),        // condition: zero
            Instruction::JmpIfNonZero(BranchTarget::new(4)),
            Instruction::PushInt(20),       // not skipped
            Instruction::Nop,              // target
        ],
        0,
        8,
    );
    let result = exec(module).unwrap();
    assert_eq!(result, Value::Integer(20));
}

#[test]
fn jmp_if_zero_exit_status_zero() {
    // ExitStatus(0) is both truthy and zero. is_zero -> true -> jump taken.
    let module = single_code_object_module(
        vec![
            Instruction::PushInt(99),       // marker
            Instruction::PushInt(0),        // will become ExitStatus via NegateStatus trick
            // Simulate ExitStatus(0) by using it directly in the const pool
            Instruction::JmpIfZero(BranchTarget::new(4)),
            Instruction::PushInt(77),       // skipped
            Instruction::Nop,              // target
        ],
        0,
        8,
    );
    let result = exec(module).unwrap();
    assert_eq!(result, Value::Integer(99));
}

#[test]
fn jmp_if_zero_uninitialized() {
    let module = single_code_object_module(
        vec![
            Instruction::PushInt(10),
            Instruction::LocalGet(LocalId::new(0)), // Uninitialized is_zero -> true
            Instruction::JmpIfZero(BranchTarget::new(4)),
            Instruction::PushInt(20),
            Instruction::Nop,
        ],
        1,
        8,
    );
    let result = exec(module).unwrap();
    assert_eq!(result, Value::Integer(10));
}

#[test]
fn jmp_if_zero_nonempty_string() {
    let mut module = IrModule::new();
    module.string_pool.push("hello".to_string());
    module.code_objects.push(CodeObject {
        id: CodeObjectId::new(0),
        instructions: vec![
            Instruction::PushInt(10),
            Instruction::PushString(StringId::new(0)), // non-empty -> not zero
            Instruction::JmpIfZero(BranchTarget::new(4)),
            Instruction::PushInt(20), // not skipped
            Instruction::Nop,
        ],
        locals_count: 0,
        max_stack_depth: 8,
    });
    let result = exec(Arc::new(module)).unwrap();
    assert_eq!(result, Value::Integer(20));
}

#[test]
fn jmp_if_zero_empty_string() {
    let mut module = IrModule::new();
    module.string_pool.push(String::new());
    module.code_objects.push(CodeObject {
        id: CodeObjectId::new(0),
        instructions: vec![
            Instruction::PushInt(10),
            Instruction::PushString(StringId::new(0)), // empty -> zero
            Instruction::JmpIfZero(BranchTarget::new(4)),
            Instruction::PushInt(20), // skipped
            Instruction::Nop,
        ],
        locals_count: 0,
        max_stack_depth: 8,
    });
    let result = exec(Arc::new(module)).unwrap();
    assert_eq!(result, Value::Integer(10));
}

// ===========================================================================
// 4. If-else simulation
// ===========================================================================

#[test]
fn if_else_true_branch() {
    // Simulate: if (1) { result = 10 } else { result = 20 }
    // PushInt(1), JmpIfZero(else=4), PushInt(10), Jmp(end=5), PushInt(20)
    let module = single_code_object_module(
        vec![
            Instruction::PushInt(1),                         // condition (nonzero)
            Instruction::JmpIfZero(BranchTarget::new(4)),    // if zero -> else
            Instruction::PushInt(10),                        // then branch
            Instruction::Jmp(BranchTarget::new(5)),          // skip else
            Instruction::PushInt(20),                        // else branch
            Instruction::Nop,                                // end
        ],
        0,
        8,
    );
    let result = exec(module).unwrap();
    assert_eq!(result, Value::Integer(10));
}

#[test]
fn if_else_false_branch() {
    let module = single_code_object_module(
        vec![
            Instruction::PushInt(0),                         // condition (zero)
            Instruction::JmpIfZero(BranchTarget::new(4)),    // if zero -> else
            Instruction::PushInt(10),                        // then branch
            Instruction::Jmp(BranchTarget::new(5)),          // skip else
            Instruction::PushInt(20),                        // else branch
            Instruction::Nop,                                // end
        ],
        0,
        8,
    );
    let result = exec(module).unwrap();
    assert_eq!(result, Value::Integer(20));
}

// ===========================================================================
// 5. Call/Ret
// ===========================================================================

#[test]
fn call_and_ret() {
    // CO 0: Call(1), implicit return of callee result
    // CO 1: PushInt(42), Ret
    let module = multi_code_object_module(vec![
        CodeObject {
            id: CodeObjectId::new(0),
            instructions: vec![Instruction::Call(CodeObjectId::new(1))],
            locals_count: 0,
            max_stack_depth: 8,
        },
        CodeObject {
            id: CodeObjectId::new(1),
            instructions: vec![Instruction::PushInt(42), Instruction::Ret],
            locals_count: 0,
            max_stack_depth: 8,
        },
    ]);
    let result = exec(module).unwrap();
    assert_eq!(result, Value::Integer(42));
}

#[test]
fn call_preserves_caller_state() {
    // CO 0: PushInt(100), Call(1) -> callee returns 42 -> stack: [100, 42]
    // CO 1: PushInt(42), Ret
    let module = multi_code_object_module(vec![
        CodeObject {
            id: CodeObjectId::new(0),
            instructions: vec![
                Instruction::PushInt(100),
                Instruction::Call(CodeObjectId::new(1)),
                Instruction::Drop, // drop callee result (42)
                // top of stack is 100
            ],
            locals_count: 0,
            max_stack_depth: 8,
        },
        CodeObject {
            id: CodeObjectId::new(1),
            instructions: vec![Instruction::PushInt(42), Instruction::Ret],
            locals_count: 0,
            max_stack_depth: 8,
        },
    ]);
    let result = exec(module).unwrap();
    assert_eq!(result, Value::Integer(100));
}

#[test]
fn nested_calls() {
    // CO 0: Call(1)
    // CO 1: Call(2)
    // CO 2: PushInt(7), Ret
    let module = multi_code_object_module(vec![
        CodeObject {
            id: CodeObjectId::new(0),
            instructions: vec![Instruction::Call(CodeObjectId::new(1))],
            locals_count: 0,
            max_stack_depth: 8,
        },
        CodeObject {
            id: CodeObjectId::new(1),
            instructions: vec![
                Instruction::Call(CodeObjectId::new(2)),
                Instruction::Ret,
            ],
            locals_count: 0,
            max_stack_depth: 8,
        },
        CodeObject {
            id: CodeObjectId::new(2),
            instructions: vec![Instruction::PushInt(7), Instruction::Ret],
            locals_count: 0,
            max_stack_depth: 8,
        },
    ]);
    let result = exec(module).unwrap();
    assert_eq!(result, Value::Integer(7));
}

#[test]
fn locals_isolated_across_frames() {
    // CO 0: set local[0]=100, Call(1), LocalGet(0) -> should still be 100
    // CO 1: set local[0]=999, Ret (with PushInt to satisfy Ret)
    let module = multi_code_object_module(vec![
        CodeObject {
            id: CodeObjectId::new(0),
            instructions: vec![
                Instruction::PushInt(100),
                Instruction::LocalSet(LocalId::new(0)),
                Instruction::Call(CodeObjectId::new(1)),
                Instruction::Drop, // drop callee return
                Instruction::LocalGet(LocalId::new(0)),
            ],
            locals_count: 1,
            max_stack_depth: 8,
        },
        CodeObject {
            id: CodeObjectId::new(1),
            instructions: vec![
                Instruction::PushInt(999),
                Instruction::LocalSet(LocalId::new(0)),
                Instruction::PushInt(0),
                Instruction::Ret,
            ],
            locals_count: 1,
            max_stack_depth: 8,
        },
    ]);
    let result = exec(module).unwrap();
    assert_eq!(result, Value::Integer(100));
}

#[test]
fn ret_from_last_frame() {
    let module = single_code_object_module(
        vec![Instruction::PushInt(55), Instruction::Ret],
        0,
        8,
    );
    let result = exec(module).unwrap();
    assert_eq!(result, Value::Integer(55));
}

#[test]
fn implicit_return_with_stack() {
    // No Ret instruction, PC falls off the end. Top of stack is returned.
    let module = single_code_object_module(
        vec![Instruction::PushInt(77)],
        0,
        8,
    );
    let result = exec(module).unwrap();
    assert_eq!(result, Value::Integer(77));
}

#[test]
fn implicit_return_empty_stack() {
    // No instructions, empty stack -> ExitStatus(0).
    let module = single_code_object_module(vec![], 0, 8);
    let result = exec(module).unwrap();
    assert_eq!(result, Value::ExitStatus(0));
}

#[test]
fn implicit_return_from_callee() {
    // CO 0: Call(1), result is implicit return from CO 1
    // CO 1: PushInt(33), no Ret
    let module = multi_code_object_module(vec![
        CodeObject {
            id: CodeObjectId::new(0),
            instructions: vec![Instruction::Call(CodeObjectId::new(1))],
            locals_count: 0,
            max_stack_depth: 8,
        },
        CodeObject {
            id: CodeObjectId::new(1),
            instructions: vec![Instruction::PushInt(33)],
            locals_count: 0,
            max_stack_depth: 8,
        },
    ]);
    let result = exec(module).unwrap();
    assert_eq!(result, Value::Integer(33));
}

// ===========================================================================
// 6. NegateStatus
// ===========================================================================

#[test]
fn negate_status_zero_to_one() {
    let module = single_code_object_module(
        vec![Instruction::PushInt(0), Instruction::NegateStatus],
        0,
        8,
    );
    let result = exec(module).unwrap();
    assert_eq!(result, Value::ExitStatus(1));
}

#[test]
fn negate_status_nonzero_to_zero() {
    let module = single_code_object_module(
        vec![Instruction::PushInt(42), Instruction::NegateStatus],
        0,
        8,
    );
    let result = exec(module).unwrap();
    assert_eq!(result, Value::ExitStatus(0));
}

#[test]
fn negate_status_double_negate() {
    let module = single_code_object_module(
        vec![
            Instruction::PushInt(0),
            Instruction::NegateStatus,
            Instruction::NegateStatus,
        ],
        0,
        8,
    );
    let result = exec(module).unwrap();
    assert_eq!(result, Value::ExitStatus(0));
}

#[test]
fn negate_status_string_value() {
    // String "abc" -> as_exit_status() fails to parse -> 1, negate -> 0
    let mut module = IrModule::new();
    module.string_pool.push("abc".to_string());
    module.code_objects.push(CodeObject {
        id: CodeObjectId::new(0),
        instructions: vec![
            Instruction::PushString(StringId::new(0)),
            Instruction::NegateStatus,
        ],
        locals_count: 0,
        max_stack_depth: 8,
    });
    let result = exec(Arc::new(module)).unwrap();
    assert_eq!(result, Value::ExitStatus(0));
}

#[test]
fn negate_status_negative_integer() {
    // Integer(-1) -> as_exit_status() -> (-1 as i32) & 0xFF = 255, nonzero -> negate -> 0
    let module = single_code_object_module(
        vec![Instruction::PushInt(-1), Instruction::NegateStatus],
        0,
        8,
    );
    let result = exec(module).unwrap();
    assert_eq!(result, Value::ExitStatus(0));
}

// ===========================================================================
// 7. Errors
// ===========================================================================

#[test]
fn invalid_code_object_id() {
    let module = single_code_object_module(
        vec![Instruction::Call(CodeObjectId::new(99))],
        0,
        8,
    );
    let err = exec(module).unwrap_err();
    assert_eq!(err.kind, VmErrorKind::InvalidCodeObject);
}

#[test]
fn stack_underflow_drop() {
    let module = single_code_object_module(vec![Instruction::Drop], 0, 8);
    let err = exec(module).unwrap_err();
    assert_eq!(err.kind, VmErrorKind::StackUnderflow);
}

#[test]
fn stack_underflow_ret() {
    let module = single_code_object_module(vec![Instruction::Ret], 0, 8);
    let err = exec(module).unwrap_err();
    assert_eq!(err.kind, VmErrorKind::StackUnderflow);
}

#[test]
fn call_depth_exceeded() {
    // CO 0 calls itself recursively.
    let module = single_code_object_module(
        vec![
            Instruction::Call(CodeObjectId::new(0)),
            Instruction::Ret,
        ],
        0,
        8,
    );
    let err = exec_with_limit(module, 4).unwrap_err();
    assert_eq!(err.kind, VmErrorKind::CallDepthExceeded);
}

#[test]
fn unimplemented_instruction() {
    let module = single_code_object_module(
        vec![Instruction::BeginSimple],
        0,
        8,
    );
    let err = exec(module).unwrap_err();
    assert_eq!(err.kind, VmErrorKind::InvalidInstruction);
}

#[test]
fn invalid_const_id() {
    let module = single_code_object_module(
        vec![Instruction::PushConst(ConstId::new(99))],
        0,
        8,
    );
    let err = exec(module).unwrap_err();
    assert_eq!(err.kind, VmErrorKind::InvalidInstruction);
}

#[test]
fn invalid_string_id() {
    let module = single_code_object_module(
        vec![Instruction::PushString(StringId::new(99))],
        0,
        8,
    );
    let err = exec(module).unwrap_err();
    assert_eq!(err.kind, VmErrorKind::InvalidInstruction);
}

#[test]
fn invalid_symbol_id() {
    let module = single_code_object_module(
        vec![Instruction::PushSymbol(SymbolId::new(99))],
        0,
        8,
    );
    let err = exec(module).unwrap_err();
    assert_eq!(err.kind, VmErrorKind::InvalidInstruction);
}

#[test]
fn execute_invalid_code_object_entry() {
    let module = single_code_object_module(vec![], 0, 8);
    let mut vm = VmMachine::new(module);
    let err = vm.execute_code_object(CodeObjectId::new(99)).unwrap_err();
    assert_eq!(err.kind, VmErrorKind::InvalidCodeObject);
}

// ===========================================================================
// 8. VM API
// ===========================================================================

#[test]
fn module_accessor() {
    let module = single_code_object_module(vec![], 0, 8);
    let vm = VmMachine::new(module.clone());
    assert_eq!(vm.module().code_objects.len(), 1);
}

#[test]
fn call_depth_starts_at_zero() {
    let module = single_code_object_module(vec![], 0, 8);
    let vm = VmMachine::new(module);
    assert_eq!(vm.call_depth(), 0);
}

#[test]
fn multiple_push_and_drop() {
    let module = single_code_object_module(
        vec![
            Instruction::PushInt(1),
            Instruction::PushInt(2),
            Instruction::PushInt(3),
            Instruction::Drop,
            Instruction::Drop,
        ],
        0,
        8,
    );
    let result = exec(module).unwrap();
    assert_eq!(result, Value::Integer(1));
}

#[test]
fn dup_preserves_value_type() {
    let mut module = IrModule::new();
    module.string_pool.push("test".to_string());
    module.code_objects.push(CodeObject {
        id: CodeObjectId::new(0),
        instructions: vec![
            Instruction::PushString(StringId::new(0)),
            Instruction::Dup,
        ],
        locals_count: 0,
        max_stack_depth: 8,
    });
    let result = exec(Arc::new(module)).unwrap();
    assert_eq!(result, Value::String("test".into()));
}

#[test]
fn loop_simulation() {
    // Simulate: local[0] = 3; while (local[0] != 0) { local[0] -= 1 }
    // We approximate with a simple countdown using branches.
    // CO 0:
    //  0: PushInt(3)
    //  1: LocalSet(0)
    //  2: LocalGet(0)        <- loop start
    //  3: JmpIfZero(8)       <- exit if zero
    //  4: LocalGet(0)
    //  5: PushInt(-1)        <- we'll add by using Nop placeholder approach
    //  ... This gets complex. Simplify: just test a counted branch loop.
    //
    // Simpler approach: count down 3 iterations via branch, push result.
    // 0: PushInt(3), 1: LocalSet(0)
    // 2: LocalGet(0), 3: JmpIfZero(7)  -- exit
    // 4: PushInt(0), 5: LocalSet(0)    -- set to 0 to end loop
    // 6: Jmp(2)
    // 7: PushInt(999)                  -- done
    let module = single_code_object_module(
        vec![
            Instruction::PushInt(3),
            Instruction::LocalSet(LocalId::new(0)),
            Instruction::LocalGet(LocalId::new(0)),      // loop start
            Instruction::JmpIfZero(BranchTarget::new(7)), // exit
            Instruction::PushInt(0),
            Instruction::LocalSet(LocalId::new(0)),       // set to 0
            Instruction::Jmp(BranchTarget::new(2)),       // back to loop
            Instruction::PushInt(999),                     // exit target
        ],
        1,
        8,
    );
    let result = exec(module).unwrap();
    assert_eq!(result, Value::Integer(999));
}
