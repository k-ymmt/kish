//! Tests for `Frame` operand stack and register file operations.

use kish::ir::ids::{CodeObjectId, LocalId};
use kish::vm::{Frame, Value, VmErrorKind};

// ---------------------------------------------------------------------------
// Construction
// ---------------------------------------------------------------------------

#[test]
fn new_frame_has_zero_pc() {
    let frame = Frame::new(CodeObjectId::new(1), 4, 8);
    assert_eq!(frame.pc(), 0);
}

#[test]
fn new_frame_has_correct_code_object_id() {
    let id = CodeObjectId::new(42);
    let frame = Frame::new(id, 0, 8);
    assert_eq!(frame.code_object_id(), id);
}

#[test]
fn new_frame_registers_initialized_to_uninitialized() {
    let frame = Frame::new(CodeObjectId::new(0), 3, 8);
    for i in 0..3 {
        assert_eq!(
            frame.local_get(LocalId::new(i)).unwrap(),
            &Value::Uninitialized
        );
    }
}

#[test]
fn new_frame_has_empty_stack() {
    let frame = Frame::new(CodeObjectId::new(0), 0, 8);
    assert_eq!(frame.stack_depth(), 0);
}

#[test]
fn new_frame_zero_locals() {
    let frame = Frame::new(CodeObjectId::new(0), 0, 8);
    let err = frame.local_get(LocalId::new(0)).unwrap_err();
    assert_eq!(err.kind, VmErrorKind::RegisterOutOfBounds);
}

#[test]
fn new_frame_builder_fields_are_none() {
    let frame = Frame::new(CodeObjectId::new(0), 0, 8);
    assert!(frame.command_builder.is_none());
    assert!(frame.pipeline_builder.is_none());
    assert!(frame.for_state.is_none());
    assert!(frame.case_state.is_none());
}

// ---------------------------------------------------------------------------
// PC
// ---------------------------------------------------------------------------

#[test]
fn advance_pc_returns_current_and_increments() {
    let mut frame = Frame::new(CodeObjectId::new(0), 0, 8);
    assert_eq!(frame.advance_pc(), 0);
    assert_eq!(frame.advance_pc(), 1);
    assert_eq!(frame.advance_pc(), 2);
    assert_eq!(frame.pc(), 3);
}

#[test]
fn set_pc_changes_counter() {
    let mut frame = Frame::new(CodeObjectId::new(0), 0, 8);
    frame.set_pc(42);
    assert_eq!(frame.pc(), 42);
}

// ---------------------------------------------------------------------------
// Stack push / pop
// ---------------------------------------------------------------------------

#[test]
fn push_then_pop() {
    let mut frame = Frame::new(CodeObjectId::new(0), 0, 8);
    frame.push(Value::Integer(10)).unwrap();
    assert_eq!(frame.pop().unwrap(), Value::Integer(10));
}

#[test]
fn stack_lifo_order() {
    let mut frame = Frame::new(CodeObjectId::new(0), 0, 8);
    frame.push(Value::Integer(1)).unwrap();
    frame.push(Value::Integer(2)).unwrap();
    frame.push(Value::Integer(3)).unwrap();
    assert_eq!(frame.pop().unwrap(), Value::Integer(3));
    assert_eq!(frame.pop().unwrap(), Value::Integer(2));
    assert_eq!(frame.pop().unwrap(), Value::Integer(1));
}

#[test]
fn pop_underflow_error() {
    let mut frame = Frame::new(CodeObjectId::new(0), 0, 8);
    let err = frame.pop().unwrap_err();
    assert_eq!(err.kind, VmErrorKind::StackUnderflow);
}

#[test]
fn push_overflow_error() {
    let mut frame = Frame::new(CodeObjectId::new(0), 0, 2);
    frame.push(Value::Integer(1)).unwrap();
    frame.push(Value::Integer(2)).unwrap();
    let err = frame.push(Value::Integer(3)).unwrap_err();
    assert_eq!(err.kind, VmErrorKind::StackOverflow);
}

#[test]
fn push_at_exact_max_succeeds() {
    let mut frame = Frame::new(CodeObjectId::new(0), 0, 3);
    frame.push(Value::Integer(1)).unwrap();
    frame.push(Value::Integer(2)).unwrap();
    frame.push(Value::Integer(3)).unwrap();
    assert_eq!(frame.stack_depth(), 3);
}

#[test]
fn push_one_past_max_fails() {
    let mut frame = Frame::new(CodeObjectId::new(0), 0, 3);
    for i in 0..3 {
        frame.push(Value::Integer(i)).unwrap();
    }
    let err = frame.push(Value::Integer(99)).unwrap_err();
    assert_eq!(err.kind, VmErrorKind::StackOverflow);
}

#[test]
fn stack_depth_tracking() {
    let mut frame = Frame::new(CodeObjectId::new(0), 0, 8);
    assert_eq!(frame.stack_depth(), 0);
    frame.push(Value::Integer(1)).unwrap();
    assert_eq!(frame.stack_depth(), 1);
    frame.push(Value::Integer(2)).unwrap();
    assert_eq!(frame.stack_depth(), 2);
    frame.pop().unwrap();
    assert_eq!(frame.stack_depth(), 1);
}

// ---------------------------------------------------------------------------
// Stack peek
// ---------------------------------------------------------------------------

#[test]
fn peek_returns_top_without_removing() {
    let mut frame = Frame::new(CodeObjectId::new(0), 0, 8);
    frame.push(Value::String("hello".into())).unwrap();
    assert_eq!(frame.peek().unwrap(), &Value::String("hello".into()));
    assert_eq!(frame.stack_depth(), 1);
}

#[test]
fn peek_underflow_on_empty() {
    let frame = Frame::new(CodeObjectId::new(0), 0, 8);
    let err = frame.peek().unwrap_err();
    assert_eq!(err.kind, VmErrorKind::StackUnderflow);
}

// ---------------------------------------------------------------------------
// Stack dup
// ---------------------------------------------------------------------------

#[test]
fn dup_duplicates_top() {
    let mut frame = Frame::new(CodeObjectId::new(0), 0, 8);
    frame.push(Value::Integer(7)).unwrap();
    frame.dup().unwrap();
    assert_eq!(frame.stack_depth(), 2);
    assert_eq!(frame.pop().unwrap(), Value::Integer(7));
    assert_eq!(frame.pop().unwrap(), Value::Integer(7));
}

#[test]
fn dup_underflow_on_empty() {
    let mut frame = Frame::new(CodeObjectId::new(0), 0, 8);
    let err = frame.dup().unwrap_err();
    assert_eq!(err.kind, VmErrorKind::StackUnderflow);
}

#[test]
fn dup_overflow_at_max() {
    let mut frame = Frame::new(CodeObjectId::new(0), 0, 1);
    frame.push(Value::Integer(1)).unwrap();
    let err = frame.dup().unwrap_err();
    assert_eq!(err.kind, VmErrorKind::StackOverflow);
}

// ---------------------------------------------------------------------------
// Stack swap
// ---------------------------------------------------------------------------

#[test]
fn swap_exchanges_top_two() {
    let mut frame = Frame::new(CodeObjectId::new(0), 0, 8);
    frame.push(Value::Integer(1)).unwrap();
    frame.push(Value::Integer(2)).unwrap();
    frame.swap().unwrap();
    assert_eq!(frame.pop().unwrap(), Value::Integer(1));
    assert_eq!(frame.pop().unwrap(), Value::Integer(2));
}

#[test]
fn swap_underflow_with_zero_elements() {
    let mut frame = Frame::new(CodeObjectId::new(0), 0, 8);
    let err = frame.swap().unwrap_err();
    assert_eq!(err.kind, VmErrorKind::StackUnderflow);
}

#[test]
fn swap_underflow_with_one_element() {
    let mut frame = Frame::new(CodeObjectId::new(0), 0, 8);
    frame.push(Value::Integer(1)).unwrap();
    let err = frame.swap().unwrap_err();
    assert_eq!(err.kind, VmErrorKind::StackUnderflow);
}

#[test]
fn swap_is_self_inverse() {
    let mut frame = Frame::new(CodeObjectId::new(0), 0, 8);
    frame.push(Value::Integer(1)).unwrap();
    frame.push(Value::Integer(2)).unwrap();
    frame.swap().unwrap();
    frame.swap().unwrap();
    assert_eq!(frame.pop().unwrap(), Value::Integer(2));
    assert_eq!(frame.pop().unwrap(), Value::Integer(1));
}

// ---------------------------------------------------------------------------
// Registers
// ---------------------------------------------------------------------------

#[test]
fn register_get_uninitialized() {
    let frame = Frame::new(CodeObjectId::new(0), 4, 8);
    assert_eq!(
        frame.local_get(LocalId::new(0)).unwrap(),
        &Value::Uninitialized
    );
}

#[test]
fn register_set_then_get() {
    let mut frame = Frame::new(CodeObjectId::new(0), 4, 8);
    frame
        .local_set(LocalId::new(2), Value::String("foo".into()))
        .unwrap();
    assert_eq!(
        frame.local_get(LocalId::new(2)).unwrap(),
        &Value::String("foo".into())
    );
}

#[test]
fn register_overwrite() {
    let mut frame = Frame::new(CodeObjectId::new(0), 4, 8);
    frame
        .local_set(LocalId::new(1), Value::Integer(10))
        .unwrap();
    frame
        .local_set(LocalId::new(1), Value::Integer(20))
        .unwrap();
    assert_eq!(
        frame.local_get(LocalId::new(1)).unwrap(),
        &Value::Integer(20)
    );
}

#[test]
fn register_out_of_bounds_get() {
    let frame = Frame::new(CodeObjectId::new(0), 4, 8);
    let err = frame.local_get(LocalId::new(4)).unwrap_err();
    assert_eq!(err.kind, VmErrorKind::RegisterOutOfBounds);
}

#[test]
fn register_out_of_bounds_set() {
    let mut frame = Frame::new(CodeObjectId::new(0), 4, 8);
    let err = frame
        .local_set(LocalId::new(4), Value::Integer(0))
        .unwrap_err();
    assert_eq!(err.kind, VmErrorKind::RegisterOutOfBounds);
}

#[test]
fn register_boundary_index() {
    let mut frame = Frame::new(CodeObjectId::new(0), 4, 8);
    frame
        .local_set(LocalId::new(3), Value::Integer(99))
        .unwrap();
    assert_eq!(
        frame.local_get(LocalId::new(3)).unwrap(),
        &Value::Integer(99)
    );
}

#[test]
fn register_zero_locals_always_fails() {
    let frame = Frame::new(CodeObjectId::new(0), 0, 8);
    let err = frame.local_get(LocalId::new(0)).unwrap_err();
    assert_eq!(err.kind, VmErrorKind::RegisterOutOfBounds);
}

// ---------------------------------------------------------------------------
// Edge cases
// ---------------------------------------------------------------------------

#[test]
fn max_stack_zero_rejects_all_pushes() {
    let mut frame = Frame::new(CodeObjectId::new(0), 0, 0);
    let err = frame.push(Value::Integer(1)).unwrap_err();
    assert_eq!(err.kind, VmErrorKind::StackOverflow);
}

#[test]
fn large_locals_count() {
    let count = 1024;
    let frame = Frame::new(CodeObjectId::new(0), count, 8);
    assert_eq!(
        frame.local_get(LocalId::new(count - 1)).unwrap(),
        &Value::Uninitialized
    );
    let err = frame.local_get(LocalId::new(count)).unwrap_err();
    assert_eq!(err.kind, VmErrorKind::RegisterOutOfBounds);
}

#[test]
fn mixed_stack_and_register_ops() {
    let mut frame = Frame::new(CodeObjectId::new(5), 2, 4);
    // Use registers
    frame
        .local_set(LocalId::new(0), Value::String("a".into()))
        .unwrap();
    frame
        .local_set(LocalId::new(1), Value::Integer(42))
        .unwrap();
    // Use stack
    frame.push(Value::ExitStatus(0)).unwrap();
    frame.push(Value::Integer(100)).unwrap();
    // Verify independence
    assert_eq!(
        frame.local_get(LocalId::new(0)).unwrap(),
        &Value::String("a".into())
    );
    assert_eq!(frame.pop().unwrap(), Value::Integer(100));
    assert_eq!(
        frame.local_get(LocalId::new(1)).unwrap(),
        &Value::Integer(42)
    );
    assert_eq!(frame.pop().unwrap(), Value::ExitStatus(0));
}

#[test]
fn push_pop_various_value_types() {
    let mut frame = Frame::new(CodeObjectId::new(0), 0, 8);
    frame.push(Value::String("hello".into())).unwrap();
    frame.push(Value::Integer(42)).unwrap();
    frame.push(Value::ExitStatus(1)).unwrap();
    frame
        .push(Value::FieldList(vec!["a".into(), "b".into()]))
        .unwrap();
    frame.push(Value::Uninitialized).unwrap();

    assert_eq!(frame.pop().unwrap(), Value::Uninitialized);
    assert_eq!(
        frame.pop().unwrap(),
        Value::FieldList(vec!["a".into(), "b".into()])
    );
    assert_eq!(frame.pop().unwrap(), Value::ExitStatus(1));
    assert_eq!(frame.pop().unwrap(), Value::Integer(42));
    assert_eq!(frame.pop().unwrap(), Value::String("hello".into()));
}

#[test]
fn advance_pc_after_set_pc() {
    let mut frame = Frame::new(CodeObjectId::new(0), 0, 8);
    frame.set_pc(10);
    assert_eq!(frame.advance_pc(), 10);
    assert_eq!(frame.pc(), 11);
}
