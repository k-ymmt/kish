use kish::vm::{VmError, VmErrorKind};

// ---------------------------------------------------------------------------
// Convenience constructors
// ---------------------------------------------------------------------------

#[test]
fn stack_overflow_constructor() {
    let e = VmError::stack_overflow();
    assert_eq!(e.kind, VmErrorKind::StackOverflow);
    assert!(!e.message.is_empty());
}

#[test]
fn stack_underflow_constructor() {
    let e = VmError::stack_underflow();
    assert_eq!(e.kind, VmErrorKind::StackUnderflow);
}

#[test]
fn register_out_of_bounds_constructor() {
    let e = VmError::register_out_of_bounds(5, 4);
    assert_eq!(e.kind, VmErrorKind::RegisterOutOfBounds);
    assert!(e.message.contains("5"));
    assert!(e.message.contains("4"));
}

#[test]
fn invalid_code_object_constructor() {
    let e = VmError::invalid_code_object(99);
    assert_eq!(e.kind, VmErrorKind::InvalidCodeObject);
    assert!(e.message.contains("99"));
}

#[test]
fn type_mismatch_constructor() {
    let e = VmError::type_mismatch("bad type");
    assert_eq!(e.kind, VmErrorKind::TypeMismatch);
    assert_eq!(e.message, "bad type");
}

#[test]
fn division_by_zero_constructor() {
    let e = VmError::division_by_zero();
    assert_eq!(e.kind, VmErrorKind::DivisionByZero);
}

#[test]
fn command_not_found_constructor() {
    let e = VmError::command_not_found("foo");
    assert_eq!(e.kind, VmErrorKind::CommandNotFound("foo".into()));
    assert!(e.message.contains("foo"));
}

#[test]
fn command_not_executable_constructor() {
    let e = VmError::command_not_executable("bar");
    assert_eq!(e.kind, VmErrorKind::CommandNotExecutable("bar".into()));
    assert!(e.message.contains("bar"));
}

#[test]
fn internal_error_constructor() {
    let e = VmError::internal("something broke");
    assert_eq!(e.kind, VmErrorKind::InternalError);
    assert_eq!(e.message, "something broke");
}

// ---------------------------------------------------------------------------
// Display
// ---------------------------------------------------------------------------

#[test]
fn display_shows_message() {
    let e = VmError::stack_overflow();
    let display = format!("{e}");
    assert_eq!(display, e.message);
}

// ---------------------------------------------------------------------------
// VmErrorKind Display
// ---------------------------------------------------------------------------

#[test]
fn error_kind_display() {
    assert_eq!(format!("{}", VmErrorKind::StackOverflow), "stack overflow");
    assert_eq!(
        format!("{}", VmErrorKind::CommandNotFound("ls".into())),
        "command not found: ls"
    );
}

// ---------------------------------------------------------------------------
// std::error::Error
// ---------------------------------------------------------------------------

#[test]
fn implements_std_error() {
    let e = VmError::internal("test");
    let _: &dyn std::error::Error = &e;
}

// ---------------------------------------------------------------------------
// Clone and PartialEq
// ---------------------------------------------------------------------------

#[test]
fn clone_preserves_equality() {
    let e = VmError::command_not_found("git");
    assert_eq!(e.clone(), e);
}

#[test]
fn different_errors_not_equal() {
    let a = VmError::stack_overflow();
    let b = VmError::stack_underflow();
    assert_ne!(a, b);
}
