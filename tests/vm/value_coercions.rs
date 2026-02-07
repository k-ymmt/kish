use kish::vm::{Value, VmErrorKind};

// ---------------------------------------------------------------------------
// as_string
// ---------------------------------------------------------------------------

#[test]
fn as_string_from_string() {
    let v = Value::String("hello".into());
    assert_eq!(v.as_string(), "hello");
}

#[test]
fn as_string_from_empty_string() {
    let v = Value::String(String::new());
    assert_eq!(v.as_string(), "");
}

#[test]
fn as_string_from_positive_integer() {
    let v = Value::Integer(42);
    assert_eq!(v.as_string(), "42");
}

#[test]
fn as_string_from_negative_integer() {
    let v = Value::Integer(-7);
    assert_eq!(v.as_string(), "-7");
}

#[test]
fn as_string_from_zero_integer() {
    let v = Value::Integer(0);
    assert_eq!(v.as_string(), "0");
}

#[test]
fn as_string_from_i64_max() {
    let v = Value::Integer(i64::MAX);
    assert_eq!(v.as_string(), i64::MAX.to_string());
}

#[test]
fn as_string_from_i64_min() {
    let v = Value::Integer(i64::MIN);
    assert_eq!(v.as_string(), i64::MIN.to_string());
}

#[test]
fn as_string_from_exit_status_zero() {
    let v = Value::ExitStatus(0);
    assert_eq!(v.as_string(), "0");
}

#[test]
fn as_string_from_exit_status_nonzero() {
    let v = Value::ExitStatus(127);
    assert_eq!(v.as_string(), "127");
}

#[test]
fn as_string_from_field_list() {
    let v = Value::FieldList(vec!["a".into(), "b".into(), "c".into()]);
    assert_eq!(v.as_string(), "a b c");
}

#[test]
fn as_string_from_empty_field_list() {
    let v = Value::FieldList(vec![]);
    assert_eq!(v.as_string(), "");
}

#[test]
fn as_string_from_single_field_list() {
    let v = Value::FieldList(vec!["only".into()]);
    assert_eq!(v.as_string(), "only");
}

#[test]
fn as_string_from_uninitialized() {
    let v = Value::Uninitialized;
    assert_eq!(v.as_string(), "");
}

// ---------------------------------------------------------------------------
// as_integer
// ---------------------------------------------------------------------------

#[test]
fn as_integer_from_integer() {
    let v = Value::Integer(99);
    assert_eq!(v.as_integer().unwrap(), 99);
}

#[test]
fn as_integer_from_negative_integer() {
    let v = Value::Integer(-1);
    assert_eq!(v.as_integer().unwrap(), -1);
}

#[test]
fn as_integer_from_exit_status() {
    let v = Value::ExitStatus(42);
    assert_eq!(v.as_integer().unwrap(), 42);
}

#[test]
fn as_integer_from_numeric_string() {
    let v = Value::String("123".into());
    assert_eq!(v.as_integer().unwrap(), 123);
}

#[test]
fn as_integer_from_negative_string() {
    let v = Value::String("-45".into());
    assert_eq!(v.as_integer().unwrap(), -45);
}

#[test]
fn as_integer_from_empty_string() {
    let v = Value::String(String::new());
    assert_eq!(v.as_integer().unwrap(), 0);
}

#[test]
fn as_integer_from_whitespace_string() {
    let v = Value::String("  ".into());
    assert_eq!(v.as_integer().unwrap(), 0);
}

#[test]
fn as_integer_from_padded_string() {
    let v = Value::String("  77  ".into());
    assert_eq!(v.as_integer().unwrap(), 77);
}

#[test]
fn as_integer_from_non_numeric_string() {
    let v = Value::String("abc".into());
    let err = v.as_integer().unwrap_err();
    assert_eq!(err.kind, VmErrorKind::TypeMismatch);
}

#[test]
fn as_integer_from_partially_numeric_string() {
    let v = Value::String("12abc".into());
    let err = v.as_integer().unwrap_err();
    assert_eq!(err.kind, VmErrorKind::TypeMismatch);
}

#[test]
fn as_integer_from_field_list_is_error() {
    let v = Value::FieldList(vec!["1".into()]);
    let err = v.as_integer().unwrap_err();
    assert_eq!(err.kind, VmErrorKind::TypeMismatch);
}

#[test]
fn as_integer_from_empty_field_list_is_error() {
    let v = Value::FieldList(vec![]);
    let err = v.as_integer().unwrap_err();
    assert_eq!(err.kind, VmErrorKind::TypeMismatch);
}

#[test]
fn as_integer_from_uninitialized() {
    let v = Value::Uninitialized;
    assert_eq!(v.as_integer().unwrap(), 0);
}

// ---------------------------------------------------------------------------
// as_exit_status
// ---------------------------------------------------------------------------

#[test]
fn as_exit_status_from_exit_status() {
    let v = Value::ExitStatus(0);
    assert_eq!(v.as_exit_status(), 0);
}

#[test]
fn as_exit_status_from_exit_status_signal() {
    // Signal values (128+signum) should be preserved as-is.
    let v = Value::ExitStatus(130);
    assert_eq!(v.as_exit_status(), 130);
}

#[test]
fn as_exit_status_from_integer() {
    let v = Value::Integer(0);
    assert_eq!(v.as_exit_status(), 0);
}

#[test]
fn as_exit_status_from_integer_masked() {
    let v = Value::Integer(256);
    assert_eq!(v.as_exit_status(), 0);
}

#[test]
fn as_exit_status_from_integer_negative() {
    let v = Value::Integer(-1);
    // (-1 as i32) & 0xFF = 255
    assert_eq!(v.as_exit_status(), 255);
}

#[test]
fn as_exit_status_from_numeric_string() {
    let v = Value::String("42".into());
    assert_eq!(v.as_exit_status(), 42);
}

#[test]
fn as_exit_status_from_large_numeric_string() {
    let v = Value::String("300".into());
    assert_eq!(v.as_exit_status(), 300 & 0xFF);
}

#[test]
fn as_exit_status_from_non_numeric_string() {
    let v = Value::String("abc".into());
    assert_eq!(v.as_exit_status(), 1);
}

#[test]
fn as_exit_status_from_empty_string() {
    let v = Value::String(String::new());
    assert_eq!(v.as_exit_status(), 1);
}

#[test]
fn as_exit_status_from_field_list() {
    let v = Value::FieldList(vec!["0".into()]);
    assert_eq!(v.as_exit_status(), 1);
}

#[test]
fn as_exit_status_from_empty_field_list() {
    let v = Value::FieldList(vec![]);
    assert_eq!(v.as_exit_status(), 1);
}

#[test]
fn as_exit_status_from_uninitialized() {
    let v = Value::Uninitialized;
    assert_eq!(v.as_exit_status(), 0);
}

// ---------------------------------------------------------------------------
// is_truthy
// ---------------------------------------------------------------------------

#[test]
fn is_truthy_exit_status_success() {
    assert!(Value::ExitStatus(0).is_truthy());
}

#[test]
fn is_truthy_exit_status_failure() {
    assert!(!Value::ExitStatus(1).is_truthy());
}

#[test]
fn is_truthy_exit_status_signal() {
    assert!(!Value::ExitStatus(130).is_truthy());
}

#[test]
fn is_truthy_string_nonempty() {
    assert!(Value::String("hello".into()).is_truthy());
}

#[test]
fn is_truthy_string_zero_is_truthy() {
    // Critical POSIX case: "0" as a string is truthy.
    assert!(Value::String("0".into()).is_truthy());
}

#[test]
fn is_truthy_string_empty() {
    assert!(!Value::String(String::new()).is_truthy());
}

#[test]
fn is_truthy_integer_nonzero() {
    assert!(Value::Integer(1).is_truthy());
}

#[test]
fn is_truthy_integer_negative() {
    assert!(Value::Integer(-1).is_truthy());
}

#[test]
fn is_truthy_integer_zero() {
    assert!(!Value::Integer(0).is_truthy());
}

#[test]
fn is_truthy_field_list_nonempty() {
    assert!(Value::FieldList(vec!["a".into()]).is_truthy());
}

#[test]
fn is_truthy_field_list_empty() {
    assert!(!Value::FieldList(vec![]).is_truthy());
}

#[test]
fn is_truthy_uninitialized() {
    assert!(!Value::Uninitialized.is_truthy());
}

// ---------------------------------------------------------------------------
// is_zero
// ---------------------------------------------------------------------------

#[test]
fn is_zero_integer_zero() {
    assert!(Value::Integer(0).is_zero());
}

#[test]
fn is_zero_integer_nonzero() {
    assert!(!Value::Integer(1).is_zero());
}

#[test]
fn is_zero_integer_negative() {
    assert!(!Value::Integer(-1).is_zero());
}

#[test]
fn is_zero_exit_status_zero() {
    assert!(Value::ExitStatus(0).is_zero());
}

#[test]
fn is_zero_exit_status_nonzero() {
    assert!(!Value::ExitStatus(1).is_zero());
}

#[test]
fn is_zero_string_empty() {
    assert!(Value::String(String::new()).is_zero());
}

#[test]
fn is_zero_string_nonempty() {
    assert!(!Value::String("hello".into()).is_zero());
}

#[test]
fn is_zero_string_zero_char() {
    // "0" is a non-empty string, so is_zero returns false.
    assert!(!Value::String("0".into()).is_zero());
}

#[test]
fn is_zero_field_list_empty() {
    assert!(Value::FieldList(vec![]).is_zero());
}

#[test]
fn is_zero_field_list_nonempty() {
    assert!(!Value::FieldList(vec!["a".into()]).is_zero());
}

#[test]
fn is_zero_uninitialized() {
    assert!(Value::Uninitialized.is_zero());
}

// ---------------------------------------------------------------------------
// Default
// ---------------------------------------------------------------------------

#[test]
fn default_is_uninitialized() {
    assert_eq!(Value::default(), Value::Uninitialized);
}

// ---------------------------------------------------------------------------
// Display
// ---------------------------------------------------------------------------

#[test]
fn display_matches_as_string() {
    let cases = vec![
        Value::String("test".into()),
        Value::Integer(42),
        Value::ExitStatus(0),
        Value::FieldList(vec!["a".into(), "b".into()]),
        Value::Uninitialized,
    ];
    for v in &cases {
        assert_eq!(format!("{v}"), v.as_string());
    }
}

// ---------------------------------------------------------------------------
// Clone and PartialEq
// ---------------------------------------------------------------------------

#[test]
fn clone_preserves_equality() {
    let v = Value::Integer(42);
    assert_eq!(v.clone(), v);
}

#[test]
fn different_values_not_equal() {
    assert_ne!(Value::Integer(1), Value::Integer(2));
}
