//! VM value type with POSIX-compliant coercions.

use std::fmt;

use super::error::{VmError, VmErrorKind};

/// Runtime value carried on the VM operand stack and in registers.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum Value {
    /// A shell string (the default representation).
    String(std::string::String),
    /// A 64-bit signed integer (arithmetic context).
    Integer(i64),
    /// A process exit status (0-255 normal, >128 signal).
    ExitStatus(i32),
    /// A list of fields produced by word splitting / glob expansion.
    FieldList(Vec<std::string::String>),
    /// Variable has not been assigned a value.
    #[default]
    Uninitialized,
}

impl Value {
    /// Coerces the value to a string.
    ///
    /// - `Integer` / `ExitStatus` -> decimal representation.
    /// - `FieldList` -> fields joined with a single space.
    /// - `Uninitialized` -> empty string.
    pub fn as_string(&self) -> std::string::String {
        match self {
            Self::String(s) => s.clone(),
            Self::Integer(n) => n.to_string(),
            Self::ExitStatus(n) => n.to_string(),
            Self::FieldList(fields) => fields.join(" "),
            Self::Uninitialized => std::string::String::new(),
        }
    }

    /// Coerces the value to an integer.
    ///
    /// - `String` -> trim then parse; empty or all-whitespace -> 0.
    /// - `ExitStatus` -> widened to `i64`.
    /// - `FieldList` -> `TypeMismatch` error.
    /// - `Uninitialized` -> 0.
    pub fn as_integer(&self) -> Result<i64, VmError> {
        match self {
            Self::Integer(n) => Ok(*n),
            Self::ExitStatus(n) => Ok(i64::from(*n)),
            Self::String(s) => {
                let trimmed = s.trim();
                if trimmed.is_empty() {
                    return Ok(0);
                }
                trimmed.parse::<i64>().map_err(|_| {
                    VmError::new(
                        VmErrorKind::TypeMismatch,
                        format!("cannot convert string to integer: {s:?}"),
                    )
                })
            }
            Self::FieldList(_) => Err(VmError::type_mismatch(
                "cannot convert field list to integer",
            )),
            Self::Uninitialized => Ok(0),
        }
    }

    /// Coerces the value to an exit status.
    ///
    /// - `ExitStatus` -> returned directly (preserves signal values).
    /// - `Integer` -> `(n as i32) & 0xFF`.
    /// - `String` -> parse and mask, or 1 on failure.
    /// - `FieldList` -> 1.
    /// - `Uninitialized` -> 0.
    pub fn as_exit_status(&self) -> i32 {
        match self {
            Self::ExitStatus(n) => *n,
            Self::Integer(n) => (*n as i32) & 0xFF,
            Self::String(s) => s
                .trim()
                .parse::<i32>()
                .map(|n| n & 0xFF)
                .unwrap_or(1),
            Self::FieldList(_) => 1,
            Self::Uninitialized => 0,
        }
    }

    /// Tests truthiness using POSIX conventions.
    ///
    /// - `ExitStatus(0)` -> true (success).
    /// - `String` non-empty -> true (note: `"0"` is truthy).
    /// - `Integer` != 0 -> true.
    /// - `FieldList` non-empty -> true.
    /// - `Uninitialized` -> false.
    pub fn is_truthy(&self) -> bool {
        match self {
            Self::ExitStatus(n) => *n == 0,
            Self::String(s) => !s.is_empty(),
            Self::Integer(n) => *n != 0,
            Self::FieldList(fields) => !fields.is_empty(),
            Self::Uninitialized => false,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_string())
    }
}
