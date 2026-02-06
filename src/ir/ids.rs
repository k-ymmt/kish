//! Stable identifier wrappers for IR pools and program handles.

macro_rules! define_id {
    ($name:ident) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
        pub struct $name(u32);

        impl $name {
            /// Creates an identifier from a raw value.
            pub const fn new(value: u32) -> Self {
                Self(value)
            }

            /// Returns the raw identifier value.
            pub const fn value(self) -> u32 {
                self.0
            }
        }
    };
}

define_id!(CodeObjectId);
define_id!(StringId);
define_id!(SymbolId);
define_id!(ConstId);
define_id!(LabelId);
define_id!(LocalId);
define_id!(WordProgramId);
define_id!(RedirectProgramId);
define_id!(ArithProgramId);
