use std::fmt::{Debug, Display};

use lib::AOCError;

// a value that converts uppercase ascii alphabetic strings to a unique integer
// and back again for display purposes
// this way comparisons are done on integers instead of strings
// and we can easily implement copy
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub(super) struct NodeID {
    value: usize,
}

impl TryFrom<&str> for NodeID {
    type Error = AOCError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let chars = value.bytes();

        let mut value = 0;

        for c in chars {
            if !c.is_ascii_alphabetic() {
                return Err(AOCError::AOCError {
                    msg: "not an ascii string",
                });
            }

            // offset one, so AA wont be 0
            value = value * 26 + (c - b'A' + 1) as usize;
        }

        Ok(Self { value })
    }
}

impl Display for NodeID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut value = self.value;
        let mut chars = Vec::new();

        while value > 0 {
            chars.push((b'A' - 1 + (value % 26) as u8) as char);
            value /= 26;
        }

        chars.reverse();

        write!(f, "{}", chars.iter().collect::<String>())
    }
}

impl Debug for NodeID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}: {}", self.value, self)
    }
}
