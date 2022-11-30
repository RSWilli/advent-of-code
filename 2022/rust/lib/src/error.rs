use std::io::Error as IOError;

use thiserror::Error;

#[derive(Error, Debug)]
pub enum AOCError {
    #[error(transparent)]
    IOError(#[from] IOError),
    #[error("test failed: expected {expected:?}, got {actual:?}")]
    TestFailed { expected: String, actual: String },
    #[error("parse error")]
    ParseErr(),
}
