use std::{io::Error as IOError, num::ParseIntError};

use thiserror::Error;

#[derive(Error, Debug)]
pub enum AOCError {
    #[error(transparent)]
    IOError(#[from] IOError),
    #[error(transparent)]
    ParseIntError(#[from] ParseIntError),
    #[error("test failed: expected {expected:?}, got {actual:?}")]
    TestFailed { expected: String, actual: String },
    #[error("parse error")]
    ParseErr(),
    #[error("Solving failed: {msg:?}")]
    AOCError { msg: &'static str },
}
