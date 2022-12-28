use std::fmt::Debug;

use nom::{
    error::{self, ContextError, ErrorKind},
    IResult,
};

pub(super) type ParseResult<'a, T> = IResult<&'a str, T, ParseError>;

pub(super) struct ParseError {
    pub(super) message: String,
}

impl Debug for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ParseError: {}", self.message)
    }
}

impl error::ParseError<&str> for ParseError {
    // on one line, we show the error code and the input that caused it
    fn from_error_kind(input: &str, kind: ErrorKind) -> Self {
        let message = format!("{:?}:\t{:?}\n", kind, input);
        // println!("{}", message);
        ParseError { message }
    }

    // if combining multiple errors, we show them one after the other
    fn append(input: &str, kind: ErrorKind, other: Self) -> Self {
        let message = format!("{}{:?}:\t{:?}\n", other.message, kind, input);
        // println!("{}", message);
        ParseError { message }
    }

    fn from_char(input: &str, c: char) -> Self {
        let message = format!("'{}':\t{:?}\n", c, input);
        // println!("{}", message);
        ParseError { message }
    }

    fn or(self, other: Self) -> Self {
        let message = format!("{}\tOR\n{}\n", self.message, other.message);
        // println!("{}", message);
        ParseError { message }
    }
}

impl ContextError<&str> for ParseError {
    fn add_context(input: &str, ctx: &'static str, other: Self) -> Self {
        let message = format!("{}\"{}\":\t{:?}\n", other.message, ctx, input);
        // println!("{}", message);
        ParseError { message }
    }
}
