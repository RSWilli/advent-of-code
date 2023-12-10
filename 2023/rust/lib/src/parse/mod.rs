pub use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    character::complete::{self, alphanumeric0, anychar, char, i64, one_of, space1, u64},
    error::VerboseError,
    multi::{many0, many1, many_m_n, separated_list0, separated_list1},
    sequence::tuple,
    Finish, IResult,
};
use nom::{combinator::eof, error::context};

pub mod parsers;
pub use parsers::*;

use crate::AOCError;

pub type ParseResult<'a, U> = IResult<&'a str, U, VerboseError<&'a str>>;

pub fn parse_all<'a, T, F>(parser: F) -> impl FnMut(&'a str) -> ParseResult<'a, T>
where
    F: nom::Parser<&'a str, T, VerboseError<&'a str>> + Copy,
{
    move |input| {
        let (s, (r, _, _)) = tuple((parser, tag("\n"), eof))(input)?;

        Ok((s, r))
    }
}

pub fn parse_lines<'a, T, F>(parser: F) -> impl FnMut(&'a str) -> ParseResult<'a, Vec<T>>
where
    F: nom::Parser<&'a str, T, VerboseError<&'a str>> + Copy,
{
    move |input| {
        let (s, (r, _, _)) = tuple((
            separated_list0(tag("\n"), parser),
            tag("\n"),
            context("eof", eof),
        ))(input)?;

        Ok((s, r))
    }
}

pub fn run_parser<'a, T, F>(mut parser: F, input: &'a str) -> Result<T, AOCError>
where
    F: nom::Parser<&'a str, T, VerboseError<&'a str>> + Copy,
{
    match parser.parse(input).finish() {
        // rest of input is empty, we always finish with eof
        Ok((_, x)) => Ok(x),
        Err(x) => {
            println!("{}", x);
            Err(AOCError::ParseErr())
        }
    }
}
