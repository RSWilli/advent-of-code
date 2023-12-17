use nom::error::context;
pub use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    character::complete::{
        self, alpha0, alphanumeric0, anychar, char, i64, none_of, one_of, space1, u64,
    },
    combinator::eof,
    error::VerboseError,
    multi::{many0, many1, many_m_n, separated_list0, separated_list1},
    sequence::tuple,
    Finish, IResult,
};

pub mod parsers;
pub use parsers::*;

use crate::AOCError;

pub type ParseResult<'a, U> = IResult<&'a str, U, VerboseError<&'a str>>;

pub fn parse_all<'a, T, F>(mut parser: F) -> impl FnMut(&'a str) -> ParseResult<'a, T>
where
    F: nom::Parser<&'a str, T, VerboseError<&'a str>>,
{
    move |input| {
        let (s, (r, _, _)) = tuple((|s| parser.parse(s), tag("\n"), eof))(input)?;

        Ok((s, r))
    }
}

pub fn parse_lines<'a, T, F>(mut parser: F) -> impl FnMut(&'a str) -> ParseResult<'a, Vec<T>>
where
    F: nom::Parser<&'a str, T, VerboseError<&'a str>>,
{
    move |input| {
        let (s, (r, _, _)) = tuple((
            separated_list0(tag("\n"), |s| parser.parse(s)),
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
