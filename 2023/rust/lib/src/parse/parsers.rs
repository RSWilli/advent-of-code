use nom::character::complete::alphanumeric1;

use super::ParseResult;

pub fn alphanumeric1_owned(s: &str) -> ParseResult<String> {
    alphanumeric1(s).map(|(s, str)| (s, str.to_owned()))
}
