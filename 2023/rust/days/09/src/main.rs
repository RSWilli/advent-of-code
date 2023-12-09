use std::str::FromStr;

use lib::{AOCError, AOCReader, AdventOfCode};
use nom::{
    bytes::complete::{tag, take},
    character::complete::{self, char, one_of, space1},
    error::VerboseError,
    multi::many0,
    multi::{many_m_n, separated_list0, separated_list1},
    sequence::{delimited, tuple},
    Err, Finish, IResult,
};

type ParseResult<'a, U> = IResult<&'a str, U, VerboseError<&'a str>>;

#[derive(Debug)]
struct Sequence {
    nums: Vec<i64>,
}

impl Sequence {
    fn extrapolate(&self) -> (i64, i64) {
        let diffs: Vec<_> = self
            .nums
            .iter()
            .zip(self.nums.iter().skip(1))
            .map(|(a, b)| b - a)
            .collect();

        if diffs.iter().any(|&x| x != 0) {
            let diffeseq = Sequence { nums: diffs };

            let (front, back) = diffeseq.extrapolate();

            (self.nums[0] - front, self.nums.last().unwrap() + back)
        } else {
            (self.nums[0], *self.nums.last().unwrap())
        }
    }
}

fn parse_sequence(s: &str) -> ParseResult<Sequence> {
    let (s, nums) = separated_list0(space1, complete::i64)(s)?;

    Ok((s, Sequence { nums }))
}

impl FromStr for Sequence {
    type Err = AOCError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_sequence(s).finish() {
            Ok(("", x)) => Ok(x),
            Err(x) => {
                println!("{}", x);
                Err(AOCError::ParseErr())
            }
            Ok((s, x)) => {
                println!("{:?}", x);
                println!("unconsumed: {:?}", s);
                Err(AOCError::ParseErr())
            }
        }
    }
}

struct Day {}

impl AdventOfCode for Day {
    const DAY: usize = 9;

    type In = Vec<Sequence>;

    type Out = i64;

    fn parse(&self, inp: AOCReader) -> Result<Self::In, AOCError> {
        inp.parse_lines().collect()
    }

    fn part1(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        Ok(input.iter().map(Sequence::extrapolate).map(|p| p.1).sum())
    }

    fn part2(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        Ok(input.iter().map(Sequence::extrapolate).map(|p| p.0).sum())
    }
}

fn main() -> Result<(), AOCError> {
    lib::run(Day {})
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test1() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part1, 1, 114)
    }

    #[test]
    fn test2() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part2, 1, 2)
    }
}
