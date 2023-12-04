use std::{
    collections::{HashSet, VecDeque},
    str::FromStr,
};

use lib::{AOCError, AOCReader, AdventOfCode};
use nom::{
    bytes::complete::tag,
    character::complete::{self, space1},
    error::ErrorKind,
    multi::separated_list1,
    sequence::tuple,
    Finish, IResult,
};

#[derive(Debug)]
struct Card {
    winning: HashSet<usize>,
    chosen: HashSet<usize>,
}

fn parse_nums(s: &str) -> IResult<&str, HashSet<usize>, (&str, ErrorKind)> {
    let (s, nums) = separated_list1(space1, complete::u32)(s)?;

    Ok((s, nums.into_iter().map(|x| x as usize).collect()))
}

// Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
fn parse_card(s: &str) -> IResult<&str, Card, (&str, ErrorKind)> {
    let (s, (_, _, _, _, _, winning, _, _, chosen)) = tuple((
        tag("Card"),
        space1,
        complete::u32,
        tag(":"),
        space1,
        parse_nums,
        tag(" |"),
        space1,
        parse_nums,
    ))(s)?;

    Ok((s, Card { winning, chosen }))
}

impl FromStr for Card {
    type Err = AOCError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_card(s).finish() {
            Ok((_, x)) => Ok(x),
            Err(x) => {
                println!("{:?}", x);
                Err(AOCError::ParseErr())
            }
        }
    }
}
struct Day {}

impl AdventOfCode for Day {
    const DAY: usize = 4;

    type In = Vec<Card>;

    type Out = usize;

    fn parse(&self, inp: AOCReader) -> Result<Self::In, AOCError> {
        inp.parse_lines().collect()
    }

    fn part1(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        Ok(input
            .iter()
            .map(|c| c.winning.intersection(&c.chosen).count())
            .map(|x| {
                if x == 0 {
                    0
                } else {
                    2_usize.pow((x as u32) - 1)
                }
            })
            .sum())
    }

    fn part2(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        Ok(input
            .iter()
            .map(|c| c.winning.intersection(&c.chosen).count())
            .scan(VecDeque::new(), |factors, x| {
                let current_factor = factors.pop_front().unwrap_or(1);

                for i in 0..x {
                    match factors.get_mut(i) {
                        Some(f) => *f += current_factor,
                        None => factors.push_back(1 + current_factor),
                    }
                }

                Some(current_factor)
            })
            .sum())
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
        lib::test(Day {}, lib::Part::Part1, 1, 13)
    }

    #[test]
    fn test2() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part2, 1, 30)
    }
}
