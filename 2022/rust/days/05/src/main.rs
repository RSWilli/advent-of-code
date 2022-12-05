use std::{collections::VecDeque, str::FromStr};

use lib::{AOCError, AOCReader, AdventOfCode};
use operations::Operation;
use stacks::Stacks;
mod operations;
mod stacks;

struct Day {}

struct Crane {
    stacks: Stacks,
    operations: Vec<Operation>,
}

impl FromStr for Crane {
    type Err = AOCError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (stacks, ops) = s.split_once("\n\n").ok_or(AOCError::ParseErr())?;

        let stacks = stacks.parse()?;

        let ops = ops
            .lines()
            .map(|line| line.parse())
            .collect::<Result<Vec<Operation>, AOCError>>()?;

        Ok(Crane {
            stacks,
            operations: ops,
        })
    }
}

impl AdventOfCode for Day {
    const DAY: usize = 5;

    type In = Crane;

    type Out = usize;

    fn parse(&self, inp: AOCReader) -> Result<Self::In, AOCError> {
        inp.parse_content()
    }

    fn part1(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        print!("{:?}", input.stacks);

        Err(AOCError::AOCError { msg: "uninmpl" })
    }

    fn part2(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        unimplemented!()
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
        lib::test(Day {}, lib::Part::Part1, 1, 10)
    }
}
