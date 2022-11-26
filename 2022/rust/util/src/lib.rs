use std::{
    fmt::{Debug, Display},
    fs::File,
    io,
};

use error::AOCError;

pub mod error;

mod read;

// alias the return type so more traits can be easily added later
pub trait AOCReturn: Debug + Display + std::cmp::PartialEq {}
impl<T: Debug + Display + std::cmp::PartialEq> AOCReturn for T {}

/**
 * Trait for an Advent of Code solution.
 */
pub trait AdventOfCode<Parse, Res: AOCReturn> {
    const DAY: usize;
    fn parse(&self, inp: io::BufReader<File>) -> Result<Parse, error::AOCError>;
    fn part1(&self, input: &Parse) -> Result<Res, AOCError>;
    fn part2(&self, input: &Parse) -> Result<Res, AOCError>;
}

/**
 * Run the solutions for the given day.
 */
pub fn run<P, R: AOCReturn, T: AdventOfCode<P, R>>(aoc: T) -> Result<(), error::AOCError> {
    let input = read::read_input(T::DAY)?;
    let parsed = aoc.parse(input)?;

    let p1 = aoc.part1(&parsed)?;
    println!("Part 1: {}", p1);

    let p2 = aoc.part2(&parsed)?;
    println!("Part 2: {}", p2);

    Ok(())
}

pub enum Part {
    Part1,
    Part2,
}

/**
 * test the given part of the solution for the given day.
 */
pub fn test<P, R: AOCReturn, T: AdventOfCode<P, R>>(
    aoc: T,
    part: Part,
    test: usize,
    cmp: R,
) -> Result<(), AOCError> {
    let input = read::read_test(T::DAY, test)?;
    let parsed = aoc.parse(input)?;

    let res = match part {
        Part::Part1 => aoc.part1(&parsed)?,
        Part::Part2 => aoc.part2(&parsed)?,
    };

    if res == cmp {
        Ok(())
    } else {
        Err(AOCError::TestFailed {
            expected: format!("{:?}", cmp),
            actual: format!("{:?}", res),
        })
    }
}

#[cfg(test)]
mod tests {}
