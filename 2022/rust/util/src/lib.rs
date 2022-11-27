use std::fmt::{Debug, Display};

pub use error::AOCError;

mod error;
mod read;

pub use read::AOCReader;

// alias the return type so more traits can be easily added later
pub trait AOCReturn: Debug + Display + std::cmp::PartialEq {}
impl<T: Debug + Display + std::cmp::PartialEq> AOCReturn for T {}

/**
 * Trait for an Advent of Code solution.
 */
pub trait AdventOfCode {
    const DAY: usize;
    type In;
    type Out: AOCReturn;
    fn parse(&self, inp: AOCReader) -> Result<Self::In, error::AOCError>;
    fn part1(&self, input: &Self::In) -> Result<Self::Out, AOCError>;
    fn part2(&self, input: &Self::In) -> Result<Self::Out, AOCError>;
}

/**
 * Run the solutions for the given day.
 */
pub fn run<P, R: AOCReturn, T: AdventOfCode<In = P, Out = R>>(
    aoc: T,
) -> Result<(), error::AOCError> {
    let parse_start = std::time::Instant::now();
    let input = read::read_input(T::DAY)?;

    let parsed = aoc.parse(input)?;

    println!("Parsed input in {}µs", parse_start.elapsed().as_micros());

    let part1_start = std::time::Instant::now();
    let p1 = aoc.part1(&parsed)?;
    println!("Part 1: {} ({}µs)", p1, part1_start.elapsed().as_micros());

    let part2_start = std::time::Instant::now();
    let p2 = aoc.part2(&parsed)?;
    println!("Part 2: {} ({}µs)", p2, part2_start.elapsed().as_micros());

    Ok(())
}

pub enum Part {
    Part1,
    Part2,
}

/**
 * test the given part of the solution for the given day.
 */
pub fn test<P, R: AOCReturn, T: AdventOfCode<In = P, Out = R>>(
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
