use std::fmt::{Debug, Display};

pub use error::AOCError;

pub mod distance;
mod error;
pub mod math;
pub mod ocr;
mod read;
pub mod spatial;
pub mod util;

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

#[derive(Debug, Eq, PartialEq)]
pub enum Solution<P1, P2> {
    Part1(P1),
    Part2(P2),
}

impl<P1: Display, P2: Display> Display for Solution<P1, P2> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Solution::Part1(p1) => write!(f, "{}", p1),
            Solution::Part2(p2) => write!(f, "{}", p2),
        }
    }
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

    println!("Parsed input ({:?})", parse_start.elapsed());

    let part1_start = std::time::Instant::now();
    let p1 = aoc.part1(&parsed)?;
    println!("Part 1: {} ({:?})", p1, part1_start.elapsed());

    let part2_start = std::time::Instant::now();
    let p2 = aoc.part2(&parsed)?;
    println!("Part 2: {} ({:?})", p2, part2_start.elapsed());

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

pub fn get_test<P, R: AOCReturn, T: AdventOfCode<In = P, Out = R>>(
    aoc: T,
    test: usize,
) -> Result<P, AOCError> {
    let input = read::read_test(T::DAY, test)?;
    aoc.parse(input)
}

#[cfg(test)]
mod tests {}
