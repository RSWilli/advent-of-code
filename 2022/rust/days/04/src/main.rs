use std::str::FromStr;

use lib::{AOCError, AOCReader, AdventOfCode};

struct Day {}

struct AssignmentRange {
    start: u8,
    end: u8,
}

impl FromStr for AssignmentRange {
    type Err = AOCError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (start, end) = s.split_once("-").ok_or(AOCError::ParseErr())?;

        let start = start.parse()?;
        let end = end.parse()?;

        Ok(AssignmentRange { start, end })
    }
}

impl AssignmentRange {
    fn contains(&self, other: &AssignmentRange) -> bool {
        self.start <= other.start && other.end <= self.end
    }

    fn overlaps(&self, other: &AssignmentRange) -> bool {
        other.start <= self.end && self.start <= other.end
    }
}

struct ElvePair {
    elve1: AssignmentRange,
    elve2: AssignmentRange,
}

impl FromStr for ElvePair {
    type Err = AOCError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (elve1, elve2) = s.split_once(",").ok_or(AOCError::ParseErr())?;

        let elve1 = elve1.parse()?;
        let elve2 = elve2.parse()?;

        Ok(ElvePair { elve1, elve2 })
    }
}

impl AdventOfCode for Day {
    const DAY: usize = 4;

    type In = Vec<ElvePair>;

    type Out = usize;

    fn parse(&self, inp: AOCReader) -> Result<Self::In, AOCError> {
        inp.parse_lines().collect()
    }

    fn part1(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        Ok(input
            .into_iter()
            .filter(|ep| ep.elve1.contains(&ep.elve2) || ep.elve2.contains(&ep.elve1))
            .count())
    }

    fn part2(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        Ok(input
            .into_iter()
            .filter(|ep| ep.elve1.overlaps(&ep.elve2) || ep.elve2.overlaps(&ep.elve1))
            .count())
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
        lib::test(Day {}, lib::Part::Part1, 1, 2)
    }

    #[test]
    fn test2() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part2, 1, 4)
    }
}
