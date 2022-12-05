use std::collections::BinaryHeap;

use lib::{AOCError, AOCReader, AdventOfCode};

struct Day {}

fn sum_calories(elves: &[Vec<usize>]) -> Result<Vec<usize>, AOCError> {
    elves.iter().map(|s| s.iter().map(Ok).sum()).collect()
}

impl AdventOfCode for Day {
    const DAY: usize = 1;

    type In = Vec<Vec<usize>>;

    type Out = usize;

    fn parse(&self, inp: AOCReader) -> Result<Self::In, AOCError> {
        let all = inp.content()?;

        let elves = all.split("\n\n");

        elves
            .map(|s| {
                s.split('\n')
                    .map(|s| -> Result<usize, AOCError> {
                        let cal = s.parse()?;

                        Ok(cal)
                    })
                    .collect()
            })
            .collect()
    }

    fn part1(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        let sums = sum_calories(input)?;
        sums.into_iter()
            .max()
            .ok_or(AOCError::AOCError { msg: "no max" })
    }

    fn part2(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        let sums = sum_calories(input)?;
        let mut ordered: BinaryHeap<_> = sums.into_iter().collect();

        let max = ordered.pop().ok_or(AOCError::AOCError { msg: "no max" })?;
        let snd = ordered.pop().ok_or(AOCError::AOCError { msg: "no snd" })?;
        let third = ordered
            .pop()
            .ok_or(AOCError::AOCError { msg: "no third" })?;

        Ok(max + snd + third)
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
        lib::test(Day {}, lib::Part::Part1, 1, 24000)
    }

    #[test]
    fn test2() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part2, 1, 45000)
    }
}
