use std::collections::HashSet;

use lib::{AOCError, AOCReader, AdventOfCode};

struct Day {}

fn priority(c: &char) -> u32 {
    let ascii = *c as u32;

    if c.is_lowercase() {
        ascii - 97 + 1
    } else {
        ascii - 65 + 27
    }
}

impl AdventOfCode for Day {
    const DAY: usize = 3;

    type In = Vec<String>;

    type Out = u32;

    fn parse(&self, inp: AOCReader) -> Result<Self::In, AOCError> {
        inp.lines().collect()
    }

    fn part1(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        let mut sum = 0;
        for line in input {
            let first: HashSet<_> = line[0..line.len() / 2].chars().collect();
            let second: HashSet<_> = line[line.len() / 2..].chars().collect();

            let common = first
                .intersection(&second)
                .next()
                .ok_or(AOCError::AOCError {
                    msg: "no common found",
                })?;

            sum += priority(common)
        }

        Ok(sum)
    }

    fn part2(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        let chunks = input.chunks(3);

        let mut sum = 0;

        for group in chunks {
            let first: HashSet<_> = group[0].chars().collect();
            let second: HashSet<_> = group[1].chars().collect();
            let third: HashSet<_> = group[2].chars().collect();

            let common_badges = &(&first & &second) & &third;

            let badge = common_badges.into_iter().next().ok_or(AOCError::AOCError {
                msg: "no common badge",
            })?;

            sum += priority(&badge)
        }

        Ok(sum)
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
        lib::test(Day {}, lib::Part::Part1, 1, 157)
    }

    #[test]
    fn test2() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part2, 1, 70)
    }
}
