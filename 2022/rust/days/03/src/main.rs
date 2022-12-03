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

    type In = Vec<(HashSet<char>, HashSet<char>)>;

    type Out = u32;

    fn parse(&self, inp: AOCReader) -> Result<Self::In, AOCError> {
        inp.lines()
            .map(|line| {
                let line = line?;

                let first = &line[0..line.len() / 2];
                let second = &line[line.len() / 2..];

                println!("{} {}", first, second);

                Ok((first.chars().collect(), second.chars().collect()))
            })
            .collect()
    }

    fn part1(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        input
            .into_iter()
            .map(|(first, second)| {
                let overlap = first.intersection(&second);

                let in_both = overlap
                    .into_iter()
                    .nth(0)
                    .ok_or(AOCError::AOCError { msg: "no overlap" })?;

                Ok(priority(in_both))
            })
            .sum()
    }

    fn part2(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        let joined: Vec<_> = input.into_iter().map(|(f, s)| f | s).collect();

        let mut sum = 0;

        for i in 0..input.len() / 3 {
            let first = &joined[3 * i + 0];
            let second = &joined[3 * i + 1];
            let third = &joined[3 * i + 2];

            let common_badges = &(first & second) & third;

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
