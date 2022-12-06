use lib::{AOCError, AOCReader, AdventOfCode};

struct Day {}

fn all_unique(v: &[char]) -> bool {
    for (i, n) in v.iter().enumerate() {
        for n2 in v.iter().skip(i + 1) {
            if n == n2 {
                return false;
            }
        }
    }
    true
}

impl AdventOfCode for Day {
    const DAY: usize = 6;

    type In = Vec<char>;

    type Out = usize;

    fn parse(&self, inp: AOCReader) -> Result<Self::In, AOCError> {
        let content = inp.content()?;

        Ok(content.bytes().map(|c| c as char).collect())
    }

    fn part1(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        for i in 4..input.len() {
            let ns = &input[i - 4..i];

            if all_unique(ns) {
                return Ok(i);
            }
        }

        Err(AOCError::AOCError {
            msg: "did not find index",
        })
    }

    fn part2(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        for i in 14..input.len() {
            let ns = &input[i - 14..i];

            if all_unique(ns) {
                return Ok(i);
            }
        }

        Err(AOCError::AOCError {
            msg: "did not find index",
        })
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
        lib::test(Day {}, lib::Part::Part1, 1, 7)
    }

    #[test]
    fn test2() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part2, 1, 19)
    }
}
