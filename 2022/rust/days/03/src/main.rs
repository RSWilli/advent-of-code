use lib::{AOCError, AOCReader, AdventOfCode};

struct Day {}

fn priority(ascii: &u8) -> usize {
    (if ascii.is_ascii_lowercase() {
        ascii - b'a' + 1
    } else {
        ascii - b'A' + 27
    }) as usize
}

impl AdventOfCode for Day {
    const DAY: usize = 3;

    type In = Vec<Vec<u8>>;

    type Out = usize;

    fn parse(&self, inp: AOCReader) -> Result<Self::In, AOCError> {
        inp.lines()
            .map(|line| {
                let line = line?;

                Ok(line.bytes().collect())
            })
            .collect()
    }

    fn part1(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        let mut sum = 0;
        for rucksack in input {
            let first = &rucksack[0..rucksack.len() / 2];
            let second = &rucksack[rucksack.len() / 2..];

            let common = second
                .into_iter()
                .filter(|el| first.contains(el))
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
            let first = &group[0];
            let second = &group[1];
            let third = &group[2];

            let badge = third
                .into_iter()
                .filter(|el| first.contains(el))
                .filter(|el| second.contains(el))
                .next()
                .ok_or(AOCError::AOCError {
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
