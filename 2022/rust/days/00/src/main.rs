use util::{AOCError, AOCReader, AdventOfCode};

struct Day {}

type Parse = Vec<usize>;

impl AdventOfCode<Parse, usize> for Day {
    const DAY: usize = 0;

    fn parse(&self, inp: AOCReader) -> Result<Parse, AOCError> {
        inp.parse_lines().collect()
    }

    fn part1(&self, input: &Parse) -> Result<usize, AOCError> {
        Ok(input.into_iter().sum())
    }

    fn part2(&self, input: &Parse) -> Result<usize, AOCError> {
        unimplemented!()
    }
}

fn main() -> Result<(), AOCError> {
    util::run(Day {})
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test1() -> Result<(), AOCError> {
        util::test(Day {}, util::Part::Part1, 1, 10)
    }
}
