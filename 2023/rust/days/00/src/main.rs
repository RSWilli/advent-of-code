use lib::{parse::*, AOCError, AdventOfCode};

struct Day {}

impl AdventOfCode for Day {
    const DAY: usize = 0;

    type In = Vec<u64>;

    type Out = u64;

    fn parse(s: &str) -> ParseResult<Self::In> {
        parse_lines(u64)(s)
    }

    fn part1(input: &Self::In) -> Result<Self::Out, AOCError> {
        println!("{:?}", input);

        unimplemented!()
    }

    fn part2(input: &Self::In) -> Result<Self::Out, AOCError> {
        unimplemented!()
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
        lib::test(Day {}, lib::Part::Part1, 1, 10)
    }

    #[test]
    fn test2() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part2, 1, 10)
    }
}
