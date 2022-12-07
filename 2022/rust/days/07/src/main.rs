use filesystem::FileSystem;
use lib::{AOCError, AOCReader, AdventOfCode};
use terminalOutput::TerminalOutput;

struct Day {}

mod filesystem;

mod terminalOutput;

impl AdventOfCode for Day {
    const DAY: usize = 7;

    type In = FileSystem;

    type Out = usize;

    fn parse(&self, inp: AOCReader) -> Result<Self::In, AOCError> {
        let output: Result<Vec<TerminalOutput>, AOCError> = inp.parse_lines().collect();

        output?.try_into()
    }

    fn part1(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        unimplemented!()
        // Ok(input.iter().sum())
    }

    fn part2(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
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
}
