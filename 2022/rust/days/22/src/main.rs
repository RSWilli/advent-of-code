use cube_net::parse_cube_net;
use face::Face;
use instruction::{parse_instructions, Instruction};
use lib::{AOCError, AOCReader, AdventOfCode};
use nom::{bytes::complete::tag, sequence::tuple, Finish};
use parse::ParseResult;

mod cube_net;

mod cube;
mod face;
mod instruction;
mod parse;

fn parse(s: &str) -> ParseResult<(Vec<Vec<Face>>, Vec<Instruction>)> {
    let (s, (tiles, _, instructions)) = tuple((parse_cube_net, tag("\n"), parse_instructions))(s)?;

    Ok((s, (tiles, instructions)))
}

struct Day {}

impl AdventOfCode for Day {
    const DAY: usize = 22;

    type In = (Vec<Vec<Face>>, Vec<Instruction>);

    type Out = usize;

    fn parse(&self, inp: AOCReader) -> Result<Self::In, AOCError> {
        let s = inp.content()?;

        match parse(&s).finish() {
            Ok((_, x)) => Ok(x),
            Err(e) => {
                println!("{:?}", e);
                Err(AOCError::ParseErr())
            }
        }
    }

    fn part1(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        println!("{:?}", input);

        unimplemented!()
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
