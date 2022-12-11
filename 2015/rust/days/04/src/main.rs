use lib::{AOCError, AOCReader, AdventOfCode};

struct Day {}

impl AdventOfCode for Day {
    const DAY: usize = 4;

    type In = String;

    type Out = usize;

    fn parse(&self, inp: AOCReader) -> Result<Self::In, AOCError> {
        let first_line = inp.lines().next().ok_or(AOCError::ParseErr())??;
        Ok(first_line)
    }

    fn part1(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        let mut key = 0;

        loop {
            let hash = md5::compute(format!("{}{}", input, key));
            let hash = format!("{:x}", hash);
            if hash.starts_with("00000") {
                return Ok(key);
            }
            key += 1;
        }
    }

    fn part2(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        let mut key = 0;

        loop {
            let hash = md5::compute(format!("{}{}", input, key));
            let hash = format!("{:x}", hash);
            if hash.starts_with("000000") {
                return Ok(key);
            }
            key += 1;
        }
    }
}

fn main() -> Result<(), AOCError> {
    lib::run(Day {})
}
