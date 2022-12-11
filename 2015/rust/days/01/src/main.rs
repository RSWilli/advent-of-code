use lib::{AOCError, AOCReader, AdventOfCode};

struct Day {}

impl AdventOfCode for Day {
    const DAY: usize = 1;

    type In = Vec<i32>;

    type Out = i32;

    fn parse(&self, inp: AOCReader) -> Result<Self::In, AOCError> {
        inp.content()?
            .bytes()
            .map(|c| {
                if c == b'(' {
                    Ok(1)
                } else if c == b')' {
                    Ok(-1)
                } else {
                    Err(AOCError::ParseErr())
                }
            })
            .collect::<Result<Vec<_>, _>>()
    }

    fn part1(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        Ok(input.iter().sum())
    }

    fn part2(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        let mut current_floor = 0;

        for (i, &c) in input.iter().enumerate() {
            current_floor += c;
            if current_floor == -1 {
                return Ok(i as i32 + 1);
            }
        }

        Err(AOCError::AOCError {
            msg: "no floor -1 found",
        })
    }
}

fn main() -> Result<(), AOCError> {
    lib::run(Day {})
}
