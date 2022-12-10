use std::{collections::VecDeque, fmt::Debug, str::FromStr};

use lib::{
    ocr::{DARK_PIXEL, LIGHT_PIXEL},
    spatial::dense::Spatial,
    AOCError, AOCReader, AdventOfCode, Solution,
};

struct Day {}

#[derive(Debug, Clone)]
enum Op {
    Noop,
    AddX(i32),
}

impl FromStr for Op {
    type Err = AOCError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.split_once(' ') {
            Some((_, val)) => {
                let val = val.parse::<i32>()?;
                Ok(Op::AddX(val))
            }
            None => Ok(Op::Noop),
        }
    }
}

impl AdventOfCode for Day {
    const DAY: usize = 10;

    type In = VecDeque<Op>;

    type Out = Solution<i32, String>;

    fn parse(&self, inp: AOCReader) -> Result<Self::In, AOCError> {
        inp.parse_lines().collect()
    }

    fn part1(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        let mut signal_strength_sum = 0;

        let mut cycles = 1;

        let mut reg_x = 1;

        for op in input {
            match op {
                Op::Noop => {
                    cycles += 1;
                }
                Op::AddX(val) => {
                    if (cycles - 19) % 40 == 0 {
                        signal_strength_sum += reg_x * (cycles + 1);
                    }
                    reg_x += val;
                    cycles += 2;
                }
            }
            if (cycles - 20) % 40 == 0 {
                signal_strength_sum += reg_x * cycles;
            }
        }

        Ok(Solution::Part1(signal_strength_sum))
    }

    fn part2(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        let mut sprite_pos = 1;

        let mut screen = Vec::with_capacity(40 * 6);

        let mut beam_x = 0;

        for op in input {
            if sprite_pos - 1 <= beam_x && beam_x <= sprite_pos + 1 {
                screen.push(DARK_PIXEL);
            } else {
                screen.push(LIGHT_PIXEL);
            }

            match op {
                Op::Noop => {}
                Op::AddX(val) => {
                    beam_x = (beam_x + 1) % 40;

                    if sprite_pos - 1 <= beam_x && beam_x <= sprite_pos + 1 {
                        screen.push(DARK_PIXEL);
                    } else {
                        screen.push(LIGHT_PIXEL);
                    }

                    sprite_pos += val;
                }
            }

            beam_x = (beam_x + 1) % 40;
        }

        let screen = Spatial::with_content((0, 0).into(), (39, 5).into(), screen);

        Ok(Solution::Part2(screen.read()))
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
        lib::test(Day {}, lib::Part::Part1, 2, Solution::Part1(13140))
    }
}
