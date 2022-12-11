use std::str::FromStr;

use lib::{AOCError, AOCReader, AdventOfCode};

struct Present {
    l: usize,
    w: usize,
    h: usize,
}

impl Present {
    fn surface_area(&self) -> usize {
        2 * self.l * self.w + 2 * self.w * self.h + 2 * self.h * self.l
    }

    fn smallest_side(&self) -> usize {
        let mut sides = [self.l * self.w, self.w * self.h, self.h * self.l];
        sides.sort();
        sides[0]
    }

    fn smallest_perimeter(&self) -> usize {
        let mut dims = [self.l, self.w, self.h];
        dims.sort();

        2 * dims[0] + 2 * dims[1]
    }
}

impl FromStr for Present {
    type Err = AOCError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut dims = s.split('x').map(|s| s.parse::<usize>().unwrap());
        Ok(Present {
            l: dims.next().unwrap(),
            w: dims.next().unwrap(),
            h: dims.next().unwrap(),
        })
    }
}

struct Day {}

impl AdventOfCode for Day {
    const DAY: usize = 2;

    type In = Vec<Present>;

    type Out = usize;

    fn parse(&self, inp: AOCReader) -> Result<Self::In, AOCError> {
        inp.parse_lines().collect()
    }

    fn part1(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        Ok(input
            .iter()
            .map(|b| b.surface_area() + b.smallest_side())
            .sum())
    }

    fn part2(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        Ok(input
            .iter()
            .map(|b| b.smallest_perimeter() + b.l * b.w * b.h)
            .sum())
    }
}

fn main() -> Result<(), AOCError> {
    lib::run(Day {})
}
