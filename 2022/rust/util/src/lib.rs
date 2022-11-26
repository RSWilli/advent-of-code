use std::{fmt::Display, fs::File, io};

pub mod error;

mod read;
use read::*;

pub trait AdventOfCode<Parse, Res: Display> {
    const DAY: usize;
    fn parse(&self, inp: io::BufReader<File>) -> Parse;
    fn part1(&self, input: &Parse) -> Res;
    fn part2(&self, input: &Parse) -> Res;
}

pub fn run<P, R: Display, T: AdventOfCode<P, R>>(aoc: T) {
    let input = read_input(T::DAY);
    let parsed = aoc.parse(input);

    println!("Part 1: {}", aoc.part1(&parsed));
    println!("Part 2: {}", aoc.part2(&parsed));
}

pub fn test_part1<P, R: Display, T: AdventOfCode<P, R>>(aoc: T, test: usize) -> R {
    let input = read_test(T::DAY, test);
    let parsed = aoc.parse(input);

    aoc.part1(&parsed)
}

pub fn test_part2<P, R: Display, T: AdventOfCode<P, R>>(aoc: T, test: usize) -> R {
    let input = read_test(T::DAY, test);
    let parsed = aoc.parse(input);

    aoc.part2(&parsed)
}

#[cfg(test)]
mod tests {}
