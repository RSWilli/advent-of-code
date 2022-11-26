use std::{
    fs::File,
    io::{BufRead, BufReader},
};

use util::AdventOfCode;

struct Day {}

type Parse = Vec<usize>;

impl AdventOfCode<Parse, usize> for Day {
    const DAY: usize = 0;

    fn parse(&self, inp: BufReader<File>) -> Parse {
        inp.lines().map(|l| l.unwrap().parse().unwrap()).collect()
    }

    fn part1(&self, input: &Parse) -> usize {
        input.into_iter().sum()
    }

    fn part2(&self, input: &Parse) -> usize {
        unimplemented!()
    }
}

fn main() {
    util::run(Day {});
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test1() {
        assert_eq!(util::test_part1(Day {}, 1), 10)
    }
}
