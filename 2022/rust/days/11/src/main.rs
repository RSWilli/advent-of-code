use std::{collections::VecDeque, str::FromStr};

use lib::{AOCError, AOCReader, AdventOfCode};
use operation::Operation;

mod operation;

#[derive(Debug, Clone)]
struct Monkey {
    items: VecDeque<usize>,
    divisor: usize,
    op: Operation,
    if_true: usize,
    if_false: usize,
}

impl FromStr for Monkey {
    type Err = AOCError;
    // Monkey 1:
    //   Starting items: 76, 92, 53, 93, 79, 86, 81
    //   Operation: new = old + 4
    //   Test: divisible by 2
    //     If true: throw to monkey 2
    //     If false: throw to monkey 6
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut lines = s.lines();

        let _monkey = lines.next().ok_or(AOCError::ParseErr())?;
        let items = lines.next().ok_or(AOCError::ParseErr())?;
        let op = lines.next().ok_or(AOCError::ParseErr())?;
        let test = lines.next().ok_or(AOCError::ParseErr())?;
        let if_true = lines.next().ok_or(AOCError::ParseErr())?;
        let if_false = lines.next().ok_or(AOCError::ParseErr())?;

        let items = items
            .strip_prefix("  Starting items: ")
            .ok_or(AOCError::ParseErr())?;

        let items = items
            .split(", ")
            .map(|s| s.parse().or(Err(AOCError::ParseErr())))
            .collect::<Result<_, AOCError>>()?;

        let op = op
            .strip_prefix("  Operation: new = ")
            .ok_or(AOCError::ParseErr())?;

        let op: Operation = op.parse()?;

        let divisor = test
            .strip_prefix("  Test: divisible by ")
            .ok_or(AOCError::ParseErr())?;

        let divisor: usize = divisor.parse().or(Err(AOCError::ParseErr()))?;

        let throw_to_true = if_true
            .strip_prefix("    If true: throw to monkey ")
            .ok_or(AOCError::ParseErr())?;

        let throw_to_true = throw_to_true.parse().or(Err(AOCError::ParseErr()))?;

        let throw_to_false = if_false
            .strip_prefix("    If false: throw to monkey ")
            .ok_or(AOCError::ParseErr())?;

        let throw_to_false = throw_to_false.parse().or(Err(AOCError::ParseErr()))?;

        Ok(Monkey {
            items,
            divisor,
            op,
            if_true: throw_to_true,
            if_false: throw_to_false,
        })
    }
}

impl Monkey {
    fn throw(&self, item: usize) -> (usize, usize) {
        let new = self.op.eval(item);

        let new = new / 3;

        if new % self.divisor == 0 {
            (self.if_true, new)
        } else {
            (self.if_false, new)
        }
    }
}

struct Day {}

impl AdventOfCode for Day {
    const DAY: usize = 11;

    type In = Vec<Monkey>;

    type Out = usize;

    fn parse(&self, inp: AOCReader) -> Result<Self::In, AOCError> {
        inp.content()?.split("\n\n").map(|s| s.parse()).collect()
    }

    fn part1(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        println!("{:?}", input);

        // let mut monkeys: Vec<_> = input.clone();
        // let len = monkeys.len();

        // for round in 0..20 {
        //     let active_monkey = &mut monkeys[round % len];

        //     while let Some(item) = active_monkey.items.pop_front() {
        //         let (monkey, item) = active_monkey.throw(item);

        //         monkeys[monkey].items.push_back(item);
        //     }
        // }

        Ok(0)
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
        lib::test(Day {}, lib::Part::Part1, 1, 10605)
    }
}
