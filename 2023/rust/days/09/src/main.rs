use lib::{parse::*, AOCError, AdventOfCode};

#[derive(Debug)]
struct Sequence {
    nums: Vec<i64>,
}

impl Sequence {
    fn extrapolate(&self) -> (i64, i64) {
        let diffs: Vec<_> = self
            .nums
            .iter()
            .zip(self.nums.iter().skip(1))
            .map(|(a, b)| b - a)
            .collect();

        if diffs.iter().any(|&x| x != 0) {
            let diffeseq = Sequence { nums: diffs };

            let (front, back) = diffeseq.extrapolate();

            (self.nums[0] - front, self.nums.last().unwrap() + back)
        } else {
            (self.nums[0], *self.nums.last().unwrap())
        }
    }
}

fn parse_sequence(s: &str) -> ParseResult<Sequence> {
    let (s, nums) = separated_list1(char(' '), i64)(s)?;

    Ok((s, Sequence { nums }))
}

struct Day {}

impl AdventOfCode for Day {
    const DAY: usize = 9;

    type In = Vec<Sequence>;

    type Out = i64;

    fn parse(s: &str) -> ParseResult<Self::In> {
        parse_lines(parse_sequence)(s)
    }

    fn part1(input: &Self::In) -> Result<Self::Out, AOCError> {
        println!("{:?}", input);
        Ok(input.iter().map(Sequence::extrapolate).map(|p| p.1).sum())
    }

    fn part2(input: &Self::In) -> Result<Self::Out, AOCError> {
        Ok(input.iter().map(Sequence::extrapolate).map(|p| p.0).sum())
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
        lib::test(Day {}, lib::Part::Part1, 1, 114)
    }

    #[test]
    fn test2() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part2, 1, 2)
    }
}
