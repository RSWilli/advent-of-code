use lib::{AOCError, AOCReader, AdventOfCode};

struct Day {}

fn parsenum(c: &str) -> Result<u32, &str> {
    match c {
        "one" => Ok(1),
        "two" => Ok(2),
        "three" => Ok(3),
        "four" => Ok(4),
        "five" => Ok(5),
        "six" => Ok(6),
        "seven" => Ok(7),
        "eight" => Ok(8),
        "nine" => Ok(9),
        "0" => Ok(0),
        "1" => Ok(1),
        "2" => Ok(2),
        "3" => Ok(3),
        "4" => Ok(4),
        "5" => Ok(5),
        "6" => Ok(6),
        "7" => Ok(7),
        "8" => Ok(8),
        "9" => Ok(9),
        _ => Err("Invalid number"),
    }
}

impl AdventOfCode for Day {
    const DAY: usize = 1;

    type In = Vec<String>;

    type Out = u32;

    fn parse(&self, inp: AOCReader) -> Result<Self::In, AOCError> {
        inp.lines().collect()
    }

    fn part1(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        Ok(input
            .iter()
            .map(|code| {
                let nums: Vec<u32> = code
                    .chars()
                    .filter(|c| c.is_ascii_digit())
                    .map(|c| char::to_digit(c, 10).unwrap())
                    .collect();

                10 * nums[0] + nums.last().unwrap()
            })
            .sum())
    }

    fn part2(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        Ok(input
            .iter()
            .map(|code| {
                let mut nums: Vec<u32> = Vec::new();

                for x in 0..=code.len() {
                    for y in x..=code.len() {
                        if let Ok(n) = parsenum(&code[x..y]) {
                            nums.push(n);
                        }
                    }
                }

                10 * nums[0] + nums.last().unwrap()
            })
            .sum())
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
        lib::test(Day {}, lib::Part::Part1, 1, 142)
    }

    #[test]
    fn test2() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part2, 2, 281)
    }
}
