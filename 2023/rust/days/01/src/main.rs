use lib::{parse::*, AOCError, AdventOfCode};

struct Day {}

fn parsenum(c: &str, words: bool) -> Result<u32, &str> {
    match c {
        // "0" => Ok(0),
        "1" => Ok(1),
        "2" => Ok(2),
        "3" => Ok(3),
        "4" => Ok(4),
        "5" => Ok(5),
        "6" => Ok(6),
        "7" => Ok(7),
        "8" => Ok(8),
        "9" => Ok(9),
        "one" if words => Ok(1),
        "two" if words => Ok(2),
        "three" if words => Ok(3),
        "four" if words => Ok(4),
        "five" if words => Ok(5),
        "six" if words => Ok(6),
        "seven" if words => Ok(7),
        "eight" if words => Ok(8),
        "nine" if words => Ok(9),
        _ => Err("Invalid number"),
    }
}

fn find_first_num(s: &str, words: bool) -> u32 {
    let max_len = if words { 5 } else { 1 };

    for x in 0..=s.len() {
        for y in x..=s.len().min(x + max_len) {
            if let Ok(n) = parsenum(&s[x..y], words) {
                return n;
            }
        }
    }

    unreachable!()
}

fn find_last_num(s: &str, words: bool) -> u32 {
    let max_len = if words { 5 } else { 1 };

    for x in (0..=s.len()).rev() {
        for y in (x..=s.len().min(x + max_len)).rev() {
            if let Ok(n) = parsenum(&s[x..y], words) {
                return n;
            }
        }
    }

    unreachable!()
}

impl AdventOfCode for Day {
    const DAY: usize = 1;

    type In = Vec<String>;

    type Out = u32;

    fn parse(s: &str) -> ParseResult<Self::In> {
        parse_lines(alphanumeric1_owned)(s)
    }

    fn part1(input: &Self::In) -> Result<Self::Out, AOCError> {
        Ok(input
            .iter()
            .map(|code| {
                let first = find_first_num(code, false);
                let last = find_last_num(code, false);

                10 * first + last
            })
            .sum())
    }

    fn part2(input: &Self::In) -> Result<Self::Out, AOCError> {
        Ok(input
            .iter()
            .map(|code| {
                let first = find_first_num(code, true);
                let last = find_last_num(code, true);

                10 * first + last
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
