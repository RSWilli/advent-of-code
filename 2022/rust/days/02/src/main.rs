use std::str::FromStr;

use lib::{AOCError, AOCReader, AdventOfCode};

struct Day {}

struct Move {
    hand: Hand,
    response: Response,
}

impl FromStr for Move {
    type Err = AOCError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut iter = s.chars();

        let hand = iter.next().ok_or(AOCError::ParseErr())?.try_into()?;
        iter.next();
        let response = iter.next().ok_or(AOCError::ParseErr())?.try_into()?;

        Ok(Move { hand, response })
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum Hand {
    Rock,
    Paper,
    Scissors,
}

impl Hand {
    fn score(&self) -> usize {
        match &self {
            Hand::Rock => 1,
            Hand::Paper => 2,
            Hand::Scissors => 3,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum Response {
    Loose,
    Draw,
    Win,
}

impl Response {
    fn part1_hand(&self) -> Hand {
        //ignore the semantics of response for p1
        match self {
            Response::Loose => Hand::Rock,
            Response::Draw => Hand::Paper,
            Response::Win => Hand::Scissors,
        }
    }

    fn part2_hand(&self, opponent: &Hand) -> Hand {
        match (self, opponent) {
            (Response::Loose, Hand::Rock) => Hand::Scissors,
            (Response::Loose, Hand::Paper) => Hand::Rock,
            (Response::Loose, Hand::Scissors) => Hand::Paper,
            (Response::Win, Hand::Rock) => Hand::Paper,
            (Response::Win, Hand::Paper) => Hand::Scissors,
            (Response::Win, Hand::Scissors) => Hand::Rock,
            (Response::Draw, x) => x.clone(),
        }
    }
}

fn play(f: &Hand, s: &Hand) -> usize {
    if f == s {
        return 3;
    }
    match (f, s) {
        (Hand::Rock, Hand::Paper) => 6,
        (Hand::Rock, Hand::Scissors) => 0,
        (Hand::Paper, Hand::Scissors) => 6,
        (Hand::Paper, Hand::Rock) => 0,
        (Hand::Scissors, Hand::Paper) => 0,
        (Hand::Scissors, Hand::Rock) => 6,
        _ => panic!("cannot happen"),
    }
}

impl TryFrom<char> for Hand {
    type Error = AOCError;
    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            'A' => Ok(Hand::Rock),
            'B' => Ok(Hand::Paper),
            'C' => Ok(Hand::Scissors),
            _ => Err(AOCError::ParseErr()),
        }
    }
}

impl TryFrom<char> for Response {
    type Error = AOCError;
    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            'X' => Ok(Response::Loose),
            'Y' => Ok(Response::Draw),
            'Z' => Ok(Response::Win),
            _ => Err(AOCError::ParseErr()),
        }
    }
}

impl AdventOfCode for Day {
    const DAY: usize = 2;

    type In = Vec<Move>;

    type Out = usize;

    fn parse(&self, inp: AOCReader) -> Result<Self::In, AOCError> {
        inp.parse_lines().collect()
    }

    fn part1(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        Ok(input
            .into_iter()
            .map(|mov| (&mov.hand, mov.response.part1_hand()))
            .map(|(f, s)| play(&f, &s) + s.score())
            .sum())
    }

    fn part2(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        Ok(input
            .into_iter()
            .map(|mov| (&mov.hand, mov.response.part2_hand(&mov.hand)))
            .map(|(f, s)| play(&f, &s) + s.score())
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
        lib::test(Day {}, lib::Part::Part1, 1, 15)
    }

    #[test]
    fn test2() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part2, 1, 12)
    }
}
