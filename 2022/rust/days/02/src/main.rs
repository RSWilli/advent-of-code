use std::{collections::BinaryHeap, str::FromStr};

use lib::{AOCError, AOCReader, AdventOfCode};

struct Day {}

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

impl FromStr for Hand {
    type Err = AOCError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "A" => Ok(Hand::Rock),
            "B" => Ok(Hand::Paper),
            "C" => Ok(Hand::Scissors),
            _ => Err(AOCError::ParseErr()),
        }
    }
}

impl FromStr for Response {
    type Err = AOCError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "X" => Ok(Response::Loose),
            "Y" => Ok(Response::Draw),
            "Z" => Ok(Response::Win),
            _ => Err(AOCError::ParseErr()),
        }
    }
}

impl AdventOfCode for Day {
    const DAY: usize = 2;

    type In = Vec<(Hand, Response)>;

    type Out = usize;

    fn parse(&self, inp: AOCReader) -> Result<Self::In, AOCError> {
        inp.lines()
            .map(|line| -> Result<(Hand, Response), lib::AOCError> {
                let line = line?;

                let (f, s) = line.split_once(" ").ok_or(AOCError::ParseErr())?;

                let first = f.parse()?;
                let second = s.parse()?;

                Ok((first, second))
            })
            .collect()
    }

    fn part1(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        Ok(input
            .into_iter()
            .map(|(f, s)| -> (Hand, Hand) { (*f, s.part1_hand()) })
            .map(|(f, s)| -> usize { play(&f, &s) + s.score() })
            .sum())
    }

    fn part2(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        Ok(input
            .into_iter()
            .map(|(f, s)| -> (Hand, Hand) { (*f, s.part2_hand(f)) })
            .map(|(f, s)| -> usize { play(&f, &s) + s.score() })
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
