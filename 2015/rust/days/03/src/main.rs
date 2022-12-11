use std::{collections::HashSet, str::FromStr};

use lib::{spatial::point2d::Point2D, AOCError, AOCReader, AdventOfCode};

enum Dir {
    Up,
    Down,
    Left,
    Right,
}

impl TryFrom<char> for Dir {
    type Error = AOCError;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            '^' => Ok(Dir::Up),
            'v' => Ok(Dir::Down),
            '<' => Ok(Dir::Left),
            '>' => Ok(Dir::Right),
            _ => Err(AOCError::ParseErr()),
        }
    }
}

struct Day {}

impl AdventOfCode for Day {
    const DAY: usize = 3;

    type In = Vec<Dir>;

    type Out = usize;

    fn parse(&self, inp: AOCReader) -> Result<Self::In, AOCError> {
        inp.content()?
            .bytes()
            .map(|b| (b as char).try_into())
            .collect()
    }

    fn part1(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        let mut houses = HashSet::new();

        let mut pos = Point2D { x: 0, y: 0 };

        for dir in input {
            houses.insert(pos);

            match dir {
                Dir::Up => pos.y += 1,
                Dir::Down => pos.y -= 1,
                Dir::Left => pos.x -= 1,
                Dir::Right => pos.x += 1,
            }
        }

        Ok(houses.len())
    }

    fn part2(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        let mut houses = HashSet::new();

        let mut santa = input.iter().step_by(2);
        let mut robo = input.iter().skip(1).step_by(2);

        let mut santa_pos = Point2D { x: 0, y: 0 };
        let mut robo_pos = Point2D { x: 0, y: 0 };

        for (santa_dir, robo_dir) in santa.zip(robo) {
            houses.insert(santa_pos);
            houses.insert(robo_pos);

            match santa_dir {
                Dir::Up => santa_pos.y += 1,
                Dir::Down => santa_pos.y -= 1,
                Dir::Left => santa_pos.x -= 1,
                Dir::Right => santa_pos.x += 1,
            }

            match robo_dir {
                Dir::Up => robo_pos.y += 1,
                Dir::Down => robo_pos.y -= 1,
                Dir::Left => robo_pos.x -= 1,
                Dir::Right => robo_pos.x += 1,
            }
        }

        Ok(houses.len())
    }
}

fn main() -> Result<(), AOCError> {
    lib::run(Day {})
}
