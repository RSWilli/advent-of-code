use std::{collections::HashMap, str::FromStr};

use lib::{AOCError, AOCReader, AdventOfCode};
use nom::{
    branch, bytes::complete::tag, character::complete, error::ErrorKind, multi::separated_list0,
    sequence::tuple, Finish, IResult,
};

#[derive(Debug, PartialEq, Eq, Hash)]
enum Cube {
    Red,
    Green,
    Blue,
}

#[derive(Debug)]
struct Game {
    id: u32,
    moves: Vec<HashMap<Cube, u32>>,
}

impl Game {
    fn power(&self) -> u32 {
        let mut max_red = 0;
        let mut max_green = 0;
        let mut max_blue = 0;

        for move_set in &self.moves {
            max_red = max_red.max(*move_set.get(&Cube::Red).unwrap_or(&0));
            max_green = max_green.max(*move_set.get(&Cube::Green).unwrap_or(&0));
            max_blue = max_blue.max(*move_set.get(&Cube::Blue).unwrap_or(&0));
        }

        max_red * max_green * max_blue
    }
}

fn parse_cube(s: &str) -> IResult<&str, Cube, (&str, ErrorKind)> {
    let (s, color) = branch::alt((tag("red"), tag("green"), tag("blue")))(s)?;

    let cube = match color {
        "red" => Cube::Red,
        "green" => Cube::Green,
        "blue" => Cube::Blue,
        _ => unreachable!(),
    };

    Ok((s, cube))
}

fn parse_cube_count(s: &str) -> IResult<&str, (u32, Cube), (&str, ErrorKind)> {
    let (s, (count, _, cube)) = tuple((complete::u32, complete::space0, parse_cube))(s)?;

    Ok((s, (count, cube)))
}

// 5 green, 4 blue, 1 red
fn parse_cube_set(s: &str) -> IResult<&str, HashMap<Cube, u32>, (&str, ErrorKind)> {
    let (s, cube_list) = separated_list0(tag(", "), parse_cube_count)(s)?;

    let mut cubes = HashMap::new();

    for cube in cube_list {
        *cubes.entry(cube.1).or_insert(0) += cube.0;
    }

    Ok((s, cubes))
}

// Game 74: 5 green, 4 blue, 1 red; 4 red, 6 blue; 2 red; 2 blue, 1 red; 3 blue, 1 green, 3 red
fn parse_game(s: &str) -> IResult<&str, Game, (&str, ErrorKind)> {
    let (s, (_, id, _, moves)) = tuple((
        tag("Game "),
        complete::u32,
        tag(": "),
        separated_list0(tag("; "), parse_cube_set),
    ))(s)?;

    Ok((s, Game { id, moves }))
}

impl FromStr for Game {
    type Err = AOCError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_game(s).finish() {
            Ok((_, x)) => Ok(x),
            Err(_) => Err(AOCError::ParseErr()),
        }
    }
}

struct Day {}

impl AdventOfCode for Day {
    const DAY: usize = 2;

    type In = Vec<Game>;

    type Out = u32;

    fn parse(&self, inp: AOCReader) -> Result<Self::In, AOCError> {
        inp.parse_lines().collect()
    }

    fn part1(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        // only 12 red cubes, 13 green cubes, and 14 blue cubes
        let limit_red = 12_u32;
        let limit_green = 13_u32;
        let limit_blue = 14_u32;

        let valid_games_id_sum = input
            .iter()
            .filter(|game| {
                game.moves.iter().all(|move_set| {
                    move_set.get(&Cube::Red).unwrap_or(&0) <= &limit_red
                        && move_set.get(&Cube::Green).unwrap_or(&0) <= &limit_green
                        && move_set.get(&Cube::Blue).unwrap_or(&0) <= &limit_blue
                })
            })
            .map(|game| game.id)
            .sum();

        Ok(valid_games_id_sum)
    }

    fn part2(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        Ok(input.iter().map(|game| game.power()).sum())
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
        lib::test(Day {}, lib::Part::Part1, 1, 8)
    }

    #[test]
    fn test2() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part2, 1, 2286)
    }
}
