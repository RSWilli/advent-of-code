use std::str::FromStr;

use lib::{AOCError, AOCReader, AdventOfCode};
use nom::{
    bytes::complete::tag, character::complete, error::ErrorKind, sequence::tuple, Finish, IResult,
};

#[derive(Debug, Clone, Copy)]
struct Blueprint {
    id: usize,

    ore_robot_cost: usize,

    clay_robot_cost: usize,

    obsidian_robot_ore_cost: usize,
    obsidian_robot_clay_cost: usize,

    geode_robot_ore_cost: usize,
    geode_robot_obsidian_cost: usize,
}

// Blueprint 22: Each ore robot costs 4 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 15 clay. Each geode robot costs 2 ore and 13 obsidian.
// Blueprint 23: Each ore robot costs 3 ore. Each clay robot costs 3 ore. Each obsidian robot costs 2 ore and 16 clay. Each geode robot costs 3 ore and 14 obsidian.
fn parse(s: &str) -> IResult<&str, Blueprint, (&str, ErrorKind)> {
    let (
        s,
        (
            _,
            id,
            _,
            ore_robot_cost,
            _,
            clay_robot_cost,
            _,
            obsidian_robot_ore_cost,
            _,
            obsidian_robot_clay_cost,
            _,
            geode_robot_ore_cost,
            _,
            geode_robot_obsidian_cost,
            _,
        ),
    ) = tuple((
        tag("Blueprint "),
        complete::u32,
        tag(": Each ore robot costs "),
        complete::u32,
        tag(" ore. Each clay robot costs "),
        complete::u32,
        tag(" ore. Each obsidian robot costs "),
        complete::u32,
        tag(" ore and "),
        complete::u32,
        tag(" clay. Each geode robot costs "),
        complete::u32,
        tag(" ore and "),
        complete::u32,
        tag(" obsidian."),
    ))(s)?;

    Ok((
        s,
        Blueprint {
            id: id as usize,
            ore_robot_cost: ore_robot_cost as usize,
            clay_robot_cost: clay_robot_cost as usize,
            obsidian_robot_ore_cost: obsidian_robot_ore_cost as usize,
            obsidian_robot_clay_cost: obsidian_robot_clay_cost as usize,
            geode_robot_ore_cost: geode_robot_ore_cost as usize,
            geode_robot_obsidian_cost: geode_robot_obsidian_cost as usize,
        },
    ))
}

impl FromStr for Blueprint {
    type Err = AOCError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse(s).finish() {
            Ok((_, blueprint)) => Ok(blueprint),
            Err(_) => Err(AOCError::ParseErr()),
        }
    }
}

struct Day {}

impl AdventOfCode for Day {
    const DAY: usize = 19;

    type In = Vec<Blueprint>;

    type Out = usize;

    fn parse(&self, inp: AOCReader) -> Result<Self::In, AOCError> {
        inp.parse_lines().collect()
    }

    fn part1(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        println!("{:?}", input);

        unimplemented!()
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
        lib::test(Day {}, lib::Part::Part1, 1, 10)
    }
}
