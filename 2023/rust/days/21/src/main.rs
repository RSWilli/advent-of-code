use std::fmt::Display;

use lib::{
    parse::*,
    two_dimensional::{parse_grid, Grid, Position},
    AOCError, AdventOfCode,
};
use pathfinding::directed::bfs::bfs_reach;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Object {
    GardenPlot,
    Rock,
    Start,
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let c = match self {
            Object::GardenPlot => '.',
            Object::Rock => '#',
            Object::Start => 'S',
        };

        write!(f, "{}", c)
    }
}

fn parse_object(s: &str) -> ParseResult<Object> {
    let (s, c) = one_of(".#S")(s)?;

    Ok((
        s,
        match c {
            '.' => Object::GardenPlot,
            '#' => Object::Rock,
            'S' => Object::Start,
            _ => unreachable!(),
        },
    ))
}

#[derive(Debug, Clone, Copy)]
struct State {
    position: Position,
    steps: usize,
}

impl State {
    fn new(position: Position) -> Self {
        Self { position, steps: 0 }
    }

    fn neighbors(&self) -> Vec<Self> {
        self.position
            .neighbors()
            .iter()
            .cloned()
            .map(|p| Self {
                position: p,
                steps: self.steps + 1,
            })
            .collect()
    }
}

// eq ignores the steps
impl PartialEq for State {
    fn eq(&self, other: &Self) -> bool {
        self.position == other.position
    }
}

impl Eq for State {}

// hash ignores the steps
impl std::hash::Hash for State {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.position.hash(state);
    }
}

fn walk(start: Position, grid: &Grid<Object>, steps: usize) -> usize {
    let start = State::new(start);
    bfs_reach(start, |s| {
        s.neighbors()
            .into_iter()
            // we can ignore the start position, since even when we let it through
            // it will be filtered out, because it was already visited
            .filter(|s| grid.lookup(&s.position) == Some(&Object::GardenPlot))
    })
    .filter(|s| s.steps <= steps && s.steps % 2 == steps % 2)
    .count()
}

struct Day {}

impl AdventOfCode for Day {
    const DAY: usize = 21;

    type In = (Position, Grid<Object>);

    type Out = usize;

    fn parse(s: &str) -> ParseResult<Self::In> {
        let (s, grid) = parse_all(parse_grid(parse_object))(s)?;

        let start = grid.entries().find_map(|(p, o)| match o {
            Object::Start => Some(p),
            _ => None,
        });

        Ok((s, (start.unwrap(), grid)))
    }

    fn part1(input: &Self::In) -> Result<Self::Out, AOCError> {
        let (start, grid) = input;

        Ok(walk(*start, grid, 64))
    }

    fn part2(input: &Self::In) -> Result<Self::Out, AOCError> {
        let (start, grid) = input;

        Ok(walk(*start, grid, 26501365))
    }
}

fn main() -> Result<(), AOCError> {
    lib::run(Day {})
}

#[cfg(test)]
mod tests {
    // use super::*;

    // #[test]
    // fn test1() -> Result<(), AOCError> {
    //     lib::test(Day {}, lib::Part::Part1, 1, 10)
    // }

    // #[test]
    // fn test2() -> Result<(), AOCError> {
    //     lib::test(Day {}, lib::Part::Part2, 1, 10)
    // }
}
