use lib::{
    parse::*,
    two_dimensional::{parse_grid, Direction, Grid, Position},
    AOCError, AdventOfCode,
};
use pathfinding::prelude::astar;

struct Day {}

fn parse_digit(s: &str) -> ParseResult<usize> {
    let (s, c) = one_of("0123456789")(s)?;

    Ok((s, c.to_digit(10).unwrap() as usize))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct State {
    position: Position,
    direction: Option<Direction>,
    steps: usize,
}

const MAX_CRUCUBLE_STEPS_SAME_DIRECTION: usize = 3;

const MIN_ULTRA_CRUCUBLE_STEPS_SAME_DIRECTION: usize = 4;
const MAX_ULTRA_CRUCUBLE_STEPS_SAME_DIRECTION: usize = 10;

impl State {
    fn new() -> Self {
        Self {
            position: Position { x: 0, y: 0 },
            direction: None,
            steps: 0,
        }
    }

    fn step(&self) -> Vec<State> {
        [
            Direction::North,
            Direction::East,
            Direction::South,
            Direction::West,
        ]
        .iter()
        .filter(|d| {
            self.direction
                .map(|dir| dir.reverse() != **d)
                .unwrap_or(true)
        })
        .filter_map(|dir| {
            if self.direction == Some(*dir) && self.steps == MAX_CRUCUBLE_STEPS_SAME_DIRECTION {
                return None;
            }

            Some(State {
                position: self.position.step(dir),
                direction: Some(*dir),
                steps: if self.direction == Some(*dir) {
                    self.steps + 1
                } else {
                    1
                },
            })
        })
        .collect()
    }

    fn step_ultra(&self) -> Vec<State> {
        if let Some(current_dir) = self.direction {
            if self.steps < MIN_ULTRA_CRUCUBLE_STEPS_SAME_DIRECTION {
                return vec![State {
                    position: self.position.step(&current_dir),
                    direction: self.direction,
                    steps: self.steps + 1,
                }];
            }
        }

        [
            Direction::North,
            Direction::East,
            Direction::South,
            Direction::West,
        ]
        .iter()
        .filter(|d| {
            self.direction
                .map(|dir| dir.reverse() != **d)
                .unwrap_or(true)
        })
        .filter_map(|dir| {
            if self.direction == Some(*dir) && self.steps == MAX_ULTRA_CRUCUBLE_STEPS_SAME_DIRECTION
            {
                return None;
            }

            Some(State {
                position: self.position.step(dir),
                direction: Some(*dir),
                steps: if self.direction == Some(*dir) {
                    self.steps + 1
                } else {
                    1
                },
            })
        })
        .collect()
    }

    fn heuristic(&self, target: &Position) -> usize {
        self.position.manhattan_distance(target)
    }
}

impl AdventOfCode for Day {
    const DAY: usize = 17;

    type In = Grid<usize>;

    type Out = usize;

    fn parse(s: &str) -> ParseResult<Self::In> {
        parse_all(parse_grid(parse_digit))(s)
    }

    fn part1(input: &Self::In) -> Result<Self::Out, AOCError> {
        let res = astar(
            &State::new(),
            |s| {
                s.step()
                    .iter()
                    .filter_map(|s| input.lookup(&s.position).map(|v| (*s, *v)))
                    .collect::<Vec<_>>()
            },
            |s| s.heuristic(&input.bottom_right()),
            |s| s.position == input.bottom_right(),
        );

        if let Some((_, cost)) = res {
            Ok(cost)
        } else {
            Err(AOCError::NoSolution())
        }
    }

    fn part2(input: &Self::In) -> Result<Self::Out, AOCError> {
        let res = astar(
            &State::new(),
            |s| {
                s.step_ultra()
                    .iter()
                    .filter_map(|s| input.lookup(&s.position).map(|v| (*s, *v)))
                    .collect::<Vec<_>>()
            },
            |s| s.heuristic(&input.bottom_right()),
            |s| {
                s.steps >= MIN_ULTRA_CRUCUBLE_STEPS_SAME_DIRECTION
                    && s.position == input.bottom_right()
            },
        );

        if let Some((_, cost)) = res {
            Ok(cost)
        } else {
            Err(AOCError::NoSolution())
        }
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
        lib::test(Day {}, lib::Part::Part1, 1, 102)
    }

    #[test]
    fn test2() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part2, 1, 94)
    }
}
