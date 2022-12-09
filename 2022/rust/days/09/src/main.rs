use std::{
    collections::{HashSet, VecDeque},
    str::FromStr,
};

use lib::{spatial::point2d::Point2D, AOCError, AOCReader, AdventOfCode};

#[derive(Debug, Clone, Copy)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl TryFrom<char> for Direction {
    type Error = AOCError;
    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            'U' => Ok(Direction::Up),
            'D' => Ok(Direction::Down),
            'L' => Ok(Direction::Left),
            'R' => Ok(Direction::Right),
            _ => Err(AOCError::AOCError {
                msg: "received unknown char",
            }),
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct Move {
    direction: Direction,
    steps: usize,
}

impl FromStr for Move {
    type Err = AOCError;

    // U 8
    // D 10
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (direction, steps) = s.split_once(' ').ok_or(AOCError::ParseErr())?;

        let direction = direction.chars().next().ok_or(AOCError::AOCError {
            msg: "could not get direction",
        })?;

        let direction = Direction::try_from(direction)?;

        let steps = steps.parse()?;

        Ok(Move { direction, steps })
    }
}

impl Move {
    fn derease_steps(&mut self) {
        self.steps -= 1;
    }

    fn get_coords(&self) -> (i32, i32) {
        match self.direction {
            Direction::Up => (0, 1),
            Direction::Down => (0, -1),
            Direction::Left => (-1, 0),
            Direction::Right => (1, 0),
        }
    }
}

struct Rope {
    head: Point2D,
    tail: Vec<Point2D>,
    moves: VecDeque<Move>,
    current_move: Move,
}

impl Rope {
    fn new(moves: VecDeque<Move>, length: usize) -> Self {
        Rope {
            head: Point2D { x: 0, y: 0 },
            tail: vec![Point2D { x: 0, y: 0 }; length],
            moves,
            current_move: Move {
                direction: Direction::Up,
                steps: 0,
            },
        }
    }
}

fn move_tail(tail: Point2D, head: Point2D) -> Point2D {
    let tail_dx = head.x - tail.x;
    let tail_dy = head.y - tail.y;

    if tail_dx.abs() > 1 || tail_dy.abs() > 1 {
        tail.move_by(tail_dx.min(1).max(-1), tail_dy.min(1).max(-1))
    } else {
        tail
    }
}

impl Iterator for Rope {
    type Item = Point2D;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current_move.steps != 0 {
            let (dx, dy) = self.current_move.get_coords();
            self.head = self.head.move_by(dx, dy);

            self.tail = self
                .tail
                .iter()
                .scan(self.head, |last, tail| {
                    let new_tail = move_tail(*tail, *last);
                    *last = new_tail;
                    Some(new_tail)
                })
                .collect();

            self.current_move.derease_steps();

            Some(*self.tail.last().expect("no last element"))
        } else {
            if self.moves.is_empty() {
                return None;
            }
            self.current_move = self.moves.pop_front().expect("can't get first move");

            self.next()
        }
    }
}

struct Day {}

impl AdventOfCode for Day {
    const DAY: usize = 9;

    type In = VecDeque<Move>;

    type Out = usize;

    fn parse(&self, inp: AOCReader) -> Result<Self::In, AOCError> {
        inp.parse_lines().collect()
    }

    fn part1(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        let rope = Rope::new(input.clone(), 1);

        let poses = rope.collect::<HashSet<_>>();

        Ok(poses.len())
    }

    fn part2(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        let rope = Rope::new(input.clone(), 9);

        let poses = rope.collect::<HashSet<_>>();

        Ok(poses.len())
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
        lib::test(Day {}, lib::Part::Part1, 1, 13)
    }

    #[test]
    fn test2() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part2, 1, 1)
    }
    #[test]
    fn test3() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part2, 2, 36)
    }
}
