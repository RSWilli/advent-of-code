use std::collections::HashSet;

use lib::{parse::*, AOCError, AdventOfCode};
use pathfinding::directed::bfs::bfs_reach;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
struct Position {
    x: isize,
    y: isize,
}

impl Position {
    fn step(&self, dir: &Direction) -> Position {
        match dir {
            Direction::North => Position {
                x: self.x,
                y: self.y - 1,
            },
            Direction::East => Position {
                x: self.x + 1,
                y: self.y,
            },
            Direction::South => Position {
                x: self.x,
                y: self.y + 1,
            },
            Direction::West => Position {
                x: self.x - 1,
                y: self.y,
            },
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Direction {
    North,
    East,
    South,
    West,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Object {
    None,
    // "/"
    Mirror45,
    // "\"
    Mirror135,
    VerticalSplitter,
    HorizontalSplitter,
}

impl Object {
    fn reflect(&self, pos: &Position, dir: &Direction) -> Vec<(Position, Direction)> {
        match self {
            Object::None => vec![(pos.step(dir), *dir)],
            Object::VerticalSplitter => {
                if dir == &Direction::North || dir == &Direction::South {
                    vec![(pos.step(dir), *dir)]
                } else {
                    vec![
                        (pos.step(&Direction::North), Direction::North),
                        (pos.step(&Direction::South), Direction::South),
                    ]
                }
            }
            Object::HorizontalSplitter => {
                if dir == &Direction::East || dir == &Direction::West {
                    vec![(pos.step(dir), *dir)]
                } else {
                    vec![
                        (pos.step(&Direction::East), Direction::East),
                        (pos.step(&Direction::West), Direction::West),
                    ]
                }
            }
            // "/"
            Object::Mirror45 => match dir {
                Direction::North => vec![(pos.step(&Direction::East), Direction::East)],
                Direction::East => vec![(pos.step(&Direction::North), Direction::North)],
                Direction::South => vec![(pos.step(&Direction::West), Direction::West)],
                Direction::West => vec![(pos.step(&Direction::South), Direction::South)],
            },
            // "\"
            Object::Mirror135 => match dir {
                Direction::North => vec![(pos.step(&Direction::West), Direction::West)],
                Direction::East => vec![(pos.step(&Direction::South), Direction::South)],
                Direction::South => vec![(pos.step(&Direction::East), Direction::East)],
                Direction::West => vec![(pos.step(&Direction::North), Direction::North)],
            },
        }
    }
}

#[derive(Debug)]
struct Contraption {
    objects: Vec<Vec<Object>>,
    width: usize,
    height: usize,
}

impl Contraption {
    fn contains(&self, pos: &Position) -> bool {
        pos.x >= 0 && pos.y >= 0 && pos.x < self.width as isize && pos.y < self.height as isize
    }

    fn object_at(&self, pos: &Position) -> Object {
        self.objects[pos.y as usize][pos.x as usize]
    }

    fn illuminate(&self, pos: Position, dir: Direction) -> usize {
        let start = (pos, dir);

        let reachable = bfs_reach(start, |(pos, dir)| {
            let object = self.object_at(pos);

            let next = object.reflect(pos, dir);

            next.iter()
                .filter(|(p, _)| self.contains(p))
                .cloned()
                .collect::<Vec<_>>()
        });

        let unique = reachable.map(|(p, _)| p).collect::<HashSet<_>>();

        unique.len()
    }

    fn top_edge(&self) -> Vec<Position> {
        (0..self.width)
            .map(|x| Position {
                x: x as isize,
                y: 0,
            })
            .collect()
    }

    fn bottom_edge(&self) -> Vec<Position> {
        (0..self.width)
            .map(|x| Position {
                x: x as isize,
                y: self.height as isize - 1,
            })
            .collect()
    }

    fn left_edge(&self) -> Vec<Position> {
        (0..self.height)
            .map(|y| Position {
                x: 0,
                y: y as isize,
            })
            .collect()
    }

    fn right_edge(&self) -> Vec<Position> {
        (0..self.height)
            .map(|y| Position {
                x: self.width as isize - 1,
                y: y as isize,
            })
            .collect()
    }
}

fn parse_object(s: &str) -> ParseResult<Object> {
    let (s, c) = one_of("|-./\\")(s)?;

    Ok((
        s,
        match c {
            '|' => Object::VerticalSplitter,
            '-' => Object::HorizontalSplitter,
            '/' => Object::Mirror45,
            '\\' => Object::Mirror135,
            '.' => Object::None,
            _ => unreachable!(),
        },
    ))
}

fn parse_row(s: &str) -> ParseResult<Vec<Object>> {
    many1(parse_object)(s)
}

fn parse_network(s: &str) -> ParseResult<Contraption> {
    let (s, rows) = separated_list1(tag("\n"), parse_row)(s)?;

    Ok((
        s,
        Contraption {
            width: rows[0].len(),
            height: rows.len(),
            objects: rows,
        },
    ))
}

struct Day {}

impl AdventOfCode for Day {
    const DAY: usize = 16;

    type In = Contraption;

    type Out = usize;

    fn parse(s: &str) -> ParseResult<Self::In> {
        parse_all(parse_network)(s)
    }

    fn part1(input: &Self::In) -> Result<Self::Out, AOCError> {
        Ok(input.illuminate(Position { x: 0, y: 0 }, Direction::East))
    }

    fn part2(input: &Self::In) -> Result<Self::Out, AOCError> {
        let left_edge = input.left_edge().into_iter().map(|p| (p, Direction::East));
        let right_edge = input.right_edge().into_iter().map(|p| (p, Direction::West));
        let top_edge = input.top_edge().into_iter().map(|p| (p, Direction::South));
        let bottom_edge = input
            .bottom_edge()
            .into_iter()
            .map(|p| (p, Direction::North));

        Ok(left_edge
            .chain(right_edge)
            .chain(top_edge)
            .chain(bottom_edge)
            .map(|(p, d)| input.illuminate(p, d))
            .max()
            .unwrap_or(0))
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
        lib::test(Day {}, lib::Part::Part1, 1, 46)
    }

    #[test]
    fn test2() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part2, 1, 51)
    }
}
