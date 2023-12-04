use std::{collections::HashMap, str::FromStr};

use lib::{AOCError, AOCReader, AdventOfCode};

struct Day {}

#[derive(Debug, PartialEq, Eq, Hash)]
struct Position {
    x: i32,
    y: i32,
}

impl Position {
    fn surrounding(&self) -> Vec<Position> {
        vec![
            Position {
                x: self.x - 1,
                y: self.y - 1,
            },
            Position {
                x: self.x,
                y: self.y - 1,
            },
            Position {
                x: self.x + 1,
                y: self.y - 1,
            },
            Position {
                x: self.x - 1,
                y: self.y,
            },
            Position {
                x: self.x + 1,
                y: self.y,
            },
            Position {
                x: self.x - 1,
                y: self.y + 1,
            },
            Position {
                x: self.x,
                y: self.y + 1,
            },
            Position {
                x: self.x + 1,
                y: self.y + 1,
            },
        ]
    }
}

#[derive(Debug)]
struct Component {
    value: usize,
    col_start: i32,
    col_end: i32,
    row: i32,
}

impl Component {
    fn surrounding(&self) -> Vec<Position> {
        let mut positions = Vec::new();

        for x in self.col_start..=self.col_end {
            positions.push(Position { x, y: self.row - 1 });
            positions.push(Position { x, y: self.row + 1 });
        }

        for y in self.row - 1..=self.row + 1 {
            positions.push(Position {
                x: self.col_start - 1,
                y,
            });
            positions.push(Position {
                x: self.col_end + 1,
                y,
            });
        }

        positions
    }

    fn contains(&self, pos: &Position) -> bool {
        pos.y == self.row && self.col_start <= pos.x && pos.x <= self.col_end
    }
}

#[derive(Debug)]
enum Symbol {
    Gear,
    Other,
}

impl Symbol {
    fn is_gear(&self) -> bool {
        matches!(self, Symbol::Gear)
    }
}

#[derive(Debug)]
struct Engine {
    components: Vec<Component>,
    symbols: HashMap<Position, Symbol>,
}

impl FromStr for Engine {
    type Err = AOCError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let lines: Vec<_> = s.lines().collect();

        let mut components = vec![];
        let mut symbols = HashMap::new();

        for (y, line) in lines.iter().enumerate() {
            let mut num_start: Option<usize> = None;
            for (x, char) in line.chars().enumerate() {
                if char.is_ascii_digit() {
                    if num_start.is_none() {
                        num_start = Some(x);
                    }

                    continue;
                } else {
                    if let Some(start) = num_start {
                        let str = line[start..x].to_owned();
                        let number = str.parse::<usize>()?;

                        let col_start = start as i32;
                        let col_end = (x - 1) as i32;
                        let row = y as i32;

                        num_start = None;

                        components.push(Component {
                            value: number,
                            col_start,
                            col_end,
                            row,
                        });
                    }

                    match char {
                        '.' => {
                            // ignored
                        }
                        '*' => {
                            // gear
                            symbols.insert(
                                Position {
                                    x: x as i32,
                                    y: y as i32,
                                },
                                Symbol::Gear,
                            );
                        }
                        _ => {
                            // other
                            symbols.insert(
                                Position {
                                    x: x as i32,
                                    y: y as i32,
                                },
                                Symbol::Other,
                            );
                        }
                    }
                }
            }
        }

        Ok(Engine {
            components,
            symbols,
        })
    }
}

impl AdventOfCode for Day {
    const DAY: usize = 3;

    type In = Engine;

    type Out = usize;

    fn parse(&self, inp: AOCReader) -> Result<Self::In, AOCError> {
        inp.parse_content()
    }

    fn part1(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        Ok(input
            .components
            .iter()
            .filter(|c| {
                c.surrounding()
                    .iter()
                    .any(|p| input.symbols.contains_key(p))
            })
            .map(|c| c.value)
            .sum())
    }

    fn part2(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        let possible_gears = input.symbols.iter().filter(|&(_, s)| s.is_gear());

        let mut total_ratio = 0;

        for (pos, _) in possible_gears {
            let adjacent_components: Vec<_> = input
                .components
                .iter()
                .filter(|c| pos.surrounding().iter().any(|pos| c.contains(pos)))
                .collect();

            if adjacent_components.len() != 2 {
                continue;
            }

            total_ratio += adjacent_components[0].value * adjacent_components[1].value;
        }

        Ok(total_ratio)
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
        lib::test(Day {}, lib::Part::Part1, 1, 4361)
    }
    #[test]
    fn test2() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part1, 2, 0)
    }
    #[test]
    fn test3() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part2, 1, 467835)
    }
}
