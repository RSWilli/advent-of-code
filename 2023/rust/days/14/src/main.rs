use std::fmt::Display;

use lib::{parse::*, AOCError, AdventOfCode};
use pathfinding::cycle_detection::floyd;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Rock {
    Cube,
    Round,
    None,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Platform {
    objects: Vec<Vec<Rock>>,
    width: usize,
    height: usize,
}

impl Display for Platform {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for row in &self.objects {
            for rock in row {
                let c = match rock {
                    Rock::Cube => '#',
                    Rock::Round => 'O',
                    Rock::None => '.',
                };
                write!(f, "{}", c)?;
            }
            writeln!(f)?;
        }

        Ok(())
    }
}

fn parse_row(s: &str) -> ParseResult<Vec<Rock>> {
    let (s, row) = many1(one_of("#O."))(s)?;

    let objects = row
        .iter()
        .map(|c| match c {
            '.' => Rock::None,
            '#' => Rock::Cube,
            'O' => Rock::Round,
            _ => unreachable!(),
        })
        .collect();

    Ok((s, objects))
}
fn parse_platform(s: &str) -> ParseResult<Platform> {
    let (s, rows) = separated_list1(tag("\n"), parse_row)(s)?;

    Ok((
        s,
        Platform {
            width: rows[0].len(),
            height: rows.len(),
            objects: rows,
        },
    ))
}

impl Platform {
    fn tilt_north(&mut self) {
        for y in 1..self.height {
            'rockloop: for x in 0..self.width {
                if self.objects[y][x] != Rock::Round {
                    continue;
                }

                for new_y in (0..y).rev() {
                    match self.objects[new_y][x] {
                        Rock::None => continue,
                        Rock::Cube | Rock::Round => {
                            self.objects[new_y + 1][x] = Rock::Round;
                            if y != new_y + 1 {
                                self.objects[y][x] = Rock::None;
                            }
                            continue 'rockloop;
                        }
                    }
                }

                self.objects[0][x] = Rock::Round;
                self.objects[y][x] = Rock::None;
            }
        }
    }

    fn tilt_east(&mut self) {
        for x in (0..self.width - 1).rev() {
            'rockloop: for y in 0..self.height {
                if self.objects[y][x] != Rock::Round {
                    continue;
                }

                for new_x in x + 1..self.width {
                    match self.objects[y][new_x] {
                        Rock::None => continue,
                        Rock::Cube | Rock::Round => {
                            self.objects[y][new_x - 1] = Rock::Round;
                            if x != new_x - 1 {
                                self.objects[y][x] = Rock::None;
                            }
                            continue 'rockloop;
                        }
                    }
                }

                self.objects[y][self.width - 1] = Rock::Round;
                self.objects[y][x] = Rock::None;
            }
        }
    }

    fn tilt_south(&mut self) {
        for y in (0..self.height - 1).rev() {
            'rockloop: for x in 0..self.width {
                if self.objects[y][x] != Rock::Round {
                    continue;
                }

                for new_y in y + 1..self.height {
                    match self.objects[new_y][x] {
                        Rock::None => continue,
                        Rock::Cube | Rock::Round => {
                            self.objects[new_y - 1][x] = Rock::Round;
                            if y != new_y - 1 {
                                self.objects[y][x] = Rock::None;
                            }
                            continue 'rockloop;
                        }
                    }
                }

                self.objects[self.height - 1][x] = Rock::Round;
                self.objects[y][x] = Rock::None;
            }
        }
    }

    fn tilt_west(&mut self) {
        for x in 1..self.width {
            'rockloop: for y in 0..self.height {
                if self.objects[y][x] != Rock::Round {
                    continue;
                }

                for new_x in (0..x).rev() {
                    match self.objects[y][new_x] {
                        Rock::None => continue,
                        Rock::Cube | Rock::Round => {
                            self.objects[y][new_x + 1] = Rock::Round;
                            if x != new_x + 1 {
                                self.objects[y][x] = Rock::None;
                            }
                            continue 'rockloop;
                        }
                    }
                }

                self.objects[y][0] = Rock::Round;
                self.objects[y][x] = Rock::None;
            }
        }
    }

    fn tilt_around(&mut self) {
        self.tilt_north();
        self.tilt_west();
        self.tilt_south();
        self.tilt_east();
    }

    fn load(&self) -> usize {
        let mut sum = 0;
        for y in 0..self.height {
            for x in 0..self.width {
                let rock = self.objects[y][x];
                if rock == Rock::Round {
                    sum += self.height - y;
                }
            }
        }

        sum
    }
}

struct Day {}

impl AdventOfCode for Day {
    const DAY: usize = 14;

    type In = Platform;

    type Out = usize;

    fn parse(s: &str) -> ParseResult<Self::In> {
        parse_all(parse_platform)(s)
    }

    fn part1(input: &Self::In) -> Result<Self::Out, AOCError> {
        let mut input = input.clone();

        input.tilt_north();

        Ok(input.load())
    }

    fn part2(input: &Self::In) -> Result<Self::Out, AOCError> {
        let input = input.clone();

        let (period, x0, i) = floyd(input, |p| {
            let mut p = p.clone();
            p.tilt_around();
            p
        });

        let mut x0 = x0;

        let cycles = 1_000_000_000;

        let cycles_left = (cycles - i) % period;

        for _ in 0..cycles_left {
            x0.tilt_around();
        }

        Ok(x0.load())
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
        lib::test(Day {}, lib::Part::Part1, 1, 136)
    }

    #[test]
    fn test2() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part2, 1, 64)
    }
}
