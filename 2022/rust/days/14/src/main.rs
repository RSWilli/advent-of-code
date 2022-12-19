use std::{
    collections::{HashSet, VecDeque},
    str::FromStr,
};

use lib::{spatial::point2d::Point2D, AOCError, AOCReader, AdventOfCode};

struct Day {}

const START_SAND: Point2D = Point2D { x: 500, y: 0 };
const DOWN: Point2D = Point2D { x: 0, y: 1 };
const DOWN_LEFT: Point2D = Point2D { x: -1, y: 1 };
const DOWN_RIGHT: Point2D = Point2D { x: 1, y: 1 };

struct SandPit<'a> {
    walls: &'a HashSet<Point2D>,
    sand: HashSet<Point2D>,
    min_y: i32,
    sand_count: usize,
}

impl<'a> SandPit<'a> {
    fn new(walls: &'a HashSet<Point2D>, min_y: i32) -> Self {
        SandPit {
            walls,
            sand: HashSet::new(),
            min_y,
            sand_count: 0,
        }
    }

    fn sand_until_exit(&mut self, current: Point2D) -> bool {
        let Point2D { x: _, y } = current;

        if self.walls.contains(&current) {
            return true;
        }

        if self.sand.contains(&current) {
            return true;
        }

        if y == self.min_y {
            return false;
        }

        let backtracked = self.sand_until_exit(current + DOWN)
            && self.sand_until_exit(current + DOWN_LEFT)
            && self.sand_until_exit(current + DOWN_RIGHT);

        if backtracked {
            // if we backtracked all 3 paths, that means we need to backtrack further
            // which places a sand block at the current position
            self.sand.insert(current);
            self.sand_count += 1;
        }

        backtracked
    }

    // essentially a bfs, but count how many steps were taken
    // and return it
    fn sand_until_full(&mut self, current: Point2D) -> bool {
        let Point2D { x: _, y } = current;

        if self.walls.contains(&current) {
            return true;
        }

        if self.sand.contains(&current) {
            return true;
        }

        if y == self.min_y + 2 {
            return true;
        }

        let backtracked = self.sand_until_full(current + DOWN)
            && self.sand_until_full(current + DOWN_LEFT)
            && self.sand_until_full(current + DOWN_RIGHT);

        if backtracked {
            // if we backtracked all 3 paths, that means we need to backtrack further
            // which places a sand block at the current position
            self.sand.insert(current);
            self.sand_count += 1;
        }

        backtracked
    }
}

impl AdventOfCode for Day {
    const DAY: usize = 14;

    type In = (HashSet<Point2D>, i32);

    type Out = usize;

    fn parse(&self, inp: AOCReader) -> Result<Self::In, AOCError> {
        let mut points = HashSet::new();

        let mut goal = 0;

        for line in inp.lines() {
            let line = line?;

            let endpoints = line
                .split(" -> ")
                .map(|s| {
                    let (x, y) = s.split_once(',').ok_or(AOCError::ParseErr())?;

                    let x = x.parse::<i32>()?;
                    let y = y.parse::<i32>()?;

                    Ok(Point2D { x, y })
                })
                .collect::<Result<Vec<_>, AOCError>>()?;

            for w in endpoints.windows(2) {
                let Point2D {
                    x: startx,
                    y: starty,
                } = w[0];
                let Point2D { x: endx, y: endy } = w[1];

                // add all points in between, watch out for rusts range semantics

                let (startx, endx) = if startx < endx {
                    (startx, endx)
                } else {
                    (endx, startx)
                };

                let (starty, endy) = if starty < endy {
                    (starty, endy)
                } else {
                    (endy, starty)
                };

                for y in starty..=endy {
                    for x in startx..=endx {
                        points.insert(Point2D { x, y });

                        if y > goal {
                            goal = y;
                        }
                    }
                }
            }
        }

        Ok((points, goal))
    }

    fn part1(&self, (input, goal): &Self::In) -> Result<Self::Out, AOCError> {
        let mut pit = SandPit::new(input, *goal);

        pit.sand_until_exit(START_SAND);

        Ok(pit.sand_count)
    }

    fn part2(&self, (input, goal): &Self::In) -> Result<Self::Out, AOCError> {
        let mut pit = SandPit::new(input, *goal);

        pit.sand_until_full(START_SAND);

        Ok(pit.sand_count)
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
        lib::test(Day {}, lib::Part::Part1, 1, 24)
    }

    #[test]
    fn test2() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part2, 1, 93)
    }
}
