use std::collections::HashSet;

use lib::{
    spatial::{point3d::Point3D, position::Position},
    AOCError, AOCReader, AdventOfCode,
};
use pathfinding::prelude::astar;

struct Day {}

impl AdventOfCode for Day {
    const DAY: usize = 18;

    type In = HashSet<Point3D>;

    type Out = usize;

    fn parse(&self, inp: AOCReader) -> Result<Self::In, AOCError> {
        inp.lines()
            .map(|s| {
                let s = s?;
                let (x, s) = s.split_once(',').ok_or(AOCError::ParseErr())?;
                let (y, z) = s.split_once(',').ok_or(AOCError::ParseErr())?;

                Ok(Point3D {
                    x: x.parse()?,
                    y: y.parse()?,
                    z: z.parse()?,
                })
            })
            .collect()
    }

    fn part1(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        Ok(input
            .iter()
            .map(|p| p.neighbors().iter().filter(|n| !input.contains(n)).count())
            .sum())
    }

    fn part2(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        let mut min_x = i32::MAX;
        let mut max_x = i32::MIN;
        let mut min_y = i32::MAX;
        let mut max_y = i32::MIN;
        let mut min_z = i32::MAX;
        let mut max_z = i32::MIN;

        for p in input {
            min_x = min_x.min(p.x);
            max_x = max_x.max(p.x);
            min_y = min_y.min(p.y);
            max_y = max_y.max(p.y);
            min_z = min_z.min(p.z);
            max_z = max_z.max(p.z);
        }

        Ok(input
            .iter()
            .map(|p| {
                p.neighbors()
                    .iter()
                    .filter(|n| {
                        if input.contains(n) {
                            return false;
                        }

                        astar(
                            *n,
                            |p| {
                                p.neighbors()
                                    .iter()
                                    .filter(|n| !input.contains(n))
                                    .map(|p| (*p, 1))
                                    // .cloned()
                                    .collect::<Vec<_>>()
                            },
                            |p| {
                                // heuristic to explore the outermost points first
                                let dx = (p.x - min_x).abs();
                                let dy = (p.y - min_y).abs();
                                let dz = (p.z - min_z).abs();

                                dx.min(dy.min(dz))
                            },
                            |p| {
                                p.x < min_x
                                    || p.x > max_x
                                    || p.y < min_y
                                    || p.y > max_y
                                    || p.z < min_z
                                    || p.z > max_z
                            },
                        )
                        .is_some()
                    })
                    .count()
            })
            .sum())
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
        lib::test(Day {}, lib::Part::Part1, 1, 64)
    }

    #[test]
    fn test2() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part2, 1, 58)
    }
}
