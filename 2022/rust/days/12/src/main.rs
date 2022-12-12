use lib::{
    spatial::{dense::SpatialDense, point2d::Point2D, position::Position, spatial_trait::Spatial},
    AOCError, AOCReader, AdventOfCode,
};
use pathfinding::prelude::dijkstra;

#[derive(Debug)]
struct Hill(SpatialDense<Point2D, char>);

impl Hill {
    fn climb_up_moves(&self, pos: Point2D) -> Vec<(Point2D, usize)> {
        let current = (*self.0.get(pos).unwrap()) as u8;

        pos.neighbors()
            .iter()
            .filter(|p| {
                let target = self.0.get(**p);

                let current = if current == b'S' { b'a' } else { current };

                match target {
                    Some(&'S') => false,
                    Some(&'E') => current == b'z' || current == b'y',
                    Some(val) => *val as u8 <= current + 1,
                    None => false,
                }
            })
            .map(|p| (*p, 1))
            .collect()
    }

    fn climb_down_moves(&self, pos: Point2D) -> Vec<(Point2D, usize)> {
        let current = (*self.0.get(pos).unwrap()) as u8;

        pos.neighbors()
            .iter()
            .filter(|p| {
                let target = self.0.get(**p);

                let current = if current == b'E' { b'z' } else { current };

                match target {
                    Some(&'S') => false,
                    Some(&'E') => false,
                    Some(val) => *val as u8 >= current - 1,
                    None => false,
                }
            })
            .map(|p| (*p, 1))
            .collect()
    }
}

struct Day {}

impl AdventOfCode for Day {
    const DAY: usize = 12;

    type In = (Point2D, Point2D, Hill);

    type Out = usize;

    fn parse(&self, inp: AOCReader) -> Result<Self::In, AOCError> {
        let field: SpatialDense<Point2D, char> = inp.parse_content()?;

        let start = field.find(&'S').ok_or(AOCError::ParseErr())?;
        let end = field.find(&'E').ok_or(AOCError::ParseErr())?;

        Ok((start, end, Hill(field)))
    }

    fn part1(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        let shortest_path = dijkstra(&input.0, |p| input.2.climb_up_moves(*p), |p| *p == input.1);

        if let Some((path, _)) = shortest_path {
            Ok(path.len() - 1)
        } else {
            Err(AOCError::AOCError {
                msg: "no path found",
            })
        }
    }

    fn part2(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        let shortest_path = dijkstra(
            &input.1,
            |p| input.2.climb_down_moves(*p),
            |p| Some(&'a') == (input.2).0.get(*p),
        );

        if let Some((path, _)) = shortest_path {
            Ok(path.len() - 1)
        } else {
            Err(AOCError::AOCError {
                msg: "no path found",
            })
        }
    }
}

fn main() -> Result<(), AOCError> {
    lib::run(Day {})
}

#[cfg(test)]
mod tests {
    use lib::get_test;

    use super::*;

    #[test]
    fn test1() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part1, 1, 31)
    }

    #[test]
    fn test2() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part2, 1, 29)
    }

    #[test]
    fn test_neighbors() -> Result<(), AOCError> {
        let (s, e, hill) = get_test(Day {}, 1)?;

        let neighbors = hill.climb_up_moves(s);
        println!("{:?}", neighbors);
        assert_eq!(neighbors.len(), 2);

        let neighbors = hill.climb_up_moves(e);
        println!("{:?}", neighbors);
        assert_eq!(neighbors.len(), 0);

        let neighbors = hill.climb_up_moves(Point2D { x: 2, y: 1 });
        println!("{:?}", neighbors);
        assert_eq!(neighbors.len(), 3);

        Ok(())
    }
}
