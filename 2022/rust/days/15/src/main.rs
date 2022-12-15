use std::str::FromStr;

use lib::{
    spatial::{point2d::Point2D, position::Position},
    AOCError, AOCReader, AdventOfCode,
};

struct Day {}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct Sensor {
    pos: Point2D,
    closest_beacon: Point2D,
    distance: usize,
}

impl FromStr for Sensor {
    type Err = AOCError;

    //Sensor at x=2, y=18: closest beacon is at x=-2, y=15
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.strip_prefix("Sensor at x=").ok_or(AOCError::ParseErr())?;
        let (x, s) = s.split_once(", y=").ok_or(AOCError::ParseErr())?;
        let (y, s) = s
            .split_once(": closest beacon is at x=")
            .ok_or(AOCError::ParseErr())?;
        let (x_beacon, s) = s.split_once(", y=").ok_or(AOCError::ParseErr())?;

        let x = x.parse()?;
        let y = y.parse()?;
        let x_beacon = x_beacon.parse()?;
        let y_beacon = s.parse()?;

        let pos = Point2D { x, y };
        let closest_beacon = Point2D {
            x: x_beacon,
            y: y_beacon,
        };
        let distance = pos.distance(&closest_beacon);

        Ok(Sensor {
            pos,
            closest_beacon,
            distance,
        })
    }
}

const Y: i32 = 2000000;

impl AdventOfCode for Day {
    const DAY: usize = 15;

    type In = (Vec<Sensor>, i32, i32, i32, i32);

    type Out = usize;

    fn parse(&self, inp: AOCReader) -> Result<Self::In, AOCError> {
        let sensors = inp
            .parse_lines()
            .collect::<Result<Vec<Sensor>, AOCError>>()?;

        let mut min_x = i32::MAX;
        let mut max_x = i32::MIN;
        let mut min_y = i32::MAX;
        let mut max_y = i32::MIN;

        for sensor in sensors.iter() {
            min_x = min_x.min(sensor.pos.x).min(sensor.closest_beacon.x);
            max_x = max_x.max(sensor.pos.x).max(sensor.closest_beacon.x);
            min_y = min_y.min(sensor.pos.y).min(sensor.closest_beacon.y);
            max_y = max_y.max(sensor.pos.y).max(sensor.closest_beacon.y);
        }

        Ok((sensors, min_x, max_x, min_y, max_y))
    }

    fn part1(
        &self,
        (sensors, min_x, max_x, min_y, max_y): &Self::In,
    ) -> Result<Self::Out, AOCError> {
        println!("sensors: {}", sensors.len());
        println!("min_x: {} max_x: {}", min_x, max_x);

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
        lib::test(Day {}, lib::Part::Part1, 1, 26)
    }
}
