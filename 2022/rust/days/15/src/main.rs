use std::{collections::HashSet, ops::RangeInclusive, str::FromStr};

use lib::{
    spatial::{point2d::Point2D, position::Position},
    AOCError, AOCReader, AdventOfCode,
};

struct Day {}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct Sensor {
    pos: Point2D,
    closest_beacon: Point2D,
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

        Ok(Sensor {
            pos,
            closest_beacon,
        })
    }
}

#[cfg(test)]
const Y: i32 = 10;

#[cfg(not(test))]
const Y: i32 = 2_000_000;

impl AdventOfCode for Day {
    const DAY: usize = 15;

    type In = Vec<Sensor>;

    type Out = usize;

    fn parse(&self, inp: AOCReader) -> Result<Self::In, AOCError> {
        inp.parse_lines().collect::<Result<Vec<Sensor>, AOCError>>()
    }

    fn part1(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        let mut beacons: HashSet<i32> = HashSet::new();
        let mut not_beacons: Vec<RangeInclusive<i32>> = Vec::new();

        for sensor in input {
            if sensor.closest_beacon.y == Y {
                beacons.insert(sensor.closest_beacon.x);
            }
            let dist = sensor.pos.distance(&sensor.closest_beacon);
            let dy = (sensor.pos.y - Y).abs();

            let diff = dist as i32 - dy;

            if diff > 0 {
                not_beacons.push((sensor.pos.x - diff)..=(sensor.pos.x + diff));
            }
        }

        not_beacons.sort_by_key(|r| *r.start());

        let mut count = 0_usize;

        let mut last_x = i32::MIN;

        for range in not_beacons {
            if range.start() > &last_x {
                count += (range.end() - range.start() + 1) as usize;
                last_x = *range.end();
            } else if range.end() > &last_x {
                count += (range.end() - last_x) as usize;
                last_x = *range.end();
            }
        }

        Ok(count - beacons.len())
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
