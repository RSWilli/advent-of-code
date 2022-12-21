use std::{collections::HashSet, ops::RangeInclusive, str::FromStr};

use lib::{
    spatial::{point2d::Point2D, position::Position},
    AOCError, AOCReader, AdventOfCode, Solution,
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

fn tuning_freq(p: Point2D) -> isize {
    p.x * 4_000_000 + p.y
}

#[cfg(test)]
const Y: isize = 10;

#[cfg(not(test))]
const Y: isize = 2_000_000;

#[cfg(test)]
const MAX_X_Y: isize = 20;

#[cfg(not(test))]
const MAX_X_Y: isize = 4_000_000;

impl AdventOfCode for Day {
    const DAY: usize = 15;

    type In = Vec<Sensor>;

    type Out = Solution<usize, isize>;

    fn parse(&self, inp: AOCReader) -> Result<Self::In, AOCError> {
        inp.parse_lines().collect::<Result<Vec<Sensor>, AOCError>>()
    }

    fn part1(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        let mut beacons: HashSet<isize> = HashSet::new();
        let mut not_beacons: Vec<RangeInclusive<isize>> = Vec::new();

        // collect all beacon visibilities
        for sensor in input {
            if sensor.closest_beacon.y == Y {
                beacons.insert(sensor.closest_beacon.x);
            }
            let dist = sensor.pos.distance(&sensor.closest_beacon);
            let dy = (sensor.pos.y - Y).abs();

            let diff = dist as isize - dy;

            if diff > 0 {
                not_beacons.push((sensor.pos.x - diff)..=(sensor.pos.x + diff));
            }
        }

        // sort by start
        not_beacons.sort_by_key(|r| *r.start());

        let mut count = 0_usize;

        let mut last_x = isize::MIN;

        // count the length of all ranges, keep track of the last x
        // so we don't count the same x twice
        for range in not_beacons {
            if range.start() > &last_x {
                count += (range.end() - range.start() + 1) as usize;
                last_x = *range.end();
            } else if range.end() > &last_x {
                count += (range.end() - last_x) as usize;
                last_x = *range.end();
            }
        }

        // subtract the actual beacons at the end
        Ok(Solution::Part1(count - beacons.len()))
    }

    fn part2(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        // the point we are looking for has to be just at the outside of the
        // sensor range, so we can intersect the border lines
        // a sensor has 4 borders, two ascending:
        // y = (x - x_sensor) + y_sensor + range + 1 = x + (- x_sensor + y_sensor + range + 1)
        // y = (x - x_sensor) + y_sensor - range - 1 = x + (- x_sensor + y_sensor - range - 1)
        // and two descending:
        // y = - (x - x_sensor) + y_sensor + range + 1 = - x + (x_sensor + y_sensor + range + 1)
        // y = - (x - x_sensor) + y_sensor - range - 1 = - x + (x_sensor + y_sensor - range - 1)

        // to result in a single point, we only care about the intersection
        // of an ascending and a descending line

        // since these lines are essentially:
        // ascending: y = x + a
        // descending: y = -x + b
        // the intersection is at x = (b - a) / 2
        // and y = (a + b) / 2

        let mut acoeffs: Vec<isize> = Vec::new();
        let mut bcoeffs: Vec<isize> = Vec::new();

        for sensor in input {
            let dist = sensor.pos.distance(&sensor.closest_beacon) as isize;

            let x_sensor = sensor.pos.x;
            let y_sensor = sensor.pos.y;

            // ascending:
            acoeffs.push(-x_sensor + y_sensor + dist + 1);
            acoeffs.push(-x_sensor + y_sensor - dist - 1);

            // descending:
            bcoeffs.push(x_sensor + y_sensor + dist + 1);
            bcoeffs.push(x_sensor + y_sensor - dist - 1);
        }

        for a in &acoeffs {
            for b in &bcoeffs {
                let x = (b - a) / 2;
                let y = (a + b) / 2;
                let p = Point2D { x, y };

                // check the bounds of p and if p is outside of the range of all sensors
                if (0..=MAX_X_Y).contains(&x)
                    && (0..=MAX_X_Y).contains(&y)
                    && input.iter().all(|sensor| {
                        sensor.pos.distance(&p) > sensor.pos.distance(&sensor.closest_beacon)
                    })
                {
                    return Ok(Solution::Part2(tuning_freq(p)));
                }
            }
        }

        Err(AOCError::AOCError {
            msg: "did not find solution",
        })
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
        lib::test(Day {}, lib::Part::Part1, 1, Solution::Part1(26))
    }
    #[test]
    fn test2() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part2, 1, Solution::Part2(56000011))
    }
}
