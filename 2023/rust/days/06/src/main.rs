use std::str::FromStr;

use lib::{AOCError, AOCReader, AdventOfCode};
use nom::{
    bytes::complete::tag,
    character::complete::{self, space1},
    error::VerboseError,
    multi::{separated_list0, separated_list1},
    sequence::tuple,
    Finish, IResult,
};

struct Day {}

type ParseResult<'a, U> = IResult<&'a str, U, VerboseError<&'a str>>;

#[derive(Debug, Clone, Copy)]
struct Race {
    time: i64,
    distance: i64,
}

#[derive(Debug)]
struct Races {
    races: Vec<Race>,
}

// Time:      7  15   30
// Distance:  9  40  200
fn parse_races(s: &str) -> ParseResult<Races> {
    let (s, (_, _, times, _, _, _, distances, _)) = tuple((
        tag("Time:"),
        space1,
        separated_list0(space1, complete::i64),
        tag("\n"),
        tag("Distance:"),
        space1,
        separated_list1(space1, complete::i64),
        tag("\n"),
    ))(s)?;

    Ok((
        s,
        Races {
            races: times
                .iter()
                .zip(distances)
                .map(|(t, d)| Race {
                    time: *t,
                    distance: d,
                })
                .collect(),
        },
    ))
}

impl FromStr for Races {
    type Err = AOCError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_races(s).finish() {
            Ok(("", x)) => Ok(x),
            Err(x) => {
                println!("{}", x);
                Err(AOCError::ParseErr())
            }
            Ok((s, x)) => {
                println!("{:?}", x);
                println!("unconsumed: {:?}", s);
                Err(AOCError::ParseErr())
            }
        }
    }
}

impl AdventOfCode for Day {
    const DAY: usize = 6;

    type In = Races;

    type Out = i64;

    fn parse(&self, inp: AOCReader) -> Result<Self::In, AOCError> {
        inp.parse_content()
    }

    fn part1(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        Ok(input
            .races
            .iter()
            .map(|r| -> i64 {
                // let x be the wait time
                // x * (time-x) > dist
                // time*x  - x² > dist
                // x² - time*x + dist < 0
                let (upper, lower) = lib::math::pq(-r.time as f64, r.distance as f64).unwrap();

                ((upper - 1.0).ceil() - (lower + 1.0).floor() + 1.0) as i64
            })
            .product())
    }

    fn part2(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        let mut total_time = 0;
        let mut total_distance = 0;

        for r in input.races.iter() {
            // lazy: time is always 10<time<100
            total_time = total_time * 100 + r.time;

            if r.distance >= 1000 {
                total_distance = total_distance * 10000 + r.distance;
            } else {
                total_distance = total_distance * 1000 + r.distance;
            }
        }

        let (upper, lower) = lib::math::pq(-total_time as f64, total_distance as f64).unwrap();

        Ok(((upper - 1.0).ceil() - (lower + 1.0).floor() + 1.0) as i64)
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
        lib::test(Day {}, lib::Part::Part1, 1, 288)
    }
}
