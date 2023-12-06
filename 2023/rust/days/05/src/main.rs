use std::{str::FromStr, vec};

use lib::{AOCError, AOCReader, AdventOfCode};
use nom::{
    bytes::complete::tag,
    bytes::complete::take_until,
    character::complete::{self, space1},
    error::VerboseError,
    multi::{separated_list0, separated_list1},
    sequence::tuple,
    Finish, IResult,
};

type ParseResult<'a, U> = IResult<&'a str, U, VerboseError<&'a str>>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct MappingRange {
    diff: i64,
    start: i64,
    end: i64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Range {
    start: i64,
    end: i64,
}

impl Range {
    fn len(&self) -> i64 {
        self.end - self.start
    }
}

impl MappingRange {
    fn intersect_and_map(&self, other: Range) -> Vec<Range> {
        let new_start = self.start.max(other.start);
        let new_end = self.end.min(other.end);

        [
            // old start until intersection, will not get mapped:
            Range {
                start: self.start,
                end: new_start,
            },
            // intersecting:
            Range {
                start: new_start + self.diff,
                end: new_end + self.diff,
            },
            // intersectiong until old end, will not get mapped:
            Range {
                start: new_end,
                end: self.end,
            },
        ]
        .iter()
        .filter(|r| r.len() > 0)
        .copied()
        .collect()
    }
}

#[derive(Debug)]
struct AlmanacMap {
    source_name: String, // TODO can we use &str here?
    dest_name: String,
    mappings: Vec<MappingRange>,
}

impl AlmanacMap {
    fn lookup(&self, search: i64) -> i64 {
        self.mappings
            .iter()
            .find(|r| r.start <= search && search < r.end)
            .map(|r| search + r.diff)
            .unwrap_or(search)
    }

    // find all ranges that intersect the given range
    fn lookup_range(&self, range: Range) -> Vec<Range> {
        self.mappings
            .iter()
            .flat_map(|r| r.intersect_and_map(range))
            .collect()
    }
}

#[derive(Debug)]
struct Almanac {
    seeds: Vec<i64>,
    maps: Vec<AlmanacMap>,
}

// seed-to-soil map:
// 50 98 2
// 52 50 48
fn parse_almanac_map(s: &str) -> ParseResult<AlmanacMap> {
    let (s, (source_name, _, dest_name, _)) = tuple((
        take_until("-"),
        tag("-to-"),
        take_until(" "),
        tag(" map:\n"),
    ))(s)?;
    let (s, mappings) = separated_list0(
        tag("\n"),
        tuple((
            complete::i64,
            tag(" "),
            complete::i64,
            tag(" "),
            complete::i64,
        )),
    )(s)?;

    let mut map = Vec::new();

    for (dest, _, source, _, len) in mappings {
        map.push(MappingRange {
            diff: dest - source,
            start: source,
            end: source + len,
        });
    }

    Ok((
        s,
        AlmanacMap {
            source_name: source_name.to_owned(),
            dest_name: dest_name.to_owned(),
            mappings: map,
        },
    ))
}

// seeds: 79 14 55 13
//
// seed-to-soil map:
// 50 98 2
// 52 50 48
//
// soil-to-fertilizer map:
// 0 15 37
// 37 52 2
// 39 0 15
fn parse_almanac(s: &str) -> ParseResult<Almanac> {
    let (s, (_, _, seeds, _, mapping, _)) = tuple((
        tag("seeds:"),
        space1,
        separated_list0(tag(" "), complete::i64),
        tag("\n\n"),
        separated_list1(tag("\n\n"), parse_almanac_map),
        tag("\n"),
    ))(s)?;

    Ok((
        s,
        Almanac {
            seeds,
            maps: mapping,
        },
    ))
}

impl FromStr for Almanac {
    type Err = AOCError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_almanac(s).finish() {
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
struct Day {}

impl AdventOfCode for Day {
    const DAY: usize = 5;

    type In = Almanac;

    type Out = i64;

    fn parse(&self, inp: AOCReader) -> Result<Self::In, AOCError> {
        inp.parse_content()
    }

    fn part1(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        println!("{:?}", input);
        Ok(input
            .seeds
            .iter()
            .map(|seed| input.maps.iter().fold(*seed, |acc, map| map.lookup(acc)))
            .min()
            .unwrap_or(0))
    }

    fn part2(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        Ok(input
            .seeds
            .iter()
            .step_by(2)
            .zip(input.seeds.iter().skip(1).step_by(2))
            .map(|(start, end)| Range {
                start: *start,
                end: *start + *end,
            })
            // .inspect(|r| println!("new range: {:?}", r))
            .flat_map(|range| {
                input.maps.iter().fold(vec![range], |ranges, map| {
                    println!("{:?}", ranges);
                    println!("{:?}", map);
                    ranges
                        .iter()
                        // .inspect(|r| println!("{:?}", r))
                        .flat_map(|range| map.lookup_range(*range))
                        // .inspect(|r| println!("{:?}", r))
                        .collect()
                })
            })
            .map(|r| r.start)
            .min()
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
        lib::test(Day {}, lib::Part::Part1, 1, 35)
    }

    #[test]
    fn test2() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part2, 1, 46)
    }
}
