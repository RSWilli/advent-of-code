use std::str::FromStr;

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
    fn intersect_and_map(&self, other: Range) -> Option<Range> {
        let new_start = self.start.max(other.start);
        let new_end = self.end.min(other.end);

        let intersected = Range {
            start: new_start + self.diff,
            end: new_end + self.diff,
        };

        if intersected.len() > 0 {
            Some(intersected)
        } else {
            None
        }
    }
}

#[derive(Debug)]
struct AlmanacMap {
    // source_name: String, // TODO can we use &str here?
    // dest_name: String,
    mappings: Vec<MappingRange>,
    identities: Vec<MappingRange>,
}

impl AlmanacMap {
    fn lookup(&self, search: i64) -> i64 {
        self.mappings
            .iter()
            .find(|r| r.start <= search && search < r.end)
            .map(|r| search + r.diff)
            .unwrap_or(search)
    }

    // intersect and map a whole list of ranges
    fn lookup_ranges(&self, ranges: &[Range]) -> Vec<Range> {
        let mapped = ranges.iter().flat_map(|range| {
            self.mappings
                .iter()
                .filter_map(|mr| mr.intersect_and_map(*range))
        });
        let ids = ranges.iter().flat_map(|range| {
            self.identities
                .iter()
                .filter_map(|mr| mr.intersect_and_map(*range))
        });

        let mut res = Vec::new();

        res.extend(mapped);
        res.extend(ids);

        res
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
    let (s, (_source_name, _, _dest_name, _)) = tuple((
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

    map.sort_by_key(|r| r.start);

    let mut identities: Vec<_> = map
        .iter()
        .zip(map.iter().skip(1))
        .map(|(a, b)| MappingRange {
            diff: 0,
            start: a.end,
            end: b.start,
        })
        .collect();

    // add an identity mapping in the front
    if let Some(first) = map.first() {
        if first.start > 0 {
            identities.push(MappingRange {
                diff: 0,
                start: 0,
                end: first.start,
            });
        }
    }

    // add an identity mapping in the back up to i64 max
    if let Some(last) = map.last() {
        identities.push(MappingRange {
            diff: 0,
            start: last.end,
            end: i64::MAX,
        });
    }

    identities.sort_by_key(|r| r.start);

    Ok((
        s,
        AlmanacMap {
            // source_name: source_name.to_owned(),
            // dest_name: dest_name.to_owned(),
            mappings: map,
            identities,
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
        Ok(input
            .seeds
            .iter()
            .map(|seed| input.maps.iter().fold(*seed, |acc, map| map.lookup(acc)))
            .min()
            .unwrap_or(0))
    }

    fn part2(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        let initial_ranges: Vec<_> = input
            .seeds
            .iter()
            .step_by(2)
            .zip(input.seeds.iter().skip(1).step_by(2))
            .map(|(start, end)| Range {
                start: *start,
                end: *start + *end,
            })
            .collect();

        Ok(input
            .maps
            .iter()
            .fold(initial_ranges, |ranges, map| map.lookup_ranges(&ranges))
            .iter()
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
