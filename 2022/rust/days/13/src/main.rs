use std::str::FromStr;

use lib::{AOCError, AOCReader, AdventOfCode};
use nom::{
    branch, character::complete, error::ErrorKind, multi::separated_list0, sequence::tuple, Finish,
    IResult,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum PacketData {
    List(Vec<PacketData>),
    Single(usize),
}

fn parse_list(s: &str) -> IResult<&str, PacketData, (&str, ErrorKind)> {
    let (s, (_, l, _)) = tuple((
        complete::char('['),
        separated_list0(complete::char(','), parse),
        complete::char(']'),
    ))(s)?;

    Ok((s, PacketData::List(l)))
}

fn parse_single(s: &str) -> IResult<&str, PacketData, (&str, ErrorKind)> {
    let (s, x) = complete::u32(s)?;

    Ok((s, PacketData::Single(x as usize)))
}

// [1,[2,[3,[4,[5,6,7]]]],8,9]
// [1,[2,[3,[4,[5,6,0]]]],8,9]
fn parse(s: &str) -> IResult<&str, PacketData, (&str, ErrorKind)> {
    branch::alt((parse_list, parse_single))(s)
}

impl PartialOrd for PacketData {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (PacketData::Single(x), PacketData::Single(y)) => x.partial_cmp(y),
            (PacketData::Single(_), PacketData::List(_)) => {
                PacketData::List(vec![self.clone()]).partial_cmp(other)
            }
            (PacketData::List(_), PacketData::Single(_)) => {
                self.partial_cmp(&PacketData::List(vec![other.clone()]))
            }
            (PacketData::List(x), PacketData::List(y)) => x.partial_cmp(y),
        }
    }
}

impl Ord for PacketData {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl FromStr for PacketData {
    type Err = AOCError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse(s).finish() {
            Ok((_, x)) => Ok(x),
            Err(_) => Err(AOCError::ParseErr()),
        }
    }
}

struct Day {}

impl AdventOfCode for Day {
    const DAY: usize = 13;

    type In = Vec<(PacketData, PacketData)>;

    type Out = usize;

    fn parse(&self, inp: AOCReader) -> Result<Self::In, AOCError> {
        let lines = inp.lines().collect::<Result<Vec<_>, _>>()?;

        lines
            .chunks(3)
            .map(|c| {
                let x = c[0].parse()?;
                let y = c[1].parse()?;

                Ok((x, y))
            })
            .collect()
    }

    fn part1(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        Ok(input
            .iter()
            .enumerate()
            .filter(|(_, (x, y))| x < y)
            .map(|(i, _)| i + 1)
            .sum())
    }

    fn part2(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        let div1: PacketData = "[[2]]".parse()?;
        let div2: PacketData = "[[6]]".parse()?;

        let mut flat: Vec<_> = input.iter().flat_map(|(x, y)| vec![x, y]).collect();

        flat.push(&div1);
        flat.push(&div2);

        flat.sort();

        let index1 = flat
            .iter()
            .position(|x| x == &&div1)
            .ok_or(AOCError::ParseErr())?;

        let index2 = flat
            .iter()
            .position(|x| x == &&div2)
            .ok_or(AOCError::ParseErr())?;

        Ok((index1 + 1) * (index2 + 1))
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
        lib::test(Day {}, lib::Part::Part1, 1, 13)
    }

    #[test]
    fn test2() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part2, 1, 140)
    }
}
