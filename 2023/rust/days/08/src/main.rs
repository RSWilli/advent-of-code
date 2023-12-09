use std::{collections::HashMap, str::FromStr};

use lib::{math::lcm, AOCError, AOCReader, AdventOfCode};
use nom::{
    bytes::complete::tag,
    character::complete::{self, char, one_of},
    error::VerboseError,
    multi::many0,
    multi::{many_m_n, separated_list0},
    sequence::tuple,
    Finish, IResult,
};

#[derive(Debug, Hash, Clone, Copy)]
enum Direction {
    Left,
    Right,
}

type Node = (char, char, char);

#[derive(Debug, Hash, Clone, Copy)]
struct Element {
    left: Node,
    right: Node,
}

impl Element {
    fn get(&self, d: Direction) -> Node {
        match d {
            Direction::Left => self.left,
            Direction::Right => self.right,
        }
    }
}

#[derive(Debug)]
struct Map {
    directions: Vec<Direction>,
    elements: HashMap<Node, Element>,
}

impl Map {
    fn steps(&self, from: Node, done: fn(Node) -> bool) -> usize {
        let mut current = from;

        for (i, d) in (0..)
            .map(|i| self.directions[i % self.directions.len()])
            .enumerate()
        {
            if done(current) {
                return i;
            }

            let next = match self.elements.get(&current) {
                Some(e) => e.get(d),
                None => unreachable!(),
            };

            current = next;
        }

        unreachable!()
    }
}

fn parse_direction(s: &str) -> ParseResult<Direction> {
    let (s, c) = complete::one_of("RL")(s)?;

    Ok((
        s,
        match c {
            'R' => Direction::Right,
            'L' => Direction::Left,
            _ => unreachable!(),
        },
    ))
}

fn parse_node(s: &str) -> ParseResult<Node> {
    let (s, c) = many_m_n(3, 3, one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ12"))(s)?;

    Ok((s, (c[0], c[1], c[2])))
}

fn parse_element(s: &str) -> ParseResult<(Node, Element)> {
    let (s, (n1, _, _, n2, _, n3, _)) = tuple((
        parse_node,
        tag(" = "),
        char('('),
        parse_node,
        tag(", "),
        parse_node,
        tag(")"),
    ))(s)?;

    Ok((
        s,
        (
            n1,
            Element {
                left: n2,
                right: n3,
            },
        ),
    ))
}

fn parse_map(s: &str) -> ParseResult<Map> {
    let (s, (directions, _, elements, _)) = tuple((
        many0(parse_direction),
        tag("\n\n"),
        separated_list0(tag("\n"), parse_element),
        tag("\n"),
    ))(s)?;

    Ok((
        s,
        Map {
            directions,
            elements: elements.into_iter().collect(),
        },
    ))
}

impl FromStr for Map {
    type Err = AOCError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_map(s).finish() {
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

type ParseResult<'a, U> = IResult<&'a str, U, VerboseError<&'a str>>;

impl AdventOfCode for Day {
    const DAY: usize = 8;

    type In = Map;

    type Out = usize;

    fn parse(&self, inp: AOCReader) -> Result<Self::In, AOCError> {
        inp.parse_content()
    }

    fn part1(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        Ok(input.steps(('A', 'A', 'A'), |c| c == ('Z', 'Z', 'Z')))
    }

    fn part2(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        let starts = input.elements.keys().filter(|c| c.2 == 'A');

        let steps = starts.map(|start| input.steps(*start, |c| c.2 == 'Z'));

        Ok(steps.fold(1, |acc, s| lcm(acc as i64, s as i64) as usize))
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
        lib::test(Day {}, lib::Part::Part1, 1, 2)
    }

    #[test]
    fn test2() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part2, 2, 6)
    }
}
