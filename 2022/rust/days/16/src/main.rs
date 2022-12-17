use std::{
    cmp::Reverse,
    collections::{HashMap, HashSet},
    ops::Add,
    str::FromStr,
};

use lib::{AOCError, AOCReader, AdventOfCode};
use nom::{
    branch::alt,
    bytes::complete::{tag, take},
    character::complete,
    error::ErrorKind,
    multi::separated_list1,
    sequence::tuple,
    Finish, IResult,
};
use pathfinding::num_traits::Zero;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct FlowRate(usize);

impl Add for FlowRate {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        FlowRate(self.0 + rhs.0)
    }
}

impl Zero for FlowRate {
    fn zero() -> Self {
        FlowRate(0)
    }

    fn is_zero(&self) -> bool {
        self.0 == 0
    }
}

impl PartialOrd for FlowRate {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Reverse(self.0).partial_cmp(&Reverse(other.0))
    }
}

impl Ord for FlowRate {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        Reverse(self.0).cmp(&Reverse(other.0))
    }
}

#[derive(Debug)]
struct Valve {
    name: String,
    flow: usize,
    tunnel_to: Vec<String>,
}

// Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
// Valve HH has flow rate=22; tunnel leads to valve GG
fn parse(s: &str) -> IResult<&str, Valve, (&str, ErrorKind)> {
    let (s, (_valve, name, _flowrate, rate, _leads, tunnel)) = tuple((
        tag("Valve "),
        take(2_usize),
        tag(" has flow rate="),
        complete::u32,
        alt((
            tag("; tunnels lead to valves "),
            tag("; tunnel leads to valve "),
        )),
        separated_list1(tag(", "), take(2_usize)),
    ))(s)?;

    Ok((
        s,
        Valve {
            name: name.to_string(),
            flow: rate as usize,
            tunnel_to: tunnel.into_iter().map(|s| s.to_string()).collect(),
        },
    ))
}

impl FromStr for Valve {
    type Err = AOCError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse(s)
            .finish()
            .map(|(_, v)| v)
            .map_err(|_| AOCError::ParseErr())
    }
}

#[derive(Debug)]
struct State<'a> {
    remaining_time: usize,
    position: String,
    valves: &'a HashMap<String, Valve>,
    open: HashSet<String>,
}

impl<'a> State<'a> {
    fn new(valves: &'a HashMap<String, Valve>) -> State<'a> {
        State {
            remaining_time: 30,
            position: "AA".to_string(),
            valves,
            open: HashSet::new(),
        }
    }

    fn next(&self) -> Vec<(Self, FlowRate)> {
        let mut next = vec![];

        if self.remaining_time == 0 {
            return next;
        }

        let next_time = self.remaining_time - 1;

        // not moving:
        next.push((
            State {
                remaining_time: next_time,
                position: self.position.clone(),
                valves: self.valves,
                open: self.open.clone(),
            },
            FlowRate(0),
        ));

        let current = self.valves.get(&self.position).unwrap();

        // open:
        if !self.open.contains(&current.name) && current.flow > 0 {
            next.push((
                State {
                    remaining_time: next_time,
                    position: self.position.clone(),
                    valves: self.valves,
                    open: {
                        let mut open = self.open.clone();
                        open.insert(current.name.clone());
                        open
                    },
                },
                FlowRate(current.flow * next_time),
            ));
        }

        // moving:
        for next_pos in &current.tunnel_to {
            let next_valve = self.valves.get(next_pos).unwrap();
            next.push((
                State {
                    remaining_time: next_time,
                    position: next_pos.clone(),
                    valves: self.valves,
                    open: self.open.clone(),
                },
                FlowRate(next_valve.flow),
            ));
        }

        next
    }
}

struct Day {}

impl AdventOfCode for Day {
    const DAY: usize = 16;

    type In = HashMap<String, Valve>;

    type Out = usize;

    fn parse(&self, inp: AOCReader) -> Result<Self::In, AOCError> {
        inp.parse_lines()
            .map(|v: Result<Valve, AOCError>| {
                let v = v?;
                Ok((v.name.clone(), v))
            })
            .collect()
    }

    fn part1(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        println!("{:?}", input);

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
        lib::test(Day {}, lib::Part::Part1, 1, 10)
    }
}
