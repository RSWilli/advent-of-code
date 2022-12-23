use std::{
    cmp::min,
    collections::{HashMap, HashSet},
    str::FromStr,
};

use id::NodeID;
use lib::{distance::Distance, AOCError, AOCReader, AdventOfCode};
use nom::{
    branch::alt,
    bytes::complete::{tag, take},
    character::complete,
    error::ErrorKind,
    multi::separated_list1,
    sequence::tuple,
    Err, Finish, IResult,
};
use pathfinding::prelude::dijkstra;
use state::State;

mod id;
mod state;

#[derive(Debug)]
struct Valve {
    id: NodeID,
    flow: usize,
    tunnel_to: Vec<NodeID>,
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

    let name = name
        .try_into()
        .map_err(|_| Err::Failure((s, ErrorKind::Not)))?;

    let tunnel = tunnel
        .iter()
        .map(|s| (*s).try_into())
        .collect::<Result<Vec<_>, _>>()
        .map_err(|_| Err::Failure((s, ErrorKind::Not)))?;

    Ok((
        s,
        Valve {
            id: name,
            flow: rate as usize,
            tunnel_to: tunnel,
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

struct Day {}

impl AdventOfCode for Day {
    const DAY: usize = 16;

    type In = Vec<Valve>;

    type Out = usize;

    fn parse(&self, inp: AOCReader) -> Result<Self::In, AOCError> {
        inp.parse_lines().collect()
    }

    fn part1(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        let n = input.len();

        let indices: HashMap<_, _> = input.iter().enumerate().map(|(i, v)| (v.id, i)).collect();

        let names: HashMap<_, _> = input.iter().enumerate().map(|(i, v)| (i, v.id)).collect();

        let mut dist = vec![vec![Distance::Infinity; n]; n];

        for valve in input {
            let i = indices.get(&valve.id).unwrap();
            dist[*i][*i] = 0.into();
            for conn in &valve.tunnel_to {
                let j = indices.get(conn).unwrap();

                dist[*i][*j] = 1.into();
            }
        }

        // floyd warshall:
        for k in 0..n {
            for i in 0..n {
                for j in 0..n {
                    dist[i][j] = min(dist[i][j], dist[i][k] + dist[k][j])
                }
            }
        }

        // print all distances for debugging with node names:
        for i in 0..n {
            let n1 = names.get(&i).unwrap();
            for j in 0..n {
                let n2 = names.get(&j).unwrap();
                println!("{} -> {}: {}", n1, n2, dist[i][j]);
            }
        }

        let step = |current| {
            let ci = indices.get(&current).unwrap();

            dist[*ci]
                .iter()
                .enumerate()
                .filter_map(|(di, cost)| {
                    let valve = &input[di];

                    if valve.flow == 0 {
                        return None;
                    }

                    let dn = names.get(&di).unwrap();
                    match cost {
                        Distance::Infinity => None,
                        Distance::N(c) => Some((*dn, *c, valve.flow)),
                    }
                })
                .collect()
        };

        let s = State::new(NodeID::try_from("AA").unwrap(), 30, &step);

        // TODO: doesn't work
        dijkstra(&s, |p| p.successors(), |p| p.is_time_up())
            .map(|(_, cost)| cost)
            .ok_or(AOCError::AOCError { msg: "no solution" })
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
