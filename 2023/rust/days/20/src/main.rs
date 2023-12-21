use std::collections::{HashMap, VecDeque};

use lib::{parse::*, AOCError, AdventOfCode};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Pulse {
    High,
    Low,
}

impl Pulse {
    fn toggle(&mut self) {
        match self {
            Self::High => *self = Self::Low,
            Self::Low => *self = Self::High,
        }
    }
}

#[derive(Debug, Clone)]
enum Circuit {
    FlipFlop { state: Pulse },
    Conjunction { connections: HashMap<String, Pulse> },
    Simple,
}

impl Circuit {
    fn new_flipflop() -> Self {
        Self::FlipFlop { state: Pulse::Low }
    }

    fn new_conjunction() -> Self {
        Self::Conjunction {
            connections: HashMap::new(),
        }
    }

    fn new_simple() -> Self {
        Self::Simple
    }

    fn add_connection(&mut self, name: String) {
        match self {
            Self::FlipFlop { .. } => {}
            Self::Simple => {}
            Self::Conjunction { connections } => {
                connections.insert(name, Pulse::Low);
            }
        }
    }

    fn input(&mut self, from: &str, pulse: &Pulse) -> Option<Pulse> {
        match (self, pulse) {
            (Circuit::Simple, x) => Some(*x),
            (Circuit::FlipFlop { .. }, Pulse::High) => None,
            (Circuit::FlipFlop { state }, Pulse::Low) => {
                state.toggle();
                Some(*state)
            }
            // essentially a NAND gate
            (Circuit::Conjunction { connections }, x) => {
                connections.insert(from.to_owned(), *x);

                // println!("{:?}", connections);

                for (_, pulse) in connections.iter() {
                    if pulse == &Pulse::Low {
                        return Some(Pulse::High);
                    }
                }

                Some(Pulse::Low)
            }
        }
    }
}

fn parse_circuit(s: &str) -> ParseResult<(String, Circuit)> {
    let (s, circuit) = alt((
        map(tuple((tag("%"), alpha0)), |(_, name): (&str, &str)| {
            (name.to_owned(), Circuit::new_flipflop())
        }),
        map(tuple((tag("&"), alpha0)), |(_, name): (&str, &str)| {
            (name.to_owned(), Circuit::new_conjunction())
        }),
        map(alpha0, |name: &str| {
            (name.to_owned(), Circuit::new_simple())
        }),
    ))(s)?;
    Ok((s, circuit))
}

// a, b, c
fn parse_names(s: &str) -> ParseResult<Vec<String>> {
    let (s, cons) = separated_list1(tag(", "), alpha0)(s)?;

    let cons = cons.into_iter().map(|s| s.to_owned()).collect();

    Ok((s, cons))
}

fn parse_connection(s: &str) -> ParseResult<(String, (Circuit, Vec<String>))> {
    let (s, ((name, circuit), _, output)) = tuple((parse_circuit, tag(" -> "), parse_names))(s)?;

    Ok((s, (name, (circuit, output))))
}

fn push_button(input: &mut HashMap<String, (Circuit, Vec<String>)>) -> (u64, u64) {
    let mut todo = VecDeque::from([("button".to_owned(), "broadcaster".to_owned(), Pulse::Low)]);

    let mut high_count = 0;
    let mut low_count = 0;

    while let Some((from, to, incoming_pulse)) = todo.pop_front() {
        match incoming_pulse {
            Pulse::High => high_count += 1,
            Pulse::Low => low_count += 1,
        };

        if let Some((circuit, outputs)) = input.get_mut(&to) {
            if let Some(pulse) = circuit.input(&from, &incoming_pulse) {
                for output in outputs.iter() {
                    todo.push_back((to.to_owned(), output.to_owned(), pulse));
                }
            }
        }
    }

    (high_count, low_count)
}

struct Day {}

impl AdventOfCode for Day {
    const DAY: usize = 20;

    type In = HashMap<String, (Circuit, Vec<String>)>;

    type Out = u64;

    fn parse(s: &str) -> ParseResult<Self::In> {
        let (s, mut cons) = parse_lines(parse_connection)(s)?;

        // add the button:
        cons.push((
            "button".to_owned(),
            (Circuit::new_simple(), vec!["broadcaster".to_owned()]),
        ));

        let mut graph: HashMap<_, _> = cons.iter().cloned().collect();

        // track the connections:
        for (name, (_, outputs)) in cons.iter() {
            for output in outputs.iter() {
                if let Some((circuit, _)) = graph.get_mut(output) {
                    circuit.add_connection(name.to_owned());
                }
            }
        }

        Ok((s, graph))
    }

    fn part1(input: &Self::In) -> Result<Self::Out, AOCError> {
        let mut input = input.clone();

        let mut high_count = 0;
        let mut low_count = 0;

        for _ in 0..1000 {
            let (high, low) = push_button(&mut input);

            high_count += high;
            low_count += low;
        }

        Ok(low_count * high_count)
    }

    fn part2(input: &Self::In) -> Result<Self::Out, AOCError> {
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
    fn test1_1() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part1, 1, 32000000)
    }

    #[test]
    fn test1_2() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part1, 2, 11687500)
    }

    #[test]
    fn test2() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part2, 1, 10)
    }
}
