use std::collections::HashMap;

use lib::{parse::*, AOCError, AdventOfCode};

struct Day {}

enum Operation {
    Set { name: String, value: usize },
    Remove { name: String },
}

const MINUS: usize = '-' as usize;
const EQUALS: usize = '=' as usize;
const ZERO: usize = '0' as usize;

impl Operation {
    fn hash_label(&self) -> usize {
        match self {
            Operation::Set { name, .. } | Operation::Remove { name } => name
                .as_bytes()
                .iter()
                .fold(0, |c, &v| hash_one(c, v as usize)),
        }
    }

    fn hash(&self) -> usize {
        let state = self.hash_label();
        match self {
            Operation::Set { value, .. } => {
                let state = hash_one(state, EQUALS);

                hash_one(state, *value + ZERO)
            }
            Operation::Remove { .. } => hash_one(state, MINUS),
        }
    }
}

// rn=1
fn parse_set(s: &str) -> ParseResult<Operation> {
    let (s, (name, _, value)) = tuple((alpha0, tag("="), u64))(s)?;

    Ok((
        s,
        Operation::Set {
            name: name.to_owned(),
            value: value as usize,
        },
    ))
}

// cm-
fn parse_remove(s: &str) -> ParseResult<Operation> {
    let (s, (name, _)) = tuple((alpha0, tag("-")))(s)?;

    Ok((
        s,
        Operation::Remove {
            name: name.to_owned(),
        },
    ))
}

fn parse_operation(s: &str) -> ParseResult<Operation> {
    alt((parse_set, parse_remove))(s)
}

fn hash_one(current_state: usize, value: usize) -> usize {
    ((current_state + value) * 17) % 256
}

struct Boxes {
    boxes: HashMap<usize, Vec<(String, usize)>>,
}

impl Boxes {
    fn perform_op(&mut self, op: &Operation) {
        let box_idx = op.hash_label();

        let lenses = self.boxes.entry(box_idx).or_default();

        match op {
            Operation::Set { name, value } => {
                for lens in lenses.iter_mut() {
                    if &lens.0 == name {
                        lens.1 = *value;
                        return;
                    }
                }

                // not found:
                lenses.push((name.to_owned(), *value));
            }
            Operation::Remove { name } => {
                if let Some((idx, _n)) = lenses.iter().enumerate().find(|(_idx, l)| &l.0 == name) {
                    lenses.remove(idx);
                }
            }
        }
    }

    fn calc_focusing_power(&self) -> usize {
        self.boxes
            .iter()
            .map(|(box_num, lenses)| {
                lenses
                    .iter()
                    .enumerate()
                    .map(|(idx, (_, value))| (box_num + 1) * (idx + 1) * value)
                    .sum::<usize>()
            })
            .sum()
    }
}

impl AdventOfCode for Day {
    const DAY: usize = 15;

    type In = Vec<Operation>;

    type Out = usize;

    fn parse(s: &str) -> ParseResult<Self::In> {
        let (s, (r, _, _)) =
            tuple((separated_list1(tag(","), parse_operation), tag("\n"), eof))(s)?;

        Ok((s, r))
    }

    fn part1(input: &Self::In) -> Result<Self::Out, AOCError> {
        Ok(input.iter().map(Operation::hash).sum())
    }

    fn part2(input: &Self::In) -> Result<Self::Out, AOCError> {
        let boxes = Boxes {
            boxes: HashMap::new(),
        };

        Ok(input
            .iter()
            .fold(boxes, |mut boxes, op| {
                boxes.perform_op(op);

                boxes
            })
            .calc_focusing_power())
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
        lib::test(Day {}, lib::Part::Part1, 1, 1320)
    }

    #[test]
    fn test2() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part2, 1, 145)
    }
}
