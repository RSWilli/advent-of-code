use std::{collections::HashMap, str::FromStr};

use lib::{AOCError, AOCReader, AdventOfCode};
use nom::{
    character::complete::{self, space1},
    error::VerboseError,
    sequence::tuple,
    Finish, IResult,
};

struct Day {}

type ParseResult<'a, U> = IResult<&'a str, U, VerboseError<&'a str>>;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
enum Card {
    // part2 only, but the weakest card:
    Joker,

    C2,
    C3,
    C4,
    C5,
    C6,
    C7,
    C8,
    C9,
    C10,
    CJ,
    CQ,
    CK,
    CA,
}

#[derive(Debug, Clone, Copy)]
struct Hand {
    first: Card,
    second: Card,
    third: Card,
    fourth: Card,
    fifth: Card,

    value: usize,
}

impl Hand {
    fn count_cards(&self) -> [usize; 14] {
        let mut counts = [0; 14];

        counts[self.first as usize] += 1;
        counts[self.second as usize] += 1;
        counts[self.third as usize] += 1;
        counts[self.fourth as usize] += 1;
        counts[self.fifth as usize] += 1;

        counts
    }

    fn cards(&self) -> [Card; 5] {
        [self.first, self.second, self.third, self.fourth, self.fifth]
    }

    // part2 only, map the jacks to jokers:
    fn jacks_to_jokers(&self) -> Hand {
        Hand {
            value: self.value,

            first: if self.first == Card::CJ {
                Card::Joker
            } else {
                self.first
            },
            second: if self.second == Card::CJ {
                Card::Joker
            } else {
                self.second
            },
            third: if self.third == Card::CJ {
                Card::Joker
            } else {
                self.third
            },
            fourth: if self.fourth == Card::CJ {
                Card::Joker
            } else {
                self.fourth
            },
            fifth: if self.fifth == Card::CJ {
                Card::Joker
            } else {
                self.fifth
            },
        }
    }
}

impl PartialEq for Hand {
    fn eq(&self, other: &Self) -> bool {
        self.first == other.first
            && self.second == other.second
            && self.third == other.third
            && self.fourth == other.fourth
            && self.fifth == other.fifth
    }
}

impl Eq for Hand {}

impl PartialOrd for Hand {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Hand {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // first, count the cards:
        let mut my_counts = self.count_cards();
        let mut other_counts = other.count_cards();

        let my_jokers = my_counts[Card::Joker as usize];
        let other_jokers = other_counts[Card::Joker as usize];

        // add the jokers to the max of the other cards, make sure not to count the jokers twice:
        let my_max_index = my_counts[1..]
            .iter()
            .enumerate()
            .max_by_key(|p| p.1)
            .unwrap()
            .0;

        let other_max_index = other_counts[1..]
            .iter()
            .enumerate()
            .max_by_key(|p| p.1)
            .unwrap()
            .0;

        // add the jokers to the max of the other cards, add 1 since we skipped the first element:
        my_counts[my_max_index + 1] += my_jokers;
        other_counts[other_max_index + 1] += other_jokers;

        // remove the jokers for counting:
        my_counts[Card::Joker as usize] = 0;
        other_counts[Card::Joker as usize] = 0;

        let my_five = my_counts.iter().filter(|x| **x == 5).count();
        let other_five = other_counts.iter().filter(|x| **x == 5).count();

        // five of a kind
        match my_five.cmp(&other_five) {
            std::cmp::Ordering::Equal => (),
            x => return x,
        }

        let my_four = my_counts.iter().filter(|x| **x == 4).count();
        let other_four = other_counts.iter().filter(|x| **x == 4).count();

        // four of a kind
        match my_four.cmp(&other_four) {
            std::cmp::Ordering::Equal => (),
            x => return x,
        }

        let my_three = my_counts.iter().filter(|x| **x == 3).count();
        let other_three = other_counts.iter().filter(|x| **x == 3).count();
        let my_pairs = my_counts.iter().filter(|x| **x == 2).count();
        let other_pairs = other_counts.iter().filter(|x| **x == 2).count();

        match (my_three, my_pairs, other_three, other_pairs) {
            // both full house:
            (1, 1, 1, 1) => { /* tie, break it later */ }

            // my full house, i win:
            (1, 1, x, y) if x != 1 || y != 1 => return std::cmp::Ordering::Greater,

            // their full house, i lose:
            (x, y, 1, 1) if x != 1 || y != 1 => return std::cmp::Ordering::Less,

            // both three of a kind:
            (1, _, 1, _) => { /* tie, break it later */ }

            // my three of a kind, i win:
            (1, 0, 0, _) => return std::cmp::Ordering::Greater,

            // their three of a kind, i lose:
            (0, _, 1, 0) => return std::cmp::Ordering::Less,

            // both two pairs:
            (0, 2, 0, 2) => { /* tie, break it later */ }

            // my two pairs, i win:
            (0, 2, 0, _) => return std::cmp::Ordering::Greater,

            // their two pairs, i lose:
            (0, _, 0, 2) => return std::cmp::Ordering::Less,

            // both one pair:
            (0, 1, 0, 1) => { /* tie, break it later */ }

            // my one pair, i win:
            (0, 1, 0, 0) => return std::cmp::Ordering::Greater,

            // their one pair, i lose:
            (0, 0, 0, 1) => return std::cmp::Ordering::Less,

            // both high card:
            (0, 0, 0, 0) => { /* tie, break it later */ }

            _ => unreachable!(),
        }

        // tie, first highest card counts:
        for (my, other) in self.cards().iter().zip(other.cards().iter()) {
            match my.cmp(other) {
                std::cmp::Ordering::Equal => continue,
                x => return x,
            }
        }

        std::cmp::Ordering::Equal
    }
}

fn parse_card(s: &str) -> ParseResult<Card> {
    let (s, c) = complete::one_of("23456789TJQKA")(s)?;

    Ok((
        s,
        match c {
            '2' => Card::C2,
            '3' => Card::C3,
            '4' => Card::C4,
            '5' => Card::C5,
            '6' => Card::C6,
            '7' => Card::C7,
            '8' => Card::C8,
            '9' => Card::C9,
            'T' => Card::C10,
            'J' => Card::CJ,
            'Q' => Card::CQ,
            'K' => Card::CK,
            'A' => Card::CA,
            _ => unreachable!(),
        },
    ))
}

// T55J5 684
fn parse_hand(s: &str) -> ParseResult<Hand> {
    let (s, (first, second, third, fourth, fifth, _, value)) = tuple((
        parse_card,
        parse_card,
        parse_card,
        parse_card,
        parse_card,
        space1,
        complete::u64,
    ))(s)?;

    Ok((
        s,
        Hand {
            first,
            second,
            third,
            fourth,
            fifth,
            value: value as usize,
        },
    ))
}

impl FromStr for Hand {
    type Err = AOCError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_hand(s).finish() {
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
    const DAY: usize = 7;

    type In = Vec<Hand>;

    type Out = usize;

    fn parse(&self, inp: AOCReader) -> Result<Self::In, AOCError> {
        inp.parse_lines().collect()
    }

    fn part1(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        let mut input = input.clone();

        input.sort();

        Ok(input
            .iter()
            .enumerate()
            .map(|(i, card)| (i + 1) * card.value)
            .sum())
    }

    fn part2(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        let mut input = input
            .iter()
            .map(|x| x.jacks_to_jokers())
            .collect::<Vec<_>>();

        input.sort();

        Ok(input
            .iter()
            .enumerate()
            .map(|(i, card)| (i + 1) * card.value)
            .sum())
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
        lib::test(Day {}, lib::Part::Part1, 1, 6440)
    }

    #[test]
    fn test2() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part2, 1, 5905)
    }
}
