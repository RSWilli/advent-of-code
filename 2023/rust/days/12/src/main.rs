use std::{collections::HashMap, fmt::Display, usize};

use lib::{parse::*, AOCError, AdventOfCode};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Spring {
    Operational,
    Damaged,
    Unknown,
}

impl Display for Spring {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Spring::Operational => write!(f, "."),
            Spring::Damaged => write!(f, "#"),
            Spring::Unknown => write!(f, "?"),
        }
    }
}

fn parse_spring(s: &str) -> ParseResult<Spring> {
    let (s, c) = one_of(".#?")(s)?;

    let spring = match c {
        '.' => Spring::Operational,
        '?' => Spring::Unknown,
        '#' => Spring::Damaged,
        _ => unreachable!(),
    };

    Ok((s, spring))
}

// .??..??...?##. 1,1,3
fn parse_condition_record(s: &str) -> ParseResult<ConditionRecord> {
    let (s, (springs, _, damaged)) =
        tuple((many1(parse_spring), space1, separated_list1(tag(","), u64)))(s)?;

    Ok((
        s,
        ConditionRecord {
            springs,
            damaged_groups: damaged,
        },
    ))
}

#[derive(Debug, PartialEq, Eq)]
struct ConditionRecord {
    springs: Vec<Spring>,
    damaged_groups: Vec<u64>,
}

impl Display for ConditionRecord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let springs = self
            .springs
            .iter()
            .map(|s| s.to_string())
            .collect::<Vec<_>>()
            .join("");

        let damaged_springs = self
            .damaged_groups
            .iter()
            .map(|s| s.to_string())
            .collect::<Vec<_>>()
            .join(",");

        write!(f, "{} {}", springs, damaged_springs)
    }
}

type ConditionRecordView = (Vec<Spring>, Vec<u64>);

impl ConditionRecord {
    fn unfold(&self) -> Self {
        ConditionRecord {
            springs: (0..10)
                .flat_map(|i| {
                    if i % 2 == 0 {
                        self.springs.clone()
                    } else {
                        vec![Spring::Unknown]
                    }
                })
                .collect(),
            damaged_groups: (0..5).flat_map(|_| self.damaged_groups.clone()).collect(),
        }
    }

    fn count_defect_arrangements(&self) -> usize {
        let mut cache = HashMap::new();
        self.count_defect_arrangements_cached(&mut cache)
    }
    fn count_defect_arrangements_cached(
        &self,
        cache: &mut HashMap<ConditionRecordView, usize>,
    ) -> usize {
        let key = (self.springs.clone(), self.damaged_groups.clone());

        if let Some(&count) = cache.get(&key) {
            return count;
        }

        let res = match (self.springs.as_slice(), self.damaged_groups.as_slice()) {
            ([], []) => 1,
            ([], _) => 0,

            // early temination if we do not have enough springs to fill the groups
            (springs, damaged_groups)
                if ((damaged_groups.iter().cloned().sum::<u64>() as usize)
                    + damaged_groups.len())
                    > springs.len() + 1 =>
            {
                0
            }

            // "." and "?" without any more groups have to be operational
            ([Spring::Operational | Spring::Unknown, springs @ ..], []) => ConditionRecord {
                springs: springs.to_owned(),
                damaged_groups: vec![],
            }
            .count_defect_arrangements_cached(cache),

            // ". 1" cannot be
            ([Spring::Operational], [1]) => 0,

            // "# 1" or "? 1", which has to be a #
            ([_], [1]) => 1,

            // "## 1" is not possible
            ([Spring::Damaged, Spring::Damaged, ..], [1, ..]) => 0,

            // "#. 1" and "#? 1" means the group is done, so we can decrease the search space
            (
                [Spring::Damaged, Spring::Operational | Spring::Unknown, springs @ ..],
                [1, damaged_groups @ ..],
            ) => ConditionRecord {
                springs: springs.to_owned(),
                damaged_groups: damaged_groups.to_owned(),
            }
            .count_defect_arrangements_cached(cache),

            // ". any" decrease the search space
            ([Spring::Operational, springs @ ..], damaged_groups) => ConditionRecord {
                springs: springs.to_owned(),
                damaged_groups: damaged_groups.to_owned(),
            }
            .count_defect_arrangements_cached(cache),

            // "#" the current spring has to start now
            (springs @ [Spring::Damaged, ..], [x, damaged_groups @ ..]) => {
                if springs.len() < *x as usize {
                    return 0;
                }

                let has_spring = springs
                    .iter()
                    .take(*x as usize)
                    .all(|s| s == &Spring::Damaged || s == &Spring::Unknown);

                if !has_spring {
                    return 0;
                }

                if springs.len() == *x as usize {
                    1
                } else if springs.len() > *x as usize && springs[*x as usize] != Spring::Damaged {
                    // "#?? 2" needs to step to "? "
                    ConditionRecord {
                        springs: springs[(*x as usize) + 1..].to_owned(),
                        damaged_groups: damaged_groups.to_owned(),
                    }
                    .count_defect_arrangements_cached(cache)
                } else {
                    0
                }
            }

            (springs @ [Spring::Unknown, ..], damaged_groups) => {
                let mut springs_with_operational = Vec::from(springs);
                springs_with_operational[0] = Spring::Operational;

                let mut springs_with_damaged = Vec::from(springs);
                springs_with_damaged[0] = Spring::Damaged;

                let c1 = ConditionRecord {
                    springs: springs_with_damaged.to_owned(),
                    damaged_groups: damaged_groups.to_owned(),
                }
                .count_defect_arrangements_cached(cache);

                let c2 = ConditionRecord {
                    springs: springs_with_operational.to_owned(),
                    damaged_groups: damaged_groups.to_owned(),
                }
                .count_defect_arrangements_cached(cache);

                c1 + c2
            }

            _ => 0,
        };

        cache.insert(key, res);

        res
    }
}

struct Day {}

impl AdventOfCode for Day {
    const DAY: usize = 12;

    type In = Vec<ConditionRecord>;

    type Out = usize;

    fn parse(s: &str) -> ParseResult<Self::In> {
        parse_lines(parse_condition_record)(s)
    }

    fn part1(input: &Self::In) -> Result<Self::Out, AOCError> {
        Ok(input.iter().map(|cr| cr.count_defect_arrangements()).sum())
    }

    fn part2(input: &Self::In) -> Result<Self::Out, AOCError> {
        Ok(input
            .iter()
            .map(|cr| cr.unfold())
            .map(|cr| cr.count_defect_arrangements())
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
        lib::test(Day {}, lib::Part::Part1, 1, 21)
    }

    #[test]
    fn test2() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part2, 1, 525152)
    }

    #[test]
    fn test_cr_unfold() {
        let cr = parse_condition_record(".# 1").unwrap().1;
        let cr2 = parse_condition_record(".#?.#?.#?.#?.# 1,1,1,1,1")
            .unwrap()
            .1;

        assert_eq!(cr.unfold(), cr2)
    }

    #[test]
    fn test_cr1() {
        let cr = parse_condition_record("???? 1").unwrap().1;

        assert_eq!(cr.count_defect_arrangements(), 4)
    }

    #[test]
    fn test_cr2() {
        let cr = parse_condition_record("?###???????? 3,2,1").unwrap().1;

        assert_eq!(cr.count_defect_arrangements(), 10)
    }

    #[test]
    fn test_cr3() {
        let cr = parse_condition_record("????? 2,1").unwrap().1;

        assert_eq!(cr.count_defect_arrangements(), 3)
    }

    #[test]
    fn test_cr4() {
        let cr = parse_condition_record("#??? 2,1").unwrap().1;

        assert_eq!(cr.count_defect_arrangements(), 1)
    }

    #[test]
    fn test_cr5() {
        let cr = parse_condition_record("### 2").unwrap().1;

        assert_eq!(cr.count_defect_arrangements(), 0)
    }
}
