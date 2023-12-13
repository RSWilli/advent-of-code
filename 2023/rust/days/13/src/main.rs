use lib::{parse::*, AOCError, AdventOfCode};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Object {
    Rocks,
    Ash,
}

#[derive(Debug)]
struct Pattern {
    objects: Vec<Vec<Object>>,
    width: usize,
    height: usize,
}

fn parse_row(s: &str) -> ParseResult<Vec<Object>> {
    let (s, row) = many1(one_of("#."))(s)?;

    let objects = row
        .iter()
        .map(|c| match c {
            '.' => Object::Ash,
            '#' => Object::Rocks,
            _ => unreachable!(),
        })
        .collect();

    Ok((s, objects))
}
fn parse_pattern(s: &str) -> ParseResult<Pattern> {
    let (s, rows) = separated_list1(tag("\n"), parse_row)(s)?;

    Ok((
        s,
        Pattern {
            width: rows[0].len(),
            height: rows.len(),
            objects: rows,
        },
    ))
}

fn parse_patterns(s: &str) -> ParseResult<Vec<Pattern>> {
    separated_list0(tag("\n\n"), parse_pattern)(s)
}

impl Pattern {
    fn vertical_mirror_at(&self, index: usize, smudges: usize) -> bool {
        self.objects
            .iter()
            .map(|row| {
                let (l, r) = row.split_at(index);

                // cannot use ne because of different lengths
                l.iter().rev().zip(r.iter()).filter(|(l, r)| l != r).count()
            })
            .sum::<usize>()
            == smudges
    }

    fn horizontal_mirror_at(&self, index: usize, smudges: usize) -> bool {
        let (tops, bottoms) = self.objects.split_at(index);

        tops.iter()
            .rev()
            .zip(bottoms.iter())
            .map(|(t, b)| t.iter().zip(b.iter()).filter(|(t, b)| t != b).count())
            .sum::<usize>()
            == smudges
    }

    fn find_mirror_index(&self, smudges: usize) -> usize {
        for x in 1..self.width {
            if self.vertical_mirror_at(x, smudges) {
                return x;
            }
        }

        for y in 1..self.height {
            if self.horizontal_mirror_at(y, smudges) {
                return y * 100;
            }
        }

        0
    }
}

struct Day {}

impl AdventOfCode for Day {
    const DAY: usize = 13;

    type In = Vec<Pattern>;

    type Out = usize;

    fn parse(s: &str) -> ParseResult<Self::In> {
        parse_all(parse_patterns)(s)
    }

    fn part1(input: &Self::In) -> Result<Self::Out, AOCError> {
        Ok(input
            .iter()
            .map(|pattern| pattern.find_mirror_index(0))
            .sum())
    }

    fn part2(input: &Self::In) -> Result<Self::Out, AOCError> {
        Ok(input
            .iter()
            .map(|pattern| pattern.find_mirror_index(1))
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
        lib::test(Day {}, lib::Part::Part1, 1, 405)
    }

    #[test]
    fn test2() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part2, 1, 400)
    }
}
