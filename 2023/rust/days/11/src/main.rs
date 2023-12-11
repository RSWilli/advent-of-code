use lib::{parse::*, AOCError, AdventOfCode};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct Position {
    x: usize,
    y: usize,
}

impl Position {
    fn manhatten_dist(&self, to: &Position) -> usize {
        (self.x as isize - to.x as isize).unsigned_abs()
            + (self.y as isize - to.y as isize).unsigned_abs()
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Object {
    Galaxy,
    Empty,
}

#[derive(Debug)]
struct Space {
    galaxies: Vec<Position>,
    width: usize,
    height: usize,
}

impl Space {
    fn expanded_distances(&self, expand_by: usize) -> Vec<usize> {
        let mut emtpy_rows = vec![];

        for y in 0..self.height {
            if self.galaxies.iter().any(|g| g.y == y) {
                continue;
            } else {
                emtpy_rows.push(y);
            }
        }

        let mut emtpy_cols = vec![];

        for x in 0..self.width {
            if self.galaxies.iter().any(|g| g.x == x) {
                continue;
            } else {
                emtpy_cols.push(x);
            }
        }

        let mut distances = vec![];

        for g1 in self.galaxies.iter() {
            for g2 in self.galaxies.iter() {
                if g1 == g2 {
                    continue;
                }

                let dist = g1.manhatten_dist(g2);

                // count the "expanded" rows and cols
                let expanded_rows = emtpy_cols
                    .iter()
                    .filter(|&x| g1.x <= *x && *x <= g2.x || g2.x <= *x && *x <= g1.x)
                    .count();

                let expanded_cols = emtpy_rows
                    .iter()
                    .filter(|&y| g1.y <= *y && *y <= g2.y || g2.y <= *y && *y <= g1.y)
                    .count();

                distances.push(
                    dist + expand_by * expanded_rows - expanded_rows + expand_by * expanded_cols
                        - expanded_cols,
                );
            }
        }

        distances
    }
}

fn parse_row(s: &str) -> ParseResult<Vec<Object>> {
    let (s, row) = many1(one_of("#."))(s)?;

    let objects = row
        .iter()
        .map(|c| match c {
            '.' => Object::Empty,
            '#' => Object::Galaxy,
            _ => unreachable!(),
        })
        .collect();

    Ok((s, objects))
}
fn parse_space(s: &str) -> ParseResult<Space> {
    let (s, rows) = separated_list1(tag("\n"), parse_row)(s)?;

    let galaxies = rows.iter().enumerate().fold(vec![], |mut acc, (y, row)| {
        for (x, object) in row.iter().enumerate() {
            if *object == Object::Galaxy {
                acc.push(Position { x, y });
            }
        }
        acc
    });

    Ok((
        s,
        Space {
            galaxies,
            width: rows[0].len(),
            height: rows.len(),
        },
    ))
}

struct Day {}

impl AdventOfCode for Day {
    const DAY: usize = 11;

    type In = Space;

    type Out = usize;

    fn parse(s: &str) -> ParseResult<Self::In> {
        parse_all(parse_space)(s)
    }

    fn part1(input: &Self::In) -> Result<Self::Out, AOCError> {
        Ok(input.expanded_distances(2).iter().sum::<usize>() / 2)
    }
    fn part2(input: &Self::In) -> Result<Self::Out, AOCError> {
        Ok(input.expanded_distances(1_000_000).iter().sum::<usize>() / 2)
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
        lib::test(Day {}, lib::Part::Part1, 1, 374)
    }
}
