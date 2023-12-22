use std::collections::{HashMap, HashSet};

use lib::{
    parse::*, three_dimensional::Position as Position3D, two_dimensional::Position as Position2D,
    AOCError, AdventOfCode,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Brick {
    from: Position3D,
    to: Position3D,
}

// 0,0,4~0,2,4
fn parse_brick(s: &str) -> ParseResult<Brick> {
    let (s, (x1, _, y1, _, z1, _, x2, _, y2, _, z2)) = tuple((
        u64,
        tag(","),
        u64,
        tag(","),
        u64,
        tag("~"),
        u64,
        tag(","),
        u64,
        tag(","),
        u64,
    ))(s)?;

    Ok((
        s,
        Brick {
            from: Position3D {
                x: x1 as isize,
                y: y1 as isize,
                z: z1 as isize,
            },
            to: Position3D {
                x: x2 as isize,
                y: y2 as isize,
                z: z2 as isize,
            },
        },
    ))
}

impl Brick {
    fn shadow(&self) -> Vec<Position2D> {
        let mut shadow = Vec::new();

        for x in self.from.x..=self.to.x {
            for y in self.from.y..=self.to.y {
                shadow.push(Position2D { x, y });
            }
        }

        shadow
    }

    fn height(&self) -> usize {
        (self.to.z - self.from.z + 1) as usize
    }
}

fn jenga(
    bricks: &[Brick],
) -> (
    HashMap<Brick, HashSet<Brick>>,
    HashMap<Brick, HashSet<Brick>>,
) {
    // position -> Brick+stack height
    let mut floor = HashMap::<Position2D, (Brick, usize)>::new();

    // track the bricks that are below the dropped brick
    let mut bricks_below = HashMap::<Brick, HashSet<Brick>>::new();

    // track the bricks that are above the dropped brick
    let mut bricks_above = HashMap::<Brick, HashSet<Brick>>::new();

    // we need to start with the lowest bricks
    let mut bricks = bricks.to_vec();
    bricks.sort_by_key(|b| b.from.z.min(b.to.z));

    for brick in bricks.iter() {
        let shadow = brick.shadow();

        // find the layer that the brick will settle on:
        let hit_layer = shadow
            .iter()
            .filter_map(|pos| floor.get(pos))
            .map(|(_, height)| height)
            .max()
            .copied()
            .unwrap_or(0);

        if hit_layer > 0 {
            for pos in shadow.iter() {
                if let Some((brick_below, layer)) = floor.get(pos) {
                    if layer == &hit_layer {
                        // the dropped brick is supported by this brick
                        bricks_below.entry(*brick).or_default().insert(*brick_below);

                        // this brick is supported by the dropped brick
                        bricks_above.entry(*brick_below).or_default().insert(*brick);
                    }
                }
            }
        }

        for pos in shadow.iter() {
            floor.insert(*pos, (*brick, hit_layer + brick.height()));
        }
    }

    (bricks_below, bricks_above)
}

struct Day {}

impl AdventOfCode for Day {
    const DAY: usize = 22;

    type In = Vec<Brick>;

    type Out = usize;

    fn parse(s: &str) -> ParseResult<Self::In> {
        parse_lines(parse_brick)(s)
    }

    fn part1(input: &Self::In) -> Result<Self::Out, AOCError> {
        let (bricks_below, bricks_above) = jenga(input);

        // important bricks are the ones that have a brick above them
        // which is not supported by any other brick
        let mut important_bricks = HashSet::<Brick>::new();

        for (b, bricks_above_b) in bricks_above.iter() {
            for brick_above_b in bricks_above_b.iter() {
                if bricks_below.get(brick_above_b).unwrap().len() == 1 {
                    important_bricks.insert(*b);
                }
            }
        }
        Ok(input.len() - important_bricks.len())
    }

    fn part2(input: &Self::In) -> Result<Self::Out, AOCError> {
        let (bricks_below, bricks_above) = jenga(input);

        let mut total_fallen = 0;

        // how many bricks will fall if we remove the given brick?
        for brick in input.iter() {
            let mut fallen = HashSet::<Brick>::new();
            fallen.insert(*brick);

            let mut could_fall = Vec::new();

            if let Some(bricks_above_brick) = bricks_above.get(&brick) {
                could_fall.extend(bricks_above_brick.iter().copied());
            }

            while let Some(brick) = could_fall.pop() {
                if fallen.contains(&brick) {
                    continue;
                }

                // check if any brick is still holding it
                if bricks_below
                    .get(&brick)
                    .unwrap()
                    .difference(&fallen)
                    .count()
                    == 0
                {
                    // this brick will fall
                    fallen.insert(brick);

                    // add the bricks above it to the list of bricks that could fall
                    if let Some(bricks_above_brick) = bricks_above.get(&brick) {
                        could_fall.extend(bricks_above_brick.iter().copied());
                    }
                }
            }

            // subtract 1 because the brick itself doesn't count
            total_fallen += fallen.len() - 1;
        }

        Ok(total_fallen)
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
        lib::test(Day {}, lib::Part::Part1, 1, 5)
    }

    #[test]
    fn test2() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part2, 1, 7)
    }
}
