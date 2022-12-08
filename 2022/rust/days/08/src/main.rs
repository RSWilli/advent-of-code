use lib::{
    spatial::{dense::Spatial, point2d::Point2D},
    AOCError, AOCReader, AdventOfCode,
};

struct Day {}

fn visible(current: &usize, trees: &[&usize]) -> usize {
    trees.iter().take_while(|t| **t < current).count()
}

fn count_visible(current: &usize, trees: &[&usize]) -> usize {
    let shorter = visible(current, trees);

    if shorter < trees.len() {
        return shorter + 1;
    }

    shorter
}

impl AdventOfCode for Day {
    const DAY: usize = 8;

    type In = Spatial<Point2D, usize>;

    type Out = usize;

    fn parse(&self, inp: AOCReader) -> Result<Self::In, AOCError> {
        inp.parse_content()
    }

    fn part1(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        let width = input.width();
        let height = input.height();

        let border = width * 2 + (height - 2) * 2;

        let mut inner_visible = 0;

        for x in 1..width - 1 {
            for y in 1..height - 1 {
                let x = x as i32;
                let y = y as i32;

                let current_tree = input.get((x, y).into()).ok_or(AOCError::AOCError {
                    msg: "no tree found",
                })?;

                let col_top = input.slice((x, input.min.y).into(), (x, y - 1).into());
                let col_bottom = input.slice((x, y + 1).into(), (x, input.max.y).into());

                let row_left = input.slice((input.min.x, y).into(), (x - 1, y).into());
                let row_right = input.slice((x + 1, y).into(), (input.max.x, y).into());

                if visible(current_tree, &row_left) == row_left.len()
                    || visible(current_tree, &row_right) == row_right.len()
                    || visible(current_tree, &col_top) == col_top.len()
                    || visible(current_tree, &col_bottom) == col_bottom.len()
                {
                    inner_visible += 1;
                }
            }
        }

        Ok(border + inner_visible)
    }

    fn part2(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        let width = input.width();
        let height = input.height();

        let mut max_score = 0;

        for x in 1..width - 1 {
            for y in 1..height - 1 {
                let x = x as i32;
                let y = y as i32;

                let current_tree = input.get((x, y).into()).ok_or(AOCError::AOCError {
                    msg: "no tree found",
                })?;

                let col_top = input.slice((x, y - 1).into(), (x, input.min.y).into());
                let col_bottom = input.slice((x, y + 1).into(), (x, input.max.y).into());

                let row_left = input.slice((x - 1, y).into(), (input.min.x, y).into());
                let row_right = input.slice((x + 1, y).into(), (input.max.x, y).into());

                max_score = max_score.max(
                    count_visible(current_tree, &col_top)
                        * count_visible(current_tree, &col_bottom)
                        * count_visible(current_tree, &row_left)
                        * count_visible(current_tree, &row_right),
                );
            }
        }

        Ok(max_score)
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
        lib::test(Day {}, lib::Part::Part2, 1, 8)
    }
}
