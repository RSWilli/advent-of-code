use std::{collections::VecDeque, fmt::Debug, str::FromStr};

use lib::{
    spatial::{dense::Spatial, point2d::Point2D},
    AOCError,
};

use crate::operations::Operation;

#[derive(Clone)]
pub(crate) struct Stacks {
    stacks: Vec<VecDeque<char>>,
}

impl Stacks {
    /**
     * a single move of crates
     * amount is the amount of crates to pick up with one move operation
     */
    fn move_crates(&mut self, from: usize, to: usize, amount: usize) -> Result<(), AOCError> {
        let mut picked_crates = Vec::with_capacity(amount);

        for _ in 0..amount {
            let picked = self.stacks[from].pop_front().ok_or(AOCError::AOCError {
                msg: "could not pop front",
            })?;

            picked_crates.push(picked)
        }

        for picked in picked_crates.iter().rev() {
            self.stacks[to].push_front(*picked)
        }

        Ok(())
    }

    pub(crate) fn do_op_part1(
        &mut self,
        Operation { amount, from, to }: &Operation,
    ) -> Result<(), AOCError> {
        for _ in 0..*amount {
            self.move_crates(from - 1, to - 1, 1)?
        }

        Ok(())
    }

    pub(crate) fn do_op_part2(
        &mut self,
        Operation { amount, from, to }: &Operation,
    ) -> Result<(), AOCError> {
        self.move_crates(from - 1, to - 1, *amount)
    }

    pub(crate) fn read_top(&self) -> String {
        let mut res = "".to_owned();

        for stack in &self.stacks {
            if let Some(c) = stack.front() {
                res.push(*c);
            }
        }

        res
    }
}

impl Debug for Stacks {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let stacks = &self.stacks;

        for (i, stack) in stacks.into_iter().enumerate() {
            println!(
                "{} ({}): {:?}",
                i,
                stack.len(),
                stack.into_iter().rev().collect::<Vec<_>>()
            )
        }

        Ok(())
    }
}

impl FromStr for Stacks {
    type Err = AOCError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mat: Spatial<_, _> = s.parse().or(Err(AOCError::ParseErr()))?;

        let Point2D {
            x: width,
            y: height,
        } = mat.max;

        let mut stacks: Vec<VecDeque<char>> = Vec::new();

        for col in (1..=width).step_by(4) {
            let mut current_stack = VecDeque::new();

            for row in (0..height).rev() {
                let i = Point2D { x: col, y: row };

                let c = mat.get(i).ok_or(AOCError::ParseErr())?;

                let char = *c as char;

                if c != &' ' {
                    current_stack.push_front(char)
                }
            }

            stacks.push(current_stack)
        }

        Ok(Stacks { stacks })
    }
}
