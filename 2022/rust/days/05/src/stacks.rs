use std::{collections::VecDeque, fmt::Debug, str::FromStr};

use lib::{
    spatial::{dense::Spatial, point2d::Point2D},
    AOCError,
};

pub(crate) struct Stacks {
    stacks: Vec<VecDeque<char>>,
}

impl Debug for Stacks {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let stacks = &self.stacks;

        for (i, stack) in stacks.into_iter().enumerate() {
            println!("{} ({}): {:?}", i, stack.len(), stack)
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
                    current_stack.push_back(char)
                }
            }

            stacks.push(current_stack)
        }

        Ok(Stacks { stacks })
    }
}
