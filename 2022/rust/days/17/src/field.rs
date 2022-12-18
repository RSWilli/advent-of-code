use std::{collections::HashSet, fmt::Debug};

use lib::spatial::point2d::Point2D;

use crate::piece::Piece;

pub(super) struct TetrisField {
    field: HashSet<Point2D>,
    pub(super) height: i32,
}

impl TetrisField {
    pub(super) fn new() -> Self {
        TetrisField {
            field: HashSet::new(),
            height: 0,
        }
    }
    pub(super) fn possible_position(&self, piece: &Piece) -> bool {
        piece
            .parts_iter()
            .all(|p| 0 <= p.x && p.x < 7 && 0 <= p.y && !self.field.contains(&p))
    }

    pub(super) fn place(&mut self, piece: &Piece) {
        for p in piece.parts_iter() {
            self.height = self.height.max(1 + p.y);
            self.field.insert(p);
        }
    }

    fn is_row_equal(&self, y1: i32, y2: i32) -> bool {
        (0..7).all(|x| {
            self.field.contains(&Point2D { x, y: y1 }) == self.field.contains(&Point2D { x, y: y2 })
        })
    }

    // return true if there is a cycle of length l
    // starting from the top
    fn has_cycle_of_length(&self, l: i32) -> bool {
        ((self.height - l)..self.height).all(|y| self.is_row_equal(y, y - l))
    }

    pub(super) fn find_cycle(&self) -> Option<i32> {
        (10..self.height).find(|&l| self.has_cycle_of_length(l))
    }
}

impl Debug for TetrisField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for y in (0..self.height).rev() {
            write!(f, "|")?;
            for x in 0..7 {
                if self.field.contains(&Point2D { x, y }) {
                    write!(f, "#")?;
                } else {
                    write!(f, " ")?;
                }
            }
            write!(f, "|")?;
            writeln!(f)?;
        }
        writeln!(f, "+-------+")?;
        Ok(())
    }
}
