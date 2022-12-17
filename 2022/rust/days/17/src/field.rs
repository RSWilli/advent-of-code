use std::{collections::HashSet, fmt::Debug};

use lib::spatial::point2d::Point2D;

use crate::piece::Piece;

#[derive(Debug)]
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
}
