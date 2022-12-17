use std::fmt::Debug;

use lib::spatial::point2d::Point2D;

#[derive(Clone, Copy)]
pub(super) struct Piece<'a> {
    pub(super) position: Point2D,
    parts: &'a [Point2D],
}

impl Debug for Piece<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let poses: Vec<_> = self.parts_iter().collect();

        write!(f, "{:?}", poses)
    }
}

// ####
const HORIZONTAL_BAR: [lib::spatial::point2d::Point2D; 4] = [
    Point2D { x: 0, y: 0 },
    Point2D { x: 1, y: 0 },
    Point2D { x: 2, y: 0 },
    Point2D { x: 3, y: 0 },
];

//  #
// ###
//  #
const CROSS: [Point2D; 5] = [
    Point2D { x: 0, y: 1 }, // left
    Point2D { x: 1, y: 2 }, // top
    Point2D { x: 1, y: 1 }, // middle
    Point2D { x: 1, y: 0 }, // bottom
    Point2D { x: 2, y: 1 }, // right
];

//   #
//   #
// ###
const L: [Point2D; 5] = [
    Point2D { x: 0, y: 0 },
    Point2D { x: 1, y: 0 },
    Point2D { x: 2, y: 0 },
    Point2D { x: 2, y: 1 },
    Point2D { x: 2, y: 2 },
];

// #
// #
// #
// #
const VERTICAL_BAR: [Point2D; 4] = [
    Point2D { x: 0, y: 0 },
    Point2D { x: 0, y: 1 },
    Point2D { x: 0, y: 2 },
    Point2D { x: 0, y: 3 },
];

// ##
// ##
const SQUARE: [Point2D; 4] = [
    Point2D { x: 0, y: 0 },
    Point2D { x: 0, y: 1 },
    Point2D { x: 1, y: 0 },
    Point2D { x: 1, y: 1 },
];

impl<'a> Piece<'a> {
    fn new_horizontal_bar() -> Self {
        Piece {
            position: Point2D { x: 0, y: 0 },
            parts: &HORIZONTAL_BAR,
        }
    }

    fn new_cross() -> Self {
        Piece {
            position: Point2D { x: 0, y: 0 },
            parts: &CROSS,
        }
    }

    fn new_l() -> Self {
        Piece {
            position: Point2D { x: 0, y: 0 },
            parts: &L,
        }
    }

    fn new_vertical_bar() -> Self {
        Piece {
            position: Point2D { x: 0, y: 0 },
            parts: &VERTICAL_BAR,
        }
    }

    fn new_square() -> Self {
        Piece {
            position: Point2D { x: 0, y: 0 },
            parts: &SQUARE,
        }
    }

    pub(super) fn piece_order() -> Vec<Piece<'a>> {
        vec![
            Piece::new_horizontal_bar(),
            Piece::new_cross(),
            Piece::new_l(),
            Piece::new_vertical_bar(),
            Piece::new_square(),
        ]
    }

    pub(super) fn parts_iter(&self) -> impl Iterator<Item = Point2D> + '_ {
        self.parts.iter().map(|p| self.position + *p)
    }

    pub(super) fn drop(&self) -> Self {
        Piece {
            position: self.position - Point2D { x: 0, y: 1 },
            parts: self.parts,
        }
    }
    pub(super) fn move_left(&self) -> Self {
        Piece {
            position: self.position - Point2D { x: 1, y: 0 },
            parts: self.parts,
        }
    }
    pub(super) fn move_right(&self) -> Self {
        Piece {
            position: self.position + Point2D { x: 1, y: 0 },
            parts: self.parts,
        }
    }
}
