use std::{
    fmt::{Debug, Display, Formatter},
    ops::{Add, Sub},
};

use super::position::Position;

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub struct Point2D {
    pub x: isize,
    pub y: isize,
}

impl Point2D {
    pub fn move_by(&self, dx: isize, dy: isize) -> Self {
        Point2D {
            x: self.x + dx,
            y: self.y + dy,
        }
    }
}

impl From<(isize, isize)> for Point2D {
    fn from((x, y): (isize, isize)) -> Self {
        Point2D { x, y }
    }
}

impl Display for Point2D {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.x, self.y)
    }
}

impl Debug for Point2D {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.x, self.y)
    }
}

impl Add for Point2D {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Point2D {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

impl Sub for Point2D {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Point2D {
            x: self.x - rhs.x,
            y: self.y - rhs.y,
        }
    }
}

impl Position for Point2D {
    fn to_index(&self, min: Self, max: Self) -> Option<usize> {
        if self.x < min.x || self.x > max.x || self.y < min.y || self.y > max.y {
            None
        } else {
            let width = max.x - min.x + 1;

            let row = self.y - min.y;
            let col = self.x - min.x;

            Some((row * width + col) as usize)
        }
    }

    fn from_index(index: usize, min: Self, max: Self) -> Option<Self> {
        let width = max.x - min.x + 1;

        let row = (index as isize) / width;
        let col = (index as isize) % width;

        if row > (max.y - min.y) || col > (max.x - min.x) {
            None
        } else {
            Some(Point2D {
                x: col as isize + min.x,
                y: row as isize + min.y,
            })
        }
    }

    fn neighbors(&self) -> Vec<Self> {
        let x = self.x;
        let y = self.y;
        vec![
            (x - 1, y).into(),
            (x + 1, y).into(),
            (x, y - 1).into(),
            (x, y + 1).into(),
        ]
    }

    fn origin() -> Self {
        Point2D { x: 0, y: 0 }
    }

    fn distance(&self, other: &Self) -> usize {
        let dx = (self.x - other.x).unsigned_abs();
        let dy = (self.y - other.y).unsigned_abs();

        dx + dy
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_to_index() {
        let min = Point2D { x: 0, y: 0 };
        let max = Point2D { x: 4, y: 4 };

        assert_eq!(Point2D { x: 0, y: 0 }.to_index(min, max), Some(0));
        assert_eq!(Point2D { x: 1, y: 0 }.to_index(min, max), Some(1));
        assert_eq!(Point2D { x: 2, y: 0 }.to_index(min, max), Some(2));
        assert_eq!(Point2D { x: 3, y: 0 }.to_index(min, max), Some(3));
        assert_eq!(Point2D { x: 4, y: 0 }.to_index(min, max), Some(4));
        assert_eq!(Point2D { x: 0, y: 1 }.to_index(min, max), Some(5));
        assert_eq!(Point2D { x: 1, y: 1 }.to_index(min, max), Some(6));
        assert_eq!(Point2D { x: 2, y: 1 }.to_index(min, max), Some(7));
        assert_eq!(Point2D { x: 3, y: 1 }.to_index(min, max), Some(8));
        assert_eq!(Point2D { x: 4, y: 1 }.to_index(min, max), Some(9));
        assert_eq!(Point2D { x: 0, y: 2 }.to_index(min, max), Some(10));
        assert_eq!(Point2D { x: 1, y: 2 }.to_index(min, max), Some(11));
        assert_eq!(Point2D { x: 2, y: 2 }.to_index(min, max), Some(12));
        assert_eq!(Point2D { x: 3, y: 2 }.to_index(min, max), Some(13));
        assert_eq!(Point2D { x: 4, y: 2 }.to_index(min, max), Some(14));
        assert_eq!(Point2D { x: 0, y: 3 }.to_index(min, max), Some(15));
        assert_eq!(Point2D { x: 1, y: 3 }.to_index(min, max), Some(16));
        assert_eq!(Point2D { x: 2, y: 3 }.to_index(min, max), Some(17));
        assert_eq!(Point2D { x: 3, y: 3 }.to_index(min, max), Some(18));
        assert_eq!(Point2D { x: 4, y: 3 }.to_index(min, max), Some(19));
        assert_eq!(Point2D { x: 0, y: 4 }.to_index(min, max), Some(20));
        assert_eq!(Point2D { x: 1, y: 4 }.to_index(min, max), Some(21));
        assert_eq!(Point2D { x: 2, y: 4 }.to_index(min, max), Some(22));
        assert_eq!(Point2D { x: 3, y: 4 }.to_index(min, max), Some(23));
        assert_eq!(Point2D { x: 4, y: 4 }.to_index(min, max), Some(24));
    }

    #[test]
    fn test_from_index() {
        // \ 0  1  2  3  4
        // 0 0  1  2  3  4
        // 1 5  6  7  8  9
        // 2 10 11 12 13 14
        // 3 15 16 17 18 19
        // 4 20 21 22 23 24
        let min = Point2D { x: 0, y: 0 };
        let max = Point2D { x: 4, y: 4 };

        assert_eq!(
            Point2D::from_index(7, min, max),
            Some(Point2D { x: 2, y: 1 })
        )
    }

    #[test]
    fn test_from_to_index() {
        let min = Point2D { x: 0, y: 0 };
        let max = Point2D { x: 4, y: 4 };

        for y in min.y..=max.y {
            for x in min.x..=max.x {
                let p = Point2D { x, y };
                let index = p.to_index(min, max).unwrap();
                let p2 = Point2D::from_index(index, min, max).unwrap();
                assert_eq!(p, p2);
            }
        }
    }
}
