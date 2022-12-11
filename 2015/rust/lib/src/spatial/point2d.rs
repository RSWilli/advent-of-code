use std::{
    fmt::{Debug, Display, Formatter},
    ops::{Add, Sub},
};

use super::position::Position;

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub struct Point2D {
    pub x: i32,
    pub y: i32,
}

impl Point2D {
    pub fn move_by(&self, dx: i32, dy: i32) -> Self {
        Point2D {
            x: self.x + dx,
            y: self.y + dy,
        }
    }
}

impl From<(i32, i32)> for Point2D {
    fn from((x, y): (i32, i32)) -> Self {
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
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_to_index() {
        let min = Point2D { x: 0, y: 0 };
        let max = Point2D { x: 4, y: 4 };

        assert_eq!(Point2D { x: 1, y: 1 }.to_index(min, max), Some(6))
    }
}
