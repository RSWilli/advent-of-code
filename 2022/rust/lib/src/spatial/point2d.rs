use std::fmt::{Display, Formatter};

use super::position::Position;

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub struct Point2D {
    pub x: i32,
    pub y: i32,
}

impl Display for Point2D {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.x, self.y)
    }
}

impl Position for Point2D {
    fn to_index(&self, min: Self, max: Self) -> Option<usize> {
        if self.x < min.x || self.x > max.x || self.y < min.y || self.y > max.y {
            None
        } else {
            let width = max.x + 1;

            let row = self.y - min.y;
            let col = self.x - min.x;

            Some((row * width + col) as usize)
        }
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
