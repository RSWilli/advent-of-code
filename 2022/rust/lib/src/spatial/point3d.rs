use std::fmt::{Display, Formatter};

use super::position::Position;

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub struct Point3D {
    pub x: i32,
    pub y: i32,
    pub z: i32,
}

impl Display for Point3D {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {}, {})", self.x, self.y, self.z)
    }
}

impl Position for Point3D {
    fn to_index(&self, min: Self, max: Self) -> Option<usize> {
        if self.x < min.x
            || self.x > max.x
            || self.y < min.y
            || self.y > max.y
            || self.z < min.z
            || self.z > max.z
        {
            None
        } else {
            let row = self.y - min.y;
            let col = self.x - min.x;
            let depth = self.z - min.z;

            Some((row * col * depth + col * depth + depth) as usize)
        }
    }
}
