use std::{
    fmt::{Display, Formatter},
    ops::{Add, Sub},
};

use super::position::Position;

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
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

impl Add for Point3D {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Point3D {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
            z: self.z + rhs.z,
        }
    }
}

impl Sub for Point3D {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Point3D {
            x: self.x - rhs.x,
            y: self.y - rhs.y,
            z: self.z - rhs.z,
        }
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

            let width = max.x - min.x + 1;
            let height = max.y - min.y + 1;

            Some((row * width * height + col * height + depth) as usize)
        }
    }

    fn from_index(index: usize, min: Self, max: Self) -> Option<Self> {
        let width = max.x - min.x + 1;
        let height = max.y - min.y + 1;

        let row = index / (width * height) as usize;
        let col = (index % (width * height) as usize) / height as usize;
        let depth = index % height as usize;

        if row > (max.y - min.y) as usize
            || col > (max.x - min.x) as usize
            || depth > (max.z - min.z) as usize
        {
            None
        } else {
            Some(Point3D {
                x: col as i32 + min.x,
                y: row as i32 + min.y,
                z: depth as i32 + min.z,
            })
        }
    }

    fn neighbors(&self) -> Vec<Self> {
        let x = self.x;
        let y = self.y;
        let z = self.z;

        vec![
            Point3D { x, y, z: z - 1 },
            Point3D { x, y, z: z + 1 },
            Point3D { x, y: y - 1, z },
            Point3D { x, y: y + 1, z },
            Point3D { x: x - 1, y, z },
            Point3D { x: x + 1, y, z },
        ]
    }

    fn origin() -> Self {
        Point3D { x: 0, y: 0, z: 0 }
    }
}

mod tests {
    use super::*;

    #[test]
    fn test_to_index() {
        let min = Point3D { x: 0, y: 0, z: 0 };
        let max = Point3D { x: 2, y: 2, z: 2 };

        assert_eq!(Point3D { x: 0, y: 0, z: 0 }.to_index(min, max), Some(0));
        assert_eq!(Point3D { x: 1, y: 0, z: 0 }.to_index(min, max), Some(1));
        assert_eq!(Point3D { x: 2, y: 0, z: 0 }.to_index(min, max), Some(2));
        assert_eq!(Point3D { x: 0, y: 1, z: 0 }.to_index(min, max), Some(3));
        assert_eq!(Point3D { x: 1, y: 1, z: 0 }.to_index(min, max), Some(4));
        assert_eq!(Point3D { x: 2, y: 1, z: 0 }.to_index(min, max), Some(5));
        assert_eq!(Point3D { x: 0, y: 2, z: 0 }.to_index(min, max), Some(6));
    }
}
