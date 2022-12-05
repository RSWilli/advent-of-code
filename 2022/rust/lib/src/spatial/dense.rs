use std::{
    error::Error,
    fmt::Debug,
    ops::{Index, IndexMut},
    str::FromStr,
};

use super::{point2d::Point2D, position::Position};

pub struct Spatial<P: Position, T> {
    // the minimum index in the grid
    pub min: P,
    // the maximum index in the grid
    pub max: P,
    field: Vec<T>,
}

impl<P: Position, T> Spatial<P, T> {
    pub fn new(min: P, max: P) -> Self {
        if let Some(size) = max.to_index(min, max) {
            Spatial {
                min,
                max,
                field: Vec::with_capacity(size + 1),
            }
        } else {
            panic!("index out of bounds");
        }
    }

    fn with_content(min: P, max: P, field: Vec<T>) -> Self {
        Spatial { min, max, field }
    }

    pub fn get(&self, pos: P) -> Option<&T> {
        if let Some(index) = pos.to_index(self.min, self.max) {
            self.field.get(index)
        } else {
            None
        }
    }

    pub fn get_mut(&mut self, pos: P) -> Option<&mut T> {
        if let Some(index) = pos.to_index(self.min, self.max) {
            self.field.get_mut(index)
        } else {
            None
        }
    }
}

impl<P: Position, T> Index<P> for Spatial<P, T> {
    type Output = T;

    fn index(&self, pos: P) -> &Self::Output {
        if let Some(index) = pos.to_index(self.min, self.max) {
            &self.field[index]
        } else {
            panic!("index out of bounds");
        }
    }
}

impl<P: Position, T> IndexMut<P> for Spatial<P, T> {
    fn index_mut(&mut self, pos: P) -> &mut Self::Output {
        if let Some(index) = pos.to_index(self.min, self.max) {
            &mut self.field[index]
        } else {
            panic!("index out of bounds");
        }
    }
}

impl FromStr for Spatial<Point2D, char> {
    type Err = Box<dyn Error>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let lines: Vec<_> = s.lines().collect();

        let height = lines.len();

        let field: Vec<_> = lines
            .into_iter()
            .flat_map(|line| line.bytes().map(|b| b as char))
            .collect();

        let size = field.len();

        let width = size / height;

        let min = Point2D { x: 0, y: 0 };

        let max = Point2D {
            x: (width - 1) as i32,
            y: (height - 1) as i32,
        };

        Ok(Spatial::with_content(min, max, field))
    }
}

impl Debug for Spatial<Point2D, char> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let min = self.min;
        let max = self.max;

        let width = (max.x - min.x + 1) as usize;
        let height = (max.y - min.y + 1) as usize;

        writeln!(
            f,
            "Spatial min: {}, max: {}; dimensions: {}x{}",
            min, max, width, height
        )?;

        for row in self.field.chunks(width) {
            let row = String::from_iter(row.iter());
            writeln!(f, "{:?}", row)?
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_indices() {
        let inp = "123\n456\n789";

        let mat: Spatial<_, _> = inp.parse().expect("could not parse");

        println!("{:?}", mat);

        assert_eq!(mat.get(Point2D { x: 0, y: 0 }), Some(&'1'));
        assert_eq!(mat.get(Point2D { x: 1, y: 0 }), Some(&'2'));
        assert_eq!(mat.get(Point2D { x: 2, y: 0 }), Some(&'3'));
        assert_eq!(mat.get(Point2D { x: 3, y: 0 }), None);

        assert_eq!(mat.get(Point2D { x: 0, y: 1 }), Some(&'4'));
        assert_eq!(mat.get(Point2D { x: 1, y: 1 }), Some(&'5'));
        assert_eq!(mat.get(Point2D { x: 2, y: 1 }), Some(&'6'));
        assert_eq!(mat.get(Point2D { x: 3, y: 1 }), None);

        assert_eq!(mat.get(Point2D { x: 0, y: 2 }), Some(&'7'));
        assert_eq!(mat.get(Point2D { x: 1, y: 2 }), Some(&'8'));
        assert_eq!(mat.get(Point2D { x: 2, y: 2 }), Some(&'9'));
        assert_eq!(mat.get(Point2D { x: 3, y: 2 }), None);

        assert_eq!(mat.get(Point2D { x: 0, y: 3 }), None);
        assert_eq!(mat.get(Point2D { x: 1, y: 3 }), None);
        assert_eq!(mat.get(Point2D { x: 2, y: 3 }), None);
        assert_eq!(mat.get(Point2D { x: 3, y: 3 }), None);
    }
}
