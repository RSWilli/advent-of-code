use std::{error::Error, str::FromStr};

use super::{dense::SpatialDense, point2d::Point2D};

impl FromStr for SpatialDense<Point2D, char> {
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
            x: (width - 1) as isize,
            y: (height - 1) as isize,
        };

        Ok(SpatialDense::with_content(min, max, field))
    }
}

impl FromStr for SpatialDense<Point2D, usize> {
    type Err = Box<dyn Error>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let lines: Vec<_> = s.lines().collect();

        let height = lines.len();

        let field: Vec<_> = lines
            .into_iter()
            .flat_map(|line| line.bytes().map(|b| (b - b'0') as usize))
            .collect();

        let size = field.len();

        let width = size / height;

        let min = Point2D { x: 0, y: 0 };

        let max = Point2D {
            x: (width - 1) as isize,
            y: (height - 1) as isize,
        };

        Ok(SpatialDense::with_content(min, max, field))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_indices() {
        let inp = "123\n456\n789";

        let mat: SpatialDense<_, _> = inp.parse().expect("could not parse");

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
