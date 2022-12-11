use std::fmt::{Debug, Display};

use crate::ocr::{self};

use super::{point2d::Point2D, position::Position, spatial_trait::Spatial};

/**
 * this represents a section of a Spatial
 *
 * it borrows the underlying grid and can be used to get values from it
 * with a 0 position being the min position of the section
 */
pub struct Section<'a, P: Position, T, Grid: Spatial<P, Item = T>> {
    // the minimum index in the underlying grid
    min: P,
    // the maximum index in the underlying grid
    max: P,
    field: &'a Grid,
}

impl<'a, P: Position, T, Grid: Spatial<P, Item = T>> Section<'a, P, T, Grid> {
    pub(super) fn new(field: &'a Grid, min: P, max: P) -> Self {
        Section { min, max, field }
    }

    pub fn section(&self, min: P, max: P) -> Section<P, T, Self> {
        Section::new(self, min + self.min, max + self.min)
    }
}
impl<'a, P: Position, T, Grid: Spatial<P, Item = T>> Spatial<P> for Section<'a, P, T, Grid> {
    type Item = T;

    fn get(&self, pos: P) -> Option<&T> {
        let mapped_pos = pos + self.min;
        if mapped_pos.to_index(self.min, self.max).is_some() {
            self.field.get(mapped_pos)
        } else {
            None
        }
    }

    fn min(&self) -> P {
        P::origin()
    }

    fn max(&self) -> P {
        self.max - self.min
    }
}

impl<'a, T: Display, Grid: Spatial<Point2D, Item = T>> Display for Section<'a, Point2D, T, Grid> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for y in self.min.y..=self.max.y {
            for x in self.min.x..=self.max.x {
                let pos = Point2D { x, y };

                let out = self.field.get(pos).expect("index out of bounds");

                write!(f, "{}", out)?;
            }

            writeln!(f)?;
        }

        Ok(())
    }
}

impl<'a, T: Debug, Grid: Spatial<Point2D, Item = T>> Debug for Section<'a, Point2D, T, Grid> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Section min: {:?}, max: {:?}", self.min, self.max)?;
        for y in self.min.y..=self.max.y {
            for x in self.min.x..=self.max.x {
                let pos = Point2D { x, y };

                let out = self.field.get(pos).expect("index out of bounds");

                write!(f, "{:?}", out)?;
            }

            writeln!(f)?;
        }

        Ok(())
    }
}

impl<'a, Grid: Spatial<Point2D, Item = char>> Section<'a, Point2D, char, Grid> {
    pub fn read(&self) -> char {
        let s = self.to_string();

        if s == ocr::A {
            'A'
        } else if s == ocr::B {
            'B'
        } else if s == ocr::C {
            'C'
        } else if s == ocr::E {
            'E'
        } else if s == ocr::F {
            'F'
        } else if s == ocr::G {
            'G'
        } else if s == ocr::H {
            'H'
        } else if s == ocr::I {
            'I'
        } else if s == ocr::J {
            'J'
        } else if s == ocr::K {
            'K'
        } else if s == ocr::L {
            'L'
        } else if s == ocr::O {
            'O'
        } else if s == ocr::P {
            'P'
        } else if s == ocr::R {
            'R'
        } else if s == ocr::S {
            'S'
        } else if s == ocr::U {
            'U'
        } else if s == ocr::Y {
            'Y'
        } else if s == ocr::Z {
            'Z'
        } else {
            panic!("could not read char from section: \n{}\n ({:?})", self, s)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::spatial::{dense::SpatialDense, point2d::Point2D};

    use super::*;

    #[test]
    fn test_section() {
        let inp = "123\n456\n789";

        let mat: SpatialDense<_, _> = inp.parse().expect("could not parse");

        println!("{:?}", mat);

        let sec = mat.section(Point2D { x: 1, y: 1 }, Point2D { x: 1, y: 1 });

        println!("{:?}", sec);

        assert_eq!(sec.get(Point2D { x: 0, y: 0 }), Some(&'5'));

        let sec = mat.section(Point2D { x: 0, y: 0 }, Point2D { x: 2, y: 2 });

        println!("{:?}", sec);

        assert_eq!(sec.get(Point2D { x: 0, y: 0 }), Some(&'1'));
        assert_eq!(sec.get(Point2D { x: 1, y: 0 }), Some(&'2'));
        assert_eq!(sec.get(Point2D { x: 2, y: 0 }), Some(&'3'));
        assert_eq!(sec.get(Point2D { x: 0, y: 1 }), Some(&'4'));
    }

    #[test]
    fn test_subsection() {
        let inp = "123\n456\n789";

        let mat: SpatialDense<_, _> = inp.parse().expect("could not parse");

        println!("{:?}", mat);

        let sec = mat.section(Point2D { x: 0, y: 1 }, Point2D { x: 2, y: 1 });

        println!("{:?}", sec);

        let subsect = sec.section(Point2D { x: 1, y: 0 }, Point2D { x: 1, y: 0 });

        println!("{:?}", subsect);

        assert_eq!(subsect.get(Point2D { x: 0, y: 0 }), Some(&'5'));
    }
}
