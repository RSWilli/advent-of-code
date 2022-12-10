use std::{
    fmt::{Debug, Display},
    ops::{Index, IndexMut},
    vec,
};

use crate::math::gcd;

use super::{get::Get, point2d::Point2D, position::Position, section::Section};

pub struct Spatial<P: Position, T> {
    // the minimum index in the grid
    pub min: P,
    // the maximum index in the grid
    pub max: P,
    pub(super) field: Vec<T>,
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

    pub fn with_content(min: P, max: P, field: Vec<T>) -> Self {
        if let Some(size) = max.to_index(min, max) {
            if field.len() != size + 1 {
                panic!("field size does not match the size of the grid");
            }

            Spatial { min, max, field }
        } else {
            panic!("index out of bounds");
        }
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

    pub fn section(&self, min: P, max: P) -> Section<P, T, Self> {
        Section::new(self, min, max)
    }
}

impl<T> Spatial<Point2D, T> {
    pub fn slice(&self, from: Point2D, to: Point2D) -> Vec<&T> {
        let mut dx = to.x - from.x;
        let mut dy = to.y - from.y;

        let m = gcd(dx as i64, dy as i64) as i32;

        if m != 0 {
            dx /= m;
            dy /= m;
        }

        let mut res = vec![];

        let mut currentpoint = from;

        loop {
            if let Some(index) = currentpoint.to_index(self.min, self.max) {
                res.push(&self.field[index])
            } else {
                panic!("could not slice because of index ot of bounds");
            }

            if currentpoint == to {
                break;
            }

            currentpoint = Point2D {
                x: currentpoint.x + dx,
                y: currentpoint.y + dy,
            };
        }

        res
    }

    pub fn width(&self) -> usize {
        (self.max.x - self.min.x + 1) as usize
    }

    pub fn height(&self) -> usize {
        (self.max.y - self.min.y + 1) as usize
    }
}

impl<P: Position, T> Get<P> for Spatial<P, T> {
    type Output = T;

    fn get(&self, index: P) -> Option<&Self::Output> {
        self.get(index)
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

impl<T: Debug> Debug for Spatial<Point2D, T> {
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
            for item in row {
                write!(f, "{:?}", item)?;
            }
            writeln!(f)?;
        }

        Ok(())
    }
}

impl<T: Display> Display for Spatial<Point2D, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let min = self.min;
        let max = self.max;

        let width = (max.x - min.x + 1) as usize;

        for row in self.field.chunks(width) {
            for item in row {
                write!(f, "{}", item)?;
            }
            writeln!(f)?;
        }

        Ok(())
    }
}

impl Spatial<Point2D, char> {
    /**
     * read splits the grid into 4x6 sections and reads the characters in each section
     *
     * e.g:
     *
     * ###  ####  ##  ###  #  # ###  #### ###  
     * #  #    # #  # #  # # #  #  # #    #  #
     * #  #   #  #    #  # ##   #  # ###  ###  
     * ###   #   # ## ###  # #  ###  #    #  #
     * #    #    #  # #    # #  #    #    #  #
     * #    ####  ### #    #  # #    #### ###
     *
     * becomes
     *  PZGPKPEB
     */
    pub fn read(&self) -> String {
        let mut res = String::new();

        // the charaters are 4x6 and spaced by 1
        let width = 4;

        let Point2D { x: minx, y: miny } = self.min;
        let Point2D { x: maxx, y: maxy } = self.max;

        if maxy - miny != 5 {
            panic!("invalid grid height");
        }

        for x in (minx..=maxx).step_by(width + 1) {
            let letter = self.section(
                Point2D { x, y: miny },
                Point2D {
                    x: x + width as i32 - 1,
                    y: maxy,
                },
            );

            res.push(letter.read());
        }

        res
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_read() {
        let str = "###  ####  ##  ###  #  # ###  #### ###  
#  #    # #  # #  # # #  #  # #    #  # 
#  #   #  #    #  # ##   #  # ###  ###  
###   #   # ## ###  # #  ###  #    #  # 
#    #    #  # #    # #  #    #    #  # 
#    ####  ### #    #  # #    #### ###  ";
        let grid: Spatial<Point2D, char> = str.parse().expect("could not parse grid");

        println!("{}", grid);

        assert_eq!(grid.read(), "PZGPKPEB");
    }
}
