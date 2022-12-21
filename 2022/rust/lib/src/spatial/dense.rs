use std::{
    fmt::{Debug, Display},
    ops::{Index, IndexMut},
    vec,
};

use crate::math::gcd;

use super::{
    point2d::Point2D,
    position::Position,
    spatial_trait::{Spatial, Spatial2D},
};

pub struct SpatialDense<P: Position, T> {
    // the minimum index in the grid
    min: P,
    // the maximum index in the grid
    max: P,
    field: Vec<T>,
}

impl<P: Position, T> SpatialDense<P, T> {
    pub fn with_content(min: P, max: P, field: Vec<T>) -> Self {
        if let Some(size) = max.to_index(min, max) {
            if field.len() != size + 1 {
                panic!("field size does not match the size of the grid");
            }

            SpatialDense { min, max, field }
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
}

// impl<P: Position, T> IntoIterator for SpatialDense<P, T> {
//     type Item = (P, T);
//     type IntoIter = std::iter::Scan<
//         std::iter::Enumerate<std::vec::IntoIter<T>>,
//         (P, P),
//         fn(&mut (P, P), (usize, T)) -> Option<(P, T)>,
//     >;

//     fn into_iter(self) -> Self::IntoIter {
//         let min = self.min;
//         let max = self.max;
//         self.field
//             .into_iter()
//             .enumerate()
//             .scan((min, max), |(min, max), (index, item)| {
//                 P::from_index(index, *min, *max).map(|pos| (pos, item))
//             })
//     }
// }

impl<T> SpatialDense<Point2D, T> {
    pub fn slice(&self, from: Point2D, to: Point2D) -> Vec<&T> {
        let mut dx = to.x - from.x;
        let mut dy = to.y - from.y;

        let m = gcd(dx as i64, dy as i64) as isize;

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

impl<P: Position, T> Spatial<P> for SpatialDense<P, T> {
    type Item = T;

    fn get(&self, index: P) -> Option<&Self::Item> {
        self.get(index)
    }

    fn min(&self) -> P {
        self.min
    }

    fn max(&self) -> P {
        self.max
    }

    fn find(&self, item: &Self::Item) -> Option<P>
    where
        Self::Item: PartialEq,
    {
        if let Some(index) = self.field.iter().position(|x| x == item) {
            P::from_index(index, self.min, self.max)
        } else {
            None
        }
    }
}

impl<T> Spatial2D for SpatialDense<Point2D, T> {
    fn row(&self, y: isize) -> Vec<&Self::Item> {
        let min_index = Point2D { y, x: self.min.x }
            .to_index(self.min, self.max)
            .unwrap();
        let max_index = Point2D { y, x: self.max.x }
            .to_index(self.min, self.max)
            .unwrap();

        self.field[min_index..=max_index].iter().collect()
    }

    fn col(&self, x: isize) -> Vec<&Self::Item> {
        let min_index = Point2D { x, y: self.min.y }
            .to_index(self.min, self.max)
            .unwrap();
        let max_index = Point2D { x, y: self.max.y }
            .to_index(self.min, self.max)
            .unwrap();

        self.field[min_index..=max_index]
            .iter()
            .step_by(self.width())
            .collect()
    }

    fn width(&self) -> usize {
        (self.max.x - self.min.x + 1) as usize
    }

    fn height(&self) -> usize {
        (self.max.y - self.min.y + 1) as usize
    }
}

impl<T: Debug> Debug for SpatialDense<Point2D, T> {
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

impl<T: Display> Display for SpatialDense<Point2D, T> {
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

impl SpatialDense<Point2D, char> {
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
                    x: x + width as isize - 1,
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
        let grid: SpatialDense<Point2D, char> = str.parse().expect("could not parse grid");

        println!("{}", grid);

        assert_eq!(grid.read(), "PZGPKPEB");
    }

    #[test]
    fn test_find() {
        let str = "123\n456\n789";
        let grid: SpatialDense<Point2D, char> = str.parse().expect("could not parse grid");

        assert_eq!(grid.find(&'1'), Some(Point2D { x: 0, y: 0 }));
        assert_eq!(grid.find(&'2'), Some(Point2D { x: 1, y: 0 }));
        assert_eq!(grid.find(&'3'), Some(Point2D { x: 2, y: 0 }));
        assert_eq!(grid.find(&'4'), Some(Point2D { x: 0, y: 1 }));
        assert_eq!(grid.find(&'5'), Some(Point2D { x: 1, y: 1 }));
        assert_eq!(grid.find(&'6'), Some(Point2D { x: 2, y: 1 }));
        assert_eq!(grid.find(&'7'), Some(Point2D { x: 0, y: 2 }));
        assert_eq!(grid.find(&'8'), Some(Point2D { x: 1, y: 2 }));
        assert_eq!(grid.find(&'9'), Some(Point2D { x: 2, y: 2 }));
        assert_eq!(grid.find(&'0'), None);
    }
}
