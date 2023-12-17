use crate::parse::*;
use nom::error::VerboseError;
mod direction;
mod position;
pub use direction::*;
pub use position::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Grid<T> {
    pub data: Vec<Vec<T>>,
    pub width: usize,
    pub height: usize,
}

impl<T> Grid<T> {
    pub fn lookup(&self, pos: &Position) -> Option<&T> {
        if pos.x < 0 || pos.y < 0 {
            return None;
        }

        let x = pos.x as usize;
        let y = pos.y as usize;

        if x >= self.width || y >= self.height {
            return None;
        }

        Some(&self.data[y][x])
    }

    pub fn lookup_mut(&mut self, pos: &Position) -> Option<&mut T> {
        if pos.x < 0 || pos.y < 0 {
            return None;
        }

        let x = pos.x as usize;
        let y = pos.y as usize;

        if x >= self.width || y >= self.height {
            return None;
        }

        Some(&mut self.data[y][x])
    }

    pub fn row_iter(&self) -> impl Iterator<Item = &T> {
        self.data.iter().flatten()
    }

    pub fn row_iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.data.iter_mut().flatten()
    }

    pub fn bottom_right(&self) -> Position {
        Position {
            x: self.width as isize - 1,
            y: self.height as isize - 1,
        }
    }

    // pub fn column_iter(&self) -> impl Iterator<Item = &T> {
    //     (0..self.width).flat_map(|x| self.data.iter().map(move |row| &row[x]))
    // }
}

impl<T> std::ops::Index<Position> for Grid<T> {
    type Output = T;

    fn index(&self, index: Position) -> &Self::Output {
        &self.data[index.y as usize][index.x as usize]
    }
}

pub fn parse_grid<'a, T, F>(mut parse_item: F) -> impl FnMut(&'a str) -> ParseResult<'a, Grid<T>>
where
    F: nom::Parser<&'a str, T, VerboseError<&'a str>>,
{
    move |input| {
        let (s, rows) = separated_list1(tag("\n"), many1(|s| parse_item.parse(s)))(input)?;

        Ok((
            s,
            Grid {
                width: rows[0].len(),
                height: rows.len(),
                data: rows,
            },
        ))
    }
}
