use std::ops::{Index, IndexMut};

use super::position::Position;

pub struct Spatial<P: Position, T> {
    min: P,
    max: P,
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
