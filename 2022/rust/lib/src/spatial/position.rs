use std::{
    hash::Hash,
    ops::{Add, Sub},
};

pub trait Position:
    PartialEq + Eq + Hash + Copy + Clone + Add<Output = Self> + Sub<Output = Self>
{
    fn to_index(&self, min: Self, max: Self) -> Option<usize>;

    fn from_index(index: usize, min: Self, max: Self) -> Option<Self>;

    fn neighbors(&self) -> Vec<Self>;

    // fn neighbors_with_diagonals(&self) -> Vec<Self>;

    fn origin() -> Self;

    fn distance(&self, other: &Self) -> u32;
}
