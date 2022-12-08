use std::hash::Hash;

pub trait Position: PartialEq + Eq + Hash + Copy + Clone {
    fn to_index(&self, min: Self, max: Self) -> Option<usize>;

    fn neighbors(&self) -> Vec<Self>;
}
