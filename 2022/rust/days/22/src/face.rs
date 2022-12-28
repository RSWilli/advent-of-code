use crate::cube::Tile;

#[derive(Clone, Debug)]
pub(super) enum Face {
    Face(Vec<Vec<Tile>>),
    Empty,
}
