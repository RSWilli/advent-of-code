use lib::spatial::{dense::SpatialDense, point2d::Point2D};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(super) enum Tile {
    Wall,
    Empty,
}

pub(super) struct Cube {
    pub(super) top: SpatialDense<Point2D, Tile>,
    pub(super) bottom: SpatialDense<Point2D, Tile>,
    pub(super) north: SpatialDense<Point2D, Tile>,
    pub(super) south: SpatialDense<Point2D, Tile>,
    pub(super) east: SpatialDense<Point2D, Tile>,
    pub(super) west: SpatialDense<Point2D, Tile>,
}
