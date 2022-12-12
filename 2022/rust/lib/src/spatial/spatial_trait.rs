use super::{point2d::Point2D, position::Position, section::Section};

pub trait Spatial<P>
where
    Self: Sized,
    P: Position,
{
    type Item;

    fn get(&self, index: P) -> Option<&Self::Item>;

    fn min(&self) -> P;

    fn max(&self) -> P;

    fn find(&self, item: &Self::Item) -> Option<P>
    where
        Self::Item: PartialEq;

    fn section(&self, min: P, max: P) -> Section<P, Self::Item, Self> {
        Section::new(self, min, max)
    }
}

pub trait Spatial2D: Spatial<Point2D> {
    fn row(&self, y: i32) -> Vec<&Self::Item>;
    fn col(&self, x: i32) -> Vec<&Self::Item>;

    fn width(&self) -> usize;

    fn height(&self) -> usize;
}
