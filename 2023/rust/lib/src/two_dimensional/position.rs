use super::Direction;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Position {
    pub x: isize,
    pub y: isize,
}

impl Position {
    pub fn new(x: isize, y: isize) -> Self {
        Self { x, y }
    }

    pub fn step(&self, dir: &Direction) -> Position {
        match dir {
            Direction::North => Position {
                x: self.x,
                y: self.y - 1,
            },
            Direction::East => Position {
                x: self.x + 1,
                y: self.y,
            },
            Direction::South => Position {
                x: self.x,
                y: self.y + 1,
            },
            Direction::West => Position {
                x: self.x - 1,
                y: self.y,
            },
            _ => *self,
        }
    }

    pub fn walk(&mut self, dir: &Direction, steps: usize) {
        match dir {
            Direction::North => self.y -= steps as isize,
            Direction::East => self.x += steps as isize,
            Direction::South => self.y += steps as isize,
            Direction::West => self.x -= steps as isize,
        }
    }

    pub fn neighbors(&self) -> Vec<Position> {
        vec![
            self.step(&Direction::North),
            self.step(&Direction::East),
            self.step(&Direction::South),
            self.step(&Direction::West),
        ]
    }

    pub fn manhattan_distance(&self, other: &Position) -> usize {
        (self.x - other.x).unsigned_abs() + (self.y - other.y).unsigned_abs()
    }
}
