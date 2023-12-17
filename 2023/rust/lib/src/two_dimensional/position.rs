use super::Direction;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Position {
    pub x: isize,
    pub y: isize,
}

impl Position {
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
