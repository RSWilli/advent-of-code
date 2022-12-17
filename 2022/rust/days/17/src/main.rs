use field::TetrisField;
use lib::{spatial::point2d::Point2D, AOCError, AOCReader, AdventOfCode};
use piece::Piece;

mod field;
mod piece;

#[derive(Clone, Copy, Debug)]
enum Direction {
    Left,
    Right,
}

impl TryFrom<char> for Direction {
    type Error = AOCError;
    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            '<' => Ok(Direction::Left),
            '>' => Ok(Direction::Right),
            _ => Err(AOCError::ParseErr()),
        }
    }
}

struct Day {}

impl AdventOfCode for Day {
    const DAY: usize = 17;

    type In = Vec<Direction>;

    type Out = i32;

    fn parse(&self, inp: AOCReader) -> Result<Self::In, AOCError> {
        inp.content()?.chars().map(|c| c.try_into()).collect()
    }

    fn part1(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        let pieces = Piece::piece_order();

        let mut moves = input.iter().cycle();

        let mut field = TetrisField::new();

        for piece in pieces.iter().cycle().take(2022) {
            let mut piece = *piece;
            piece.position = Point2D {
                x: 2,
                y: field.height + 3,
            };

            loop {
                // println!("piece: {:?}", piece);

                match moves.next() {
                    None => {
                        return Err(AOCError::AOCError {
                            msg: "ran out of moves",
                        })
                    }
                    Some(Direction::Left) => {
                        let moved = piece.move_left();

                        if field.possible_position(&moved) {
                            // println!("moved left");
                            piece = moved;
                        } else {
                            // println!("tried left");
                        }
                    }
                    Some(Direction::Right) => {
                        let moved = piece.move_right();

                        if field.possible_position(&moved) {
                            // println!("moved right");
                            piece = moved;
                        } else {
                            // println!("tried right");
                        }
                    }
                }

                let dropped = piece.drop();

                if !field.possible_position(&dropped) {
                    field.place(&piece);
                    // println!("set piece");
                    // println!("{:?}", field);
                    break;
                } else {
                    // println!("moved down");
                    piece = dropped;
                }
            }
        }

        Ok(field.height)
    }

    fn part2(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        unimplemented!()
    }
}

fn main() -> Result<(), AOCError> {
    lib::run(Day {})
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test1() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part1, 1, 3068)
    }
}
