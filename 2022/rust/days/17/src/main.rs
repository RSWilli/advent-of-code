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

// play i pieces and return the height of the field
fn play_for(input: &[Direction], amount: usize) -> Result<isize, AOCError> {
    let mut target_moves = amount;
    let pieces = Piece::piece_order();

    let mut moves = input.iter().cycle();

    let mut field = TetrisField::new();

    for (piece_nr, piece) in pieces.iter().cycle().enumerate() {
        let mut piece = *piece;
        piece.position = Point2D {
            x: 2,
            y: field.height + 3,
        };

        loop {
            match moves.next() {
                None => {
                    return Err(AOCError::AOCError {
                        msg: "ran out of moves",
                    })
                }
                Some(Direction::Left) => {
                    let moved = piece.move_left();

                    if field.possible_position(&moved) {
                        piece = moved;
                    }
                }
                Some(Direction::Right) => {
                    let moved = piece.move_right();

                    if field.possible_position(&moved) {
                        piece = moved;
                    }
                }
            }

            let dropped = piece.drop();

            if !field.possible_position(&dropped) {
                field.place(&piece);
                break;
            } else {
                piece = dropped;
            }
        }

        // optimize for large i
        // detect cycles
        if let Some(l) = field.find_cycle() {
            // the moves are periodic with period l, so we calculate the
            // number of moves that need to be made
        }
    }

    Ok(field.height)
}

struct Day {}

impl AdventOfCode for Day {
    const DAY: usize = 17;

    type In = Vec<Direction>;

    type Out = i64;

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
                match moves.next() {
                    None => {
                        return Err(AOCError::AOCError {
                            msg: "ran out of moves",
                        })
                    }
                    Some(Direction::Left) => {
                        let moved = piece.move_left();

                        if field.possible_position(&moved) {
                            piece = moved;
                        }
                    }
                    Some(Direction::Right) => {
                        let moved = piece.move_right();

                        if field.possible_position(&moved) {
                            piece = moved;
                        }
                    }
                }

                let dropped = piece.drop();

                if !field.possible_position(&dropped) {
                    field.place(&piece);
                    break;
                } else {
                    piece = dropped;
                }
            }
        }

        Ok(0)
    }

    fn part2(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        play_for(input, 1_000_000_000_000)?;
        Ok(0)
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

    #[test]
    fn test2() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part2, 1, 1_514_285_714_288)
    }
}
