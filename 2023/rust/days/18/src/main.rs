use lib::{
    parse::*,
    two_dimensional::{polygon_area, Direction, Position},
    AOCError, AdventOfCode,
};

#[derive(Debug)]
struct DigInstruction {
    direction: Direction,
    length: u64,
    hex_instruction: (Direction, u64),
}

fn parse_hex_instruction(s: &str) -> ParseResult<(Direction, u64)> {
    let hex = one_of("abcdef0123456789");

    let (s, (_, length, dir)) = tuple((tag("#"), many_m_n(5, 5, hex), one_of("0123")))(s)?;

    let length = length
        .iter()
        .filter_map(|c| c.to_digit(16))
        .fold(0, |acc, d| acc * 16 + d);

    let dir = match dir {
        '0' => Direction::East,
        '1' => Direction::South,
        '2' => Direction::West,
        '3' => Direction::North,
        _ => unreachable!(),
    };

    Ok((s, (dir, length as u64)))
}

// R 6 (#70c710)
fn parse_dig_instruction(s: &str) -> ParseResult<DigInstruction> {
    let (s, (direction, _, length, _, color)) = tuple((
        one_of("UDLR"),
        space1,
        u64,
        space1,
        delimited(tag("("), parse_hex_instruction, tag(")")),
    ))(s)?;

    let direction = match direction {
        'U' => Direction::North,
        'D' => Direction::South,
        'L' => Direction::West,
        'R' => Direction::East,
        _ => unreachable!(),
    };

    Ok((
        s,
        DigInstruction {
            direction,
            length,
            hex_instruction: color,
        },
    ))
}

struct Day {}

impl AdventOfCode for Day {
    const DAY: usize = 18;

    type In = Vec<DigInstruction>;

    type Out = u64;

    fn parse(s: &str) -> ParseResult<Self::In> {
        parse_lines(parse_dig_instruction)(s)
    }

    fn part1(input: &Self::In) -> Result<Self::Out, AOCError> {
        let points = input
            .iter()
            .scan(Position { x: 0, y: 0 }, |pos, instr| {
                pos.walk(&instr.direction, instr.length as usize);

                Some(*pos)
            })
            .collect::<Vec<_>>();

        // shoelace is missing the border, so we add it here (border is half the perimeter)
        let border: f64 = input.iter().map(|instr| instr.length as f64).sum();

        Ok((polygon_area(&points) + border / 2.0 + 1.0) as u64)
    }

    fn part2(input: &Self::In) -> Result<Self::Out, AOCError> {
        let points = input
            .iter()
            .scan(Position { x: 0, y: 0 }, |pos, instr| {
                pos.walk(&instr.hex_instruction.0, instr.hex_instruction.1 as usize);

                Some(*pos)
            })
            .collect::<Vec<_>>();

        let border: f64 = input
            .iter()
            .map(|instr| instr.hex_instruction.1 as f64)
            .sum();

        Ok((polygon_area(&points) + border / 2.0 + 1.0) as u64)
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
        lib::test(Day {}, lib::Part::Part1, 1, 62)
    }

    #[test]
    fn test2() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part2, 1, 952408144115)
    }
}
