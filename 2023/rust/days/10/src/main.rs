use lib::{parse::*, AOCError, AdventOfCode};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct Position {
    x: usize,
    y: usize,
}

impl Position {
    fn north(&self) -> Position {
        Position {
            x: self.x,
            y: self.y - 1,
        }
    }

    fn east(&self) -> Position {
        Position {
            x: self.x + 1,
            y: self.y,
        }
    }

    fn south(&self) -> Position {
        Position {
            x: self.x,
            y: self.y + 1,
        }
    }

    fn west(&self) -> Position {
        Position {
            x: self.x - 1,
            y: self.y,
        }
    }
}

#[derive(Debug)]
struct Network {
    pipes: Vec<Vec<Pipe>>,
    start: Position,
}

impl Network {
    fn lookup(&self, pos: &Position) -> Pipe {
        let p = self.pipes[pos.y][pos.x];

        if p == Pipe::Start {
            // figure out the direction of the pipe
            // by looking at the connections of the neighbors

            // the pipe is connected to the north if there is a pipe to the north, that is connected to the south
            let is_north = pos.y > 0 && self.pipes[pos.y - 1][pos.x].connected_to_south();

            // the pipe is connected to the south if there is a pipe to the south, that is connected to the north
            let is_south =
                pos.y < self.pipes.len() - 1 && self.pipes[pos.y + 1][pos.x].connected_to_north();

            // the pipe is connected to the east if there is a pipe to the east, that is connected to the west
            let is_east = pos.x < self.pipes[pos.y].len() - 1
                && self.pipes[pos.y][pos.x + 1].connected_to_west();

            // the pipe is connected to the west if there is a pipe to the west, that is connected to the east
            let is_west = pos.x > 0 && self.pipes[pos.y][pos.x - 1].connected_to_east();

            if is_north && is_south {
                Pipe::NS
            } else if is_east && is_west {
                Pipe::EW
            } else if is_north && is_east {
                Pipe::NE
            } else if is_north && is_west {
                Pipe::NW
            } else if is_south && is_west {
                Pipe::SW
            } else if is_south && is_east {
                Pipe::SE
            } else {
                unreachable!()
            }
        } else {
            p
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Pipe {
    NS, // North-South
    EW, // East-West
    NE, // North-East
    NW, // North-West
    SW, // South-West
    SE, // South-East
    None,
    Start,
}

impl Pipe {
    fn neighbours_at(&self, pos: &Position) -> (Position, Position) {
        match self {
            Pipe::NS => (pos.north(), pos.south()),
            Pipe::EW => (pos.east(), pos.west()),
            Pipe::NE => (pos.north(), pos.east()),
            Pipe::NW => (pos.north(), pos.west()),
            Pipe::SW => (pos.south(), pos.west()),
            Pipe::SE => (pos.south(), pos.east()),
            Pipe::None => unreachable!(),
            Pipe::Start => unreachable!(), // we can go nowhere from the start, since we don't know the direction of the pipe
        }
    }

    fn connected_to_north(&self) -> bool {
        self == &Pipe::NS || self == &Pipe::NE || self == &Pipe::NW
    }

    fn connected_to_south(&self) -> bool {
        self == &Pipe::NS || self == &Pipe::SE || self == &Pipe::SW
    }

    fn connected_to_east(&self) -> bool {
        self == &Pipe::EW || self == &Pipe::NE || self == &Pipe::SE
    }

    fn connected_to_west(&self) -> bool {
        self == &Pipe::EW || self == &Pipe::NW || self == &Pipe::SW
    }
}

fn parse_pipe(s: &str) -> ParseResult<Pipe> {
    let (s, c) = one_of("|-LJ7F.S")(s)?;

    let pipe = match c {
        '|' => Pipe::NS,
        '-' => Pipe::EW,
        'L' => Pipe::NE,
        'J' => Pipe::NW,
        '7' => Pipe::SW,
        'F' => Pipe::SE,
        '.' => Pipe::None,
        'S' => Pipe::Start,
        _ => unreachable!(),
    };

    Ok((s, pipe))
}

fn parse_row(s: &str) -> ParseResult<Vec<Pipe>> {
    many1(parse_pipe)(s)
}

fn parse_network(s: &str) -> ParseResult<Network> {
    let (s, rows) = separated_list1(tag("\n"), parse_row)(s)?;

    let start = rows.iter().enumerate().find_map(|(y, r)| {
        r.iter().enumerate().find_map(|(x, p)| {
            if p == &Pipe::Start {
                Some(Position { x, y })
            } else {
                None
            }
        })
    });

    Ok((
        s,
        Network {
            pipes: rows,
            start: start.unwrap(), // TODO: error handling
        },
    ))
}

struct Day {}

impl AdventOfCode for Day {
    const DAY: usize = 10;

    type In = Network;

    type Out = u64;

    fn parse(s: &str) -> ParseResult<Self::In> {
        parse_all(parse_network)(s)
    }

    fn part1(input: &Self::In) -> Result<Self::Out, AOCError> {
        let mut current = input.start;
        let mut visited = vec![vec![false; input.pipes[0].len()]; input.pipes.len()];

        let mut steps = 0;

        loop {
            visited[current.y][current.x] = true;
            steps += 1;

            let current_pipe = input.lookup(&current);

            let (n1, n2) = current_pipe.neighbours_at(&current);

            if !visited[n1.y][n1.x] {
                current = n1;
            } else if !visited[n2.y][n2.x] {
                current = n2;
            } else {
                break;
            }
        }

        Ok(steps / 2)
    }

    fn part2(input: &Self::In) -> Result<Self::Out, AOCError> {
        let mut current = input.start;
        let mut visited = vec![vec![false; input.pipes[0].len()]; input.pipes.len()];

        loop {
            visited[current.y][current.x] = true;

            let current_pipe = input.lookup(&current);

            let (n1, n2) = current_pipe.neighbours_at(&current);

            if !visited[n1.y][n1.x] {
                current = n1;
            } else if !visited[n2.y][n2.x] {
                current = n2;
            } else {
                break;
            }
        }

        let mut count_inside = 0;

        for (y, row) in visited.iter().enumerate() {
            let mut inside = false;
            for (x, &part_of_loop) in row.iter().enumerate() {
                let pos = Position { x, y };
                let pipe = input.lookup(&pos);

                if part_of_loop {
                    inside = !inside && pipe.connected_to_south()
                        || inside && !pipe.connected_to_south();
                }

                if !part_of_loop && inside {
                    count_inside += 1;
                }
            }
        }

        Ok(count_inside)
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
        lib::test(Day {}, lib::Part::Part1, 1, 4)
    }

    #[test]
    fn test2() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part1, 2, 8)
    }

    #[test]
    fn test3() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part2, 1, 1)
    }

    #[test]
    fn test4() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part2, 3, 4)
    }
}
