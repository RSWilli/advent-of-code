use std::collections::{HashMap, HashSet};

use lib::{
    parse::*,
    two_dimensional::{parse_grid, Direction, Grid, Position},
    AOCError, AdventOfCode,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Tile {
    Path,
    Forest,
    Slope(Direction),
}

fn parse_tile(s: &str) -> ParseResult<Tile> {
    let (s, c) = one_of(".#<>v^")(s)?;

    Ok((
        s,
        match c {
            '.' => Tile::Path,
            '#' => Tile::Forest,
            '<' => Tile::Slope(Direction::West),
            '>' => Tile::Slope(Direction::East),
            'v' => Tile::Slope(Direction::South),
            '^' => Tile::Slope(Direction::North),
            _ => unreachable!(),
        },
    ))
}

fn neighbors<'a>(grid: &'a Grid<Tile>, pos: &Position) -> impl Iterator<Item = Position> + 'a {
    match grid[pos] {
        Tile::Path => pos.neighbors(),
        Tile::Slope(dir) => vec![pos.step(&dir)],
        Tile::Forest => unreachable!(),
    }
    .into_iter()
    .filter(|p| {
        if let Some(tile) = grid.lookup(p) {
            match tile {
                Tile::Path | Tile::Slope(_) => true,
                Tile::Forest => false,
            }
        } else {
            false
        }
    })
}

fn neighbors_p2<'a>(grid: &'a Grid<Tile>, pos: &Position) -> impl Iterator<Item = Position> + 'a {
    pos.neighbors().into_iter().filter(|p| {
        if let Some(tile) = grid.lookup(p) {
            match tile {
                Tile::Path | Tile::Slope(_) => true,
                Tile::Forest => false,
            }
        } else {
            false
        }
    })
}

fn find_longest_path<'a>(
    from: &Position,
    grid: &'a Grid<Tile>,
    seen: &'a mut HashSet<Position>,
) -> usize {
    seen.insert(*from);

    let mut longest_path = 0;

    for neighbor in neighbors(grid, from) {
        if !seen.contains(&neighbor) {
            let path = 1 + find_longest_path(&neighbor, grid, seen);

            longest_path = longest_path.max(path);
        }
    }

    seen.remove(from);

    longest_path
}

fn minimize_labyrinth(
    grid: &Grid<Tile>,
    current: Position,
    last_crossing: &Position,
    steps_since: usize,
    graph: &mut HashMap<Position, Position>,
    seen: &mut HashSet<Position>,
) {
    let next: Vec<_> = neighbors_p2(grid, &current)
        .filter(|p| !seen.contains(p))
        .collect();

    if next.len() == 1 {
        // continue on the path
        let next = next[0];

        seen.insert(next);

        minimize_labyrinth(grid, next, last_crossing, steps_since + 1, graph, seen)
    } else {
        // we have a crossing or the end
        let next = next[0];
        graph.insert(*last_crossing, current);
        graph.insert(current, *last_crossing);

        seen.insert(next);

        minimize_labyrinth(grid, next, &current, steps_since + 1, graph, seen);

        let next = next[1];

        minimize_labyrinth(grid, next, &current, steps_since + 1, graph, seen);
    }
}

struct Day {}

impl AdventOfCode for Day {
    const DAY: usize = 23;

    type In = Grid<Tile>;

    type Out = usize;

    fn parse(s: &str) -> ParseResult<Self::In> {
        parse_all(parse_grid(parse_tile))(s)
    }

    fn part1(input: &Self::In) -> Result<Self::Out, AOCError> {
        let start = Position::new(1, 0);

        Ok(find_longest_path(&start, input, &mut HashSet::new()))
    }

    fn part2(input: &Self::In) -> Result<Self::Out, AOCError> {
        let start = Position::new(1, 0);

        Ok(find_longest_path_p2(&start, input, &mut HashSet::new()) - 1)
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
        lib::test(Day {}, lib::Part::Part1, 1, 94)
    }

    #[test]
    fn test2() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part2, 1, 154)
    }
}
