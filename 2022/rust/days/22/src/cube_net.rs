use nom::{
    branch,
    character::complete,
    multi::{count, many0_count, many1},
    sequence::{terminated, tuple},
    Parser,
};

use crate::{
    cube::Tile,
    face::Face,
    parse::{ParseError, ParseResult},
};

pub(super) fn parse_cube_net(s: &str) -> ParseResult<Vec<Vec<Face>>> {
    let mut input = s;

    let mut rows = Vec::new();
    let mut face_count = 0;

    loop {
        let (s, faces) = parse_row(input)?;

        faces.iter().for_each(|face| match face {
            Face::Face(_) => face_count += 1,
            Face::Empty => (),
        });

        if face_count > 6 {
            return Err(nom::Err::Failure(ParseError {
                message: format!("Too many faces: {}", face_count),
            }));
        }

        rows.push(faces);

        if face_count == 6 {
            return Ok((s, rows));
        }

        input = s;
    }
}

fn parse_row(s: &str) -> ParseResult<Vec<Face>> {
    branch::alt((
        parse_row_1,
        parse_row_2,
        parse_row_3,
        parse_row_4,
        parse_row_5,
    ))(s)
}

// function to parse a row of the cube with only one face
// on 4x4 dimensions, this could look like:
// ...#
// .#..
// #...
// ....
fn parse_row_1(s: &str) -> ParseResult<Vec<Face>> {
    // parse the first row to get the dimensions
    let (s, tiles) = terminated(parse_tiles, complete::char('\n'))(s).map_err(|_| {
        nom::Err::Error(ParseError {
            message: "Row is padded, cannot be 1 face".into(),
        })
    })?;

    let dimensions = tiles.len();

    // parse the remaining rows of the face
    let (s, mut rows) =
        parse_remaining_rows(parse_tiles_len(dimensions), dimensions)(s).map_err(|_| {
            nom::Err::Error(ParseError {
                message: format!("Invalid size for 1 face. tiles: {}", tiles.len()),
            })
        })?;

    rows.insert(0, tiles);

    Ok((s, vec![Face::Face(rows)]))
}

// function to parse a row of the cube with two faces (may start with empty faces)
// on 4x4 dimensions, this could look like:
// ...#...#
// .#...#..
// #...#...
// ........
// or
//     ...#
//     .#..
//     #...
//     ....
fn parse_row_2(s: &str) -> ParseResult<Vec<Face>> {
    let (s, padding) = parse_empties(s)?;

    // parse the first row to get the dimensions
    let (s, tiles) = terminated(parse_tiles, complete::char('\n'))(s)?;

    // figure out if there are one or two tiles in the row
    if padding == 0 && tiles.len() % 2 == 0 {
        // we have two tiles
        let dimensions = tiles.len() / 2;

        // parse the remaining rows of the face
        let (s, rows) = parse_remaining_rows(
            tuple((parse_tiles_len(dimensions), parse_tiles_len(dimensions))),
            dimensions,
        )(s)?;

        let (row_1_first, row_1_second) = tiles.split_at(dimensions);

        let (first, second) = rows.iter().fold(
            (
                {
                    let mut vec = Vec::with_capacity(dimensions);
                    vec.push(row_1_first.to_vec());
                    vec
                },
                {
                    let mut vec = Vec::with_capacity(dimensions);
                    vec.push(row_1_second.to_vec());
                    vec
                },
            ),
            |(mut firsts, mut seconds), (first, second)| {
                firsts.push(first.clone());
                seconds.push(second.clone());

                (firsts, seconds)
            },
        );

        Ok((s, vec![Face::Face(first), Face::Face(second)]))
    } else if padding == tiles.len() {
        // we have one tile, padded by an empty one
        let dimensions = tiles.len();

        // parse the remaining rows of the face
        let (s, rows) = parse_remaining_rows(
            tuple((parse_empties_len(dimensions), parse_tiles_len(dimensions))),
            dimensions,
        )(s)?;

        let second = rows.iter().fold(
            {
                let mut vec = Vec::with_capacity(dimensions);
                vec.push(tiles);
                vec
            },
            |mut seconds, (_, second)| {
                seconds.push(second.clone());
                seconds
            },
        );

        Ok((s, vec![Face::Empty, Face::Face(second)]))
    } else {
        Err(nom::Err::Error(ParseError {
            message: format!(
                "Invalid size for 2 faces. padding: {}, tiles: {}",
                padding,
                tiles.len()
            ),
        }))
    }
}

// function to parse a row of the cube with three faces (may start with empty faces)
// on 4x4 dimensions, this could look like:
// ...#...#...#
// .#...#...#..
// #...#...#...
// ............
// or
//     ...#...#
//     .#...#..
//     #...#...
//     ........
// or
//         ...#
//         .#..
//         #...
//         ....
fn parse_row_3(s: &str) -> ParseResult<Vec<Face>> {
    let (s, padding) = parse_empties(s)?;

    // parse the first row to get the dimensions
    let (s, tiles) = terminated(parse_tiles, complete::char('\n'))(s)?;

    // figure out if there are one, two or three tiles in the row
    if padding == 0 && tiles.len() % 3 == 0 {
        // we have three tiles
        let dimensions = tiles.len() / 3;

        // parse the remaining rows of the face
        let (s, rows) = parse_remaining_rows(
            tuple((
                parse_tiles_len(dimensions),
                parse_tiles_len(dimensions),
                parse_tiles_len(dimensions),
            )),
            dimensions,
        )(s)?;

        let (row_1_first, row_1_rest) = tiles.split_at(dimensions);

        let (row_1_second, row_1_third) = row_1_rest.split_at(dimensions);

        let (first, second, third) = rows.iter().fold(
            (
                {
                    let mut vec = Vec::with_capacity(dimensions);
                    vec.push(row_1_first.to_vec());
                    vec
                },
                {
                    let mut vec = Vec::with_capacity(dimensions);
                    vec.push(row_1_second.to_vec());
                    vec
                },
                {
                    let mut vec = Vec::with_capacity(dimensions);
                    vec.push(row_1_third.to_vec());
                    vec
                },
            ),
            |(mut firsts, mut seconds, mut thirds), (first, second, third)| {
                firsts.push(first.clone());
                seconds.push(second.clone());
                thirds.push(third.clone());

                (firsts, seconds, thirds)
            },
        );

        Ok((
            s,
            vec![Face::Face(first), Face::Face(second), Face::Face(third)],
        ))
    } else if padding * 2 == tiles.len() {
        // we have two tiles, padded by one empty one
        let dimensions = padding;

        // parse the remaining rows of the face
        let (s, rows) = parse_remaining_rows(
            tuple((
                parse_empties_len(dimensions),
                parse_tiles_len(dimensions),
                parse_tiles_len(dimensions),
            )),
            dimensions,
        )(s)?;

        let (row_1_first, row_1_second) = tiles.split_at(dimensions);

        let (second, third) = rows.iter().fold(
            (
                {
                    let mut vec = Vec::with_capacity(dimensions);
                    vec.push(row_1_first.to_vec());
                    vec
                },
                {
                    let mut vec = Vec::with_capacity(dimensions);
                    vec.push(row_1_second.to_vec());
                    vec
                },
            ),
            |(mut seconds, mut thirds), (_, second, third)| {
                seconds.push(second.clone());
                thirds.push(third.clone());

                (seconds, thirds)
            },
        );

        Ok((s, vec![Face::Empty, Face::Face(second), Face::Face(third)]))
    } else if padding == 2 * tiles.len() {
        // we have one tile, padded by two empty ones

        let dimensions = tiles.len();

        // parse the remaining rows of the face

        let (s, rows) = parse_remaining_rows(
            tuple((
                parse_empties_len(dimensions),
                parse_empties_len(dimensions),
                parse_tiles_len(dimensions),
            )),
            dimensions,
        )(s)?;

        let third = rows.iter().fold(
            {
                let mut vec = Vec::with_capacity(dimensions);
                vec.push(tiles);
                vec
            },
            |mut thirds, (_, _, third)| {
                thirds.push(third.clone());
                thirds
            },
        );

        Ok((s, vec![Face::Empty, Face::Empty, Face::Face(third)]))
    } else {
        Err(nom::Err::Error(ParseError {
            message: format!(
                "Invalid size for 3 faces. padding: {}, tiles: {}",
                padding,
                tiles.len()
            ),
        }))
    }
}

// function to parse a row of the cube with four faces (may start with empty faces)
// on 4x4 dimensions, this could look like:
// ...#...#...#...#
// .#...#...#...#..
// #...#...#...#...
// ................
// or
//     ...#...#...#
//     .#...#...#..
//     #...#...#...
//     ............
// or
//         ...#...#
//         .#...#..
//         #...#...
//         ........
// or
//             ...#
//             .#..
//             #...
//             ....
fn parse_row_4(s: &str) -> ParseResult<Vec<Face>> {
    let (s, padding) = parse_empties(s)?;

    // parse the first row to get the dimensions
    let (s, tiles) = terminated(parse_tiles, complete::char('\n'))(s)?;

    // figure out if there are one, two or three tiles in the row
    if padding == 0 && tiles.len() % 4 == 0 {
        // we have four tiles
        let dimensions = tiles.len() / 4;

        // parse the remaining rows of the face
        let (s, rows) = parse_remaining_rows(
            tuple((
                parse_tiles_len(dimensions),
                parse_tiles_len(dimensions),
                parse_tiles_len(dimensions),
                parse_tiles_len(dimensions),
            )),
            dimensions,
        )(s)?;

        let (row_1_first, row_1_rest) = tiles.split_at(dimensions);

        let (row_1_second, row_1_rest) = row_1_rest.split_at(dimensions);

        let (row_1_third, row_1_fourth) = row_1_rest.split_at(dimensions);

        let (first, second, third, fourth) = rows.iter().fold(
            (
                {
                    let mut vec = Vec::with_capacity(dimensions);
                    vec.push(row_1_first.to_vec());
                    vec
                },
                {
                    let mut vec = Vec::with_capacity(dimensions);
                    vec.push(row_1_second.to_vec());
                    vec
                },
                {
                    let mut vec = Vec::with_capacity(dimensions);
                    vec.push(row_1_third.to_vec());
                    vec
                },
                {
                    let mut vec = Vec::with_capacity(dimensions);
                    vec.push(row_1_fourth.to_vec());
                    vec
                },
            ),
            |(mut firsts, mut seconds, mut thirds, mut fourths), (first, second, third, fourth)| {
                firsts.push(first.clone());
                seconds.push(second.clone());
                thirds.push(third.clone());
                fourths.push(fourth.clone());

                (firsts, seconds, thirds, fourths)
            },
        );

        Ok((
            s,
            vec![
                Face::Face(first),
                Face::Face(second),
                Face::Face(third),
                Face::Face(fourth),
            ],
        ))
    } else if padding * 3 == tiles.len() {
        // we have three tiles, padded by one empty one
        let dimensions = padding;

        // parse the remaining rows of the face
        let (s, rows) = parse_remaining_rows(
            tuple((
                parse_empties_len(dimensions),
                parse_tiles_len(dimensions),
                parse_tiles_len(dimensions),
                parse_tiles_len(dimensions),
            )),
            dimensions,
        )(s)?;

        let (row_1_second, row_1_rest) = tiles.split_at(dimensions);

        let (row_1_third, row_1_fourth) = row_1_rest.split_at(dimensions);

        let (second, third, fourth) = rows.iter().fold(
            (
                {
                    let mut vec = Vec::with_capacity(dimensions);
                    vec.push(row_1_second.to_vec());
                    vec
                },
                {
                    let mut vec = Vec::with_capacity(dimensions);
                    vec.push(row_1_third.to_vec());
                    vec
                },
                {
                    let mut vec = Vec::with_capacity(dimensions);
                    vec.push(row_1_fourth.to_vec());
                    vec
                },
            ),
            |(mut seconds, mut thirds, mut fourths), (_, second, third, fourth)| {
                seconds.push(second.clone());
                thirds.push(third.clone());
                fourths.push(fourth.clone());

                (seconds, thirds, fourths)
            },
        );

        Ok((
            s,
            vec![
                Face::Empty,
                Face::Face(second),
                Face::Face(third),
                Face::Face(fourth),
            ],
        ))
    } else if padding == tiles.len() {
        // we have two tiles, padded by two empty ones
        let dimensions = padding / 2;

        // parse the remaining rows of the face
        let (s, rows) = parse_remaining_rows(
            tuple((
                parse_empties_len(dimensions),
                parse_empties_len(dimensions),
                parse_tiles_len(dimensions),
                parse_tiles_len(dimensions),
            )),
            dimensions,
        )(s)?;

        let (row_1_third, row_1_fourth) = tiles.split_at(dimensions);

        let (third, fourth) = rows.iter().fold(
            (
                {
                    let mut vec = Vec::with_capacity(dimensions);
                    vec.push(row_1_third.to_vec());
                    vec
                },
                {
                    let mut vec = Vec::with_capacity(dimensions);
                    vec.push(row_1_fourth.to_vec());
                    vec
                },
            ),
            |(mut thirds, mut fourths), (_, _, third, fourth)| {
                thirds.push(third.clone());
                fourths.push(fourth.clone());

                (thirds, fourths)
            },
        );

        Ok((
            s,
            vec![
                Face::Empty,
                Face::Empty,
                Face::Face(third),
                Face::Face(fourth),
            ],
        ))
    } else if padding * 3 == tiles.len() {
        // we have one tile, padded by three empty ones
        let dimensions = tiles.len();

        // parse the remaining rows of the face
        let (s, rows) = parse_remaining_rows(
            tuple((
                parse_empties_len(dimensions),
                parse_empties_len(dimensions),
                parse_empties_len(dimensions),
                parse_tiles_len(dimensions),
            )),
            dimensions,
        )(s)?;

        let fourth = rows.iter().fold(
            {
                let mut vec = Vec::with_capacity(dimensions);
                vec.push(tiles.to_vec());
                vec
            },
            |mut fourths, (_, _, _, fourth)| {
                fourths.push(fourth.clone());
                fourths
            },
        );

        Ok((
            s,
            vec![Face::Empty, Face::Empty, Face::Empty, Face::Face(fourth)],
        ))
    } else {
        Err(nom::Err::Error(ParseError {
            message: format!(
                "invalid size for 4 faces. padding: {}, tiles: {}",
                padding,
                tiles.len()
            ),
        }))
    }
}

// function to parse a row of the cube with five faces
// this is a special case, since five faces are always padded by two empty ones
// on 4x4 dimensions, this must look like:
//         ...#...#...#
//         .#...#...#..
//         #...#...#...
//         ............
// other combinations will not result in a valid cube
fn parse_row_5(s: &str) -> ParseResult<Vec<Face>> {
    let (s, padding) = parse_empties(s)?;

    // parse the first row to get the dimensions
    let (s, tiles) = terminated(parse_tiles, complete::char('\n'))(s)?;

    if padding * 3 == 2 * tiles.len() {
        // we have three tiles, padded by two empty ones
        let dimensions = padding / 2;

        // parse the remaining rows of the face
        let (s, rows) = parse_remaining_rows(
            tuple((
                parse_empties_len(dimensions),
                parse_empties_len(dimensions),
                parse_tiles_len(dimensions),
                parse_tiles_len(dimensions),
                parse_tiles_len(dimensions),
            )),
            dimensions,
        )(s)?;

        let (row_1_third, row_1_rest) = tiles.split_at(dimensions);

        let (row_1_fourth, row_1_fifth) = row_1_rest.split_at(dimensions);

        let (third, fourth, fifth) = rows.iter().fold(
            (
                {
                    let mut vec = Vec::with_capacity(dimensions);
                    vec.push(row_1_third.to_vec());
                    vec
                },
                {
                    let mut vec = Vec::with_capacity(dimensions);
                    vec.push(row_1_fourth.to_vec());
                    vec
                },
                {
                    let mut vec = Vec::with_capacity(dimensions);
                    vec.push(row_1_fifth.to_vec());
                    vec
                },
            ),
            |(mut thirds, mut fourths, mut fifths), (_, _, third, fourth, fifth)| {
                thirds.push(third.clone());
                fourths.push(fourth.clone());
                fifths.push(fifth.clone());

                (thirds, fourths, fifths)
            },
        );

        Ok((
            s,
            vec![
                Face::Empty,
                Face::Empty,
                Face::Face(third),
                Face::Face(fourth),
                Face::Face(fifth),
            ],
        ))
    } else {
        // all other combinations are invalid
        Err(nom::Err::Error(ParseError {
            message: format!(
                "invalid size for 5 faces. padding: {}, tiles: {}",
                padding,
                tiles.len()
            ),
        }))
    }
}

// function to parse the remaining rows of the faces in the row
// takes a parser as an argument which defines the structure of each row
fn parse_remaining_rows<'a, Result>(
    mut p: impl Parser<&'a str, Result, ParseError>,
    dimensions: usize,
) -> impl FnMut(&'a str) -> ParseResult<'a, Vec<Result>> {
    move |s| {
        count(
            terminated(|s| p.parse(s), complete::char('\n')),
            dimensions - 1,
        )(s)
    }
}

fn parse_empties(s: &str) -> ParseResult<usize> {
    // allow 0 length, since we need to detect the padding:
    many0_count(complete::char(' '))(s)
}

fn parse_empties_len(size_hint: usize) -> impl FnMut(&str) -> ParseResult<()> {
    move |s| count(complete::char(' '), size_hint)(s).map(|(s, _)| (s, ()))
}

fn parse_tiles(s: &str) -> ParseResult<Vec<Tile>> {
    many1(parse_tile)(s)
}

fn parse_tiles_len(size_hint: usize) -> impl FnMut(&str) -> ParseResult<Vec<Tile>> {
    move |s| count(parse_tile, size_hint)(s)
}

fn parse_tile(s: &str) -> ParseResult<Tile> {
    branch::alt((parse_floor, parse_wall))(s)
}

fn parse_wall(s: &str) -> ParseResult<Tile> {
    complete::char('#')(s).map(|(s, _)| (s, Tile::Wall))
}

fn parse_floor(s: &str) -> ParseResult<Tile> {
    complete::char('.')(s).map(|(s, _)| (s, Tile::Empty))
}
