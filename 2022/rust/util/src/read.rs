use std::{fs::File, io};

use crate::error::AOCError;

pub fn read_input(day: usize) -> Result<io::BufReader<File>, AOCError> {
    // the working dir when running cargo run is the root of the project
    let path = format!("./inputs/day{:02}.txt", day);
    println!("Reading input file: {}", path);

    let file = File::open(path).unwrap();

    Ok(io::BufReader::new(file))
}

pub fn read_test(day: usize, test: usize) -> Result<io::BufReader<File>, AOCError> {
    // the working dir for tests is the subproject root, so we need to go up two dirs
    let path = format!("../../tests/day{:02}_{}.txt", day, test);

    println!("Reading test file: {}", path);

    let file = File::open(path)?;

    Ok(io::BufReader::new(file))
}
