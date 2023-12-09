use std::{fs::File, io::Read};

use crate::error::AOCError;

pub fn read_input(day: usize) -> Result<String, AOCError> {
    // the working dir when running cargo run is the root of the project
    let path = format!("./inputs/day{:02}.txt", day);
    println!("Reading input file: {}", path);

    let mut file = File::open(path)?;
    let mut buffer = String::new();
    file.read_to_string(&mut buffer)?;

    Ok(buffer)
}

pub fn read_test(day: usize, test: usize) -> Result<String, AOCError> {
    // the working dir for tests is the subproject root, so we need to go up two dirs
    let path = format!("../../tests/day{:02}_{}.txt", day, test);

    println!("Reading test file: {}", path);

    let mut file = File::open(path)?;
    let mut buffer = String::new();
    file.read_to_string(&mut buffer)?;

    Ok(buffer)
}
