use std::{
    fs::File,
    io::{self, BufRead, Read},
    str::FromStr,
};

use crate::error::AOCError;

pub struct AOCReader {
    file: File,
}

impl AOCReader {
    pub fn new(file: File) -> Self {
        Self { file }
    }

    pub fn lines(self) -> impl Iterator<Item = Result<String, AOCError>> {
        io::BufReader::new(self.file).lines().map(|line| {
            let line = line?;

            Ok(line)
        })
    }

    pub fn parse_lines<T: FromStr>(self) -> impl Iterator<Item = Result<T, AOCError>> {
        self.lines().map(|line| {
            let line = line?;

            if let Ok(v) = line.parse() {
                Ok(v)
            } else {
                Err(AOCError::ParseErr())
            }
        })
    }

    pub fn content(mut self) -> Result<String, AOCError> {
        let mut buffer = String::new();
        self.file.read_to_string(&mut buffer)?;

        Ok(buffer)
    }

    pub fn parse_content<T: FromStr>(self) -> Result<T, AOCError> {
        let content = self.content()?;

        if let Ok(v) = content.parse() {
            Ok(v)
        } else {
            Err(AOCError::ParseErr())
        }
    }
}

pub fn read_input(day: usize) -> Result<AOCReader, AOCError> {
    // the working dir when running cargo run is the root of the project
    let path = format!("./inputs/day{:02}.txt", day);
    println!("Reading input file: {}", path);

    let file = File::open(path).unwrap();

    Ok(AOCReader::new(file))
}

pub fn read_test(day: usize, test: usize) -> Result<AOCReader, AOCError> {
    // the working dir for tests is the subproject root, so we need to go up two dirs
    let path = format!("../../tests/day{:02}_{}.txt", day, test);

    println!("Reading test file: {}", path);

    let file = File::open(path)?;

    Ok(AOCReader::new(file))
}
