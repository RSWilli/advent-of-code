use std::{collections::HashMap, path::PathBuf};

use lib::{AOCError, AOCReader, AdventOfCode};

struct Day {}

fn sum_directories(files: &[(String, usize)]) -> HashMap<String, usize> {
    let mut tree = HashMap::new();

    for (path, size) in files {
        let pathb = PathBuf::from(path);
        for ancestor in pathb.parent().unwrap().ancestors() {
            let dirsize = tree
                .entry(ancestor.to_string_lossy().to_string())
                .or_insert(0);

            *dirsize += size
        }
    }

    tree
}

impl AdventOfCode for Day {
    const DAY: usize = 7;

    type In = Vec<(String, usize)>;

    type Out = usize;

    fn parse(&self, inp: AOCReader) -> Result<Self::In, AOCError> {
        let mut files = Vec::new();
        let mut currentpath = PathBuf::from("/");

        for line in inp.lines().skip(2) {
            let line = line?;

            if let Some(cmd) = line.strip_prefix("$ ") {
                match cmd {
                    "ls" => continue,
                    _ => {
                        if let Some(target) = cmd.strip_prefix("cd ") {
                            match target {
                                ".." => {
                                    currentpath = currentpath
                                        .parent()
                                        .ok_or(AOCError::ParseErr())?
                                        .to_path_buf();
                                }
                                target => currentpath = currentpath.join(target),
                            }
                        } else {
                            return Err(AOCError::ParseErr());
                        }
                    }
                }
            } else if let Some((dir_or_size, name)) = line.split_once(' ') {
                match dir_or_size {
                    "dir" => {}
                    size => {
                        let size = size.parse()?;

                        let path = currentpath.join(name).to_string_lossy().to_string();

                        files.push((path, size));
                    }
                }
            } else {
                return Err(AOCError::ParseErr());
            }
        }

        Ok(files)
    }

    fn part1(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        let dir_sizes = sum_directories(input);

        Ok(dir_sizes
            .into_iter()
            .filter(|(_, size)| size <= &100000)
            .map(|(_, s)| s)
            .sum())
    }

    fn part2(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        let dir_sizes = sum_directories(input);

        let root_size = dir_sizes.get("/").ok_or(AOCError::AOCError {
            msg: "no root found",
        })?;

        let space_needed = *root_size - (70000000 - 30000000);

        Ok(*dir_sizes
            .values()
            .filter(|x| x > &&space_needed)
            .min()
            .unwrap_or(&0))
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
        lib::test(Day {}, lib::Part::Part1, 1, 95437)
    }

    #[test]
    fn test2() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part2, 1, 24933642)
    }
}
