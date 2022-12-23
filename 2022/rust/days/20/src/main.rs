use lib::{AOCError, AOCReader, AdventOfCode};

struct Day {}

impl AdventOfCode for Day {
    const DAY: usize = 20;

    type In = Vec<isize>;

    type Out = isize;

    fn parse(&self, inp: AOCReader) -> Result<Self::In, AOCError> {
        inp.parse_lines().collect()
    }

    fn part1(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        let mut current = input.clone();
        let mut old_indices: Vec<_> = (0..input.len()).into_iter().collect();

        let mut i = 0_usize;

        while i < old_indices.len() {
            let oldindex = old_indices[i];
            let current_el = current.remove(oldindex);
            let newindex = ((oldindex as isize + current_el) % current.len() as isize) as usize;

            current.insert(newindex, current_el);

            i += 1;
        }

        unimplemented!()
        // Ok(input.iter().sum())
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
        lib::test(Day {}, lib::Part::Part1, 1, 3)
    }
}
