use lib::{AOCError, AOCReader, AdventOfCode};

struct Day {}

struct NaughtyString(String);

impl NaughtyString {
    fn is_nice_p1(&self) -> bool {
        let chars = self.0.bytes().collect::<Vec<_>>();

        if chars
            .windows(2)
            .any(|f| matches!(f, [b'a', b'b'] | [b'c', b'd'] | [b'p', b'q'] | [b'x', b'y']))
        {
            return false;
        }

        if chars.windows(2).filter(|f| f[0] == f[1]).count() < 1 {
            return false;
        }

        if chars
            .iter()
            .filter(|&&c| matches!(c, b'a' | b'e' | b'i' | b'o' | b'u'))
            .count()
            < 3
        {
            return false;
        }

        true
    }

    fn is_nice_p2(&self) -> bool {
        let chars = self.0.bytes().collect::<Vec<_>>();

        if chars.windows(3).filter(|f| f[0] == f[2]).count() < 1 {
            return false;
        }

        if chars
            .windows(2)
            .enumerate()
            .filter(|(i, f)| chars[i + 2..].windows(2).any(|g| g == *f))
            .count()
            < 1
        {
            return false;
        }

        true
    }
}

impl AdventOfCode for Day {
    const DAY: usize = 5;

    type In = Vec<NaughtyString>;

    type Out = usize;

    fn parse(&self, inp: AOCReader) -> Result<Self::In, AOCError> {
        inp.lines().map(|l| l.map(NaughtyString)).collect()
    }

    fn part1(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        Ok(input.iter().filter(|s| s.is_nice_p1()).count())
    }

    fn part2(&self, input: &Self::In) -> Result<Self::Out, AOCError> {
        Ok(input.iter().filter(|s| s.is_nice_p2()).count())
    }
}

fn main() -> Result<(), AOCError> {
    lib::run(Day {})
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test1() {
        assert!(NaughtyString("ugknbfddgicrmopn".into()).is_nice_p1())
    }
    #[test]
    fn test2() {
        assert!(NaughtyString("aaa".into()).is_nice_p1())
    }
    #[test]
    fn test3() {
        assert!(!NaughtyString("jchzalrnumimnmhp".into()).is_nice_p1())
    }
    #[test]
    fn test4() {
        assert!(!NaughtyString("haegwjzuvuyypxyu".into()).is_nice_p1())
    }
    #[test]
    fn test5() {
        assert!(!NaughtyString("dvszwmarrgswjxmb".into()).is_nice_p1())
    }
}
