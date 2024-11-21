use lib::{parse::*, AOCError, AdventOfCode};

#[derive(Debug, Clone, Copy)]
struct Hail {
    x: f64,
    y: f64,
    z: f64,

    dx: f64,
    dy: f64,
    dz: f64,
}

// 18, 19, 22 @ -1, -1, -2
fn parse_hail(s: &str) -> ParseResult<Hail> {
    let (s, (x, _, y, _, z, _, dx, _, dy, _, dz)) = tuple((
        u64,
        tuple((tag(","), space1)),
        u64,
        tuple((tag(","), space1)),
        u64,
        tuple((space1, tag("@"), space1)),
        i64,
        tuple((tag(","), space1)),
        i64,
        tuple((tag(","), space1)),
        i64,
    ))(s)?;

    Ok((
        s,
        Hail {
            x: x as f64,
            y: y as f64,
            z: z as f64,

            dx: dx as f64,
            dy: dy as f64,
            dz: dz as f64,
        },
    ))
}

impl Hail {
    fn collide(&self, other: &Self, min_x: f64, max_x: f64, min_y: f64, max_y: f64) -> bool {
        // determine if the two hails collide
        // by checking if the distance between them
        //
        // reference: The shortest line between two lines in 3D space - Paul Bourke

        // two hail lines: P1(x1) <-> P2(x1 + dx1), P3(x2) <-> P4(x2 + dx2)

        let p1 = Vector::new(self.x, self.y, self.z);
        let p2 = Vector::new(self.x + self.dx, self.y + self.dy, self.z + self.dz);

        let p3 = Vector::new(other.x, other.y, other.z);
        let p4 = Vector::new(other.x + other.dx, other.y + other.dy, other.z + other.dz);

        // the following is loosely taken from the reference

        let p13 = p1 - p3;
        let p43 = p4 - p3;

        if p43.too_small_components() {
            return false;
        }

        let p21 = p2 - p1;

        if p21.too_small_components() {
            return false;
        }

        let d1343 = p13.dot(&p43);
        let d4321 = p43.dot(&p21);
        let d1321 = p13.dot(&p21);
        let d4343 = p43.dot(&p43);
        let d2121 = p21.dot(&p21);

        let denom = d2121 * d4343 - d4321 * d4321;

        if denom < 0.00000001 {
            return false;
        }

        let numer = d1343 * d4321 - d1321 * d4343;

        let mua = numer / denom;
        let mub = (d1343 + d4321 * mua) / d4343;

        let pa = p1 + p21 * mua;
        let pb = p3 + p43 * mub;

        // we can only look forwards in time

        if mua < 0.0 || mub < 0.0 {
            return false;
        }

        if pa.distance(&pb) > 0.00000001 {
            return false;
        }

        // also check bounds
        pa.x >= min_x
            && pa.x <= max_x
            && pa.y >= min_y
            && pa.y <= max_y
            && pb.x >= min_x
            && pb.x <= max_x
            && pb.y >= min_y
            && pb.y <= max_y
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
struct Vector {
    x: f64,
    y: f64,
    z: f64,
}

impl Vector {
    fn new(x: f64, y: f64, z: f64) -> Self {
        Self { x, y, z }
    }

    fn dot(&self, other: &Self) -> f64 {
        self.x * other.x + self.y * other.y + self.z * other.z
    }

    fn too_small_components(&self) -> bool {
        self.x.abs() < 0.00000001 && self.y.abs() < 0.00000001 && self.z.abs() < 0.00000001
    }
    fn distance(&self, other: &Self) -> f64 {
        let dx = self.x - other.x;
        let dy = self.y - other.y;
        let dz = self.z - other.z;

        (dx * dx + dy * dy + dz * dz).sqrt()
    }
}

impl std::ops::Sub for Vector {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self {
            x: self.x - rhs.x,
            y: self.y - rhs.y,
            z: self.z - rhs.z,
        }
    }
}

impl std::ops::Add for Vector {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
            z: self.z + rhs.z,
        }
    }
}

impl std::ops::Mul<f64> for Vector {
    type Output = Self;

    fn mul(self, rhs: f64) -> Self::Output {
        Self {
            x: self.x * rhs,
            y: self.y * rhs,
            z: self.z * rhs,
        }
    }
}

struct Day {}

impl AdventOfCode for Day {
    const DAY: usize = 24;

    type In = Vec<Hail>;

    type Out = usize;

    fn parse(s: &str) -> ParseResult<Self::In> {
        parse_lines(parse_hail)(s)
    }

    fn part1(input: &Self::In) -> Result<Self::Out, AOCError> {
        println!("{:?}", input);

        let min = 200_000_000_000_000.0;
        let max = 400_000_000_000_000.0;

        Ok(input
            .iter()
            .flat_map(|h| input.iter().filter(|h2| h.collide(h2, min, max, min, max)))
            .count())
    }

    fn part2(input: &Self::In) -> Result<Self::Out, AOCError> {
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
        lib::test(Day {}, lib::Part::Part1, 1, 2)
    }

    #[test]
    fn test2() -> Result<(), AOCError> {
        lib::test(Day {}, lib::Part::Part2, 1, 10)
    }
}
