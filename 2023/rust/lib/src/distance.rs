use std::{
    fmt::{Debug, Display},
    ops::Add,
};

// a usize wrapper that can be used to represent infinity
// that way usize::MAX wont overflow addition
#[derive(Clone, Copy)]
pub enum Distance {
    N(usize),
    Infinity,
}

impl From<usize> for Distance {
    fn from(n: usize) -> Self {
        Distance::N(n)
    }
}

impl Display for Distance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Distance::N(n) => write!(f, "{}", n),
            Distance::Infinity => write!(f, "∞"),
        }
    }
}

impl Debug for Distance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Distance::N(n) => write!(f, "{:?}", n),
            Distance::Infinity => write!(f, "∞"),
        }
    }
}

impl PartialEq for Distance {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Distance::N(n1), Distance::N(n2)) => n1 == n2,
            (Distance::Infinity, Distance::Infinity) => true,
            _ => false,
        }
    }
}

impl Eq for Distance {}

impl PartialOrd for Distance {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Distance {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (Distance::N(n1), Distance::N(n2)) => n1.cmp(n2),
            (Distance::Infinity, Distance::Infinity) => std::cmp::Ordering::Equal,
            (Distance::Infinity, _) => std::cmp::Ordering::Greater,
            (_, Distance::Infinity) => std::cmp::Ordering::Less,
        }
    }
}

impl Add for Distance {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Distance::N(n1), Distance::N(n2)) => Distance::N(n1 + n2),
            (Distance::Infinity, _) => Distance::Infinity,
            (_, Distance::Infinity) => Distance::Infinity,
        }
    }
}
