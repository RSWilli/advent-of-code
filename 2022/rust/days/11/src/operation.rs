use std::str::FromStr;

use lib::AOCError;

// the operation the monkey performs
#[derive(Debug, Clone)]
pub(crate) enum Operation {
    Add(Value, Value),
    Mul(Value, Value),
}

impl FromStr for Operation {
    type Err = AOCError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut op = s.split_ascii_whitespace();

        let first = op.next().ok_or(AOCError::ParseErr())?;
        let operand = op.next().ok_or(AOCError::ParseErr())?;
        let second = op.next().ok_or(AOCError::ParseErr())?;

        let first: Value = first.parse()?;
        let second: Value = second.parse()?;

        match operand {
            "+" => Ok(Self::Add(first, second)),
            "*" => Ok(Self::Mul(first, second)),
            _ => Err(AOCError::ParseErr()),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Value {
    Old,
    Num(usize),
}

impl Value {
    fn eval(&self, old: usize) -> usize {
        match self {
            Value::Old => old,
            Value::Num(n) => *n,
        }
    }
}

impl FromStr for Value {
    type Err = AOCError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "old" => Ok(Self::Old),
            _ => Ok(Self::Num(s.parse().or(Err(AOCError::ParseErr()))?)),
        }
    }
}

impl Operation {
    pub(crate) fn eval(&self, old: usize) -> usize {
        match self {
            Operation::Add(l, r) => {
                let l = l.eval(old);
                let r = r.eval(old);

                l + r
            }
            Operation::Mul(l, r) => {
                let l = l.eval(old);
                let r = r.eval(old);

                l * r
            }
        }
    }
}
