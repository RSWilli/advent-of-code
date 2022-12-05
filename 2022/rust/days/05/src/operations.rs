use std::str::FromStr;

use lib::AOCError;

pub(crate) struct Operation {
    pub(crate) from: usize,
    pub(crate) to: usize,
    pub(crate) amount: usize,
}

impl FromStr for Operation {
    type Err = AOCError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let rest = s.strip_prefix("move ").ok_or(AOCError::ParseErr())?;

        let (amount, rest) = rest.split_once(" ").ok_or(AOCError::ParseErr())?;

        let amount = amount.parse()?;

        let rest = rest.strip_prefix("from ").ok_or(AOCError::ParseErr())?;

        let (from, rest) = rest.split_once(" ").ok_or(AOCError::ParseErr())?;

        let from = from.parse()?;

        let to = rest.strip_prefix("to ").ok_or(AOCError::ParseErr())?;

        let to = to.parse()?;

        Ok(Operation { from, to, amount })
    }
}
