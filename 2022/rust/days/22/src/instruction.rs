use nom::{branch, character::complete, multi::many0, sequence::tuple};

use crate::parse::ParseResult;

// 10R5L5R10L4R5L5

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(super) enum Instruction {
    Forward(usize),
    TurnLeft,
    TurnRight,
}

pub(super) fn parse_instructions(s: &str) -> ParseResult<Vec<Instruction>> {
    let (s, instructions) = many0(tuple((
        parse_forward,
        branch::alt((parse_turn_left, parse_turn_right)),
    )))(s)?;

    let instructions = instructions.iter().fold(
        Vec::with_capacity(2 * instructions.len()),
        |mut f, (i, j)| {
            f.push(*i);
            f.push(*j);
            f
        },
    );

    Ok((s, instructions))
}

fn parse_forward(s: &str) -> ParseResult<Instruction> {
    let (s, n) = complete::u32(s)?;

    Ok((s, Instruction::Forward(n as usize)))
}

fn parse_turn_left(s: &str) -> ParseResult<Instruction> {
    let (s, _) = complete::char('L')(s)?;

    Ok((s, Instruction::TurnLeft))
}

fn parse_turn_right(s: &str) -> ParseResult<Instruction> {
    let (s, _) = complete::char('R')(s)?;

    Ok((s, Instruction::TurnRight))
}
