use std::str::FromStr;

use lib::AOCError;

pub(crate) enum TerminalOutput {
    Ls,
    Cd { arg: String },
    Dir { name: String },
    File { size: usize, name: String },
}

impl<'a> FromStr for TerminalOutput {
    type Err = AOCError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some(s) = s.strip_prefix("$ ") {
            let (cmd, arg) = s.split_once(' ').ok_or(AOCError::ParseErr())?;

            let arg = arg.try_into().or(Err(AOCError::ParseErr()))?;

            match cmd {
                "ls" => Ok(TerminalOutput::Ls),
                "cd" => Ok(TerminalOutput::Cd { arg }),
                _ => Err(AOCError::ParseErr()),
            }
        } else if let Some(s) = s.strip_prefix("dir ") {
            let name = s.try_into().or(Err(AOCError::ParseErr()))?;

            Ok(TerminalOutput::Dir { name })
        } else {
            let (size, name) = s.split_once(' ').ok_or(AOCError::ParseErr())?;

            let size = size.parse()?;

            let name = name.try_into().or(Err(AOCError::ParseErr()))?;

            Ok(TerminalOutput::File { size, name })
        }
    }
}
