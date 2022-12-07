use std::collections::{HashMap, VecDeque};

use lib::AOCError;

use crate::terminalOutput::TerminalOutput;

pub(crate) enum FileSystem {
    File {
        name: String,
        size: usize,
    },
    Dir {
        name: String,
        children: HashMap<String, FileSystem>,
    },
}

impl TryFrom<Vec<TerminalOutput>> for FileSystem {
    type Error = AOCError;

    fn try_from(value: Vec<TerminalOutput>) -> Result<Self, Self::Error> {
        let mut filesystem = FileSystem::Dir {
            name: "/".into(),
            children: HashMap::new(),
        };
        let mut currentPath: VecDeque<&mut FileSystem> = VecDeque::from([&mut filesystem]);

        let mut value_iter = value.iter();

        let Some(TerminalOutput::Cd { arg: _ }) = value_iter.next() else {
            return Err(AOCError::ParseErr());
        };

        let Some(TerminalOutput::Ls) = value_iter.next() else {
            return Err(AOCError::ParseErr());
        };

        for out in value_iter {
            match out {
                TerminalOutput::Dir { name } => {
                    let item = FileSystem::Dir {
                        name: name.into(),
                        children: HashMap::new(),
                    };

                    let Some(current_node) = currentPath.front_mut() else {
                        return Err(AOCError::AOCError { msg: "pwd was empty" });
                    };

                    current_node.appendChild(item)?;
                }
                TerminalOutput::File { size, name } => {
                    let item = FileSystem::File {
                        name: name.into(),
                        size: *size,
                    };

                    let Some(current_node) = currentPath.front_mut() else {
                        return Err(AOCError::AOCError { msg: "pwd was empty" });
                    };

                    current_node.appendChild(item)?;
                }
                TerminalOutput::Cd { arg } => match &arg[..] {
                    ".." => {
                        currentPath.pop_front();
                    }
                    x => {
                        let mut child: &mut FileSystem;
                        if let Some(current_node) = currentPath.front_mut() {
                            child = current_node.get_child_mut(x)?;
                        } else {
                            return Err(AOCError::AOCError {
                                msg: "pwd was empty",
                            });
                        };

                        // TODO: uncomment and please borrow checker:
                        // currentPath.push_front(child)
                    }
                },
                TerminalOutput::Ls => continue,
            }
        }

        Ok(filesystem)
    }
}

impl FileSystem {
    fn appendChild(&mut self, other: FileSystem) -> Result<(), AOCError> {
        match self {
            FileSystem::File { name, size } => Err(AOCError::AOCError {
                msg: "cannot append a file or dir to a file",
            }),
            FileSystem::Dir { name, children } => match other {
                FileSystem::File { ref name, size } => {
                    children.insert(name.clone(), other);

                    Ok(())
                }
                FileSystem::Dir {
                    ref name,
                    children: _,
                } => {
                    children.insert(name.clone(), other);

                    Ok(())
                }
            },
        }
    }

    fn get_child_mut(&mut self, child_name: &str) -> Result<&mut Self, AOCError> {
        match self {
            FileSystem::File { name, size } => Err(AOCError::AOCError {
                msg: "file does not have children",
            }),
            FileSystem::Dir { name, children } => {
                let child = children.get_mut(child_name).ok_or(AOCError::AOCError {
                    msg: "child not found",
                })?;

                Ok(child)
            }
        }
    }
}
