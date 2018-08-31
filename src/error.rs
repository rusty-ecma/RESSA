use node::Position;
use std::fmt::{Formatter, Result, Display};
#[derive(Debug)]
pub enum Error {
    UnexpectedToken(Position, String),
    UnexpectedEoF,
    ParseAfterEoF,
    UnableToReinterpret(Position, String, String),
    Redecl(Position, String),
    OperationError(Position, String),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            &Error::UnexpectedToken(ref pos, ref msg) => write!(f, "Unexpected Token at {}: {}", pos, msg),
            &Error::UnexpectedEoF => write!(f, "Unexpectedly found the end of the file"),
            &Error::ParseAfterEoF => write!(f, "Parser attempted to get the next token after finding the end of the file"),
            &Error::UnableToReinterpret(ref pos, ref from, ref to) => write!(f, "Unable to re-interpret from {} to {} at {}", from, to, pos),
            &Error::Redecl(ref pos, ref ident) => write!(f, "{}, {} has already been declared", pos, ident),
            &Error::OperationError(ref pos, ref msg) => write!(f, "Invalid operation, {}: {}", pos, msg),
        }
    }
}

impl Error {
    pub fn unable_to_reinterpret(pos: Position, from: &str, to: &str) -> Self {
        Error::UnableToReinterpret(pos, from.to_owned(), to.to_owned())
    }
}