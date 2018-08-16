use node::Position;
use std::fmt::{Formatter, Result, Display};
#[derive(Debug)]
pub enum Error {
    UnexpectedToken(Position, String),
    UnexpectedEoF,
    ParseAfterEoF,
    UnableToReinterpret(String, String),
    Redecl(String),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            &Error::UnexpectedToken(ref pos, ref msg) => write!(f, "Unexpected Token at column {}, line {}: {}", pos.column, pos.line, msg),
            &Error::UnexpectedEoF => write!(f, "Unexpectedly found the end of the file"),
            &Error::ParseAfterEoF => write!(f, "Parser attempted to get the next token after finding the end of the file"),
            &Error::UnableToReinterpret(ref from, ref to) => write!(f, "Unable to re-interpret from {} to {}", from, to),
            &Error::Redecl(ref ident) => write!(f, "{} has already been declared", ident),
        }
    }
}

impl Error {
    pub fn unable_to_reinterpret(from: &str, to: &str) -> Self {
        Error::UnableToReinterpret(from.to_string(), to.to_string())
    }

    pub fn redecl(ident: &str) -> Self {
        Error::Redecl(ident.to_string())
    }
}