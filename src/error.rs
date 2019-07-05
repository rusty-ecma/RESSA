use ress::Position;
use std::fmt::{Display, Formatter, Result};
#[derive(Debug)]
pub enum Error {
    UnexpectedToken(Position, String),
    UnexpectedEoF,
    ParseAfterEoF,
    UnableToReinterpret(Position, String, String),
    Redecl(Position, String),
    OperationError(Position, String),
    InvalidGetterParams(Position),
    InvalidSetterParams(Position),
    NonStrictFeatureInStrictContext(Position, String),
    Other(Box<::std::error::Error>),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Error::UnexpectedToken(ref pos, ref msg) => write!(f, "Unexpected Token at {}{}: {}", pos.line, pos.column, msg),
            Error::UnexpectedEoF => write!(f, "Unexpectedly found the end of the file"),
            Error::ParseAfterEoF => write!(f, "Parser attempted to get the next token after finding the end of the file"),
            Error::UnableToReinterpret(ref pos, ref from, ref to) => write!(f, "Unable to re-interpret from {} to {} at {}", from, to, pos),
            Error::Redecl(ref pos, ref ident) => write!(f, "Nested label at {} shadows parent: {}", pos, ident),
            Error::OperationError(ref pos, ref msg) => write!(f, "Invalid operation, {}: {}", pos, msg),
            Error::InvalidGetterParams(ref pos) => write!(f, "Found a getter method that takes arguments at {}, getter methods cannot take arguments", pos),
            Error::InvalidSetterParams(ref pos) => write!(f, "Found a setter method that takes more or less than 1 argument at {}, setter methods must take only one argument", pos),
            Error::NonStrictFeatureInStrictContext(ref pos, ref name) => write!(f, "Attempting to use non-strict feature {} in a strict context at {}", name, pos),
            Error::Other(ref e) => write!(f, "{}", e),
        }
    }
}

impl Error {
    pub fn unable_to_reinterpret(pos: Position, from: &str, to: &str) -> Self {
        Error::UnableToReinterpret(pos, from.to_owned(), to.to_owned())
    }
}

impl From<::std::io::Error> for Error {
    fn from(other: ::std::io::Error) -> Self {
        Error::Other(Box::new(other))
    }
}
