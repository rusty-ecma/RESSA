use ress::{tokens::Keyword, Position};
use std::fmt::{Display, Formatter, Result};
#[derive(Debug)]
pub enum Error {
    UnexpectedEoF,
    ParseAfterEoF,
    UnexpectedToken(Position, String),
    UnableToReinterpret(Position, String, String),
    Redecl(Position, String),
    OperationError(Position, String),
    InvalidGetterParams(Position),
    InvalidSetterParams(Position),
    NonStrictFeatureInStrictContext(Position, String),
    InvalidImportError(Position),
    InvalidExportError(Position),
    InvalidYield(Position),
    InvalidUseOfContextualKeyword(Position, String),
    TryWithNoCatchOrFinally(Position),
    InvalidCatchArg(Position),
    ThrowWithNoArg(Position),
    UnknownOptionalLabel(Position, Keyword<()>, String),
    InvalidOptionalLabel(Position),
    UseOfModuleFeatureOutsideOfModule(Position, String),
    NewLineAfterFatArrow(Position),
    StrictModeArgumentsOrEval(Position),
    InvalidSuper(Position),
    InvalidFuncPosition(Position, String),
    ForOfInAssign(Position, String),
    ContinueOutsideOfIteration(Position),
    InvalidParameter(Position, String),
    OctalLiteral(Position),
    Scanner(ress::error::Error),
    Other(Box<dyn ::std::error::Error>),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Error::UnexpectedToken(ref pos, ref msg) => write!(f, "Unexpected Token at {}: {}", pos, msg),
            Error::UnexpectedEoF => write!(f, "Unexpectedly found the end of the file"),
            Error::ParseAfterEoF => write!(f, "Parser attempted to get the next token after finding the end of the file"),
            Error::UnableToReinterpret(ref pos, ref from, ref to) => write!(f, "Unable to re-interpret from {} to {} at {}", from, to, pos),
            Error::Redecl(ref pos, ref ident) => write!(f, "Nested label at {} shadows parent: {}", pos, ident),
            Error::OperationError(ref pos, ref msg) => write!(f, "Invalid operation, {}: {}", pos, msg),
            Error::InvalidGetterParams(ref pos) => write!(f, "Found a getter method that takes arguments at {}, getter methods cannot take arguments", pos),
            Error::InvalidSetterParams(ref pos) => write!(f, "Found a setter method that takes more or less than 1 argument at {}, setter methods must take only one argument", pos),
            Error::InvalidYield(ref pos) => write!(f, "Yield cannot be used as an identifier at {}", pos),
            Error::NonStrictFeatureInStrictContext(ref pos, ref name) => write!(f, "Attempting to use non-strict feature {} in a strict context at {}", name, pos),
            Error::InvalidImportError(ref pos) => write!(f, "Inavlid use of es6 import syntax at {}", pos),
            Error::InvalidExportError(ref pos) => write!(f, "Inavlid use of es6 export syntax at {}", pos),
            Error::InvalidUseOfContextualKeyword(ref pos, ref ident) => write!(f, "Inavlid use of contextual keyword {} at {}", pos, ident),
            Error::TryWithNoCatchOrFinally(ref pos) => write!(f, "Try catch block must have catch or finally clause at {}", pos),
            Error::InvalidCatchArg(ref pos) => write!(f, "Catch block with parentheses must have 1 argument: {}", pos),
            Error::ThrowWithNoArg(ref pos) => write!(f, "Throw statements without an argument {}", pos),
            Error::UnknownOptionalLabel(ref pos, ref key, ref label) => write!(f, "Attempt to {0} {1} but {1} is unknown in this scope at {2}", key.to_string(), label, pos),
            Error::InvalidOptionalLabel(ref pos) => write!(f, "Attempt to break with no label is not allowed unless in an iteration or switch: {}", pos),
            Error::UseOfModuleFeatureOutsideOfModule(ref pos, ref feature) => write!(f, "Used {} at {} which is only available inside of an es6 module", feature, pos),
            Error::NewLineAfterFatArrow(ref pos) => write!(f, "New line after fat arrow at {}", pos),
            Error::StrictModeArgumentsOrEval(ref pos) => write!(f, "arguments or eval used as an identifier in strict mode near {}", pos),
            Error::InvalidFuncPosition(ref pos, ref msg) => write!(f, "{} at {}", msg, pos),
            Error::ForOfInAssign(ref pos, ref msg) => write!(f, "{} at {}", msg, pos),
            Error::InvalidSuper(ref pos) => write!(f, "Invalid use of super at {}", pos),
            Error::ContinueOutsideOfIteration(ref pos) => write!(f, "Invalid use of continue at {}", pos),
            Error::InvalidParameter(ref pos, ref msg) => write!(f, "Invalid paramter at {} -- {}", pos, msg),
            Error::OctalLiteral(ref pos) => write!(f, "Invalid use of octal literal at {}", pos),
            Error::Scanner(ref e) => write!(f, "Error when tokenizing {}", e),
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
impl ::std::error::Error for Error {}

impl From<ress::error::Error> for Error {
    fn from(other: ress::error::Error) -> Self {
        Error::Scanner(other)
    }
}
