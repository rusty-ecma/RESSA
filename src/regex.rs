use super::{Error, Res};
use res_regex::RegexParser;
use ress::Position;
/// Validate that an already parsed regular expression
/// literal does not contain any illegal constructs
/// like a duplicate flag or invalid class range
pub fn validate_regex(pos: Position, regex: &str) -> Res<()> {
    RegexParser::new(regex)
        .map_err(|e| Error::invalid_regex(pos, e))?
        .validate()
        .map_err(|e| Error::invalid_regex(pos, e))?;
    Ok(())
}
