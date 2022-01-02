use super::{Error, Res};
use res_regex::RegexParser;
/// Validate that an already parsed regular expression
/// literal does not contain any illegal constructs
/// like a duplicate flag or invalid class range
pub fn validate_regex<'a>(regex: &'a str) -> Res<()> {
    RegexParser::new(&regex)
        .map_err(|e| Error::Other(Box::new(e)))?
        .validate()
        .map_err(|e| Error::Other(Box::new(e)))?;
    Ok(())
}
