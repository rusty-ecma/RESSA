use super::{Error, Position, Res};
use resast::prelude::RegEx;

/// Validate that an already parsed regular expression
/// literal does not contain any illegal constructs
/// like a duplicate flag or invalid class range
pub fn validate_regex(regex: &RegEx, start: Position) -> Res<()> {
    let mut flags = RegeExFlags::default();
    for c in regex.flags.chars() {
        flags.add_flag(c, start)?;
    }
    if regex.pattern.starts_with('*') {
        return Err(Error::InvalidRegEx(
            start,
            "regular expression literal cannot start with '*'".to_string(),
        ));
    }
    let _p = regex_syntax::ParserBuilder::new()
        .unicode(true)
        .case_insensitive(flags.case_insensitive)
        .dot_matches_new_line(flags.dot_matches_new_line)
        .multi_line(flags.multi_line)
        .allow_invalid_utf8(true)
        .octal(true)
        .build()
        .parse(&regex_syntax::escape(&regex.pattern))
        .map_err(|e| Error::InvalidRegEx(start, format!("failed to parse regex: `{}`", e)))?;

    Ok(())
}

struct RegeExFlags {
    case_insensitive: bool,
    multi_line: bool,
    dot_matches_new_line: bool,
    unicode: bool,
    global: bool,
    sticky: bool,
}

impl Default for RegeExFlags {
    fn default() -> Self {
        RegeExFlags {
            case_insensitive: false,
            multi_line: false,
            dot_matches_new_line: false,
            unicode: false,
            global: false,
            sticky: false,
        }
    }
}

impl RegeExFlags {
    fn add_flag(&mut self, c: char, pos: Position) -> Res<()> {
        match c {
            'g' => {
                if self.global {
                    Err(Error::InvalidRegEx(
                        pos,
                        "duplicate global flag".to_string(),
                    ))
                } else {
                    self.global = true;
                    Ok(())
                }
            }
            'i' => {
                if self.case_insensitive {
                    Err(Error::InvalidRegEx(
                        pos,
                        "duplicate case insensitive flag".to_string(),
                    ))
                } else {
                    self.case_insensitive = true;
                    Ok(())
                }
            }
            'm' => {
                if self.multi_line {
                    Err(Error::InvalidRegEx(
                        pos,
                        "duplicate multi line flag".to_string(),
                    ))
                } else {
                    self.multi_line = true;
                    Ok(())
                }
            }
            's' => {
                if self.dot_matches_new_line {
                    Err(Error::InvalidRegEx(
                        pos,
                        "duplicate dot matches new line flag".to_string(),
                    ))
                } else {
                    self.dot_matches_new_line = true;
                    Ok(())
                }
            }
            'u' => {
                if self.unicode {
                    Err(Error::InvalidRegEx(
                        pos,
                        "duplicate unicode flag".to_string(),
                    ))
                } else {
                    self.unicode = true;
                    Ok(())
                }
            }
            'y' => {
                if self.sticky {
                    Err(Error::InvalidRegEx(
                        pos,
                        "duplicate sticky flag".to_string(),
                    ))
                } else {
                    self.sticky = true;
                    Ok(())
                }
            }
            _ => Err(Error::InvalidRegEx(
                pos,
                format!("{:?} is not a valid regex flag", c),
            )),
        }
    }
}
