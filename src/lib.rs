extern crate ress;

use ress::{Scanner, Item, Keyword};
pub mod node;
use node::{Node, Position};
pub struct Parser {
    context: Context,
    scanner: Scanner,
    lines: Vec<Line>,
    parsed: Vec<node::Node>,
}
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Line {
    start: usize, 
    end: usize,
}

struct Context {
    
}

impl Default for Context {
    fn default() -> Self {
        Context {
            is_assignment_target: false,
            is_binding_element: false,
            in_function_body: false,
            in_iteration: false,
            in_switch: false,
            strict: false,
        }
    }
}
fn get_lines(text: &str) -> Vec<Line> {
    let mut line_start = 0;
    let mut ret: Vec<Line> = text.chars().enumerate().filter_map(|(i, c)| {
        match c {
        '\r' => {
            // look ahead 1 char to see if it is a newline pair
            // if so, don't include it, it will get included in the next
            // iteration
            if let Some(next) = text.get(i..i+2) {
                if next == "\r\n" {
                    None
                } else {
                    let ret = Line {
                        start: line_start,
                        end: i,
                    };
                    line_start = i + 1;
                    Some(ret)
                }
            } else {
                None
            }
        },
        '\n' | '\u{2028}' | '\u{2029}' => {
            let ret = Line {
                start: line_start,
                end: i,
            };
            line_start = i + 1;
            Some(ret)
        },
        _ => None,
    }}).collect();

    ret.push(Line {
        start: line_start,
        end: text.len() - 1,
    });
    ret
}
impl Parser {
    pub fn new(text: &str) -> Parser {
        let mut lines = get_lines(text);
        Parser {
            scanner: Scanner::new(text),
            context: Context::default(),
            parsed: vec![],
            lines,
        }
    }
    pub fn parse(&mut self) {
        loop {
            if let Some(ref item) = self.scanner.next() {
                let position = self.get_line_position(item);
                match item.token {
                    Token::Keyword(ref keyword) => start_with_keyword(keyword),
                    _ => ()
                }
            } else {
                break;
            }
        }
    }

    pub fn start_with_keyword(start: &ress::Keyword) {
        match start {
            &Keyword::Export => (),
            &Keyword::Import => (),
            // &Keyword::Const => (),
            &Keyword::Function => (),
            // &Keyword::Class => (),
            &Keyword::Let => (),
            &Keyword::Break => (),
            &Keyword::Continue => (),
            &Keyword::Debugger => (),
            &Keyword::Do => (),
            &Keyword::For => (),
            &Keyword::Function => (),
            &Keyword::If => (),
            &Keyword::Return => (),
            &Keyword::Switch => (),
            &Keyword::Throw => (),
            &Keyword::Try => (),
            &Keyword::Var => (),
            &Keyword::While => (),
            &Keyword::With => (),
            _ => (),
        }
    }

    fn get_line_position(&self, item: &Item) -> Position {
        let (idx, line) = if let Some((idx, line)) = self.lines.iter().enumerate().find(|(i, l)| l.end + 1 >= item.span.start) {
            (idx, line)
        } else {
            panic!("Unable to determine item's line number {:?}", item);
        };
        let column = item.span.start.saturating_sub(line.start);
        Position {
            line: idx+1,
            column,
        }
    }

}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn parse() {
        let js = "function() {\n\r\n\n}";
        let mut p = Parser::new(js);
        p.parse();
    }
}
