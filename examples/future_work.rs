use std::{fs::read_to_string, path::PathBuf};

use docopt::Docopt;
use ress::prelude::*;
use ressa::{CommentHandler, Parser, Span};
use serde::Deserialize;
use term_painter::{Color, ToStyle};
use walkdir::WalkDir;

static USAGE: &str = "
Future Work
A command line tool for evaluating TODO and FIXME comments
in your javascript files

Usage:
    future-work [options]
    future-work -h | --help

Options:
    -h --help       show this screen
    -d --dir PATH   Evaluate all .js files in a directory
    -f --file PATH  Evaluate only one file
    --no-color      Do not color output
";
#[derive(Deserialize)]
struct Args {
    flag_no_color: bool,
    flag_dir: Option<PathBuf>,
    flag_file: Option<PathBuf>,
}

fn main() -> Result<(), Box<dyn ::std::error::Error>> {
    let args: Args = Docopt::new(USAGE)
        .and_then(|o| o.deserialize())
        .unwrap_or_else(|e| e.exit());
    if args.flag_dir.is_none() && args.flag_file.is_none() {
        println!("{}", USAGE);
        return Ok(());
    }
    print_header(!args.flag_no_color);
    if let Some(dir) = args.flag_dir {
        for entry in WalkDir::new(&dir) {
            let entry = entry?;
            if entry.file_name().to_string_lossy().ends_with(".js") {
                parse_file(&entry.into_path(), !args.flag_no_color)?;
            }
        }
    }
    if let Some(f) = args.flag_file {
        parse_file(&f, !args.flag_no_color)?;
    }
    Ok(())
}

fn parse_file(path: &PathBuf, color: bool) -> Result<(), Box<dyn ::std::error::Error>> {
    print_path(path, color);
    let js = get_js(path)?;
    let lines = find_lines(&js);
    let ch = WorkHandler {
        color,
        lines,
        counter: 0,
    };
    let mut p = Parser::builder().js(&js).with_comment_handler(ch)?;
    let _ = p.parse()?;
    Ok(())
}

fn get_js(path: &PathBuf) -> Result<String, Box<dyn ::std::error::Error>> {
    let text = read_to_string(&path)?;
    if text.starts_with("#!") {
        let mut lines = text.lines();
        let _ = lines.next();
        Ok(lines.collect::<Vec<&str>>().join("\n"))
    } else {
        Ok(text)
    }
}

fn print_header(color: bool) {
    let line1 = "▐▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▚▌";
    let line2 = "    FUTURE WORK     ";
    if color {
        println!("{}", Color::BrightCyan.bold().paint(line1));
        println!("{}", Color::BrightCyan.bold().paint(line2));
        println!("{}", Color::BrightCyan.bold().paint(line1));
    } else {
        println!("{}", line1);
        println!("{}", line2);
        println!("{}", line1);
    }
}

fn print_path(path: &PathBuf, color: bool) {
    let line1 = "═══════════════════";
    let line2 = format!("{}", path.display());
    if color {
        println!("{}", Color::BrightCyan.bold().paint(line1));
        println!("{}", Color::BrightCyan.bold().paint(&line2));
        println!("{}", Color::BrightCyan.bold().paint(line1));
    } else {
        println!("{}", line1);
        println!("{}", line2);
        println!("{}", line1);
    }
}

enum FutureWork {
    FixMe(Span, String),
    ToDo(Span, String),
}

struct WorkHandler {
    color: bool,
    lines: Vec<(usize, usize)>,
    counter: usize,
}

impl Drop for WorkHandler {
    fn drop(&mut self) {
        if self.counter == 0 {
            let msg = "No future work here!";
            if self.color {
                println!("{}", Color::Green.bold().paint(msg));
            } else {
                println!("{}", msg);
            }
        }
    }
}

impl WorkHandler {
    fn report_work(&self, work: FutureWork) {
        match work {
            FutureWork::FixMe(span, msg) => {
                let line = self.get_line_number(&span);
                if self.color {
                    println!("{}: ({}) {}", Color::Red.bold().paint("FIXME"), line, msg);
                } else {
                    println!("{}: ({}) {}", "FIXME", line, msg);
                }
            }
            FutureWork::ToDo(span, msg) => {
                let line = self.get_line_number(&span);
                if self.color {
                    println!(
                        " {}: ({}) {}",
                        Color::BrightCyan.bold().paint("TODO"),
                        line,
                        msg
                    );
                } else {
                    println!(" {}: ({}) {}", "TODO", line, msg);
                }
            }
        }
    }

    fn get_line_number(&self, span: &Span) -> usize {
        fn find_line(lines: &[(usize, usize)], index: usize, span: &Span) -> usize {
            let current_len = lines.len();
            if current_len == 1 {
                index + 1
            } else {
                let half = current_len >> 1;
                if lines[half - 1].1 + 1 >= span.start {
                    find_line(&lines[..half], index, span)
                } else {
                    find_line(&lines[half..], index + half, span)
                }
            }
        }
        find_line(&self.lines, 0, span)
    }
}

impl<'a> CommentHandler<'a> for WorkHandler {
    fn handle_comment(&mut self, comment: Item<&str>) {
        match comment.token {
            Token::Comment(c) => {
                if c.is_multi_line() {
                    let mut counter = 0;
                    for line in c.content.lines() {
                        let span = Span::new(comment.span.start + counter, comment.span.end);
                        if let Some(work) = parse_line(span, line) {
                            self.counter += 1;
                            self.report_work(work);
                        }
                        counter += line.len();
                    }
                } else {
                    if let Some(work) = parse_line(comment.span, &c.content) {
                        self.counter += 1;
                        self.report_work(work);
                    }
                }
            }
            _ => (),
        }
    }
}

fn parse_line(span: Span, line: &str) -> Option<FutureWork> {
    let (todo, prefix) = if line.trim().starts_with("TODO") {
        (true, "TODO")
    } else if line.trim().starts_with("FIXME") {
        (false, "FIXME")
    } else {
        return None;
    };
    let work = line.trim_start_matches(prefix);
    let work = work.trim_start_matches(":");
    let work = work.trim_start().to_owned();
    Some(if todo {
        FutureWork::ToDo(span, work)
    } else {
        FutureWork::FixMe(span, work)
    })
}

fn find_lines(text: &str) -> Vec<(usize, usize)> {
    // Obviously we will start at 0
    let mut line_start = 0;
    // This is the byte position, not the character
    // position to account for multi byte chars
    let mut byte_position = 0;
    // loop over the characters
    let mut ret: Vec<(usize, usize)> = text
        .chars()
        .filter_map(|c| {
            let ret = match c {
                '\r' => {
                    // look ahead 1 char to see if it is a newline pair
                    // if so, don't include it, it will get included in the next
                    // iteration
                    if let Some(next) = text.get(byte_position..byte_position + 2) {
                        if next == "\r\n" {
                            None
                        } else {
                            let ret = (line_start, byte_position);
                            line_start = byte_position + 1;
                            Some(ret)
                        }
                    } else {
                        None
                    }
                }
                '\n' => {
                    let ret = (line_start, byte_position);
                    line_start = byte_position + 1;
                    Some(ret)
                }
                '\u{2028}' | '\u{2029}' => {
                    //These new line characters are both 3 bytes in length
                    //that means we need to include this calculation in both
                    // the end field and the next line start
                    let ret = (line_start, byte_position + 2);
                    line_start = byte_position + 3;
                    Some(ret)
                }
                _ => None,
            };
            // Since chars can be up to 4 bytes wide, we
            // want to move the full width of the current byte
            byte_position += c.len_utf8();
            ret
        })
        .collect();
    // Since we shouldn't have only a new line char at EOF,
    // This will capture the last line of the text
    ret.push((line_start, text.len().saturating_sub(1)));
    ret
}
