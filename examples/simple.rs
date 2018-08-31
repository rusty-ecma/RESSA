extern crate resp;
use resp::*;
fn main() {
    let js = include_str!("simple.js");
    let mut p = Parser::new(js).unwrap();
    let script = p.parse_script().unwrap();
    println!("{:#?}", script);
}