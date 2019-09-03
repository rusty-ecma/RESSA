extern crate ressa;
use ressa::*;
fn main() {
    let js = include_str!("simple.mjs");
    let mut p = Parser::builder().module(true).tolerant(false).js(js).build().unwrap();
    let module = p.parse().unwrap();
    println!("{:#?}", module);
}
