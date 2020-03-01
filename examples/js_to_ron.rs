extern crate ressa;
use ressa::*;

fn main() {
    let _ = env_logger::try_init();
    let mut args = ::std::env::args();
    let _ = args.next().unwrap();
    let path = args.next().expect("One argument required");
    let js = ::std::fs::read_to_string(path).expect("Failed to read path");
    let module = if let Some(flag) = args.next() {
        flag.ends_with("m") || flag.ends_with("module")
    } else {
        false
    };
    let mut p = Parser::builder().module(module).js(&js).build().unwrap();
    let ast = p.parse().unwrap();
    println!("{:#?}", ast);
}
