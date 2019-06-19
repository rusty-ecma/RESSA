extern crate ressa;

use ressa::Parser;

fn main() {
    let js = include_str!("../node_modules/angular/angular.js");
    for _item in Parser::new(js).unwrap() {}
}
