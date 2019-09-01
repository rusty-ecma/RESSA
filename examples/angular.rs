extern crate ressa;

use ressa::Parser;

fn main() {
    let js = include_str!("../node_modules/angular/angular.js");
    for _ in 0..1000 {
        for item in Parser::new(js).unwrap() {
            let unwrapped = item.unwrap();
            ::std::mem::forget(unwrapped);
        }
    }
}
