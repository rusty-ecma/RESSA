extern crate ressa;

use ressa::Parser;

fn main() {
    let _ = env_logger::try_init();
    let js = include_str!("../node_modules/angular/angular.js");
    for i in 0..100 {
        log::debug!("starting pass {}", i);
        for item in Parser::new(js).unwrap() {
            let unwrapped = item.unwrap();
            ::std::mem::forget(unwrapped);
        }
        log::debug!("ended pass {}", i);
    }
}
