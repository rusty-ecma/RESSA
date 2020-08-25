#![feature(prelude_import)]
#[prelude_import]
use std::prelude::v1::*;
#[macro_use]
extern crate std;
use in_and_out_logger::log_in_and_out;
use log::trace;
fn main() {
    {
        ::std::io::_print(::core::fmt::Arguments::new_v1(
            &["-> ", "\n"],
            &match (&"main",) {
                (arg0,) => [::core::fmt::ArgumentV1::new(
                    arg0,
                    ::core::fmt::Display::fmt,
                )],
            },
        ));
    };
    let mut f = move || recursive_one(0);
    let ret = f();
    {
        ::std::io::_print(::core::fmt::Arguments::new_v1(
            &["<- ", "\n"],
            &match (&"main",) {
                (arg0,) => [::core::fmt::ArgumentV1::new(
                    arg0,
                    ::core::fmt::Display::fmt,
                )],
            },
        ));
    };
    ret
}
fn recursive_one(n: u32) {
    {
        ::std::io::_print(::core::fmt::Arguments::new_v1(
            &["-> ", "\n"],
            &match (&"recursive_one",) {
                (arg0,) => [::core::fmt::ArgumentV1::new(
                    arg0,
                    ::core::fmt::Display::fmt,
                )],
            },
        ));
    };
    let mut f = move || {
        if n > 100 {
            return;
        }
        recursive_two(n + 1)
    };
    let ret = f();
    {
        ::std::io::_print(::core::fmt::Arguments::new_v1(
            &["<- ", "\n"],
            &match (&"recursive_one",) {
                (arg0,) => [::core::fmt::ArgumentV1::new(
                    arg0,
                    ::core::fmt::Display::fmt,
                )],
            },
        ));
    };
    ret
}
pub fn recursive_two(n: u32) {
    {
        ::std::io::_print(::core::fmt::Arguments::new_v1(
            &["-> ", "\n"],
            &match (&"recursive_two",) {
                (arg0,) => [::core::fmt::ArgumentV1::new(
                    arg0,
                    ::core::fmt::Display::fmt,
                )],
            },
        ));
    };
    let mut f = move || recursive_one(n + 1);
    let ret = f();
    {
        ::std::io::_print(::core::fmt::Arguments::new_v1(
            &["<- ", "\n"],
            &match (&"recursive_two",) {
                (arg0,) => [::core::fmt::ArgumentV1::new(
                    arg0,
                    ::core::fmt::Display::fmt,
                )],
            },
        ));
    };
    ret
}
