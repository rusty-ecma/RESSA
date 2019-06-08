#![feature(test)]
//! This benchmarking suite is extremely naive
//! I use it internally to determine if I have
//! been able to make large impact performance
//! improvements in both this crate
//! and `ress`
//! If you want to full output please run
//! `node ./bencher.js` in the crate root
//! this will collect the results and
//! build a table that will be written to
//! benchmark.md
//! This will include information about
//! the parser overhead above the scanner
//! and a naive comparison against
//! [esprima](https://github.com/jquery/esprima)
extern crate ress;
extern crate ressa;
extern crate test;

use std::{fs::read_to_string, path::PathBuf};

use ress::refs::{RefItem as Item, RefScanner as Scanner};
use ressa::Parser;
use test::{black_box, Bencher};
#[bench]
fn angular1(b: &mut Bencher) {
    if let Ok(js) = get_js(Lib::Angular) {
        b.iter(|| {
            let mut p = Parser::new(&js).expect("Unable to crate new parser for angular.js");
            black_box(p.parse().expect("unable to parse angular1"));
        });
    }
}
#[bench]
fn angular1_scanner_only(b: &mut Bencher) {
    if let Ok(js) = get_js(Lib::Angular) {
        b.iter(|| {
            let s = Scanner::new(&js);
            let res: Vec<Item> = s.collect();
            black_box(res);
        });
    }
}
#[bench]
fn angular1_min(b: &mut Bencher) {
    if let Ok(js) = get_min_js(Lib::Angular) {
        b.iter(|| {
            let mut p = Parser::new(&js).expect("Unable to crate new parser for angular.js (min)");
            black_box(p.parse().expect("unable to parse angular1_min"));
        });
    }
}
#[bench]
fn angular1_min_scanner_only(b: &mut Bencher) {
    if let Ok(js) = get_min_js(Lib::Angular) {
        b.iter(|| {
            let s = Scanner::new(&js);
            let res: Vec<Item> = s.collect();
            black_box(res);
        });
    }
}
#[bench]
fn jquery(b: &mut Bencher) {
    if let Ok(js) = get_js(Lib::Jquery) {
        b.iter(|| {
            let mut p = Parser::new(&js).expect("Unable to create new parser for jquery");
            black_box(p.parse().expect("unable to parse jquery"));
        });
    }
}
#[bench]
fn jquery_scanner_only(b: &mut Bencher) {
    if let Ok(js) = get_js(Lib::Jquery) {
        b.iter(|| {
            let s = Scanner::new(&js);
            let res: Vec<Item> = s.collect();
            black_box(res);
        });
    }
}
#[bench]
fn jquery_min(b: &mut Bencher) {
    if let Ok(js) = get_min_js(Lib::Jquery) {
        b.iter(|| {
            let mut p = Parser::new(&js).expect("Unable to create new parser for jquery (min)");
            black_box(p.parse().expect("unable to parse jquery_min"));
        });
    }
}
#[bench]
fn jquery_min_scanner_only(b: &mut Bencher) {
    if let Ok(js) = get_min_js(Lib::Jquery) {
        b.iter(|| {
            let s = Scanner::new(&js);
            let res: Vec<Item> = s.collect();
            black_box(res);
        });
    }
}
#[bench]
fn react(b: &mut Bencher) {
    if let Ok(js) = get_js(Lib::React) {
        b.iter(|| {
            let mut p = Parser::new(&js).expect("Unable to create new parser for react");
            black_box(p.parse().expect("unable to parse react"));
        });
    }
}
#[bench]
fn react_scanner_only(b: &mut Bencher) {
    if let Ok(js) = get_js(Lib::React) {
        b.iter(|| {
            let s = Scanner::new(&js);
            let res: Vec<Item> = s.collect();
            black_box(res);
        });
    }
}
#[bench]
fn react_min(b: &mut Bencher) {
    if let Ok(js) = get_min_js(Lib::React) {
        b.iter(|| {
            let mut p = Parser::new(&js).expect("Unable to create new parser for react (min)");
            black_box(p.parse().expect("unable to parse react_min"));
        });
    }
}

#[bench]
fn react_min_scanner_only(b: &mut Bencher) {
    if let Ok(js) = get_min_js(Lib::React) {
        b.iter(|| {
            let s = Scanner::new(&js);
            let res: Vec<Item> = s.collect();
            black_box(res);
        });
    }
}

#[bench]
fn react_dom(b: &mut Bencher) {
    if let Ok(js) = get_js(Lib::ReactDom) {
        b.iter(|| {
            let mut p = Parser::new(&js).expect("Unable to create new parser for react-dom");
            black_box(p.parse().expect("unable to parse react_dom"));
        });
    }
}

#[bench]
fn react_dom_scanner_only(b: &mut Bencher) {
    if let Ok(js) = get_js(Lib::ReactDom) {
        b.iter(|| {
            let s = Scanner::new(&js);
            let res: Vec<Item> = s.collect();
            black_box(res);
        });
    }
}

#[bench]
fn react_dom_min(b: &mut Bencher) {
    if let Ok(js) = get_min_js(Lib::ReactDom) {
        b.iter(|| {
            let mut p = Parser::new(&js).expect("Unable to create new parser for react-dom (min)");
            black_box(p.parse().expect("unable to parse react_dom_min"));
        });
    }
}

#[bench]
fn react_dom_min_scanner_only(b: &mut Bencher) {
    if let Ok(js) = get_min_js(Lib::ReactDom) {
        b.iter(|| {
            let s = Scanner::new(&js);
            let res: Vec<Item> = s.collect();
            black_box(res);
        });
    }
}

#[bench]
fn vue(b: &mut Bencher) {
    if let Ok(js) = get_js(Lib::Vue) {
        b.iter(|| {
            let mut p = Parser::new(&js).expect("Unable to create new parser for vue");
            black_box(p.parse().expect("unable to parse vue"));
        });
    }
}

#[bench]
fn vue_scanner_only(b: &mut Bencher) {
    if let Ok(js) = get_js(Lib::Vue) {
        b.iter(|| {
            let s = Scanner::new(&js);
            let res: Vec<Item> = s.collect();
            black_box(res);
        });
    }
}

#[bench]
fn vue_min(b: &mut Bencher) {
    if let Ok(js) = get_min_js(Lib::Vue) {
        b.iter(|| {
            let mut p = Parser::new(&js).expect("Unable to create new parser for vue (min)");
            black_box(p.parse().expect("unable to parse vue_min"));
        });
    }
}

#[bench]
fn vue_min_scanner_only(b: &mut Bencher) {
    if let Ok(js) = get_min_js(Lib::Vue) {
        b.iter(|| {
            let s = Scanner::new(&js);
            let res: Vec<Item> = s.collect();
            black_box(res);
        });
    }
}

fn npm_install() -> Result<(), ::std::io::Error> {
    eprintln!("Downloading required js dependencies");
    let mut c = ::std::process::Command::new("npm");
    c.arg("i");
    c.output()?;
    Ok(())
}

enum Lib {
    Jquery,
    Angular,
    React,
    ReactDom,
    Vue,
}

impl Lib {
    pub fn path(&self) -> String {
        match self {
            &Lib::Jquery => "node_modules/jquery/dist/jquery.js".into(),
            &Lib::Angular => "node_modules/angular/angular.js".into(),
            &Lib::React => "node_modules/react/umd/react.development.js".into(),
            &Lib::ReactDom => "node_modules/react-dom/umd/react-dom.development.js".into(),
            &Lib::Vue => "node_modules/vue/dist/vue.js".into(),
        }
    }

    pub fn min_path(&self) -> String {
        match self {
            &Lib::Jquery => "node_modules/jquery/dist/jquery.min.js".into(),
            &Lib::Angular => "node_modules/angular/angular.min.js".into(),
            &Lib::React => "node_modules/react/umd/react.production.min.js".into(),
            &Lib::ReactDom => "node_modules/react-dom/umd/react-dom.production.min.js".into(),
            &Lib::Vue => "node_modules/vue/dist/vue.min.js".into(),
        }
    }
}

fn get_js(l: Lib) -> Result<String, ::std::io::Error> {
    let path = PathBuf::from(l.path());
    if !path.exists() {
        npm_install()?;
        if !path.exists() {
            panic!("npm install failed to make {} available", path.display());
        }
    }
    read_to_string(path)
}

fn get_min_js(l: Lib) -> Result<String, ::std::io::Error> {
    let path = PathBuf::from(l.min_path());
    if !path.exists() {
        npm_install()?;
        if !path.exists() {
            panic!("npm install failed to make {} available", path.display());
        }
    }
    read_to_string(path)
}
