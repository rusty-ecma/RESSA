#![cfg(test)]
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
#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate criterion;

use std::{fs::read_to_string, path::PathBuf};

use criterion::{black_box, Criterion};
use ressa::Parser;

lazy_static! {
    static ref NG: String = get_js(Lib::Angular).unwrap();
    static ref NG_MIN: String = get_min_js(Lib::Angular).unwrap();
    static ref JQ: String = get_js(Lib::Jquery).unwrap();
    static ref JQ_MIN: String = get_min_js(Lib::Jquery).unwrap();
    static ref REACT: String = get_js(Lib::React).unwrap();
    static ref REACT_MIN: String = get_min_js(Lib::React).unwrap();
    static ref REACT_DOM: String = get_js(Lib::ReactDom).unwrap();
    static ref REACT_DOM_MIN: String = get_min_js(Lib::ReactDom).unwrap();
    static ref VUE: String = get_js(Lib::Vue).unwrap();
    static ref VUE_MIN: String = get_min_js(Lib::Vue).unwrap();
    static ref EV5: String = get_js(Lib::Es5).unwrap();
    static ref EV2015: String = get_js(Lib::Es2015S).unwrap();
    static ref EVMOD: String = get_js(Lib::Es2015M).unwrap();
}

fn angular1(c: &mut Criterion) {
    c.bench_function("angular1", |b| {
        b.iter(|| {
            let p = Parser::new(&NG).expect("Unable to crate new parser for angular.js");
            for i in p {
                black_box(i.unwrap());
            }
        });
    });
}

fn angular1_min(c: &mut Criterion) {
    c.bench_function("angular1_min", |b| {
        b.iter(|| {
            let p = Parser::new(&NG_MIN).expect("Unable to crate new parser for angular.min.js");
            for i in p {
                black_box(i.unwrap());
            }
        });
    });
}

fn jquery(c: &mut Criterion) {
    c.bench_function("jquery", |b| {
        b.iter(|| {
            let p = Parser::new(&JQ).expect("Unable to crate new parser for jquery");
            for i in p {
                black_box(i.unwrap());
            }
        });
    });
}

fn jquery_min(c: &mut Criterion) {
    c.bench_function("jquery_min", |b| {
        b.iter(|| {
            let p = Parser::new(&JQ_MIN).expect("Unable to crate new parser for jquery.min");
            for i in p {
                black_box(i.unwrap());
            }
        });
    });
}

fn react(c: &mut Criterion) {
    c.bench_function("react", |b| {
        b.iter(|| {
            let p = Parser::new(&REACT).expect("Unable to crate new parser for react.js");
            for i in p {
                black_box(i.unwrap());
            }
        });
    });
}

fn react_min(c: &mut Criterion) {
    c.bench_function("react_min", |b| {
        b.iter(|| {
            let p = Parser::new(&REACT_MIN).expect("Unable to crate new parser for react.min.js");
            for i in p {
                black_box(i.unwrap());
            }
        });
    });
}

fn react_dom(c: &mut Criterion) {
    c.bench_function("react_dom", |b| {
        b.iter(|| {
            let p = Parser::new(&REACT_DOM).expect("Unable to crate new parser for react-dom.js");
            for i in p {
                black_box(i.unwrap());
            }
        });
    });
}

fn react_dom_min(c: &mut Criterion) {
    c.bench_function("react_dom_min", |b| {
        b.iter(|| {
            let p = Parser::new(&REACT_DOM_MIN)
                .expect("Unable to crate new parser for react-dom.min.js");
            for i in p {
                black_box(i.unwrap());
            }
        });
    });
}

fn vue(c: &mut Criterion) {
    c.bench_function("vue", |b| {
        b.iter(|| {
            let p = Parser::new(&VUE).expect("Unable to crate new parser for vue.js");
            for i in p {
                black_box(i.unwrap());
            }
        });
    });
}

fn vue_min(c: &mut Criterion) {
    c.bench_function("vue_min", |b| {
        b.iter(|| {
            let p = Parser::new(&VUE_MIN).expect("Unable to crate new parser for vue.min.js");
            for i in p {
                black_box(i.unwrap());
            }
        });
    });
}

fn es5(c: &mut Criterion) {
    c.bench_function("es5", |b| {
        b.iter(|| {
            let p = Parser::new(&EV5).expect("Unable to crate new parser for es5.js");
            for i in p {
                black_box(i.unwrap());
            }
        });
    });
}

fn es2015(c: &mut Criterion) {
    c.bench_function("es2015", |b| {
        b.iter(|| {
            let p = Parser::new(&EV2015).expect("Unable to crate new parser for es2015-script.js");
            for i in p {
                black_box(i.unwrap());
            }
        });
    });
}

fn es_module(c: &mut Criterion) {
    c.bench_function("es_module", |b| {
        b.iter(|| {
            let p = Parser::builder()
                .js(&EVMOD)
                .module(true)
                .build()
                .expect("Unable to crate new parser for es2015-module.js");
            for i in p {
                black_box(i.unwrap());
            }
        });
    });
}

fn npm_install() -> Result<(), ::std::io::Error> {
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
    Es5,
    Es2015S,
    Es2015M,
}

impl Lib {
    pub fn path(&self) -> String {
        match self {
            &Lib::Jquery => "node_modules/jquery/dist/jquery.js".into(),
            &Lib::Angular => "node_modules/angular/angular.js".into(),
            &Lib::React => "node_modules/react/umd/react.development.js".into(),
            &Lib::ReactDom => "node_modules/react-dom/umd/react-dom.development.js".into(),
            &Lib::Vue => "node_modules/vue/dist/vue.js".into(),
            Lib::Es5 => "node_modules/everything.js/es5.js".into(),
            Lib::Es2015S => "node_modules/everything.js/es2015-script.js".into(),
            Lib::Es2015M => "node_modules/everything.js/es2015-module.js".into(),
        }
    }

    pub fn min_path(&self) -> String {
        match self {
            &Lib::Jquery => "node_modules/jquery/dist/jquery.min.js".into(),
            &Lib::Angular => "node_modules/angular/angular.min.js".into(),
            &Lib::React => "node_modules/react/umd/react.production.min.js".into(),
            &Lib::ReactDom => "node_modules/react-dom/umd/react-dom.production.min.js".into(),
            &Lib::Vue => "node_modules/vue/dist/vue.min.js".into(),
            _ => unreachable!(),
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

criterion_group!(
    benches,
    angular1,
    angular1_min,
    jquery,
    jquery_min,
    react,
    react_min,
    react_dom,
    react_dom_min,
    vue,
    vue_min,
    es5,
    es2015,
    es_module
);
criterion_main!(benches);
