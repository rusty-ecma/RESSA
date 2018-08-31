#![feature(test)]
extern crate test;
extern crate resp;

use std::{
    fs::read_to_string,
    path::PathBuf,
};

use resp::Parser;
use test::{Bencher};
#[bench]
fn angular1(b: &mut Bencher) {
    if let Ok(js) = get_js(Lib::Angular) {
        let mut p = Parser::new(&js).expect("Unable to crate new parser for angular.js");
        b.iter(|| p.parse_script().expect("unable to parse angular1"));
    }
}
#[bench]
fn angular1_min(b: &mut Bencher) {
    if let Ok(js) = get_min_js(Lib::Angular) {
        let mut p = Parser::new(&js).expect("Unable to crate new parser for angular.js (min)");
        b.iter(|| p.parse_script().expect("unable to parse angular1_min"));
    }
}

#[bench]
fn jquery(b: &mut Bencher) {
    if let Ok(js) = get_js(Lib::Jquery) {
        let mut p = Parser::new(&js).expect("Unable to create new parser for jquery");
        b.iter(|| p.parse_script().expect("unable to parse jquery"));
    }
}
#[bench]
fn jquery_min(b: &mut Bencher) {
    if let Ok(js) = get_min_js(Lib::Jquery) {
        let mut p = Parser::new(&js).expect("Unable to create new parser for jquery (min)");
        b.iter(|| p.parse_script().expect("unable to parse jquery_min"));
    }
}

#[bench]
fn react(b: &mut Bencher) {
    if let Ok(js) = get_js(Lib::React) {
        let mut p = Parser::new(&js).expect("Unable to create new parser for react");
        b.iter(|| p.parse_script().expect("unable to parse react"));
    }
}
#[bench]
fn react_min(b: &mut Bencher) {
    if let Ok(js) = get_min_js(Lib::React) {
        let mut p = Parser::new(&js).expect("Unable to create new parser for react (min)");
        b.iter(|| p.parse_script().expect("unable to parse react_min"));
    }
}

#[bench]
fn react_dom(b: &mut Bencher) {
    if let Ok(js) = get_js(Lib::ReactDom) {
        let mut p = Parser::new(&js).expect("Unable to create new parser for react-dom");
        b.iter(|| p.parse_script().expect("unable to parse react_dom"));
    }
}
#[bench]
fn react_dom_min(b: &mut Bencher) {
    if let Ok(js) = get_min_js(Lib::ReactDom) {
        let mut p = Parser::new(&js).expect("Unable to create new parser for react-dom (min)");
        b.iter(|| p.parse_script().expect("unable to parse react_dom_min"));
    }
}

#[bench]
fn vue(b: &mut Bencher) {
    if let Ok(js) = get_js(Lib::Vue) {
        let mut p = Parser::new(&js).expect("Unable to create new parser for vue");
        b.iter(|| p.parse_script().expect("unable to parse vue"));
    }
}
#[bench]
fn vue_min(b: &mut Bencher) {
    if let Ok(js) = get_min_js(Lib::Vue) {
        let mut p = Parser::new(&js).expect("Unable to create new parser for vue (min)");
        b.iter(|| p.parse_script().expect("unable to parse vue_min"));
    }
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
            &Lib::Vue => "node_modules/vue/dist/vue.js".into(),
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