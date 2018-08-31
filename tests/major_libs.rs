
#![cfg(all(test, feature = "major_libs"))]
extern crate resp;
extern crate log;
extern crate env_logger;

use std::{
    fs::read_to_string,
    path::PathBuf,
};

use resp::Parser;
#[test]
fn angular1() {
    let (normal, min) = get_js(Lib::Angular).expect("Unable to get angular js");
    run_test("angular", normal, min);
}
#[test]
fn react_core() {
    let (normal, min) = get_js(Lib::React).expect("Unable to get react js");
    run_test("react", normal, min);
}

#[test]
fn react_dom() {
    let (normal, min) = get_js(Lib::ReactDom).expect("Unable to get react-dom js");
    run_test("react-dom", normal, min);
}

#[test]
fn vue() {
    let (normal, min) = get_js(Lib::Vue).expect("Unable to get vue js");
    run_test("vue", normal, min);
}

#[test]
fn jquery() {
    let (normal, min) = get_js(Lib::Jquery).expect("Unable to get jquery js");
    run_test("jquery", normal, min);
}

fn run_test(name: &str, normal: String, min: String) {
    let mut p = Parser::new(&normal).expect(&format!("Unable to create {} parser", name));
    let _result = p.parse_script().expect(&format!("Unable to parse {}", name));
    p = Parser::new(&min).expect(&format!("Unable to create react.min {}", name));
    let _result = p.parse_script().expect(&format!("Unable to parse {}.min", name));
}
#[derive(Clone, Copy)]
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

fn get_js(l: Lib) -> Result<(String, String), ::std::io::Error> {
    init_logging();
    Ok((get_normal_js(l)?, get_min_js(l)?))
}

fn get_normal_js(l: Lib) -> Result<String, ::std::io::Error> {
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

fn npm_install() -> Result<(), ::std::io::Error> {
    let mut c = ::std::process::Command::new("npm");
    c.arg("i");
    c.output()?;
    Ok(())
}

pub(crate) fn init_logging() {
    unsafe {
        if LOGGING {
            return;
        }
    }
    let mut b = env_logger::Builder::new();
    b.target(env_logger::Target::Stdout);

    if let Ok(args) = ::std::env::var("RUST_LOG") {
        b.parse(&args);
    }
    if let Ok(_) = b.try_init() {
        unsafe {
            LOGGING = true;
        }
    }
}

static mut LOGGING: bool = false;