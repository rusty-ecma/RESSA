#![cfg(test)]
use env_logger;
use ressa::Parser;

mod libs_common;

use libs_common::Lib;

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
fn vue_esm() {
    let js = ::std::fs::read_to_string("node_modules/vue/dist/vue.esm.js").unwrap();
    run_test("vue_module", js, String::new());
}

#[test]
fn jquery() {
    let (normal, min) = get_js(Lib::Jquery).expect("Unable to get jquery js");
    run_test("jquery", normal, min);
}

#[test]
fn moment() {
    let (normal, min) = get_js(Lib::Moment).expect("Unable to get moment js");
    run_test("moment", normal, min);
}

#[test]
fn dexie() {
    let (normal, min) = get_js(Lib::Dexie).expect("Unable to get dexie js");
    run_test("dexie", normal, min);
}

fn run_test(name: &str, normal: String, min: String) {
    let _ = env_logger::builder().is_test(true).try_init().ok();
    println!("parsing: {} chars", min.len());
    let mut p = Parser::builder()
        .js(&normal)
        .module(name.contains("module"))
        .build()
        .expect(&format!("Unable to create {} parser", name));
    let r = p.parse();
    handle_result(r, name);
    let mut p = Parser::builder()
        .js(&normal)
        .module(name.contains("module"))
        .build()
        .expect(&format!("Unable to create {}.min", name));
    let r = p.parse();
    handle_result(r, &format!("{}.min", name));
}

fn handle_result<'a>(
    result: Result<resast::Program<'a>, ressa::Error>,
    name: &str,
) -> resast::Program<'a> {
    match result {
        Ok(result) => result,
        Err(e) => panic!("Unable to parse {0}\n{1}\n{1:?}", name, e),
    }
}

fn get_js(l: Lib) -> Result<(String, String), ::std::io::Error> {
    Ok((get_normal_js(l)?, get_min_js(l)?))
}

fn get_normal_js(l: Lib) -> Result<String, ::std::io::Error> {
    libs_common::get_js_file(l.path())
}

fn get_min_js(l: Lib) -> Result<String, ::std::io::Error> {
    if let Some(p) = l.min_path() {
        libs_common::get_js_file(&p)
    } else {
        Err(::std::io::Error::new(
            ::std::io::ErrorKind::NotFound,
            format!("No min path for lib: {:?}", l),
        ))
    }
}
