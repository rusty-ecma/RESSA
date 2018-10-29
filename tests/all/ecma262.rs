#![cfg(test)]
use super::{get_js_file, EverythingVersion, Lib};
use env_logger;
use ressa::{node::ProgramPart, Builder, Parser};

#[test]
fn es5() {
    let _ = env_logger::try_init();
    let path = Lib::Everything(EverythingVersion::Es5).path();
    let js = get_js_file(&path).expect(&format!("Faield to get {:?}", path));
    let _res: Vec<ProgramPart> = Parser::new(&js)
        .expect("Failed to create parser")
        .map(|i| match i {
            Ok(i) => i,
            Err(e) => panic!("Error parsing {:?}\n{}", path, e),
        }).collect();
}

#[test]
fn es2015_script() {
    let _ = env_logger::try_init();
    let path = Lib::Everything(EverythingVersion::Es2015Script).path();
    let js = get_js_file(&path).expect(&format!("Faield to get {:?}", path));
    let mut p = Parser::new(&js).expect("Failed to create parser");
    let mut res = vec![];
    while let Some(item) = p.next() {
        let item = item.unwrap();
        res.push(item);
    }
}

#[test]
fn es2015_module() {
    let _ = env_logger::try_init();
    let path = Lib::Everything(EverythingVersion::Es2015Module).path();
    let js = get_js_file(&path).expect(&format!("Faield to get {:?}", path));
    let p = Builder::new()
        .module(true)
        .js(js)
        .build()
        .expect("Failed to create parser");
    let _res: Vec<ProgramPart> = p
        .map(|i| match i {
            Ok(i) => i,
            Err(e) => panic!("Error parsing {:?}\n{}", path, e),
        }).collect();
    // only one default export is allowed so these must be run ad-hoc
    let js_list = vec![
        "export default function (){}",
        "export default function* i16(){}",
        "export default function* (){}",
        "export default class i17 {}",
        "export default class i18 extends i19 {}",
        "export default class {}",
        "export default x = 0;",
        "export default 0;",
        "export default (0, 1);",
    ];
    for export in js_list {
        let mut b = Builder::new();
        let p = b
            .module(true)
            .js(export)
            .build()
            .expect("Failed to create parser");
        let _res: Vec<ProgramPart> = p
            .map(|i| match i {
                Ok(i) => i,
                Err(e) => panic!("Error parsing {}\n{}", export, e),
            }).collect();
    }
}
