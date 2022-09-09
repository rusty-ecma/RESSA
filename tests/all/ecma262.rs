#![cfg(test)]
use super::{get_js_file, EverythingVersion, Lib};
use env_logger;

use ressa::Parser;
#[test]
fn es5() {
    let _ = env_logger::try_init();
    info!("ES5");
    let path = Lib::Everything(EverythingVersion::Es5).path();
    debug!("path: {:?}", path);
    let js = get_js_file(&path).unwrap_or_else(|e| panic!("Faield to get {:?}\n{}", path, e));
    let mut p = Parser::new(&js).expect("Failed to create parser");
    let tokens = p.parse().unwrap();
    insta::assert_debug_snapshot!(tokens);
}

#[test]
fn es2015_script() {
    let _ = env_logger::try_init();
    info!("ES2015 Script");
    let path = Lib::Everything(EverythingVersion::Es2015Script).path();
    let js = get_js_file(&path).expect(&format!("Failed to get {:?}", path));
    let mut p = Parser::new(&js).expect("Failed to create parser");
    let tokens = p.parse().unwrap();
    insta::assert_debug_snapshot!(tokens);
}

#[test]
fn es2015_module() {
    info!("ES2015 Module");
    let _ = env_logger::try_init();
    let path = Lib::Everything(EverythingVersion::Es2015Module).path();
    let js = get_js_file(&path).expect(&format!("Failed to get {:?}", path));
    let mut p = ressa::spanned::Parser::builder()
        .module(true)
        .js(&js)
        .build()
        .expect("Failed to create parser");
    let tokens = p.parse().unwrap();
    insta::assert_debug_snapshot!(tokens);
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
        let p = Parser::builder()
            .module(true)
            .js(export)
            .build()
            .expect("Failed to create parser");
        let res: Vec<_> = p
            .map(|i| match i {
                Ok(i) => i,
                Err(e) => panic!("Error parsing {}\n{}", export, e),
            })
            .collect();
        insta::assert_debug_snapshot!(res)
    }
}
