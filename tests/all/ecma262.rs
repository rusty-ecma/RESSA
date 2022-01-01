#![cfg(test)]
use super::{get_js_file, EverythingVersion, Lib};
use env_logger;
use resast::spanned::SourceLocation;

use crate::es_tokens;
use ressa::Parser;
#[test]
fn es5() {
    let _ = env_logger::try_init();
    info!("ES5");
    let path = Lib::Everything(EverythingVersion::Es5).path();
    println!("path: {:?}", path);
    let js = get_js_file(&path).unwrap_or_else(|e| panic!("Faield to get {:?}\n{}", path, e));
    let mut p = Parser::new(&js).expect("Failed to create parser");
    let mut tokens = es_tokens::ES5.iter();
    let mut i = 0;
    let mut last_position = p.next_position();
    while let Some(ref item) = p.next() {
        if let Some(part) = tokens.next() {
            let item = match item {
                Ok(i) => i,
                Err(e) => {
                    let path = if let Some(pos) = e.position() {
                        format!("{}:{}:{}", path, pos.line, pos.column)
                    } else {
                        path
                    };
                    panic!(
                    "Error parsing {:?}\n{}",
                    path,
                    super::format_error(&js, e)
                )},
            };
            if item != part {
                panic!(
                    "Error, part {} doesn't match \n{:?}\n{:?}\nnext start: line: {}, column: {}\n{}",
                    i, item, part, last_position.start.line, last_position.start.column,
                    super::hilight_position(&js, &last_position).unwrap_or_else(String::new)
                )
            }
        }
        i += 1;
        last_position = p.next_position();
    }
}

#[test]
fn es2015_script() {
    let _ = env_logger::try_init();
    info!("ES2015 Script");
    let path = Lib::Everything(EverythingVersion::Es2015Script).path();
    let js = get_js_file(&path).expect(&format!("Failed to get {:?}", path));
    let mut p = Parser::new(&js).expect("Failed to create parser");
    let mut tokens = es_tokens::ES2015.iter();
    let mut i = 0;
    while let Some(ref item) = p.next() {
        if let Some(part) = tokens.next() {
            let item = match item {
                Ok(i) => i,
                Err(e) => {
                    let path = if let Some(pos) = e.position() {
                        format!("{}:{}:{}", path, pos.line, pos.column)
                    } else {
                        path
                    };
                    panic!("Error parsing {:?}\n{}", path, e)
                },
            };
            if item != part {
                let pos = p.next_position();
                let _ = ::std::fs::write("1.parsed.out", format!("{:#?}", item));
                let _ = ::std::fs::write("2.expected.out", format!("{:#?}", part));
                panic!(
                    "Error, part {} does't match from around {}:{}:{} \n{:?}\n{:?}\n",
                    i, path, pos.start.line, pos.start.column, item, part,
                )
            }
        }
        i += 1;
    }
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
    let mut tokens = es_tokens::ESMOD.iter();
    let mut i = 0;
    while let Some(ref item) = p.next() {
        if let Some(part) = tokens.next() {
            let item = match item {
                Ok(i) => i,
                Err(e) => {
                    let path = if let Some(pos) = e.position() {
                        format!("{}:{}:{}", path, pos.line, pos.column)
                    } else {
                        path
                    };
                    panic!("Error parsing {:?}\n{}", path, super::format_error(&js, &e))
                },
            };
            let simple = item.clone();
            let simple: resast::ProgramPart = simple.into();
            if &simple != part {
                use resast::spanned::Node;
                let _ = ::std::fs::write("1.parsed.out", format!("{:#?}", simple));
                let _ = ::std::fs::write("2.expected.out", format!("{:#?}", part));
                let loc = item.loc();
                let path = format!("{}:{}:{}", path, loc.start.line, loc.start.column);
                let end = format!("{}:{}", loc.end.line, loc.end.column);
                panic!(
                    "{:?}\n{:?}\nError, part {} does't match from {} to {} \n",
                    simple, part, i, path, end
                )
            }
        }
        i += 1;
    }
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
        let _res: Vec<_> = p
            .map(|i| match i {
                Ok(i) => i,
                Err(e) => panic!("Error parsing {}\n{}", export, e),
            })
            .collect();
    }
}
