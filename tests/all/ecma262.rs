#![cfg(test)]
use resp::{Parser, Builder, node::ProgramPart,};
use super::{Lib, EverythingVersion, get_js_file};
use env_logger;

#[test]
fn es5() {
    let _ = env_logger::try_init();
    let path = Lib::Everything(EverythingVersion::Es5).path();
    let js = get_js_file(&path).expect(&format!("Faield to get {:?}", path));
    let res: Vec<ProgramPart> = Parser::new(&js).expect("Failed to create parser").map(|i| match i {
        Ok(i) => {
            println!("{:?}", i);
            i
        },
        Err(e) => panic!("Error parsing {:?}\n{}", path, e),
    }).collect();
    println!("{:#?}", res);
}

#[test]
fn es2015_script() {
    let _ = env_logger::try_init();
    println!("es2015_script");
    let path = Lib::Everything(EverythingVersion::Es2015Script).path();
    let raw = get_js_file(&path).expect(&format!("Faield to get {:?}", path));
    let mut lines = raw.lines();
    let _ = lines.next();
    let _ = lines.next();
    let js: String = lines.map(|l| l.to_owned() + "\n").collect();
    let mut p = Parser::new(&js).expect("Failed to create parser");
    let mut i = 0;
    let mut res = vec![];
    while let Some(item) = p.next() {
        let item = item.unwrap();
        println!("{}: {:?}",i, item);
        res.push(item);
        i += 1;
    }
    println!("{:#?}", res);
}

#[test]
fn es2015_module() {
    let _ = env_logger::try_init();
    let path = Lib::Everything(EverythingVersion::Es2015Module).path();
    let js = get_js_file(&path).expect(&format!("Faield to get {:?}", path));
    let p = Builder::new().module(true).js(js).build().expect("Failed to create parser");
    let res: Vec<ProgramPart> = p.map(|i| match i {
        Ok(i) => i,
        Err(e) => panic!("Error parsing {:?}\n{}", path, e),
    }).collect();
    println!("{:#?}", res);
}