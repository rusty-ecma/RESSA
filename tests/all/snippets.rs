use resast::prelude::*;
use ressa::*;
use std::borrow::Cow;
#[test]
fn doc1() {
    let js = "function helloWorld() { alert('Hello world'); }";
    let p = Parser::new(&js).unwrap();
    let f = ProgramPart::decl(Decl::Func(Func {
        id: Some(Ident::from("helloWorld")),
        params: vec![],
        body: FuncBody(vec![ProgramPart::Stmt(Stmt::Expr(Expr::Call(CallExpr {
            callee: Box::new(Expr::ident_from("alert")),
            arguments: vec![Expr::Lit(Lit::single_string_from("Hello world"))],
        })))]),
        generator: false,
        is_async: false,
    }));
    for part in p {
        assert_eq!(part.unwrap(), f);
    }
}

#[test]
fn readme_iter_example() {
    let js = "function helloWorld() { alert('Hello world'); }";
    let p = Parser::new(&js).unwrap();
    let f = ProgramPart::decl(Decl::Func(Func {
        id: Some(Ident::from("helloWorld")),
        params: vec![],
        body: FuncBody(vec![ProgramPart::Stmt(Stmt::Expr(Expr::Call(CallExpr {
            callee: Box::new(Expr::ident_from("alert")),
            arguments: vec![Expr::Lit(Lit::String(StringLit::Single(Cow::Owned(
                "Hello world".to_string(),
            ))))],
        })))]),
        generator: false,
        is_async: false,
    }));
    for part in p {
        assert_eq!(part.unwrap(), f);
    }
}

#[test]
fn arrow_func_args() {
    let js = "(a, b = 0, [c,, d = 0, ...e], {f, g: h, i = 0, i: j = 0}, ...k) => {;};";
    let mut parser = Parser::new(&js).unwrap();
    let _parsed = parser.parse().unwrap();
}

#[test]
fn destructuring_default() {
    let _ = env_logger::try_init();
    let js = "[a = {y: 2}, a.x = 1] = [];";
    let mut parser = Parser::new(js).expect("failed to create parser");
    parser.parse().expect("failed to parser js");
}
#[test]
fn destructuring_obj() {
    let _ = env_logger::try_init();
    let js = "0, [...{} [throwlhs()]] = iterable;";
    let mut parser = Parser::new(js).expect("failed to create parser");
    parser.parse().expect("failed to parser js");
}

#[test]
fn strict_global_yield() {
    let _ = env_logger::try_init();
    let js = "'use strict'
yield;
";
    let mut parser = Parser::new(js).expect("failed to create parser");
    let expect = parser.parse();
    if let Err(ressa::Error::NonStrictFeatureInStrictContext(_, _)) = expect {
        ()
    } else {
        panic!("Incorrectly parsed reserved word as identifier");
    }
}
