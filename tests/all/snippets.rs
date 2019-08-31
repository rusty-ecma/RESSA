use resast::prelude::*;
use ressa::*;
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
fn arrow_func_args() {
    let js = "(a, b = 0, [c,, d = 0, ...e], {f, g: h, i = 0, i: j = 0}, ...k) => {;};";
    let mut parser = Parser::new(&js).unwrap();
    let _parsed = parser.parse().unwrap();
    // assert_eq!(
    //     parsed,
    //     Program::Script(
    //         vec![
    //             ProgramPart::Stmt(
    //                 Stmt::Expr(
    //                     Expr::ArrowFunc(
    //                         ArrowFuncExpr {
    //                             body: ArrowFuncBody::FuncBody(FuncBody(vec![
    //                                 ProgramPart::Stmt(
    //                                     Stmt::Empty
    //                                 )
    //                             ])),
    //                             expression: false,
    //                             generator: false,
    //                             id: None,
    //                             is_async: false,
    //                             params: vec![
    //                                 FuncArg::Expr(Expr::Ident(Ident::from("a"))),
    //                                 FuncArg::Pat(Pat::Assign(AssignPat {
    //                                     left: Box::new(Pat::Ident(Ident::from("b"))),
    //                                     right: Box::new(Expr::Lit(Lit::number_from("0")))
    //                                 })),
    //                                 FuncArg::Pat(Pat::)
    //                             ]
    //                         }
    //                     )
    //                 )
    //             )
    //         ]
    //     )
    // )
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
