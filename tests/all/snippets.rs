use resast::prelude::*;
use ressa::*;
#[test]
fn doc1() {
    let js = "function helloWorld() { alert('Hello world'); }";
    let p = Parser::new(&js).unwrap();
    let f = ProgramPart::decl(
        Decl::Func(
            Func {
                id: Some(Ident::from("helloWorld")),
                params: vec![],
                body: FuncBody(
                    vec![
                        ProgramPart::Stmt(
                            Stmt::Expr(
                                Expr::Call(
                                    CallExpr {
                                        callee: Box::new(
                                            Expr::ident_from("alert")
                                        ),
                                        arguments: vec![
                                            Expr::Lit(
                                                Lit::single_string_from("Hello world")
                                            )
                                        ],
                                    }
                                )
                            )
                        )
                    ]
                ),
                generator: false,
                is_async: false,
            }
        )
    );
    for part in p {
        assert_eq!(part.unwrap(), f);
    }
}
