#![cfg(test)]

use ressa::node::*;
use ressa::Parser;
use env_logger;

#[test]
fn parse_directive_prologues() {
    let js = "'use strict'";
    let expectation = Program::Script(vec![ProgramPart::use_strict(false)]);
    execute(js, expectation);
    let js = r#""use strict";"#;
    let expectation = Program::Script(vec![ProgramPart::use_strict(true)]);
    execute(js, expectation);
    let js = "'use strict';\n'use strict';";
    let expectation = Program::Script(vec![ProgramPart::use_strict(false),
                                        ProgramPart::use_strict(false)]);
    execute(js, expectation);
}

#[test]
fn parse_with_statement() {
    let js = "with (Math) {
        floor(PI * random())
        }";
    let random = Expression::call(Expression::ident("random"), vec![]);
    let pi = Expression::ident("PI");
    let bin = Expression::binary(pi, BinaryOperator::Times, random);
    let floor = Expression::call(Expression::ident("floor"), vec![bin]);
    let part = ProgramPart::Statement(Statement::Expr(floor));
    let body = Statement::Block(vec![part]);
    let stmt = Statement::with(Expression::ident("Math"), body);
    let part = ProgramPart::Statement(stmt);
    let expectation = Program::Script(vec![part]);
    execute(js, expectation);
}

#[test]
fn parse_while_stmt() {
    let js = "while (true) {
        console.log('false');
        break;
    }";
    let test = Expression::boolean(true);
    let bk = Statement::Break(None);
    let log_args = Expression::string("'false'");
    let member = Expression::member(Expression::ident("console"), Expression::ident("log"), false);
    let call = Expression::call(member, vec![log_args]);
    let call = Statement::Expr(call);
    let part = ProgramPart::Statement(call);
    let part2 = ProgramPart::Statement(bk);
    let body = Statement::Block(vec![
        part,
        part2,
    ]);
    let stmt = Statement::while_stmt(test, body);
    let part = ProgramPart::Statement(stmt);
    let expectation = Program::Script(vec![part]);
    execute(js, expectation);
}

#[test]
fn parse_while_stmt_2() {
    let js = "
    while (Math.random() > 0.1) {
        console.log('loop');
    }";
    let math_random = Expression::call(
        Expression::member(Expression::ident("Math"), Expression::ident("random"), false),
        vec![]
    );
    let test = Expression::binary(math_random, BinaryOperator::GreaterThan, Expression::number("0.1"));
    let lp = Expression::string("'loop'");
    let console_log = Expression::call(
        Expression::member(Expression::ident("console"), Expression::ident("log"), false),
        vec![lp]
    );
    let body = Statement::Block(vec![ProgramPart::Statement(Statement::Expr(console_log))]);
    let while_loop = Statement::while_stmt(test, body);
    let part = ProgramPart::Statement(while_loop);
    execute(js, Program::Script(vec![part]));
}

#[test]
fn parse_var_stmt() {
    let js = "var i;";
    let decl = VariableDecl::uninitialized("i");
    let stmt = Statement::Var(vec![decl]);
    let part = ProgramPart::Statement(stmt);
    let program = Program::Script(vec![part]);
    execute(js, program);
}

#[test]
fn parse_var_stmt_2() {
    let js = "var i = 0;";
    let decl = VariableDecl::with_value("i", Expression::number("0"));
    let stmt = Statement::Var(vec![decl]);
    let part = ProgramPart::Statement(stmt);
    let program = Program::Script(vec![part]);
    execute(js, program);
}

#[test]
fn parse_var_stmt_3() {
    let js = "var a, b, c, d = 22";
    let stmt = Statement::Var(
        vec![VariableDecl::uninitialized("a"),
        VariableDecl::uninitialized("b"),
        VariableDecl::uninitialized("c"),
        VariableDecl::with_value("d", Expression::number("22"))]
    );
    let part = ProgramPart::Statement(stmt);
    let program = Program::Script(vec![part]);
    execute(js, program);
}

#[test]
fn parse_var_stmt_fn() {
    let js = "var fn = function (one, two) {
        return one + two;
        }";
    let addition = Expression::binary(
        Expression::ident("one"),
        BinaryOperator::Plus,
        Expression::ident("two")
    );
    let body = vec![ProgramPart::Statement(Statement::Return(Some(addition)))];
    let func = Expression::function(None, vec![FunctionArg::ident("one"), FunctionArg::ident("two")], body, false, false);
    let v = Statement::Var(vec![VariableDecl::with_value("fn", func)]);
    let part = ProgramPart::Statement(v);
    let program = Program::Script(vec![part]);
    execute(js, program);
}

#[test]
fn parse_var_stmt_fn_2() {
    let js = "var fn = function* x() {
        yield 'one';
        yield 'two';
    }";

    let one = Expression::yield_with_arg(Expression::string("'one'"), false);
    let two = Expression::yield_with_arg(Expression::string("'two'"), false);
    let body = vec![
        ProgramPart::Statement(Statement::Expr(one)),
        ProgramPart::Statement(Statement::Expr(two)),
    ];
    let func = Expression::function(Some("x".to_string()), vec![], body, true, false);
    let v = Statement::Var(vec![VariableDecl::with_value("fn", func)]);
    let part = ProgramPart::Statement(v);
    let program = Program::Script(vec![part]);
    execute(js, program);
}

#[test]
fn parse_var_stmt_destructure() {
    let js = "var {a, b, c} = {a: 0, b: 1, c: 2}";
    let init = vec![
        ObjectProperty::number("a", "0"),
        ObjectProperty::number("b", "1"),
        ObjectProperty::number("c", "2"),
    ];
    let decl = VariableDecl::destructed(&["a", "b", "c"], init);
    let stmt = Statement::Var(vec![decl]);
    let program = Program::Script(vec![ProgramPart::Statement(stmt)]);
    execute(js, program);
}

#[test]
fn parse_var_stmt_destructure_rest() {
    let js = "var {a, b, c, ...arg} = {a: 0, b: 1, c: 2, d: 3, e: 4}";
    let init = vec![
        ObjectProperty::number("a", "0"),
        ObjectProperty::number("b", "1"),
        ObjectProperty::number("c", "2"),
        ObjectProperty::number("d", "3"),
        ObjectProperty::number("e", "4"),
    ];
    let decl = VariableDecl::destructed_with_rest(&["a", "b", "c"], "arg", init);
    let stmt = Statement::Var(vec![decl]);
    let program = Program::Script(vec![ProgramPart::Statement(stmt)]);
    execute(js, program);
}

#[test]
fn parse_try_stmt() {
    let js = "try {
            console.log('trying');
        } finally {
            console.log('done trying');
        }";


    parse(js);
}

#[test]
fn parse_try_stmt_2() {
    let js = "try {
        console.log('trying');
    } catch (e) {
        console.log('caught', e);
    }";
    parse(js);
}
#[test]
fn parse_labeled_stmt_lf() {
    let js = "linefeed:0\n0;";
    let program = Program::Script(vec![
        ProgramPart::Statement(
            Statement::Labeled(
                LabeledStatement {
                    label: "linefeed".to_string(),
                    body: Box::new(
                        Statement::Expr(Expression::number("0"))
                    )
                }
            )
        ),
        ProgramPart::Statement(
            Statement::Expr(Expression::number("0"))
        )
    ]);
    execute(js, program);
}

#[test]
fn parse_var_ident_fn() {
    let _ = env_logger::try_init();
    let js = "({var(a, b = 0, [c,, d = 0, ...e], {f, g: h, i = 0, i: j = 0}, ...k){}})";
    parse(js);
}

#[test]
fn parse_arg_ident_assign() {
    let _ = env_logger::try_init();
    let js = "({
        var({i = 0, i: j = 0}) {

        }
    })";
    parse(js);
}

#[test]
fn parse_nested_delegated_yield() {
    let _ = env_logger::try_init();
    let js = "function*g(a, b = 0, [c,, d = 0, ...e], {f, g: h, i = 0, i: j = 0}, ...k){
  return a = yield* b = yield c = yield yield;
}";
    parse(js);
}
#[test]
fn parse_obj_patter_fn_fat_arrow() {
    let _ = env_logger::try_init();
    let js = "({i = 0, i: j = 0}) => {;};";
    parse(js);
}
#[test]
fn parse_obj_pattern_fn_fat_arrow2() {
    let _ = env_logger::try_init();
    let js = "({x}) => ({x});";

    parse(js);
}

#[test]
fn parse_super_call() {
    let _ = env_logger::try_init();
    let js = "class A extends B {
        constructor() {
            super(new.target);
        }
    }";
    parse(js);
}

#[test]
fn parse_delete_comma_op() {
    let _ = env_logger::try_init();
    let js = "remove: function(a){
        if(l<Number.MAX_VALUE){
            var b=m[a];
            if(!b)return;
            b==n && (n=b.p);
            b==p && (p=b.n);
            f(b.n,b.p);
            delete m[a]
        }
        a in k&&(delete k[a],g--)
    }";
    parse(js);
}

#[test]
fn doc_snippet() {
    let js = "function helloWorld() { alert('Hello world'); }";
    let p = Parser::new(&js).unwrap();
    let f = ProgramPart::decl(
        Declaration::Function(
            Function {
                id: Some("helloWorld".to_string()),
                params: vec![],
                body: vec![
                    ProgramPart::Statement(
                        Statement::Expr(
                            Expression::call(Expression::ident("alert"), vec![Expression::string("'Hello world'")])
                        )
                    )
                ],
                generator: false,
                is_async: false,
            }
        )
    );
    for part in p {
        assert_eq!(part.unwrap(), f);
    }
}

#[test]
fn builder_doc_snippet() {
    use ressa::Builder;
    let js = "for (var i = 0; i < 100; i++) {
        console.log('loop', i);
        }";
    let p = Builder::new()
                    .comments(false)
                    .module(false)
                    .js(js)
                    .build()
                    .unwrap();
    for part in p {
        let expecation = ProgramPart::Statement(
            Statement::For(
                ForStatement {
                    init: Some(
                        LoopInit::Variable(
                            vec![VariableDecl::with_value("i", Expression::number("0"))]
                        )
                    ),
                    test: Some(
                        Expression::binary(Expression::ident("i"), BinaryOperator::LessThan, Expression::number("100"))
                    ),
                    update: Some(
                        Expression::Update(
                            UpdateExpression {
                                operator: UpdateOperator::Increment,
                                argument: Box::new(Expression::ident("i")),
                                prefix: false,
                            }
                        )
                    ),
                    body:
                        Box::new(Statement::Block(
                            vec![ProgramPart::Statement(
                                Statement::Expr(
                                    Expression::call(Expression::member(Expression::ident("console"), Expression::ident("log"), false),
                                    vec![
                                        Expression::string("'loop'"),
                                        Expression::ident("i"),
                                    ])
                                )
                            )]
                        )
                    )
                }
            )
        );
        assert_eq!(part.unwrap(), expecation);
    }
}

#[test]
fn parse_doc_example() {
    let js = "function helloWorld() { alert('Hello world'); }";
    let mut p = Parser::new(&js).unwrap();
    let expectation = Program::Script(vec![
        ProgramPart::decl(
        Declaration::Function(
            Function {
                id: Some("helloWorld".to_string()),
                params: vec![],
                body: vec![
                    ProgramPart::Statement(
                        Statement::Expr(
                            Expression::call(Expression::ident("alert"), vec![Expression::string("'Hello world'")])
                        )
                    )
                ],
                generator: false,
                is_async: false,
            }
        )
    )
    ]);
    let program = p.parse().unwrap();
    assert_eq!(program, expectation);
}


fn execute(js: &str, expectation: Program) {
    let s = parse(js);
    assert_eq!(s, expectation);
}

fn parse(js: &str) -> Program {
    let mut p = Parser::new(js).unwrap();
    let s = p.parse().unwrap();
    s
}