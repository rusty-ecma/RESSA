#![cfg(test)]

use resp::node::*;
use resp::Parser;
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

    let program = Program::Script(vec![]);
    execute(js, program);
}

#[test]
fn parse_try_stmt_2() {
    let js = "try {
        console.log('trying');
    } catch (e) {
        console.log('caught', e);
    }";
    let program = Program::Script(vec![]);
    execute(js, program);
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
    let program = Program::Script(vec![]);
    execute(js, program);
}

#[test]
fn parse_arg_ident_assign() {
    let _ = env_logger::try_init();
    let js = "({
        var({i = 0, i: j = 0}) {

        }
    })";
    let program = Program::Script(vec![]);
    execute(js, program);
}

#[test]
fn parse_nested_delegated_yield() {
    let _ = env_logger::try_init();
    let js = "function*g(a, b = 0, [c,, d = 0, ...e], {f, g: h, i = 0, i: j = 0}, ...k){
  return a = yield* b = yield c = yield yield;
}";
    let program = Program::Script(vec![]);
    execute(js, program);
}
#[test]
fn parse_obj_patter_fn_fat_arrow() {
    let _ = env_logger::try_init();
    let js = "(a, b = 0, [c,, d = 0, ...e], {f, g: h, i = 0, i: j = 0}, ...k) => {;};";
    let program = Program::Script(vec![]);
    execute(js, program);
}
#[test]
fn parse_obj_pattern_fn_fat_arrow2() {
    let _ = env_logger::try_init();
    let js = "({x}) => ({x});";
    let program = Program::Script(vec![]);
    execute(js, program);
}


fn execute(js: &str, expectation: Program) {
    let mut p = Parser::new(js).unwrap();
    let s = p.parse().unwrap();
    println!("{:#?}", s);
    assert_eq!(s, expectation);
}