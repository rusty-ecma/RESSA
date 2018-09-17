#![cfg(test)]
extern crate resp;
use resp::Parser;
use resp::node::*;


#[test]
fn parse_directive_prologues() {
    let js = "'use strict'";
    let expectation = Program::Script(vec![
        ProgramPart::Directive(
            Directive {
                expression: Literal::String(String::from("'use strict'")),
                directive: String::from("use strict")
            }
        )
    ]);
    execute(js, expectation);
    let js = r#""use strict";"#;
    let expectation = Program::Script(vec![
        ProgramPart::Directive(
            Directive {
                expression: Literal::String(String::from(r#""use strict""#)),
                directive: String::from("use strict")
            }
        )
    ]);
    execute(js, expectation);
    let js = "'use strict';\n'use strict';";
    let expectation = Program::Script(vec![
        ProgramPart::Directive(
            Directive {
                expression: Literal::String(String::from(r#"'use strict'"#)),
                directive: String::from("use strict")
            }
        ),
        ProgramPart::Directive(
            Directive {
                expression: Literal::String(String::from(r#"'use strict'"#)),
                directive: String::from("use strict")
            }
        ),
    ]);
    execute(js, expectation);
}

#[test]
fn parse_with_statement() {
    let js = "with (Math) {
        floor(PI * random())
        }";
    let random = Expression::Call(CallExpression {
        callee: Box::new(Expression::Ident(String::from("random"))),
        arguments: vec![]
    });
    let pi = Expression::Ident(String::from("PI"));
    let bin = Expression::Binary(BinaryExpression{
        operator: BinaryOperator::Times,
        left: Box::new(pi),
        right: Box::new(random),
    });
    let floor = Expression::Call(CallExpression {
        callee: Box::new(Expression::Ident(String::from("floor"))),
        arguments: vec![bin],
    });

    let body = Statement::Block(vec![Statement::Expr(floor)]);
    let obj = Expression::Ident(String::from("Math"));
    let with = WithStatement{
        object: obj,
        body: Box::new(body),
    };
    let stmt = Statement::With(with);
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
    let test = Expression::Literal(Literal::Boolean(true));
    let bk = Statement::Break(None);
    let log_args = Expression::Literal(Literal::String(String::from("'false'")));
    let console = Expression::Ident(String::from("console"));
    let log = Expression::Ident(String::from("log"));
    let member = MemberExpression {
        object: Box::new(console),
        property: Box::new(log),
        computed: false,
    };
    let mem_expr = Expression::Member(member);
    let call = CallExpression {
        callee: Box::new(mem_expr),
        arguments: vec![log_args],
    };
    let expr = Expression::Call(call);
    let body_one = Statement::Expr(expr);
    let body = Statement::Block(vec![
        body_one,
        bk,
    ]);
    let while_stmt = WhileStatement {
        test,
        body: Box::new(body),
    };
    let stmt = Statement::While(while_stmt);
    let part = ProgramPart::Statement(stmt);
    let expectation = Program::Script(vec![part]);
    execute(js, expectation);
}

fn execute(js: &str, expectation: Program) {
    let mut p = Parser::new(js).unwrap();
    let s = p.parse().unwrap();
    println!("{:#?}", s);
    assert_eq!(s, expectation);
}