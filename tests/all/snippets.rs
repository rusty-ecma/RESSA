#![cfg(test)]

use env_logger;
use ressa::node::*;
use ressa::Parser;

#[test]
fn parse_directive_prologues() {
    let js = "'use strict'";
    let expectation = Program::Script(vec![ProgramPart::use_strict(false)]);
    execute(js, expectation);
    let js = r#""use strict";"#;
    let expectation = Program::Script(vec![ProgramPart::use_strict(true)]);
    execute(js, expectation);
    let js = "'use strict';\n'use strict';";
    let expectation = Program::Script(vec![
        ProgramPart::use_strict(false),
        ProgramPart::use_strict(false),
    ]);
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
    let member = Expression::member(
        Expression::ident("console"),
        Expression::ident("log"),
        false,
    );
    let call = Expression::call(member, vec![log_args]);
    let call = Statement::Expr(call);
    let part = ProgramPart::Statement(call);
    let part2 = ProgramPart::Statement(bk);
    let body = Statement::Block(vec![part, part2]);
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
        Expression::member(
            Expression::ident("Math"),
            Expression::ident("random"),
            false,
        ),
        vec![],
    );
    let test = Expression::binary(
        math_random,
        BinaryOperator::GreaterThan,
        Expression::number("0.1"),
    );
    let lp = Expression::string("'loop'");
    let console_log = Expression::call(
        Expression::member(
            Expression::ident("console"),
            Expression::ident("log"),
            false,
        ),
        vec![lp],
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
    let stmt = Statement::Var(vec![
        VariableDecl::uninitialized("a"),
        VariableDecl::uninitialized("b"),
        VariableDecl::uninitialized("c"),
        VariableDecl::with_value("d", Expression::number("22")),
    ]);
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
        Expression::ident("two"),
    );
    let body = vec![ProgramPart::Statement(Statement::Return(Some(addition)))];
    let func = Expression::function(
        None,
        vec![FunctionArg::ident("one"), FunctionArg::ident("two")],
        body,
        false,
        false,
    );
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
        ProgramPart::Statement(Statement::Labeled(LabeledStatement {
            label: "linefeed".to_string(),
            body: Box::new(Statement::Expr(Expression::number("0"))),
        })),
        ProgramPart::Statement(Statement::Expr(Expression::number("0"))),
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
    let f = ProgramPart::decl(Declaration::Function(Function {
        id: Some("helloWorld".to_string()),
        params: vec![],
        body: vec![ProgramPart::Statement(Statement::Expr(Expression::call(
            Expression::ident("alert"),
            vec![Expression::string("'Hello world'")],
        )))],
        generator: false,
        is_async: false,
    }));
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
    let p = Builder::new().module(false).js(js).build().unwrap();
    for part in p {
        let expecation = ProgramPart::Statement(Statement::For(ForStatement {
            init: Some(LoopInit::Variable(vec![VariableDecl::with_value(
                "i",
                Expression::number("0"),
            )])),
            test: Some(Expression::binary(
                Expression::ident("i"),
                BinaryOperator::LessThan,
                Expression::number("100"),
            )),
            update: Some(Expression::Update(UpdateExpression {
                operator: UpdateOperator::Increment,
                argument: Box::new(Expression::ident("i")),
                prefix: false,
            })),
            body: Box::new(Statement::Block(vec![ProgramPart::Statement(
                Statement::Expr(Expression::call(
                    Expression::member(
                        Expression::ident("console"),
                        Expression::ident("log"),
                        false,
                    ),
                    vec![Expression::string("'loop'"), Expression::ident("i")],
                )),
            )])),
        }));
        assert_eq!(part.unwrap(), expecation);
    }
}

#[test]
fn parse_doc_example() {
    let js = "function helloWorld() { alert('Hello world'); }";
    let mut p = Parser::new(&js).unwrap();
    let expectation = Program::Script(vec![ProgramPart::decl(Declaration::Function(Function {
        id: Some("helloWorld".to_string()),
        params: vec![],
        body: vec![ProgramPart::Statement(Statement::Expr(Expression::call(
            Expression::ident("alert"),
            vec![Expression::string("'Hello world'")],
        )))],
        generator: false,
        is_async: false,
    }))]);
    let program = p.parse().unwrap();
    assert_eq!(program, expectation);
}

#[test]
fn readme_example() {
    let js = "
function Thing() {
    return 'stuff';
}
";
    let parser = Parser::new(js).expect("Failed to create parser");
    for part in parser {
        let part = part.expect("Error parsing part");
        match part {
            ProgramPart::Decl(decl) => match decl {
                Declaration::Function(f) => {
                    if let Some(ref id) = f.id {
                        assert_eq!(id, "Thing");
                    }
                    assert!(f.params.len() == 0);
                    assert!(!f.generator);
                    assert!(!f.is_async);
                    for part in f.body {
                        match part {
                            ProgramPart::Statement(stmt) => match stmt {
                                Statement::Return(expr) => {
                                    if let Some(expr) = expr {
                                        match expr {
                                            Expression::Literal(lit) => match lit {
                                                Literal::String(value) => {
                                                    assert_eq!(value, String::from("'stuff'"))
                                                }
                                                _ => (),
                                            },
                                            _ => (),
                                        }
                                    }
                                }
                                _ => (),
                            },
                            _ => (),
                        }
                    }
                }
                _ => (),
            },
            _ => (),
        }
    }
}

#[test]
fn first_blog_post() {
    let expect = ProgramPart::Decl(Declaration::Function(Function {
        id: Some(String::from("print")),
        params: vec![FunctionArg::Pattern(Pattern::Identifier(String::from(
            "message",
        )))],
        body: vec![ProgramPart::Statement(Statement::Expr(Expression::Call(
            CallExpression {
                callee: Box::new(Expression::Member(MemberExpression {
                    object: Box::new(Expression::Ident(String::from("console"))),
                    property: Box::new(Expression::Ident(String::from("log"))),
                    computed: false,
                })),
                arguments: vec![Expression::Ident(String::from("message"))],
            },
        )))],
        generator: false,
        is_async: false,
    }));
    let js = "function print(message) {
    console.log(message)
}";
    execute(js, Program::Script(vec![expect]));
}

#[test]
fn obj_pattern() {
    let js = "console.log({a: 'thing', b() {console.log('b')}});";
    let out = parse(js);
    println!("{:?}", out);
}

#[test]
fn loop_decl_error() {
    let js = "function dependArray (value) {
  for (let e, i = 0, l = value.length; i < l; i++) {
    e = value[i];
    e && e.__ob__ && e.__ob__.dep.depend();
    if (Array.isArray(e)) {
      dependArray(e);
    }
  }
}";
    let mut p = ressa::Builder::new().module(true).js(js).build().unwrap();
    let _ = p.parse().unwrap();
}

#[test]
fn template_tail_error() {
    let _ = env_logger::try_init();
    let js = "function getRawDirName (dir) {
  return dir.rawName || `${dir.name}.${Object.keys(dir.modifiers || {}).join('.')}`
}";
    let mut p = ressa::Builder::new().module(true).js(js).build().unwrap();
    let _ = p.parse().unwrap();
}

#[test]
fn comment_handler_test() {
    use ress::Item;
    let js = "//things
    /* things */
    <!-- things -->";
    let mut i = 0;
    let expectation = [
        ress::Comment::new_single_line("things"),
        ress::Comment::new_multi_line(" things "),
        ress::Comment::new_html_no_tail(" things "),
    ];
    let mut p = ressa::Builder::new()
        .js(js)
        .with_comment_handler(|item: Item| {
            if let ress::Token::Comment(ref c) = item.token {
                assert_eq!(c, &expectation[i])
            }
            i += 1;
        })
        .unwrap();
    p.parse().unwrap();
}

#[test]
fn comment_handler_test_2() {
    use ress::Item;
    let js = "//things
    /*things*/
    <!--things-->";
    let mut p = ressa::Builder::new()
        .js(js)
        .with_comment_handler(|item: Item| {
            assert!(item.matches_comment_str("things"));
        })
        .unwrap();
    p.parse().unwrap();
}

#[test]
fn class_expr() {
    let _ = env_logger::try_init();
    let js = r#"for(let {a = new class extends Array { constructor(b = (a = eval("()=>super()"))){} }} of [[]]);"#;
    println!("{:#?}", parse(js));
}

#[test]
fn class_expr_pattern() {
    let _ = env_logger::try_init();
    let js = "for ([class get {} ().iterator] of []);";
    println!("{:#?}", parse(js));
}

#[test]
fn expr_to_pat() {
    let _ = env_logger::try_init();
    let js = "let x = { [Symbol.species]: function(length) { return; } }";
    println!("{:#?}", parse(js));
}

#[test]
fn non_strict_mode_keywords() {
    let _ = env_logger::try_init();
    let js = "var f = package => 0;";
    println!("{:#?}", parse(js));
}

#[test]
fn obj_literal_in_binary_op() {
    let _ = env_logger::try_init();
    let js = r#""" + {toString: Date.prototype.toJSON};"#;
    println!("{:#?}", parse(js));
}

#[test]
fn template_middle() {
    let _ = env_logger::try_init();
    let js = r#"let bitval = `(${prefix}.const ${format(val, i)})`"#;
    println!("{:#?}", parse(js));
}

#[test]
fn loop_left_expr() {
    let _ = env_logger::try_init();
    let js = r#"for (t[t++] in object);"#;
    println!("{:#?}", parse(js));
}
#[test]
fn for_loop_nonsense() {
    let _ = env_logger::try_init();
    let js = "for (var i = 1000; i-- > 0; ) {
    expr = `(f32.neg ${expr})`;
}";
    println!("{:#?}", parse(js));
}

#[test]
fn function_param_pattern() {
    let _ = env_logger::try_init();
    let js = r#"function f(x, await = () => Array.isArray(revocable.proxy), ...get) {
    dbg.getNewestFrame().older.eval("print(a)");
}"#;
    println!("{:#?}", parse(js));
}

#[test]
fn more_for_loop_nonsense() {
    let _ = env_logger::try_init();
    let js = "for (f().x of [])
    s += '.';";
    println!("{:#?}", parse(js));
}

#[test]
fn deeply_nested_template() {
    let _ = env_logger::try_init();
    let js = r#"`
 (func ${name} (param $barrierValue i32) (result i32)
   (local $n i32)
   (local $tmp ${prefix})
   (set_local $n (i32.const ${ITERATIONS}))
   (loop $outer
    (if (get_local $n)
        (block
         ${isMaster ? `;; Init
(${prefix}.atomic.store${tag} ${loc} (${prefix}.const ${distribute(initial)}))` : ``}
         ${barrier}

${(() => {
    let s = `;; Do\n`;
    for (let i=0; i < NUMVALS; i++) {
        let bitval = `(${prefix}.const ${format(val, i)})`
        // The load must be atomic though it would be better if it were relaxed,
        // we would avoid fences in that case.
        if (op.match(/cmpxchg/)) {
            s += `(loop $doit
                   (set_local $tmp (${prefix}.atomic.load${tag} ${loc}))
                   (br_if $doit (i32.eqz
                                 (${prefix}.eq
                                  (get_local $tmp)
                                  (${op} ${loc} (get_local $tmp) (${prefix}.or (get_local $tmp) ${bitval}))))))
            `;
        } else {
            s += `(drop (${op} ${loc} ${bitval}))
            `;
       }
     }
    return s
})()}
         (loop $wait_done
          (br_if $wait_done (${prefix}.ne (${prefix}.atomic.load${tag} ${loc}) (${prefix}.const ${distribute(expected)}))))
         ${barrier}
         (set_local $n (i32.sub (get_local $n) (i32.const 1)))
         (br $outer))))
  (get_local $barrierValue))`"#;
    println!("{:#?}", parse(js));
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
