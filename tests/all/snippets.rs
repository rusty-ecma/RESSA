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

#[test]
fn new_line_in_fat_arrow() {
    let js = "var af = x
=> x;";
    let mut parser = Parser::new(js).expect("failed to create parser");
    let expect = parser.parse();
    if let Err(ressa::Error::NewLineAfterFatArrow(_)) = expect {
        ()
    } else {
        panic!(
            "Incorrectly parsed arrow function with new line after =>\n{:?}",
            expect
        );
    }
}

#[test]
fn arguments_as_param_arrow() {
    let _ = env_logger::try_init();
    let js = "'use strict';
var x = arguments => arguments;";
    let mut parser = Parser::new(js).expect("failed to create parser");
    let expect = parser.parse();
    if let Err(ressa::Error::StrictModeArgumentsOrEval(_)) = expect {
        ()
    } else {
        panic!("Incorrectly parsed arguments as param in strict mode");
    }
}

#[test]
fn duplicate_proto() {
    let _ = env_logger::try_init();
    let js = "({
__proto__: Number,
'__proto__': Number,
});";
    let mut parser = Parser::new(js).expect("failed to create parser");
    let expect = parser.parse();
    if let Err(ressa::Error::Redecl(_, _)) = expect {
        ()
    } else {
        panic!(
            "Incorrectly parsed multiple __proto__ properties:\n\t{:?}",
            expect
        );
    }
}
#[test]
#[should_panic = "expected to fail on super call in function"]
fn super_in_func() {
    let _ = env_logger::try_init();
    let js = "function() { super() }";
    let mut p = Parser::new(js).unwrap();
    p.parse()
        .expect("expected to fail on super call in function");
}

#[test]
fn super_in_ctor() {
    let _ = env_logger::try_init();
    let js = "
class A {}
class B extends A {
    constructor() {
        super()
    }
}";
    let mut p = Parser::new(js).unwrap();
    p.parse().expect("failed to handle super call in ctor");
}

#[test]
fn super_in_method() {
    let _ = env_logger::try_init();
    let js = "
class A {}
class B extends A {
    thing() {
        return super.stuff;
    }
}";
    let mut p = Parser::new(js).unwrap();
    p.parse()
        .expect("failed to handle super property in method");
}

#[test]
fn super_in_async_method() {
    run_test(
        "var x = { async method(x = super.method()) { await 1; } }",
        false,
    )
    .unwrap();
}

#[test]
#[should_panic = "super calls should only be allowed in ctors"]
fn super_in_method_neg() {
    let _ = env_logger::try_init();
    let js = "
class A {}
class B {
    thing() {
        super();
    }
}";
    let mut p = Parser::new(js).unwrap();
    p.parse()
        .expect("super calls should only be allowed in ctors");
}
#[test]
#[should_panic = "super is invalid in an arrow"]
fn super_in_arrow_func() {
    let _ = env_logger::try_init();
    let js = "() => super();";
    let mut p = Parser::new(js).unwrap();
    p.parse().expect("super is invalid in an arrow");
}
#[test]
fn super_in_lit_getter() {
    let _ = env_logger::try_init();
    let js = "({
get a() { return super.stuff; }
});";
    let mut p = Parser::new(js).unwrap();
    p.parse().unwrap();
}

#[test]
#[should_panic = "assignment not allowed in `for in` lhs"]
fn init_in_for_in_loop() {
    let _ = env_logger::try_init();
    let js = "for (a = 0 in {}) {}";
    let mut p = Parser::new(js).unwrap();
    p.parse().expect("assignment not allowed in `for in` lhs");
}

#[test]
#[should_panic = "assignment not allowed in `for in` lhs"]
fn var_init_in_for_in_loop_strict() {
    let _ = env_logger::try_init();
    let js = "'use strict';
for (var a = 0 in {}) {}";
    let mut p = Parser::new(js).unwrap();
    p.parse().expect("assignment not allowed in `for in` lhs");
}
#[test]
fn var_init_in_for_in_loop_not_strict() {
    let _ = env_logger::try_init();
    let js = "for (var a = 0 in {}) {}";
    let mut p = Parser::new(js).unwrap();
    p.parse().expect("assignment not allowed in `for in` lhs");
}
#[test]
#[should_panic = "use strict not allowed with fancy params"]
fn arrow_funct_non_simple_args_use_strict_in_body() {
    let _ = env_logger::try_init();
    let js = "var x = (a = 0) => {
'use strict';
};";
    let mut p = Parser::new(js).unwrap();
    p.parse().expect("use strict not allowed with fancy params");
}

#[test]
fn nested_await_ident() {
    run_test(
        "var await;
async function i() {
    function b() {
        await = 0;
    }
}",
        false,
    )
    .unwrap();
}

#[test]
fn nested_dupe_ident() {
    run_test(
        "(function() {})(function() {
            function offset (token, separator) {
            addFormatToken(token, 0, 0, function () {
                var offset = this.utcOffset();
                var sign = '+';
                if (offset < 0) {
                    offset = -offset;
                    sign = '-';
                }
                return sign + zeroFill(~~(offset / 60), 2) + separator + zeroFill(~~(offset) % 60, 2);
            });
        }
        })",
        false,
    ).unwrap();
}

#[test]
fn super_in_class_expr_ctor() {
    let _ = env_logger::try_init();
    let js = "new class extends Other {
    constructor() {
        super();
    }
}";
    let mut p = Parser::new(js).unwrap();
    p.parse().expect("super in class expr ctor");
}
#[test]
fn line_term_comment() {
    let _ = env_logger::try_init();
    let js = "''/*
*/''";
    let mut parser = Parser::new(js).expect("failed to create parser");
    parser.parse().unwrap();
}

#[test]
fn await_as_ident() {
    run_test("var await;", false).unwrap();
}
#[test]
#[should_panic = "await is always reserved in a module"]
fn await_as_ident_module() {
    run_test("var await;", true).expect("await is always reserved in a module");
}
#[test]
fn await_as_ident_strict() {
    run_test("'use strict';var await;", false).unwrap();
}
#[test]
#[should_panic = "await is reserved in an async fn"]
fn await_as_ident_async_fn() {
    let _ = env_logger::try_init();
    let js = "async function() { var await = 0; }";
    let mut p = Parser::builder().js(js).module(true).build().unwrap();
    p.parse().expect("await is reserved in an async fn");
}

#[test]
#[should_panic = "export is reserved"]
fn export_as_ident() {
    run_test(r"var expor\u0074;", false).expect("export is reserved");
}

#[test]
fn async_arrow_await() {
    run_test("async () => await 0;", false).unwrap();
}

#[test]
fn use_strict_in_complicated_arrow() {
    run_test("f = (a = 0) => b => { 'use strict'; };", false).unwrap();
}

#[test]
fn yield_as_param_in_method() {
    run_test("var x = { y(yield) { return yield; } };", false).unwrap();
}

#[test]
fn async_await() {
    run_test(
        "var f = async function() {
        try {
            await new Promise((r) => r());
        } catch (e) {
            await new Promise((r, j) => j(e));
        } finally {
            await new Promise((r) => r());
        }
    }",
        false,
    )
    .unwrap();
}

#[test]
fn for_in_head_let() {
    run_test("for(var let in {});", false).unwrap();
}

#[test]
fn import_aliased_eval() {
    run_test("import { eval as _eval } from './other.js';", true).unwrap();
}

#[test]
fn static_ident() {
    run_test("var static = 0;", false).unwrap();
}

#[test]
fn let_in_obj_init() {
    run_test(
        "var let = 1;
var o = {let};",
        false,
    )
    .unwrap();
}

#[test]
fn await_as_class_ident() {
    run_test("class await {}", false).unwrap();
}
#[test]
fn await_as_class_ident_expr() {
    run_test("var ctor = class a\\u0077ait {}", false).unwrap();
}

#[test]
fn let_in_for_loop() {
    run_test(
        "let = 1;
for (let; ; )
    break;",
        false,
    )
    .unwrap();
}
#[test]
fn let_in_for_loop2() {
    run_test(
        "let = 1;
for (let = 2; ; )
    break;",
        false,
    )
    .unwrap();
}

#[test]
fn async_obj_lit_method() {
    run_test("var x = { async m() { await 0 } }", false).unwrap();
}

#[test]
fn await_as_label() {
    run_test("await: 1;", false).unwrap();
}

#[test]
#[should_panic]
fn await_as_default_in_param() {
    run_test("(q=await) => {}", false).unwrap();
}

#[test]
#[should_panic]
fn duplicate_params_strict() {
    run_test(
        "'use strict';
function fn(x, x) { }",
        false,
    )
    .unwrap();
}

#[test]
#[should_panic]
fn duplicate_params_strict_inner() {
    run_test("function fn(x, x) { 'use strict' }", false).unwrap();
}
#[test]
#[should_panic]
fn duplicate_params_strict_inner_expr() {
    run_test("(function (x, x) { 'use strict' })", false).unwrap();
}

#[test]
#[should_panic]
fn duplicate_params_arrow() {
    run_test("var fn = (x, x) => {}", false).unwrap();
}
#[test]
#[should_panic]
fn duplicate_params_arrow_array_pattern() {
    run_test("var fn = ([x, x]) => {}", false).unwrap();
}
#[test]
#[should_panic]
fn duplicate_params_arrow_array_ident_pattern() {
    run_test("var fn = (x, [y, x]) => {}", false).unwrap();
}
#[test]
#[should_panic]
fn duplicate_params_arrow_obj_pattern() {
    run_test("var fn = ({x, x}) => {}", false).unwrap();
}
#[test]
#[should_panic]
fn duplicate_params_arrow_obj_ident_pattern() {
    run_test("var fn = (x, {y, x}) => {}", false).unwrap();
}

#[test]
fn html_comment_close() {
    run_test(
        "--> this is a comment
  --> also a comment",
        false,
    )
    .unwrap()
}
#[test]
#[should_panic]
fn html_comment_close_module() {
    run_test("/**/--> this is a comment", true).unwrap()
}
#[test]
#[should_panic]
fn html_comment_close_not_first_token() {
    run_test(";--> this is not a comment", false).unwrap()
}

#[test]
#[should_panic = "LexicalRedecl"]
fn dupe_ident_let_then_var() {
    run_test("{let a; var a;}", false).unwrap()
}
#[test]
#[should_panic = "LexicalRedecl"]
fn dupe_in_switch_let_then_var() {
    run_test("switch (true) { case 1: let q; default: var q; }", false).unwrap();
}
#[test]
fn dupe_ident_var_then_var() {
    run_test("function q() { { var a; var a } }", false).unwrap()
}

#[test]
fn dupe_simple_catch() {
    run_test("try { } catch (e) { var e = 'stuff'; }", false).unwrap();
}
#[test]
#[should_panic = "LexicalRedecl"]
fn dupe_switch_func_then_let() {
    run_test(
        "switch (true) { case 0: function a() {}; default: let a; }",
        false,
    )
    .unwrap();
}

#[test]
fn dupe_func_at_top() {
    run_test(
        "
function f() { return; }
function f() { return 0; }
",
        false,
    )
    .unwrap();
}

#[test]
fn labeled_function() {
    run_test("label: function g() {}", false).unwrap();
}

#[test]
fn closure_argument_scoping() {
    run_test(
        "check(() => {
    let a = 0;
    let b = 1;
    });
check(() => {
    let a = 0;
    let b = 1;
});",
        false,
    )
    .unwrap();
}
#[test]
fn destructing_with_defaults_complicated() {
    run_test(
        "
const {a: {a: b = 3} = {a: undefined}} = {};
var a = 0;
    ",
        false,
    )
    .unwrap();
}

#[test]
#[should_panic = "ForOfNotSimple"]
fn for_of_this_lhs() {
    run_test("for (this of []) ;", false).unwrap();
}

#[test]
#[should_panic = "InvalidStartOfExpressionStmt"]
fn let_open_brace() {
    run_test(
        "for (var i in []) let
[a] = 0;",
        false,
    )
    .unwrap();
}
#[test]
#[should_panic]
fn async_async_obj_prop() {
    run_test("({async async});", false).unwrap();
}
#[test]
#[should_panic]
fn export_in_obj_method_body() {
    run_test("({ get m() { export default null; } });", true).unwrap();
}

#[test]
fn export_default_class() {
    run_test(
        "
export default class Name { }",
        true,
    )
    .unwrap();
}
#[test]
#[should_panic = "DuplicateExport"]
fn duplicate_export() {
    run_test(
        "export default class Name {}
export function Name() {}",
        true,
    )
    .unwrap();
}
#[test]
#[should_panic = "DuplicateExport"]
fn duplicate_export_prev_decl() {
    run_test(
        "let x = 0;
export { x, x }",
        true,
    )
    .unwrap();
}

#[test]
#[should_panic = "LexicalRedecl"]
fn arrow_fn_dupe_param_lex() {
    run_test("(x, y) => { let x = 0; }", false).unwrap();
}

#[test]
#[should_panic]
fn async_arrow_rest_trailing_comma() {
    run_test("(async (...a,) => { });", false).unwrap()
}

#[test]
#[should_panic]
fn async_function_dupe_param_lex() {
    run_test("async function f(a) { let a; }", false).unwrap();
}

#[test]
fn export_as_as_as() {
    run_test(
        "
var as = null;
export {
    as as as
};",
        true,
    )
    .unwrap();
}

#[test]
fn dupe_var_strict() {
    run_test(
        "'use strict';
function f(a) {
    var a;
}",
        false,
    )
    .unwrap();
}

#[test]
#[should_panic]
fn strict_mode_string_oct_lit() {
    run_test("'use strict';'\\08'", false).unwrap();
}

#[test]
#[should_panic]
fn strict_mode_after_oct_escape() {
    run_test(r#"'\07'; 'use strict';"#, false).unwrap();
}

#[test]
fn obj_init_arg() {
    run_test("({a = 2}) => a", false).unwrap();
}

#[test]
#[should_panic]
fn obj_init_prop_init() {
    run_test("({a = 2})", false).unwrap();
}

#[test]
#[should_panic]
fn strict_fn_body_arguments_as_arg() {
    run_test("function a(arguments) { 'use strict'; }", false).unwrap();
}

#[test]
#[should_panic]
fn dupe_ident_in_loop_left() {
    run_test("for (const [a, a] of []);", false).unwrap();
}

#[test]
fn invalid_group_regression() {
    run_test(r#"var re = /(?<x>a)|b/"#, false).unwrap();
}

#[test]
fn array_for_each() {
    let js = r#"'use strict';
[
        
      ].forEach((s) => {
        try {
          validateNativeFunctionSource(s);
        } catch (unused) {
          $ERROR(`${JSON.stringify(s)} should pass`);
        }
      });
      
      [
        'native code',
        'function() {}',
        'function(){ "native code" }',
        'function(){ [] native code }',
        'function()) { [native code] }',
        'function(() { [native code] }',
        'function []] () { [native code] }',
        'function [[] () { [native code] }',
        'function ["]] () { [native code] }',
        'function [\']] () { [native code] }',
        'function() { [native code] /* }',
        '// function() { [native code] }',
      ].forEach((s) => {
        let fail = false;
        try {
          validateNativeFunctionSource(s);
          fail = true;
        } catch (unused) {}
        if (fail) {
          $ERROR(`${JSON.stringify(s)} should fail`);
        }
      });"#;
    run_test(js, false).unwrap();
}

#[test]
fn async_generator() {
    run_test(
        "export default async function * AG() {}
    AG.foo = '';
    ",
        true,
    )
    .unwrap();
}

fn run_test(js: &str, as_mod: bool) -> Result<(), ressa::Error> {
    let _ = env_logger::try_init();
    let mut p = Parser::builder().js(js).module(as_mod).build()?;
    p.parse()?;
    Ok(())
}
