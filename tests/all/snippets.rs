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
fn assign_multiple_props_nested() {
    run_test(
        "var a = {
        b: c += 1,
        d: e += 1,
      };",
        false,
    )
    .unwrap();
}

#[test]
#[ignore]
fn invalid_group_regression() {
    // TODO: named regex groups
    run_test(r#"var re = /(?<x>a)|b/"#, false).unwrap();
}

#[test]
fn async_func_tokens() {
    let mut p = Parser::builder()
        .js("async function f() {}")
        .build()
        .unwrap();
    let tokens = p.parse().unwrap();
    assert_eq!(
        Program::Script(vec![ProgramPart::Decl(Decl::Func(Func {
            body: FuncBody(vec![]),
            generator: false,
            id: Some(Ident::new("f".to_owned())),
            is_async: true,
            params: vec![],
        }))]),
        tokens
    );
}

#[test]
fn func_decl_tokens() {
    let mut p = Parser::builder().js("function f() {}").build().unwrap();
    let tokens = p.parse().unwrap();
    assert_eq!(
        Program::Script(vec![ProgramPart::Decl(Decl::Func(Func {
            body: FuncBody(vec![]),
            generator: false,
            id: Some(Ident::new("f".to_owned())),
            is_async: false,
            params: vec![],
        }))]),
        tokens
    );
}

#[test]
fn class_extended_by_call() {
    env_logger::try_init().ok();
    let mut p = Parser::builder()
        .js("class C extends D() {}")
        .build()
        .unwrap();
    let tokens = p.parse().unwrap();
    let callee = Ident::new("D".to_owned());
    let callee = Expr::Ident(callee);
    let callee = Box::new(callee);
    let super_class = CallExpr {
        callee,
        arguments: vec![],
    };
    let super_class = Expr::Call(super_class);
    let super_class = Box::new(super_class);
    let super_class = Some(super_class);
    assert_eq!(
        Program::Script(vec![ProgramPart::Decl(Decl::Class(Class {
            body: ClassBody(vec![]),
            id: Some(Ident::new("C".to_owned())),
            super_class,
        }))]),
        tokens
    );
}
#[test]
fn class_anon_extended_by_call() {
    env_logger::try_init().ok();
    let mut p = Parser::builder()
        .js("let c = class extends D() {}")
        .build()
        .unwrap();
    let tokens = p.parse().unwrap();
    let callee = Ident::new("D".to_owned());
    let callee = Expr::Ident(callee);
    let callee = Box::new(callee);
    let super_class = CallExpr {
        callee,
        arguments: vec![],
    };
    let super_class = Expr::Call(super_class);
    let super_class = Box::new(super_class);
    let super_class = Some(super_class);
    assert_eq!(
        Program::Script(vec![ProgramPart::Decl(Decl::Var(
            VarKind::Let,
            vec![VarDecl {
                id: Pat::Ident(Ident::new("c".to_owned())),
                init: Some(Expr::Class(Class {
                    body: ClassBody(vec![]),
                    id: None,
                    super_class,
                }))
            }]
        ))]),
        tokens
    );
}
#[test]
fn class_async_static_method() {
    env_logger::try_init().ok();
    let mut p = Parser::builder()
        .js("class C {
            static async m() {}
        }")
        .build()
        .unwrap();
    let tokens = p.parse().unwrap();

    assert_eq!(
        Program::Script(vec![ProgramPart::Decl(Decl::Class(Class {
            body: ClassBody(vec![Prop {
                key: PropKey::Expr(Expr::Ident(Ident::new("m".to_owned()))),
                value: PropValue::Expr(Expr::Func(Func {
                    id: None,
                    params: vec![],
                    body: FuncBody(vec![]),
                    generator: false,
                    is_async: true
                })),
                kind: PropKind::Method,
                method: true,
                computed: false,
                short_hand: false,
                is_static: true
            }]),
            id: Some(Ident::new("C".to_owned())),
            super_class: None,
        }))]),
        tokens
    );
}

#[test]
fn redecl_error_in_nested_arrow() {
    let js = r#"(() => {
        var a = [1];
        let arrow = (b) => {
            var [b] = a;
        };
    })();"#;
    run_test(js, false).unwrap();
}

#[test]
fn redecl_error_in_nested_funcs() {
    let js = r#"(function() {
        var a = [1];
        let arrow = function(b) {
            var [b] = a;
        };
    })();"#;
    run_test(js, false).unwrap();
}

#[test]
fn generator_prop() {
    env_logger::try_init().ok();
    let mut p = Parser::builder().js("({*g() {}})").build().unwrap();
    let tokens = p.parse().unwrap();

    assert_eq!(
        Program::Script(vec![ProgramPart::Stmt(Stmt::Expr(Expr::Obj(vec![
            ObjProp::Prop(Prop {
                computed: false,
                is_static: false,
                key: PropKey::Expr(Expr::Ident(Ident::new("g".to_owned()))),
                kind: PropKind::Method,
                method: true,
                short_hand: false,
                value: PropValue::Expr(Expr::Func(Func {
                    body: FuncBody(vec![]),
                    generator: true,
                    id: None,
                    is_async: false,
                    params: vec![],
                }))
            })
        ])))]),
        tokens
    );
}

#[test]
fn super_tagged_template_in_ctor() {
    env_logger::try_init().ok();
    let mut p = Parser::builder()
        .js("class X {
            constructor() {
                super()`template`
            }
        }")
        .build()
        .unwrap();
    let tokens = p.parse().unwrap();

    assert_eq!(
        Program::Script(vec![ProgramPart::Decl(Decl::Class(Class {
            id: Some(Ident::from("X")),
            super_class: None,
            body: ClassBody(vec![Prop {
                computed: false,
                is_static: false,
                key: PropKey::Expr(Expr::Ident(Ident::from("constructor"))),
                kind: PropKind::Ctor,
                method: true,
                short_hand: false,
                value: PropValue::Expr(Expr::Func(Func {
                    id: None,
                    generator: false,
                    is_async: false,
                    params: vec![],
                    body: FuncBody(vec![ProgramPart::Stmt(Stmt::Expr(Expr::TaggedTemplate(
                        TaggedTemplateExpr {
                            tag: Box::new(Expr::Call(CallExpr {
                                callee: Box::new(Expr::Super),
                                arguments: vec![],
                            })),
                            quasi: TemplateLit {
                                expressions: vec![],
                                quasis: vec![TemplateElement {
                                    tail: true,
                                    cooked: std::borrow::Cow::Borrowed("template"),
                                    raw: std::borrow::Cow::Borrowed("`template`"),
                                }]
                            }
                        }
                    )))])
                })),
            }])
        }))]),
        tokens
    );
}

#[test]
fn super_in_new_class_expr() {
    env_logger::try_init().ok();
    let mut p = Parser::builder()
        .js("new class extends X { constructor(a = (()=>{ super() })()) { } }")
        .build()
        .unwrap();
    let tokens = p.parse().unwrap();
    let call_super = CallExpr {
        callee: Box::new(Expr::Super),
        arguments: vec![],
    };
    let arrow_call_super = ArrowFuncExpr {
        id: None,
        expression: false,
        generator: false,
        body: ArrowFuncBody::FuncBody(FuncBody(vec![ProgramPart::Stmt(Stmt::Expr(Expr::Call(
            call_super,
        )))])),
        is_async: false,
        params: vec![],
    };
    let arrow_call_super = Expr::ArrowFunc(arrow_call_super);
    let call_arrow = CallExpr {
        callee: Box::new(arrow_call_super),
        arguments: vec![],
    };
    let call_arrow = Expr::Call(call_arrow);
    let assign_left = Box::new(Pat::ident_from("a"));
    let assign_arrow = AssignPat {
        left: assign_left,
        right: Box::new(call_arrow),
    };
    let key = PropKey::Expr(Expr::ident_from("constructor"));
    let value = Func {
        id: None,
        params: vec![FuncArg::Pat(Pat::Assign(assign_arrow))],
        body: FuncBody(vec![]),
        generator: false,
        is_async: false,
    };
    let value = PropValue::Expr(Expr::Func(value));
    let ctor = Prop {
        computed: false,
        is_static: false,
        method: true,
        short_hand: false,
        kind: PropKind::Ctor,
        key,
        value,
    };

    assert_eq!(
        Program::Script(vec![ProgramPart::Stmt(Stmt::Expr(Expr::New(NewExpr {
            arguments: vec![],
            callee: Box::new(Expr::Class(Class {
                id: None,
                super_class: Some(Box::new(Expr::Ident(Ident::from("X")))),
                body: ClassBody(vec![ctor])
            }))
        })))]),
        tokens
    );
}

#[test]
fn static_get_method() {
    env_logger::try_init().ok();
    let mut p = Parser::builder()
        .js("class X {
            static get e() {}
        }")
        .build()
        .unwrap();
    let tokens = p.parse().unwrap();

    assert_eq!(
        Program::Script(vec![ProgramPart::Decl(Decl::Class(Class {
            id: Some(Ident::from("X")),
            super_class: None,
            body: ClassBody(vec![Prop {
                computed: false,
                is_static: true,
                key: PropKey::Expr(Expr::Ident(Ident::from("e"))),
                kind: PropKind::Get,
                method: false,
                short_hand: false,
                value: PropValue::Expr(Expr::Func(Func {
                    id: None,
                    generator: false,
                    is_async: false,
                    params: vec![],
                    body: FuncBody(vec![])
                })),
            }])
        }))]),
        tokens
    );
}

#[test]
fn generator_method() {
    env_logger::try_init().ok();
    let mut p = Parser::builder()
        .js("class X {
            static *e() {}
        }")
        .build()
        .unwrap();
    let tokens = p.parse().unwrap();

    assert_eq!(
        Program::Script(vec![ProgramPart::Decl(Decl::Class(Class {
            id: Some(Ident::from("X")),
            super_class: None,
            body: ClassBody(vec![Prop {
                computed: false,
                is_static: true,
                key: PropKey::Expr(Expr::Ident(Ident::from("e"))),
                kind: PropKind::Method,
                method: true,
                short_hand: false,
                value: PropValue::Expr(Expr::Func(Func {
                    id: None,
                    generator: true,
                    is_async: false,
                    params: vec![],
                    body: FuncBody(vec![])
                })),
            }])
        }))]),
        tokens
    );
}

#[test]
fn export_all() {
    env_logger::try_init().ok();
    let mut p = Parser::builder()
        .js("export * from 'module';")
        .module(true)
        .build()
        .unwrap();
    let tokens = p.parse().unwrap();

    assert_eq!(
        Program::Mod(vec![ProgramPart::Decl(Decl::Export(Box::new(
            ModExport::All(Lit::String(StringLit::Single(Cow::Borrowed("module"))))
        )))]),
        tokens
    );
}

#[test]
fn for_lhs() {
    env_logger::try_init().ok();
    run_test("for(var x=(0 in[])in{});", false).unwrap();
}

#[test]
fn import_default() {
    env_logger::try_init().ok();
    let mut p = Parser::builder()
        .js("import i from 'module'")
        .module(true)
        .build()
        .unwrap();
    let tokens = p.parse().unwrap();

    assert_eq!(
        Program::Mod(vec![ProgramPart::Decl(Decl::Import(Box::new(ModImport {
            source: Lit::String(StringLit::Single(Cow::Borrowed("module"))),
            specifiers: vec![ImportSpecifier::Default(Ident::from("i"))]
        })))]),
        tokens
    );
}

#[test]
fn loop_yield() {
    env_logger::try_init().ok();
    let mut p = Parser::builder()
        .js("var x = {
            *['y']() {
                yield 0;
                yield 0;
            }
        };")
        .build()
        .unwrap();
    let tokens = p.parse().unwrap();
    assert_eq!(
        Program::Script(vec![ProgramPart::Decl(Decl::Var(
            VarKind::Var,
            vec![VarDecl {
                id: Pat::Ident(Ident::from("x")),
                init: Some(Expr::Obj(vec![ObjProp::Prop(Prop {
                    key: PropKey::Lit(Lit::String(StringLit::Single(Cow::Borrowed("y")))),
                    computed: true,
                    is_static: false,
                    kind: PropKind::Method,
                    method: true,
                    short_hand: false,
                    value: PropValue::Expr(Expr::Func(Func {
                        id: None,
                        params: vec![],
                        body: FuncBody(vec![
                            ProgramPart::Stmt(Stmt::Expr(Expr::Yield(YieldExpr {
                                argument: Some(Box::new(Expr::Lit(Lit::Number(Cow::Borrowed(
                                    "0"
                                ))))),
                                delegate: false,
                            }))),
                            ProgramPart::Stmt(Stmt::Expr(Expr::Yield(YieldExpr {
                                argument: Some(Box::new(Expr::Lit(Lit::Number(Cow::Borrowed(
                                    "0"
                                ))))),
                                delegate: false,
                            }))),
                        ]),
                        generator: true,
                        is_async: false,
                    }))
                })]))
            }]
        ))]),
        tokens
    );
}

#[test]
fn obj_expr_stmt() {
    use resast::spanned::{
        expr::{Expr, ObjExpr, WrappedExpr},
        stmt::Stmt,
        Program, ProgramPart, Slice, SourceLocation,
    };
    use ressa::spanned::Parser;
    env_logger::try_init().ok();
    let mut p = Parser::builder().js("({});").build().unwrap();
    let tokens = p.parse().unwrap();
    let open_brace = Slice {
        source: "{".into(),
        loc: SourceLocation::new(1, 2, 1, 3),
    };
    let close_brace = Slice {
        source: "}".into(),
        loc: SourceLocation::new(1, 3, 1, 4),
    };
    let obj = ObjExpr {
        open_brace,
        close_brace,
        props: vec![],
    };
    let expr = Expr::Obj(obj);
    let wrapped = WrappedExpr {
        open_paren: Slice {
            source: "(".into(),
            loc: SourceLocation::new(1, 1, 1, 2),
        },
        expr,
        close_paren: Slice {
            source: ")".into(),
            loc: SourceLocation::new(1, 4, 1, 5),
        },
    };
    let expr = Expr::Wrapped(Box::new(wrapped));
    assert_eq!(
        Program::Script(vec![ProgramPart::Stmt(Stmt::Expr {
            expr,
            semi_colon: Some(Slice {
                source: ";".into(),
                loc: SourceLocation::new(1, 5, 1, 6)
            })
        })]),
        tokens
    );
}

#[test]
#[ignore = "Diagnostic to see how badly our recursive decent is performing"]
fn blow_the_stack() {
    fn do_it(ct: usize) {
        println!("do_it {}", ct);
        let mut js = String::from("function x() {");
        for _i in 1..ct {
            js.push_str("return function() {");
        }
        for _i in 0..ct {
            js.push('}');
        }
        run_test(&js, false).unwrap();
    }
    for i in 1..100 {
        do_it(i)
    }
}

fn run_test(js: &str, as_mod: bool) -> Result<(), ressa::Error> {
    let _ = env_logger::try_init();
    let mut p = Parser::builder().js(js).module(as_mod).build()?;
    p.parse()?;
    Ok(())
}
