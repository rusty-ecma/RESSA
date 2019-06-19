#![cfg(test)]
use env_logger;
use ressa::Parser;

// #[test]
// fn parse_directive_prologues() {
//     let js = "'use strict'";
//     let expectation = resast::ref_tree::Program::Script(vec![resast::ref_tree::ProgramPart::use_strict(false)]);
//     execute(js, expectation);
//     let js = r#""use strict";"#;
//     let expectation = resast::ref_tree::Program::Script(vec![resast::ref_tree::ProgramPart::use_strict(true)]);
//     execute(js, expectation);
//     let js = "'use strict';\n'use strict';";
// let expectation = resast::ref_tree::Program::Script(vec![
//     resast::ref_tree::ProgramPart::Stmt(resast::ref_tree::stmt::Stmt::S,
//     resast::ref_tree::ProgramPart::use_strict(false),
// ]);
// execute(js, expectation);
// }

// #[test]
// fn parse_with_stmt() {
//     let js = "with (Math) {
//         floor(PI * random())
//         }";
//     let random = Expr::call(Expr::ident("random"), vec![]);
//     let pi = Expr::ident("PI");
//     let bin = Expr::binary(pi, BinaryOperator::Times, random);
//     let floor = Expr::call(Expr::ident("floor"), vec![bin]);
//     let part = ProgramPart::Stmt(Stmt::Expr(floor));
//     let body = Stmt::Block(vec![part]);
//     let stmt = Stmt::with(Expr::ident("Math"), body);
//     let part = ProgramPart::Stmt(stmt);
//     let expectation = Program::Script(vec![part]);
//     execute(js, expectation);
// }

// #[test]
// fn parse_while_stmt() {
//     let js = "while (true) {
//         console.log('false');
//         break;
//     }";
//     let test = Expr::boolean(true);
//     let bk = Stmt::Break(None);
//     let log_args = Expr::string("'false'");
//     let member = Expr::member(Expr::ident("console"), Expr::ident("log"), false);
//     let call = Expr::call(member, vec![log_args]);
//     let call = Stmt::Expr(call);
//     let part = ProgramPart::Stmt(call);
//     let part2 = ProgramPart::Stmt(bk);
//     let body = Stmt::Block(vec![part, part2]);
//     let stmt = Stmt::while_stmt(test, body);
//     let part = ProgramPart::Stmt(stmt);
//     let expectation = Program::Script(vec![part]);
//     execute(js, expectation);
// }

// #[test]
// fn parse_while_stmt_2() {
//     let js = "
//     while (Math.random() > 0.1) {
//         console.log('loop');
//     }";
//     let math_random = Expr::call(
//         Expr::member(Expr::ident("Math"), Expr::ident("random"), false),
//         vec![],
//     );
//     let test = Expr::binary(
//         math_random,
//         BinaryOperator::GreaterThan,
//         Expr::number("0.1"),
//     );
//     let lp = Expr::string("'loop'");
//     let console_log = Expr::call(
//         Expr::member(Expr::ident("console"), Expr::ident("log"), false),
//         vec![lp],
//     );
//     let body = Stmt::Block(vec![ProgramPart::Stmt(Stmt::Expr(console_log))]);
//     let while_loop = Stmt::while_stmt(test, body);
//     let part = ProgramPart::Stmt(while_loop);
//     execute(js, Program::Script(vec![part]));
// }

// #[test]
// fn parse_var_stmt() {
//     let js = "var i;";
//     let decl = VariableDecl::uninitialized("i");
//     let stmt = Stmt::Var(vec![decl]);
//     let part = ProgramPart::Stmt(stmt);
//     let program = Program::Script(vec![part]);
//     execute(js, program);
// }

// #[test]
// fn parse_var_stmt_2() {
//     let js = "var i = 0;";
//     let decl = VariableDecl::with_value("i", Expr::number("0"));
//     let stmt = Stmt::Var(vec![decl]);
//     let part = ProgramPart::Stmt(stmt);
//     let program = Program::Script(vec![part]);
//     execute(js, program);
// }

// #[test]
// fn parse_var_stmt_3() {
//     let js = "var a, b, c, d = 22";
//     let stmt = Stmt::Var(vec![
//         VariableDecl::uninitialized("a"),
//         VariableDecl::uninitialized("b"),
//         VariableDecl::uninitialized("c"),
//         VariableDecl::with_value("d", Expr::number("22")),
//     ]);
//     let part = ProgramPart::Stmt(stmt);
//     let program = Program::Script(vec![part]);
//     execute(js, program);
// }

// #[test]
// fn parse_var_stmt_fn() {
//     let js = "var fn = function (one, two) {
//         return one + two;
//         }";
//     let addition = Expr::binary(Expr::ident("one"), BinaryOperator::Plus, Expr::ident("two"));
//     let body = vec![ProgramPart::Stmt(Stmt::Return(Some(addition)))];
//     let func = Expr::function(
//         None,
//         vec![FunctionArg::ident("one"), FunctionArg::ident("two")],
//         body,
//         false,
//         false,
//     );
//     let v = Stmt::Var(vec![VariableDecl::with_value("fn", func)]);
//     let part = ProgramPart::Stmt(v);
//     let program = Program::Script(vec![part]);
//     execute(js, program);
// }

// #[test]
// fn parse_var_stmt_fn_2() {
//     let js = "var fn = function* x() {
//         yield 'one';
//         yield 'two';
//     }";

//     let one = Expr::yield_with_arg(Expr::string("'one'"), false);
//     let two = Expr::yield_with_arg(Expr::string("'two'"), false);
//     let body = vec![
//         ProgramPart::Stmt(Stmt::Expr(one)),
//         ProgramPart::Stmt(Stmt::Expr(two)),
//     ];
//     let func = Expr::function(Some("x".to_string()), vec![], body, true, false);
//     let v = Stmt::Var(vec![VariableDecl::with_value("fn", func)]);
//     let part = ProgramPart::Stmt(v);
//     let program = Program::Script(vec![part]);
//     execute(js, program);
// }

// #[test]
// fn parse_var_stmt_destructure() {
//     let js = "var {a, b, c} = {a: 0, b: 1, c: 2}";
//     let init = vec![
//         ObjectProperty::number("a", "0"),
//         ObjectProperty::number("b", "1"),
//         ObjectProperty::number("c", "2"),
//     ];
//     let decl = VariableDecl::destructed(&["a", "b", "c"], init);
//     let stmt = Stmt::Var(vec![decl]);
//     let program = Program::Script(vec![ProgramPart::Stmt(stmt)]);
//     execute(js, program);
// }

// #[test]
// fn parse_var_stmt_destructure_rest() {
//     let js = "var {a, b, c, ...arg} = {a: 0, b: 1, c: 2, d: 3, e: 4}";
//     let init = vec![
//         ObjectProperty::number("a", "0"),
//         ObjectProperty::number("b", "1"),
//         ObjectProperty::number("c", "2"),
//         ObjectProperty::number("d", "3"),
//         ObjectProperty::number("e", "4"),
//     ];
//     let decl = VariableDecl::destructed_with_rest(&["a", "b", "c"], "arg", init);
//     let stmt = Stmt::Var(vec![decl]);
//     let program = Program::Script(vec![ProgramPart::Stmt(stmt)]);
//     execute(js, program);
// }

// #[test]
// fn parse_try_stmt() {
//     let js = "try {
//             console.log('trying');
//         } finally {
//             console.log('done trying');
//         }";

//     parse(js);
// }

// #[test]
// fn parse_try_stmt_2() {
//     let js = "try {
//         console.log('trying');
//     } catch (e) {
//         console.log('caught', e);
//     }";
//     parse(js);
// }
// #[test]
// fn parse_labeled_stmt_lf() {
//     let js = "linefeed:0\n0;";
//     let program = Program::Script(vec![
//         ProgramPart::Stmt(Stmt::Labeled(LabeledStmt {
//             label: "linefeed".to_string(),
//             body: Box::new(Stmt::Expr(Expr::Literal(Literal::Number(String::from(
//                 "0",
//             ))))),
//         })),
//         ProgramPart::Stmt(Stmt::Expr(Expr::Literal(Literal::Number(String::from(
//             "0",
//         ))))),
//     ]);
//     execute(js, program);
// }

// #[test]
// fn parse_var_ident_fn() {
//     let _ = env_logger::try_init();
//     let js = "({var(a, b = 0, [c,, d = 0, ...e], {f, g: h, i = 0, i: j = 0}, ...k){}})";
//     parse(js);
// }

// #[test]
// fn parse_arg_ident_assign() {
//     let _ = env_logger::try_init();
//     let js = "({
//         var({i = 0, i: j = 0}) {

//         }
//     })";
//     parse(js);
// }

// #[test]
// fn parse_nested_delegated_yield() {
//     let _ = env_logger::try_init();
//     let js = "function*g(a, b = 0, [c,, d = 0, ...e], {f, g: h, i = 0, i: j = 0}, ...k){
//   return a = yield* b = yield c = yield yield;
// }";
//     parse(js);
// }
// #[test]
// fn parse_obj_pattern_fn_fat_arrow() {
//     let _ = env_logger::try_init();
//     let js = "({i = 0, i: j = 0}) => {;};";
//     parse(js);
// }
// #[test]
// fn parse_obj_pat_fn_fat_arrow2() {
//     let _ = env_logger::try_init();
//     let js = "({x}) => ({x});";

//     parse(js);
// }

// #[test]
// fn parse_super_call() {
//     let _ = env_logger::try_init();
//     let js = "class A extends B {
//         constructor() {
//             super(new.target);
//         }
//     }";
//     parse(js);
// }

// #[test]
// fn parse_delete_comma_op() {
//     let _ = env_logger::try_init();
//     let js = "remove: function(a){
//         if(l<Number.MAX_VALUE){
//             var b=m[a];
//             if(!b)return;
//             b==n && (n=b.p);
//             b==p && (p=b.n);
//             f(b.n,b.p);
//             delete m[a]
//         }
//         a in k&&(delete k[a],g--)
//     }";
//     parse(js);
// }

// #[test]
// fn doc_snippet() {
//     let js = "function helloWorld() { alert('Hello world'); }";
//     let p = Parser::new(&js).unwrap();
//     let f = ProgramPart::decl(Decl::Function(Function {
//         id: Some("helloWorld".to_string()),
//         params: vec![],
//         body: vec![ProgramPart::Stmt(Stmt::Expr(Expr::call(
//             Expr::ident("alert"),
//             vec![Expr::string("'Hello world'")],
//         )))],
//         generator: false,
//         is_async: false,
//     }));
//     for part in p {
//         assert_eq!(part.unwrap(), f);
//     }
// }

// #[test]
// fn builder_doc_snippet() {
//     use ressa::Builder;
//     let js = "for (var i = 0; i < 100; i++) {
//         console.log('loop', i);
//         }";
//     let p = Builder::new().module(false).js(js).build().unwrap();
//     for part in p {
//         let var = VariableDecl {
//             id: Pat::Identifier("i".to_string()),
//             init: Some(Expr::Literal(Literal::Number("0".to_string()))),
//         };
//         let test = Expr::Binary(BinaryExpr {
//             left: Box::new(Expr::Ident("i".to_string())),
//             operator: BinaryOperator::LessThan,
//             right: Box::new(Expr::Literal(Literal::Number("100".to_string()))),
//         });
//         let body = Box::new(Stmt::Block(vec![ProgramPart::Stmt(Stmt::Expr(
//             Expr::Call(CallExpr {
//                 callee: Box::new(Expr::Member(MemberExpr {
//                     object: Box::new(Expr::Ident("console".to_string())),
//                     property: Box::new(Expr::Ident("log".to_string())),
//                     computed: false,
//                 })),
//                 arguments: vec![
//                     Expr::Literal(Literal::String("'loop'".to_string())),
//                     Expr::Ident("i".to_string()),
//                 ],
//             }),
//         ))]));
//         let expecation = ProgramPart::Stmt(Stmt::For(ForStmt {
//             init: Some(LoopInit::Variable(VariableKind::Var, vec![var])),
//             test: Some(test),
//             update: Some(Expr::Update(UpdateExpr {
//                 operator: UpdateOperator::Increment,
//                 argument: Box::new(Expr::Ident("i".to_string())),
//                 prefix: false,
//             })),
//             body,
//         }));
//         assert_eq!(part.unwrap(), expecation);
//     }
// }

// #[test]
// fn parse_doc_example() {
//     let js = "function helloWorld() { alert('Hello world'); }";
//     let mut p = Parser::new(&js).unwrap();
//     let call = CallExpr {
//         callee: Box::new(Expr::Ident(String::from("alert"))),
//         arguments: vec![Expr::Literal(Literal::String(String::from(
//             "'Hello world'",
//         )))],
//     };
//     let expectation = Program::Script(vec![ProgramPart::Decl(Decl::Function(Function {
//         id: Some("helloWorld".to_string()),
//         params: vec![],
//         body: vec![ProgramPart::Stmt(Stmt::Expr(Expr::Call(call)))],
//         generator: false,
//         is_async: false,
//     }))]);
//     let program = p.parse().unwrap();
//     assert_eq!(program, expectation);
// }

// #[test]
// fn readme_example() {
//     let js = "
// function Thing() {
//     return 'stuff';
// }
// ";
//     let parser = Parser::new(js).expect("Failed to create parser");
//     for part in parser {
//         let part = part.expect("Error parsing part");
//         match part {
//             ProgramPart::Decl(decl) => match decl {
//                 Decl::Function(f) => {
//                     if let Some(ref id) = f.id {
//                         assert_eq!(id, "Thing");
//                     }
//                     assert!(f.params.len() == 0);
//                     assert!(!f.generator);
//                     assert!(!f.is_async);
//                     for part in f.body {
//                         match part {
//                             ProgramPart::Stmt(stmt) => match stmt {
//                                 Stmt::Return(expr) => {
//                                     if let Some(expr) = expr {
//                                         match expr {
//                                             Expr::Literal(lit) => match lit {
//                                                 Literal::String(value) => {
//                                                     assert_eq!(value, String::from("'stuff'"))
//                                                 }
//                                                 _ => (),
//                                             },
//                                             _ => (),
//                                         }
//                                     }
//                                 }
//                                 _ => (),
//                             },
//                             _ => (),
//                         }
//                     }
//                 }
//                 _ => (),
//             },
//             _ => (),
//         }
//     }
// }

// #[test]
// fn first_blog_post() {
//     let expect = ProgramPart::Decl(Decl::Function(Function {
//         id: Some(String::from("print")),
//         params: vec![FunctionArg::Pat(Pat::Identifier(String::from("message")))],
//         body: vec![ProgramPart::Stmt(Stmt::Expr(Expr::Call(CallExpr {
//             callee: Box::new(Expr::Member(MemberExpr {
//                 object: Box::new(Expr::Ident(String::from("console"))),
//                 property: Box::new(Expr::Ident(String::from("log"))),
//                 computed: false,
//             })),
//             arguments: vec![Expr::Ident(String::from("message"))],
//         })))],
//         generator: false,
//         is_async: false,
//     }));
//     let js = "function print(message) {
//     console.log(message)
// }";
//     execute(js, Program::Script(vec![expect]));
// }

// #[test]
// fn obj_pat() {
//     let js = "console.log({a: 'thing', b() {console.log('b')}});";
//     let out = parse(js);
//     println!("{:?}", out);
// }

// #[test]
// fn loop_decl_error() {
//     let js = "function dependArray (value) {
//   for (let e, i = 0, l = value.length; i < l; i++) {
//     e = value[i];
//     e && e.__ob__ && e.__ob__.dep.depend();
//     if (Array.isArray(e)) {
//       dependArray(e);
//     }
//   }
// }";
//     let mut p = ressa::Builder::new().module(true).js(js).build().unwrap();
//     let _ = p.parse().unwrap();
// }

// #[test]
// fn template_tail_error() {
//     let _ = env_logger::try_init();
//     let js = "function getRawDirName (dir) {
//   return dir.rawName || `${dir.name}.${Object.keys(dir.modifiers || {}).join('.')}`
// }";
//     let mut p = ressa::Builder::new().module(true).js(js).build().unwrap();
//     let _ = p.parse().unwrap();
// }

// #[test]
// fn ref_template() {
//     let _ = env_logger::try_init();
//     let js = r#"`\0\n\x0A\u000A\u{A}`"#;
//     let p = ressa::Builder::new().module(true).js(js).build().unwrap();
//     for part in p {
//         println!("{:#?}", part.unwrap());
//     }
// }

// // #[test]
// // fn comment_handler_test() {
// //     use ress::Item;
// //     let js = "//things
// //     /* things */
// //     <!-- things -->";
// //     let mut i = 0;
// //     let expectation = [
// //         ress::Comment::new_single_line("//things"),
// //         ress::Comment::new_multi_line("/* things */"),
// //         ress::Comment::new_html_no_tail("<!-- things -->"),
// //     ];
// //     let mut p = ressa::Builder::new()
// //         .js(js)
// //         .with_comment_handler(|item| {
// //             if let ress::RefToken::Comment(ref c) = item.token {
// //                 assert_eq!(c, &expectation[i])
// //             }
// //             i += 1;
// //         })
// //         .unwrap();
// //     p.parse().unwrap();
// // }

// #[test]
// fn class_expr() {
//     let _ = env_logger::try_init();
//     let js = r#"for(let {a = new class extends Array { constructor(b = (a = eval("()=>super()"))){} }} of [[]]);"#;
//     println!("{:#?}", parse(js));
// }

// #[test]
// fn class_expr_pat() {
//     let _ = env_logger::try_init();
//     let js = "for ([class get {} ().iterator] of []);";
//     println!("{:#?}", parse(js));
// }

// #[test]
// fn expr_to_pat() {
//     let _ = env_logger::try_init();
//     let js = "let x = { [Symbol.species]: function(length) { return; } }";
//     println!("{:#?}", parse(js));
// }

// #[test]
// fn non_strict_mode_keywords() {
//     let _ = env_logger::try_init();
//     let js = "var f = package => 0;";
//     println!("{:#?}", parse(js));
// }

// #[test]
// fn obj_literal_in_binary_op() {
//     let _ = env_logger::try_init();
//     let js = r#""" + {toString: Date.prototype.toJSON};"#;
//     println!("{:#?}", parse(js));
// }

// #[test]
// fn template_middle() {
//     let _ = env_logger::try_init();
//     let js = r#"let bitval = `(${prefix}.const ${format(val, i)})`"#;
//     println!("{:#?}", parse(js));
// }

// #[test]
// fn loop_left_expr() {
//     let _ = env_logger::try_init();
//     let js = r#"for (t[t++] in object);"#;
//     println!("{:#?}", parse(js));
// }
// #[test]
// fn for_loop_nonsense() {
//     let _ = env_logger::try_init();
//     let js = "for (var i = 1000; i-- > 0; ) {
//     expr = `(f32.neg ${expr})`;
// }";
//     println!("{:#?}", parse(js));
// }

// #[test]
// fn function_param_pat() {
//     let _ = env_logger::try_init();
//     let js = r#"function f(x, await = () => Array.isArray(revocable.proxy), ...get) {
//     dbg.getNewestFrame().older.eval("print(a)");
// }"#;
//     println!("{:#?}", parse(js));
// }

// #[test]
// fn more_for_loop_nonsense() {
//     let _ = env_logger::try_init();
//     let js = "for (f().x of [])
//     s += '.';";
//     println!("{:#?}", parse(js));
// }

// #[test]
// fn deeply_nested_template() {
//     let _ = env_logger::try_init();
//     let js = r#"`
//  (func ${name} (param $barrierValue i32) (result i32)
//    (local $n i32)
//    (local $tmp ${prefix})
//    (set_local $n (i32.const ${ITERATIONS}))
//    (loop $outer
//     (if (get_local $n)
//         (block
//          ${isMaster ? `;; Init
// (${prefix}.atomic.store${tag} ${loc} (${prefix}.const ${distribute(initial)}))` : ``}
//          ${barrier}

// ${(() => {
//     let s = `;; Do\n`;
//     for (let i=0; i < NUMVALS; i++) {
//         let bitval = `(${prefix}.const ${format(val, i)})`
//         // The load must be atomic though it would be better if it were relaxed,
//         // we would avoid fences in that case.
//         if (op.match(/cmpxchg/)) {
//             s += `(loop $doit
//                    (set_local $tmp (${prefix}.atomic.load${tag} ${loc}))
//                    (br_if $doit (i32.eqz
//                                  (${prefix}.eq
//                                   (get_local $tmp)
//                                   (${op} ${loc} (get_local $tmp) (${prefix}.or (get_local $tmp) ${bitval}))))))
//             `;
//         } else {
//             s += `(drop (${op} ${loc} ${bitval}))
//             `;
//        }
//      }
//     return s
// })()}
//          (loop $wait_done
//           (br_if $wait_done (${prefix}.ne (${prefix}.atomic.load${tag} ${loc}) (${prefix}.const ${distribute(expected)}))))
//          ${barrier}
//          (set_local $n (i32.sub (get_local $n) (i32.const 1)))
//          (br $outer))))
//   (get_local $barrierValue))`"#;
//     println!("{:#?}", parse(js));
// }
// #[test]
// fn ghost_semi_colon() {
//     let _ = env_logger::try_init();
//     let js = "function() {
//     'string' + 0;
// }";
//     println!("{:#?}", parse(js));
// }

// #[test]
// fn spread_param_in_complicated_params() {
//     let _ = env_logger::try_init();
//     let js = "(...k) => {;};";
//     println!("{:#?}", parse(js));
// }

// #[test]
// fn assignment_in_complicated_params() {
//     let _ = env_logger::try_init();
//     let js = "({f, g: h, i = 0, i: j = 0}) => {;}";
//     println!("{:#?}", parse(js));
// }

// #[test]
// fn less_than() {
//     let _ = env_logger::try_init();
//     let js = "0>>0; 0>>>0;

// 0<0;";
//     println!("{:#?}", parse(js));
// }
// #[test]
// fn ref_regex() {
//     let _ = env_logger::try_init();
//     let js = r#"var
// 	r20 = /%20/g,
// 	rhash = /#.*$/,
// 	rantiCache = /([?&])_=[^&]*/,
// 	rheaders = /^(.*?):[ \t]*([^\r\n]*)$/mg,

// 	// #7653, #8125, #8152: local protocol detection
// 	rlocalProtocol = /^(?:about|app|app-storage|.+-extension|file|res|widget):$/,
// 	rnoContent = /^(?:GET|HEAD)$/,
// 	rprotocol = /^\/\//,

// 	/* Prefilters
// 	 * 1) They are useful to introduce custom dataTypes (see ajax/jsonp.js for an example)
// 	 * 2) These are called:
// 	 *    - BEFORE asking for a transport
// 	 *    - AFTER param serialization (s.data is a string if s.processData is true)
// 	 * 3) key is the dataType
// 	 * 4) the catchall symbol "*" can be used
// 	 * 5) execution will start with transport dataType and THEN continue down to "*" if needed
// 	 */
// 	prefilters = {},

// 	/* Transports bindings
// 	 * 1) key is the dataType
// 	 * 2) the catchall symbol "*" can be used
// 	 * 3) selection will start with transport dataType and THEN go to "*" if needed
// 	 */
// 	transports = {},

// 	// Avoid comment-prolog char sequence (#10098); must appease lint and evade compression
// 	allTypes = "*/".concat( "*" ),

// 	// Anchor tag for parsing the document origin
// 	originAnchor = document.createElement( "a" );
// 	originAnchor.href = location.href;"#;
//     println!("{:#?}", parse(js));
// }
// #[test]
// fn job_queue() {
//     let _ = env_logger::try_init();
//     let js = "var log = '';
// async function f(label, k) {
//   log += label + '1';
//   await 1;
//   log += label + '2';
//   await 1;
//   log += label + '3';

//   return k();
// }";
//     println!("{:?}", parse(js));
// }

// #[test]
// fn another_async_fn_failure() {
//     let _ = env_logger::try_init();
//     let js =
//         r#"// Test uncatchable error when a stream's queuing strategy's size() method is called.

// // Make `debugger;` raise an uncatchable exception.
// let g = newGlobal({newCompartment: true});
// g.parent = this;
// g.hit = false;
// g.eval(`
//     var dbg = new Debugger(parent);
//     dbg.onDebuggerStatement = (_frame, exc) => (hit = true, null);
// `);

// let fnFinished = false;
// async function fn() {
//     // Await once to postpone the uncatchable error until we're running inside
//     // a reaction job. We don't want the rest of the test to be terminated.
//     // (`drainJobQueue` catches uncatchable errors!)
//     await 1;

//     try {
//         // Create a stream with a strategy whose .size() method raises an
//         // uncatchable exception, and have it call that method.
//         new ReadableStream({
//             start(controller) {
//                 controller.enqueue("FIRST POST");  // this calls .size()
//             }
//         }, {
//             size() {
//                 debugger;
//             }
//         });
//     } finally {
//         fnFinished = true;
//     }
// }

// fn();
// drainJobQueue();
// assertEq(g.hit, true);
// assertEq(fnFinished, false);
// "#;
//     println!("{:?}", parse(js));
// }

// #[test]
// fn async_method_failure() {
//     let _ = env_logger::try_init();
//     let js = r#"class X {
//     async ["foo"]() {
//         return eval();
//     }
// }"#;
//     println!("{:?}", parse(js));
// }

// #[test]
// fn new_member_expr_failure() {
//     let _ = env_logger::try_init();
//     let js =
// "function isElement(node) {
//   return !!(node &&
//     (node.nodeName  // We are a direct element.
//     || (node.prop && node.attr && node.find)));  // We have an on and find method part of jQuery API.
// }";
//     println!("{:#?}", parse(js));
// }

// #[test]
// fn big_int() {
//     let _ = env_logger::try_init();
//     let js = "function big_one() {
//     return 1n;
// }";
//     let expectation = Program::Script(vec![ProgramPart::decl(Decl::Function(Function {
//         id: Some("big_one".to_string()),
//         generator: false,
//         is_async: false,
//         params: vec![],
//         body: vec![ProgramPart::stmt(Stmt::Return(Some(Expr::Literal(
//             Literal::Number("1n".to_string()),
//         ))))],
//     }))]);
//     execute(js, expectation);
// }

// #[test]
// fn big_int_fails_as_obj_key() {
//     let js = "{ 1n: 2n }";

//     let p = Parser::new(js).unwrap();
//     for r in p {
//         println!("{:?}", r);
//         assert!(!r.is_ok());
//     }
// }

// fn execute(js: &str, expectation: Program) {
//     let s = parse(js);
//     assert_eq!(s, expectation);
// }

// fn parse(js: &str) -> Program {
//     let mut p = Parser::new(js).unwrap();
//     let s = p.parse().unwrap();
//     s
// }
