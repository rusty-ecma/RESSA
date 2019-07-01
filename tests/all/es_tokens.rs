use resast::ref_tree::prelude::*;
use resast::expr::{
    PropertyKind,
    AssignmentOperator,
    UpdateOperator,
    UnaryOperator,
};
type Part = ProgramPart<'static>;
type S = Stmt<'static>;
type E = Expr<'static>;
type L = Literal<'static>;
type OP = ObjectProperty<'static>;
lazy_static! {
    pub static ref ES5: Vec<ProgramPart<'static>> =
        vec![
            labeled_statement("tab"),
            labeled_statement("verticalTab"),
            labeled_statement("formFeed"),
            labeled_statement("space"),
            labeled_statement("nbsp"),
            labeled_statement("bom"),
            line_term("lineFeed"),
            number_literal_part("0"),
            line_term("carriageReturn"),
            number_literal_part("0"),
            line_term("carriageReturnLineFeed"),
            number_literal_part("0"),
            line_term("lineSeparator"),
            number_literal_part("0"),
            line_term("paragraphSeparator"),
            number_literal_part("0"),
            var_decl_one(),
            var_decl_two(),
            null_literal_part(),
            bool_literal_part(true),
            bool_literal_part(false),
            number_literal_part("0"),
            number_literal_part("00"),
            number_literal_part("1234567890"),
            number_literal_part("01234567"),
            number_literal_part("0."),
            number_literal_part("0.00"),
            number_literal_part("10.00"),
            number_literal_part(".0"),
            number_literal_part(".00"),
            number_literal_part("0e0"),
            number_literal_part("0E0"),
            number_literal_part("0.e0"),
            number_literal_part("0.00e+0"),
            number_literal_part(".00e-0"),
            number_literal_part("0x0"),
            number_literal_part("0X0"),
            number_literal_part("0x0123456789abcdefABCDEF"),
            number_literal_part("2e308"),
            string_literal_part(r#""""#,),
            string_literal_part(r#""'""#,),
            string_literal_part(r#""\'\"\\\b\f\n\r\t\v\0""#,),
            string_literal_part(r#""\1\00\400\000""#,),
            string_literal_part(r#""\x01\x23\x45\x67\x89\xAB\xCD\xEF""#,),
            string_literal_part(r#""\u0123\u4567\u89AB\uCDEF""#,),
            string_literal_part(r#""\
""#),
            string_literal_part(r"''"),
            string_literal_part(r#"'"'"#),
            string_literal_part(r#"'\'\"\\\b\f\n\r\t\v\0'"#),
            string_literal_part(r#"'\1\00\400\000'"#),
            string_literal_part(r#"'\x01\x23\x45\x67\x89\xAB\xCD\xEF'"#),
            string_literal_part(r#"'\u0123\u4567\u89AB\uCDEF'"#),
            string_literal_part(r#"'\
'"#),
            regex_literal_part(r#"x"#, ""),
            regex_literal_part(r#"|"#, ""),
            regex_literal_part(r#"|||"#, ""),
            regex_literal_part(r#"^$\b\B"#, ""),
            regex_literal_part(r#"(?=(?!(?:(.))))"#, ""),
            regex_literal_part(r#"a.\f\n\r\t\v\0\[\-\/\\\x00\u0000"#,""),
            regex_literal_part(r#"\d\D\s\S\w\W"#, ""),
            regex_literal_part(r#"\ca\cb\cc\cd\ce\cf\cg\ch\ci\cj\ck\cl\cm\cn\co\cp\cq\cr\cs\ct\cu\cv\cw\cx\cy\cz"#, ""),
            regex_literal_part(r#"\cA\cB\cC\cD\cE\cF\cG\cH\cI\cJ\cK\cL\cM\cN\cO\cP\cQ\cR\cS\cT\cU\cV\cW\cX\cY\cZ"#, ""),
            regex_literal_part(r#"[a-z-]"#,""),
            regex_literal_part(r#"[^\b\-^]"#,""),
            regex_literal_part(r#"[/\]\\]"#, ""),
            regex_literal_part(r#"."#, "i"),
            regex_literal_part(r#"."#, "g"),
            regex_literal_part(r#"."#, "m"),
            regex_literal_part(r#"."#, "igm"),
            regex_literal_part(r#".*"#, ""),
            regex_literal_part(r#".*?"#, ""),
            regex_literal_part(r#".+"#, ""),
            regex_literal_part(r#".+?"#, ""),
            regex_literal_part(r#".?"#, ""),
            regex_literal_part(r#".??"#, ""),
            regex_literal_part(r#".{0}"#, ""),
            regex_literal_part(r#".{0,}"#, ""),
            regex_literal_part(r#".{0,0}"#, ""),
            this_part(),
            ident_stmt("x"),
            array(vec![]),
            // TODO: Double Check this
            array(vec![None]),
            array(vec![Some(number_literal_expr("0"))]),
            array(vec![Some(number_literal_expr("0"))]),
            array(vec![None, Some(number_literal_expr("0"))]),
            array(vec![Some(number_literal_expr("0")), Some(number_literal_expr("0"))]),
            array(vec![Some(number_literal_expr("0")), Some(number_literal_expr("0"))]),
            array(vec![Some(number_literal_expr("0")), None, Some(number_literal_expr("0"))]),
            array(vec![None, None]),
            //^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            obj_literal_part(vec![]),
            obj_literal_part(vec![
                obj_prop_ident_number("x", "0")
            ]),
            obj_literal_part(vec![
                obj_prop_ident_number("x", "0"),
                obj_prop_ident_number("y", "0"),
            ]),
            obj_literal_part(vec![
                obj_prop_ident_number("x", "0")
            ]),
            obj_literal_part(vec![
                obj_prop_str_number("'x'", "0"),
                obj_prop_str_number("\"y\"", "0"),
                obj_prop_ident_number("var", "0"),
            ]),
            obj_literal_part(vec![
                obj_prop_number_number("0", "0"),
            ]),
            obj_literal_part(vec![
                obj_prop_number_number("0.", "0"),
            ]),
            obj_literal_part(vec![
                obj_prop_number_number("0.0", "0"),
            ]),
            obj_literal_part(vec![
                obj_prop_number_number(".0", "0"),
            ]),
            obj_literal_part(vec![
                obj_prop_number_number("0e0", "0"),
            ]),
            obj_literal_part(vec![
                obj_prop_number_number("0x0", "0"),
            ]),
            obj_literal_part(vec![
                obj_prop_ident_getter("x"),
                obj_prop_ident_setter("x", vec![
                    fn_arg_ident_pat("a")
                ]),
                obj_prop_str_getter("'y'"),
                obj_prop_str_setter("\"y\"", vec![
                    fn_arg_ident_pat("a")
                ]),
                obj_prop_number_getter("0"),
                obj_prop_number_setter("0", vec![
                    fn_arg_ident_pat("a")
                ]),
                obj_prop_ident_getter("var"),
                obj_prop_ident_setter("var", vec![
                    fn_arg_ident_pat("x")
                ]),
            ]),
            member_number_ident_part("0.", "a"),
            member_number_number_part("0", "0"),
            assign_part(
                assign_left_ident("x"), 
                func_expr(
                    "f", vec![], vec![
                        return_ident_part("f")
                    ]
                )
            ),
            assign_part(
                assign_left_expr(
                    member_ident_number_expr("x", "0")
                ),
                Expr::Ident("x")
            ),
            assign_part(
                assign_left_expr(
                    member_ident_ident_expr("x", "a")
                ), 
                Expr::Ident("x")
            ),
            new_ident_part("x"),
            new_part(
                new_ident_expr("x", vec![]),
                vec![]
            ),
            new_part(
                Expr::Member(member_ident_number("x", "0")),
                vec![]
            ),
            new_part(
                Expr::Member(member_ident_ident("x", "a")),
                vec![]
            ),
            new_part(
                Expr::Member(
                    member(
                        Expr::Member(
                            member_ident_number("x", "0")
                        ),
                        Expr::Ident("a"),
                        false
                    )
                ),
                vec![]
            ),
            new_part(
                Expr::Member(
                    member(
                        Expr::Member(
                            member_ident_ident("x", "a")
                        ),
                        number_literal_expr("0"),
                        true,
                    )
                ),
                vec![]
            ),
            new_ident_part("x"),
            new_part(
                new_ident_expr("x", vec![]),
                vec![]
            ),
            new_part(
                new_ident_expr("x", vec![]),
                vec![]
            ),
            new_part(
                Expr::Member(
                    member(
                        new_ident_expr("x", vec![]),
                        Expr::Ident("a"),
                        false
                    )
                ),
                vec![]
            ),
            new_part(
                Expr::Member(
                    member(
                        new_ident_expr("x", vec![]),
                        number_literal_expr("0"),
                        true,
                    )
                ),
                vec![]
            ),
            call_ident_part("x", vec![]),
            call_part(
                call_ident_expr("x", vec![]),
                vec![],
            ),
            call_ident_part(
                "x", vec![
                    Expr::Ident("x")
                ]
            ),
            call_ident_part(
                "x", vec![
                    Expr::Ident("x"),
                    Expr::Ident("x"),
                ]
            ),
            call_part(
                member_expr(
                    call_expr(
                        member_ident_ident_expr(
                            "x", "a"
                        ),
                        vec![]
                    ),
                    Expr::Ident("a"),
                    false,
                ),
                vec![]
            ),
            call_part(
                member_expr(
                    call_expr(
                        member_ident_number_expr("x", "0"),
                        vec![],
                    ),
                    number_literal_expr("0"),
                    true
                ),
                vec![]
            ),
            call_part(
                member_expr(
                    member_expr(
                        call_expr(
                            Expr::Ident("x"), 
                            vec![]
                        ),
                        Expr::Ident("a"),
                        false,
                    ),
                    number_literal_expr("0"),
                    true,
                )
                , vec![]
            ),
            update_part(
                Expr::Ident("x"), 
                UpdateOperator::Increment, 
                false
            ),
            update_part(
                Expr::Ident("x"), 
                UpdateOperator::Decrement, 
                false
            ),
            unary_part(
                UnaryOperator::Delete, 
                unary_expr(
                    UnaryOperator::Void,
                    unary_expr(
                        UnaryOperator::TypeOf, 
                        unary_expr(
                            UnaryOperator::Plus,
                            unary_expr(
                                UnaryOperator::Minus,
                                unary_expr(
                                    UnaryOperator::Tilde,
                                    unary_expr(
                                        UnaryOperator::Not,
                                        Expr::Ident("x"),
                                        true,
                                    ),
                                    true,
                                ),
                                true,
                            ),
                            true,
                        ),
                        true,
                    ),
                    true,
                ), 
                true
            ),
            update_part(
                Expr::Ident("x"), 
                UpdateOperator::Increment, 
                true
            ),
            update_part(
                Expr::Ident("x"), 
                UpdateOperator::Decrement, 
                true
            ),
        ];
}

fn labeled_statement(label: &'static str) -> ProgramPart {
    ProgramPart::Stmt(
        Stmt::Labeled(
            LabeledStmt {
                label,
                body: Box::new(
                    Stmt::For(
                        ForStmt {
                            init: None,
                            test: None,
                            update: None,
                            body: Box::new(
                                Stmt::Break(
                                    Some(label)
                                )
                            )
                        }
                    )
                )
            }
        )
    )
}

fn line_term(label: &str) -> ProgramPart {
    ProgramPart::Stmt(
        Stmt::Labeled(
            LabeledStmt {
                label,
                body: Box::new(
                    Stmt::Expr(
                        Expr::Literal(
                            Literal::Number("0")
                        )
                    )
                )
            }
        )
    )
}

fn number_literal_part(number: &'static str) -> ProgramPart {
    ProgramPart::Stmt(
        number_literal_stmt(number)
    )
}

fn number_literal_stmt(number: &'static str) -> Stmt<'static> {
    Stmt::Expr(
        number_literal_expr(number)
    )
}

fn number_literal_expr(number: &'static str) -> Expr<'static> {
    Expr::Literal(
        number_literal(number)
    )
}

fn number_literal(number: &'static str) -> Literal<'static> {
    Literal::Number(number)
}

fn null_literal_part() -> ProgramPart<'static> {
    ProgramPart::Stmt(
        null_literal_stmt()
    )
}

fn null_literal_stmt() -> Stmt<'static> {
    Stmt::Expr(
        null_literal_expr()
    )
}

fn null_literal_expr() -> Expr<'static> {
    Expr::Literal(null_literal())
}

fn null_literal() -> Literal<'static> {
    Literal::Null
}

fn bool_literal_part(b: bool) -> ProgramPart<'static> {
    ProgramPart::Stmt(
        bool_literal_stmt(b)
    )
}

fn bool_literal_stmt(b: bool) -> Stmt<'static> {
    Stmt::Expr(
        bool_literal_expr(b)
    )
}
fn bool_literal_expr(b: bool) -> Expr<'static> {
    Expr::Literal(
        bool_literal(b)
    )
}

fn bool_literal(b: bool) -> Literal<'static> {
    Literal::Boolean(
        b
    )
}

fn var_decl_one() -> ProgramPart<'static> {
    ProgramPart::Stmt(
        Stmt::Var(
            var_decls(&[
                r"$", 
                r"_", 
                r"\u0078", 
                r"x$", 
                r"x_", 
                r"x\u0030", 
                r"xa", 
                r"x0", 
                r"x0a", 
                r"x0123456789",
                r"qwertyuiopasdfghjklzxcvbnm", 
                r"QWERTYUIOPASDFGHJKLZXCVBNM",
            ])      
        )
    )
}

fn var_decl_two() -> ProgramPart<'static> {
    ProgramPart::Stmt(
        Stmt::Var(
            var_decls(&[
                r"œ一", 
                r"ǻ둘", 
                r"ɤ〩", 
                r"φ", 
                r"ﬁⅷ", 
                r"ユニコード", 
                r"x‌‍",
            ])
        )
    )
}

fn var_decls(decls: &[&'static str]) -> Vec<VariableDecl<'static>> {
    decls.iter().map(|s| var_decl(*s)).collect()
}

fn var_decl(id: &'static str) -> VariableDecl {
    VariableDecl {
        id: Pat::Identifier(id),
        init: None,
    }
}

fn string_literal_part(s: &'static str) -> Part {
    ProgramPart::Stmt(
        string_literal_stmt(s)
    )
}

fn string_literal_stmt(s: &'static str) -> S {
    Stmt::Expr(
        string_literal_expr(s)
    )
}

fn string_literal_expr(s: &'static str) -> E {
    Expr::Literal(
        string_literal(s)
    )
}

fn string_literal(s: &'static str) -> L {
    Literal::String(
        s
    )
}

fn regex_literal_part(pattern: &'static str, flags: &'static str) -> ProgramPart<'static> {
    ProgramPart::Stmt(
        regex_literal_stmt(pattern, flags)
    )
}

fn regex_literal_stmt(pattern: &'static str, flags: &'static str) -> Stmt<'static> {
    Stmt::Expr(
        regex_literal_expr(pattern, flags)
    )
}

fn regex_literal_expr(pattern: &'static str, flags: &'static str) -> Expr<'static> {
    Expr::Literal(
        regex_literal(pattern, flags)
    )
}

fn regex_literal(pattern: &'static str, flags: &'static str) -> Literal<'static> {
    Literal::RegEx(
        regex(pattern, flags)
    )
}

fn regex(pattern: &'static str, flags: &'static str) -> RegEx<'static> {
    RegEx {
        pattern,
        flags
    }
}

fn this_part() -> ProgramPart<'static> {
    ProgramPart::Stmt(
        this_stmt()
    )
}

fn this_stmt() -> Stmt<'static> {
    Stmt::Expr(
        this_expr()
    )
}

fn this_expr() -> Expr<'static> {
    Expr::This
}

fn ident_stmt(id: &'static str) -> ProgramPart<'static> {
    ProgramPart::Stmt(
        Stmt::Expr(
            Expr::Ident(id)
        )
    )
}

fn array(content: Vec<Option<Expr<'static>>>) -> ProgramPart<'static> {
    ProgramPart::Stmt(
        Stmt::Expr(
            Expr::Array(
                content
            )
        )
    )
}

fn obj_literal_part(content: Vec<OP>) -> Part {
    ProgramPart::Stmt(
        obj_literal_stmt(content)
    )
}

fn obj_literal_stmt(content: Vec<OP>) -> S {
    Stmt::Expr(
        obj_literal_expr(content)
    )
}

fn obj_literal_expr(content: Vec<OP>) -> E {
    Expr::Object(
        content
    )
}

type PK = PropertyKey<'static>;
type PV = PropertyValue<'static>;
fn obj_prop_ident_number(ident: &'static str, number: &'static str) -> OP {
    obj_prop(
        obj_key_ident(ident),
        obj_value_number(number),
        PropertyKind::Init,
        false,
        false,
        false,
    )
}
fn obj_prop_str_number(s: &'static str, number: &'static str) -> OP {
    obj_prop(
        obj_key_string(s),
        obj_value_number(number),
        PropertyKind::Init,
        false,
        false,
        false,
    )
}
fn obj_prop_number_number(n: &'static str, number: &'static str) -> OP {
    obj_prop(
        obj_key_number(n),
        obj_value_number(number),
        PropertyKind::Init,
        false,
        false,
        false,
    )
}
type F = Function<'static>;
type FA = FunctionArg<'static>;
type FB = FunctionBody<'static>;
fn empty_anon_fn(args: Vec<FA>) -> F {
    Function {
        id: None,
        generator: false,
        is_async: false,
        body: vec![],
        params: args
    }
}

fn empty_fn(id: &'static str, args: Vec<FA>) -> F {
    Function {
        id: Some(id),
        generator: false,
        is_async: false,
        body: vec![],
        params: args
    }
}

fn func_expr(id: &'static str, args: Vec<FA>, body: Vec<Part>) -> E {
    Expr::Function(
        func(id, args, body)
    )
}

fn func(id: &'static str, args: Vec<FA>, body: Vec<Part>) -> F {
    Function {
        id: Some(id),
        generator: false,
        is_async: false,
        body,
        params: args,
    }
}

fn fn_arg_ident_expr(i: &'static str) -> FA {
    FunctionArg::Expr(
        Expr::Ident(i)
    )
}

fn fn_arg_ident_pat(i: &'static str) -> FA {
    FunctionArg::Pat(
        Pat::Identifier(i)
    )
}

fn obj_prop_ident_fn(i: &'static str, f: Function<'static>, kind: PropertyKind) -> OP {
    obj_prop(
        obj_key_ident(i), 
        obj_value_fn(f), 
        kind, 
        true, 
        false, 
        false
    )
}
fn obj_prop_str_fn(i: &'static str, f: Function<'static>, kind: PropertyKind) -> OP {
    obj_prop(
        obj_key_string(i), 
        obj_value_fn(f), 
        kind, 
        true, 
        false, 
        false
    )
}
fn obj_prop_number_fn(n: &'static str, f: Function<'static>, kind: PropertyKind) -> OP {
    obj_prop(
        obj_key_number(n), 
        obj_value_fn(f), 
        kind, 
        true, 
        false, 
        false
    )
}
fn obj_prop_ident_getter(i: &'static str) -> OP {
    obj_prop(
        obj_key_ident(i),
        obj_value_fn(empty_anon_fn(vec![])),
        PropertyKind::Get,
        false,
        false,
        false,
    )
}

fn obj_prop_str_getter(s: &'static str) -> OP {
    obj_prop(
        obj_key_string(s),
        obj_value_fn(empty_anon_fn(vec![])),
        PropertyKind::Get,
        false,
        false,
        false,
    )
}
fn obj_prop_number_getter(n: &'static str) -> OP {
    obj_prop(
        obj_key_number(n),
        obj_value_fn(empty_anon_fn(vec![])),
        PropertyKind::Get,
        false,
        false,
        false,
    )
}

fn obj_prop_ident_setter(i: &'static str, args: Vec<FA>) -> OP {
    obj_prop(
        obj_key_ident(i),
        obj_value_fn(empty_anon_fn(args)),
        PropertyKind::Set,
        false,
        false,
        false,
    )
}
fn obj_prop_str_setter(i: &'static str, args: Vec<FA>) -> OP {
    obj_prop(
        obj_key_string(i),
        obj_value_fn(empty_anon_fn(args)),
        PropertyKind::Set,
        false,
        false,
        false,
    )
}
fn obj_prop_number_setter(i: &'static str, args: Vec<FA>) -> OP {
    obj_prop(
        obj_key_number(i),
        obj_value_fn(empty_anon_fn(args)),
        PropertyKind::Set,
        false,
        false,
        false,
    )
}
fn obj_prop(key: PK, value: PV, kind: PropertyKind, method: bool, computed: bool, short_hand: bool) -> OP {
    ObjectProperty::Property(
        Property {
            key,
            value,
            kind,
            computed,
            method,
            short_hand,
        }
    )
}
fn obj_value_number(n: &'static str) -> PV {
    PropertyValue::Expr(
        number_literal_expr(n)
    )
}
fn obj_value_fn(f: Function<'static>) -> PV {
    PropertyValue::Expr(
        Expr::Function(f)
    )
}
fn obj_key_ident(i: &'static str) -> PK {
    obj_prop_key_expr(
        Expr::Ident(i)
    )
}
fn obj_prop_key_expr(expr: E) -> PK {
    PropertyKey::Expr(
        expr
    )
}
fn obj_key_number(n: &'static str) -> PK {
    obj_prop_key_lit(
        number_literal(n)
    )
}
fn obj_key_string(s: &'static str) -> PK {
    obj_prop_key_lit(
        string_literal(s)
    )
}

fn obj_prop_key_lit(lit: L) -> PK {
    PropertyKey::Literal(
        lit
    )
}
type P = Pat<'static>;
fn obj_prop_key_pat(pat: P) -> PK {
    PropertyKey::Pat(
        pat
    )
}

type Mem = MemberExpr<'static>;
fn member_number_ident_part(n: &'static str, i: &'static str) -> Part {
    ProgramPart::Stmt(
        member_number_ident_stmt(n, i)
    )
}
fn member_number_number_part(n: &'static str, i: &'static str) -> Part {
    ProgramPart::Stmt(
        member_number_number_stmt(n, i)
    )
}
fn member_ident_number_part(i: &'static str, n: &'static str) -> Part {
    ProgramPart::Stmt(
        member_ident_number_stmt(i, n)
    )
}
fn member_ident_ident_part(i: &'static str, i2: &'static str) -> Part {
    ProgramPart::Stmt(
        member_ident_ident_stmt(i, i2)
    )
}
fn member_number_ident_stmt(n: &'static str, i: &'static str) -> S {
    Stmt::Expr(
        member_number_ident_expr(n, i)
    )
}
fn member_number_number_stmt(n: &'static str, i: &'static str) -> S {
    Stmt::Expr(
        member_number_number_expr(n, i)
    )
}
fn member_ident_number_stmt(i: &'static str, n: &'static str) -> S {
    Stmt::Expr(
        member_ident_number_expr(i, n)
    )
}
fn member_ident_ident_stmt(i: &'static str, i2: &'static str) -> S {
    Stmt::Expr(
        member_ident_ident_expr(i, i2)
    )
}
fn member_number_ident_expr(n: &'static str, i: &'static str) -> E {
    Expr::Member(
        member_number_ident(n, i)
    )
}
fn member_number_number_expr(n: &'static str, i: &'static str) -> E {
    Expr::Member(
        member_number_number(n, i)
    )
}
fn member_ident_number_expr(i: &'static str, n: &'static str) -> E {
    Expr::Member(
        member_ident_number(i, n)
    )
}
fn member_ident_ident_expr(i: &'static str, i2: &'static str) -> E {
    Expr::Member(
        member_ident_ident(i, i2)
    )
}

fn member_number_ident(n: &'static str, i: &'static str) -> MemberExpr<'static> {
    member(
        number_literal_expr(n),
        Expr::Ident(i),
        false,
    )
}
fn member_number_number(n: &'static str, i: &'static str) -> MemberExpr<'static> {
    member(
        number_literal_expr(n),
        number_literal_expr(i),
        true,
    )
}
fn member_ident_number(i: &'static str, n: &'static str) -> MemberExpr<'static> {
    member(
        Expr::Ident(i),
        number_literal_expr(n),
        true,
    )
}
fn member_ident_ident(i: &'static str, i2: &'static str) -> MemberExpr<'static> {
    member(
        Expr::Ident(i),
        Expr::Ident(i2),
        false,
    )
}

fn member_expr(obj: E, prop: E, computed: bool) -> E {
    Expr::Member(member(obj, prop, computed))
}

fn member(obj: E, prop: E, computed: bool) -> MemberExpr<'static> {
    MemberExpr {
        computed,
        object: Box::new(obj),
        property: Box::new(prop),
    }
}

fn assign_part(left: AssignmentLeft<'static>, right: E) -> Part {
    ProgramPart::Stmt(
        assign_stmt(left, right)
    )
}

fn assign_stmt(left: AssignmentLeft<'static>, right: E) -> S {
    Stmt::Expr(
        assign_expr(left, right)
    )
}
fn assign_expr(left: AssignmentLeft<'static>, right: E) -> E {
    Expr::Assignment(
        assign(left, right)
    )
}

fn assign(left: AssignmentLeft<'static>, right: E) -> AssignmentExpr<'static> {
    AssignmentExpr {
        left: left,
        operator: AssignmentOperator::Equal,
        right: Box::new(right),
    }
}

fn assign_left_ident(i: &'static str) -> AssignmentLeft<'static> {
        assign_left_expr(Expr::Ident(i))
}

fn assign_left_expr(e: E) -> AssignmentLeft<'static> {
    AssignmentLeft::Expr(
        Box::new(e)
    )
}

fn return_ident_part(ident: &'static str) -> Part {
    ProgramPart::Stmt(
        return_ident_stmt(ident)
    )
}

fn return_ident_stmt(ident: &'static str) -> S {
    Stmt::Return(
        Some(Expr::Ident(ident))
    )
}

fn new_ident_part(i: &'static str) -> Part {
    ProgramPart::Stmt(
        new_stmt(
            Expr::Ident(i),
            vec![],
        )
    )
}

fn new_part(c: E, args: Vec<E>) -> Part {
    ProgramPart::Stmt(
        new_stmt(c, args)
    )
}

fn new_stmt(c: E, args: Vec<E>) -> S {
    Stmt::Expr(
        new_expr(c, args)
    )
}
fn new_expr(c: E, args: Vec<E>) -> E {
    Expr::New(
        new(c, args)
    )
}

fn new(c: E, args: Vec<E>) -> NewExpr<'static> {
    NewExpr {
        callee: Box::new(c),
        arguments: args,
    }
}

fn new_ident_expr(i: &'static str, args: Vec<E>) -> E {
    Expr::New(
        new_ident(i, args)
    )
}

fn new_ident(i: &'static str, args: Vec<E>) -> NewExpr<'static> {
    NewExpr {
        callee: Box::new(Expr::Ident(i)),
        arguments: args,
    }
}

fn call_ident_part(i: &'static str, args: Vec<E>) -> Part {
    call_part(Expr::Ident(i), args)
}

fn call_part(callee: E, args: Vec<E>) -> Part {
    ProgramPart::Stmt(
        Stmt::Expr(
            Expr::Call(
                call(callee, args)
            )
        )
    )
}

fn call_expr(callee: E, args: Vec<E>) -> E {
    Expr::Call(call(callee, args))
}

fn call_ident_expr(i: &'static str, args: Vec<E>) -> E {
    Expr::Call(
        call_ident(i, args)
    )
}

fn call_ident(i: &'static str, args: Vec<E>) -> CallExpr<'static> {
    call(
        Expr::Ident(i),
        args,
    )
}

fn call(callee: E, args: Vec<E>) -> CallExpr<'static> {
    CallExpr {
        callee: Box::new(callee),
        arguments: args,
    }
}

fn update_part(e: E, op: UpdateOperator, prefix: bool) -> Part {
    ProgramPart::Stmt(
        update_stmt(e, op, prefix)
    )
}
fn update_stmt(e: E, op: UpdateOperator, prefix: bool) -> S {
    Stmt::Expr(
        update_expr(e, op, prefix)
    )
}

fn update_expr(e: E, op: UpdateOperator, prefix: bool) -> E {
    Expr::Update(
        UpdateExpr {
            argument: Box::new(e),
            operator: op,
            prefix,
        }
    )
}

fn unary_part(op: UnaryOperator, e: E, prefix: bool) -> Part {
    ProgramPart::Stmt(unary_stmt(op, e, prefix))
}

fn unary_stmt(op: UnaryOperator, e: E, prefix: bool) -> S {
    Stmt::Expr(unary_expr(op, e, prefix))
}

fn unary_expr(op: UnaryOperator, e: E, prefix: bool) -> E {
    Expr::Unary(unary(op, e, prefix))
}

fn unary(op: UnaryOperator, e: E, prefix: bool) -> UnaryExpr<'static> {
    UnaryExpr {
        operator: op,
        argument: Box::new(e),
        prefix
    }
}