use resast::prelude::*;
type Part = ProgramPart<'static>;
type S = Stmt<'static>;
type E = Expr<'static>;
type L = Lit<'static>;
type OP = ObjProp<'static>;
type PR = Prop<'static>;
type P = Pat<'static>;
lazy_static! {
    pub static ref ES5: Vec<ProgramPart<'static>> =
        vec![
            preamble_labeled_statement("tab"),
            preamble_labeled_statement("verticalTab"),
            preamble_labeled_statement("formFeed"),
            preamble_labeled_statement("space"),
            preamble_labeled_statement("nbsp"),
            preamble_labeled_statement("bom"),
            line_term("lineFeed"),
            number_lit_part("0"),
            line_term("carriageReturn"),
            number_lit_part("0"),
            line_term("carriageReturnLineFeed"),
            number_lit_part("0"),
            line_term("lineSeparator"),
            number_lit_part("0"),
            line_term("paragraphSeparator"),
            number_lit_part("0"),
            var_decl_one(),
            var_decl_two(),
            null_lit_part(),
            bool_lit_part(true),
            bool_lit_part(false),
            number_lit_part("0"),
            number_lit_part("00"),
            number_lit_part("1234567890"),
            number_lit_part("01234567"),
            number_lit_part("0."),
            number_lit_part("0.00"),
            number_lit_part("10.00"),
            number_lit_part(".0"),
            number_lit_part(".00"),
            number_lit_part("0e0"),
            number_lit_part("0E0"),
            number_lit_part("0.e0"),
            number_lit_part("0.00e+0"),
            number_lit_part(".00e-0"),
            number_lit_part("0x0"),
            number_lit_part("0X0"),
            number_lit_part("0x0123456789abcdefABCDEF"),
            number_lit_part("2e308"),
            string_lit_double_part(r#""#),
            string_lit_double_part(r#"'"#),
            string_lit_double_part(r#"\'\"\\\b\f\n\r\t\v\0"#),
            string_lit_double_part(r#"\1\00\400\000"#),
            string_lit_double_part(r#"\x01\x23\x45\x67\x89\xAB\xCD\xEF"#),
            string_lit_double_part(r#"\u0123\u4567\u89AB\uCDEF"#),
            string_lit_double_part(r#"\
"#),
            string_lit_single_part(r""),//20
            string_lit_single_part(r#"""#),
            string_lit_single_part(r#"\'\"\\\b\f\n\r\t\v\0"#),
            string_lit_single_part(r#"\1\00\400\000"#),
            string_lit_single_part(r#"\x01\x23\x45\x67\x89\xAB\xCD\xEF"#),
            string_lit_single_part(r#"\u0123\u4567\u89AB\uCDEF"#),
            string_lit_single_part(r#"\
"#),
            regex_lit_part(r#"x"#, ""),
            regex_lit_part(r#"|"#, ""),
            regex_lit_part(r#"|||"#, ""),
            regex_lit_part(r#"^$\b\B"#, ""), //30
            regex_lit_part(r#"(?=(?!(?:(.))))"#, ""),
            regex_lit_part(r#"a.\f\n\r\t\v\0\[\-\/\\\x00\u0000"#,""),
            regex_lit_part(r#"\d\D\s\S\w\W"#, ""),
            regex_lit_part(r#"\ca\cb\cc\cd\ce\cf\cg\ch\ci\cj\ck\cl\cm\cn\co\cp\cq\cr\cs\ct\cu\cv\cw\cx\cy\cz"#, ""),
            regex_lit_part(r#"\cA\cB\cC\cD\cE\cF\cG\cH\cI\cJ\cK\cL\cM\cN\cO\cP\cQ\cR\cS\cT\cU\cV\cW\cX\cY\cZ"#, ""),
            regex_lit_part(r#"[a-z-]"#,""),
            regex_lit_part(r#"[^\b\-^]"#,""),
            regex_lit_part(r#"[/\]\\]"#, ""),
            regex_lit_part(r#"."#, "i"),
            regex_lit_part(r#"."#, "g"),//40
            regex_lit_part(r#"."#, "m"),
            regex_lit_part(r#"."#, "igm"),
            regex_lit_part(r#".*"#, ""),
            regex_lit_part(r#".*?"#, ""),
            regex_lit_part(r#".+"#, ""),
            regex_lit_part(r#".+?"#, ""),
            regex_lit_part(r#".?"#, ""),
            regex_lit_part(r#".??"#, ""),
            regex_lit_part(r#".{0}"#, ""),
            regex_lit_part(r#".{0,}"#, ""), //50
            regex_lit_part(r#".{0,0}"#, ""),
            this_part(),
            ident_stmt("x"),
            array(vec![]),
            // TODO: Double Check this
            array(vec![None]),
            array(vec![Some(number_lit_expr("0"))]),
            array(vec![Some(number_lit_expr("0"))]),
            array(vec![None, Some(number_lit_expr("0"))]),
            array(vec![Some(number_lit_expr("0")), Some(number_lit_expr("0"))]),
            array(vec![Some(number_lit_expr("0")), Some(number_lit_expr("0"))]), //60
            array(vec![Some(number_lit_expr("0")), None, Some(number_lit_expr("0"))]),
            array(vec![None, None]),
            //^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            obj_lit_part(vec![]),
            obj_lit_part(vec![
                obj_prop_ident_number("x", "0")
            ]),
            obj_lit_part(vec![
                obj_prop_ident_number("x", "0"),
                obj_prop_ident_number("y", "0"),
            ]),
            obj_lit_part(vec![
                obj_prop_ident_number("x", "0")
            ]),
            obj_lit_part(vec![
                obj_prop_str_single_number("x", "0"),
                obj_prop_str_double_number("y", "0"),
                obj_prop_ident_number("var", "0"),
            ]),
            obj_lit_part(vec![
                obj_prop_number_number("0", "0"),
            ]),
            obj_lit_part(vec![
                obj_prop_number_number("0.", "0"),
            ]),
            obj_lit_part(vec![
                obj_prop_number_number("0.0", "0"), // 80
            ]),
            obj_lit_part(vec![
                obj_prop_number_number(".0", "0"),
            ]),
            obj_lit_part(vec![
                obj_prop_number_number("0e0", "0"),
            ]),
            obj_lit_part(vec![
                obj_prop_number_number("0x0", "0"),
            ]),
            obj_lit_part(vec![
                obj_prop_ident_getter("x"),
                obj_prop_ident_setter("x", vec![
                    fn_arg_ident_pat("a")
                ]),
                obj_prop_str_getter_single("y"),
                obj_prop_str_setter_double("y", vec![
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
                Expr::ident_from("x")
            ),
            assign_part(
                assign_left_expr(
                    member_ident_ident_expr("x", "a")
                ),
                Expr::ident_from("x")
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
                        Expr::ident_from("a"),
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
                        number_lit_expr("0"),
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
                        Expr::ident_from("a"),
                        false
                    )
                ),
                vec![]
            ),
            new_part(
                Expr::Member(
                    member(
                        new_ident_expr("x", vec![]),
                        number_lit_expr("0"),
                        true,
                    )
                ),
                vec![]
            ),
            call_ident_part("x", vec![]), //100
            call_part(
                call_ident_expr("x", vec![]),
                vec![],
            ),
            call_ident_part(
                "x", vec![
                    Expr::ident_from("x")
                ]
            ),
            call_ident_part(
                "x", vec![
                    Expr::ident_from("x"),
                    Expr::ident_from("x"),
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
                    Expr::ident_from("a"),
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
                    number_lit_expr("0"),
                    true
                ),
                vec![]
            ),
            call_part(
                member_expr(
                    member_expr(
                        call_expr(
                            Expr::ident_from("x"),
                            vec![]
                        ),
                        Expr::ident_from("a"),
                        false,
                    ),
                    number_lit_expr("0"),
                    true,
                )
                , vec![]
            ),
            update_part(
                Expr::ident_from("x"),
                UpdateOp::Increment,
                false
            ),
            update_part(
                Expr::ident_from("x"),
                UpdateOp::Decrement,
                false
            ),
            unary_part(
                UnaryOp::Delete,
                unary_expr(
                    UnaryOp::Void,
                    unary_expr(
                        UnaryOp::TypeOf,
                        unary_expr(
                            UnaryOp::Plus,
                            unary_expr(
                                UnaryOp::Minus,
                                unary_expr(
                                    UnaryOp::Tilde,
                                    unary_expr(
                                        UnaryOp::Not,
                                        Expr::ident_from("x"),
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
            update_part( //110
                Expr::ident_from("x"),
                UpdateOp::Increment,
                true
            ),
            update_part(
                Expr::ident_from("x"),
                UpdateOp::Decrement,
                true
            ),
            zero_bin_zero_part(BinaryOp::Times),
            zero_bin_zero_part(BinaryOp::Over),
            zero_bin_zero_part(BinaryOp::Mod),
            zero_bin_zero_part(BinaryOp::Plus),
            zero_bin_zero_part(BinaryOp::Minus),
            zero_bin_zero_part(BinaryOp::LeftShift),
            zero_bin_zero_part(BinaryOp::RightShift),
            zero_bin_zero_part(BinaryOp::UnsignedRightShift),
            zero_bin_zero_part(BinaryOp::LessThan), //120
            zero_bin_zero_part(BinaryOp::GreaterThan),
            zero_bin_zero_part(BinaryOp::LessThanEqual),
            zero_bin_zero_part(BinaryOp::GreaterThanEqual),
            zero_bin_expr_part(
                BinaryOp::InstanceOf,
                Expr::Func(empty_anon_fn(vec![])),
            ),
            zero_bin_expr_part(
                BinaryOp::In,
                obj_lit_expr(vec![]),
            ),
            zero_bin_zero_part(BinaryOp::Equal),
            zero_bin_zero_part(BinaryOp::NotEqual),
            zero_bin_zero_part(BinaryOp::StrictEqual),
            zero_bin_zero_part(BinaryOp::StrictNotEqual),
            zero_bin_zero_part(BinaryOp::And),//130
            zero_bin_zero_part(BinaryOp::XOr),
            zero_bin_zero_part(BinaryOp::Or),
            zero_log_zero(LogicalOp::And),
            zero_log_zero(LogicalOp::Or),
            conditional_part(
                number_lit_expr("0"),
                number_lit_expr("0"),
                number_lit_expr("0"),
            ),
            // TODO: Validate this
            conditional_part(
                number_lit_expr("0"),
                conditional_expr(
                    number_lit_expr("0"),
                    number_lit_expr("0"),
                    number_lit_expr("0"),
                ),
                number_lit_expr("0"),
            ),
            conditional_part(
                zero_log_zero_expr(LogicalOp::Or),
                assign_expr(
                    assign_left_ident("x"),
                    number_lit_expr("0"),
                ),
                assign_expr(
                    assign_left_ident("x"),
                    number_lit_expr("0"),
                ),
            ),
            // ^^^^^^^^^^^^^^^^^^^^^^^^^^
            ident_assign_zero_part("x", AssignOp::Equal),
            ident_assign_zero_part("x", AssignOp::TimesEqual),//140
            ident_assign_zero_part("x", AssignOp::DivEqual),
            ident_assign_zero_part("x", AssignOp::ModEqual),
            ident_assign_zero_part("x", AssignOp::PlusEqual),
            ident_assign_zero_part("x", AssignOp::MinusEqual),
            ident_assign_zero_part("x", AssignOp::LeftShiftEqual),
            ident_assign_zero_part("x", AssignOp::RightShiftEqual),
            ident_assign_zero_part("x", AssignOp::UnsignedRightShiftEqual),
            ident_assign_zero_part("x", AssignOp::AndEqual),
            ident_assign_zero_part("x", AssignOp::XOrEqual),
            ident_assign_zero_part("x", AssignOp::OrEqual), //150
            zero_sequence(2),
            zero_sequence(3),
            sequence(vec![
                ident_assign_zero_expr("x", AssignOp::Equal),
                ident_assign_zero_expr("x", AssignOp::Equal),
            ]),
            block_part(vec![]),
            block_part(vec![ProgramPart::Stmt(Stmt::Empty)]),
            block_part(vec![number_lit_part("0")]),
            block_part(vec![number_lit_part("0")]),
            block_part(vec![
                number_lit_part("0"),
                number_lit_part("0"),
            ]),
            block_part(vec![
                number_lit_part("0"),
                number_lit_part("0"),
            ]),
            variable_decl_part(
                VarKind::Var,
                vec![
                    variable_decl_none("x") //160
                ]
            ),
            variable_decl_part(
                VarKind::Var,
                vec![
                variable_decl_none("x"),
                variable_decl_none("y"),
            ]),
            variable_decl_part(VarKind::Var,vec![
                variable_decl_none("x"),
                variable_decl_none("y"),
                variable_decl_none("z"),
            ]),
            variable_decl_part(VarKind::Var,vec![
                variable_decl_zero("x"),
            ]),
            variable_decl_part(VarKind::Var,vec![
                variable_decl_zero("x"),
                variable_decl_none("y"),
            ]),
            variable_decl_part(VarKind::Var,vec![
                variable_decl_none("x"),
                variable_decl_zero("y"),
            ]),
            variable_decl_part(VarKind::Var,vec![
                variable_decl_zero("x"),
                variable_decl_zero("y"),
            ]),
            ProgramPart::Stmt(Stmt::Empty),
            if_zero_empty(),
            if_zero_empty_else(),
            do_while_zero(),//170
            while_zero(),
            for_exprs_part(None, None, None, Stmt::Break(None)),
            for_exprs_part(
                Some(number_lit_expr("0")),
                Some(number_lit_expr("0")),
                Some(number_lit_expr("0")),
                Stmt::Empty,
            ),
            for_exprs_part(
                Some(zero_bin_expr_expr(
                    BinaryOp::In,
                    array_expr(vec![]),
                )),
                Some(number_lit_expr("0")),
                None,
                Stmt::Empty,
            ),
            for_var_part(
                VarKind::Var,
                vec![
                var_decl("a")
                ],
                None,
                None,
                Stmt::Break(None),
            ),
            for_var_part(
                VarKind::Var,
                vec![
                var_decl("a"),
                var_decl("b"),
                ],
                Some(number_lit_expr("0")),
                Some(number_lit_expr("0")),
                Stmt::Empty,
            ),
            for_var_part(
                VarKind::Var,
                vec![
                variable_decl_zero("a")
                ],
                None,
                None,
                Stmt::Break(None),
            ),
            for_var_part(
                VarKind::Var,
                vec![
                    VarDecl {
                        id: Pat::ident_from("a"),
                        init: Some(
                            zero_bin_expr_expr(
                                BinaryOp::In,
                                array_expr(vec![])
                            )
                        )
                    }
                ],
                Some(number_lit_expr("0")),
                None,
                Stmt::Empty,
            ),
            for_in_loop_part(
                LoopLeft::Expr(Expr::ident_from("x")),
                obj_lit_expr(vec![]),
                Stmt::Empty,
            ),
            for_in_loop_part(
                LoopLeft::Variable(
                    VarKind::Var,
                    var_decl("x")
                ),
                obj_lit_expr(vec![]),
                Stmt::Empty,
            ),
            for_in_loop_part(
                LoopLeft::Variable(
                    VarKind::Var,
                    var_decl_init("x", array_expr(vec![]))
                ),
                obj_lit_expr(vec![]),
                Stmt::Empty,
            ),
            for_in_loop_part(
                LoopLeft::Variable(
                    VarKind::Var,
                    var_decl_init(
                        "x",
                        zero_bin_expr_expr(BinaryOp::In, array_expr(vec![]))
                    )
                ),
                obj_lit_expr(vec![]),
                Stmt::Empty,
            ),
            for_exprs_part(None, Some(number_lit_expr("0")), None, Stmt::Continue(None)),
            labeled_statement_continue("x"),
            for_exprs_part(None, None, None, Stmt::Break(None)),
            preamble_labeled_statement("x"),
            switch_zero_part(vec![
                case_zero(vec![ProgramPart::Stmt(Stmt::Break(None))])
            ]),
            func_part(
                "f",
                vec![],
                vec![ProgramPart::Stmt(Stmt::Return(None))]
            ),
            func_part(
                "f",
                vec![],
                vec![ProgramPart::Stmt(Stmt::Return(
                    Some(number_lit_expr("0"))
                ))]
            ),
            ProgramPart::Stmt(Stmt::With(WithStmt { object: number_lit_expr("0"), body: Box::new(Stmt::Empty)})),
            switch_zero_part(vec![]),
            switch_zero_part(vec![
                case_zero(vec![]),
            ]),
            switch_zero_part(vec![
                case_zero(vec![]),
                case_zero(vec![]),
            ]),
            switch_zero_part(vec![
                default_case(vec![]),
            ]),
            switch_zero_part(vec![
                case_zero(vec![]),
                default_case(vec![]),
                case_zero(vec![]),
            ]),
            switch_zero_part(vec![
                case_zero(vec![
                    empty_part(),
                ]),
            ]),
            switch_zero_part(vec![
                case_zero(vec![
                    empty_part(),
                    empty_part(),
                ]),
            ]),
            switch_zero_part(vec![
                default_case(vec![
                    empty_part(),
                ]),
            ]),
            switch_zero_part(vec![
                default_case(vec![
                    empty_part(),
                    empty_part(),
                ]),
            ]),
            labeled_part("x", Stmt::Empty),
            labeled_part(
                "x",
                labeled_stmt("y", Stmt::Empty)
            ),
            try_part(
                vec![
                    throw_part(
                        number_lit_expr("0")
                    )
                ],
                Some(catch_(Some(Pat::ident_from("x")), vec![])),
                None
            ),
            try_part(
                vec![],
                Some(catch_(Some(Pat::ident_from("x")), vec![])),
                None
            ),
            try_part(
                vec![],
                None,
                Some(vec![])
            ),
            try_part(
                vec![],
                Some(catch_(Some(Pat::ident_from(
                    "x")), vec![])),
                Some(vec![])
            ),
            ProgramPart::Stmt(Stmt::Debugger),
            func_part("f", vec![], vec![]),
            func_part(
                "f",
                vec![
                    fn_arg_ident_pat("x"),
                ],
                vec![]
            ),
            func_part(
                "f",
                vec![
                    fn_arg_ident_pat("x"),
                    fn_arg_ident_pat("y"),
                ],
                vec![]
            ),
            func_part(
                "f",
                vec![],
                vec![
                    func_part(
                        "f",
                        vec![],
                        vec![]
                    ),
                ]
            ),
            func_part(
                "f",
                vec![],
                vec![
                    directive_part_double("use strict"),
                ],
            ),
            func_part(
                "f",
                vec![],
                vec![
                    directive_part_single("use strict"),
                ],
            ),
            func_part(
                "f",
                vec![],
                vec![
                    directive_part_double("other directive"),
                ],
            ),
            func_part(
                "f",
                vec![],
                vec![
                    directive_part_single("other directive"),
                ],
            ),
            func_part(
                "f",
                vec![],
                vec![
                    string_lit_double_part("string"),
                ],
            ),
            func_part(
                "f",
                vec![],
                vec![
                    string_lit_single_part("string"),
                ],
            ),
            func_part(
                "f",
                vec![],
                vec![
                    ProgramPart::Stmt(
                        Stmt::Expr(
                            Expr::Binary(
                                binary(
                                    string_lit_single_expr("string"),
                                    BinaryOp::Plus,
                                    number_lit_expr("0")
                                )
                            )
                        )
                    ),
                ],
            ),
            func_expr_part(None, vec![], vec![]),
            func_expr_part(None, vec![
                fn_arg_ident_pat("x")
            ], vec![]),
            func_expr_part(None, vec![
                fn_arg_ident_pat("x"),
                fn_arg_ident_pat("y"),
            ], vec![]),
            func_expr_part(None, vec![], vec![
                func_part("f", vec![], vec![]),
            ]),
            func_expr_part(Some("f"), vec![], vec![]),
            func_expr_part(Some("f"), vec![
                fn_arg_ident_pat("x")
            ], vec![]),
            func_expr_part(Some("f"), vec![
                fn_arg_ident_pat("x"),
                fn_arg_ident_pat("y"),
            ], vec![]),
            func_expr_part(Some("f"), vec![], vec![
                func_part("f", vec![], vec![]),
            ]),
        ];
}

lazy_static! {
    pub static ref ES2015: Vec<ProgramPart<'static>> = vec![
        preamble_labeled_statement("tab"),
        preamble_labeled_statement("verticalTab"),
        preamble_labeled_statement("formFeed"),
        preamble_labeled_statement("space"),
        preamble_labeled_statement("nbsp"),
        preamble_labeled_statement("bom"),
        line_term("lineFeed"),
        number_lit_part("0"),
        line_term("carriageReturn"),
        number_lit_part("0"),
        line_term("carriageReturnLineFeed"),
        number_lit_part("0"),
        line_term("lineSeparator"),
        number_lit_part("0"),
        line_term("paragraphSeparator"),
        number_lit_part("0"),
        var_decl_one_2015(),
        var_decl_two_2015(),
        variable_decl_part(
            VarKind::Var,
            vec![var_decl("yield")]
        ),
        variable_decl_part(
            VarKind::Let,
            vec![var_decl("letx")],
        ),
        variable_decl_part(
            VarKind::Let,
            vec![
                VarDecl {
                    id: Pat::Array(
                        vec![
                            Some(
                                ArrayPatPart::Pat(
                                    Pat::ident_from(r"x\u0078")
                                )
                            )
                        ]
                    ),
                    init: Some(number_lit_expr("0"))
                }
            ]
        ),
        variable_decl_part(
            VarKind::Const,
            vec![var_decl_init("constx", number_lit_expr("0"))],
        ),
        block_part(
            vec![
                variable_decl_part(
                    VarKind::Let,
                    vec![var_decl("x")]
                ),
                variable_decl_part(
                    VarKind::Let,
                    vec![variable_decl("y", number_lit_expr("0"))],
                ),
                variable_decl_part(
                    VarKind::Const,
                    vec![variable_decl("z", number_lit_expr("0"))],
                )
            ]
        ),
        null_lit_part(),
        bool_lit_part(true),
        bool_lit_part(false),
        number_lit_part("0"),
        number_lit_part("00"),
        number_lit_part("1234567890"),
        number_lit_part("01234567"),
        number_lit_part("0."),
        number_lit_part("0.00"),
        number_lit_part("10.00"),
        number_lit_part(".0"),
        number_lit_part(".00"),
        number_lit_part("0e0"),
        number_lit_part("0E0"),
        number_lit_part("0.e0"),
        number_lit_part("0.00e+0"),
        number_lit_part(".00e-0"),
        number_lit_part("0x0"),
        number_lit_part("0X0"),
        number_lit_part("0x0123456789abcdefABCDEF"),
        number_lit_part("0b0"),
        number_lit_part("0B0"),
        number_lit_part("0b01"),
        number_lit_part("0b10"),
        number_lit_part("0b10101010"),
        number_lit_part("0o0"),
        number_lit_part("0O0"),
        number_lit_part("0o01234567"),
        number_lit_part("2e308"),
        string_lit_double_part(r#""#,),
        string_lit_double_part(r#"'"#,),
        string_lit_double_part(r#"\'\"\\\b\f\n\r\t\v\0"#),
        string_lit_double_part(r#"\1\00\400\000"#),
        string_lit_double_part(r#"\x01\x23\x45\x67\x89\xAB\xCD\xEF\xab\xcd\xef"#),
        string_lit_double_part(r#"\u0123\u4567\u89AB\uCDEF\u00ab\ucdef"#),
        string_lit_double_part(r#"\uD834\uDF06\u2603\u03C6 \u{0000001F4a9}\u{1D306}\u{2603}\u{3c6} 𝌆☃φ"#),
        string_lit_double_part(r#"\
"#),
        string_lit_single_part(r""),
        string_lit_single_part(r#"""#),
        string_lit_single_part(r#"\'\"\\\b\f\n\r\t\v\0"#),
        string_lit_single_part(r#"\1\00\400\000"#),
        string_lit_single_part(r#"\x01\x23\x45\x67\x89\xAB\xCD\xEF\xab\xcd\xef"#),
        string_lit_single_part(r#"\u0123\u4567\u89AB\uCDEF\u00ab\ucdef"#),
        string_lit_single_part(r#"\uD834\uDF06\u2603\u03C6 \u{0000001F4a9} \u{1D306}\u{2603}\u{3c6} 𝌆☃φ"#),
        string_lit_single_part(r#"\
"#),
        regex_lit_part(r#"x"#, ""),
        regex_lit_part(r#"|"#, ""),
        regex_lit_part(r#"|||"#, ""),
        regex_lit_part(r#"^$\b\B"#, ""),
        regex_lit_part(r#"(?=(?!(?:(.))))"#, ""),
        regex_lit_part(r#"a.\f\n\r\t\v\0\[\-\/\\\x00\u0000\uD834\uDF06"#,""),
        regex_lit_part(r#"\u{00000001d306}"#,"u"),
        regex_lit_part(r#"\d\D\s\S\w\W"#, ""),
        regex_lit_part(r#"\ca\cb\cc\cd\ce\cf\cg\ch\ci\cj\ck\cl\cm\cn\co\cp\cq\cr\cs\ct\cu\cv\cw\cx\cy\cz"#, ""),
        regex_lit_part(r#"\cA\cB\cC\cD\cE\cF\cG\cH\cI\cJ\cK\cL\cM\cN\cO\cP\cQ\cR\cS\cT\cU\cV\cW\cX\cY\cZ"#, ""),
        regex_lit_part(r#"[a-z-]"#,""),
        regex_lit_part(r#"[^\b\-^]"#,""),
        regex_lit_part(r#"[/\]\\]"#, ""),
        regex_lit_part(r#"."#, "i"),
        regex_lit_part(r#"."#, "g"),
        regex_lit_part(r#"."#, "m"),
        regex_lit_part(r#"."#, "igm"),
        regex_lit_part(r#".*"#, ""),
        regex_lit_part(r#".*?"#, ""),
        regex_lit_part(r#".+"#, ""),
        regex_lit_part(r#".+?"#, ""),
        regex_lit_part(r#".?"#, ""),
        regex_lit_part(r#".??"#, ""),
        regex_lit_part(r#".{0}"#, ""),
        regex_lit_part(r#".{0,}"#, ""),
        regex_lit_part(r#".{0,0}"#, ""),
        template_part(vec![
            template_element("`a`", true)
        ], vec![
        ]),
        template_part(vec![
            template_element("`${", false),
            template_element("}`", true),
        ], vec![
            number_lit_expr("0")
        ]),
        template_part(vec![
            template_element("`0${", false),
            template_element("}2`", true),

        ], vec![
            Expr::Sequence(vec![
                number_lit_expr("0"),
                number_lit_expr("1"),
            ])
        ]),
        template_part(vec![
            template_element("`0${", false),
            template_element("}4`", true),
        ], vec![
            template_expr(vec![
                template_element("`1${", false),
                template_element("}3`", true),
            ], vec![
                number_lit_expr("2")
            ])
        ]),
        template_part(vec![
            template_element(r"`\``", true)
        ], vec![

        ]),
        template_part(vec![
            template_element(r"`a\${b`", true)
        ], vec![

        ]),
        template_part(vec![
            template_element(r"`\0\n\x0A\u000A\u{A}`", true)
        ], vec![

        ]),
        this_part(),
        ProgramPart::Stmt(Stmt::Expr(Expr::ident_from("x"))),
        array(vec![]),
        array(vec![None]),
        array(vec![Some(number_lit_expr("0"))]),
        array(vec![Some(number_lit_expr("0"))]),
        array(vec![None, Some(number_lit_expr("0"))]),
        array(vec![Some(number_lit_expr("0")), Some(number_lit_expr("0"))]),
        array(vec![Some(number_lit_expr("0")), Some(number_lit_expr("0"))]), //60
        array(vec![Some(number_lit_expr("0")), None, Some(number_lit_expr("0"))]),
        array(vec![None, None]),
        obj_lit_part(vec![]),
        obj_lit_part(vec![
            obj_prop(
                obj_key_ident("x"),
                PropValue::None,
                PropKind::Init,
                false,
                false,
                true,
            )
        ]),
        obj_lit_part(vec![
            obj_prop_ident_number("x", "0")
        ]),
        obj_lit_part(vec![
            obj_prop_ident_number("x", "0"),
            obj_prop_ident_number("y", "0"),
        ]),
        obj_lit_part(vec![
            obj_prop_ident_number("x", "0")
        ]),
        obj_lit_part(vec![
            obj_prop_str_single_number("x", "0"),
            obj_prop_str_double_number("y", "0"),
            obj_prop_ident_number("in", "0")
        ]),
        obj_lit_part(vec![
            obj_prop_number_number("0", "0"),
            obj_prop_number_number("0.", "0"),
            obj_prop_number_number("0.0", "0"),
            obj_prop_number_number(".0", "0"),
            obj_prop_number_number("0e0", "0"),
            obj_prop_number_number("0x0", "0"),
            obj_prop(
                obj_key_number("0"),
                obj_value_number("0"),
                PropKind::Init,
                false,
                true,
                false,
            ),
            obj_prop_ident_getter("x"),
            obj_prop_ident_setter("x", vec![
                fn_arg_ident_pat("a")
            ]),
            obj_prop_str_getter_single("y"),
            obj_prop_str_setter_double("y", vec![
                fn_arg_ident_pat("a")
            ]),
            obj_prop_number_getter("0"),
            obj_prop_number_setter("0", vec![
                fn_arg_ident_pat("a")
            ]),
            obj_prop(
                obj_key_ident("var"),
                obj_value_fn(empty_anon_fn(vec![])),
                PropKind::Get,
                false,
                false,
                false,
            ),
            obj_prop(
                obj_key_ident("var"),
                obj_value_fn(empty_anon_fn(vec![
                    fn_arg_ident_pat("a")
                ])),
                PropKind::Set,
                false,
                false,
                false,
            ),
            obj_prop(
                obj_key_number("0"),
                obj_value_fn(empty_anon_fn(vec![])),
                PropKind::Get,
                false,
                true,
                false,
            ),
            obj_prop(
                obj_key_number("0"),
                obj_value_fn(empty_anon_fn(vec![
                    fn_arg_ident_pat("a")
                ])),
                PropKind::Set,
                false,
                true,
                false,
            ),
            obj_prop(
                obj_key_number("1"),
                obj_value_fn(empty_anon_fn(vec![])),
                PropKind::Init,
                true,
                true,
                false,
            ),
            obj_prop_ident_fn("a", empty_anon_fn(vec![]), PropKind::Init),
            obj_prop_sstr_fn("b", empty_anon_fn(vec![]), PropKind::Init),
            obj_prop_dstr_fn("c", empty_anon_fn(vec![]), PropKind::Init),
            obj_prop_number_fn("0", empty_anon_fn(vec![]), PropKind::Init),
            obj_prop_number_fn(".1", empty_anon_fn(vec![]), PropKind::Init),
            obj_prop_number_fn("1.", empty_anon_fn(vec![]), PropKind::Init),
            obj_prop_number_fn("1e1", empty_anon_fn(vec![]), PropKind::Init),
            obj_prop_ident_fn("var", empty_anon_fn(
                long_args()
            ), PropKind::Init),
            obj_prop_ident_setter("in", vec![
                FuncArg::Pat(
                    Pat::Array(
                        long_args_array_pat().into_iter().map(|a| Some(ArrayPatPart::Pat(a))).collect()
                    )
                )
            ]),
            empty_generator_prop(obj_key_ident("d")),
            empty_generator_prop(obj_key_string_single("e")),
            empty_generator_prop(obj_key_string_double("f")),
            empty_generator_prop(obj_key_number("2")),
            empty_generator_prop(obj_key_number(".2")),
            empty_generator_prop(obj_key_number("3.")),
            empty_generator_prop(obj_key_number("2e2")),
            empty_generator_prop(obj_key_ident("in")),
        ]),
        obj_lit_part(vec![
            obj_prop(
                obj_key_ident("__proto__"),
                PropValue::Expr(null_lit_expr()),
                PropKind::Init,
                false,
                false,
                false,
            ),
            obj_prop_ident_getter("__proto__"),
            obj_prop_ident_setter("__proto__", vec![fn_arg_ident_pat("a")])
        ]),
        obj_lit_part(vec![
            obj_prop(
                obj_key_string_double("__proto__"),
                PropValue::Expr(null_lit_expr()),
                PropKind::Init,
                false,
                false,
                false,
            ),
            obj_prop_ident_fn(
                "__proto__",
                empty_anon_fn(vec![]),
                PropKind::Init
            )
        ]),
        member_number_ident_part("0.", "a"),
        member_number_ident_part("0", "a"),
        member_number_ident_part("0", "a"),
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
                Expr::ident_from("x")
            ),
            assign_part(
                assign_left_expr(
                    member_ident_ident_expr("x", "a")
                ),
                Expr::ident_from("x")
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
                        Expr::ident_from("a"),
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
                        number_lit_expr("0"),
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
                        Expr::ident_from("a"),
                        false
                    )
                ),
                vec![]
            ),
            new_part(
                Expr::Member(
                    member(
                        new_ident_expr("x", vec![]),
                        number_lit_expr("0"),
                        true,
                    )
                ),
                vec![]
            ),
            call_ident_part("x", vec![]), //100
            call_part(
                call_ident_expr("x", vec![]),
                vec![],
            ),
            call_ident_part(
                "x", vec![
                    Expr::ident_from("x")
                ]
            ),
            call_ident_part(
                "x", vec![
                    Expr::ident_from("x"),
                    Expr::ident_from("x"),
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
                    Expr::ident_from("a"),
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
                    number_lit_expr("0"),
                    true
                ),
                vec![]
            ),
            call_part(
                member_expr(
                    member_expr(
                        call_expr(
                            Expr::ident_from("x"),
                            vec![]
                        ),
                        Expr::ident_from("a"),
                        false,
                    ),
                    number_lit_expr("0"),
                    true,
                )
                , vec![]
            ),
            call_part(
                Expr::ident_from("x"),
                vec![
                    Expr::Spread(
                        Box::new(Expr::Array(vec![
                            Some(number_lit_expr("0")),
                            Some(number_lit_expr("1")),
                        ]))
                    ),
                    Expr::Spread(
                        Box::new(Expr::Array(vec![]))
                    ),
                    Expr::Spread(
                        Box::new(Expr::Func(
                            Func {
                                id: Some(Ident::from("f")),
                                params: vec![],
                                body: FuncBody(vec![
                                    ProgramPart::Stmt(Stmt::Return(
                                        Some(Expr::Yield(YieldExpr {
                                            argument: Some(Box::new(number_lit_expr("2"))),
                                            delegate: false,
                                        }))
                                    ))
                                ]),
                                generator: true,
                                is_async: false,
                            }
                        ))
                    )
                ]
            ),
            ProgramPart::Stmt(
                Stmt::Expr(
                    Expr::TaggedTemplate(
                        TaggedTemplateExpr {
                            tag: Box::new(Expr::ident_from("x")),
                            quasi: template(vec![
                                template_element("`a`", true),
                            ], vec![])
                        }
                    )
                )
            ),
            ProgramPart::Stmt(
                Stmt::Expr(
                    Expr::TaggedTemplate(
                        TaggedTemplateExpr {
                            tag: Box::new(Expr::ident_from("x")),
                            quasi: template(vec![
                                template_element("`0${", false),
                                template_element("}2`", true),
                            ], vec![
                                number_lit_expr("1"),
                            ])
                        }
                    )
                )
            ),
            update_part(
                Expr::ident_from("x"),
                UpdateOp::Increment,
                false
            ),
            update_part(
                Expr::ident_from("x"),
                UpdateOp::Decrement,
                false
            ),
            unary_part(
                UnaryOp::Delete,
                unary_expr(
                    UnaryOp::Void,
                    unary_expr(
                        UnaryOp::TypeOf,
                        unary_expr(
                            UnaryOp::Plus,
                            unary_expr(
                                UnaryOp::Minus,
                                unary_expr(
                                    UnaryOp::Tilde,
                                    unary_expr(
                                        UnaryOp::Not,
                                        Expr::ident_from("x"),
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
            update_part( //110
                Expr::ident_from("x"),
                UpdateOp::Increment,
                true
            ),
            update_part(
                Expr::ident_from("x"),
                UpdateOp::Decrement,
                true
            ),
            zero_bin_zero_part(BinaryOp::Times),
            zero_bin_zero_part(BinaryOp::Over),
            zero_bin_zero_part(BinaryOp::Mod),
            zero_bin_zero_part(BinaryOp::Plus),
            zero_bin_zero_part(BinaryOp::Minus),
            zero_bin_zero_part(BinaryOp::LeftShift),
            zero_bin_zero_part(BinaryOp::RightShift),
            zero_bin_zero_part(BinaryOp::UnsignedRightShift),
            zero_bin_zero_part(BinaryOp::LessThan), //120
            zero_bin_zero_part(BinaryOp::GreaterThan),
            zero_bin_zero_part(BinaryOp::LessThanEqual),
            zero_bin_zero_part(BinaryOp::GreaterThanEqual),
            zero_bin_expr_part(
                BinaryOp::InstanceOf,
                Expr::Func(empty_anon_fn(vec![])),
            ),
            zero_bin_expr_part(
                BinaryOp::In,
                obj_lit_expr(vec![]),
            ),
            zero_bin_zero_part(BinaryOp::Equal),
            zero_bin_zero_part(BinaryOp::NotEqual),
            zero_bin_zero_part(BinaryOp::StrictEqual),
            zero_bin_zero_part(BinaryOp::StrictNotEqual),
            zero_bin_zero_part(BinaryOp::And),//130
            zero_bin_zero_part(BinaryOp::XOr),
            zero_bin_zero_part(BinaryOp::Or),
            zero_log_zero(LogicalOp::And),
            zero_log_zero(LogicalOp::Or),
            conditional_part(
                number_lit_expr("0"),
                number_lit_expr("0"),
                number_lit_expr("0"),
            ),
            // TODO: Validate this
            conditional_part(
                number_lit_expr("0"),
                conditional_expr(
                    number_lit_expr("0"),
                    number_lit_expr("0"),
                    number_lit_expr("0"),
                ),
                number_lit_expr("0"),
            ),
            conditional_part(
                zero_log_zero_expr(LogicalOp::Or),
                assign_expr(
                    assign_left_ident("x"),
                    number_lit_expr("0"),
                ),
                assign_expr(
                    assign_left_ident("x"),
                    number_lit_expr("0"),
                ),
            ),
            // ^^^^^^^^^^^^^^^^^^^^^^^^^^
            ident_assign_zero_part("x", AssignOp::Equal),
            ident_assign_zero_part("x", AssignOp::TimesEqual),//140
            ident_assign_zero_part("x", AssignOp::DivEqual),
            ident_assign_zero_part("x", AssignOp::ModEqual),
            ident_assign_zero_part("x", AssignOp::PlusEqual),
            ident_assign_zero_part("x", AssignOp::MinusEqual),
            ident_assign_zero_part("x", AssignOp::LeftShiftEqual),
            ident_assign_zero_part("x", AssignOp::RightShiftEqual),
            ident_assign_zero_part("x", AssignOp::UnsignedRightShiftEqual),
            ident_assign_zero_part("x", AssignOp::AndEqual),
            ident_assign_zero_part("x", AssignOp::XOrEqual),
            ident_assign_zero_part("x", AssignOp::OrEqual), //150
            zero_sequence(2),
            zero_sequence(3),
            sequence(vec![
                ident_assign_zero_expr("x", AssignOp::Equal),
                ident_assign_zero_expr("x", AssignOp::Equal),
            ]),
            block_part(vec![]),
            block_part(vec![ProgramPart::Stmt(Stmt::Empty)]),
            block_part(vec![number_lit_part("0")]),
            block_part(vec![number_lit_part("0")]),
            block_part(vec![
                number_lit_part("0"),
                number_lit_part("0"),
            ]),
            block_part(vec![
                number_lit_part("0"),
                number_lit_part("0"),
            ]),
            variable_decl_part(
                VarKind::Var,
                vec![
                    variable_decl_none("x") //160
                ]
            ),
            variable_decl_part(
                VarKind::Var,
                vec![
                variable_decl_none("x"),
                variable_decl_none("y"),
            ]),
            variable_decl_part(VarKind::Var,vec![
                variable_decl_none("x"),
                variable_decl_none("y"),
                variable_decl_none("z"),
            ]),
            variable_decl_part(VarKind::Var,vec![
                variable_decl_zero("x"),
            ]),
            variable_decl_part(VarKind::Var,vec![
                variable_decl_zero("x"),
                variable_decl_none("y"),
            ]),
            variable_decl_part(VarKind::Var,vec![
                variable_decl_none("x"),
                variable_decl_zero("y"),
            ]),
            variable_decl_part(VarKind::Var,vec![
                variable_decl_zero("x"),
                variable_decl_zero("y"),
            ]),
            ProgramPart::Stmt(Stmt::Empty),
            if_zero_empty(),
            if_zero_empty_else(),
            do_while_zero(),
            number_lit_part("0"),
            do_while_zero(),
            do_while_zero(),
            number_lit_part("0"),
            while_zero(),
            for_exprs_part(None, None, None, Stmt::Break(None)),
            for_exprs_part(
                Some(number_lit_expr("0")),
                Some(number_lit_expr("0")),
                Some(number_lit_expr("0")),
                Stmt::Empty,
            ),
            for_exprs_part(
                Some(zero_bin_expr_expr(
                    BinaryOp::In,
                    array_expr(vec![]),
                )),
                Some(number_lit_expr("0")),
                None,
                Stmt::Empty,
            ),
            for_var_part(
                VarKind::Var,
                vec![
                var_decl("a")
                ],
                None,
                None,
                Stmt::Break(None),
            ),
            for_var_part(
                VarKind::Var,
                vec![
                var_decl("a"),
                var_decl("b"),
                ],
                Some(number_lit_expr("0")),
                Some(number_lit_expr("0")),
                Stmt::Empty,
            ),
            for_var_part(
                VarKind::Var,
                vec![
                variable_decl_zero("a")
                ],
                None,
                None,
                Stmt::Break(None),
            ),
            for_var_part(
                VarKind::Var,
                vec![
                    VarDecl {
                        id: Pat::ident_from("a"),
                        init: Some(
                            zero_bin_expr_expr(
                                BinaryOp::In,
                                array_expr(vec![])
                            )
                        )
                    }
                ],
                Some(number_lit_expr("0")),
                None,
                Stmt::Empty,
            ),
            for_in_loop_part(
                LoopLeft::Expr(Expr::ident_from("x")),
                obj_lit_expr(vec![]),
                Stmt::Empty,
            ),
            for_in_loop_part(
                LoopLeft::Variable(
                    VarKind::Var,
                    var_decl("x")
                ),
                obj_lit_expr(vec![]),
                Stmt::Empty,
            ),
            ProgramPart::Stmt(
                Stmt::ForOf(
                    ForOfStmt {
                        left: LoopLeft::Expr(
                            Expr::ident_from("x")
                        ),
                        right: array_expr(vec![]),
                        body: Box::new(Stmt::Empty),
                        is_await: false
                    }
                )
            ),
            ProgramPart::Stmt(
                Stmt::ForOf(
                    ForOfStmt {
                        left: LoopLeft::Variable(
                            VarKind::Var,
                            var_decl("x")
                        ),
                        right: array_expr(vec![]),
                        body: Box::new(Stmt::Empty),
                        is_await: false
                    }
                )
            ),
            for_exprs_part(None, Some(number_lit_expr("0")), None, Stmt::Continue(None)),
            labeled_statement_continue("x"),
            for_exprs_part(None, None, None, Stmt::Break(None)),
            preamble_labeled_statement("x"),

            switch_zero_part(vec![
                case_zero(vec![ProgramPart::Stmt(Stmt::Break(None))])
            ]),
            func_part(
                "f",
                vec![],
                vec![ProgramPart::Stmt(Stmt::Return(None))]
            ),
            func_part(
                "f",
                vec![],
                vec![ProgramPart::Stmt(Stmt::Return(
                    Some(number_lit_expr("0"))
                ))]
            ),
            ProgramPart::Stmt(Stmt::With(WithStmt { object: number_lit_expr("0"), body: Box::new(Stmt::Empty)})),
            switch_zero_part(vec![]),
            switch_zero_part(vec![
                case_zero(vec![]),
            ]),
            switch_zero_part(vec![
                case_zero(vec![]),
                case_zero(vec![]),
            ]),
            switch_zero_part(vec![
                default_case(vec![]),
            ]),
            switch_zero_part(vec![
                case_zero(vec![]),
                default_case(vec![]),
                case_zero(vec![]),
            ]),
            switch_zero_part(vec![
                case_zero(vec![
                    empty_part(),
                ]),
            ]),
            switch_zero_part(vec![
                case_zero(vec![
                    empty_part(),
                    empty_part(),
                ]),
            ]),
            switch_zero_part(vec![
                default_case(vec![
                    empty_part(),
                ]),
            ]),
            switch_zero_part(vec![
                default_case(vec![
                    empty_part(),
                    empty_part(),
                ]),
            ]),
            labeled_part("x", Stmt::Empty),
            labeled_part(
                "x",
                labeled_stmt("y", Stmt::Empty)
            ),
            try_part(
                vec![
                    throw_part(
                        number_lit_expr("0")
                    )
                ],
                Some(catch_(Some(Pat::ident_from("x")), vec![])),
                None
            ),
            try_part(
                vec![],
                Some(catch_(Some(Pat::ident_from("x")), vec![])),
                None
            ),
            try_part(
                vec![],
                None,
                Some(vec![])
            ),
            try_part(
                vec![],
                Some(catch_(Some(Pat::ident_from("x")), vec![])),
                Some(vec![])
            ),
            ProgramPart::Stmt(Stmt::Debugger),
            func_part("f", vec![], vec![]),
            func_part(
                "f",
                vec![
                    fn_arg_ident_pat("x"),
                ],
                vec![]
            ),
            func_part(
                "f",
                vec![
                    fn_arg_ident_pat("x"),
                    fn_arg_ident_pat("y"),
                ],
                vec![]
            ),
            func_part(
                "f",
                vec![],
                vec![
                    func_part(
                        "f",
                        vec![],
                        vec![]
                    ),
                ]
            ),
        block_part(vec![
            func_part("f", vec![], vec![])
        ]),
        empty_part(),
        func_part("f", long_args(), vec![]),
        func_part(
                "f",
                vec![],
                vec![
                    directive_part_double("use strict"),
                ],
            ),
            func_part(
                "f",
                vec![],
                vec![
                    directive_part_single("use strict"),
                ],
            ),
            func_part(
                "f",
                vec![],
                vec![
                    directive_part_double("other directive"),
                ],
            ),
            func_part(
                "f",
                vec![],
                vec![
                    directive_part_single("other directive"),
                ],
            ),
            func_part(
                "f",
                vec![],
                vec![
                    string_lit_double_part("string"),
                ],
            ),
            func_part(
                "f",
                vec![],
                vec![
                    string_lit_single_part("string"),
                ],
            ),
            func_part(
                "f",
                vec![],
                vec![
                    ProgramPart::Stmt(
                        Stmt::Expr(
                            Expr::Binary(
                                binary(
                                    string_lit_single_expr("string"),
                                    BinaryOp::Plus,
                                    number_lit_expr("0")
                                )
                            )
                        )
                    ),
                ],
            ),
            ProgramPart::Decl(
                Decl::Func(
                    Func {
                        params: long_args(),
                        id: Some(Ident::from("g")),
                        body: FuncBody(long_gen_body()),
                        generator: true,
                        is_async: false,
                    }
                )
            ),
            ProgramPart::Stmt(
                Stmt::Expr(
                    Expr::Func(
                        Func {
                            params: long_args(),
                            id: Some(Ident::from("g")),
                            body: FuncBody(long_gen_body()),
                            generator: true,
                            is_async: false,
                        }
                    )
                )
            ),
            ident_stmt("yield"),
            ProgramPart::Stmt(
                Stmt::Expr(
                    Expr::Binary(
                        BinaryExpr {
                            left: Box::new(
                                Expr::ident_from("yield")
                            ),
                            operator: BinaryOp::Plus,
                            right: Box::new(
                                number_lit_expr("0")
                            )
                        }
                    )
                )
            ),
            ProgramPart::Stmt(
                Stmt::Expr(
                    Expr::Binary(
                        BinaryExpr {
                            left: Box::new(
                                Expr::ident_from("yield")
                            ),
                            operator: BinaryOp::Times,
                            right: Box::new(
                                number_lit_expr("0")
                            )
                        }
                    )
                )
            ),
            func_expr_part(None, vec![], vec![]),
            func_expr_part(None, vec![
                fn_arg_ident_pat("x")
            ], vec![]),
            func_expr_part(None, vec![
                fn_arg_ident_pat("x"),
                fn_arg_ident_pat("y"),
            ], vec![]),
            func_expr_part(None, vec![], vec![
                func_part("f", vec![], vec![]),
            ]),
            func_expr_part(Some("f"), vec![], vec![]),
            func_expr_part(Some("f"), vec![
                fn_arg_ident_pat("x")
            ], vec![]),
            func_expr_part(Some("f"), vec![
                fn_arg_ident_pat("x"),
                fn_arg_ident_pat("y"),
            ], vec![]),
            func_expr_part(Some("f"), vec![], vec![
                func_part("f", vec![], vec![]),
            ]),
            arrow_expr_part(vec![], number_lit_expr("0")),
            arrow_expr_body_part(vec![],vec![
                empty_part()
            ]),
            arrow_expr_part(vec![
                fn_arg_ident_expr("x"),
            ], Expr::ident_from("x")),
            arrow_expr_part(vec![
                fn_arg_ident_expr("x"),
            ], assign_expr(
                assign_left_ident("x"),
                number_lit_expr("0"))),
            arrow_expr_part(vec![
                fn_arg_ident_expr("x")
            ], Expr::ArrowFunc(arrow_expr(vec![
                fn_arg_ident_expr("y")
            ], Expr::ident_from("x")))),
            arrow_expr_body_part(vec![
                fn_arg_ident_expr("x")
            ], vec![
                ident_stmt("x")
            ]),
            arrow_expr_part(vec![
                fn_arg_ident_expr("x"),
            ], obj_lit_expr(vec![
                    obj_prop(
                        obj_key_ident("x"),
                        PropValue::None,
                        PropKind::Init,
                        false,
                        false,
                        true
                    )
                ])
            ),
            arrow_expr_part(vec![
                fn_arg_ident_expr("x"),
            ], Expr::ident_from("x")),
            arrow_expr_body_part(vec![
                fn_arg_ident_expr("x"),
            ],vec![
                ProgramPart::Stmt(
                    return_ident_stmt("x")
                )
            ]),
            arrow_expr_part(vec![
                fn_arg_ident_expr("x"),
            ], obj_lit_expr(vec![
                    obj_prop(
                        obj_key_ident("x"),
                        PropValue::None,
                        PropKind::Init,
                        false,
                        false,
                        true
                    )
                ])
            ),
            arrow_expr_part(vec![
                FuncArg::Expr(
                    Expr::Obj(vec![
                        obj_prop(
                            obj_key_ident("x"),
                            PropValue::None,
                            PropKind::Init,
                            false,
                            false,
                            true,
                        )
                    ])
                ),
            ], obj_lit_expr(vec![
                    obj_prop(
                        obj_key_ident("x"),
                        PropValue::None,
                        PropKind::Init,
                        false,
                        false,
                        true
                    )
                ])
            ),
            arrow_expr_body_part(
                long_args_array_expr(),
                vec![
                    empty_part()
                ]
            ),
            assign_part(
                assign_left_expr(
                    array_expr(vec![
                        Some(Expr::ident_from("a"))
                    ])
                ),
                array_expr(vec![
                    Some(Expr::Spread(
                        Box::new(
                            array_expr(vec![
                                Some(number_lit_expr("0"))
                            ])
                        )
                    ))
                ])
            ),
            assign_part(
                assign_left_expr(
                    obj_lit_expr(vec![
                        obj_prop(
                            obj_key_ident("a"),
                            PropValue::None,
                            PropKind::Init,
                            false,
                            false,
                            true,
                        )
                    ])
                ),
                obj_lit_expr(vec![])
            ),
            try_part(vec![], Some(
                CatchClause {
                    param: Some(
                        Pat::Array(vec![
                            Some(ArrayPatPart::Pat(
                                Pat::ident_from("e")
                            ))
                        ]
                        )
                    ),
                    body: vec![]
                }
            ), None),
            try_part(vec![], Some(
                CatchClause {
                    param: Some(
                        Pat::Obj(vec![
                            ObjPatPart::Assign(
                                Prop {
                                    key: obj_key_ident("e"),
                                    value: PropValue::None,
                                    kind: PropKind::Init,
                                    method: false,
                                    computed: false,
                                    short_hand: true,
                                    is_static: false,
                                }
                            )
                        ]
                        )
                    ),
                    body: vec![]
                }
            ), None),
            class_part(
                "A",
                None,
                vec![],
            ),
            class_part(
                "B",
                Some(new_expr(Expr::ident_from("A"), vec![])),
                vec![
                    class_prop(
                        obj_key_ident("constructor"),
                        anon_fn(long_args(), vec![
                            call_part(Expr::Super, vec![
                                Expr::MetaProp(
                                    MetaProp {
                                        meta: Ident::from("new"),
                                        property: Ident::from("target"),
                                    }
                                )
                            ]),
                            ProgramPart::Stmt(
                                Stmt::Expr(
                                    Expr::TaggedTemplate(
                                        TaggedTemplateExpr {
                                            tag: Box::new(Expr::Call(
                                                CallExpr {
                                                    callee: Box::new(Expr::Super),
                                                    arguments: vec![]
                                                }
                                            )),
                                            quasi: template(vec![
                                                template_element("`template`", true),
                                            ], vec![])
                                        }
                                    )
                                )
                            ),
                            arrow_expr_part(vec![], call_expr(Expr::Super, vec![
                                Expr::This
                            ])),
                        ],
                        false,
                    ),
                    true,
                    false),
                    class_prop(
                        obj_key_ident("m"),
                        anon_fn(long_args(), vec![
                            call_part(
                                member_expr(
                                    Expr::Super,
                                    Expr::ident_from("m"),
                                    false,
                                ),
                                vec![]
                            ),
                            ProgramPart::Stmt(
                                Stmt::Expr(
                                    Expr::TaggedTemplate(
                                        TaggedTemplateExpr {
                                            tag: Box::new(
                                                member_expr(Expr::Super, Expr::ident_from("m"), false)
                                            ),
                                            quasi: template(vec![
                                                template_element("`template`", true),
                                            ], vec![])
                                        }
                                    )
                                )
                            ),
                            arrow_expr_part(vec![], call_expr(
                                member_expr(Expr::Super, Expr::ident_from("m"), false),
                                vec![
                                    Expr::This
                                ]
                            )),
                        ], false),
                        false,
                        false,
                    ),
                    class_prop(
                        obj_key_ident("a"),
                        anon_fn(vec![], vec![], false),
                        false,
                        true,
                    ),
                    class_prop(
                        obj_key_string_single("b"),
                        anon_fn(vec![], vec![], false),
                        false,
                        true,
                    ),
                    class_prop(
                        obj_key_number("0"),
                        anon_fn(vec![], vec![], false),
                        false,
                        true
                    ),
                    class_prop_computed(
                        obj_key_number("0"),
                        anon_fn(vec![], vec![], false),
                        false,
                        true
                    ),
                    class_prop(
                        obj_key_ident("c"),
                        anon_fn(vec![], vec![
                            yield_part(None, false),
                        ], true),
                        false,
                        true
                    ),
                    class_prop(
                        obj_key_string_double("d"),
                        anon_fn(vec![], vec![
                            yield_part(None, false),
                        ], true),
                        false,
                        true
                    ),
                    class_prop(
                        obj_key_number("1"),
                        anon_fn(vec![], vec![
                            yield_part(None, false),
                        ], true),
                        false,
                        true
                    ),
                    class_prop_computed(
                        obj_key_number("1"),
                        anon_fn(vec![], vec![
                            yield_part(None, false),
                        ], true),
                        false,
                        true
                    ),
                    class_prop(
                        obj_key_ident("var"),
                        anon_fn(vec![], vec![

                        ], false),
                        false,
                        true
                    ),
                    class_prop(
                        obj_key_ident("in"),
                        anon_fn(vec![], vec![
                        ], true),
                        false,
                        true
                    ),
                    class_prop_getter(
                        obj_key_ident("e"),
                        anon_fn(vec![], vec![], false),
                        true,
                        false,
                    ),
                    class_prop_getter(
                        obj_key_string_single("f"),
                        anon_fn(vec![], vec![], false),
                        true,
                        false
                    ),
                    class_prop_getter(
                        obj_key_number("2"),
                        anon_fn(vec![], vec![], false),
                        true,
                        false
                    ),
                    class_prop_getter(
                        obj_key_number("2"),
                        anon_fn(vec![], vec![], false),
                        true,
                        true
                    ),
                    class_prop_setter(
                        obj_key_ident("g"),
                        anon_fn(vec![
                            fn_arg_ident_pat("a")
                        ], vec![], false),
                        true,
                        false,
                    ),
                    class_prop_setter(
                        obj_key_string_double("h"),
                        anon_fn(vec![
                            fn_arg_ident_pat("a")
                        ], vec![], false),
                        true,
                        false,
                    ),
                    class_prop_setter(
                        obj_key_number("3"),
                        anon_fn(vec![
                            fn_arg_ident_pat("a")
                        ], vec![], false),
                        true,
                        false,
                    ),
                    class_prop_setter(
                        obj_key_number("3"),
                        anon_fn(vec![
                            fn_arg_ident_pat("a")
                        ], vec![], false),
                        true,
                        true,
                    ),
                    class_prop_getter(
                        obj_key_ident("if"),
                        anon_fn(vec![], vec![], false),
                        true,
                        false,
                    ),
                    class_prop_setter(
                        obj_key_ident("if"),
                        anon_fn(vec![
                            fn_arg_ident_pat("a")
                        ], vec![], false),
                        true,
                        false,
                    ),
                    class_prop(
                        obj_key_ident("a"),
                        anon_fn(vec![], vec![], false),
                        false,
                        false,
                    ),
                    class_prop(
                        obj_key_string_single("b"),
                        anon_fn(vec![], vec![], false),
                        false,
                        false,
                    ),
                    class_prop(
                        obj_key_number("0"),
                        anon_fn(vec![], vec![], false),
                        false,
                        false,
                    ),
                    class_prop_computed(
                        obj_key_number("0"),
                        anon_fn(vec![], vec![], false),
                        false,
                        false,
                    ),
                    class_prop(
                        obj_key_ident("c"),
                        anon_fn(vec![], vec![
                            yield_part(None, false)
                        ], true),
                        false,
                        false,
                    ),
                    class_prop(
                        obj_key_string_double("d"),
                        anon_fn(vec![], vec![
                            yield_part(None, false)
                        ], true),
                        false,
                        false,
                    ),
                    class_prop(
                        obj_key_number("1"),
                        anon_fn(vec![], vec![
                            yield_part(None, false)
                        ], true),
                        false,
                        false,
                    ),
                    class_prop_computed(
                        obj_key_number("1"),
                        anon_fn(vec![], vec![
                            yield_part(None, false)
                        ], true),
                        false,
                        false,
                    ),
                    class_prop(
                        obj_key_ident("var"),
                        anon_fn(vec![], vec![
                        ], false),
                        false,
                        false,
                    ),
                    class_prop(
                        obj_key_ident("in"),
                        anon_fn(vec![], vec![
                            yield_part(None, false)
                        ], true),
                        false,
                        false,
                    ),
                    class_prop_getter(
                        obj_key_ident("e"),
                        anon_fn(vec![], vec![], false),
                        false,
                        false,
                    ),
                    class_prop_getter(
                        obj_key_string_single("f"),
                        anon_fn(vec![], vec![], false),
                        false,
                        false,
                    ),
                    class_prop_getter(
                        obj_key_number("2"),
                        anon_fn(vec![], vec![], false),
                        false,
                        false,
                    ),
                    class_prop_getter(
                        obj_key_number("2"),
                        anon_fn(vec![], vec![], false),
                        false,
                        true,
                    ),
                    class_prop_setter(
                        obj_key_ident("g"),
                        anon_fn(vec![fn_arg_ident_pat("a")], vec![], false),
                        false,
                        false,
                    ),
                    class_prop_setter(
                        obj_key_string_double("h"),
                        anon_fn(vec![fn_arg_ident_pat("a")], vec![], false),
                        false,
                        false,
                    ),
                    class_prop_setter(
                        obj_key_number("3"),
                        anon_fn(vec![fn_arg_ident_pat("a")], vec![], false),
                        false,
                        false,
                    ),
                    class_prop_setter(
                        obj_key_number("3"),
                        anon_fn(vec![fn_arg_ident_pat("a")], vec![], false),
                        false,
                        true,
                    ),
                    class_prop_getter(
                        obj_key_ident("if"),
                        anon_fn(vec![], vec![
                        ], false),
                        false,
                        false,
                    ),
                    class_prop_setter(
                        obj_key_ident("if"),
                        anon_fn(vec![fn_arg_ident_pat("f")], vec![], false),
                        false,
                        false,
                    ),
                ]
            ),
            class_part("C", Some(
                Expr::ident_from("B")
            ), vec![
                class_prop(obj_key_string_double("constructor"), anon_fn(vec![], vec![
                    call_part(Expr::Super, vec![])
                ], false), true, false),
            ])
        // ProgramPart::Stmt(Stmt::Empty)
    ];
}
lazy_static! {
    pub static ref ESMOD: Vec<ProgramPart<'static>> = vec![
        ProgramPart::Decl(
            Decl::Import(
                Box::new(
                    ModImport {
                        specifiers: vec![
                            ImportSpecifier::Default(
                                Ident::from("i0")
                            )
                        ],
                        source: string_lit_double("module")
                    }
                )
            )
        ),
        ProgramPart::Decl(
            Decl::Import(
                Box::new(
                    ModImport {
                        specifiers: vec![
                            ImportSpecifier::Namespace(
                                Ident::from("i1")
                            )
                        ],
                        source: string_lit_double("module")
                    }
                )
            )
        ),
        ProgramPart::Decl(
            Decl::Import(
                Box::new(
                    ModImport {
                        specifiers: vec![
                        ],
                        source: string_lit_double("module")
                    }
                )
            )
        ),
        ProgramPart::Decl(
            Decl::Import(
                Box::new(
                    ModImport {
                        specifiers: vec![
                            normal_import("i2", "i2"),
                            normal_import("a", "i3"),
                        ],
                        source: string_lit_double("module")
                    }
                )
            )
        ),
        ProgramPart::Decl(
            Decl::Import(
                Box::new(
                    ModImport {
                        specifiers: vec![
                            ImportSpecifier::Default(Ident::from("i4")),
                            ImportSpecifier::Namespace(Ident::from("i5")),
                        ],
                        source: string_lit_double("module")
                    }
                )
            )
        ),
        ProgramPart::Decl(
            Decl::Import(
                Box::new(
                    ModImport {
                        specifiers: vec![
                            ImportSpecifier::Default(Ident::from("i6")),
                        ],
                        source: string_lit_double("module")
                    }
                )
            )
        ),
        ProgramPart::Decl(
            Decl::Import(
                Box::new(
                    ModImport {
                        specifiers: vec![
                            ImportSpecifier::Default(Ident::from("i7")),
                            normal_import("i8", "i8"),
                            normal_import("var", "i9"),
                        ],
                        source: string_lit_double("module")
                    }
                )
            )
        ),
        ProgramPart::Decl(
            Decl::Import(
                Box::new(
                    ModImport {
                        specifiers: vec![
                        ],
                        source: string_lit_double("module")
                    }
                )
            )
        ),
        ProgramPart::Decl(
            Decl::Export(
                Box::new(
                    ModExport::All(string_lit_double("module"))
                )
            )
        ),
        ProgramPart::Decl(
            Decl::Export(
                Box::new(
                    ModExport::Named(
                        NamedExportDecl::Specifier(
                            vec![],
                            Some(string_lit_double("module"))
                        )
                    ),
                )
            )
        ),
        ProgramPart::Decl(
            Decl::Export(
                Box::new(
                    ModExport::Named(
                        NamedExportDecl::Specifier(
                            vec![
                                export_spec("i0", "i0"),
                                export_spec("i1", "a"),
                                export_spec("i2", "var"),
                            ],
                            Some(string_lit_double("module"))
                        )
                    )
                )
            )
        ),
        ProgramPart::Decl(
            Decl::Export(
                Box::new(
                    ModExport::Named(
                        NamedExportDecl::Specifier(
                            vec![
                            ],
                            None
                        )
                    )
                )
            )
        ),
        ProgramPart::Decl(
            Decl::Export(
                Box::new(
                    ModExport::Named(
                        NamedExportDecl::Specifier(
                            vec![
                                export_spec("i3", "i3"),
                                export_spec("i4", "in"),
                            ],
                            None
                        )
                    )
                )
            )
        ),
        ProgramPart::Decl(
            Decl::Export(
                Box::new(
                    ModExport::Named(
                        NamedExportDecl::Decl(
                            Decl::Var(
                                VarKind::Var,
                                vec![
                                    variable_decl_none("e5"),
                                    var_decl_init(
                                        "e6",
                                        number_lit_expr("0")
                                    )
                                ]
                            )
                        )
                    )
                )
            )
        ),
        ProgramPart::Decl(
            Decl::Export(
                Box::new(
                    ModExport::Named(
                        NamedExportDecl::Decl(
                            Decl::Var(
                                VarKind::Let,
                                vec![
                                    variable_decl_none("e7"),
                                    var_decl_init(
                                        "e8",
                                        number_lit_expr("0")
                                    )
                                ]
                            )
                        )
                    )
                )
            )
        ),
        ProgramPart::Decl(
            Decl::Export(
                Box::new(
                    ModExport::Named(
                        NamedExportDecl::Decl(
                            Decl::Var(
                                VarKind::Const,
                                vec![
                                    var_decl_init(
                                        "e9",
                                        number_lit_expr("0")
                                    ),
                                    var_decl_init(
                                        "e10",
                                        number_lit_expr("0")
                                    )
                                ]
                            )
                        )
                    )
                )
            )
        ),
        ProgramPart::Decl(
            Decl::Export(
                Box::new(
                    ModExport::Named(
                        NamedExportDecl::Decl(
                            Decl::Func(
                                func("e11", vec![], vec![])
                            )
                        )
                    )
                )
            )
        ),
        ProgramPart::Decl(
            Decl::Export(
                Box::new(
                    ModExport::Named(
                        NamedExportDecl::Decl(
                            Decl::Func(
                                func_generator("e12", vec![], vec![])
                            )
                        )
                    )
                )
            )
        ),
        ProgramPart::Decl(
            Decl::Export(
                Box::new(
                    ModExport::Named(
                        NamedExportDecl::Decl(
                            class_decl("e13", None, vec![])
                        )
                    )
                )
            )
        ),
        ProgramPart::Decl(
            Decl::Export(
                Box::new(
                    ModExport::Named(
                        NamedExportDecl::Decl(
                            class_decl("e14", Some(Expr::ident_from("e15")), vec![])
                        )
                    )
                )
            )
        ),
        ProgramPart::Decl(
            Decl::Export(
                Box::new(
                    ModExport::Default(
                        DefaultExportDecl::Decl(
                            Decl::Func(
                                func("e16", vec![], vec![])
                            )
                        )
                    )
                )
            )
        ),
        preamble_labeled_statement("tab"),
        preamble_labeled_statement("verticalTab"),
        preamble_labeled_statement("formFeed"),
        preamble_labeled_statement("space"),
        preamble_labeled_statement("nbsp"),
        preamble_labeled_statement("bom"),
        line_term("lineFeed"),
        number_lit_part("0"),
        line_term("carriageReturn"),
        number_lit_part("0"),
        line_term("carriageReturnLineFeed"),
        number_lit_part("0"),
        line_term("lineSeparator"),
        number_lit_part("0"),
        line_term("paragraphSeparator"),
        number_lit_part("0"),
        var_decl_one_2015(),
        var_decl_two_2015(),
        variable_decl_part(
            VarKind::Let,
            vec![var_decl("letx")],
        ),
        variable_decl_part(
            VarKind::Let,
            vec![
                VarDecl {
                    id: Pat::Array(
                        vec![
                            Some(
                                ArrayPatPart::Pat(
                                    Pat::ident_from(r"x\u0078")
                                )
                            )
                        ]
                    ),
                    init: Some(number_lit_expr("0"))
                }
            ]
        ),
        variable_decl_part(
            VarKind::Const,
            vec![var_decl_init("constx", number_lit_expr("0"))],
        ),
        block_part(
            vec![
                variable_decl_part(
                    VarKind::Let,
                    vec![var_decl("x")]
                ),
                variable_decl_part(
                    VarKind::Let,
                    vec![variable_decl("y", number_lit_expr("0"))],
                ),
                variable_decl_part(
                    VarKind::Const,
                    vec![variable_decl("z", number_lit_expr("0"))],
                )
            ]
        ),
        null_lit_part(),
        bool_lit_part(true),
        bool_lit_part(false),
        number_lit_part("0"),
        number_lit_part("1234567890"),
        number_lit_part("0."),
        number_lit_part("0.00"),
        number_lit_part("10.00"),
        number_lit_part(".0"),
        number_lit_part(".00"),
        number_lit_part("0e0"),
        number_lit_part("0E0"),
        number_lit_part("0.e0"),
        number_lit_part("0.00e+0"),
        number_lit_part(".00e-0"),
        number_lit_part("0x0"),
        number_lit_part("0X0"),
        number_lit_part("0x0123456789abcdefABCDEF"),
        number_lit_part("0b0"),
        number_lit_part("0B0"),
        number_lit_part("0b01"),
        number_lit_part("0b10"),
        number_lit_part("0b10101010"),
        number_lit_part("0o0"),
        number_lit_part("0O0"),
        number_lit_part("0o01234567"),
        number_lit_part("2e308"),
        string_lit_double_part(r#""#,),
        string_lit_double_part(r#"'"#,),
        string_lit_double_part(r#"\'\"\\\b\f\n\r\t\v\0"#,),
        string_lit_double_part(r#"\0"#,),
        string_lit_double_part(r#"\x01\x23\x45\x67\x89\xAB\xCD\xEF\xab\xcd\xef"#,),
        string_lit_double_part(r#"\u0123\u4567\u89AB\uCDEF\u00ab\ucdef"#,),
        string_lit_double_part(r#"\uD834\uDF06\u2603\u03C6 \u{0000001F4a9}\u{1D306}\u{2603}\u{3c6} 𝌆☃φ"#),
        string_lit_double_part(r#"\
"#),
        string_lit_single_part(r""),
        string_lit_single_part(r#"""#),
        string_lit_single_part(r#"\'\"\\\b\f\n\r\t\v\0"#),
        string_lit_single_part(r#"\0"#,),
        string_lit_single_part(r#"\x01\x23\x45\x67\x89\xAB\xCD\xEF\xab\xcd\xef"#),
        string_lit_single_part(r#"\u0123\u4567\u89AB\uCDEF\u00ab\ucdef"#),
        string_lit_single_part(r#"\uD834\uDF06\u2603\u03C6 \u{0000001F4a9} \u{1D306}\u{2603}\u{3c6} 𝌆☃φ"#),
        string_lit_single_part(r#"\
"#),
        regex_lit_part(r#"x"#, ""),
        regex_lit_part(r#"|"#, ""),
        regex_lit_part(r#"|||"#, ""),
        regex_lit_part(r#"^$\b\B"#, ""),
        regex_lit_part(r#"(?=(?!(?:(.))))"#, ""),
        regex_lit_part(r#"a.\f\n\r\t\v\0\[\-\/\\\x00\u0000\uD834\uDF06"#,""),
        regex_lit_part(r#"\u{00000001d306}"#,"u"),
        regex_lit_part(r#"\d\D\s\S\w\W"#, ""),
        regex_lit_part(r#"\ca\cb\cc\cd\ce\cf\cg\ch\ci\cj\ck\cl\cm\cn\co\cp\cq\cr\cs\ct\cu\cv\cw\cx\cy\cz"#, ""),
        regex_lit_part(r#"\cA\cB\cC\cD\cE\cF\cG\cH\cI\cJ\cK\cL\cM\cN\cO\cP\cQ\cR\cS\cT\cU\cV\cW\cX\cY\cZ"#, ""),
        regex_lit_part(r#"[a-z-]"#,""),
        regex_lit_part(r#"[^\b\-^]"#,""),
        regex_lit_part(r#"[/\]\\]"#, ""),
        regex_lit_part(r#"."#, "i"),
        regex_lit_part(r#"."#, "g"),
        regex_lit_part(r#"."#, "m"),
        regex_lit_part(r#"."#, "igm"),
        regex_lit_part(r#".*"#, ""),
        regex_lit_part(r#".*?"#, ""),
        regex_lit_part(r#".+"#, ""),
        regex_lit_part(r#".+?"#, ""),
        regex_lit_part(r#".?"#, ""),
        regex_lit_part(r#".??"#, ""),
        regex_lit_part(r#".{0}"#, ""),
        regex_lit_part(r#".{0,}"#, ""),
        regex_lit_part(r#".{0,0}"#, ""),
        template_part(vec![
            template_element("`a`", true)
        ], vec![
        ]),
        template_part(vec![
            template_element("`${", false),
            template_element("}`", true),
        ], vec![
            number_lit_expr("0")
        ]),
        template_part(vec![
            template_element("`0${", false),
            template_element("}2`", true),

        ], vec![
            Expr::Sequence(vec![
                number_lit_expr("0"),
                number_lit_expr("1"),
            ])
        ]),
        template_part(vec![
            template_element("`0${", false),
            template_element("}4`", true),
        ], vec![
            template_expr(vec![
                template_element("`1${", false),
                template_element("}3`", true),
            ], vec![
                number_lit_expr("2")
            ])
        ]),
        template_part(vec![
            template_element(r"`\``", true)
        ], vec![

        ]),
        template_part(vec![
            template_element(r"`a\${b`", true)
        ], vec![

        ]),
        template_part(vec![
            template_element(r"`\0\n\x0A\u000A\u{A}`", true)
        ], vec![

        ]),
        this_part(),
        ProgramPart::Stmt(Stmt::Expr(Expr::ident_from("x"))),
        array(vec![]),
        array(vec![None]),
        array(vec![Some(number_lit_expr("0"))]),
        array(vec![Some(number_lit_expr("0"))]),
        array(vec![None, Some(number_lit_expr("0"))]),
        array(vec![Some(number_lit_expr("0")), Some(number_lit_expr("0"))]),
        array(vec![Some(number_lit_expr("0")), Some(number_lit_expr("0"))]), //60
        array(vec![Some(number_lit_expr("0")), None, Some(number_lit_expr("0"))]),
        array(vec![None, None]),
        obj_lit_part(vec![]),
        obj_lit_part(vec![
            obj_prop(
                obj_key_ident("x"),
                PropValue::None,
                PropKind::Init,
                false,
                false,
                true,
            )
        ]),
        obj_lit_part(vec![
            obj_prop_ident_number("x", "0")
        ]),
        obj_lit_part(vec![
            obj_prop_ident_number("x", "0"),
            obj_prop_ident_number("y", "0"),
        ]),
        obj_lit_part(vec![
            obj_prop_ident_number("x", "0")
        ]),
        obj_lit_part(vec![
            obj_prop_str_single_number("x", "0"),
            obj_prop_str_double_number("y", "0"),
            obj_prop_ident_number("in", "0")
        ]),
        obj_lit_part(vec![
            obj_prop_number_number("0", "0"),
            obj_prop_number_number("0.", "0"),
            obj_prop_number_number("0.0", "0"),
            obj_prop_number_number(".0", "0"),
            obj_prop_number_number("0e0", "0"),
            obj_prop_number_number("0x0", "0"),
            obj_prop(
                obj_key_number("0"),
                obj_value_number("0"),
                PropKind::Init,
                false,
                true,
                false,
            ),
            obj_prop_ident_getter("x"),
            obj_prop_ident_setter("x", vec![
                fn_arg_ident_pat("a")
            ]),
            obj_prop_str_getter_single("y"),
            obj_prop_str_setter_double("y", vec![
                fn_arg_ident_pat("a")
            ]),
            obj_prop_number_getter("0"),
            obj_prop_number_setter("0", vec![
                fn_arg_ident_pat("a")
            ]),
            obj_prop(
                obj_key_ident("var"),
                obj_value_fn(empty_anon_fn(vec![])),
                PropKind::Get,
                false,
                false,
                false,
            ),
            obj_prop(
                obj_key_ident("var"),
                obj_value_fn(empty_anon_fn(vec![
                    fn_arg_ident_pat("a")
                ])),
                PropKind::Set,
                false,
                false,
                false,
            ),
            obj_prop(
                obj_key_number("0"),
                obj_value_fn(empty_anon_fn(vec![])),
                PropKind::Get,
                false,
                true,
                false,
            ),
            obj_prop(
                obj_key_number("0"),
                obj_value_fn(empty_anon_fn(vec![
                    fn_arg_ident_pat("a")
                ])),
                PropKind::Set,
                false,
                true,
                false,
            ),
            obj_prop(
                obj_key_number("1"),
                obj_value_fn(empty_anon_fn(vec![])),
                PropKind::Init,
                true,
                true,
                false,
            ),
            obj_prop_ident_fn("a", empty_anon_fn(vec![]), PropKind::Init),
            obj_prop_sstr_fn("b", empty_anon_fn(vec![]), PropKind::Init),
            obj_prop_dstr_fn("c", empty_anon_fn(vec![]), PropKind::Init),
            obj_prop_number_fn("0", empty_anon_fn(vec![]), PropKind::Init),
            obj_prop_number_fn(".1", empty_anon_fn(vec![]), PropKind::Init),
            obj_prop_number_fn("1.", empty_anon_fn(vec![]), PropKind::Init),
            obj_prop_number_fn("1e1", empty_anon_fn(vec![]), PropKind::Init),
            obj_prop_ident_fn("var", empty_anon_fn(
                long_args()
            ), PropKind::Init),
            obj_prop_ident_setter("in", vec![
                FuncArg::Pat(
                    Pat::Array(
                        long_args_array_pat().into_iter().map(|a| Some(ArrayPatPart::Pat(a))).collect()
                    )
                )
            ]),
            empty_generator_prop(obj_key_ident("d")),
            empty_generator_prop(obj_key_string_single("e")),
            empty_generator_prop(obj_key_string_double("f")),
            empty_generator_prop(obj_key_number("2")),
            empty_generator_prop(obj_key_number(".2")),
            empty_generator_prop(obj_key_number("3.")),
            empty_generator_prop(obj_key_number("2e2")),
            empty_generator_prop(obj_key_ident("in")),
        ]),
        obj_lit_part(vec![
            obj_prop(
                obj_key_ident("__proto__"),
                PropValue::Expr(null_lit_expr()),
                PropKind::Init,
                false,
                false,
                false,
            ),
            obj_prop_ident_getter("__proto__"),
            obj_prop_ident_setter("__proto__", vec![fn_arg_ident_pat("a")])
        ]),
        obj_lit_part(vec![
            obj_prop(
                obj_key_string_double("__proto__"),
                PropValue::Expr(null_lit_expr()),
                PropKind::Init,
                false,
                false,
                false,
            ),
            obj_prop_ident_fn(
                "__proto__",
                empty_anon_fn(vec![]),
                PropKind::Init
            )
        ]),
        member_number_ident_part("0.", "a"),
        member_number_ident_part("0", "a"),
        member_number_ident_part("0", "a"),
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
                Expr::ident_from("x")
            ),
            assign_part(
                assign_left_expr(
                    member_ident_ident_expr("x", "a")
                ),
                Expr::ident_from("x")
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
                        Expr::ident_from("a"),
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
                        number_lit_expr("0"),
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
                        Expr::ident_from("a"),
                        false
                    )
                ),
                vec![]
            ),
            new_part(
                Expr::Member(
                    member(
                        new_ident_expr("x", vec![]),
                        number_lit_expr("0"),
                        true,
                    )
                ),
                vec![]
            ),
            call_ident_part("x", vec![]), //100
            call_part(
                call_ident_expr("x", vec![]),
                vec![],
            ),
            call_ident_part(
                "x", vec![
                    Expr::ident_from("x")
                ]
            ),
            call_ident_part(
                "x", vec![
                    Expr::ident_from("x"),
                    Expr::ident_from("x"),
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
                    Expr::ident_from("a"),
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
                    number_lit_expr("0"),
                    true
                ),
                vec![]
            ),
            call_part(
                member_expr(
                    member_expr(
                        call_expr(
                            Expr::ident_from("x"),
                            vec![]
                        ),
                        Expr::ident_from("a"),
                        false,
                    ),
                    number_lit_expr("0"),
                    true,
                )
                , vec![]
            ),
            call_part(
                Expr::ident_from("x"),
                vec![
                    Expr::Spread(
                        Box::new(Expr::Array(vec![
                            Some(number_lit_expr("0")),
                            Some(number_lit_expr("1")),
                        ]))
                    ),
                    Expr::Spread(
                        Box::new(Expr::Array(vec![]))
                    ),
                    Expr::Spread(
                        Box::new(Expr::Func(
                            Func {
                                id: Some(Ident::from("f")),
                                params: vec![],
                                body: FuncBody(vec![
                                    ProgramPart::Stmt(Stmt::Return(
                                        Some(Expr::Yield(YieldExpr {
                                            argument: Some(Box::new(number_lit_expr("2"))),
                                            delegate: false,
                                        }))
                                    ))
                                ]),
                                generator: true,
                                is_async: false,
                            }
                        ))
                    )
                ]
            ),
            ProgramPart::Stmt(
                Stmt::Expr(
                    Expr::TaggedTemplate(
                        TaggedTemplateExpr {
                            tag: Box::new(Expr::ident_from("x")),
                            quasi: template(vec![
                                template_element("`a`", true),
                            ], vec![])
                        }
                    )
                )
            ),
            ProgramPart::Stmt(
                Stmt::Expr(
                    Expr::TaggedTemplate(
                        TaggedTemplateExpr {
                            tag: Box::new(Expr::ident_from("x")),
                            quasi: template(vec![
                                template_element("`0${", false),
                                template_element("}2`", true),
                            ], vec![
                                number_lit_expr("1"),
                            ])
                        }
                    )
                )
            ),
            update_part(
                Expr::ident_from("x"),
                UpdateOp::Increment,
                false
            ),
            update_part(
                Expr::ident_from("x"),
                UpdateOp::Decrement,
                false
            ),
            unary_part(
                UnaryOp::Delete,
                unary_expr(
                    UnaryOp::Void,
                    unary_expr(
                        UnaryOp::TypeOf,
                        unary_expr(
                            UnaryOp::Plus,
                            unary_expr(
                                UnaryOp::Minus,
                                unary_expr(
                                    UnaryOp::Tilde,
                                    unary_expr(
                                        UnaryOp::Not,
                                        Expr::ident_from("x"),
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
            update_part( //110
                Expr::ident_from("x"),
                UpdateOp::Increment,
                true
            ),
            update_part(
                Expr::ident_from("x"),
                UpdateOp::Decrement,
                true
            ),
            zero_bin_zero_part(BinaryOp::Times),
            zero_bin_zero_part(BinaryOp::Over),
            zero_bin_zero_part(BinaryOp::Mod),
            zero_bin_zero_part(BinaryOp::Plus),
            zero_bin_zero_part(BinaryOp::Minus),
            zero_bin_zero_part(BinaryOp::LeftShift),
            zero_bin_zero_part(BinaryOp::RightShift),
            zero_bin_zero_part(BinaryOp::UnsignedRightShift),
            zero_bin_zero_part(BinaryOp::LessThan), //120
            zero_bin_zero_part(BinaryOp::GreaterThan),
            zero_bin_zero_part(BinaryOp::LessThanEqual),
            zero_bin_zero_part(BinaryOp::GreaterThanEqual),
            zero_bin_expr_part(
                BinaryOp::InstanceOf,
                Expr::Func(empty_anon_fn(vec![])),
            ),
            zero_bin_expr_part(
                BinaryOp::In,
                obj_lit_expr(vec![]),
            ),
            zero_bin_zero_part(BinaryOp::Equal),
            zero_bin_zero_part(BinaryOp::NotEqual),
            zero_bin_zero_part(BinaryOp::StrictEqual),
            zero_bin_zero_part(BinaryOp::StrictNotEqual),
            zero_bin_zero_part(BinaryOp::And),//130
            zero_bin_zero_part(BinaryOp::XOr),
            zero_bin_zero_part(BinaryOp::Or),
            zero_log_zero(LogicalOp::And),
            zero_log_zero(LogicalOp::Or),
            conditional_part(
                number_lit_expr("0"),
                number_lit_expr("0"),
                number_lit_expr("0"),
            ),
            // TODO: Validate this
            conditional_part(
                number_lit_expr("0"),
                conditional_expr(
                    number_lit_expr("0"),
                    number_lit_expr("0"),
                    number_lit_expr("0"),
                ),
                number_lit_expr("0"),
            ),
            conditional_part(
                zero_log_zero_expr(LogicalOp::Or),
                assign_expr(
                    assign_left_ident("x"),
                    number_lit_expr("0"),
                ),
                assign_expr(
                    assign_left_ident("x"),
                    number_lit_expr("0"),
                ),
            ),
            // ^^^^^^^^^^^^^^^^^^^^^^^^^^
            ident_assign_zero_part("x", AssignOp::Equal),
            ident_assign_zero_part("x", AssignOp::TimesEqual),//140
            ident_assign_zero_part("x", AssignOp::DivEqual),
            ident_assign_zero_part("x", AssignOp::ModEqual),
            ident_assign_zero_part("x", AssignOp::PlusEqual),
            ident_assign_zero_part("x", AssignOp::MinusEqual),
            ident_assign_zero_part("x", AssignOp::LeftShiftEqual),
            ident_assign_zero_part("x", AssignOp::RightShiftEqual),
            ident_assign_zero_part("x", AssignOp::UnsignedRightShiftEqual),
            ident_assign_zero_part("x", AssignOp::AndEqual),
            ident_assign_zero_part("x", AssignOp::XOrEqual),
            ident_assign_zero_part("x", AssignOp::OrEqual), //150
            zero_sequence(2),
            zero_sequence(3),
            sequence(vec![
                ident_assign_zero_expr("x", AssignOp::Equal),
                ident_assign_zero_expr("x", AssignOp::Equal),
            ]),
            block_part(vec![]),
            block_part(vec![ProgramPart::Stmt(Stmt::Empty)]),
            block_part(vec![number_lit_part("0")]),
            block_part(vec![number_lit_part("0")]),
            block_part(vec![
                number_lit_part("0"),
                number_lit_part("0"),
            ]),
            block_part(vec![
                number_lit_part("0"),
                number_lit_part("0"),
            ]),
            variable_decl_part(
                VarKind::Var,
                vec![
                    variable_decl_none("x0") //160
                ]
            ),
            variable_decl_part(
                VarKind::Var,
                vec![
                variable_decl_none("x1"),
                variable_decl_none("y2"),
            ]),
            variable_decl_part(VarKind::Var,vec![
                variable_decl_none("x3"),
                variable_decl_none("y4"),
                variable_decl_none("z5"),
            ]),
            variable_decl_part(VarKind::Var,vec![
                variable_decl_zero("x6"),
            ]),
            variable_decl_part(VarKind::Var,vec![
                variable_decl_zero("x7"),
                variable_decl_none("y8"),
            ]),
            variable_decl_part(VarKind::Var,vec![
                variable_decl_none("x9"),
                variable_decl_zero("y10"),
            ]),
            variable_decl_part(VarKind::Var,vec![
                variable_decl_zero("x11"),
                variable_decl_zero("y12"),
            ]),
            ProgramPart::Stmt(Stmt::Empty),
            if_zero_empty(),
            if_zero_empty_else(),
            do_while_zero(),
            number_lit_part("0"),
            do_while_zero(),
            do_while_zero(),
            number_lit_part("0"),
            while_zero(),
            for_exprs_part(None, None, None, Stmt::Break(None)),
            for_exprs_part(
                Some(number_lit_expr("0")),
                Some(number_lit_expr("0")),
                Some(number_lit_expr("0")),
                Stmt::Empty,
            ),
            for_exprs_part(
                Some(zero_bin_expr_expr(
                    BinaryOp::In,
                    array_expr(vec![]),
                )),
                Some(number_lit_expr("0")),
                None,
                Stmt::Empty,
            ),
            for_var_part(
                VarKind::Var,
                vec![
                var_decl("a0")
                ],
                None,
                None,
                Stmt::Break(None),
            ),
            for_var_part(
                VarKind::Var,
                vec![
                var_decl("a1"),
                var_decl("b2"),
                ],
                Some(number_lit_expr("0")),
                Some(number_lit_expr("0")),
                Stmt::Empty,
            ),
            for_var_part(
                VarKind::Var,
                vec![
                variable_decl_zero("a3")
                ],
                None,
                None,
                Stmt::Break(None),
            ),
            for_var_part(
                VarKind::Var,
                vec![
                    VarDecl {
                        id: Pat::ident_from("a4"),
                        init: Some(
                            zero_bin_expr_expr(
                                BinaryOp::In,
                                array_expr(vec![])
                            )
                        )
                    }
                ],
                Some(number_lit_expr("0")),
                None,
                Stmt::Empty,
            ),
            for_in_loop_part(
                LoopLeft::Expr(Expr::ident_from("x")),
                obj_lit_expr(vec![]),
                Stmt::Empty,
            ),
            for_in_loop_part(
                LoopLeft::Variable(
                    VarKind::Var,
                    var_decl("x12")
                ),
                obj_lit_expr(vec![]),
                Stmt::Empty,
            ),
            ProgramPart::Stmt(
                Stmt::ForOf(
                    ForOfStmt {
                        left: LoopLeft::Expr(
                            Expr::ident_from("x")
                        ),
                        right: array_expr(vec![]),
                        body: Box::new(Stmt::Empty),
                        is_await: false
                    }
                )
            ),
            ProgramPart::Stmt(
                Stmt::ForOf(
                    ForOfStmt {
                        left: LoopLeft::Variable(
                            VarKind::Var,
                            var_decl("x13")
                        ),
                        right: array_expr(vec![]),
                        body: Box::new(Stmt::Empty),
                        is_await: false
                    }
                )
            ),
            for_exprs_part(None, Some(number_lit_expr("0")), None, Stmt::Continue(None)),
            labeled_statement_continue("x"),
            for_exprs_part(None, None, None, Stmt::Break(None)),
            preamble_labeled_statement("x"),
            switch_zero_part(vec![
                case_zero(vec![ProgramPart::Stmt(Stmt::Break(None))])
            ]),
            func_part(
                "f0",
                vec![],
                vec![ProgramPart::Stmt(Stmt::Return(None))]
            ),
            func_part(
                "f1",
                vec![],
                vec![ProgramPart::Stmt(Stmt::Return(
                    Some(number_lit_expr("0"))
                ))]
            ),
            switch_zero_part(vec![]),
            switch_zero_part(vec![
                case_zero(vec![]),
            ]),
            switch_zero_part(vec![
                case_zero(vec![]),
                case_zero(vec![]),
            ]),
            switch_zero_part(vec![
                default_case(vec![]),
            ]),
            switch_zero_part(vec![
                case_zero(vec![]),
                default_case(vec![]),
                case_zero(vec![]),
            ]),
            switch_zero_part(vec![
                case_zero(vec![
                    empty_part(),
                ]),
            ]),
            switch_zero_part(vec![
                case_zero(vec![
                    empty_part(),
                    empty_part(),
                ]),
            ]),
            switch_zero_part(vec![
                default_case(vec![
                    empty_part(),
                ]),
            ]),
            switch_zero_part(vec![
                default_case(vec![
                    empty_part(),
                    empty_part(),
                ]),
            ]),
            labeled_part("x", Stmt::Empty),
            labeled_part(
                "x",
                labeled_stmt("y", Stmt::Empty)
            ),
            try_part(
                vec![
                    throw_part(
                        number_lit_expr("0")
                    )
                ],
                Some(catch_(Some(Pat::ident_from("x")), vec![])),
                None
            ),
            try_part(
                vec![],
                Some(catch_(Some(Pat::ident_from("x")), vec![])),
                None
            ),
            try_part(
                vec![],
                None,
                Some(vec![])
            ),
            try_part(
                vec![],
                Some(catch_(Some(Pat::ident_from("x")), vec![])),
                Some(vec![])
            ),
            ProgramPart::Stmt(Stmt::Debugger),
            func_part("f2", vec![], vec![]),
            func_part(
                "f3",
                vec![
                    fn_arg_ident_pat("x"),
                ],
                vec![]
            ),
            func_part(
                "f4",
                vec![
                    fn_arg_ident_pat("x"),
                    fn_arg_ident_pat("y"),
                ],
                vec![]
            ),
            func_part(
                "f5",
                vec![],
                vec![
                    func_part(
                        "f6",
                        vec![],
                        vec![]
                    ),
                ]
            ),
        block_part(vec![
            func_part("f7", vec![], vec![])
        ]),
        empty_part(),
        for_exprs_part(
            None,
            Some(number_lit_expr("0")),
            None,
            Stmt::Expr(
                Expr::Unary(
                    UnaryExpr {
                        operator: UnaryOp::Plus,
                        prefix: true,
                        argument: Box::new(
                            func_expr("f8", vec![], vec![]),
                        )
                    }
                )
            )
        ),
        number_lit_part("0"),

        ProgramPart::Stmt(
            Stmt::DoWhile(
                DoWhileStmt {
                    body: Box::new(
                        Stmt::Expr(
                            Expr::Unary(
                                UnaryExpr {
                                    operator: UnaryOp::Plus,
                                    argument: Box::new(func_expr("f9", vec![], vec![])),
                                    prefix: true,
                                }
                            )
                        )
                    ),
                    test: number_lit_expr("0")
                }
            )
        ),
        func_part("f10", long_args(), vec![]),
        func_part(
                "f11",
                vec![],
                vec![
                    directive_part_double("use strict"),
                ],
            ),
            func_part(
                "f12",
                vec![],
                vec![
                    directive_part_single("use strict"),
                ],
            ),
            func_part(
                "f13",
                vec![],
                vec![
                    directive_part_double("other directive"),
                ],
            ),
            func_part(
                "f14",
                vec![],
                vec![
                    directive_part_single("other directive"),
                ],
            ),
            func_part(
                "f15",
                vec![],
                vec![
                    string_lit_double_part("string"),
                ],
            ),
            func_part(
                "f16",
                vec![],
                vec![
                    string_lit_single_part("string"),
                ],
            ),
            func_part(
                "f17",
                vec![],
                vec![
                    ProgramPart::Stmt(
                        Stmt::Expr(
                            Expr::Binary(
                                binary(
                                    string_lit_single_expr("string"),
                                    BinaryOp::Plus,
                                    number_lit_expr("0")
                                )
                            )
                        )
                    ),
                ],
            ),
            ProgramPart::Decl(
                Decl::Func(
                    Func {
                        params: long_args(),
                        id: Some(Ident::from("g0")),
                        body: FuncBody(long_gen_body()),
                        generator: true,
                        is_async: false,
                    }
                )
            ),
            ProgramPart::Stmt(
                Stmt::Expr(
                    Expr::Call(
                        CallExpr {
                            callee: Box::new(
                                Expr::Func(
                                    Func {
                                        params: long_args(),
                                        id: Some(Ident::from("g1")),
                                        body: FuncBody(long_gen_body()),
                                        generator: true,
                                        is_async: false,
                                    }
                                )
                            ),
                            arguments: vec![
                                Expr::Func(
                                    anon_fn(vec![], vec![], false)
                                )
                            ]
                        }
                    )
                )
            ),
            func_expr_part(None, vec![
                fn_arg_ident_pat("x")
            ], vec![]),
            func_expr_part(None, vec![
                fn_arg_ident_pat("x"),
                fn_arg_ident_pat("y"),
            ], vec![]),
            func_expr_part(None, vec![], vec![
                func_part("f", vec![], vec![]),
            ]),
            func_expr_part(Some("f"), vec![], vec![]),
            func_expr_part(Some("f"), vec![
                fn_arg_ident_pat("x")
            ], vec![]),
            func_expr_part(Some("f"), vec![
                fn_arg_ident_pat("x"),
                fn_arg_ident_pat("y"),
            ], vec![]),
            func_expr_part(Some("f"), vec![], vec![
                func_part("f", vec![], vec![]),
            ]),
            arrow_expr_part(vec![], number_lit_expr("0")),
            arrow_expr_body_part(vec![],vec![
                empty_part()
            ]),
            arrow_expr_part(vec![
                fn_arg_ident_expr("x"),
            ], Expr::ident_from("x")),
            arrow_expr_part(vec![
                fn_arg_ident_expr("x"),
            ], assign_expr(
                assign_left_ident("x"),
                number_lit_expr("0"))),
            arrow_expr_part(vec![
                fn_arg_ident_expr("x")
            ], Expr::ArrowFunc(arrow_expr(vec![
                fn_arg_ident_expr("y")
            ], Expr::ident_from("x")))),
            arrow_expr_body_part(vec![
                fn_arg_ident_expr("x")
            ], vec![
                ident_stmt("x")
            ]),
            arrow_expr_part(vec![
                fn_arg_ident_expr("x"),
            ], obj_lit_expr(vec![
                    obj_prop(
                        obj_key_ident("x"),
                        PropValue::None,
                        PropKind::Init,
                        false,
                        false,
                        true
                    )
                ])
            ),
            arrow_expr_part(vec![
                fn_arg_ident_expr("x"),
            ], Expr::ident_from("x")),
            arrow_expr_body_part(vec![
                fn_arg_ident_expr("x"),
            ],vec![
                ProgramPart::Stmt(
                    return_ident_stmt("x")
                )
            ]),
            arrow_expr_part(vec![
                fn_arg_ident_expr("x"),
            ], obj_lit_expr(vec![
                    obj_prop(
                        obj_key_ident("x"),
                        PropValue::None,
                        PropKind::Init,
                        false,
                        false,
                        true
                    )
                ])
            ),
            arrow_expr_part(vec![
                FuncArg::Expr(
                    Expr::Obj(vec![
                        obj_prop(
                            obj_key_ident("x"),
                            PropValue::None,
                            PropKind::Init,
                            false,
                            false,
                            true,
                        )
                    ])
                ),
            ], obj_lit_expr(vec![
                    obj_prop(
                        obj_key_ident("x"),
                        PropValue::None,
                        PropKind::Init,
                        false,
                        false,
                        true
                    )
                ])
            ),
            arrow_expr_body_part(
                long_args_array_expr(),
                vec![
                    empty_part()
                ]
            ),
            assign_part(
                assign_left_expr(
                    array_expr(vec![
                        Some(Expr::ident_from("a"))
                    ])
                ),
                array_expr(vec![
                    Some(Expr::Spread(
                        Box::new(
                            array_expr(vec![
                                Some(number_lit_expr("0"))
                            ])
                        )
                    ))
                ])
            ),
            assign_part(
                assign_left_expr(
                    obj_lit_expr(vec![
                        obj_prop(
                            obj_key_ident("a"),
                            PropValue::None,
                            PropKind::Init,
                            false,
                            false,
                            true,
                        )
                    ])
                ),
                obj_lit_expr(vec![])
            ),
            try_part(vec![], Some(
                CatchClause {
                    param: Some(
                        Pat::Array(vec![
                            Some(ArrayPatPart::Pat(
                                Pat::ident_from("e")
                            ))
                        ]
                        )
                    ),
                    body: vec![]
                }
            ), None),
            try_part(vec![], Some(
                CatchClause {
                    param: Some(
                        Pat::Obj(vec![
                            ObjPatPart::Assign(
                                Prop {
                                    key: obj_key_ident("e"),
                                    value: PropValue::None,
                                    kind: PropKind::Init,
                                    method: false,
                                    computed: false,
                                    short_hand: true,
                                    is_static: false,
                                }
                            )
                        ]
                        )
                    ),
                    body: vec![]
                }
            ), None),
            class_part(
                "A",
                None,
                vec![],
            ),
            class_part(
                "B",
                Some(new_expr(Expr::ident_from("A"), vec![])),
                vec![
                    class_prop(
                        obj_key_ident("constructor"),
                        anon_fn(long_args(), vec![
                            call_part(Expr::Super, vec![
                                Expr::MetaProp(
                                    MetaProp {
                                        meta: Ident::from("new"),
                                        property: Ident::from("target")
                                    }
                                )
                            ]),
                            ProgramPart::Stmt(
                                Stmt::Expr(
                                    Expr::TaggedTemplate(
                                        TaggedTemplateExpr {
                                            tag: Box::new(Expr::Call(
                                                CallExpr {
                                                    callee: Box::new(Expr::Super),
                                                    arguments: vec![]
                                                }
                                            )),
                                            quasi: template(vec![
                                                template_element("`template`", true),
                                            ], vec![])
                                        }
                                    )
                                )
                            ),
                            arrow_expr_part(vec![], call_expr(Expr::Super, vec![
                                Expr::This
                            ])),
                        ],
                        false,
                    ),
                    true,
                    false),
                    class_prop(
                        obj_key_ident("m"),
                        anon_fn(long_args(), vec![
                            call_part(
                                member_expr(
                                    Expr::Super,
                                    Expr::ident_from("m"),
                                    false,
                                ),
                                vec![]
                            ),
                            ProgramPart::Stmt(
                                Stmt::Expr(
                                    Expr::TaggedTemplate(
                                        TaggedTemplateExpr {
                                            tag: Box::new(
                                                member_expr(Expr::Super, Expr::ident_from("m"), false)
                                            ),
                                            quasi: template(vec![
                                                template_element("`template`", true),
                                            ], vec![])
                                        }
                                    )
                                )
                            ),
                            arrow_expr_part(vec![], call_expr(
                                member_expr(Expr::Super, Expr::ident_from("m"), false),
                                vec![
                                    Expr::This
                                ]
                            )),
                        ], false),
                        false,
                        false,
                    ),
                    class_prop(
                        obj_key_ident("a"),
                        anon_fn(vec![], vec![], false),
                        false,
                        true,
                    ),
                    class_prop(
                        obj_key_string_single("b"),
                        anon_fn(vec![], vec![], false),
                        false,
                        true,
                    ),
                    class_prop(
                        obj_key_number("0"),
                        anon_fn(vec![], vec![], false),
                        false,
                        true
                    ),
                    class_prop_computed(
                        obj_key_number("0"),
                        anon_fn(vec![], vec![], false),
                        false,
                        true
                    ),
                    class_prop(
                        obj_key_ident("c"),
                        anon_fn(vec![], vec![
                            yield_part(None, false),
                        ], true),
                        false,
                        true
                    ),
                    class_prop(
                        obj_key_string_double("d"),
                        anon_fn(vec![], vec![
                            yield_part(None, false),
                        ], true),
                        false,
                        true
                    ),
                    class_prop(
                        obj_key_number("1"),
                        anon_fn(vec![], vec![
                            yield_part(None, false),
                        ], true),
                        false,
                        true
                    ),
                    class_prop_computed(
                        obj_key_number("1"),
                        anon_fn(vec![], vec![
                            yield_part(None, false),
                        ], true),
                        false,
                        true
                    ),
                    class_prop(
                        obj_key_ident("var"),
                        anon_fn(vec![], vec![

                        ], false),
                        false,
                        true
                    ),
                    class_prop(
                        obj_key_ident("in"),
                        anon_fn(vec![], vec![
                        ], true),
                        false,
                        true
                    ),
                    class_prop_getter(
                        obj_key_ident("e"),
                        anon_fn(vec![], vec![], false),
                        true,
                        false,
                    ),
                    class_prop_getter(
                        obj_key_string_single("f"),
                        anon_fn(vec![], vec![], false),
                        true,
                        false
                    ),
                    class_prop_getter(
                        obj_key_number("2"),
                        anon_fn(vec![], vec![], false),
                        true,
                        false
                    ),
                    class_prop_getter(
                        obj_key_number("2"),
                        anon_fn(vec![], vec![], false),
                        true,
                        true
                    ),
                    class_prop_setter(
                        obj_key_ident("g"),
                        anon_fn(vec![
                            fn_arg_ident_pat("a")
                        ], vec![], false),
                        true,
                        false,
                    ),
                    class_prop_setter(
                        obj_key_string_double("h"),
                        anon_fn(vec![
                            fn_arg_ident_pat("a")
                        ], vec![], false),
                        true,
                        false,
                    ),
                    class_prop_setter(
                        obj_key_number("3"),
                        anon_fn(vec![
                            fn_arg_ident_pat("a")
                        ], vec![], false),
                        true,
                        false,
                    ),
                    class_prop_setter(
                        obj_key_number("3"),
                        anon_fn(vec![
                            fn_arg_ident_pat("a")
                        ], vec![], false),
                        true,
                        true,
                    ),
                    class_prop_getter(
                        obj_key_ident("if"),
                        anon_fn(vec![], vec![], false),
                        true,
                        false,
                    ),
                    class_prop_setter(
                        obj_key_ident("if"),
                        anon_fn(vec![
                            fn_arg_ident_pat("a")
                        ], vec![], false),
                        true,
                        false,
                    ),
                    class_prop(
                        obj_key_ident("a"),
                        anon_fn(vec![], vec![], false),
                        false,
                        false,
                    ),
                    class_prop(
                        obj_key_string_single("b"),
                        anon_fn(vec![], vec![], false),
                        false,
                        false,
                    ),
                    class_prop(
                        obj_key_number("0"),
                        anon_fn(vec![], vec![], false),
                        false,
                        false,
                    ),
                    class_prop_computed(
                        obj_key_number("0"),
                        anon_fn(vec![], vec![], false),
                        false,
                        false,
                    ),
                    class_prop(
                        obj_key_ident("c"),
                        anon_fn(vec![], vec![
                            yield_part(None, false)
                        ], true),
                        false,
                        false,
                    ),
                    class_prop(
                        obj_key_string_double("d"),
                        anon_fn(vec![], vec![
                            yield_part(None, false)
                        ], true),
                        false,
                        false,
                    ),
                    class_prop(
                        obj_key_number("1"),
                        anon_fn(vec![], vec![
                            yield_part(None, false)
                        ], true),
                        false,
                        false,
                    ),
                    class_prop_computed(
                        obj_key_number("1"),
                        anon_fn(vec![], vec![
                            yield_part(None, false)
                        ], true),
                        false,
                        false,
                    ),
                    class_prop(
                        obj_key_ident("var"),
                        anon_fn(vec![], vec![
                        ], false),
                        false,
                        false,
                    ),
                    class_prop(
                        obj_key_ident("in"),
                        anon_fn(vec![], vec![
                            yield_part(None, false)
                        ], true),
                        false,
                        false,
                    ),
                    class_prop_getter(
                        obj_key_ident("e"),
                        anon_fn(vec![], vec![], false),
                        false,
                        false,
                    ),
                    class_prop_getter(
                        obj_key_string_single("f"),
                        anon_fn(vec![], vec![], false),
                        false,
                        false,
                    ),
                    class_prop_getter(
                        obj_key_number("2"),
                        anon_fn(vec![], vec![], false),
                        false,
                        false,
                    ),
                    class_prop_getter(
                        obj_key_number("2"),
                        anon_fn(vec![], vec![], false),
                        false,
                        true,
                    ),
                    class_prop_setter(
                        obj_key_ident("g"),
                        anon_fn(vec![fn_arg_ident_pat("a")], vec![], false),
                        false,
                        false,
                    ),
                    class_prop_setter(
                        obj_key_string_double("h"),
                        anon_fn(vec![fn_arg_ident_pat("a")], vec![], false),
                        false,
                        false,
                    ),
                    class_prop_setter(
                        obj_key_number("3"),
                        anon_fn(vec![fn_arg_ident_pat("a")], vec![], false),
                        false,
                        false,
                    ),
                    class_prop_setter(
                        obj_key_number("3"),
                        anon_fn(vec![fn_arg_ident_pat("a")], vec![], false),
                        false,
                        true,
                    ),
                    class_prop_getter(
                        obj_key_ident("if"),
                        anon_fn(vec![], vec![
                        ], false),
                        false,
                        false,
                    ),
                    class_prop_setter(
                        obj_key_ident("if"),
                        anon_fn(vec![fn_arg_ident_pat("f")], vec![], false),
                        false,
                        false,
                    ),
                ]
            ),
            class_part("C", Some(
                Expr::ident_from("B")
            ), vec![
                class_prop(obj_key_string_double("constructor"), anon_fn(vec![], vec![
                    call_part(Expr::Super, vec![])
                ], false), true, false),
            ])
        // ProgramPart::Stmt(Stmt::Empty)
    ];
}

fn yield_part(e: Option<E>, d: bool) -> Part {
    ProgramPart::Stmt(Stmt::Expr(Expr::Yield(YieldExpr {
        delegate: d,
        argument: e.map(Box::new),
    })))
}

fn class_part(i: &'static str, s: Option<E>, body: Vec<PR>) -> Part {
    ProgramPart::Decl(class_decl(i, s, body))
}

fn class_decl(i: &'static str, s: Option<E>, body: Vec<PR>) -> Decl<'static> {
    Decl::Class(Class {
        id: Some(Ident::from(i)),
        super_class: s.map(Box::new),
        body,
    })
}

fn class_prop(key: PK, value: F, ctor: bool, is_static: bool) -> PR {
    Prop {
        key,
        value: PropValue::Expr(Expr::Func(value)),
        kind: if ctor {
            PropKind::Ctor
        } else {
            PropKind::Method
        },
        method: true,
        short_hand: false,
        computed: false,
        is_static,
    }
}

fn class_prop_computed(key: PK, value: F, ctor: bool, is_static: bool) -> PR {
    Prop {
        key,
        value: PropValue::Expr(Expr::Func(value)),
        kind: if ctor {
            PropKind::Ctor
        } else {
            PropKind::Method
        },
        method: true,
        short_hand: false,
        computed: true,
        is_static,
    }
}

fn class_prop_getter(key: PK, value: F, is_static: bool, computed: bool) -> PR {
    Prop {
        key,
        value: PropValue::Expr(Expr::Func(value)),
        kind: PropKind::Get,
        method: false,
        short_hand: false,
        computed,
        is_static,
    }
}
fn class_prop_setter(key: PK, value: F, is_static: bool, computed: bool) -> PR {
    Prop {
        key,
        value: PropValue::Expr(Expr::Func(value)),
        kind: PropKind::Set,
        method: false,
        short_hand: false,
        computed,
        is_static,
    }
}

fn long_gen_body() -> Vec<Part> {
    vec![ProgramPart::Stmt(Stmt::Return(Some(Expr::Assign(
        AssignExpr {
            left: assign_left_ident("a"),
            right: Box::new(Expr::Yield(YieldExpr {
                delegate: true,
                argument: Some(Box::new(Expr::Assign(AssignExpr {
                    left: assign_left_ident("b"),
                    right: Box::new(Expr::Yield(YieldExpr {
                        delegate: false,
                        argument: Some(Box::new(Expr::Assign(AssignExpr {
                            left: assign_left_ident("c"),
                            operator: AssignOp::Equal,
                            right: Box::new(Expr::Yield(YieldExpr {
                                delegate: false,
                                argument: Some(Box::new(Expr::Yield(YieldExpr {
                                    delegate: false,
                                    argument: None,
                                }))),
                            })),
                        }))),
                    })),
                    operator: AssignOp::Equal,
                }))),
            })),
            operator: AssignOp::Equal,
        },
    ))))]
}

fn empty_generator_prop(key: PK) -> OP {
    obj_prop(
        key,
        PropValue::Expr(Expr::Func(Func {
            id: None,
            params: vec![],
            body: FuncBody(vec![]),
            generator: true,
            is_async: false,
        })),
        PropKind::Init,
        true,
        false,
        false,
    )
}
fn normal_import(name: &'static str, local: &'static str) -> ImportSpecifier<'static> {
    ImportSpecifier::Normal(NormalImportSpec {
        local: Ident::from(local),
        imported: Ident::from(name),
    })
}
fn long_args_array_pat() -> Vec<P> {
    vec![
        Pat::ident_from("a"),
        Pat::Assign(AssignPat {
            left: Box::new(Pat::ident_from("b")),
            right: Box::new(number_lit_expr("0")),
        }),
        Pat::Array(vec![
            Some(ArrayPatPart::Pat(Pat::ident_from("c"))),
            None,
            Some(ArrayPatPart::Pat(Pat::Assign(AssignPat {
                left: Box::new(Pat::ident_from("d")),
                right: Box::new(number_lit_expr("0")),
            }))),
            Some(ArrayPatPart::Pat(Pat::RestElement(Box::new(
                Pat::ident_from("e"),
            )))),
        ]),
        Pat::Obj(vec![
            ObjPatPart::Assign(Prop {
                key: obj_key_ident("f"),
                value: PropValue::None,
                kind: PropKind::Init,
                method: false,
                computed: false,
                short_hand: true,
                is_static: false,
            }),
            // g: h
            ObjPatPart::Assign(Prop {
                key: obj_key_ident("g"),
                value: PropValue::Pat(Pat::ident_from("h")),
                kind: PropKind::Init,
                method: false,
                computed: false,
                short_hand: false,
                is_static: false,
            }),
            //i = 0,
            ObjPatPart::Assign(Prop {
                key: obj_key_ident("i"),
                value: obj_value_number("0"),
                kind: PropKind::Init,
                method: false,
                computed: false,
                short_hand: true,
                is_static: false,
            }),
            // i: j = 0
            ObjPatPart::Assign(Prop {
                key: obj_key_ident("i"),
                value: PropValue::Pat(Pat::Assign(AssignPat {
                    left: Box::new(Pat::ident_from("j")),
                    right: Box::new(number_lit_expr("0")),
                })),
                kind: PropKind::Init,
                method: false,
                short_hand: false,
                computed: false,
                is_static: false,
            }),
        ]),
        Pat::RestElement(Box::new(Pat::ident_from("k"))),
    ]
}
fn long_args_array_expr() -> Vec<FA> {
    vec![
        FuncArg::Expr(Expr::ident_from("a")),
        FuncArg::Expr(Expr::Assign(AssignExpr {
            operator: AssignOp::Equal,
            left: AssignLeft::Expr(Box::new(Expr::ident_from("b"))),
            right: Box::new(number_lit_expr("0")),
        })),
        FuncArg::Expr(array_expr(vec![
            Some(Expr::ident_from("c")),
            None,
            Some(Expr::Assign(AssignExpr {
                operator: AssignOp::Equal,
                left: AssignLeft::Expr(Box::new(Expr::ident_from("d"))),
                right: Box::new(number_lit_expr("0")),
            })),
            Some(Expr::Spread(Box::new(Expr::ident_from("e")))),
        ])),
        FuncArg::Expr(Expr::Obj(vec![
            obj_prop(
                obj_key_ident("f"),
                PropValue::None,
                PropKind::Init,
                false,
                false,
                true,
            ),
            obj_prop(
                obj_key_ident("g"),
                PropValue::Expr(Expr::ident_from("h")),
                PropKind::Init,
                false,
                false,
                false,
            ),
            obj_prop(
                obj_key_ident("i"),
                obj_value_number("0"),
                PropKind::Init,
                false,
                false,
                true,
            ),
            obj_prop(
                obj_key_ident("i"),
                PropValue::Expr(Expr::Assign(AssignExpr {
                    operator: AssignOp::Equal,
                    left: AssignLeft::Expr(Box::new(Expr::ident_from("j"))),
                    right: Box::new(number_lit_expr("0")),
                })),
                PropKind::Init,
                false,
                false,
                false,
            ),
        ])),
        FuncArg::Pat(Pat::RestElement(Box::new(Pat::ident_from("k")))),
    ]
}

fn long_args() -> Vec<FA> {
    long_args_array_pat()
        .into_iter()
        .map(FuncArg::Pat)
        .collect()
}

fn directive_part_single(dir: &'static str) -> Part {
    ProgramPart::Dir(Dir {
        expr: string_lit_single(dir),
        dir: ::std::borrow::Cow::Borrowed(dir),
    })
}
fn directive_part_double(dir: &'static str) -> Part {
    ProgramPart::Dir(Dir {
        expr: string_lit_double(dir),
        dir: ::std::borrow::Cow::Borrowed(dir),
    })
}

fn empty_part() -> Part {
    ProgramPart::Stmt(Stmt::Empty)
}

fn preamble_labeled_statement(label: &'static str) -> ProgramPart {
    ProgramPart::Stmt(Stmt::Labeled(LabeledStmt {
        label: Ident::from(label),
        body: Box::new(Stmt::For(ForStmt {
            init: None,
            test: None,
            update: None,
            body: Box::new(Stmt::Break(Some(Ident::from(label)))),
        })),
    }))
}
fn labeled_statement_continue(label: &'static str) -> ProgramPart {
    ProgramPart::Stmt(Stmt::Labeled(LabeledStmt {
        label: Ident::from(label),
        body: Box::new(Stmt::For(ForStmt {
            init: None,
            test: Some(number_lit_expr("0")),
            update: None,
            body: Box::new(Stmt::Continue(Some(Ident::from(label)))),
        })),
    }))
}

fn labeled_part(label: &'static str, body: S) -> Part {
    ProgramPart::Stmt(labeled_stmt(label, body))
}

fn labeled_stmt(label: &'static str, body: S) -> S {
    Stmt::Labeled(labeled(label, body))
}

fn labeled(label: &'static str, body: S) -> LabeledStmt {
    LabeledStmt {
        label: Ident::from(label),
        body: Box::new(body),
    }
}

fn line_term(label: &str) -> ProgramPart {
    ProgramPart::Stmt(Stmt::Labeled(LabeledStmt {
        label: Ident::from(label),
        body: Box::new(Stmt::Expr(Expr::Lit(Lit::number_from("0")))),
    }))
}

fn export_spec(local: &'static str, exported: &'static str) -> ExportSpecifier<'static> {
    ExportSpecifier {
        local: Ident::from(local),
        exported: Ident::from(exported),
    }
}

fn number_lit_part(number: &'static str) -> ProgramPart {
    ProgramPart::Stmt(number_lit_stmt(number))
}

fn number_lit_stmt(number: &'static str) -> Stmt<'static> {
    Stmt::Expr(number_lit_expr(number))
}

fn number_lit_expr(number: &'static str) -> Expr<'static> {
    Expr::Lit(number_lit(number))
}

fn number_lit(number: &'static str) -> Lit<'static> {
    Lit::number_from(number)
}

fn null_lit_part() -> ProgramPart<'static> {
    ProgramPart::Stmt(null_lit_stmt())
}

fn null_lit_stmt() -> Stmt<'static> {
    Stmt::Expr(null_lit_expr())
}

fn null_lit_expr() -> Expr<'static> {
    Expr::Lit(null_lit())
}

fn null_lit() -> Lit<'static> {
    Lit::Null
}

fn bool_lit_part(b: bool) -> ProgramPart<'static> {
    ProgramPart::Stmt(bool_lit_stmt(b))
}

fn bool_lit_stmt(b: bool) -> Stmt<'static> {
    Stmt::Expr(bool_lit_expr(b))
}
fn bool_lit_expr(b: bool) -> Expr<'static> {
    Expr::Lit(bool_lit(b))
}

fn bool_lit(b: bool) -> Lit<'static> {
    Lit::Boolean(b)
}

fn var_decl_one() -> ProgramPart<'static> {
    ProgramPart::Decl(Decl::Var(
        VarKind::Var,
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
        ]),
    ))
}

fn var_decl_one_2015() -> Part {
    ProgramPart::Decl(Decl::Var(
        VarKind::Var,
        var_decls(&[
            r"$",
            r"_",
            r"\u0078",
            r"\u{2F9F9}",
            r"x$",
            r"x_",
            r"x\u0030",
            r"x\u{e01d5}",
            r"xa",
            r"x0",
            r"x0a",
            r"x0123456789",
            r"qwertyuiopasdfghjklzxcvbnm",
            r"QWERTYUIOPASDFGHJKLZXCVBNM",
        ]),
    ))
}

fn var_decl_two() -> ProgramPart<'static> {
    ProgramPart::Decl(Decl::Var(
        VarKind::Var,
        var_decls(&[
            r"œ一",
            r"ǻ둘",
            r"ɤ〩",
            r"φ",
            r"ﬁⅷ",
            r"ユニコード",
            r"x‌‍",
        ]),
    ))
}
fn var_decl_two_2015() -> ProgramPart<'static> {
    ProgramPart::Decl(Decl::Var(
        VarKind::Var,
        var_decls(&[
            r"䩶",
            r"x󠇕",
            r"œ一",
            r"ǻ둘",
            r"ɤ〩",
            r"φ",
            r"ﬁⅷ",
            r"ユニコード",
            r"x‌‍",
        ]),
    ))
}

fn var_decls(decls: &[&'static str]) -> Vec<VarDecl<'static>> {
    decls.iter().map(|s| var_decl(*s)).collect()
}

fn var_decl(id: &'static str) -> VarDecl {
    VarDecl {
        id: Pat::ident_from(id),
        init: None,
    }
}

fn var_decl_init(id: &'static str, init: E) -> VarDecl<'static> {
    VarDecl {
        id: Pat::ident_from(id),
        init: Some(init),
    }
}

fn string_lit_double_part(s: &'static str) -> Part {
    ProgramPart::Stmt(string_lit_double_stmt(s))
}
fn string_lit_single_part(s: &'static str) -> Part {
    ProgramPart::Stmt(string_lit_single_stmt(s))
}

fn string_lit_double_stmt(s: &'static str) -> S {
    Stmt::Expr(string_lit_double_expr(s))
}
fn string_lit_single_stmt(s: &'static str) -> S {
    Stmt::Expr(string_lit_single_expr(s))
}

fn string_lit_single_expr(s: &'static str) -> E {
    Expr::Lit(string_lit_single(s))
}
fn string_lit_double_expr(s: &'static str) -> E {
    Expr::Lit(string_lit_double(s))
}

fn string_lit_double(s: &'static str) -> L {
    Lit::double_string_from(s)
}
fn string_lit_single(s: &'static str) -> L {
    Lit::single_string_from(s)
}

fn regex_lit_part(pattern: &'static str, flags: &'static str) -> ProgramPart<'static> {
    ProgramPart::Stmt(regex_lit_stmt(pattern, flags))
}

fn regex_lit_stmt(pattern: &'static str, flags: &'static str) -> Stmt<'static> {
    Stmt::Expr(regex_lit_expr(pattern, flags))
}

fn regex_lit_expr(pattern: &'static str, flags: &'static str) -> Expr<'static> {
    Expr::Lit(regex_lit(pattern, flags))
}

fn regex_lit(pattern: &'static str, flags: &'static str) -> Lit<'static> {
    Lit::RegEx(regex(pattern, flags))
}

fn regex(pattern: &'static str, flags: &'static str) -> RegEx<'static> {
    RegEx::from(pattern, flags)
}

fn this_part() -> ProgramPart<'static> {
    ProgramPart::Stmt(this_stmt())
}

fn this_stmt() -> Stmt<'static> {
    Stmt::Expr(this_expr())
}

fn this_expr() -> Expr<'static> {
    Expr::This
}

fn ident_stmt(id: &'static str) -> ProgramPart<'static> {
    ProgramPart::Stmt(Stmt::Expr(Expr::ident_from(id)))
}

fn array(content: Vec<Option<Expr<'static>>>) -> ProgramPart<'static> {
    ProgramPart::Stmt(Stmt::Expr(array_expr(content)))
}

fn array_expr(content: Vec<Option<E>>) -> E {
    Expr::Array(content)
}

fn obj_lit_part(content: Vec<OP>) -> Part {
    ProgramPart::Stmt(obj_lit_stmt(content))
}

fn obj_lit_stmt(content: Vec<OP>) -> S {
    Stmt::Expr(obj_lit_expr(content))
}

fn obj_lit_expr(content: Vec<OP>) -> E {
    Expr::Obj(content)
}

type PK = PropKey<'static>;
type PV = PropValue<'static>;
fn obj_prop_ident_number(ident: &'static str, number: &'static str) -> OP {
    obj_prop(
        obj_key_ident(ident),
        obj_value_number(number),
        PropKind::Init,
        false,
        false,
        false,
    )
}
fn obj_prop_str_double_number(s: &'static str, number: &'static str) -> OP {
    obj_prop(
        obj_key_string_double(s),
        obj_value_number(number),
        PropKind::Init,
        false,
        false,
        false,
    )
}
fn obj_prop_str_single_number(s: &'static str, number: &'static str) -> OP {
    obj_prop(
        obj_key_string_single(s),
        obj_value_number(number),
        PropKind::Init,
        false,
        false,
        false,
    )
}
fn obj_prop_number_number(n: &'static str, number: &'static str) -> OP {
    obj_prop(
        obj_key_number(n),
        obj_value_number(number),
        PropKind::Init,
        false,
        false,
        false,
    )
}
type F = Func<'static>;
type FA = FuncArg<'static>;
fn empty_anon_fn(args: Vec<FA>) -> F {
    Func {
        id: None,
        generator: false,
        is_async: false,
        body: FuncBody(vec![]),
        params: args,
    }
}

fn anon_fn(args: Vec<FA>, body: Vec<Part>, gen: bool) -> F {
    Func {
        id: None,
        generator: gen,
        is_async: false,
        body: FuncBody(body),
        params: args,
    }
}

fn func_part(id: &'static str, args: Vec<FA>, body: Vec<Part>) -> Part {
    ProgramPart::Decl(Decl::Func(func(id, args, body)))
}

fn func_expr_part(id: Option<&'static str>, args: Vec<FA>, body: Vec<Part>) -> Part {
    let f = if let Some(id) = id {
        func(id, args, body)
    } else {
        Func {
            id: None,
            params: args,
            body: FuncBody(body),
            generator: false,
            is_async: false,
        }
    };
    ProgramPart::Stmt(Stmt::Expr(Expr::Func(f)))
}

fn func_expr(id: &'static str, args: Vec<FA>, body: Vec<Part>) -> E {
    Expr::Func(func(id, args, body))
}

fn func(id: &'static str, args: Vec<FA>, body: Vec<Part>) -> F {
    Func {
        id: Some(Ident::from(id)),
        generator: false,
        is_async: false,
        body: FuncBody(body),
        params: args,
    }
}
fn func_generator(id: &'static str, args: Vec<FA>, body: Vec<Part>) -> F {
    Func {
        id: Some(Ident::from(id)),
        generator: true,
        is_async: false,
        body: FuncBody(body),
        params: args,
    }
}

fn fn_arg_ident_pat(i: &'static str) -> FA {
    FuncArg::Pat(Pat::ident_from(i))
}
fn fn_arg_ident_expr(i: &'static str) -> FA {
    FuncArg::Expr(Expr::ident_from(i))
}

fn obj_prop_ident_fn(i: &'static str, f: Func<'static>, kind: PropKind) -> OP {
    obj_prop(obj_key_ident(i), obj_value_fn(f), kind, true, false, false)
}
fn obj_prop_dstr_fn(i: &'static str, f: Func<'static>, kind: PropKind) -> OP {
    obj_prop(
        obj_key_string_double(i),
        obj_value_fn(f),
        kind,
        true,
        false,
        false,
    )
}
fn obj_prop_sstr_fn(i: &'static str, f: Func<'static>, kind: PropKind) -> OP {
    obj_prop(
        obj_key_string_single(i),
        obj_value_fn(f),
        kind,
        true,
        false,
        false,
    )
}
fn obj_prop_number_fn(n: &'static str, f: Func<'static>, kind: PropKind) -> OP {
    obj_prop(obj_key_number(n), obj_value_fn(f), kind, true, false, false)
}
fn obj_prop_ident_getter(i: &'static str) -> OP {
    obj_prop(
        obj_key_ident(i),
        obj_value_fn(empty_anon_fn(vec![])),
        PropKind::Get,
        false,
        false,
        false,
    )
}

fn obj_prop_str_getter_single(s: &'static str) -> OP {
    obj_prop(
        obj_key_string_single(s),
        obj_value_fn(empty_anon_fn(vec![])),
        PropKind::Get,
        false,
        false,
        false,
    )
}
fn obj_prop_str_getter_double(s: &'static str) -> OP {
    obj_prop(
        obj_key_string_double(s),
        obj_value_fn(empty_anon_fn(vec![])),
        PropKind::Get,
        false,
        false,
        false,
    )
}
fn obj_prop_number_getter(n: &'static str) -> OP {
    obj_prop(
        obj_key_number(n),
        obj_value_fn(empty_anon_fn(vec![])),
        PropKind::Get,
        false,
        false,
        false,
    )
}

fn obj_prop_ident_setter(i: &'static str, args: Vec<FA>) -> OP {
    obj_prop(
        obj_key_ident(i),
        obj_value_fn(empty_anon_fn(args)),
        PropKind::Set,
        false,
        false,
        false,
    )
}
fn obj_prop_str_setter_single(i: &'static str, args: Vec<FA>) -> OP {
    obj_prop(
        obj_key_string_single(i),
        obj_value_fn(empty_anon_fn(args)),
        PropKind::Set,
        false,
        false,
        false,
    )
}
fn obj_prop_str_setter_double(i: &'static str, args: Vec<FA>) -> OP {
    obj_prop(
        obj_key_string_double(i),
        obj_value_fn(empty_anon_fn(args)),
        PropKind::Set,
        false,
        false,
        false,
    )
}
fn obj_prop_number_setter(i: &'static str, args: Vec<FA>) -> OP {
    obj_prop(
        obj_key_number(i),
        obj_value_fn(empty_anon_fn(args)),
        PropKind::Set,
        false,
        false,
        false,
    )
}
fn obj_prop(
    key: PK,
    value: PV,
    kind: PropKind,
    method: bool,
    computed: bool,
    short_hand: bool,
) -> OP {
    ObjProp::Prop(Prop {
        key,
        value,
        kind,
        computed,
        method,
        short_hand,
        is_static: false,
    })
}
fn obj_value_number(n: &'static str) -> PV {
    PropValue::Expr(number_lit_expr(n))
}
fn obj_value_fn(f: Func<'static>) -> PV {
    PropValue::Expr(Expr::Func(f))
}
fn obj_key_ident(i: &'static str) -> PK {
    obj_prop_key_expr(Expr::ident_from(i))
}
fn obj_prop_key_expr(expr: E) -> PK {
    PropKey::Expr(expr)
}
fn obj_key_number(n: &'static str) -> PK {
    obj_prop_key_lit(number_lit(n))
}
fn obj_key_string_single(s: &'static str) -> PK {
    obj_prop_key_lit(string_lit_single(s))
}
fn obj_key_string_double(s: &'static str) -> PK {
    obj_prop_key_lit(string_lit_double(s))
}

fn obj_prop_key_lit(lit: L) -> PK {
    PropKey::Lit(lit)
}

fn member_number_ident_part(n: &'static str, i: &'static str) -> Part {
    ProgramPart::Stmt(member_number_ident_stmt(n, i))
}
fn member_number_number_part(n: &'static str, i: &'static str) -> Part {
    ProgramPart::Stmt(member_number_number_stmt(n, i))
}
fn member_number_ident_stmt(n: &'static str, i: &'static str) -> S {
    Stmt::Expr(member_number_ident_expr(n, i))
}
fn member_number_number_stmt(n: &'static str, i: &'static str) -> S {
    Stmt::Expr(member_number_number_expr(n, i))
}
fn member_number_ident_expr(n: &'static str, i: &'static str) -> E {
    Expr::Member(member_number_ident(n, i))
}
fn member_number_number_expr(n: &'static str, i: &'static str) -> E {
    Expr::Member(member_number_number(n, i))
}
fn member_ident_number_expr(i: &'static str, n: &'static str) -> E {
    Expr::Member(member_ident_number(i, n))
}
fn member_ident_ident_expr(i: &'static str, i2: &'static str) -> E {
    Expr::Member(member_ident_ident(i, i2))
}

fn member_number_ident(n: &'static str, i: &'static str) -> MemberExpr<'static> {
    member(number_lit_expr(n), Expr::ident_from(i), false)
}
fn member_number_number(n: &'static str, i: &'static str) -> MemberExpr<'static> {
    member(number_lit_expr(n), number_lit_expr(i), true)
}
fn member_ident_number(i: &'static str, n: &'static str) -> MemberExpr<'static> {
    member(Expr::ident_from(i), number_lit_expr(n), true)
}
fn member_ident_ident(i: &'static str, i2: &'static str) -> MemberExpr<'static> {
    member(Expr::ident_from(i), Expr::ident_from(i2), false)
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

fn assign_part(left: AssignLeft<'static>, right: E) -> Part {
    ProgramPart::Stmt(assign_stmt(left, right))
}

fn assign_stmt(left: AssignLeft<'static>, right: E) -> S {
    Stmt::Expr(assign_expr(left, right))
}
fn assign_expr(left: AssignLeft<'static>, right: E) -> E {
    Expr::Assign(assign(left, right))
}

fn assign(left: AssignLeft<'static>, right: E) -> AssignExpr<'static> {
    AssignExpr {
        left: left,
        operator: AssignOp::Equal,
        right: Box::new(right),
    }
}

fn assign_left_ident(i: &'static str) -> AssignLeft<'static> {
    assign_left_expr(Expr::ident_from(i))
}

fn assign_left_expr(e: E) -> AssignLeft<'static> {
    AssignLeft::Expr(Box::new(e))
}

fn return_ident_part(ident: &'static str) -> Part {
    ProgramPart::Stmt(return_ident_stmt(ident))
}

fn return_ident_stmt(ident: &'static str) -> S {
    Stmt::Return(Some(Expr::ident_from(ident)))
}

fn new_ident_part(i: &'static str) -> Part {
    ProgramPart::Stmt(new_stmt(Expr::ident_from(i), vec![]))
}

fn new_part(c: E, args: Vec<E>) -> Part {
    ProgramPart::Stmt(new_stmt(c, args))
}

fn new_stmt(c: E, args: Vec<E>) -> S {
    Stmt::Expr(new_expr(c, args))
}
fn new_expr(c: E, args: Vec<E>) -> E {
    Expr::New(new(c, args))
}

fn new(c: E, args: Vec<E>) -> NewExpr<'static> {
    NewExpr {
        callee: Box::new(c),
        arguments: args,
    }
}

fn new_ident_expr(i: &'static str, args: Vec<E>) -> E {
    Expr::New(new_ident(i, args))
}

fn new_ident(i: &'static str, args: Vec<E>) -> NewExpr<'static> {
    NewExpr {
        callee: Box::new(Expr::ident_from(i)),
        arguments: args,
    }
}

fn call_ident_part(i: &'static str, args: Vec<E>) -> Part {
    call_part(Expr::ident_from(i), args)
}

fn call_part(callee: E, args: Vec<E>) -> Part {
    ProgramPart::Stmt(Stmt::Expr(Expr::Call(call(callee, args))))
}

fn call_expr(callee: E, args: Vec<E>) -> E {
    Expr::Call(call(callee, args))
}

fn call_ident_expr(i: &'static str, args: Vec<E>) -> E {
    Expr::Call(call_ident(i, args))
}

fn call_ident(i: &'static str, args: Vec<E>) -> CallExpr<'static> {
    call(Expr::ident_from(i), args)
}

fn call(callee: E, args: Vec<E>) -> CallExpr<'static> {
    CallExpr {
        callee: Box::new(callee),
        arguments: args,
    }
}

fn update_part(e: E, op: UpdateOp, prefix: bool) -> Part {
    ProgramPart::Stmt(update_stmt(e, op, prefix))
}
fn update_stmt(e: E, op: UpdateOp, prefix: bool) -> S {
    Stmt::Expr(update_expr(e, op, prefix))
}

fn update_expr(e: E, op: UpdateOp, prefix: bool) -> E {
    Expr::Update(UpdateExpr {
        argument: Box::new(e),
        operator: op,
        prefix,
    })
}

fn unary_part(op: UnaryOp, e: E, prefix: bool) -> Part {
    ProgramPart::Stmt(unary_stmt(op, e, prefix))
}

fn unary_stmt(op: UnaryOp, e: E, prefix: bool) -> S {
    Stmt::Expr(unary_expr(op, e, prefix))
}

fn unary_expr(op: UnaryOp, e: E, prefix: bool) -> E {
    Expr::Unary(unary(op, e, prefix))
}

fn unary(op: UnaryOp, e: E, prefix: bool) -> UnaryExpr<'static> {
    UnaryExpr {
        operator: op,
        argument: Box::new(e),
        prefix,
    }
}

fn zero_bin_zero_part(op: BinaryOp) -> Part {
    ProgramPart::Stmt(Stmt::Expr(Expr::Binary(binary(
        number_lit_expr("0"),
        op,
        number_lit_expr("0"),
    ))))
}

fn ident_assign_zero_part(i: &'static str, op: AssignOp) -> Part {
    ProgramPart::Stmt(Stmt::Expr(ident_assign_zero_expr(i, op)))
}

fn ident_assign_zero_expr(i: &'static str, op: AssignOp) -> Expr {
    Expr::Assign(ident_assign_zero(i, op))
}

fn ident_assign_zero(i: &'static str, op: AssignOp) -> AssignExpr {
    AssignExpr {
        left: AssignLeft::Expr(Box::new(Expr::ident_from(i))),
        operator: op,
        right: Box::new(number_lit_expr("0")),
    }
}

fn zero_bin_expr_part(op: BinaryOp, e: E) -> Part {
    ProgramPart::Stmt(Stmt::Expr(zero_bin_expr_expr(op, e)))
}

fn zero_bin_expr_expr(op: BinaryOp, e: E) -> E {
    Expr::Binary(binary(number_lit_expr("0"), op, e))
}

fn binary(l: E, op: BinaryOp, r: E) -> BinaryExpr<'static> {
    BinaryExpr {
        operator: op,
        left: Box::new(l),
        right: Box::new(r),
    }
}

fn zero_log_zero(op: LogicalOp) -> Part {
    ProgramPart::Stmt(Stmt::Expr(zero_log_zero_expr(op)))
}
fn zero_log_zero_expr(op: LogicalOp) -> E {
    Expr::Logical(logical(number_lit_expr("0"), op, number_lit_expr("0")))
}

fn logical(l: E, op: LogicalOp, r: E) -> LogicalExpr<'static> {
    LogicalExpr {
        operator: op,
        left: Box::new(l),
        right: Box::new(r),
    }
}

fn conditional_part(t: E, c: E, a: E) -> Part {
    ProgramPart::Stmt(Stmt::Expr(conditional_expr(t, c, a)))
}

fn conditional_expr(t: E, c: E, a: E) -> E {
    Expr::Conditional(conditional(t, c, a))
}

fn conditional(t: E, c: E, a: E) -> ConditionalExpr<'static> {
    ConditionalExpr {
        test: Box::new(t),
        consequent: Box::new(c),
        alternate: Box::new(a),
    }
}

fn zero_sequence(ct: usize) -> Part {
    let mut seq = vec![];
    for _ in 0..ct {
        seq.push(number_lit_expr("0"))
    }
    sequence(seq)
}

fn sequence(seq: Vec<E>) -> Part {
    ProgramPart::Stmt(Stmt::Expr(Expr::Sequence(seq)))
}

fn block_part(body: Vec<Part>) -> Part {
    ProgramPart::Stmt(Stmt::Block(body))
}

fn variable_decl_part(kind: VarKind, decls: Vec<VarDecl<'static>>) -> Part {
    ProgramPart::Decl(Decl::Var(kind, decls))
}

fn variable_decl_none(ident: &'static str) -> VarDecl<'static> {
    VarDecl {
        id: Pat::ident_from(ident),
        init: None,
    }
}
fn variable_decl_zero(ident: &'static str) -> VarDecl<'static> {
    variable_decl(ident, number_lit_expr("0"))
}
fn variable_decl(ident: &'static str, e: E) -> VarDecl<'static> {
    VarDecl {
        id: Pat::ident_from(ident),
        init: Some(e),
    }
}

fn if_zero_empty() -> Part {
    ProgramPart::Stmt(Stmt::If(if_stmt(number_lit_expr("0"), Stmt::Empty, None)))
}

fn if_zero_empty_else() -> Part {
    ProgramPart::Stmt(Stmt::If(if_stmt(
        number_lit_expr("0"),
        Stmt::Empty,
        Some(Stmt::Empty),
    )))
}

fn if_stmt(t: E, c: S, a: Option<S>) -> IfStmt<'static> {
    IfStmt {
        test: t,
        consequent: Box::new(c),
        alternate: a.map(|a| Box::new(a)),
    }
}

fn do_while_zero() -> Part {
    ProgramPart::Stmt(Stmt::DoWhile(DoWhileStmt {
        test: number_lit_expr("0"),
        body: Box::new(Stmt::Empty),
    }))
}

fn while_zero() -> Part {
    ProgramPart::Stmt(Stmt::While(WhileStmt {
        test: number_lit_expr("0"),
        body: Box::new(Stmt::Empty),
    }))
}

fn for_exprs_part(init: Option<E>, test: Option<E>, update: Option<E>, body: S) -> Part {
    ProgramPart::Stmt(Stmt::For(for_(
        init.map(LoopInit::Expr),
        test,
        update,
        body,
    )))
}

fn for_var_part(
    kind: VarKind,
    init: Vec<VarDecl<'static>>,
    test: Option<E>,
    update: Option<E>,
    body: S,
) -> Part {
    ProgramPart::Stmt(Stmt::For(for_(
        Some(LoopInit::Variable(kind, init)),
        test,
        update,
        body,
    )))
}

fn for_(
    init: Option<LoopInit<'static>>,
    test: Option<E>,
    update: Option<E>,
    body: S,
) -> ForStmt<'static> {
    ForStmt {
        init,
        test,
        update,
        body: Box::new(body),
    }
}

fn for_in_loop_part(left: LoopLeft<'static>, right: E, body: S) -> Part {
    ProgramPart::Stmt(Stmt::ForIn(for_in_loop(left, right, body)))
}

fn for_in_loop(left: LoopLeft<'static>, right: E, body: S) -> ForInStmt<'static> {
    ForInStmt {
        left,
        right,
        body: Box::new(body),
    }
}

fn switch_zero_part(cases: Vec<SwitchCase<'static>>) -> Part {
    ProgramPart::Stmt(Stmt::Switch(switch(number_lit_expr("0"), cases)))
}

fn switch(test: E, body: Vec<SwitchCase<'static>>) -> SwitchStmt<'static> {
    SwitchStmt {
        discriminant: test,
        cases: body,
    }
}

fn case_zero(body: Vec<Part>) -> SwitchCase<'static> {
    switch_case(Some(number_lit_expr("0")), body)
}

fn default_case(body: Vec<Part>) -> SwitchCase<'static> {
    switch_case(None, body)
}

fn switch_case(test: Option<E>, body: Vec<Part>) -> SwitchCase<'static> {
    SwitchCase {
        test,
        consequent: body,
    }
}

fn try_part(
    block: Vec<Part>,
    handler: Option<CatchClause<'static>>,
    finalizer: Option<Vec<Part>>,
) -> Part {
    ProgramPart::Stmt(Stmt::Try(try_(block, handler, finalizer)))
}

fn try_(
    block: Vec<Part>,
    handler: Option<CatchClause<'static>>,
    finalizer: Option<Vec<Part>>,
) -> TryStmt<'static> {
    TryStmt {
        block,
        handler,
        finalizer,
    }
}

fn catch_(param: Option<P>, body: Vec<Part>) -> CatchClause<'static> {
    CatchClause { param, body }
}

fn throw_part(e: E) -> Part {
    ProgramPart::Stmt(throw_stmt(e))
}

fn throw_stmt(e: E) -> S {
    Stmt::Throw(e)
}

fn template_part(quasis: Vec<TemplateElement<'static>>, exprs: Vec<E>) -> Part {
    ProgramPart::Stmt(Stmt::Expr(template_expr(quasis, exprs)))
}

fn template_expr(quasis: Vec<TemplateElement<'static>>, exprs: Vec<E>) -> E {
    Expr::Lit(Lit::Template(template(quasis, exprs)))
}

fn template(quasis: Vec<TemplateElement<'static>>, exprs: Vec<E>) -> TemplateLit<'static> {
    TemplateLit {
        quasis,
        expressions: exprs,
    }
}

fn template_element(raw: &'static str, tail: bool) -> TemplateElement<'static> {
    let end = if tail { raw.len() - 1 } else { raw.len() - 2 };
    TemplateElement::from(tail, &raw[1..end], raw)
}

fn arrow_expr_body_part(args: Vec<FA>, body: Vec<Part>) -> Part {
    ProgramPart::Stmt(Stmt::Expr(Expr::ArrowFunc(arrow_func(
        None,
        args,
        ArrowFuncBody::FuncBody(FuncBody(body)),
        false,
        false,
        false,
    ))))
}

fn arrow_expr_part(args: Vec<FA>, body: E) -> Part {
    ProgramPart::Stmt(Stmt::Expr(Expr::ArrowFunc(arrow_expr(args, body))))
}

fn arrow_expr(args: Vec<FA>, body: E) -> ArrowFuncExpr<'static> {
    arrow_func(
        None,
        args,
        ArrowFuncBody::Expr(Box::new(body)),
        true,
        false,
        false,
    )
}

type FAB = ArrowFuncBody<'static>;
fn arrow_func(
    id: Option<&'static str>,
    params: Vec<FA>,
    body: FAB,
    expr: bool,
    gen: bool,
    a: bool,
) -> ArrowFuncExpr<'static> {
    ArrowFuncExpr {
        id: id.map(Ident::from),
        params,
        body,
        expression: expr,
        generator: gen,
        is_async: a,
    }
}
