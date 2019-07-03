#![cfg(test)]
use resast::ref_tree::prelude::*;
use resast::expr::{
    PropertyKind,
    AssignmentOperator,
    UpdateOperator,
    UnaryOperator,
    BinaryOperator,
    LogicalOperator,
};
use resast::decl::{
    VariableKind,
};

#[test]
fn long_args() {
    let js = "function a(a, b = 0, [c,, d = 0, ...e]){}";
    let expectation = vec![
        ProgramPart::Decl(
            Decl::Function(
                Function {
                    id: Some("a"),
                    params: vec![
                        FunctionArg::Pat(
                            Pat::Identifier("a")
                        ),
                        FunctionArg::Pat(
                            Pat::Assignment(
                                AssignmentPat {
                                    left: Box::new(
                                        Pat::Identifier("b")
                                    ),
                                    right: Box::new(
                                        Expr::Literal(
                                            Literal::Number("0")
                                        )
                                    )
                                }
                            )
                        ),
                        FunctionArg::Pat(
                            Pat::Array(vec![
                                Some(ArrayPatPart::Pat(
                                    Pat::Identifier("c")
                                )),
                                None,
                                Some(ArrayPatPart::Pat(
                                    Pat::Assignment(
                                        AssignmentPat {
                                            left: Box::new(Pat::Identifier("d")),
                                            right: Box::new(Expr::Literal(
                                                Literal::Number("0")
                                            ))
                                        }
                                    )
                                )),
                                Some(ArrayPatPart::Pat(
                                    Pat::RestElement(
                                        Box::new(
                                            Pat::Identifier("e")
                                        )
                                    )
                                ))
                            ]
                            )
                        )
                    ],
                    body: vec![],
                    is_async: false,
                    generator: false,
                }
            )
        )
        
    ];
    for (i, (item, token)) in ressa::Parser::new(js).unwrap().zip(expectation.iter()).enumerate() {
        let item = item.unwrap();
        assert_eq!((i, &item), (i, token))
    }
}