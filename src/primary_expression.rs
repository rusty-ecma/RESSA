pub enum Identifier {
    Ref(Ident),
    Bind(Ident),
    Label(Ident),
}

pub enum PrimaryExpression {
    This,
    IdentifierReference(IdentifierReference),
    Literal(Literal),
    ArrayLiteral(ArrayLiteral),
    ObjectLiteral(ObjectLiteral)
    FunctionExpression(FunctionExpression),
    ClassExpression(ClassExpression),
    GeneratorExpression(GeneratorExpression),
    AsyncFunctionExpression(AsyncFunctionExpression),
    RegularExpressionLiteral(RegularExpressionLiteral),
    TemplateLiteral(TemplateLiteral),
}

pub struct Ident {
    pub is_yielded: bool,
    pub is_awaited: bool,
    pub name: String,
}

pub struct Literal;
pub struct ArrayLiteral;
pub struct ObjectLiteral;
pub struct FunctionExpression;
pub struct ClassExpression;
pub struct GeneratorExpression;
pub struct AsyncFunctionExpression;
pub struct RegularExpressionLiteral;
pub struct TemplateLiteral;
pub CoverParenthesizedExpressionAndArrowParameterList;