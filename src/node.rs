use ress;
use error::Error;
#[derive(Debug)]
pub struct Node{
    pub position: Position,
    pub item: Item,
}
#[derive(Debug, Clone, Copy)]
pub struct Position{
    pub line: usize,
    pub column: usize,
}

impl ::std::fmt::Display for Position {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "line: {}, column: {}", &self.line, &self.column)
    }
}

impl Position {
    pub fn start() -> Self {
        Self {
            line: 0,
            column: 0,
        }
    }
}

#[derive(Debug)]
pub enum Item{
    Program(Program),
    Function(Function),
    Statement(Statement),
    SwitchCase(SwitchCase),
    CatchClause(CatchClause),
    VariableDecl(VariableDecl),
    ModuleDecl(ModuleDecl),
    Expr(Expression),
    Property(Property),
    Pattern(Pattern),
    Super,
    Class(Class),
}

#[derive(Debug)]
pub enum Program{
    Module(Vec<ModulePart>),
    Script(Vec<ScriptPart>),
}

#[derive(Debug)]
pub enum ModulePart{
    Decl(Declaration),
    Statement(Statement),
}

#[derive(Debug, Clone)]
pub enum ScriptPart{
    Directive(Directive),
    Statement(Statement),
    Decl(Declaration),
}

#[derive(Debug, Clone)]
pub enum ModuleDecl{
    Import(ModuleImport),
    Export(ModuleExport),
}
#[derive(Debug, Clone)]
pub struct ModuleImport{
    pub specifiers: Vec<ImportSpecifier>,
    pub source: Literal,
}

#[derive(Debug, Clone)]
pub enum ImportSpecifier{
    Normal(Identifier, Identifier),
    Default(Identifier),
    Namespace(Identifier),
}

#[derive(Debug, Clone)]
pub enum ModuleExport{
    /// ```js
    /// export default function() {};
    /// //or
    /// export default 1;
    /// ```
    Default(DefaultExportDecl),
    ///```js
    /// export {foo} from 'mod';
    /// //or
    /// export {foo as bar} from 'mod';
    /// //or
    /// export var foo = 1;
    /// //or
    /// export function bar() {
    /// }
    /// ```
    Named(NamedExportDecl),
    /// ```js
    /// export * from 'mod';
    /// ```
    All(Literal),
}

#[derive(Debug, Clone)]
pub struct NamedExportDecl{
    pub decl: Option<Declaration>,
    pub specifiers: Vec<ExportSpecifier>,
    pub source: Option<Literal>,
}
#[derive(Debug, Clone)]
pub enum DefaultExportDecl{
    Decl(Declaration),
    Expr(Expression),
}
#[derive(Debug, Clone)]
pub struct ExportSpecifier{
    pub local: Identifier,
    pub exported: Option<Identifier>,
}

#[derive(Debug, Clone)]
pub enum Declaration{
    Variable(VariableKind, Vec<VariableDecl>),
    Function(Function),
    Class(Class),
    Import(Box<ModuleImport>),
    Export(Box<ModuleExport>),
}

#[derive(Debug, Clone)]
pub struct VariableDecl{
    pub id: Pattern,
    pub init: Option<Expression>,
}

impl VariableDecl {
    pub fn is_array(&self) -> bool {
        self.id.is_array()
    }

    pub fn is_obj(&self) -> bool {
        self.id.is_obj()
    }
}
#[derive(PartialEq, Clone, Debug)]
pub enum VariableKind{
    Var,
    Let,
    Const,
}

impl ToString for VariableKind {
    fn to_string(&self) -> String {
        match self {
            VariableKind::Const => String::from("const"),
            VariableKind::Let => String::from("let"),
            VariableKind::Var => String::from("var"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Statement{
    Expr(Expression),
    Block(Vec<Statement>),
    Empty,
    Debugger,
    With(WithStatement),
    Return(Option<Expression>),
    Labeled(LabeledStatement),
    Break(Option<Identifier>),
    Continue(Option<Identifier>),
    If(IfStatement),
    Switch(SwitchStatement),
    Throw(Expression),
    Try(TryStatement),
    While(WhileStatement),
    DoWhile(DoWhileStatement),
    For(ForStatement),
    ForIn(ForInStatement),
    ForOf(ForOfStatement),
    Var(Vec<VariableDecl>),
}

#[derive(Debug, Clone)]
pub struct WithStatement{
    pub object: Expression,
    pub body: Box<Statement>,
}

#[derive(Debug, Clone)]
pub struct LabeledStatement{
    pub label: Identifier,
    pub body: Box<Statement>,
}

#[derive(Debug, Clone)]
pub struct IfStatement{
    pub test: Expression,
    pub consequent: Box<Statement>,
    pub alternate: Option<Box<Statement>>,
}

#[derive(Debug, Clone)]
pub struct SwitchStatement{
    pub discriminant: Expression,
    pub cases: Vec<SwitchCase>,
}

pub type BlockStatement = Vec<Statement>;
#[derive(Debug, Clone)]
pub struct TryStatement{
    pub block: BlockStatement,
    pub handler: Option<CatchClause>,
    pub finalizer: Option<BlockStatement>,
}

#[derive(Debug, Clone)]
pub struct CatchClause{
    pub param: Pattern,
    pub body: BlockStatement,
}

#[derive(Debug, Clone)]
pub struct WhileStatement{
    pub test: Expression,
    pub body: Box<Statement>,
}

#[derive(Debug, Clone)]
pub struct DoWhileStatement{
    pub test: Expression,
    pub body: Box<Statement>,
}

#[derive(Debug, Clone)]
pub struct ForStatement{
    pub init: Option<LoopInit>,
    pub test: Option<Expression>,
    pub update: Option<Expression>,
    pub body: Box<Statement>,
}

#[derive(Debug, Clone)]
pub enum LoopInit{
    Variable(Vec<VariableDecl>),
    Expr(Expression),
}

#[derive(Debug, Clone)]
pub struct ForInStatement{
    pub left: LoopLeft,
    pub right: Expression,
    pub body: Box<Statement>,
}

#[derive(Debug, Clone)]
pub struct ForOfStatement{
    pub left: LoopLeft,
    pub right: Expression,
    pub body: Box<Statement>,
    pub await: bool,
}

#[derive(Debug, Clone)]
pub enum LoopLeft{
    Variable(VariableDecl),
    Pattern(Pattern),
}
pub type Identifier = String;

#[derive(Debug, Clone)]
pub struct Function{
    pub id: Option<String>,
    pub params: Vec<FunctionArg>,
    pub body: FunctionBody,
    pub generator: bool,
    pub is_async: bool,
}
#[derive(Debug, Clone)]
pub enum FunctionArg{
    Expr(Expression),
    Pattern(Pattern),
}

impl FunctionArg {
    pub fn is_simple(&self) -> bool {
        match self {
            FunctionArg::Pattern(ref p) => match p {
                Pattern::Identifier(_) => true,
                _ => false,
            },
            FunctionArg::Expr(ref e) => match e {
                Expression::Ident(_) => true,
                _ => false
            }
        }
    }

    pub fn is_assignment(&self) -> bool {
        match self {
            FunctionArg::Pattern(ref p) => match p {
                Pattern::Assignment(_) => true,
                _ => false,
            },
            FunctionArg::Expr(ref e) => match e {
                Expression::Assignment(_) => true,
                _ => false,
            }
        }
    }

    pub fn is_await(&self) -> bool {
        match self {
            FunctionArg::Expr(ref e) => match e {
                Expression::Ident(ref i) => i == "await",
                _ => false,
            },
            FunctionArg::Pattern(ref p) => match p {
                Pattern::Identifier(ref i) => i == "await",
                _ => false,
            }
        }
    }
}

pub type FunctionBody = Vec<ScriptPart>;

#[derive(Debug, Clone)]
pub struct Directive{
    pub expression: Literal,
    pub directive: String,
}

#[derive(Debug, Clone)]
pub enum Literal{
    Null,
    String(String),
    Number(String),
    Boolean(bool),
    RegEx(RegEx),
    Template(TemplateLiteral),
}

impl Literal {
    pub fn from_token(token: &ress::Token) -> Option<Self> {
        match token {
            ress::Token::Null => Some(Literal::Null),
            ress::Token::String(ref string_lit) => Some(Literal::String(token.to_string())),
            ress::Token::Numeric(ref num) => Some(Literal::Number(token.to_string())),
            ress::Token::Boolean(ref b) => Some(Literal::Boolean(b.into())),
            ress::Token::RegEx(ref r) => Some(Literal::RegEx(r.into())),
            _ => None
        }
    }
}

#[derive(Debug, Clone)]
pub struct RegEx{
    pub pattern: String,
    pub flags: String,
}

impl<'a> From<&'a ress::RegEx> for RegEx {
    fn from(other: &'a ress::RegEx) -> Self {
        Self::from_parts(&other.body, &other.flags)
    }
}

impl RegEx {
    pub fn from_parts(body: &str, flags: &Option<String>) -> Self
    {
        let f = if let Some(ref f) = flags {
            f.clone()
        } else {
            String::new()
        };
        Self {
            pattern: body.to_string(),
            flags: f,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SwitchCase{
    pub test: Option<Expression>,
    pub consequent: Vec<ScriptPart>,
}

#[derive(Debug, Clone)]
pub enum Expression{
    ThisExpression,
    SuperExpression,
    Array(ArrayExpression),
    Object(ObjectExpression),
    Function(Function),
    Unary(UnaryExpression),
    Update(UpdateExpression),
    Binary(BinaryExpression),
    Assignment(AssignmentExpression),
    Logical(LogicalExpression),
    Member(MemberExpression),
    Conditional(ConditionalExpression),
    Call(CallExpression),
    New(NewExpression),
    Sequence(SequenceExpression),
    Spread(Box<Expression>),
    ArrowFunction(ArrowFunctionExpression),
    Yield(YieldExpression),
    Class(Class),
    MetaProperty(MetaProperty),
    Await(Box<Expression>),
    Ident(Identifier),
    ArrowParamPlaceHolder(Vec<FunctionArg>, bool),
    Literal(Literal),
    TaggedTemplate(TaggedTemplateExpression),
}
impl Expression {
    pub fn is_ident(&self) -> bool {
        match self {
            &Expression::Ident(_) => true,
            _ => false,
        }
    }

    pub fn is_unary(&self) -> bool {
        match self {
            &Expression::Unary(_) => true,
            _ => false,
        }
    }

    pub fn is_arrow_param_placeholder(&self) -> bool {
        match self {
            &Expression::ArrowParamPlaceHolder(_, _) => true,
            _ => false,
        }
    }

    pub fn is_async(&self) -> bool {
        match self {
            &Expression::Function(ref f) => f.is_async,
            &Expression::ArrowFunction(ref f) => f.is_async,
            &Expression::ArrowParamPlaceHolder(_, b) => b,
            _ => false
        }
    }

    pub fn as_ident(self) -> Option<Identifier> {
        match self {
            Expression::Ident(i) => Some(i),
            _ => None,
        }
    }
}

pub type ArrayExpression = Vec<Option<Expression>>;
pub type ObjectExpression = Vec<ObjectProperty>;
#[derive(Debug, Clone)]
pub enum ObjectProperty{
    Property(Property),
    Spread(Box<Expression>)
}
#[derive(Debug, Clone)]
pub struct Property{
    pub key: PropertyKey,
    pub value: PropertyValue,
    pub kind: PropertyKind,
    pub method: bool,
    pub computed: bool,
    pub short_hand: bool,
}

#[derive(Debug, Clone)]
pub enum PropertyKey{
    Literal(Literal),
    Ident(Identifier),
    Pattern(Pattern),
}

impl PropertyKey {
    pub fn matches(&self, other: &str) -> bool {
        match self {
            PropertyKey::Literal(ref l) => match l {
                Literal::String(ref s) => s == other,
                _ => false,
            },
            PropertyKey::Ident(ref i) => i == other,
            PropertyKey::Pattern(ref p) => match p {
                Pattern::Identifier(ref i) => i == other,
                _ => false,
            }
        }
    }
}

impl PropertyKey {
    pub fn is_static(&self) -> bool {
        match self {
            PropertyKey::Literal(ref l) => match l {
                Literal::String(ref s) => s == "static",
                _ => false,
            },
            PropertyKey::Ident(ref s) => s == "static",
            PropertyKey::Pattern(ref p) => match p {
                Pattern::Identifier(ref s) => s == "static",
                _ => false
            }
        }
    }

    pub fn matches_value(&self, value: &str) -> bool {
        match self {
            PropertyKey::Ident(ref s) => s == value,
            PropertyKey::Literal(ref s) => match s {
                Literal::String(ref s) => s == value,
                _ => false,
            },
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub enum PropertyValue{
    Expr(Expression),
    Pattern(Pattern),
    None,
}

impl PropertyValue {
    pub fn is_generator(&self) -> bool {
        match &self {
            PropertyValue::Expr(ref e) => match e {
                Expression::Function(ref f) => f.generator,
                Expression::ArrowFunction(ref f) => f.generator,
                _ => false,
            },
            _ => false,
        }
    }
}

impl Property {
    pub fn assignment(key: PropertyKey, value: PropertyValue) -> Property {
        Property {
            key,
            value,
            kind: PropertyKind::Init,
            method: false,
            computed: false,
            short_hand: false,
        }
    }

}
#[derive(PartialEq, Clone)]
#[derive(Debug)]
pub enum PropertyKind{
    Init,
    Get,
    Set,
    Ctor,
    Method,
}
#[derive(Debug, Clone)]
pub enum Pattern{
    Identifier(Identifier),
    Object(ObjectPattern),
    Array(Vec<Option<Pattern>>),
    RestElement(Box<Pattern>),
    Assignment(AssignmentPattern),
}

impl Pattern {
    pub fn is_array(&self) -> bool {
        match self {
            Pattern::Array(_) => true,
            _ => false,
        }
    }

    pub fn is_obj(&self) -> bool {
        match self {
            Pattern::Object(_) => true,
            _ => false,
        }
    }

    pub fn is_yield(&self) -> bool {
        match self {
            Pattern::Identifier(ref ident) => ident == "yield",
            _ => false,
        }
    }

    pub fn is_ident(&self) -> bool {
        match self {
            Pattern::Identifier(_) => true,
            _ => false,
        }
    }

    pub fn is_restricted(&self) -> bool {
        match self {
            Pattern::Identifier(ref ident) => ident == "eval" || ident == "arguments",
            _ => false,
        }
    }
}

pub type ObjectPattern = Vec<ObjectPatternPart>;
#[derive(Debug, Clone)]
pub enum ObjectPatternPart{
    Assignment(Property),
    Rest(Box<Pattern>),
}

#[derive(Debug, Clone)]
pub struct AssignmentPattern{
    pub left: Box<Pattern>,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct UnaryExpression{
    pub operator: UnaryOperator,
    pub prefix: bool,
    pub argument: Box<Expression>,
}

impl UnaryExpression {
    pub fn has_operator(&self, op: &UnaryOperator) -> bool {
        &self.operator == op
    }

    pub fn has_ident_arg(&self) -> bool {
        self.argument.is_ident()
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum UnaryOperator{
    Minus,
    Plus,
    Not,
    Tilde,
    TypeOf,
    Void,
    Delete,
}

impl UnaryOperator {
    pub fn from_token(token: &ress::Token) -> Option<Self> {
        match token {
            ress::Token::Punct(ref p) =>  match p {
                ress::Punct::Minus => Some(UnaryOperator::Minus),
                ress::Punct::Plus => Some(UnaryOperator::Plus),
                ress::Punct::Not => Some(UnaryOperator::Not),
                ress::Punct::BitwiseNot => Some(UnaryOperator::Tilde),
                _ => None,
            }
            ress::Token::Keyword(ref k) => match k {
                ress::Keyword::TypeOf => Some(UnaryOperator::TypeOf),
                ress::Keyword::Void => Some(UnaryOperator::Void),
                ress::Keyword::Delete => Some(UnaryOperator::Delete),
                _ => None,
            },
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct UpdateExpression{
    pub operator: UpdateOperator,
    pub argument: Box<Expression>,
    pub prefix: bool,
}

#[derive(Debug, Clone)]
pub enum UpdateOperator{
    Increment,
    Decrement,
}

#[derive(Debug, Clone)]
pub struct BinaryExpression{
    pub operator: BinaryOperator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone)]
pub enum BinaryOperator{
    Equal,
    NotEqual,
    StrictEqual,
    StrictNotEqual,
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,
    LeftShift,
    RightShift,
    UnsignedRightShift,
    Plus,
    Minus,
    Times,
    Over,
    Mod,
    Or,
    XOr,
    And,
    In,
    InstanceOf,
    PowerOf,
}

impl BinaryOperator {
    pub fn from_token(token: &ress::Token) -> Option<Self> {
        match token {
            ress::Token::Keyword(ref key) => match key {
                ress::Keyword::InstanceOf => Some(BinaryOperator::InstanceOf),
                ress::Keyword::In => Some(BinaryOperator::In),
                _ => None,
            },
            ress::Token::Punct(ref p) => match p {
                ress::Punct::Equal => Some(BinaryOperator::Equal),
                ress::Punct::NotEqual => Some(BinaryOperator::NotEqual),
                ress::Punct::StrictEquals => Some(BinaryOperator::StrictEqual),
                ress::Punct::StrictNotEquals => Some(BinaryOperator::StrictNotEqual),
                ress::Punct::LessThan => Some(BinaryOperator::LessThan),
                ress::Punct::LessThanEqual => Some(BinaryOperator::LessThanEqual),
                ress::Punct::GreaterThan => Some(BinaryOperator::GreaterThan),
                ress::Punct::GreaterThanEqual => Some(BinaryOperator::GreaterThanEqual),
                ress::Punct::LeftShift => Some(BinaryOperator::LeftShift),
                ress::Punct::RightShift => Some(BinaryOperator::RightShift),
                ress::Punct::UnsignedRightShift => Some(BinaryOperator::UnsignedRightShift),
                ress::Punct::Plus => Some(BinaryOperator::Plus),
                ress::Punct::Minus => Some(BinaryOperator::Minus),
                ress::Punct::Asterisk => Some(BinaryOperator::Times),
                ress::Punct::ForwardSlash => Some(BinaryOperator::Over),
                ress::Punct::Modulo => Some(BinaryOperator::Mod),
                ress::Punct::And => Some(BinaryOperator::And),
                ress::Punct::Pipe => Some(BinaryOperator::Or),
                ress::Punct::Caret => Some(BinaryOperator::XOr),
                _ => None,
            },
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct AssignmentExpression{
    pub operator: AssignmentOperator,
    pub left: AssignmentLeft,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone)]
pub enum AssignmentLeft{
    Pattern(Pattern),
    Expr(Box<Expression>),
}

#[derive(Debug, Clone)]
pub enum AssignmentOperator{
    Equal,
    PlusEqual,
    MinusEqual,
    TimesEqual,
    DivEqual,
    ModEqual,
    LeftShiftEqual,
    RightShiftEqual,
    UnsignedRightShiftEqual,
    OrEqual,
    XOrEqual,
    AndEqual,
    PowerOfEqual,
}

impl AssignmentOperator {
    pub fn from_punct(p: &ress::Punct) -> Option<Self> {
        match p {
            ress::Punct::Assign => Some(AssignmentOperator::Equal),
            ress::Punct::AddAssign => Some(AssignmentOperator::PlusEqual),
            ress::Punct::SubtractAssign => Some(AssignmentOperator::MinusEqual),
            ress::Punct::MultiplyAssign => Some(AssignmentOperator::TimesEqual),
            ress::Punct::DivideAssign => Some(AssignmentOperator::DivEqual),
            ress::Punct::ModuloAssign => Some(AssignmentOperator::ModEqual),
            ress::Punct::LeftShiftAssign => Some(AssignmentOperator::LeftShiftEqual),
            ress::Punct::RightShiftAssign => Some(AssignmentOperator::RightShiftEqual),
            ress::Punct::UnsignedRightShiftAssign => Some(AssignmentOperator::UnsignedRightShiftEqual),
            ress::Punct::BitwiseOrAssign => Some(AssignmentOperator::OrEqual),
            ress::Punct::BitwiseXOrAssign => Some(AssignmentOperator::XOrEqual),
            ress::Punct::BitwiseAndAssign => Some(AssignmentOperator::AndEqual),
            ress::Punct::ExponentAssign => Some(AssignmentOperator::PowerOfEqual),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct LogicalExpression{
    pub operator: LogicalOperator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone)]
pub enum LogicalOperator{
    Or,
    And,
}

impl LogicalOperator {
    pub fn from_token(token: &ress::Token) -> Option<Self> {
        match token {
            ress::Token::Punct(ref p) => match p {
                ress::Punct::LogicalAnd => Some(LogicalOperator::And),
                ress::Punct::LogicalOr => Some(LogicalOperator::Or),
                _ => None,
            },
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct MemberExpression{
    pub object: Box<Expression>,
    pub property: Box<Expression>,
    pub computed: bool,
}

#[derive(Debug, Clone)]
pub struct ConditionalExpression{
    pub test: Box<Expression>,
    pub alternate: Box<Expression>,
    pub consequent: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct CallExpression{
    pub callee: Box<Expression>,
    pub arguments: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct NewExpression{
    pub callee: Box<Expression>,
    pub arguments: Vec<Expression>,
}

pub type SequenceExpression = Vec<Expression>;

#[derive(Debug, Clone)]
pub struct ArrowFunctionExpression{
    pub id: Option<String>,
    pub params: Vec<FunctionArg>,
    pub body: ArrowFunctionBody,
    pub expression: bool,
    pub generator: bool,
    pub is_async: bool,
}

#[derive(Debug, Clone)]
pub enum ArrowFunctionBody{
    FunctionBody(FunctionBody),
    Expr(Box<Expression>)
}

#[derive(Debug, Clone)]
pub struct YieldExpression{
    pub argument: Option<Box<Expression>>,
    pub delegate: bool,
}

#[derive(Debug, Clone)]
pub struct TemplateLiteral{
    pub quasis: Vec<TemplateElement>,
    pub expressions: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct TaggedTemplateExpression{
    pub tag: Box<Expression>,
    pub quasi: TemplateLiteral,
}

#[derive(Debug, Clone)]
pub struct TemplateElement{
    pub tail: bool,
    pub cooked: String,
    pub raw: String,
}

#[derive(Debug, Clone)]
pub struct Class{
    pub id: Option<Identifier>,
    pub super_class: Option<Box<Expression>>,
    pub body: Vec<Property>,
}
#[derive(Debug, Clone)]
pub struct MethodDef{
    pub key: Expression,
    pub value: Expression,
    pub kind: MethodKind,
    pub computed: bool,
    pub is_static: bool,
}

#[derive(Debug, Clone)]
pub enum MethodKind{
    Constructor,
    Method,
    Get,
    Set,
}

#[derive(Debug, Clone)]
pub struct MetaProperty{
    pub meta: Identifier,
    pub property: Identifier,
}