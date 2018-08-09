use ress;

pub struct Node {
    pub position: Position,
    pub item: Item,
}
#[derive(Debug)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl Position {
    pub fn start() -> Self {
        Self {
            line: 0,
            column: 0,
        }
    }
}

pub enum Item {
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

pub enum Program {
    Module(Vec<ModulePart>),
    Script(Vec<ScriptPart>),
}

pub enum ModulePart {
    Decl(ModuleDecl),
    Statement(Statement),
}

pub enum ScriptPart {
    Directive(Directive),
    Statement(Statement)
}

pub enum ModuleDecl {
    Import(ModuleImport),
    Export(ModuleExport),
}
pub struct ModuleImport {
    pub specifiers: Vec<ImportSpecifier>,
    pub source: Literal,
}

pub enum ImportSpecifier {
    Normal(Identifier, Identifier),
    Default(Identifier),
    Namespace(Identifier),
}

pub enum ModuleExport {
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

pub struct NamedExportDecl {
    pub decl: Option<Declaration>,
    pub specifiers: Vec<ExportSpecifier>,
    pub source: Option<Literal>,
}
pub enum DefaultExportDecl {
    Decl(Declaration),
    Expr(Expression),
}
pub struct ExportSpecifier {
    pub local: Identifier,
    pub exported: Option<Identifier>,
}

pub enum Declaration {
    Variable(VariableKind, Vec<VariableDecl>),
    Function(Function),
    Class(Identifier, Class),
}

pub struct VariableDecl {
    pub id: Pattern,
    pub init: Option<Expression>,
}

pub enum VariableKind {
    Var,
    Let,
    Const,
}

pub enum Statement {
    Expr(Expression),
    Block(Vec<Statement>),
    Empty,
    Debugger,
    With(WithStatement),
    Return(Option<Expression>),
    Labeled(LabeledStatement),
    Break(BreakStatement),
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
}

pub struct WithStatement {
    pub object: Expression,
    pub body: Box<Statement>,
}

pub struct LabeledStatement {
    pub label: Identifier,
    pub body: Box<Statement>,
}

pub struct BreakStatement {
    pub label: Option<Identifier>,
    pub body: Box<Statement>,
}

pub struct IfStatement {
    pub test: Expression,
    pub consequent: Box<Statement>,
    pub alternate: Option<Box<Statement>>,
}

pub struct SwitchStatement {
    pub discriminant: Expression,
    pub cases: Vec<SwitchCase>,
}

pub type BlockStatement = Vec<Statement>;
pub struct TryStatement {
    pub block: BlockStatement,
    pub handler: Option<CatchClause>,
    pub finalizer: Option<BlockStatement>,
}

pub struct CatchClause {
    pub param: Option<Pattern>,
    pub body: BlockStatement,
}

pub struct WhileStatement {
    pub test: Expression,
    pub body: Box<Statement>,
}

pub struct DoWhileStatement {
    pub test: Expression,
    pub body: Box<Statement>,
}

pub struct ForStatement {
    pub init: Option<LoopInit>,
    pub test: Option<Expression>,
    pub update: Option<Expression>,
    pub body: Box<Statement>,
}

pub enum LoopInit {
    Variable(VariableDecl),
    Expr(Expression),
}

pub struct ForInStatement {
    pub left: LoopLeft,
    pub right: Expression,
    pub body: Box<Statement>,
}

pub struct ForOfStatement {
    pub left: LoopLeft,
    pub right: Expression,
    pub body: Box<Statement>,
    pub await: bool,
}

pub enum LoopLeft {
    Variable(VariableDecl),
    Pattern(Pattern),
}
pub type Identifier = String;

pub struct Function {
    pub id: Option<String>,
    pub params: Vec<FunctionArg>,
    pub body: FunctionBody,
    pub generator: bool,
    pub is_async: bool,
}
pub enum FunctionArg {
    Expr(Expression),
    Pattern(Pattern),
}

pub type FunctionBody = Vec<ScriptPart>;

pub struct Directive {
    pub expression: Literal,
    pub directive: String,
}

pub enum Literal {
    Null,
    String(String),
    Number(f32),
    Boolean(bool),
    RegEx(RegEx),
    Template(TemplateLiteral),
}

pub struct RegEx {
    pub pattern: String,
    pub flags: String,
}

pub struct SwitchCase {
    pub test: Option<Expression>,
    pub consequent: Vec<Statement>,
}

pub enum Expression {
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
}

pub type ArrayExpression = Vec<Option<Expression>>;
pub type ObjectExpression = Vec<ObjectProperty>;
pub enum ObjectProperty {
    Property(Property),
    Spread(Box<Expression>)
}
pub struct Property {
    pub key: PropertyKey,
    pub value: PropertyValue,
    pub kind: PropertyKind,
    pub method: bool,
}

pub enum PropertyKey {
    Literal(Literal),
    Ident(Identifier),
    Pattern(Pattern),
}

pub enum PropertyValue {
    Expr(Expression),
    Pattern(Pattern),
}

impl Property {
    pub fn assignment(key: PropertyKey, value: PropertyValue) -> Property {
        Property {
            key,
            value,
            kind: PropertyKind::Init,
            method: false,
        }
    }
}

pub enum PropertyKind {
    Init,
    Get,
    Set,
}
pub enum Pattern {
    Identifier(Identifier),
    Object(ObjectPattern),
    Array(Vec<Option<Pattern>>),
    RestElement(Box<Pattern>),
    Assignment(AssignmentPattern),
}

pub type ObjectPattern = Vec<ObjectPatternPart>;
pub enum ObjectPatternPart {
    Assignment(Property),
    Rest(Box<Pattern>),
}
pub struct AssignmentPattern {
    pub left: Box<Pattern>,
    pub right: Box<Expression>,
}

pub struct UnaryExpression {
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

#[derive(PartialEq)]
pub enum UnaryOperator {
    Minus,
    Plus,
    Not,
    Tilde,
    TypeOf,
    Void,
    Delete,
}

pub struct UpdateExpression {
    pub operator: UpdateOperator,
    pub argument: Box<Expression>,
    pub prefix: bool,
}

pub enum UpdateOperator {
    Increment,
    Decrement,
}

pub struct BinaryExpression {
    pub operator: BinaryOperator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

pub enum BinaryOperator {
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

pub struct AssignmentExpression {
    pub operator: AssignmentOperator,
    pub left: AssignmentLeft,
    pub right: Box<Expression>,
}

pub enum AssignmentLeft {
    Pattern(Pattern),
    Expr(Box<Expression>),
}

pub enum AssignmentOperator {
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
            ress::Punct::Equal => Some(AssignmentOperator::Equal),
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

pub struct LogicalExpression {
    pub operator: LogicalOperator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

pub enum LogicalOperator {
    Or,
    And,
}

pub struct MemberExpression {
    pub object: Box<Expression>,
    pub property: Box<Expression>,
    pub computed: bool,
}

pub struct ConditionalExpression {
    pub test: Box<Expression>,
    pub alternate: Box<Expression>,
    pub consequent: Box<Expression>,
}

pub struct CallExpression {
    pub callee: Box<Expression>,
    pub arguments: Vec<Expression>,
}

pub struct NewExpression {
    pub callee: Box<Expression>,
    pub arguments: Vec<Expression>,
}

pub type SequenceExpression = Vec<Expression>;

pub struct ArrowFunctionExpression {
    pub id: Option<String>,
    pub params: Vec<FunctionArg>,
    pub body: ArrowFunctionBody,
    pub expression: bool,
    pub generator: bool,
    pub is_async: bool,
}

pub enum ArrowFunctionBody {
    FunctionBody(FunctionBody),
    Expr(Box<Expression>)
}

pub struct YieldExpression {
    pub argument: Option<Box<Expression>>,
    pub delegate: bool,
}

pub struct TemplateLiteral {
    pub quasis: Vec<TemplateElement>,
    pub expressions: Vec<Expression>,
}

pub struct TaggedTemplateExpression {
    pub tag: Expression,
    pub quasi: TemplateLiteral,
}

pub struct TemplateElement {
    pub tail: bool,
    pub cooked: Option<String>,
    pub raw: String,
}

pub struct Class {
    pub id: Option<Identifier>,
    pub super_class: Option<Box<Expression>>,
    pub body: Vec<MethodDef>,
}
pub struct MethodDef {
    pub key: Expression,
    pub value: Expression,
    pub kind: MethodKind,
    pub computed: bool,
    pub is_static: bool,
}

pub enum MethodKind {
    Constructor,
    Method,
    Get,
    Set,
}

pub struct MetaProperty {
    pub meta: Identifier,
    pub property: Identifier,
}