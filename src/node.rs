use either::Either;

enum Node {
    Program(Program),
    Function(Function),
    Statement(Statement),
    SwitchCase(SwitchCase),
    CatchClause(CatchClause),
    VariableDeclaration(VariableDeclaration),
    Expression(Expression),
    Property(Property),
    Pattern(Pattern),
}

pub enum Program {
    Module(Vec<Either<ModuleDecl, Statement>>),
    Script(Vec<Either<Directive, Statement>>)
}

pub enum ModuleDecl {

}
pub enum Declaration {
    Variable(VariableDeclaration),
}
pub struct VariableDeclaration {
    pub declarations: Vec<VariableDeclarator>,
    pub kind: VariableKind,
}

pub struct VariableDeclarator {
    pub id: Pattern,
    pub init: Option<Expression>,
}

pub enum VariableKind {
    Var,
    Let,
    Const,
}
pub enum Statement {
    Expression(Expression),
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
    pub param: Pattern,
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
    pub init: Option<Either<VariableDeclaration, Expression>>,
    pub test: Option<Expression>,
    pub update: Option<Expression>,
    pub body: Box<Statement>,
}

pub struct ForInStatement {
    pub left: Either<VariableDeclaration,  Pattern>,
    pub right: Expression,
    pub body: Box<Statement>,
}
pub struct ForOfStatement {
    pub left: Either<VariableDeclaration,  Pattern>,
    pub right: Expression,
    pub body: Box<Statement>,
}
pub type Identifier = String;

pub struct Function {
    pub id: Option<String>,
    pub params: Vec<String>,
    pub body: FunctionBody,
    pub generator: bool,
}

pub type FunctionBody = Vec<Either<Directive, Statement>>;

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
}

pub struct RegEx {
    pattern: String,
    flags: String,
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

}

pub struct ArrayExpression {
    pub elements: Vec<Option<Expression>>,
}

pub struct ObjectExpression {
    pub properties: Vec<Property>,
}

pub struct Property {
    pub key: Either<Literal, Identifier>,
    pub value: Expression,
    pub kind: PropertyKind,
}

pub enum PropertyKind {
    Init,
    Get,
    Set,
}
pub enum Pattern {
    Identifier(Identifier),
}

pub struct UnaryExpression {
    pub operator: UnaryOperator,
    pub prefix: bool,
    pub argument: Box<Expression>,
}
pub enum UnaryOperator {
    Minus,
    Plus,
    Not,
    Tilde,
    TypeOf,
    Void,
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
}

pub struct AssignmentExpression {
    pub operator: AssignmentOperator,
    pub left: Either<Pattern, Box<Expression>>,
    pub right: Box<Expression>,
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

pub struct SequenceExpression {
    pub expressions: Vec<Expression>,
}