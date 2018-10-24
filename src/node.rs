use ress;

#[derive(PartialEq,Debug)]
pub struct Node{
    pub position: Position,
    pub item: Item,
}
#[derive(PartialEq,Debug, Clone, Copy)]
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

#[derive(PartialEq,Debug)]
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

#[derive(PartialEq, Debug)]
pub enum Program{
    Module(Vec<ProgramPart>),
    Script(Vec<ProgramPart>),
}

impl Program {
    pub fn module(parts: Vec<ProgramPart>) -> Self {
        Program::Module(parts)
    }

    pub fn script(parts: Vec<ProgramPart>) -> Self {
        Program::Script(parts)
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum ProgramPart {
    Directive(Directive),
    Decl(Declaration),
    Statement(Statement),
}

impl ProgramPart {
    pub fn directive(dir: &str) -> Self {
        ProgramPart::Directive(Directive::new(dir))
    }

    pub fn use_strict(double_quotes: bool) -> Self {
        let dir = if double_quotes {
            r#""use strict""#
        } else {
            "'use strict'"
        };
        ProgramPart::directive(dir)
    }

    pub fn decl(decl: Declaration) -> Self {
        ProgramPart::Decl(decl)
    }

    pub fn statement(stmt: Statement) -> Self {
        ProgramPart::Statement(stmt)
    }
    pub fn is_import(&self) -> bool {
        match self {
            ProgramPart::Decl(ref decl) => match decl {
                Declaration::Import(_) => true,
                _ => false,
            },
            _ => false
        }
    }

    pub fn is_export(&self) -> bool {
        match self {
            ProgramPart::Decl(ref decl) => match decl {
                Declaration::Export(_) => true,
                _ => false,
            },
            _ => false,
        }
    }
}

#[derive(PartialEq,Debug, Clone)]
pub enum ModuleDecl{
    Import(ModuleImport),
    Export(ModuleExport),
}

impl ModuleDecl {
    pub fn import(import: ModuleImport) -> Self {
        ModuleDecl::Import(import)
    }

    pub fn import_parts(specifiers: Vec<ImportSpecifier>, source: &str) -> Self {
        ModuleDecl::Import(ModuleImport {
            specifiers,
            source: Literal::string(source),
        })
    }

    pub fn single_import(name: &str, from: &str) -> Self {
        Self::simple_imports(&[name], from)
    }

    pub fn single_default_import(name: &str, from: &str) -> Self {
        let specifiers = vec![ImportSpecifier::default(name)];
        let source = Literal::string(from);
        ModuleDecl::Import(ModuleImport {
            specifiers,
            source,
        })
    }

    pub fn imports_with_default(default: &str, normal: &[&str], from: &str) -> Self {
        let mut specifiers = Vec::with_capacity(normal.len() + 1);
        specifiers.push(ImportSpecifier::default(default));
        for name in normal {
            specifiers.push(ImportSpecifier::normal_no_local(name))
        }
        let source = Literal::string(from);
        ModuleDecl::Import(ModuleImport {
            specifiers,
            source
        })
    }

    pub fn simple_imports(names: &[&str], from: &str) -> Self {
        let specifiers = names.iter().map(|n| ImportSpecifier::normal_no_local(n)).collect();
        let source = Literal::string(from);
        ModuleDecl::Import(ModuleImport {
            specifiers,
            source
        })
    }

    pub fn namespace_import(local: &str, from: &str) -> Self {
        ModuleDecl::Import(ModuleImport {
            specifiers: vec![ImportSpecifier::namespace(local)],
            source: Literal::string(from)
        })
    }

    pub fn export(export: ModuleExport) -> Self {
        ModuleDecl::Export(export)
    }

    pub fn export_all(name: &str) -> Self {
        ModuleDecl::Export(ModuleExport::All(Literal::string(name)))
    }

    pub fn export_default_expr(expr: Expression) -> Self {
        ModuleDecl::Export(
            ModuleExport::Default(
                DefaultExportDecl::Expr(expr)
            )
        )
    }

    pub fn export_default_decl(decl: Declaration) -> Self {
        ModuleDecl::Export(
            ModuleExport::Default(
                DefaultExportDecl::Decl(decl)
            )
        )
    }

    pub fn export_named_decl(decl: Declaration) -> Self {
        ModuleDecl::Export(
            ModuleExport::Named(
                NamedExportDecl::Decl(decl)
            )
        )
    }

    pub fn export_single_named_spec(name: &str) -> Self {
        ModuleDecl::Export(
            ModuleExport::Named(
                NamedExportDecl::Specifier(
                    vec![ExportSpecifier::no_alias(name)],
                    None
                )
            )
        )
    }

    pub fn export_single_named_spec_with_alias(name: &str, alias: &str) -> Self {
        ModuleDecl::Export(
            ModuleExport::Named(
                NamedExportDecl::Specifier(
                    vec![ExportSpecifier::with_alias(name, alias)],
                    None
                )
            )
        )
    }
}

#[derive(PartialEq,Debug, Clone)]
pub struct ModuleImport{
    pub specifiers: Vec<ImportSpecifier>,
    pub source: Literal,
}

impl ModuleImport {
    pub fn new(specifiers: Vec<ImportSpecifier>, source: Literal) -> Self {
        Self {
            specifiers,
            source,
        }
    }
}

#[derive(PartialEq,Debug, Clone)]
pub enum ImportSpecifier{
    Normal(Identifier, Option<Identifier>),
    Default(Identifier),
    Namespace(Identifier),
}

impl ImportSpecifier {
    pub fn normal(ident: Identifier, local: Option<Identifier>) -> Self {
        ImportSpecifier::Normal(ident, local)
    }

    pub fn normal_no_local(ident: &str) -> Self {
        ImportSpecifier::Normal(ident.to_string(), None)
    }

    pub fn normal_local(ident: &str, local: &str) -> Self {
        ImportSpecifier::Normal(ident.to_string(), Some(local.to_string()))
    }

    pub fn default(ident: &str) -> Self {
        ImportSpecifier::Default(ident.to_string())
    }

    pub fn namespace(ident: &str) -> Self {
        ImportSpecifier::Namespace(ident.to_string())
    }
}

#[derive(PartialEq,Debug, Clone)]
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

#[derive(PartialEq,Debug, Clone)]
pub enum NamedExportDecl {
    Decl(Declaration),
    Specifier(Vec<ExportSpecifier>, Option<Literal>),
}

#[derive(PartialEq,Debug, Clone)]
pub enum DefaultExportDecl{
    Decl(Declaration),
    Expr(Expression),
}
#[derive(PartialEq,Debug, Clone)]
pub struct ExportSpecifier{
    pub local: Identifier,
    pub exported: Option<Identifier>,
}

impl ExportSpecifier {
    pub fn no_alias(local: &str) -> Self {
        Self {
            local: local.to_string(),
            exported: None
        }
    }

    pub fn with_alias(local: &str, alias: &str) -> Self {
        Self {
            local: local.to_string(),
            exported: Some(alias.to_string()),
        }
    }
}

#[derive(PartialEq,Debug, Clone)]
pub enum Declaration{
    Variable(VariableKind, Vec<VariableDecl>),
    Function(Function),
    Class(Class),
    Import(Box<ModuleImport>),
    Export(Box<ModuleExport>),
}

#[derive(PartialEq,Debug, Clone)]
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

    pub fn uninitialized(name: &str) -> Self {
        Self {
            id: Pattern::ident(name),
            init: None
        }
    }

    pub fn with_value(name: &str, value: Expression) -> Self {
        Self {
            id: Pattern::ident(name),
            init: Some(value),
        }
    }

    pub fn destructed(names: &[&str], value: ObjectExpression) -> Self {
        let id = Pattern::Object(names.iter().map(|name|
            ObjectPatternPart::Assignment(Property {
                key: PropertyKey::Ident(name.to_string()),
                value: PropertyValue::None,
                kind: PropertyKind::Init,
                method: false,
                short_hand: true,
                computed: false,
            })).collect()
        );
        Self {
            id,
            init: Some(Expression::Object(value)),
        }
    }

    pub fn destructed_with_rest(names: &[&str], rest: &str, value: ObjectExpression) -> Self {
        let mut props: Vec<ObjectPatternPart> = names.iter().map(|name|
            ObjectPatternPart::Assignment(
                Property {
                    key: PropertyKey::Ident(name.to_string()),
                    value: PropertyValue::None,
                    kind: PropertyKind::Init,
                    computed: false,
                    method: false,
                    short_hand: true,
                }
            )
        ).collect();
        props.push(
            ObjectPatternPart::Rest(
                Box::new(Pattern::RestElement(
                    Box::new(Pattern::ident(rest))
                ))
            )
        );
        let id = Pattern::Object(props);
        let init = Some(Expression::Object(value));
        Self {
            id,
            init,
        }
    }
}

#[derive(PartialEq, Clone, Debug, Copy)]
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

#[derive(PartialEq,Debug, Clone)]
pub enum Statement{
    Expr(Expression),
    Block(BlockStatement),
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

impl Statement {
    pub fn with(object: Expression, body: Statement) -> Self {
        Statement::With(
            WithStatement::new(object, body)
        )
    }
    pub fn while_stmt(test: Expression, body: Statement) -> Self {
        Statement::While (
            WhileStatement::new(test, body)
        )
    }
    pub fn if_stmt(test: Expression, consequent: Statement, alt: Option<Statement>) -> Self {
        Statement::If(
            IfStatement::new(test, consequent, alt)
        )
    }
}

#[derive(PartialEq,Debug, Clone)]
pub struct WithStatement{
    pub object: Expression,
    pub body: Box<Statement>,
}

impl WithStatement {
    pub fn new(object: Expression, body: Statement) -> Self {
        WithStatement {
            object,
            body: Box::new(body),
        }
    }
}

#[derive(PartialEq,Debug, Clone)]
pub struct LabeledStatement{
    pub label: Identifier,
    pub body: Box<Statement>,
}

impl LabeledStatement {
    pub fn new(label: &str, body: Statement) -> Self {
        Self {
            label: label.to_string(),
            body: Box::new(body),
        }
    }
}

#[derive(PartialEq,Debug, Clone)]
pub struct IfStatement{
    pub test: Expression,
    pub consequent: Box<Statement>,
    pub alternate: Option<Box<Statement>>,
}

impl IfStatement {
    pub fn new(test: Expression, consequent: Statement, alternate: Option<Statement>) -> Self {
        Self {
            test,
            consequent: Box::new(consequent),
            alternate: match alternate {
                Some(stmt) => Some(Box::new(stmt)),
                None => None
            }
        }
    }
}

#[derive(PartialEq,Debug, Clone)]
pub struct SwitchStatement{
    pub discriminant: Expression,
    pub cases: Vec<SwitchCase>,
}

pub type BlockStatement = Vec<ProgramPart>;
#[derive(PartialEq,Debug, Clone)]
pub struct TryStatement{
    pub block: BlockStatement,
    pub handler: Option<CatchClause>,
    pub finalizer: Option<BlockStatement>,
}

#[derive(PartialEq,Debug, Clone)]
pub struct CatchClause{
    pub param: Pattern,
    pub body: BlockStatement,
}

#[derive(PartialEq,Debug, Clone)]
pub struct WhileStatement{
    pub test: Expression,
    pub body: Box<Statement>,
}

impl WhileStatement {
    pub fn new(test: Expression, body: Statement) -> Self {
        Self {
            test,
            body: Box::new(body),
        }
    }
}

#[derive(PartialEq,Debug, Clone)]
pub struct DoWhileStatement{
    pub test: Expression,
    pub body: Box<Statement>,
}

impl DoWhileStatement {
    pub fn new(test: Expression, body: Statement) -> Self {
        Self {
            test,
            body: Box::new(body),
        }
    }
}

#[derive(PartialEq,Debug, Clone)]
pub struct ForStatement{
    pub init: Option<LoopInit>,
    pub test: Option<Expression>,
    pub update: Option<Expression>,
    pub body: Box<Statement>,
}

impl ForStatement {
    pub fn new(init: Option<LoopInit>, test: Option<Expression>, update: Option<Expression>, body: Statement) -> Self {
        Self {
            init: init,
            test: test,
            update: update,
            body: Box::new(body)
        }
    }

    pub fn normal(init: LoopInit, test: Expression, update: Expression, body: Statement) -> Self {
        Self::new(Some(init), Some(test), Some(update), body)
    }

    pub fn infinite(body: Statement) -> Self {
        Self::new(None, None, None, body)
    }

}

#[derive(PartialEq,Debug, Clone)]
pub enum LoopInit{
    Variable(Vec<VariableDecl>),
    Expr(Expression),
}

#[derive(PartialEq,Debug, Clone)]
pub struct ForInStatement{
    pub left: LoopLeft,
    pub right: Expression,
    pub body: Box<Statement>,
}

impl ForInStatement {
    pub fn new(left: LoopLeft, right: Expression, body: Statement) -> Self {
        Self {
            left,
            right,
            body: Box::new(body)
        }
    }
}

#[derive(PartialEq,Debug, Clone)]
pub struct ForOfStatement{
    pub left: LoopLeft,
    pub right: Expression,
    pub body: Box<Statement>,
    pub is_await: bool,
}

impl ForOfStatement {
    pub fn new(left: LoopLeft, right: Expression, body: Statement, is_await: bool) -> Self {
        Self {
            left,
            right,
            body: Box::new(body),
            is_await,
        }
    }
}

#[derive(PartialEq,Debug, Clone)]
pub enum LoopLeft{
    Variable(VariableDecl),
    Pattern(Pattern),
}
pub type Identifier = String;

#[derive(PartialEq,Debug, Clone)]
pub struct Function{
    pub id: Option<String>,
    pub params: Vec<FunctionArg>,
    pub body: FunctionBody,
    pub generator: bool,
    pub is_async: bool,
}
#[derive(PartialEq,Debug, Clone)]
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

    pub fn is_rest(&self) -> bool {
        match self {
            FunctionArg::Expr(ref e) => match e {
                Expression::Spread(_) => true,
                _ => false,
            },
            FunctionArg::Pattern(ref p) => match p {
                Pattern::RestElement(_) => true,
                _ => false,
            }
        }
    }

    pub fn new_expr(expr: Expression) -> Self {
        FunctionArg::Expr(expr)
    }

    pub fn new_patt(patt: Pattern) -> Self {
        FunctionArg::Pattern(patt)
    }

    pub fn ident(name: &str) -> Self {
        FunctionArg::Pattern(Pattern::ident(name))
    }
}

pub type FunctionBody = Vec<ProgramPart>;

#[derive(PartialEq,Debug, Clone)]
pub struct Directive{
    pub expression: Literal,
    pub directive: String,
}

impl Directive {
    pub fn new(value: &str) -> Self {
        let expression = Literal::string(value);
        let directive = value
                        .trim_matches(|c| c == '\''
                                        || c == '"')
                        .to_string();
        Self {
            expression,
            directive,
        }
    }
}

#[derive(PartialEq,Debug, Clone)]
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
            ress::Token::String(ref _string_lit) => Some(Literal::String(token.to_string())),
            ress::Token::Numeric(ref _num) => Some(Literal::Number(token.to_string())),
            ress::Token::Boolean(ref b) => Some(Literal::Boolean(b.into())),
            ress::Token::RegEx(ref r) => Some(Literal::RegEx(r.into())),
            _ => None
        }
    }

    pub fn string(string: &str) -> Self {
        Literal::String(string.to_string())
    }

    pub fn number(num: &str) -> Self {
        Literal::Number(num.to_string())
    }

    pub fn regex(pattern: &str, flags: &str) -> Self {
        let inner = RegEx::new(pattern, flags);
        Literal::RegEx(inner)
    }

    pub fn is_valid_property_key(&self) -> bool {
        self.is_string() || self.is_number() || self.is_boolean()
    }

    pub fn is_string(&self) -> bool {
        match self {
            Literal::String(_) => true,
            _ => false,
        }
    }

    pub fn is_number(&self) -> bool {
        match self {
            Literal::Number(_) => true,
            _ => false,
        }
    }

    pub fn is_boolean(&self) -> bool {
        match self {
            Literal::Boolean(_) => true,
            _ => false,
        }
    }
}

#[derive(PartialEq,Debug, Clone)]
pub struct RegEx{
    pub pattern: String,
    pub flags: String,
}

impl<'a> From<&'a ress::RegEx> for RegEx {
    fn from(other: &'a ress::RegEx) -> Self {
        Self::from_ress_parts(&other.body, &other.flags)
    }
}

impl RegEx {
    pub fn new(body: &str, flags: &str) -> Self {
        RegEx {
            pattern: body.to_string(),
            flags: flags.to_string(),
        }
    }
    pub fn from_ress_parts(body: &str, flags: &Option<String>) -> Self
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

#[derive(PartialEq,Debug, Clone)]
pub struct SwitchCase{
    pub test: Option<Expression>,
    pub consequent: Vec<ProgramPart>,
}

#[derive(PartialEq,Debug, Clone)]
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

    pub fn is_valid_property_key_literal(&self) -> bool {
        match self {
            Expression::Literal(ref l) => l.is_valid_property_key(),
            _ => false
        }
    }

    pub fn as_ident(self) -> Option<Identifier> {
        match self {
            Expression::Ident(i) => Some(i),
            _ => None,
        }
    }

    pub fn ident(name: &str) -> Self {
        Expression::Ident(name.to_string())
    }

    pub fn string(val: &str) -> Self {
        Expression::Literal(Literal::string(val))
    }

    pub fn number(val: &str) -> Self {
        Expression::Literal(Literal::number(val))
    }

    pub fn boolean(val: bool) -> Self {
        Expression::Literal(Literal::Boolean(val))
    }

    pub fn regex(pattern: &str, flags: &str) -> Self {
        Expression::Literal(Literal::regex(pattern, flags))
    }

    pub fn binary(left: Expression, operator: BinaryOperator, right: Expression) -> Self {
        Expression::Binary(BinaryExpression::new(left, operator, right))
    }

    pub fn call(callee: Expression, arguments: Vec<Expression>) -> Self {
        Expression::Call(CallExpression::new(callee, arguments))
    }

    pub fn member(object: Expression, property: Expression, computed: bool) -> Self {
        Expression::Member(MemberExpression::new(object, property, computed))
    }

    pub fn logical(left: Expression, operator: LogicalOperator, right: Expression) -> Self {
        Expression::Logical(
            LogicalExpression::new(operator, left, right)
        )
    }

    pub fn function(id: Option<String>, params: Vec<FunctionArg>, body: FunctionBody,
                    generator: bool, is_async: bool) -> Self {
        Expression::Function(Function {
            id,
            params,
            body,
            generator,
            is_async,
        })
    }

    pub fn yield_expr(arg: Option<Expression>, delegate: bool) -> Self {
        Expression::Yield(YieldExpression::new(
            arg, delegate
        ))
    }

    pub fn yield_with_arg(arg: Expression, delegate: bool) -> Self {
        Expression::Yield(YieldExpression::new(Some(arg), delegate))
    }

    pub fn empty_yield(delegate: bool) -> Self {
        Expression::Yield(YieldExpression::new(None, delegate))
    }
}

pub type ArrayExpression = Vec<Option<Expression>>;
pub type ObjectExpression = Vec<ObjectProperty>;
#[derive(PartialEq,Debug, Clone)]
pub enum ObjectProperty{
    Property(Property),
    Spread(Box<Expression>)
}

impl ObjectProperty {
    pub fn spread(arg: Expression) -> Self {
        ObjectProperty::Spread(Box::new(arg))
    }

    pub fn string(key: &str, value: &str) -> Self {
        ObjectProperty::Property(Property::string(key, value))
    }

    pub fn number(key: &str, value: &str) -> Self {
        ObjectProperty::Property(Property::number(key, value))
    }
}
#[derive(PartialEq,Debug, Clone)]
pub struct Property {
    pub key: PropertyKey,
    pub value: PropertyValue,
    pub kind: PropertyKind,
    pub method: bool,
    pub computed: bool,
    pub short_hand: bool,
}

impl Property {
    pub fn string(key: &str, value: &str) -> Self {
        Self {
            key: PropertyKey::Ident(key.to_string()),
            value: PropertyValue::Expr(Expression::string(value)),
            kind: PropertyKind::Init,
            method: false,
            computed: false,
            short_hand: false,
        }
    }

    pub fn number(key: &str, value: &str) -> Self {
        Self {
            key: PropertyKey::Ident(key.to_string()),
            value: PropertyValue::Expr(Expression::number(value)),
            kind: PropertyKind::Init,
            method: false,
            computed: false,
            short_hand: false,
        }
    }
}

#[derive(PartialEq,Debug, Clone)]
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
}

#[derive(PartialEq,Debug, Clone)]
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
#[derive(PartialEq,Debug, Clone)]
pub enum PropertyKind{
    Init,
    Get,
    Set,
    Ctor,
    Method,
}
#[derive(PartialEq,Debug, Clone)]
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

    pub fn ident(name: &str) -> Self {
        Pattern::Identifier(name.to_string())
    }

    pub fn rest_element(arg: Pattern) -> Self {
        Pattern::RestElement(Box::new(arg))
    }
}

pub type ObjectPattern = Vec<ObjectPatternPart>;
#[derive(PartialEq,Debug, Clone)]
pub enum ObjectPatternPart{
    Assignment(Property),
    Rest(Box<Pattern>),
}

impl ObjectPatternPart {
    pub fn rest(arg: Pattern) -> Self {
        ObjectPatternPart::Rest(Box::new(arg))
    }

    pub fn string(key: &str, value: &str) -> Self {
        ObjectPatternPart::Assignment(
            Property::string(key, value)
        )
    }

    pub fn number(key: &str, value: &str) -> Self {
        ObjectPatternPart::Assignment(
            Property::number(key, value)
        )
    }
}

#[derive(PartialEq,Debug, Clone)]
pub struct AssignmentPattern{
    pub left: Box<Pattern>,
    pub right: Box<Expression>,
}

impl AssignmentPattern {
    pub fn new(left: Pattern, right: Expression) -> Self {
        Self {
            left: Box::new(left),
            right: Box::new(right),
        }
    }
}

#[derive(PartialEq,Debug, Clone)]
pub struct UnaryExpression{
    pub operator: UnaryOperator,
    pub prefix: bool,
    pub argument: Box<Expression>,
}

impl UnaryExpression {
    pub fn new(operator: UnaryOperator, prefix: bool, arg: Expression) -> Self {
        Self {
            operator,
            prefix,
            argument: Box::new(arg)
        }
    }

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

#[derive(PartialEq,Debug, Clone)]
pub struct UpdateExpression{
    pub operator: UpdateOperator,
    pub argument: Box<Expression>,
    pub prefix: bool,
}

impl UpdateExpression {
    pub fn new(operator: UpdateOperator, arg: Expression, prefix: bool) -> Self {
        Self {
            operator,
            prefix,
            argument: Box::new(arg),
        }
    }
}

#[derive(PartialEq,Debug, Clone)]
pub enum UpdateOperator{
    Increment,
    Decrement,
}

#[derive(PartialEq,Debug, Clone)]
pub struct BinaryExpression{
    pub operator: BinaryOperator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

impl BinaryExpression {
    pub fn new(left: Expression, operator: BinaryOperator, right: Expression) -> Self {
        Self {
            operator,
            left: Box::new(left),
            right: Box::new(right)
        }
    }
}

#[derive(PartialEq,Debug, Clone)]
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

#[derive(PartialEq,Debug, Clone)]
pub struct AssignmentExpression{
    pub operator: AssignmentOperator,
    pub left: AssignmentLeft,
    pub right: Box<Expression>,
}

impl AssignmentExpression {
    pub fn new(operator: AssignmentOperator, left: AssignmentLeft, right: Expression) -> Self {
        Self {
            operator,
            left,
            right: Box::new(right),
        }
    }
}

#[derive(PartialEq,Debug, Clone)]
pub enum AssignmentLeft{
    Pattern(Pattern),
    Expr(Box<Expression>),
}

impl AssignmentLeft {
    pub fn expr(value: Expression) -> Self {
        AssignmentLeft::Expr(Box::new(value))
    }
}

#[derive(PartialEq,Debug, Clone)]
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

#[derive(PartialEq,Debug, Clone)]
pub struct LogicalExpression{
    pub operator: LogicalOperator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

impl LogicalExpression {
    pub fn new(operator: LogicalOperator, left: Expression, right: Expression) -> Self {
        Self {
            operator,
            left: Box::new(left),
            right: Box::new(right),
        }
    }
}

#[derive(PartialEq,Debug, Clone)]
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

#[derive(PartialEq,Debug, Clone)]
pub struct MemberExpression{
    pub object: Box<Expression>,
    pub property: Box<Expression>,
    pub computed: bool,
}

impl MemberExpression {
    pub fn new(object: Expression, property: Expression, computed: bool) -> Self {
        Self {
            object: Box::new(object),
            property: Box::new(property),
            computed,
        }
    }
}

#[derive(PartialEq,Debug, Clone)]
pub struct ConditionalExpression{
    pub test: Box<Expression>,
    pub alternate: Box<Expression>,
    pub consequent: Box<Expression>,
}

impl ConditionalExpression {
    pub fn new(test: Expression, alt: Expression, consequent: Expression) -> Self {
        Self {
            test: Box::new(test),
            alternate: Box::new(alt),
            consequent: Box::new(consequent),
        }
    }
}

#[derive(PartialEq,Debug, Clone)]
pub struct CallExpression{
    pub callee: Box<Expression>,
    pub arguments: Vec<Expression>,
}

impl CallExpression {
    pub fn new(callee: Expression, arguments: Vec<Expression>) -> Self {
        Self {
            callee: Box::new(callee),
            arguments,
        }
    }
}

#[derive(PartialEq,Debug, Clone)]
pub struct NewExpression{
    pub callee: Box<Expression>,
    pub arguments: Vec<Expression>,
}

impl NewExpression {
    pub fn new(callee: Expression, arguments: Vec<Expression>) -> Self {
        Self {
            callee: Box::new(callee),
            arguments,
        }
    }
}

pub type SequenceExpression = Vec<Expression>;

#[derive(PartialEq,Debug, Clone)]
pub struct ArrowFunctionExpression{
    pub id: Option<String>,
    pub params: Vec<FunctionArg>,
    pub body: ArrowFunctionBody,
    pub expression: bool,
    pub generator: bool,
    pub is_async: bool,
}

#[derive(PartialEq,Debug, Clone)]
pub enum ArrowFunctionBody{
    FunctionBody(FunctionBody),
    Expr(Box<Expression>)
}

impl ArrowFunctionBody {
    pub fn expr(value: Expression) -> Self {
        ArrowFunctionBody::Expr(Box::new(value))
    }
}

#[derive(PartialEq,Debug, Clone)]
pub struct YieldExpression{
    pub argument: Option<Box<Expression>>,
    pub delegate: bool,
}

impl YieldExpression {
    pub fn new(arg: Option<Expression>, delegate: bool) -> Self {
        Self {
            argument: match arg {
                Some(arg) => Some(Box::new(arg)),
                None => None,
            },
            delegate,
        }
    }
}

#[derive(PartialEq,Debug, Clone)]
pub struct TaggedTemplateExpression{
    pub tag: Box<Expression>,
    pub quasi: TemplateLiteral,
}

impl TaggedTemplateExpression {
    pub fn new(tag: Expression, quasi: TemplateLiteral) -> Self {
        Self {
            tag: Box::new(tag),
            quasi,
        }
    }
}

#[derive(PartialEq,Debug, Clone)]
pub struct TemplateLiteral{
    pub quasis: Vec<TemplateElement>,
    pub expressions: Vec<Expression>,
}

#[derive(PartialEq,Debug, Clone)]
pub struct TemplateElement{
    pub tail: bool,
    pub cooked: String,
    pub raw: String,
}

#[derive(PartialEq,Debug, Clone)]
pub struct Class{
    pub id: Option<Identifier>,
    pub super_class: Option<Box<Expression>>,
    pub body: Vec<Property>,
}

impl Class {
    pub fn new(id: Option<&str>, super_class: Option<Expression>, body: Vec<Property>) -> Self {
        Self {
            id: match id {
                Some(s) => Some(s.to_string()),
                None => None,
            },
            super_class: match super_class {
                Some(e) => Some(Box::new(e)),
                None => None
            },
            body,
        }
    }

    pub fn no_super(id: &str, body: Vec<Property>) -> Self {
        Self {
            id: Some(id.to_string()),
            super_class: None,
            body,
        }
    }

    pub fn no_id_no_super(body: Vec<Property>) -> Self {
        Self {
            id: None,
            super_class: None,
            body,
        }
    }
}

#[derive(PartialEq,Debug, Clone)]
pub struct MethodDef{
    pub key: Expression,
    pub value: Expression,
    pub kind: MethodKind,
    pub computed: bool,
    pub is_static: bool,
}

impl MethodDef {
    pub fn constructor(value: Expression) -> Self {
        Self {
            key: Expression::Ident("constructor".to_string()),
            value,
            kind: MethodKind::Constructor,
            computed: false,
            is_static: false,
        }
    }

    pub fn normal(key: &str, value: Expression) -> Self {
        Self {
            key: Expression::Ident(key.to_string()),
            value,
            kind: MethodKind::Method,
            computed: false,
            is_static: false,
        }
    }

    pub fn static_method(key: &str, value: Expression) -> Self {
        Self {
            key: Expression::Ident(key.to_string()),
            value,
            kind: MethodKind::Method,
            computed: false,
            is_static: true,
        }
    }

    pub fn getter(key: &str, value: Expression) -> Self {
        Self {
            key: Expression::Ident(key.to_string()),
            value,
            kind: MethodKind::Get,
            computed: false,
            is_static: false,
        }
    }

    pub fn setter(key: &str, value: Expression) -> Self {
        Self {
            key: Expression::Ident(key.to_string()),
            value,
            kind: MethodKind::Set,
            computed: false,
            is_static: false,
        }
    }
}

#[derive(PartialEq,Debug, Clone)]
pub enum MethodKind{
    Constructor,
    Method,
    Get,
    Set,
}

#[derive(PartialEq,Debug, Clone)]
pub struct MetaProperty{
    pub meta: Identifier,
    pub property: Identifier,
}

impl MetaProperty {
    pub fn new(meta: &str, property: &str) -> Self {
        Self {
            meta: meta.to_string(),
            property: property.to_string()
        }
    }
}