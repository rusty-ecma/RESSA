use ress;

#[derive(PartialEq, Debug)]
#[doc(hidden)]
pub struct Node {
    pub position: Position,
    pub item: Item,
}
#[derive(PartialEq, Debug, Clone, Copy)]
/// A position in a file
pub struct Position {
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
        Self { line: 0, column: 0 }
    }
}

#[derive(PartialEq, Debug)]
#[doc(hidden)]
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

/// A fully parsed javascript program.
///
/// It is essentially a collection of `ProgramPart`s
/// with a flag denoting if the representation is
/// a ES6 Module or a Script.
#[derive(PartialEq, Debug)]
pub enum Program {
    /// An ES6 Module
    Module(Vec<ProgramPart>),
    /// Not an ES6 Module
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
/// A single part of a Javascript program.
/// This will be either a Directive, Declaration or a Statement
#[derive(PartialEq, Debug, Clone)]
pub enum ProgramPart {
    /// A Directive like `'use strict';`
    Directive(Directive),
    /// A variable, function or module declaration
    Decl(Declaration),
    /// Any other kind of statement
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
            _ => false,
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

/// A module declaration, This would only be available
/// in an ES Module, it would be either an import or
/// export at the top level
#[derive(PartialEq, Debug, Clone)]
pub enum ModuleDecl {
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
        ModuleDecl::Import(ModuleImport { specifiers, source })
    }

    pub fn imports_with_default(default: &str, normal: &[&str], from: &str) -> Self {
        let mut specifiers = Vec::with_capacity(normal.len() + 1);
        specifiers.push(ImportSpecifier::default(default));
        for name in normal {
            specifiers.push(ImportSpecifier::normal_no_local(name))
        }
        let source = Literal::string(from);
        ModuleDecl::Import(ModuleImport { specifiers, source })
    }

    pub fn simple_imports(names: &[&str], from: &str) -> Self {
        let specifiers = names
            .iter()
            .map(|n| ImportSpecifier::normal_no_local(n))
            .collect();
        let source = Literal::string(from);
        ModuleDecl::Import(ModuleImport { specifiers, source })
    }

    pub fn namespace_import(local: &str, from: &str) -> Self {
        ModuleDecl::Import(ModuleImport {
            specifiers: vec![ImportSpecifier::namespace(local)],
            source: Literal::string(from),
        })
    }

    pub fn export(export: ModuleExport) -> Self {
        ModuleDecl::Export(export)
    }

    pub fn export_all(name: &str) -> Self {
        ModuleDecl::Export(ModuleExport::All(Literal::string(name)))
    }

    pub fn export_default_expr(expr: Expression) -> Self {
        ModuleDecl::Export(ModuleExport::Default(DefaultExportDecl::Expr(expr)))
    }

    pub fn export_default_decl(decl: Declaration) -> Self {
        ModuleDecl::Export(ModuleExport::Default(DefaultExportDecl::Decl(decl)))
    }

    pub fn export_named_decl(decl: Declaration) -> Self {
        ModuleDecl::Export(ModuleExport::Named(NamedExportDecl::Decl(decl)))
    }

    pub fn export_single_named_spec(name: &str) -> Self {
        ModuleDecl::Export(ModuleExport::Named(NamedExportDecl::Specifier(
            vec![ExportSpecifier::no_alias(name)],
            None,
        )))
    }

    pub fn export_single_named_spec_with_alias(name: &str, alias: &str) -> Self {
        ModuleDecl::Export(ModuleExport::Named(NamedExportDecl::Specifier(
            vec![ExportSpecifier::with_alias(name, alias)],
            None,
        )))
    }
}

/// A declaration that imports exported
/// members of another module
///
/// ```js
/// import {Thing} from './stuff.js';
/// ```
#[derive(PartialEq, Debug, Clone)]
pub struct ModuleImport {
    pub specifiers: Vec<ImportSpecifier>,
    pub source: Literal,
}

impl ModuleImport {
    pub fn new(specifiers: Vec<ImportSpecifier>, source: Literal) -> Self {
        Self { specifiers, source }
    }
}
/// The name of the thing being imported
#[derive(PartialEq, Debug, Clone)]
pub enum ImportSpecifier {
    /// A specifier in curly braces, this might
    /// have a local alias
    ///
    /// ```js
    /// import {Thing} from './stuff.js';
    /// import {People as Persons} from './places.js';
    /// ```
    Normal(Identifier, Option<Identifier>),
    /// A specifier that has been exported with the
    /// default keyword, this should not be wrapped in
    /// curly braces.
    /// ```js
    /// import DefaultThing from './stuff/js';
    /// ```
    Default(Identifier),
    /// Import all exported members from a module
    /// in a namespace.
    ///
    /// ```js
    /// import * as Moment from 'moment.js';
    /// ```
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
/// Something exported from this module
#[derive(PartialEq, Debug, Clone)]
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
/// An export that has a name
/// ```js
/// export function thing() {}
/// export {stuff} from 'place';
#[derive(PartialEq, Debug, Clone)]
pub enum NamedExportDecl {
    Decl(Declaration),
    Specifier(Vec<ExportSpecifier>, Option<Literal>),
}
/// A default export
/// ```js
/// export default class Thing {}
/// ```
#[derive(PartialEq, Debug, Clone)]
pub enum DefaultExportDecl {
    Decl(Declaration),
    Expr(Expression),
}
/// The name of the thing being exported
/// this might include an alias
/// ```js
/// //no-alias
/// export {Thing} from 'place';
/// //aliased
/// export {Stuff as NewThing} from 'place'
/// ```
#[derive(PartialEq, Debug, Clone)]
pub struct ExportSpecifier {
    pub local: Identifier,
    pub exported: Option<Identifier>,
}

impl ExportSpecifier {
    pub fn no_alias(local: &str) -> Self {
        Self {
            local: local.to_string(),
            exported: None,
        }
    }

    pub fn with_alias(local: &str, alias: &str) -> Self {
        Self {
            local: local.to_string(),
            exported: Some(alias.to_string()),
        }
    }
}
/// The declaration of a variable, function, class, import or export
#[derive(PartialEq, Debug, Clone)]
pub enum Declaration {
    /// A variable declaration
    /// ```js
    /// var x, b;
    /// let y, a = 0;
    /// const q = 100
    /// ```
    Variable(VariableKind, Vec<VariableDecl>),
    /// A function declaration
    /// ```js
    /// function thing() {}
    /// ```
    Function(Function),
    /// A class declaration
    /// ```js
    /// class Thing {}
    /// ```
    Class(Class),
    /// An import declaration
    /// ```js
    /// import * as moment from 'moment';
    /// import Thing, {thing} from 'stuff';
    /// ```
    Import(Box<ModuleImport>),
    /// An export declaration
    /// ```js
    /// export function thing() {}
    /// ```
    Export(Box<ModuleExport>),
}
/// The identifier and optional value of a variable declaration
#[derive(PartialEq, Debug, Clone)]
pub struct VariableDecl {
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
            init: None,
        }
    }

    pub fn with_value(name: &str, value: Expression) -> Self {
        Self {
            id: Pattern::ident(name),
            init: Some(value),
        }
    }

    pub fn destructed(names: &[&str], value: ObjectExpression) -> Self {
        let id = Pattern::Object(
            names
                .iter()
                .map(|name| {
                    ObjectPatternPart::Assignment(Property {
                        key: PropertyKey::Expr(Expression::ident(&name.to_string())),
                        value: PropertyValue::None,
                        kind: PropertyKind::Init,
                        method: false,
                        short_hand: true,
                        computed: false,
                    })
                })
                .collect(),
        );
        Self {
            id,
            init: Some(Expression::Object(value)),
        }
    }

    pub fn destructed_with_rest(names: &[&str], rest: &str, value: ObjectExpression) -> Self {
        let mut props: Vec<ObjectPatternPart> = names
            .iter()
            .map(|name| {
                ObjectPatternPart::Assignment(Property {
                    key: PropertyKey::Expr(Expression::ident(&name.to_string())),
                    value: PropertyValue::None,
                    kind: PropertyKind::Init,
                    computed: false,
                    method: false,
                    short_hand: true,
                })
            })
            .collect();
        props.push(ObjectPatternPart::Rest(Box::new(Pattern::RestElement(
            Box::new(Pattern::ident(rest)),
        ))));
        let id = Pattern::Object(props);
        let init = Some(Expression::Object(value));
        Self { id, init }
    }
}
/// The kind of variable being defined (`var`/`let`/`const`)
#[derive(PartialEq, Clone, Debug, Copy)]
pub enum VariableKind {
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
/// A slightly more granular part of an es program than ProgramPart
#[derive(PartialEq, Debug, Clone)]
pub enum Statement {
    /// Any expression
    Expr(Expression),
    /// A collection of program parts wrapped in curly braces
    Block(BlockStatement),
    /// A single semi-colon
    Empty,
    /// The contextual keyword `debugger`
    Debugger,
    /// A with statement, this puts one object at the top of
    /// the identifier search tree.
    /// > note: this cannot be used in a strict context
    /// ```js
    /// function random() {
    ///     return 0;
    /// }
    /// let rand;
    /// with (Math) {
    ///     rand = floor(random() * 100) + 1;
    /// }
    /// //rand !== 0
    /// ```
    With(WithStatement),
    /// A return statement
    /// ```js
    /// function thing() {
    ///     return 'stuff';
    /// }
    /// function stuff() {
    ///     return;
    /// }
    Return(Option<Expression>),
    /// A labeled statement
    /// ```js
    /// label: {
    ///     break label;
    /// }
    /// ```
    Labeled(LabeledStatement),
    /// A break statement
    /// ```js
    /// label: {
    ///     break label;
    /// }
    /// while (true) {
    ///     break;
    /// }
    /// ```
    Break(Option<Identifier>),
    /// A short circuit continuation of a loop
    /// ```js
    /// label: while (true) {
    ///     if (Math.floor(Math.random() * 100) > 50) {
    ///         continue;
    ///     } else {
    ///         console.log('too low')
    ///     }
    /// }
    /// ```
    Continue(Option<Identifier>),
    /// An if statement
    /// ```js
    /// if (1 < 2) {
    ///     console.log('Always true');
    /// } else {
    ///     console.log('Never true');
    /// }
    /// ```
    If(IfStatement),
    /// A switch statement
    /// ```js
    /// switch (Math.floor(Math.random()) * 10) {
    ///     case 1:
    ///
    ///     break;
    ///     case 2:
    ///     case 3:
    ///     case 4:
    ///         return false;
    ///     default:
    ///         return true;
    /// }
    /// ```
    Switch(SwitchStatement),
    /// A statement that throws an error
    /// ```js
    /// function error() {
    ///     throw 'hahahaha';
    /// }
    ///
    /// function newError() {
    ///     throw new Error('hohoho');
    /// }
    /// ```
    Throw(Expression),
    /// A try/catch block
    /// ```js
    /// try {
    ///
    /// } catch (e) {
    ///
    /// } finally {
    ///
    /// }
    /// ```
    Try(TryStatement),
    /// A while loop
    /// ```js
    /// while (false) {
    ///
    /// }
    /// var i = 0;
    /// while (i < 100) {
    ///     if (Math.floor(Math.random() * 100) > 50) {
    ///         i--;
    ///     } else {
    ///         i += 5;
    ///     }
    /// }
    /// ```
    While(WhileStatement),
    /// A while loop that executes its body first
    /// ```js
    /// do {
    ///     console.log('at least once')
    /// } while (Math.floor(Math.random() * 100) < 75)
    /// ```
    DoWhile(DoWhileStatement),
    /// A "c-style" for loop
    /// ```js
    /// for (var i = 0; i < 100; i++) console.log(i);
    /// for (;;) {
    ///     console.log('forever!');
    /// }
    /// ```
    For(ForStatement),
    /// A for in statement, this kind of for statement
    /// will extract each key from an indexable thing
    /// ```js
    /// for (var i in [2,3,4,5,6]) {
    ///     console.log(i);
    /// }
    /// //prints 0, 1, 2, 3, 4
    /// for (var k in {a: 'b', c: 'd'}) {
    ///     console.log(k);
    /// }
    /// //prints a, b
    /// ```
    ForIn(ForInStatement),
    /// A for of statement, this kind of for statement
    /// will extract the value from a generator or iterator
    /// ```js
    /// for (var k of [2, 3, 4, 5, 6]) {
    ///     console.log(k);
    /// }
    /// //prints 2, 3, 4, 5, 6
    /// ```
    ForOf(ForOfStatement),
    /// A var statement
    /// ```js
    /// var x;
    /// var x, y = 'huh?';
    /// ```
    Var(Vec<VariableDecl>),
}

impl Statement {
    pub fn with(object: Expression, body: Statement) -> Self {
        Statement::With(WithStatement::new(object, body))
    }
    pub fn while_stmt(test: Expression, body: Statement) -> Self {
        Statement::While(WhileStatement::new(test, body))
    }
    pub fn if_stmt(test: Expression, consequent: Statement, alt: Option<Statement>) -> Self {
        Statement::If(IfStatement::new(test, consequent, alt))
    }
}
/// A with statement, this puts one object at the top of
/// the identifier search tree.
/// > note: this cannot be used in a strict context
/// ```js
/// function random() {
///     return 0;
/// }
/// let rand;
/// with (Math) {
///     rand = floor(random() * 100) + 1;
/// }
/// //rand !== 0
/// ```
#[derive(PartialEq, Debug, Clone)]
pub struct WithStatement {
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
/// A break statement
/// ```js
/// label: {
///     break label;
/// }
/// while (true) {
///     break;
/// }
/// ```
#[derive(PartialEq, Debug, Clone)]
pub struct LabeledStatement {
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
/// An if statement
/// ```js
/// if (1 < 2) {
///     console.log('Always true');
/// } else {
///     console.log('Never true');
/// }
/// ```
#[derive(PartialEq, Debug, Clone)]
pub struct IfStatement {
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
                None => None,
            },
        }
    }
}
/// A switch statement
/// ```js
/// switch (Math.floor(Math.random()) * 10) {
///     case 1:
///
///     break;
///     case 2:
///     case 3:
///     case 4:
///         return false;
///     default:
///         return true;
/// }
/// ```
#[derive(PartialEq, Debug, Clone)]
pub struct SwitchStatement {
    pub discriminant: Expression,
    pub cases: Vec<SwitchCase>,
}
/// A collection of program parts wrapped in curly braces
pub type BlockStatement = Vec<ProgramPart>;
/// A try/catch block
/// ```js
/// try {
///
/// } catch (e) {
///
/// } finally {
///
/// }
/// ```
#[derive(PartialEq, Debug, Clone)]
pub struct TryStatement {
    pub block: BlockStatement,
    pub handler: Option<CatchClause>,
    pub finalizer: Option<BlockStatement>,
}
/// The error handling part of a `TryStatement`
#[derive(PartialEq, Debug, Clone)]
pub struct CatchClause {
    pub param: Option<Pattern>,
    pub body: BlockStatement,
}
/// A while loop
/// ```js
/// while (false) {
///
/// }
/// var i = 0;
/// while (i < 100) {
///     if (Math.floor(Math.random() * 100) > 50) {
///         i--;
///     } else {
///         i += 5;
///     }
/// }
/// ```
#[derive(PartialEq, Debug, Clone)]
pub struct WhileStatement {
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
/// A while loop that executes its body first
/// ```js
/// do {
///     console.log('at least once')
/// } while (Math.floor(Math.random() * 100) < 75)
/// ```
#[derive(PartialEq, Debug, Clone)]
pub struct DoWhileStatement {
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
/// A "c-style" for loop
/// ```js
/// for (var i = 0; i < 100; i++) console.log(i);
/// for (;;) {
///     console.log('forever!');
/// }
/// ```
#[derive(PartialEq, Debug, Clone)]
pub struct ForStatement {
    pub init: Option<LoopInit>,
    pub test: Option<Expression>,
    pub update: Option<Expression>,
    pub body: Box<Statement>,
}

impl ForStatement {
    pub fn new(
        init: Option<LoopInit>,
        test: Option<Expression>,
        update: Option<Expression>,
        body: Statement,
    ) -> Self {
        Self {
            init: init,
            test: test,
            update: update,
            body: Box::new(body),
        }
    }

    pub fn normal(init: LoopInit, test: Expression, update: Expression, body: Statement) -> Self {
        Self::new(Some(init), Some(test), Some(update), body)
    }

    pub fn infinite(body: Statement) -> Self {
        Self::new(None, None, None, body)
    }
}
/// The left most triple of a for loops parenthetical
/// ```js
///  //  vvvvvvvvv
/// for (var i = 0;i < 100; i++)
#[derive(PartialEq, Debug, Clone)]
pub enum LoopInit {
    Variable(VariableKind, Vec<VariableDecl>),
    Expr(Expression),
}
/// A for in statement, this kind of for statement
/// will extract each key from an indexable thing
/// ```js
/// for (var i in [2,3,4,5,6]) {
///     console.log(i);
/// }
/// //prints 0, 1, 2, 3, 4
/// for (var k in {a: 'b', c: 'd'}) {
///     console.log(k);
/// }
/// //prints a, b
/// ```
#[derive(PartialEq, Debug, Clone)]
pub struct ForInStatement {
    pub left: LoopLeft,
    pub right: Expression,
    pub body: Box<Statement>,
}

impl ForInStatement {
    pub fn new(left: LoopLeft, right: Expression, body: Statement) -> Self {
        Self {
            left,
            right,
            body: Box::new(body),
        }
    }
}
/// A for of statement, this kind of for statement
/// will extract the value from a generator or iterator
/// ```js
/// for (var k of [2, 3, 4, 5, 6]) {
///     console.log(k);
/// }
/// //prints 2, 3, 4, 5, 6
/// ```
#[derive(PartialEq, Debug, Clone)]
pub struct ForOfStatement {
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
/// The values on the left hand side of the keyword
/// in a for in or for of loop
#[derive(PartialEq, Debug, Clone)]
pub enum LoopLeft {
    Expr(Expression),
    Variable(VariableKind, VariableDecl),
    Pattern(Pattern),
}
/// A variable, class, or function name
pub type Identifier = String;
/// A function, this will be part of either a function
/// declaration (ID is required) or a function expression
/// (ID is optional)
/// ```js
/// //function declaration
/// function thing() {}
/// //function expressions
/// var x = function() {}
/// let y = function q() {}
/// ```
#[derive(PartialEq, Debug, Clone)]
pub struct Function {
    pub id: Option<String>,
    pub params: Vec<FunctionArg>,
    pub body: FunctionBody,
    pub generator: bool,
    pub is_async: bool,
}
/// A single function argument from a function signature
#[derive(PartialEq, Debug, Clone)]
pub enum FunctionArg {
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
                _ => false,
            },
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
            },
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
            },
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
            },
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
/// The block statement that makes up the function's body
pub type FunctionBody = Vec<ProgramPart>;
/// pretty much always `'use strict'`, this can appear at the
/// top of a file or function
#[derive(PartialEq, Debug, Clone)]
pub struct Directive {
    pub expression: Literal,
    pub directive: String,
}

impl Directive {
    pub fn new(value: &str) -> Self {
        let expression = Literal::string(value);
        let directive = value.trim_matches(|c| c == '\'' || c == '"').to_string();
        Self {
            expression,
            directive,
        }
    }
}
/// A literal value
#[derive(PartialEq, Debug, Clone)]
pub enum Literal {
    /// `null`
    Null,
    /// `"string"`
    /// `'string'`
    String(String),
    /// `0`
    /// `0.0`
    /// `.0`
    /// `0.0e1`
    /// `.0E1`
    /// `0xf`
    /// `0o7`
    /// `0b1`
    Number(String),
    /// `true`
    /// `false`
    Boolean(bool),
    /// `/.+/g`
    RegEx(RegEx),
    /// ```js
    /// `I have ${0} apples`
    /// ```
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
            _ => None,
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
/// A regular expression literal
#[derive(PartialEq, Debug, Clone)]
pub struct RegEx {
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
    pub fn from_ress_parts(body: &str, flags: &Option<String>) -> Self {
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
/// A single case part of a switch statement
#[derive(PartialEq, Debug, Clone)]
pub struct SwitchCase {
    pub test: Option<Expression>,
    pub consequent: Vec<ProgramPart>,
}
/// A slightly more granular program part that a statement
#[derive(PartialEq, Debug, Clone)]
pub enum Expression {
    /// `this`
    ThisExpression,
    /// `super`
    SuperExpression,
    /// `[0,,]`
    Array(ArrayExpression),
    /// `{}`
    Object(ObjectExpression),
    /// see `Function`
    Function(Function),
    /// An operation that has one argument
    /// ```js
    /// typeof 'a';
    /// +9;
    /// ```
    Unary(UnaryExpression),
    /// Increment or decrement
    /// ```js
    /// 1++
    /// --2
    /// ```
    Update(UpdateExpression),
    /// An operation that has two arguments
    Binary(BinaryExpression),
    /// Assignment or update assignment
    /// ```js
    /// a = 0
    /// b += 1
    /// ```
    Assignment(AssignmentExpression),
    /// A specialized `BinaryExpression` for logical evaluation
    /// ```js
    /// true && true
    /// false || true
    /// ```
    Logical(LogicalExpression),
    /// Accessing the member of a value
    /// ```js
    /// b['thing'];
    /// c.stuff;
    /// ```
    Member(MemberExpression),
    /// A ternery expression
    /// ```js
    /// var a = true ? 'stuff' : 'things';
    /// ```
    Conditional(ConditionalExpression),
    /// Calling a function or method
    Call(CallExpression),
    /// Calling a constructor
    New(NewExpression),
    /// Any sequence of expressions seperated with a comma
    Sequence(SequenceExpression),
    /// `...` followed by another `Expression`
    Spread(Box<Expression>),
    /// An arrow function
    /// ```js
    /// () => console.log();
    /// x => {
    ///     return x;
    /// }
    /// ```
    ArrowFunction(ArrowFunctionExpression),
    /// yield a value from inside of a generator function
    Yield(YieldExpression),
    /// A class expression see `Class`
    Class(Class),
    /// currently just `new.target`
    MetaProperty(MetaProperty),
    /// The `await` keyword followed by another `Expression`
    Await(Box<Expression>),
    /// An identifier
    Ident(Identifier),
    /// Used for resolving possible sequence exressions
    /// that are arrow parameters
    ArrowParamPlaceHolder(Vec<FunctionArg>, bool),
    /// A literal value, see `Literal`
    Literal(Literal),
    /// A template literal preceded by a tag function identifier
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
            _ => false,
        }
    }

    pub fn is_valid_property_key_literal(&self) -> bool {
        match self {
            Expression::Literal(ref l) => l.is_valid_property_key(),
            _ => false,
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
        Expression::Logical(LogicalExpression::new(operator, left, right))
    }

    pub fn function(
        id: Option<String>,
        params: Vec<FunctionArg>,
        body: FunctionBody,
        generator: bool,
        is_async: bool,
    ) -> Self {
        Expression::Function(Function {
            id,
            params,
            body,
            generator,
            is_async,
        })
    }

    pub fn yield_expr(arg: Option<Expression>, delegate: bool) -> Self {
        Expression::Yield(YieldExpression::new(arg, delegate))
    }

    pub fn yield_with_arg(arg: Expression, delegate: bool) -> Self {
        Expression::Yield(YieldExpression::new(Some(arg), delegate))
    }

    pub fn empty_yield(delegate: bool) -> Self {
        Expression::Yield(YieldExpression::new(None, delegate))
    }
}
/// `[a, b, c]`
pub type ArrayExpression = Vec<Option<Expression>>;
/// `{a: 'b', c, ...d}`
pub type ObjectExpression = Vec<ObjectProperty>;
/// A single part of an object literal
#[derive(PartialEq, Debug, Clone)]
pub enum ObjectProperty {
    Property(Property),
    Spread(Box<Expression>),
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

/// A single part of an object literal or class
#[derive(PartialEq, Debug, Clone)]
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
            key: PropertyKey::Expr(Expression::ident(&key.to_string())),
            value: PropertyValue::Expr(Expression::string(value)),
            kind: PropertyKind::Init,
            method: false,
            computed: false,
            short_hand: false,
        }
    }

    pub fn number(key: &str, value: &str) -> Self {
        Self {
            key: PropertyKey::Expr(Expression::ident(&key.to_string())),
            value: PropertyValue::Expr(Expression::number(value)),
            kind: PropertyKind::Init,
            method: false,
            computed: false,
            short_hand: false,
        }
    }
}
/// An object literal or class property identifier
#[derive(PartialEq, Debug, Clone)]
pub enum PropertyKey {
    Literal(Literal),
    Expr(Expression),
    Pattern(Pattern),
}

impl PropertyKey {
    pub fn matches(&self, other: &str) -> bool {
        match self {
            PropertyKey::Literal(ref l) => match l {
                Literal::String(ref s) => s == other,
                _ => false,
            },
            PropertyKey::Expr(ref e) => match e {
                Expression::Ident(ref i) => i == other,
                _ => false,
            },
            PropertyKey::Pattern(ref p) => match p {
                Pattern::Identifier(ref i) => i == other,
                _ => false,
            },
        }
    }

    pub fn is_static(&self) -> bool {
        match self {
            PropertyKey::Literal(ref l) => match l {
                Literal::String(ref s) => s == "static",
                _ => false,
            },
            PropertyKey::Expr(ref e) => match e {
                Expression::Ident(ref s) => s == "static",
                _ => false,
            },
            PropertyKey::Pattern(ref p) => match p {
                Pattern::Identifier(ref s) => s == "static",
                _ => false,
            },
        }
    }
}
/// The value of an object literal or class property
#[derive(PartialEq, Debug, Clone)]
pub enum PropertyValue {
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
/// A flag for determining what kind of property
#[derive(PartialEq, Debug, Clone)]
pub enum PropertyKind {
    /// A property with a value
    Init,
    /// A method with the get keyword
    Get,
    /// A method with the set keyword
    Set,
    /// A constructor
    Ctor,
    /// A standard method
    Method,
}
/// All of the different ways you can declare an identifier
/// and/or value
#[derive(PartialEq, Debug, Clone)]
pub enum Pattern {
    Identifier(Identifier),
    Object(ObjectPattern),
    Array(Vec<Option<ArrayPatternPart>>),
    RestElement(Box<Pattern>),
    Assignment(AssignmentPattern),
}
#[derive(PartialEq, Debug, Clone)]
pub enum ArrayPatternPart {
    Patt(Pattern),
    Expr(Expression),
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
/// similar to an `ObjectExpression`
pub type ObjectPattern = Vec<ObjectPatternPart>;
/// A single part of an ObjectPattern
#[derive(PartialEq, Debug, Clone)]
pub enum ObjectPatternPart {
    Assignment(Property),
    Rest(Box<Pattern>),
}

impl ObjectPatternPart {
    pub fn rest(arg: Pattern) -> Self {
        ObjectPatternPart::Rest(Box::new(arg))
    }

    pub fn string(key: &str, value: &str) -> Self {
        ObjectPatternPart::Assignment(Property::string(key, value))
    }

    pub fn number(key: &str, value: &str) -> Self {
        ObjectPatternPart::Assignment(Property::number(key, value))
    }
}
/// An assignment as a pattern
#[derive(PartialEq, Debug, Clone)]
pub struct AssignmentPattern {
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
/// An operation that takes one argument
#[derive(PartialEq, Debug, Clone)]
pub struct UnaryExpression {
    pub operator: UnaryOperator,
    pub prefix: bool,
    pub argument: Box<Expression>,
}

impl UnaryExpression {
    pub fn new(operator: UnaryOperator, prefix: bool, arg: Expression) -> Self {
        Self {
            operator,
            prefix,
            argument: Box::new(arg),
        }
    }

    pub fn has_operator(&self, op: &UnaryOperator) -> bool {
        &self.operator == op
    }

    pub fn has_ident_arg(&self) -> bool {
        self.argument.is_ident()
    }
}
/// The allowed operators for an expression
/// to be `Unary`
#[derive(PartialEq, Debug, Clone)]
pub enum UnaryOperator {
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
            ress::Token::Punct(ref p) => match p {
                ress::Punct::Minus => Some(UnaryOperator::Minus),
                ress::Punct::Plus => Some(UnaryOperator::Plus),
                ress::Punct::Not => Some(UnaryOperator::Not),
                ress::Punct::BitwiseNot => Some(UnaryOperator::Tilde),
                _ => None,
            },
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
/// Increment or decrementing a value
#[derive(PartialEq, Debug, Clone)]
pub struct UpdateExpression {
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
/// `++` or `--`
#[derive(PartialEq, Debug, Clone)]
pub enum UpdateOperator {
    Increment,
    Decrement,
}
/// An operation that requires 2 arguments
#[derive(PartialEq, Debug, Clone)]
pub struct BinaryExpression {
    pub operator: BinaryOperator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

impl BinaryExpression {
    pub fn new(left: Expression, operator: BinaryOperator, right: Expression) -> Self {
        Self {
            operator,
            left: Box::new(left),
            right: Box::new(right),
        }
    }
}
/// The available operations for `Binary` expressions
#[derive(PartialEq, Debug, Clone)]
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
/// An assignment or update + assignment operation
#[derive(PartialEq, Debug, Clone)]
pub struct AssignmentExpression {
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
/// The value being assigned to
#[derive(PartialEq, Debug, Clone)]
pub enum AssignmentLeft {
    Pattern(Pattern),
    Expr(Box<Expression>),
}

impl AssignmentLeft {
    pub fn expr(value: Expression) -> Self {
        AssignmentLeft::Expr(Box::new(value))
    }
}
/// The available operators for assignment expressions
#[derive(PartialEq, Debug, Clone)]
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
            ress::Punct::Assign => Some(AssignmentOperator::Equal),
            ress::Punct::AddAssign => Some(AssignmentOperator::PlusEqual),
            ress::Punct::SubtractAssign => Some(AssignmentOperator::MinusEqual),
            ress::Punct::MultiplyAssign => Some(AssignmentOperator::TimesEqual),
            ress::Punct::DivideAssign => Some(AssignmentOperator::DivEqual),
            ress::Punct::ModuloAssign => Some(AssignmentOperator::ModEqual),
            ress::Punct::LeftShiftAssign => Some(AssignmentOperator::LeftShiftEqual),
            ress::Punct::RightShiftAssign => Some(AssignmentOperator::RightShiftEqual),
            ress::Punct::UnsignedRightShiftAssign => {
                Some(AssignmentOperator::UnsignedRightShiftEqual)
            }
            ress::Punct::BitwiseOrAssign => Some(AssignmentOperator::OrEqual),
            ress::Punct::BitwiseXOrAssign => Some(AssignmentOperator::XOrEqual),
            ress::Punct::BitwiseAndAssign => Some(AssignmentOperator::AndEqual),
            ress::Punct::ExponentAssign => Some(AssignmentOperator::PowerOfEqual),
            _ => None,
        }
    }
}
/// A specialized `BinaryExpression` for logical evaluation
/// ```js
/// true && true
/// false || true
/// ```
#[derive(PartialEq, Debug, Clone)]
pub struct LogicalExpression {
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
/// The available logical operators
#[derive(PartialEq, Debug, Clone)]
pub enum LogicalOperator {
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
/// Accessing the member of a value
/// ```js
/// b['thing'];
/// c.stuff;
/// ```
#[derive(PartialEq, Debug, Clone)]
pub struct MemberExpression {
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
/// A ternery expression
/// ```js
/// var a = true ? 'stuff' : 'things';
/// ```
#[derive(PartialEq, Debug, Clone)]
pub struct ConditionalExpression {
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
/// Calling a function or method
/// ```js
/// Math.random()
/// ```
#[derive(PartialEq, Debug, Clone)]
pub struct CallExpression {
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
/// Calling a constructor
/// ```js
/// new Uint8Array(32);
/// ```
#[derive(PartialEq, Debug, Clone)]
pub struct NewExpression {
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
/// A collection of `Expressions` seperated by commas
pub type SequenceExpression = Vec<Expression>;
/// An arrow function
/// ```js
/// let x = () => y;
/// let q = x => {
///     return x + 1;
/// }
/// ```
#[derive(PartialEq, Debug, Clone)]
pub struct ArrowFunctionExpression {
    pub id: Option<String>,
    pub params: Vec<FunctionArg>,
    pub body: ArrowFunctionBody,
    pub expression: bool,
    pub generator: bool,
    pub is_async: bool,
}
/// The body portion of an arrow function can be either an expression or a block of statements
#[derive(PartialEq, Debug, Clone)]
pub enum ArrowFunctionBody {
    FunctionBody(FunctionBody),
    Expr(Box<Expression>),
}

impl ArrowFunctionBody {
    pub fn expr(value: Expression) -> Self {
        ArrowFunctionBody::Expr(Box::new(value))
    }
}
/// yield a value from inside of a generator function
/// ```js
/// function *gen() {
///     while ((new Date() / 1000) < Number.MAX_VALUE) {
///         yield new Date() / 1000;
///     }
/// }
/// ```
#[derive(PartialEq, Debug, Clone)]
pub struct YieldExpression {
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
/// A Tempalte literal preceded by a function identifier
/// see [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals#Tagged_templates) for more details
#[derive(PartialEq, Debug, Clone)]
pub struct TaggedTemplateExpression {
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
/// A template string literal
/// ```js
/// `I own ${0} birds`;
/// ```
#[derive(PartialEq, Debug, Clone)]
pub struct TemplateLiteral {
    pub quasis: Vec<TemplateElement>,
    pub expressions: Vec<Expression>,
}
/// The text part of a `TemplateLiteral`
#[derive(PartialEq, Debug, Clone)]
pub struct TemplateElement {
    pub tail: bool,
    /// The non-quoted version
    pub cooked: String,
    /// The quoted version
    pub raw: String,
}
/// A way to declare object templates
/// ```js
/// class Thing {
///     constructor() {
///         this._a = 0;
///     }
///     stuff() {
///         return 'stuff'
///     }
///     set a(value) {
///         if (value > 100) {
///             this._a = 0;
///         } else {
///             this._a = value;
///         }
///     }
///     get a() {
///         return this._a;
///     }
/// }
/// let y = class {
///     constructor() {
///         this.a = 100;
///     }
/// }
/// ```
#[derive(PartialEq, Debug, Clone)]
pub struct Class {
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
                None => None,
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
#[doc(hidden)]
#[derive(PartialEq, Debug, Clone)]
pub struct MethodDef {
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

#[derive(PartialEq, Debug, Clone)]
#[doc(hidden)]
pub enum MethodKind {
    Constructor,
    Method,
    Get,
    Set,
}
/// pretty much just `new.target`
/// ```js
/// function Thing(one, two) {
///     if (!new.target) {
///         return new Thing(one, two);
///     }
///     this.one = one;
///     this.two = two;
/// }
/// ```
#[derive(PartialEq, Debug, Clone)]
pub struct MetaProperty {
    pub meta: Identifier,
    pub property: Identifier,
}

impl MetaProperty {
    pub fn new(meta: &str, property: &str) -> Self {
        Self {
            meta: meta.to_string(),
            property: property.to_string(),
        }
    }
}
