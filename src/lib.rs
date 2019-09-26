//! RESSA (Rusty ECMAScript Syntax Analyzer)
//! A library for parsing js files
//!
//! The main interface for this library would be
//! the `Parser` iterator. A parser is constructed
//! either via the `::new()` function or a `Builder`.
//! As part of the constructor, you have to provide
//! the js you want to parse as an `&str`.
//!
//! Once constructed the parser will return a
//! `ProgramPart` for each iteration.
//!
//! A very simple example might look like this
//! ```
//! use ressa::Parser;
//! use resast::prelude::*;
//! fn main() {
//!     let js = "function helloWorld() { alert('Hello world'); }";
//!     let p = Parser::new(&js).unwrap();
//!     let f = ProgramPart::decl(
//!         Decl::Func(
//!             Func {
//!                 id: Some(Ident::from("helloWorld")),
//!                 params: Vec::new(),
//!                 body: FuncBody(
//!                     vec![
//!                         ProgramPart::Stmt(
//!                             Stmt::Expr(
//!                                 Expr::Call(
//!                                     CallExpr {
//!                                         callee: Box::new(
//!                                             Expr::ident_from("alert")
//!                                         ),
//!                                         arguments: vec![
//!                                             Expr::Lit(
//!                                                 Lit::single_string_from("Hello world")
//!                                             )
//!                                         ],
//!                                     }
//!                                 )
//!                             )
//!                         )
//!                     ]
//!                 ),
//!                 generator: false,
//!                 is_async: false,
//!             }
//!         )
//!     );
//!     for part in p {
//!         // assert_eq!(part.unwrap(), f);
//!     }
//! }
//!```
//! checkout the `examples` folders for slightly larger
//! examples.
//!
extern crate ress;
#[macro_use]
extern crate log;
extern crate backtrace;

use ress::prelude::*;
pub use ress::Span;

mod comment_handler;
mod error;

pub use crate::comment_handler::CommentHandler;
pub use crate::comment_handler::DefaultCommentHandler;
pub use crate::error::Error;
use resast::prelude::*;
use resast::ClassBody;
use std::{collections::HashSet, mem::replace};

/// The current configuration options.
/// This will most likely increase over time
struct Config {
    /// whether or not to tolerate a subset of errors
    tolerant: bool,
}

/// The current parsing context.
/// This structure holds the relevant
/// information to know when some
/// text might behave differently
/// depending on what has come before it
struct Context<'a> {
    /// If the current JS should be treated
    /// as a JS module
    is_module: bool,
    /// If `in` is allowed as an identifier
    allow_in: bool,
    /// If a strict directive is allowed
    allow_strict_directive: bool,
    /// If `yield` is allowed as an identifier
    allow_yield: bool,
    /// If await is allowed as an identifier
    allow_await: bool,
    /// If we have found any possible naming errors
    /// which are not yet resolved
    first_covert_initialized_name_error: Option<Item<Token<&'a str>>>,
    /// If the current expressions is an assignment target
    is_assignment_target: bool,
    /// If the current expression is a binding element
    is_binding_element: bool,
    /// If we have entered a function body
    in_function_body: bool,
    /// If we have entered a loop block
    in_iteration: bool,
    /// If we have entered a switch block
    in_switch: bool,
    /// The currently known labels, this applies
    /// to labels only, not all identifiers. Errors
    /// at that level would need to be handled by
    /// the calling scope
    label_set: HashSet<&'a str>,
    /// If the current scope has a `'use strict';` directive
    /// in the prelude
    strict: bool,
    /// If the scanner has a pending line terminator
    /// before the next token
    has_line_term: bool,
    /// If we have passed the initial prelude where a valid
    /// `'use strict'` directive would exist
    past_prolog: bool,
    /// If we encounter an error, the iterator should stop
    errored: bool,
}

impl Default for Config {
    fn default() -> Self {
        trace!("default config");
        Self { tolerant: false }
    }
}

impl<'a> Default for Context<'a> {
    fn default() -> Self {
        trace!("default context",);
        Self {
            is_module: false,
            allow_await: false,
            allow_in: true,
            allow_strict_directive: true,
            allow_yield: true,
            first_covert_initialized_name_error: None,
            is_assignment_target: false,
            is_binding_element: false,
            in_function_body: false,
            in_iteration: false,
            in_switch: false,
            label_set: HashSet::new(),
            strict: false,
            has_line_term: false,
            past_prolog: false,
            errored: false,
        }
    }
}
/// This is used to create a `Parser` using
/// the builder method
#[derive(Default)]
pub struct Builder<'b> {
    tolerant: bool,
    is_module: bool,
    js: &'b str,
}

impl<'b> Builder<'b> {
    pub fn new() -> Self {
        Self::default()
    }
    /// Enable or disable error tolerance
    /// default: `false`
    pub fn set_tolerant(&mut self, value: bool) {
        self.tolerant = value;
    }
    /// Enable or disable error tolerance with a builder
    /// pattern
    /// default: `false`
    pub fn tolerant(mut self, value: bool) -> Self {
        self.set_tolerant(value);
        self
    }
    /// Set the parsing context to module or script
    /// default: `false` (script)
    pub fn set_module(&mut self, value: bool) {
        self.is_module = value;
    }
    /// Set the parsing context to module or script
    /// with a builder pattern
    /// default: `false` (script)
    pub fn module(mut self, value: bool) -> Self {
        self.set_module(value);
        self
    }
    /// Set the js text that this parser would operate
    /// on
    pub fn set_js(&mut self, js: &'b str) {
        self.js = js;
    }
    /// Set the js text that this parser would operate
    /// on with a builder pattern
    pub fn js(mut self, js: &'b str) -> Self {
        self.set_js(js);
        self
    }
    /// Complete the builder pattern returning
    /// `Result<Parser, Error>`
    pub fn build(self) -> Res<Parser<'b, DefaultCommentHandler>> {
        let is_module = self.is_module;
        let tolerant = self.tolerant;
        let scanner = Scanner::new(self.js);
        Parser::build(tolerant, is_module, scanner, DefaultCommentHandler, self.js)
    }
}

impl<'b> Builder<'b> {
    pub fn with_comment_handler<CH>(self, handler: CH) -> Res<Parser<'b, CH>>
    where
        CH: CommentHandler<'b>,
    {
        let is_module = self.is_module;
        let tolerant = self.tolerant;
        let scanner = Scanner::new(self.js);
        Parser::build(tolerant, is_module, scanner, handler, self.js)
    }
}

/// This is the primary interface that you would interact with.
/// There are two main ways to use it, the first is to utilize
/// the `Iterator` implementation. Each iteration will return
/// a `Result<ProgramPart, Error>`.
/// The other option is to use the `parse` method, which is just
/// a wrapper around the `collect` method on `Iterator`, however
/// the final result will be a `Result<Program, Error>` and the
/// `ProgramPart` collection will be the inner data. Since modern
/// js allows for both `Module`s as well as `Script`s, these will be
/// the two `enum` variants.
pub struct Parser<'a, CH> {
    /// The current parsing context
    context: Context<'a>,
    /// The configuration provided by the user
    config: Config,
    /// The internal scanner (see the
    /// `ress` crate for more details)
    scanner: Scanner<'a>,
    /// The next item,
    look_ahead: Item<Token<&'a str>>,
    /// Since we are looking ahead, we need
    /// to make sure we don't miss the eof
    /// by using this flag
    found_eof: bool,
    /// a possible container for tokens, currently
    /// it is unused
    _tokens: Vec<Item<Token<&'a str>>>,
    /// a possible container for comments, currently
    /// it is unused
    _comments: Vec<Item<Token<&'a str>>>,
    /// The current position we are parsing
    current_position: Position,
    look_ahead_position: Position,
    /// To ease debugging this will be a String representation
    /// of the look_ahead token, it will be an empty string
    /// unless you are using the `debug_look_ahead` feature
    _look_ahead: String,

    pub comment_handler: CH,
    original: &'a str,
}
/// The start/end index of a line
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct Line {
    start: usize,
    end: usize,
}
/// The result type for the Parser operations
type Res<T> = Result<T, Error>;

impl<'a> Parser<'a, DefaultCommentHandler> {
    /// Create a new parser with the provided
    /// javascript
    /// This will default to parsing in the
    /// script context and discard comments.
    /// If you wanted change this behavior
    /// utilize the `Builder` pattern
    pub fn new(text: &'a str) -> Res<Self> {
        let s = Scanner::new(text);
        let config = Config::default();
        let context = Context::default();
        Self::_new(s, config, context, DefaultCommentHandler, text)
    }
}

impl<'a> Parser<'a, ()> {
    pub fn builder() -> Builder<'a> {
        Builder::new()
    }
}

impl<'b, CH> Parser<'b, CH>
where
    CH: CommentHandler<'b> + Sized,
{
    /// Internal constructor for completing the builder pattern
    pub fn build(
        tolerant: bool,
        is_module: bool,
        scanner: Scanner<'b>,
        comment_handler: CH,
        original: &'b str,
    ) -> Res<Self> {
        let config = Config { tolerant };
        let context = Context {
            is_module,
            ..Default::default()
        };
        Self::_new(scanner, config, context, comment_handler, original)
    }
    /// Internal constructor to allow for both builder pattern
    /// and `new` construction
    fn _new(
        scanner: Scanner<'b>,
        config: Config,
        context: Context<'b>,
        comment_handler: CH,
        original: &'b str,
    ) -> Res<Self> {
        let look_ahead = Item {
            token: Token::EoF,
            span: Span { start: 0, end: 0 },
            location: SourceLocation::new(Position::new(0, 0), Position::new(0, 0)),
        };
        let mut ret = Self {
            scanner,
            look_ahead,
            found_eof: false,
            config,
            context,
            _tokens: Vec::new(),
            _comments: Vec::new(),
            current_position: Position { line: 1, column: 0 },
            look_ahead_position: Position { line: 1, column: 0 },
            _look_ahead: String::new(),
            comment_handler,
            original,
        };
        let _ = ret.next_item()?;
        Ok(ret)
    }
    /// Wrapper around the `Iterator` implementation for
    /// Parser
    /// ```
    /// extern crate ressa;
    /// use ressa::Parser;
    /// use resast::prelude::*;
    /// fn main() {
    ///     let js = "function helloWorld() { alert('Hello world'); }";
    ///     let mut p = Parser::new(&js).unwrap();
    ///     let call = CallExpr {
    ///         callee: Box::new(Expr::ident_from("alert")),
    ///         arguments: vec![Expr::Lit(Lit::single_string_from("Hello world"))],
    ///     };
    ///     let expectation = Program::Script(vec![ProgramPart::Decl(Decl::Func(Func {
    ///         id: Some(Ident::from("helloWorld")),
    ///         params: Vec::new(),
    ///         body: FuncBody(vec![ProgramPart::Stmt(Stmt::Expr(Expr::Call(call)))]),
    ///         generator: false,
    ///         is_async: false,
    ///     }))]);
    ///     let program = p.parse().unwrap();
    ///     //assert_eq!(program, expectation);
    /// }
    /// ```
    pub fn parse(&mut self) -> Res<Program> {
        debug!("{}: parse_script", self.look_ahead.span.start);
        let mut body = Vec::new();
        while let Some(part) = self.next() {
            match part {
                Ok(part) => body.push(part),
                Err(e) => return Err(e),
            }
        }
        Ok(if self.context.is_module {
            Program::Mod(body)
        } else {
            Program::Script(body)
        })
    }
    /// Parse all of the directives into a single prologue
    #[inline]
    fn parse_directive_prologues(&mut self) -> Res<Vec<ProgramPart<'b>>> {
        debug!("{}: parse_directive_prologues", self.look_ahead.span.start);
        let mut ret = Vec::new();
        loop {
            if !self.look_ahead.token.is_string() {
                break;
            }
            ret.push(self.parse_directive()?);
        }
        Ok(ret)
    }
    /// Parse a single directive
    #[inline]
    fn parse_directive(&mut self) -> Res<ProgramPart<'b>> {
        debug!("{}: parse_directive", self.look_ahead.span.start);
        let orig = self.look_ahead.clone();
        let expr = self.parse_expression()?;
        if let Expr::Lit(lit) = expr {
            if let Lit::String(s) = lit {
                self.context.strict = s.inner_matches("use strict");
                if !self.context.allow_strict_directive && self.context.strict {
                    return self.unexpected_token_error(&orig, "use strict in an invalid location");
                }
                self.consume_semicolon()?;
                Ok(ProgramPart::Dir(Dir {
                    dir: s.clone_inner(),
                    expr: Lit::String(s),
                }))
            } else {
                Ok(ProgramPart::Stmt(Stmt::Expr(Expr::Lit(lit))))
            }
        } else {
            let stmt = ProgramPart::Stmt(Stmt::Expr(expr));
            self.consume_semicolon()?;
            Ok(stmt)
        }
    }
    /// This is where we will begin our recursive decent. First
    /// we check to see if we are at at token that is a known
    /// statement or declaration (import/export/function/const/let/class)
    /// otherwise we move on to `Parser::parse_statement`
    #[inline]
    fn parse_statement_list_item(&mut self) -> Res<ProgramPart<'b>> {
        debug!(
            "{}: parse_statement_list_item_script",
            self.look_ahead.span.start
        );
        self.context.is_assignment_target = true;
        self.context.is_binding_element = true;
        let tok = self.look_ahead.token.clone();
        match &tok {
            Token::Keyword(ref k) => match k {
                Keyword::Import => {
                    if self.at_import_call() {
                        let stmt = self.parse_statement()?;
                        Ok(ProgramPart::Stmt(stmt))
                    } else {
                        if !self.context.is_module {
                            return Err(Error::UseOfModuleFeatureOutsideOfModule(
                                self.current_position,
                                "es6 import syntax".to_string(),
                            ));
                        }
                        let import = self.parse_import_decl()?;
                        let decl = Decl::Import(Box::new(import));
                        Ok(ProgramPart::Decl(decl))
                    }
                }
                Keyword::Export => {
                    let export = self.parse_export_decl()?;
                    let decl = Decl::Export(Box::new(export));
                    Ok(ProgramPart::Decl(decl))
                }
                Keyword::Const => {
                    let decl = self.parse_lexical_decl(false)?;
                    Ok(ProgramPart::Decl(decl))
                }
                Keyword::Function => {
                    let func = self.parse_function_decl(true)?;
                    let decl = Decl::Func(func);
                    Ok(ProgramPart::Decl(decl))
                }
                Keyword::Class => {
                    let class = self.parse_class_decl(false)?;
                    let decl = Decl::Class(class);
                    Ok(ProgramPart::Decl(decl))
                }
                Keyword::Let => Ok(if self.at_lexical_decl() {
                    let decl = self.parse_lexical_decl(false)?;
                    ProgramPart::Decl(decl)
                } else {
                    let stmt = self.parse_statement()?;
                    ProgramPart::Stmt(stmt)
                }),
                Keyword::Var => {
                    let _var = self.next_item()?;
                    let decls = self.parse_var_decl_list(false)?;
                    self.consume_semicolon()?;
                    Ok(ProgramPart::Decl(Decl::Var(VarKind::Var, decls)))
                }
                _ => {
                    let stmt = self.parse_statement()?;
                    Ok(ProgramPart::Stmt(stmt))
                }
            },
            _ => {
                let stmt = self.parse_statement()?;
                Ok(ProgramPart::Stmt(stmt))
            }
        }
    }
    /// This will cover all possible import statements supported
    /// ```js
    /// import * as Stuff from 'place'; //namespace
    /// import Thing from 'place'; //default
    /// import {Thing} from 'place'; //named
    /// import Person, {Thing} from 'place';// default + named
    /// import Thing, * as Stuff from 'place';
    /// import 'place';
    /// ```
    #[inline]
    fn parse_import_decl(&mut self) -> Res<ModImport<'b>> {
        if self.context.in_function_body {
            return Err(Error::InvalidImportError(self.current_position));
        }
        self.expect_keyword(Keyword::Import)?;
        // if the next toke is a string we are at an import
        // with not specifiers
        if self.look_ahead.is_string() {
            let source = self.parse_module_specifier()?;
            self.consume_semicolon()?;
            Ok(ModImport {
                specifiers: Vec::new(),
                source,
            })
        } else {
            // If we are at an open brace, this is the named
            //variant
            let specifiers = if self.at_punct(Punct::OpenBrace) {
                self.parse_named_imports()?
            // If we are at ta *, this is the namespace variant
            } else if self.at_punct(Punct::Asterisk) {
                vec![self.parse_import_namespace_specifier()?]
            // if we are at an identifier that is not `default` this is the default variant
            } else if self.at_possible_ident() && !self.at_keyword(Keyword::Default) {
                let mut specifiers = vec![self.parse_import_default_specifier()?];
                // we we find a comma, this will be more complicated than just 1 item
                if self.at_punct(Punct::Comma) {
                    let _ = self.next_item()?;
                    // if we find a `*`, we need to add the namespace variant to the
                    // specifiers
                    if self.at_punct(Punct::Asterisk) {
                        specifiers.push(self.parse_import_namespace_specifier()?);
                    // if we find an `{` we need to extend the specifiers
                    // with the named variants
                    } else if self.at_punct(Punct::OpenBrace) {
                        specifiers.append(&mut self.parse_named_imports()?);
                    } else {
                        // A comma not followed by `{` or `*` is an error
                        return self.expected_token_error(&self.look_ahead, &["{", "*"]);
                    }
                }
                specifiers
            // import must be followed by an `{`, `*`, `identifier`, or `string`
            } else {
                return self
                    .expected_token_error(&self.look_ahead, &["{", "*", "[ident]", "[string]"]);
            };
            // Import declarations require the contextual keyword
            // `from`
            if !self.at_contextual_keyword("from") {
                return self.expected_token_error(&self.look_ahead, &["from"]);
            }
            let _ = self.next_item()?;
            // capture the source string for where this import
            // comes from
            let source = self.parse_module_specifier()?;
            self.consume_semicolon()?;
            Ok(ModImport { specifiers, source })
        }
    }
    /// This will handle the named variant of imports
    /// ```js
    /// import {Thing} from 'place';
    /// ```
    #[inline]
    fn parse_named_imports(&mut self) -> Res<Vec<ImportSpecifier<'b>>> {
        self.expect_punct(Punct::OpenBrace)?;
        let mut ret = Vec::new();
        while !self.at_punct(Punct::CloseBrace) {
            ret.push(self.parse_import_specifier()?);
            if !self.at_punct(Punct::CloseBrace) {
                self.expect_punct(Punct::Comma)?;
            }
        }
        self.expect_punct(Punct::CloseBrace)?;
        Ok(ret)
    }

    #[inline]
    fn parse_import_specifier(&mut self) -> Res<ImportSpecifier<'b>> {
        let (imported, local) = if self.look_ahead.token.is_ident() {
            let imported = self.parse_var_ident(false)?;
            let local = if self.at_contextual_keyword("as") {
                let _ = self.next_item();
                self.parse_var_ident(false)?
            } else {
                imported.clone()
            };
            (imported, local)
        } else {
            let imported = self.parse_ident_name()?;
            let local = if self.at_contextual_keyword("as") {
                let _ = self.next_item()?;
                self.parse_var_ident(false)?
            } else {
                imported.clone()
            };
            (imported, local)
        };
        Ok(ImportSpecifier::Normal(NormalImportSpec {
            imported,
            local,
        }))
    }

    #[inline]
    fn parse_import_namespace_specifier(&mut self) -> Res<ImportSpecifier<'b>> {
        self.expect_punct(Punct::Asterisk)?;
        if !self.at_contextual_keyword("as") {
            return self.expected_token_error(&self.look_ahead, &["as"]);
        }
        let _ = self.next_item()?;
        let ident = self.parse_ident_name()?;
        Ok(ImportSpecifier::Namespace(ident))
    }

    #[inline]
    fn parse_import_default_specifier(&mut self) -> Res<ImportSpecifier<'b>> {
        let ident = self.parse_ident_name()?;
        Ok(ImportSpecifier::Default(ident))
    }

    #[inline]
    fn parse_export_decl(&mut self) -> Res<ModExport<'b>> {
        if self.context.in_function_body {
            return Err(Error::InvalidExportError(self.current_position));
        }
        if !self.context.is_module {
            return Err(Error::UseOfModuleFeatureOutsideOfModule(self.current_position, "export syntax".to_string()))
        }
        self.expect_keyword(Keyword::Export)?;
        if self.at_keyword(Keyword::Default) {
            let _ = self.next_item()?;
            let decl = if self.at_keyword(Keyword::Function) {
                let func = Decl::Func(self.parse_function_decl(true)?);
                DefaultExportDecl::Decl(func)
            } else if self.at_keyword(Keyword::Class) {
                let class = Decl::Class(self.parse_class_decl(true)?);
                DefaultExportDecl::Decl(class)
            } else if self.at_contextual_keyword("async") {
                if self.at_async_function() {
                    let func = self.parse_function_decl(true)?;
                    let decl = Decl::Func(func);
                    DefaultExportDecl::Decl(decl)
                } else {
                    let expr = self.parse_assignment_expr()?;
                    DefaultExportDecl::Expr(expr)
                }
            } else {
                if self.at_contextual_keyword("from") {
                    return Err(Error::InvalidUseOfContextualKeyword(
                        self.current_position,
                        "from".to_string(),
                    ));
                }
                if self.at_punct(Punct::OpenBrace) {
                    let expr = self.parse_obj_init()?;
                    DefaultExportDecl::Expr(expr)
                } else if self.at_punct(Punct::OpenBracket) {
                    let expr = self.parse_array_init()?;
                    DefaultExportDecl::Expr(expr)
                } else {
                    let expr = self.parse_assignment_expr()?;
                    DefaultExportDecl::Expr(expr)
                }
            };
            Ok(ModExport::Default(decl))
        } else if self.at_punct(Punct::Asterisk) {
            let _ = self.next_item()?;
            if !self.at_contextual_keyword("from") {
                return Err(Error::InvalidUseOfContextualKeyword(
                    self.current_position,
                    "from".to_string(),
                ));
            }
            let _ = self.next_item()?;
            let source = self.parse_module_specifier()?;
            self.consume_semicolon()?;
            Ok(ModExport::All(source))
        } else if self.look_ahead.token.is_keyword() {
            if self.look_ahead.token.matches_keyword(Keyword::Let)
                || self.look_ahead.token.matches_keyword(Keyword::Const)
            {
                let lex = self.parse_lexical_decl(false)?;
                let decl = NamedExportDecl::Decl(lex);
                self.consume_semicolon()?;
                Ok(ModExport::Named(decl))
            } else if self.look_ahead.token.matches_keyword(Keyword::Var) {
                let _ = self.next_item()?;
                let var = Decl::Var(VarKind::Var, self.parse_variable_decl_list(false)?);
                let decl = NamedExportDecl::Decl(var);
                self.consume_semicolon()?;
                Ok(ModExport::Named(decl))
            } else if self.look_ahead.token.matches_keyword(Keyword::Class) {
                let class = self.parse_class_decl(true)?;
                let decl = Decl::Class(class);
                let decl = NamedExportDecl::Decl(decl);
                Ok(ModExport::Named(decl))
            } else if self.look_ahead.token.matches_keyword(Keyword::Function) {
                let func = self.parse_function_decl(true)?;
                let decl = Decl::Func(func);
                let decl = NamedExportDecl::Decl(decl);
                Ok(ModExport::Named(decl))
            } else {
                self.expected_token_error(
                    &self.look_ahead,
                    &["let", "var", "const", "class", "function"],
                )
            }
        } else if self.at_async_function() {
            let func = self.parse_function_decl(false)?;
            let decl = Decl::Func(func);
            let decl = NamedExportDecl::Decl(decl);
            Ok(ModExport::Named(decl))
        } else {
            self.expect_punct(Punct::OpenBrace)?;
            let mut specifiers = Vec::new();
            let mut found_default = false;
            while !self.at_punct(Punct::CloseBrace) {
                if self.at_keyword(Keyword::Default) {
                    found_default = true;
                }
                specifiers.push(self.parse_export_specifier()?);
                if !self.at_punct(Punct::CloseBrace) {
                    self.expect_punct(Punct::Comma)?;
                }
            }
            let _ = self.next_item()?;
            if self.at_contextual_keyword("from") {
                let _ = self.next_item()?;
                let source = self.parse_module_specifier()?;
                self.consume_semicolon()?;
                let decl = NamedExportDecl::Specifier(specifiers, Some(source));
                Ok(ModExport::Named(decl))
            } else if found_default {
                self.expected_token_error(&self.look_ahead, &[""])
            } else {
                self.consume_semicolon()?;
                let decl = NamedExportDecl::Specifier(specifiers, None);
                Ok(ModExport::Named(decl))
            }
        }
    }

    #[inline]
    fn parse_export_specifier(&mut self) -> Res<ExportSpecifier<'b>> {
        let local = self.parse_ident_name()?;
        let exported = if self.at_contextual_keyword("as") {
            let _ = self.next_item()?;
            self.parse_ident_name()?
        } else {
            local.clone()
        };
        Ok(ExportSpecifier { local, exported })
    }

    #[inline]
    fn parse_module_specifier(&mut self) -> Res<Lit<'b>> {
        let item = self.next_item()?;
        match &item.token {
            Token::String(ref sl) => Ok(match sl {
                ress::prelude::StringLit::Double(ref s) => {
                    resast::prelude::Lit::double_string_from(s)
                }
                ress::prelude::StringLit::Single(ref s) => {
                    resast::prelude::Lit::single_string_from(s)
                }
            }),
            _ => self.expected_token_error(&item, &["[string]"]),
        }
    }

    #[inline]
    fn parse_statement(&mut self) -> Res<Stmt<'b>> {
        debug!("{}: parse_statement", self.look_ahead.span.start);
        let lh = self.look_ahead.token.clone();
        let stmt = match lh {
            Token::Boolean(_)
            | Token::Null
            | Token::Number(_)
            | Token::String(_)
            | Token::RegEx(_)
            | Token::Template(_) => {
                let expr = self.parse_expression_statement()?;
                Stmt::Expr(expr)
            }
            Token::Punct(ref p) => match p {
                Punct::OpenBrace => {
                    let b = self.parse_block()?;
                    Stmt::Block(b)
                }
                Punct::OpenParen => {
                    let expr = self.parse_expression_statement()?;
                    Stmt::Expr(expr)
                }
                Punct::SemiColon => {
                    let _ = self.next_item()?;
                    Stmt::Empty
                }
                _ => {
                    let expr = self.parse_expression_statement()?;
                    Stmt::Expr(expr)
                }
            },
            Token::Ident(_) => {
                if self.at_async_function() {
                    let f = self.parse_function_decl(true)?;
                    Stmt::Expr(Expr::Func(f))
                } else {
                    self.parse_labelled_statement()?
                }
            }
            Token::Keyword(ref k) => match k {
                Keyword::Break => Stmt::Break(self.parse_break_stmt()?),
                Keyword::Continue => Stmt::Continue(self.parse_continue_stmt()?),
                Keyword::Debugger => self.parse_debugger_stmt()?,
                Keyword::Do => Stmt::DoWhile(self.parse_do_while_stmt()?),
                Keyword::For => self.parse_for_stmt()?,
                Keyword::Function => Stmt::Expr(self.parse_fn_stmt()?),
                Keyword::If => Stmt::If(self.parse_if_stmt()?),
                Keyword::Return => Stmt::Return(self.parse_return_stmt()?),
                Keyword::Switch => Stmt::Switch(self.parse_switch_stmt()?),
                Keyword::Throw => Stmt::Throw(self.parse_throw_stmt()?),
                Keyword::Try => Stmt::Try(self.parse_try_stmt()?),
                Keyword::Var => self.parse_var_stmt()?,
                Keyword::While => Stmt::While(self.parse_while_stmt()?),
                Keyword::With => Stmt::With(self.parse_with_stmt()?),
                _ => Stmt::Expr(self.parse_expression_statement()?),
            },
            _ => return self.expected_token_error(&self.look_ahead, &[]),
        };
        Ok(stmt)
    }

    #[inline]
    fn parse_with_stmt(&mut self) -> Res<WithStmt<'b>> {
        debug!("{}: parse_with_stmt", self.look_ahead.span.start);
        if self.context.strict {
            self.tolerate_error(Error::NonStrictFeatureInStrictContext(
                self.current_position,
                "with statements".to_string(),
            ))?;
        }
        self.expect_keyword(Keyword::With)?;
        self.expect_punct(Punct::OpenParen)?;
        let obj = self.parse_expression()?;
        Ok(if !self.at_punct(Punct::CloseParen) {
            if !self.config.tolerant {
                self.expected_token_error(&self.look_ahead, &[")"])?;
            }
            WithStmt {
                object: obj,
                body: Box::new(Stmt::Empty),
            }
        } else {
            self.expect_punct(Punct::CloseParen)?;
            WithStmt {
                object: obj,
                body: Box::new(self.parse_statement()?),
            }
        })
    }

    #[inline]
    fn parse_while_stmt(&mut self) -> Res<WhileStmt<'b>> {
        debug!("{}: parse_while_stmt", self.look_ahead.span.start);
        self.expect_keyword(Keyword::While)?;
        self.expect_punct(Punct::OpenParen)?;
        let test = self.parse_expression()?;
        let body = if !self.at_punct(Punct::CloseParen) {
            if !self.config.tolerant {
                return self.expected_token_error(&self.look_ahead, &[")"]);
            }
            Stmt::Empty
        } else {
            self.expect_punct(Punct::CloseParen)?;
            let prev_iter = self.context.in_iteration;
            self.context.in_iteration = true;
            let body = self.parse_statement()?;
            self.context.in_iteration = prev_iter;
            body
        };
        Ok(WhileStmt {
            test,
            body: Box::new(body),
        })
    }

    #[inline]
    fn parse_var_stmt(&mut self) -> Res<Stmt<'b>> {
        debug!("{}: parse_var_stmt", self.look_ahead.span.start);
        self.expect_keyword(Keyword::Var)?;
        let decls = self.parse_var_decl_list(false)?;

        let stmt = Stmt::Var(decls);
        self.consume_semicolon()?;
        Ok(stmt)
    }

    #[inline]
    fn parse_var_decl_list(&mut self, in_for: bool) -> Res<Vec<VarDecl<'b>>> {
        let mut ret = vec![self.parse_var_decl(in_for)?];
        while self.at_punct(Punct::Comma) {
            let _ = self.next_item()?;
            ret.push(self.parse_var_decl(in_for)?)
        }
        Ok(ret)
    }

    #[inline]
    fn parse_var_decl(&mut self, in_for: bool) -> Res<VarDecl<'b>> {
        let (_, patt) = self.parse_pattern(Some(VarKind::Var), &mut Vec::new())?;
        if self.context.strict && Self::is_restricted(&patt) {
            let patt = match patt {
                Pat::Ident(ident) => ident.name,
                _ => unreachable!(
                    "restricted patterns should only be reachable by identifer patterns"
                ),
            };
            return Err(Error::NonStrictFeatureInStrictContext(
                self.current_position,
                format!("{} as an identifier", patt),
            ));
        }
        let init = if self.at_punct(Punct::Equal) {
            let _ = self.next_item()?;
            let (prev_bind, prev_assign, prev_first) = self.isolate_cover_grammar();
            let init = self.parse_assignment_expr()?;
            self.set_isolate_cover_grammar_state(prev_bind, prev_assign, prev_first)?;
            Some(init)
        } else if !Self::is_pat_ident(&patt) && !in_for {
            return self.expected_token_error(&self.look_ahead, &["="]);
        } else {
            None
        };
        Ok(VarDecl { id: patt, init })
    }

    #[inline]
    fn parse_try_stmt(&mut self) -> Res<TryStmt<'b>> {
        debug!("{}: parse_try_stmt", self.look_ahead.span.start);
        self.expect_keyword(Keyword::Try)?;
        let block = self.parse_block()?;
        let handler = if self.at_keyword(Keyword::Catch) {
            Some(self.parse_catch_clause()?)
        } else {
            None
        };
        let finalizer = if self.at_keyword(Keyword::Finally) {
            Some(self.parse_finally_clause()?)
        } else {
            None
        };
        if handler.is_none() && finalizer.is_none() {
            return Err(Error::TryWithNoCatchOrFinally(self.current_position));
        }
        Ok(TryStmt {
            block,
            handler,
            finalizer,
        })
    }

    #[inline]
    fn parse_catch_clause(&mut self) -> Res<CatchClause<'b>> {
        debug!("{}: parse_catch_clause", self.look_ahead.span.start);
        self.expect_keyword(Keyword::Catch)?;
        let param = if self.at_punct(Punct::OpenParen) {
            self.expect_punct(Punct::OpenParen)?;
            if self.at_punct(Punct::CloseParen) {
                return Err(Error::InvalidCatchArg(self.current_position));
            }
            let mut params = Vec::new();
            let (_, param) = self.parse_pattern(None, &mut params)?;
            if !self.at_punct(Punct::CloseParen) {
                return Err(Error::InvalidCatchArg(self.current_position));
            }
            self.expect_punct(Punct::CloseParen)?;
            Some(param)
        } else {
            None
        };
        let body = self.parse_block()?;
        Ok(CatchClause { param, body })
    }

    #[inline]
    fn parse_finally_clause(&mut self) -> Res<BlockStmt<'b>> {
        debug!("{}: parse_finally_clause", self.look_ahead.span.start);
        self.expect_keyword(Keyword::Finally)?;
        self.parse_block()
    }

    #[inline]
    fn parse_throw_stmt(&mut self) -> Res<Expr<'b>> {
        debug!("{}: parse_throw_stmt", self.look_ahead.span.start);
        self.expect_keyword(Keyword::Throw)?;
        if self.context.has_line_term || self.at_punct(Punct::SemiColon) {
            return Err(Error::ThrowWithNoArg(self.current_position));
        }
        let arg = self.parse_expression()?;
        self.consume_semicolon()?;
        Ok(arg)
    }

    #[inline]
    fn parse_switch_stmt(&mut self) -> Res<SwitchStmt<'b>> {
        debug!("{}: parse_switch_stmt", self.look_ahead.span.start);
        self.expect_keyword(Keyword::Switch)?;
        self.expect_punct(Punct::OpenParen)?;
        let discriminant = self.parse_expression()?;
        self.expect_punct(Punct::CloseParen)?;
        self.expect_punct(Punct::OpenBrace)?;

        let prev_sw = self.context.in_switch;
        self.context.in_switch = true;
        let mut found_default = false;
        let mut cases = Vec::new();
        loop {
            if self.at_punct(Punct::CloseBrace) {
                break;
            }
            let case = self.parse_switch_case()?;
            if case.test.is_none() {
                if found_default {
                    return self.expected_token_error(&self.look_ahead, &[]);
                }
                found_default = true;
            }
            cases.push(case);
        }
        self.expect_punct(Punct::CloseBrace)?;
        self.context.in_switch = prev_sw;
        Ok(SwitchStmt {
            discriminant,
            cases,
        })
    }

    #[inline]
    fn parse_switch_case(&mut self) -> Res<SwitchCase<'b>> {
        debug!("{}: parse_switch_case", self.look_ahead.span.start);
        let test = if self.at_keyword(Keyword::Default) {
            self.expect_keyword(Keyword::Default)?;
            None
        } else {
            self.expect_keyword(Keyword::Case)?;
            Some(self.parse_expression()?)
        };
        self.expect_punct(Punct::Colon)?;
        let mut consequent = Vec::new();
        loop {
            if self.at_punct(Punct::CloseBrace)
                || self.at_keyword(Keyword::Default)
                || self.at_keyword(Keyword::Case)
            {
                break;
            }
            consequent.push(self.parse_statement_list_item()?)
        }
        Ok(SwitchCase { test, consequent })
    }

    #[inline]
    fn parse_return_stmt(&mut self) -> Res<Option<Expr<'b>>> {
        debug!("{}: parse_return_stmt", self.look_ahead.span.start);
        if !self.context.in_function_body {
            return self
                .unexpected_token_error(&self.look_ahead, "cannot return in the global context");
        }
        self.expect_keyword(Keyword::Return)?;
        // if we are at a semi-colon,or close curly brace or eof
        //the return doesn't have an arg. If we are at a line term
        //we need to account for a string Lit or template Lit
        //since they both can have new lines

        let ret = if self.at_return_arg() {
            Some(self.parse_expression()?)
        } else {
            None
        };
        debug!("return statement: {:?} {}", ret, self.context.allow_yield);
        self.consume_semicolon()?;
        Ok(ret)
    }

    #[inline]
    fn parse_if_stmt(&mut self) -> Res<IfStmt<'b>> {
        debug!("{}: parse_if_stmt", self.look_ahead.span.start);
        self.expect_keyword(Keyword::If)?;
        self.expect_punct(Punct::OpenParen)?;
        let test = self.parse_expression()?;
        let (consequent, alternate) = if !self.at_punct(Punct::CloseParen) {
            if !self.config.tolerant {
                return self.expected_token_error(&self.look_ahead, &[")"]);
            }
            (Box::new(Stmt::Empty), None)
        } else {
            self.expect_punct(Punct::CloseParen)?;
            let c = self.parse_if_clause()?;
            let a = if self.at_keyword(Keyword::Else) {
                let _ = self.next_item()?;
                Some(Box::new(self.parse_if_clause()?))
            } else {
                None
            };
            (Box::new(c), a)
        };
        Ok(IfStmt {
            test,
            consequent,
            alternate,
        })
    }

    #[inline]
    fn parse_if_clause(&mut self) -> Res<Stmt<'b>> {
        debug!("{}: parse_if_clause", self.look_ahead.span.start);
        if self.context.strict && self.at_keyword(Keyword::Function) && !self.config.tolerant {
            return self.unexpected_token_error(&self.look_ahead, "");
        }
        self.parse_statement()
    }

    #[inline]
    fn parse_fn_stmt(&mut self) -> Res<Expr<'b>> {
        debug!("{}: parse_fn_stmt", self.look_ahead.span.start);
        let decl = self.parse_function_decl(true)?;
        Ok(Expr::Func(decl))
    }

    #[inline]
    fn parse_for_stmt(&mut self) -> Res<Stmt<'b>> {
        debug!("{}: parse_for_stmt", self.look_ahead.span.start);

        self.expect_keyword(Keyword::For)?;
        let is_await = if self.at_keyword(Keyword::Await) {
            let _ = self.next_item()?;
            true
        // for await ([lookahead ≠ let] LeftHandSideExpression [?Yield, ?Await] of AssignExpression [+In, ?Yield, ?Await]) Statement [?Yield, ?Await, ?Return]
        // for await (var ForBinding [?Yield, ?Await] of AssignExpression [+In, ?Yield, ?Await]) Statement [?Yield, ?Await, ?Return]
        // for await (ForDeclaration [?Yield, ?Await] of AssignExpression [+In, ?Yield, ?Await]) Statement[?Yield, ?Await, ?Return]
        } else {
            false
        };
        self.expect_punct(Punct::OpenParen)?;
        if self.at_punct(Punct::SemiColon) {
            // any semi-colon would mean standard C style for loop
            // for (;;) {}
            let stmt = self.parse_for_loop(VarKind::Var)?;
            return Ok(Stmt::For(stmt));
        }

        if self.at_keyword(Keyword::Var) {
            let _ = self.next_item()?;
            let kind = VarKind::Var;
            let prev_in = self.context.allow_in;
            self.context.allow_in = false;
            let mut bindings = self.parse_variable_decl_list(true)?;
            self.context.allow_in = prev_in;
            if bindings.len() == 1 {
                let decl = if let Some(d) = bindings.pop() {
                    d
                } else {
                    return self.expected_token_error(&self.look_ahead, &["variable decl"]);
                };
                if self.at_keyword(Keyword::In) {
                    let left = LoopLeft::Variable(kind, decl);
                    let stmt = self.parse_for_in_loop(left)?;
                    Ok(Stmt::ForIn(stmt))
                } else if self.at_contextual_keyword("of") {
                    let left = LoopLeft::Variable(kind, decl);
                    let stmt = self.parse_for_of_loop(left, is_await)?;
                    Ok(Stmt::ForOf(stmt))
                } else {
                    let init = LoopInit::Variable(kind, vec![decl]);
                    let stmt = self.parse_for_loop_cont(Some(init))?;
                    Ok(Stmt::For(stmt))
                }
            } else {
                let init = LoopInit::Variable(kind, bindings);
                let stmt = self.parse_for_loop_cont(Some(init))?;
                Ok(Stmt::For(stmt))
            }
        } else if self.at_keyword(Keyword::Const) || self.at_keyword(Keyword::Let) {
            let kind = self.next_item()?;
            let kind = match &kind.token {
                Token::Keyword(ref k) => match k {
                    Keyword::Const => VarKind::Const,
                    Keyword::Let => VarKind::Let,
                    _ => unreachable!(),
                },
                _ => return self.expected_token_error(&kind, &["const", "let"]),
            };
            if !self.context.strict && self.look_ahead.token.matches_keyword(Keyword::In) {
                let _in = self.next_item()?;
                //const or let becomes an ident
                let k = match kind {
                    VarKind::Var => "var",
                    VarKind::Let => "let",
                    VarKind::Const => "const",
                };
                let left = LoopLeft::Expr(Expr::ident_from(k));
                let right = self.parse_expression()?;
                Ok(Stmt::ForIn(ForInStmt {
                    left,
                    right,
                    body: Box::new(self.parse_loop_body()?),
                }))
            } else {
                let prev_in = self.context.allow_in;
                self.context.allow_in = false;
                let mut decls = self.parse_binding_list(kind, true)?;
                self.context.allow_in = prev_in;
                if decls.len() == 1 {
                    let decl = if let Some(d) = decls.pop() {
                        d
                    } else {
                        return self.expected_token_error(&self.look_ahead, &["variable decl"]);
                    };
                    if decl.init.is_none() && self.at_keyword(Keyword::In) {
                        let left = LoopLeft::Variable(kind, decl);
                        let _in = self.next_item()?;
                        let right = self.parse_expression()?;
                        Ok(Stmt::ForIn(ForInStmt {
                            left,
                            right,
                            body: Box::new(self.parse_loop_body()?),
                        }))
                    } else if decl.init.is_none() && self.at_contextual_keyword("of") {
                        let left = LoopLeft::Variable(kind, decl);
                        Ok(Stmt::ForOf(self.parse_for_of_loop(left, is_await)?))
                    } else {
                        let init = LoopInit::Variable(kind, vec![decl]);
                        let stmt = self.parse_for_loop_cont(Some(init))?;
                        Ok(Stmt::For(stmt))
                    }
                } else {
                    let init = LoopInit::Variable(kind, decls);
                    let stmt = self.parse_for_loop_cont(Some(init))?;
                    Ok(Stmt::For(stmt))
                }
            }
        } else {
            let prev_in = self.context.allow_in;
            self.context.allow_in = false;
            let (prev_bind, prev_assign, prev_first) = self.inherit_cover_grammar();
            let init = self.parse_assignment_expr()?;
            self.set_inherit_cover_grammar_state(prev_bind, prev_assign, prev_first);
            self.context.allow_in = prev_in;
            if self.at_keyword(Keyword::In) {
                let _ = self.next_item()?;
                let left = LoopLeft::Expr(init);
                let right = self.parse_expression()?;
                Ok(Stmt::ForIn(ForInStmt {
                    left,
                    right,
                    body: Box::new(self.parse_loop_body()?),
                }))
            } else if self.at_contextual_keyword("of") {
                let _ = self.next_item()?;
                let left = LoopLeft::Expr(init);
                let right = self.parse_assignment_expr()?;
                let body = self.parse_loop_body()?;
                Ok(Stmt::ForOf(ForOfStmt {
                    left,
                    right,
                    body: Box::new(body),
                    is_await,
                }))
            } else {
                let init = if self.at_punct(Punct::Comma) {
                    let mut seq = vec![init];
                    while self.at_punct(Punct::Comma) {
                        let _comma = self.next_item()?;
                        let (prev_bind, prev_assign, prev_first) = self.inherit_cover_grammar();
                        seq.push(self.parse_assignment_expr()?);
                        self.set_inherit_cover_grammar_state(prev_bind, prev_assign, prev_first);
                    }
                    LoopInit::Expr(Expr::Sequence(seq))
                } else {
                    LoopInit::Expr(init)
                };
                Ok(Stmt::For(self.parse_for_loop_cont(Some(init))?))
            }
        }
    }

    #[inline]
    fn parse_for_loop(&mut self, kind: VarKind) -> Res<ForStmt<'b>> {
        debug!("{}: parse_for_loop", self.look_ahead.span.start);
        let init = if self.at_punct(Punct::SemiColon) {
            None
        } else {
            let list = self.parse_variable_decl_list(true)?;
            Some(LoopInit::Variable(kind, list))
        };
        self.parse_for_loop_cont(init)
    }

    #[inline]
    fn parse_for_loop_cont(&mut self, init: Option<LoopInit<'b>>) -> Res<ForStmt<'b>> {
        debug!("{}: parse_for_loop_cont", self.look_ahead.span.start);
        self.expect_punct(Punct::SemiColon)?;
        let test = if self.at_punct(Punct::SemiColon) {
            None
        } else {
            Some(self.parse_expression()?)
        };
        let _d = format!("{:?}", test);
        debug!("{:#?}", test);
        self.expect_punct(Punct::SemiColon)?;
        let update = if self.at_punct(Punct::CloseParen) {
            None
        } else {
            Some(self.parse_expression()?)
        };
        let body = self.parse_loop_body()?;
        Ok(ForStmt {
            init,
            test,
            update,
            body: Box::new(body),
        })
    }

    #[inline]
    fn parse_for_in_loop(&mut self, left: LoopLeft<'b>) -> Res<ForInStmt<'b>> {
        debug!("{}: parse_for_in_loop", self.look_ahead.span.start);
        let _ = self.next_item()?;
        let right = self.parse_expression()?;
        let body = self.parse_loop_body()?;
        Ok(ForInStmt {
            left,
            right,
            body: Box::new(body),
        })
    }

    #[inline]
    fn parse_for_of_loop(&mut self, left: LoopLeft<'b>, is_await: bool) -> Res<ForOfStmt<'b>> {
        debug!("{}: parse_for_of_loop", self.look_ahead.span.start);
        let _ = self.next_item()?;
        let right = self.parse_assignment_expr()?;
        let body = self.parse_loop_body()?;
        Ok(ForOfStmt {
            left,
            right,
            body: Box::new(body),
            is_await,
        })
    }

    #[inline]
    fn parse_loop_body(&mut self) -> Res<Stmt<'b>> {
        debug!("{}: parse_loop_body", self.look_ahead.span.start);
        self.expect_punct(Punct::CloseParen)?;
        let prev_iter = self.context.in_iteration;
        self.context.in_iteration = true;
        let (prev_bind, prev_assign, prev_first) = self.isolate_cover_grammar();
        let ret = self.parse_statement()?;
        self.set_isolate_cover_grammar_state(prev_bind, prev_assign, prev_first)?;
        self.context.in_iteration = prev_iter;
        Ok(ret)
    }

    #[inline]
    fn parse_do_while_stmt(&mut self) -> Res<DoWhileStmt<'b>> {
        debug!("{}: parse_do_while_stmt", self.look_ahead.span.start);
        self.expect_keyword(Keyword::Do)?;
        let prev_iter = self.context.in_iteration;
        self.context.in_iteration = true;
        let body = self.parse_statement()?;
        self.context.in_iteration = prev_iter;
        self.expect_keyword(Keyword::While)?;
        self.expect_punct(Punct::OpenParen)?;
        let test = self.parse_expression()?;
        self.expect_punct(Punct::CloseParen)?;
        if self.look_ahead.token.matches_punct(Punct::SemiColon) {
            self.expect_punct(Punct::SemiColon)?;
        }
        Ok(DoWhileStmt {
            test,
            body: Box::new(body),
        })
    }

    #[inline]
    fn parse_break_stmt(&mut self) -> Res<Option<resast::Ident<'b>>> {
        debug!("{}: parse_break_stmt", self.look_ahead.span.start);
        self.parse_optionally_labeled_statement(Keyword::Break)
    }

    #[inline]
    fn parse_continue_stmt(&mut self) -> Res<Option<resast::Ident<'b>>> {
        debug!("{}: parse_continue_stmt", self.look_ahead.span.start);
        self.parse_optionally_labeled_statement(Keyword::Continue)
    }

    #[inline]
    fn parse_optionally_labeled_statement(&mut self, k: Keyword) -> Res<Option<resast::Ident<'b>>> {
        debug!(
            "{}: parse_optionally_labeled_statement",
            self.look_ahead.span.start
        );
        self.expect_keyword(k)?;
        let ret = if self.look_ahead.token.is_ident() && !self.context.has_line_term {
            let id = self.parse_var_ident(false)?;
            if !self.context.label_set.contains(&*id.name) {
                return Err(Error::UnknownOptionalLabel(
                    self.current_position,
                    k,
                    id.name.to_string(),
                ));
            }
            Some(id)
        } else {
            None
        };
        self.consume_semicolon()?;
        if ret.is_none()
            && k == Keyword::Break
            && !self.context.in_iteration
            && !self.context.in_switch
        {
            return Err(Error::InvalidOptionalLabel(self.current_position));
        }
        Ok(ret)
    }

    #[inline]
    fn parse_debugger_stmt(&mut self) -> Res<Stmt<'b>> {
        debug!("{}: parse_debugger_stmt", self.look_ahead.span.start);
        self.expect_keyword(Keyword::Debugger)?;
        self.consume_semicolon()?;
        Ok(Stmt::Debugger)
    }

    #[inline]
    fn parse_labelled_statement(&mut self) -> Res<Stmt<'b>> {
        debug!("parse_labelled_statement, {:?}", self.look_ahead.token);
        let start = self.look_ahead.span;
        let ret = self.parse_expression()?;
        if Self::is_ident(&ret) && self.at_punct(Punct::Colon) {
            let _colon = self.next_item()?;
            let id = if let Expr::Ident(ident) = ret {
                ident
            } else {
                return Err(self.reinterpret_error("expression", "ident"));
            };
            if !self.context.label_set.insert(self.get_string(&start)?) {
                return Err(self.redecl_error(&id.name));
            }
            let body = if self.at_keyword(Keyword::Class) {
                let class = self.next_item()?;
                if !self.config.tolerant {
                    return self.unexpected_token_error(&class, "");
                }
                let body = self.parse_class_body()?;
                let cls = Class {
                    id: None,
                    super_class: None,
                    body,
                };
                let expr = Expr::Class(cls);
                Stmt::Expr(expr)
            } else if self.at_keyword(Keyword::Function) {
                let f = self.parse_function_decl(true)?;
                let expr = Expr::Func(f);
                Stmt::Expr(expr)
            } else {
                self.parse_statement()?
            };
            self.context.label_set.remove(&self.get_string(&start)?);
            Ok(Stmt::Labeled(LabeledStmt {
                label: id,
                body: Box::new(body),
            }))
        } else {
            self.consume_semicolon()?;
            Ok(Stmt::Expr(ret))
        }
    }

    #[inline]
    fn parse_expression_statement(&mut self) -> Res<Expr<'b>> {
        debug!("{}: parse_expression_statement", self.look_ahead.span.start);
        let ret = self.parse_expression()?;
        self.consume_semicolon()?;
        Ok(ret)
    }

    #[inline]
    fn parse_expression(&mut self) -> Res<Expr<'b>> {
        debug!("{}: parse_expression", self.look_ahead.span.start);
        let (prev_bind, prev_assign, prev_first) = self.isolate_cover_grammar();
        let ret = self.parse_assignment_expr()?;
        self.set_isolate_cover_grammar_state(prev_bind, prev_assign, prev_first)?;
        if self.at_punct(Punct::Comma) {
            let mut list = vec![ret];
            while !self.look_ahead.token.is_eof() {
                if !self.at_punct(Punct::Comma) {
                    break;
                }
                let _comma = self.next_item()?;
                let (prev_bind, prev_assign, prev_first) = self.isolate_cover_grammar();
                let expr = self.parse_assignment_expr()?;
                self.set_isolate_cover_grammar_state(prev_bind, prev_assign, prev_first)?;
                list.push(expr);
            }
            return Ok(Expr::Sequence(list));
        }
        Ok(ret)
    }

    #[inline]
    fn parse_block(&mut self) -> Res<BlockStmt<'b>> {
        debug!("{}: parse_block", self.look_ahead.span.start);
        self.expect_punct(Punct::OpenBrace)?;
        let mut ret = Vec::new();
        loop {
            if self.at_punct(Punct::CloseBrace) {
                break;
            }
            let part = self.parse_statement_list_item()?;
            if let ProgramPart::Decl(ref decl) = part {
                match decl {
                    Decl::Export(_) => {
                        return Err(Error::InvalidExportError(self.current_position))
                    }
                    Decl::Import(_) => {
                        return Err(Error::InvalidImportError(self.current_position))
                    }
                    _ => (),
                }
            };
            ret.push(part);
        }
        self.expect_punct(Punct::CloseBrace)?;
        Ok(BlockStmt(ret))
    }

    #[inline]
    fn parse_lexical_decl(&mut self, in_for: bool) -> Res<Decl<'b>> {
        debug!("{}: parse_lexical_decl", self.look_ahead.span.start);
        let next = self.next_item()?;
        debug!("next: {:?} {}", next, self.context.allow_yield);
        let kind = match next.token {
            Token::Keyword(ref k) => match k {
                Keyword::Let => VarKind::Let,
                Keyword::Const => VarKind::Const,
                _ => return self.expected_token_error(&next, &["let", "const"]),
            },
            _ => return self.expected_token_error(&next, &["let", "const"]),
        };
        let decl = self.parse_binding_list(kind, in_for)?;
        self.consume_semicolon()?;
        Ok(Decl::Var(kind, decl))
    }

    #[inline]
    fn parse_binding_list(&mut self, kind: VarKind, in_for: bool) -> Res<Vec<VarDecl<'b>>> {
        debug!("{}: parse_binding_list", self.look_ahead.span.start);
        let mut ret = vec![self.parse_lexical_binding(kind, in_for)?];
        while self.at_punct(Punct::Comma) {
            let _comma = self.next_item()?;
            ret.push(self.parse_lexical_binding(kind, in_for)?)
        }
        Ok(ret)
    }

    #[inline]
    fn parse_variable_decl_list(&mut self, in_for: bool) -> Res<Vec<VarDecl<'b>>> {
        let mut ret = vec![self.parse_variable_decl(in_for)?];
        while self.at_punct(Punct::Comma) {
            let _ = self.next_item()?;
            ret.push(self.parse_variable_decl(in_for)?);
        }
        Ok(ret)
    }

    #[inline]
    fn parse_variable_decl(&mut self, in_for: bool) -> Res<VarDecl<'b>> {
        let start = self.look_ahead.clone();
        let (_, id) = self.parse_pattern(Some(VarKind::Var), &mut Vec::new())?;
        if self.context.strict && Self::is_restricted(&id) && !self.config.tolerant {
            return self.unexpected_token_error(&start, "restricted word");
        }
        let init = if self.at_punct(Punct::Equal) {
            let _ = self.next_item()?;
            let (prev_bind, prev_assign, prev_first) = self.isolate_cover_grammar();
            let init = self.parse_assignment_expr()?;
            self.set_isolate_cover_grammar_state(prev_bind, prev_assign, prev_first)?;
            Some(init)
        } else if !Self::is_pat_ident(&id) && !in_for {
            self.expect_punct(Punct::Equal)?;
            None
        } else {
            None
        };
        Ok(VarDecl { id, init })
    }

    #[inline]
    fn is_pat_ident(pat: &Pat) -> bool {
        match pat {
            Pat::Ident(_) => true,
            _ => false,
        }
    }

    #[inline]
    fn parse_lexical_binding(&mut self, kind: VarKind, in_for: bool) -> Res<VarDecl<'b>> {
        debug!("{}: parse_lexical_binding", self.look_ahead.span.start);
        let start = self.look_ahead.clone();
        let (_, id) = self.parse_pattern(Some(kind), &mut Vec::new())?;
        if self.context.strict && Self::is_restricted(&id) && !self.config.tolerant {
            return self.unexpected_token_error(&start, "restricted word");
        }
        let init = if kind == VarKind::Const {
            if !self.at_keyword(Keyword::In) && !self.at_contextual_keyword("of") {
                if self.at_punct(Punct::Equal) {
                    let _ = self.next_item()?;
                    let (prev_bind, prev_assign, prev_first) = self.isolate_cover_grammar();
                    let init = self.parse_assignment_expr()?;
                    self.set_isolate_cover_grammar_state(prev_bind, prev_assign, prev_first)?;
                    Some(init)
                } else {
                    return self.expected_token_error(&self.look_ahead, &["="]);
                }
            } else {
                None
            }
        } else if !in_for && !Self::is_pat_ident(&id) || self.at_punct(Punct::Equal) {
            self.expect_punct(Punct::Equal)?;
            let (prev_bind, prev_assign, prev_first) = self.isolate_cover_grammar();
            let init = self.parse_assignment_expr()?;
            self.set_isolate_cover_grammar_state(prev_bind, prev_assign, prev_first)?;
            Some(init)
        } else {
            None
        };
        Ok(VarDecl { id, init })
    }

    #[inline]
    fn is_restricted(id: &Pat) -> bool {
        match id {
            Pat::Ident(ref ident) => ident.name == "eval" || ident.name == "arguments",
            _ => false,
        }
    }

    #[inline]
    fn parse_function_decl(&mut self, opt_ident: bool) -> Res<Func<'b>> {
        debug!("{}: parse_function_decl", self.look_ahead.span.start);
        let is_async = if self.at_contextual_keyword("async") {
            let _ = self.next_item()?;
            true
        } else {
            false
        };
        self.expect_keyword(Keyword::Function)?;
        let is_gen = if is_async {
            false
        } else {
            let is_gen = self.at_punct(Punct::Asterisk);
            if is_gen {
                let _ = self.next_item()?;
            }
            is_gen
        };
        let (id, first_restricted) = if !opt_ident || !self.at_punct(Punct::OpenParen) {
            let start = self.look_ahead.clone();
            let id = self.parse_var_ident(false)?;
            if self.context.strict && start.token.is_restricted() {
                return self.expected_token_error(&start, &[]);
            }
            let first_restricted = if !self.context.strict {
                if start.token.is_restricted() || start.token.is_strict_reserved() {
                    Some(start)
                } else {
                    None
                }
            } else {
                None
            };
            (Some(id), first_restricted)
        } else {
            (None, None)
        };
        let prev_await = self.context.allow_await;
        let prev_yield = self.context.allow_yield;
        self.context.allow_await = is_async;
        self.context.allow_yield = !is_gen;

        let formal_params = self.parse_formal_params()?;
        let strict = formal_params.strict;
        let params = formal_params.params;
        let prev_strict = self.context.strict;
        let prev_allow_strict = self.context.allow_strict_directive;
        self.context.allow_strict_directive = formal_params.simple;
        let body = self.parse_function_source_el()?;
        if self.context.strict {
            if let Some(ref item) = first_restricted {
                return self.expected_token_error(item, &[]);
            }
        }
        if self.context.strict && strict {
            return self.expected_token_error(&self.look_ahead, &[]);
        }
        self.context.strict = prev_strict;
        self.context.allow_strict_directive = prev_allow_strict;
        self.context.allow_await = prev_await;
        self.context.allow_yield = prev_yield;
        Ok(Func {
            id,
            params,
            body,
            generator: is_gen,
            is_async,
        })
    }

    #[inline]
    fn parse_function_source_el(&mut self) -> Res<FuncBody<'b>> {
        debug!("{}: parse_function_source_el", self.look_ahead.span.start);
        self.expect_punct(Punct::OpenBrace)?;
        let mut body = self.parse_directive_prologues()?;
        let prev_label = self.context.label_set.clone();
        let prev_iter = self.context.in_iteration;
        let prev_switch = self.context.in_switch;
        let prev_in_fn = self.context.in_function_body;
        self.context.label_set = HashSet::new();
        self.context.in_iteration = false;
        self.context.in_switch = false;
        self.context.in_function_body = true;
        while !self.look_ahead.token.is_eof() {
            if self.at_punct(Punct::CloseBrace) {
                break;
            }
            body.push(self.parse_statement_list_item()?)
        }
        self.expect_punct(Punct::CloseBrace)?;
        self.context.label_set = prev_label;
        self.context.in_iteration = prev_iter;
        self.context.in_switch = prev_switch;
        self.context.in_function_body = prev_in_fn;
        Ok(FuncBody(body))
    }

    #[inline]
    fn parse_class_decl(&mut self, opt_ident: bool) -> Res<Class<'b>> {
        debug!("{}: parse_class_decl", self.look_ahead.span.start);
        let prev_strict = self.context.strict;
        self.context.strict = true;
        self.expect_keyword(Keyword::Class)?;
        let mut super_class = if self.at_contextual_keyword("extends") {
            let _ = self.next_item()?;
            let (prev_bind, prev_assign, prev_first) = self.isolate_cover_grammar();
            let super_class = self.parse_left_hand_side_expr()?;
            self.set_isolate_cover_grammar_state(prev_bind, prev_assign, prev_first)?;
            Some(Box::new(super_class))
        } else {
            None
        };
        let id = if opt_ident && !self.look_ahead.token.is_ident() {
            None
        } else {
            Some(self.parse_var_ident(false)?)
        };
        super_class = if super_class.is_none() && self.at_contextual_keyword("extends") {
            let _ = self.next_item()?;
            let (prev_bind, prev_assign, prev_first) = self.isolate_cover_grammar();
            let super_class = self.parse_left_hand_side_expr()?;
            self.set_isolate_cover_grammar_state(prev_bind, prev_assign, prev_first)?;
            Some(Box::new(super_class))
        } else {
            None
        };
        let body = self.parse_class_body()?;

        self.context.strict = prev_strict;
        Ok(Class {
            id,
            super_class,
            body,
        })
    }

    #[inline]
    fn parse_class_body(&mut self) -> Res<ClassBody<'b>> {
        debug!("{}: parse_class_body", self.look_ahead.span.start);
        let mut ret = Vec::new();
        let mut has_ctor = false;
        self.expect_punct(Punct::OpenBrace)?;
        while !self.at_punct(Punct::CloseBrace) {
            if self.at_punct(Punct::SemiColon) {
                let _ = self.next_item()?;
            } else {
                let (ctor, el) = self.parse_class_el(has_ctor)?;
                has_ctor = ctor;
                ret.push(el)
            }
        }
        self.expect_punct(Punct::CloseBrace)?;
        Ok(ClassBody(ret))
    }

    #[inline]
    fn parse_class_el(&mut self, has_ctor: bool) -> Res<(bool, Prop<'b>)> {
        debug!("{}: parse_class_el", self.look_ahead.span.start);
        let mut token = self.look_ahead.token.clone();
        let mut has_ctor = has_ctor;
        let mut key: Option<PropKey> = None;
        let mut value: Option<PropValue> = None;
        let mut computed = false;
        let mut is_static = false;
        let is_async = if self.at_contextual_keyword("async") {
            let _async = self.next_item()?;
            true
        } else {
            false
        };
        if self.at_punct(Punct::Asterisk) {
            debug!("found leading asterisk");
            let _ = self.next_item()?;
        } else {
            computed = self.at_punct(Punct::OpenBracket);

            let new_key = self.parse_object_property_key()?;

            if Self::is_static(&new_key)
                && (Self::qualified_prop_name(&self.look_ahead.token)
                    || self.at_punct(Punct::Asterisk))
            {
                token = self.look_ahead.token.clone();
                computed = self.at_punct(Punct::OpenBracket);
                is_static = true;
                if self.at_punct(Punct::Asterisk) {
                    let _ = self.next_item()?;
                } else {
                    key = Some(self.parse_object_property_key()?);
                }
            } else {
                key = Some(new_key);
            }
            if token.is_ident()
                && !self.context.has_line_term
                && self.at_contextual_keyword("async")
                && !self.look_ahead.token.matches_punct(Punct::Colon)
                && !self.look_ahead.token.matches_punct(Punct::OpenParen)
                && !self.look_ahead.token.matches_punct(Punct::Asterisk)
            {
                return self.expected_token_error(&self.look_ahead, &[":", "(", "*"]);
            }
        }

        let mut kind: Option<PropKind> = None;
        let mut method = false;

        let look_ahead_prop_key = Self::qualified_prop_name(&self.look_ahead.token);
        if token.is_ident() {
            let (at_get, at_set) = if let Some(ref k) = key {
                (
                    Self::is_key(&k, "get") && look_ahead_prop_key,
                    Self::is_key(&k, "set") && look_ahead_prop_key,
                )
            } else {
                (false, false)
            };

            if at_get {
                kind = Some(PropKind::Get);
                computed = self.at_punct(Punct::OpenBracket);
                self.context.allow_yield = false;
                key = Some(self.parse_object_property_key()?);
                value = Some(self.parse_getter_method()?);
            } else if at_set {
                kind = Some(PropKind::Set);
                computed = self.at_punct(Punct::OpenBracket);
                key = Some(self.parse_object_property_key()?);
                value = Some(self.parse_setter_method()?);
            }
        } else if token.matches_punct(Punct::Asterisk) && look_ahead_prop_key {
            kind = Some(PropKind::Init);
            computed = self.at_punct(Punct::OpenBracket);
            key = Some(self.parse_object_property_key()?);
            value = Some(self.parse_generator_method()?);
            method = true;
        }

        if kind.is_none() && key.is_some() && self.at_punct(Punct::OpenParen) {
            kind = Some(PropKind::Init);
            method = true;
            value = Some(if is_async {
                self.parse_async_property_method()?
            } else {
                self.parse_property_method()?
            });
        }

        let mut kind = if let Some(k) = kind {
            k
        } else {
            return self.expected_token_error(&self.look_ahead, &["method identifier"]);
        };

        if kind == PropKind::Init {
            kind = PropKind::Method;
        }

        let key = if let Some(k) = key {
            k
        } else {
            return self.expected_token_error(&self.look_ahead, &[]);
        };
        if !computed {
            if is_static && Self::is_key(&key, "prototype") {
                return self.expected_token_error(&self.look_ahead, &[]);
            }
            if !is_static && Self::is_key(&key, "constructor") {
                if kind != PropKind::Method || !method {
                    return self
                        .expected_token_error(&self.look_ahead, &["[constructor declaration]"]);
                }
                if let Some(ref v) = value {
                    if Self::is_generator(&v) {
                        return self.expected_token_error(
                            &self.look_ahead,
                            &["[non-generator function declaration]"],
                        );
                    }
                }
                if has_ctor {
                    return self.expected_token_error(&self.look_ahead, &[]);
                } else {
                    has_ctor = true;
                }
                kind = PropKind::Ctor;
            }
        }

        let value = if let Some(v) = value {
            v
        } else {
            return self.expected_token_error(&self.look_ahead, &[]);
        };

        Ok((
            has_ctor,
            Prop {
                key,
                value,
                kind,
                method,
                computed,
                short_hand: false,
                is_static,
            },
        ))
    }

    #[inline]
    fn is_key(key: &PropKey, other: &str) -> bool {
        match key {
            PropKey::Lit(ref l) => match l {
                Lit::String(ref s) => match s {
                    resast::prelude::StringLit::Single(ref s)
                    | resast::prelude::StringLit::Double(ref s) => s == other,
                },
                _ => false,
            },
            PropKey::Expr(ref e) => match e {
                Expr::Ident(ref s) => s.name == other,
                _ => false,
            },
            PropKey::Pat(ref p) => match p {
                Pat::Ident(ref s) => s.name == other,
                _ => false,
            },
        }
    }

    #[inline]
    fn is_generator(val: &PropValue) -> bool {
        match val {
            PropValue::Expr(ref e) => match e {
                Expr::Func(ref f) => f.generator,
                Expr::ArrowFunc(ref f) => f.generator,
                _ => false,
            },
            _ => false,
        }
    }

    #[inline]
    fn is_static(key: &PropKey) -> bool {
        match key {
            PropKey::Lit(ref l) => match l {
                Lit::String(ref s) => match s {
                    resast::prelude::StringLit::Single(ref s)
                    | resast::prelude::StringLit::Double(ref s) => s == "static",
                },
                _ => false,
            },
            PropKey::Expr(ref e) => match e {
                Expr::Ident(ref s) => s.name == "static",
                _ => false,
            },
            PropKey::Pat(ref p) => match p {
                Pat::Ident(ref s) => s.name == "static",
                _ => false,
            },
        }
    }

    #[inline]
    fn parse_async_property_method(&mut self) -> Res<PropValue<'b>> {
        debug!(
            "{}: parse_property_method_async_fn",
            self.look_ahead.span.start
        );
        let prev_yield = self.context.allow_yield;
        let prev_await = self.context.allow_await;
        self.context.allow_yield = false;
        self.context.allow_await = true;
        let params = self.parse_formal_params()?;
        let body = self.parse_property_method_body(params.simple, params.found_restricted)?;
        self.context.allow_yield = prev_yield;
        self.context.allow_await = prev_await;
        let func = Func {
            id: None,
            params: params.params,
            is_async: true,
            generator: false,
            body,
        };
        Ok(PropValue::Expr(Expr::Func(func)))
    }

    #[inline]
    fn parse_property_method(&mut self) -> Res<PropValue<'b>> {
        debug!("{}: parse_property_method", self.look_ahead.span.start);
        let prev_yield = self.context.allow_yield;
        self.context.allow_yield = false;
        let params = self.parse_formal_params()?;
        let body = self.parse_property_method_body(params.simple, params.found_restricted)?;
        self.context.allow_yield = prev_yield;
        let func = Func {
            id: None,
            params: params.params,
            is_async: false,
            generator: false,
            body,
        };
        Ok(PropValue::Expr(Expr::Func(func)))
    }

    #[inline]
    fn parse_generator_method(&mut self) -> Res<PropValue<'b>> {
        debug!("{}: pares_generator_method", self.look_ahead.span.start);
        let prev_yield = self.context.allow_yield;
        self.context.allow_yield = true;
        let params = self.parse_formal_params()?;
        self.context.allow_yield = false;
        let body = self.parse_method_body(params.simple, params.found_restricted)?;
        self.context.allow_yield = prev_yield;
        let func = Func {
            id: None,
            params: params.params,
            is_async: false,
            generator: true,
            body,
        };
        Ok(PropValue::Expr(Expr::Func(func)))
    }

    #[inline]
    fn parse_getter_method(&mut self) -> Res<PropValue<'b>> {
        debug!("{}: parse_getter_method", self.look_ahead.span.start);
        let is_gen = false;
        let prev_yield = self.context.allow_yield;
        let start_position = self.look_ahead_position;
        let formal_params = self.parse_formal_params()?;
        if !formal_params.params.is_empty() {
            self.tolerate_error(Error::InvalidGetterParams(start_position))?;
        }
        let body = self.parse_method_body(formal_params.simple, formal_params.found_restricted)?;
        self.context.allow_yield = prev_yield;
        Ok(PropValue::Expr(Expr::Func(Func {
            id: None,
            params: formal_params.params,
            body,
            generator: is_gen,
            is_async: false,
        })))
    }

    #[inline]
    fn parse_method_body(&mut self, simple: bool, found_restricted: bool) -> Res<FuncBody<'b>> {
        debug!("{}: parse_method_body", self.look_ahead.span.start);
        self.context.is_assignment_target = false;
        self.context.is_binding_element = false;
        let prev_strict = self.context.strict;
        let prev_allow_strict = self.context.allow_strict_directive;
        self.context.allow_strict_directive = simple;
        let start = self.look_ahead.clone();
        let (prev_bind, prev_assign, prev_first) = self.isolate_cover_grammar();
        let body = self.parse_function_source_el()?;
        self.set_isolate_cover_grammar_state(prev_bind, prev_assign, prev_first)?;
        if self.context.strict && found_restricted && !self.config.tolerant {
            self.unexpected_token_error(&start, "restricted ident")?;
        }
        self.context.strict = prev_strict;
        self.context.allow_strict_directive = prev_allow_strict;
        Ok(body)
    }

    #[inline]
    fn parse_setter_method(&mut self) -> Res<PropValue<'b>> {
        debug!("{}: parse_setter_method", self.look_ahead.span.start);
        let prev_allow = self.context.allow_yield;
        self.context.allow_yield = true;
        let start_position = self.look_ahead_position;
        let params = self.parse_formal_params()?;
        self.context.allow_yield = prev_allow;
        if params.params.len() != 1 {
            self.tolerate_error(Error::InvalidSetterParams(start_position))?;
        } else if let Some(ref param) = params.params.get(0) {
            if Self::is_rest(param) {
                self.tolerate_error(Error::InvalidSetterParams(start_position))?;
            }
        }
        let body = self.parse_property_method_body(params.simple, params.found_restricted)?;
        let func = Func {
            id: None,
            params: params.params,
            body,
            generator: false,
            is_async: false,
        };
        Ok(PropValue::Expr(Expr::Func(func)))
    }

    #[inline]
    fn is_rest(arg: &FuncArg) -> bool {
        match arg {
            FuncArg::Expr(ref e) => match e {
                Expr::Spread(_) => true,
                _ => false,
            },
            FuncArg::Pat(ref p) => match p {
                Pat::RestElement(_) => true,
                _ => false,
            },
        }
    }

    #[inline]
    fn parse_property_method_body(
        &mut self,
        simple: bool,
        found_restricted: bool,
    ) -> Res<FuncBody<'b>> {
        debug!("{}: parse_property_method_fn", self.look_ahead.span.start);
        self.context.is_assignment_target = false;
        self.context.is_binding_element = false;
        let prev_strict = self.context.strict;
        let prev_allow = self.context.allow_strict_directive;
        self.context.allow_strict_directive = simple;
        let start_pos = self.look_ahead_position;
        let (prev_bind, prev_assign, prev_first) = self.inherit_cover_grammar();
        let ret = self.parse_function_source_el()?;
        self.set_inherit_cover_grammar_state(prev_bind, prev_assign, prev_first);
        if self.context.strict && found_restricted {
            self.tolerate_error(Error::NonStrictFeatureInStrictContext(
                start_pos,
                "restriced ident".to_string(),
            ))?;
        }
        self.context.strict = prev_strict;
        self.context.allow_strict_directive = prev_allow;
        Ok(ret)
    }

    fn qualified_prop_name(tok: &Token<&str>) -> bool {
        debug!("qualified_prop_name",);
        tok.is_ident()
            || tok.is_keyword()
            || tok.is_literal()
            || tok.matches_punct(Punct::OpenBracket)
    }

    #[inline]
    fn parse_object_property_key(&mut self) -> Res<PropKey<'b>> {
        debug!("{}: parse_object_property_key", self.look_ahead.span.start);
        let item = self.next_item()?;
        if item.token.is_string()
            || match item.token {
                Token::Number(_) => true,
                _ => false,
            }
        {
            // if item.token.is_oct_Lit() {
            //     //FIXME: possible tolerable error
            // }
            let id = match &item.token {
                Token::String(ref sl) => match sl {
                    ress::prelude::StringLit::Single(s) => Lit::single_string_from(s),
                    ress::prelude::StringLit::Double(s) => Lit::double_string_from(s),
                },
                Token::Number(_) => {
                    if self.at_big_int_flag() {
                        return self.unexpected_token_error(
                            &self.look_ahead,
                            "BigInt cannot be uses as an object Lit key",
                        );
                    }
                    Lit::number_from(self.get_string(&item.span)?)
                }
                _ => return Err(self.reinterpret_error("number or string", "Lit")),
            };
            Ok(PropKey::Lit(id))
        } else if item.token.is_ident()
            // || item.token.is_bool()
            || item.token.is_null()
            || item.token.is_keyword()
            || match item.token {
                Token::Boolean(_) => true,
                _ => false,
            }
        {
            let id = self.get_string(&item.span)?;
            Ok(PropKey::Expr(Expr::ident_from(id)))
        } else if item.token.matches_punct(Punct::OpenBracket) {
            let (prev_bind, prev_assign, prev_first) = self.isolate_cover_grammar();
            let key = self.parse_assignment_expr()?;
            self.set_isolate_cover_grammar_state(prev_bind, prev_assign, prev_first)?;
            let id = if Self::is_valid_property_key_lit(&key) {
                match key {
                    Expr::Lit(lit) => PropKey::Lit(lit),
                    _ => {
                        return self.expected_token_error(&self.look_ahead, &["property key Lit"]);
                    }
                }
            } else {
                PropKey::Expr(key)
            };
            self.expect_punct(Punct::CloseBracket)?;
            Ok(id)
        } else {
            self.expected_token_error(
                &item,
                &[
                    "[string]",
                    "[number]",
                    "[ident]",
                    "[boolean]",
                    "null",
                    "[keyword]",
                    "[",
                ],
            )
        }
    }

    #[inline]
    fn is_valid_property_key_lit(expr: &Expr) -> bool {
        match expr {
            Expr::Lit(ref l) => match l {
                Lit::String(_) | Lit::Number(_) | Lit::Boolean(_) => true,
                _ => false,
            },
            _ => false,
        }
    }

    #[inline]
    fn parse_primary_expression(&mut self) -> Res<Expr<'b>> {
        debug!("{}: parse_primary_expression", self.look_ahead.span.start);
        if self.look_ahead.token.is_ident() {
            if ((self.context.is_module || self.context.allow_await)
                && self.at_keyword(Keyword::Await))
                && !self.config.tolerant
            {
                return self.unexpected_token_error(
                    &self.look_ahead,
                    "Modules do not allow 'await' to be used as an identifier",
                );
            }
            if self.at_async_function() {
                self.parse_function_expr()
            } else {
                let ident = self.next_item()?;
                Ok(Expr::ident_from(self.get_string(&ident.span)?))
            }
        } else if self.look_ahead.token.is_number() || self.look_ahead.token.is_string() {
            // if self.context.strict && self.look_ahead.token.is_oct_Lit() {
            //     //FIXME: possible tolerable error
            // }
            self.context.is_assignment_target = false;
            self.context.is_binding_element = false;
            let item = self.next_item()?;
            let lit = match item.token {
                Token::Number(_) => {
                    let mut span = item.span;
                    if self.at_big_int_flag() {
                        span.end += 1;
                        // Consume the ident
                        let _n = self.next_item();
                    }
                    let inner = self.get_string(&span)?;
                    Lit::number_from(inner)
                }
                Token::String(sl) => {
                    let inner = match sl {
                        ress::prelude::StringLit::Single(ref s) => {
                            resast::prelude::StringLit::single_from(s)
                        }
                        ress::prelude::StringLit::Double(ref d) => {
                            resast::prelude::StringLit::double_from(d)
                        }
                    };
                    Lit::String(inner)
                }
                _ => unreachable!(),
            };
            Ok(Expr::Lit(lit))
        } else if self.look_ahead.token.is_boolean() {
            self.context.is_assignment_target = false;
            self.context.is_binding_element = false;
            let item = self.next_item()?;
            let lit = match item.token {
                Token::Boolean(b) => Lit::Boolean(b.into()),
                _ => unreachable!(),
            };
            Ok(Expr::Lit(lit))
        } else if self.look_ahead.token.is_null() {
            self.context.is_assignment_target = false;
            self.context.is_binding_element = false;
            let _ = self.next_item()?;
            Ok(Expr::Lit(Lit::Null))
        } else if self.look_ahead.is_template() {
            let lit = self.parse_template_lit()?;
            Ok(Expr::Lit(Lit::Template(lit)))
        } else if self.look_ahead.token.is_punct() {
            let (prev_bind, prev_assign, prev_first) = self.inherit_cover_grammar();
            let expr = if self.at_punct(Punct::OpenParen) {
                self.parse_group_expr()?
            } else if self.at_punct(Punct::OpenBracket) {
                self.parse_array_init()?
            } else if self.at_punct(Punct::OpenBrace) {
                self.parse_obj_init()?
            } else {
                return self.expected_token_error(&self.look_ahead, &["{", "[", "("]);
            };
            self.set_inherit_cover_grammar_state(prev_bind, prev_assign, prev_first);
            Ok(expr)
        } else if self.look_ahead.token.is_regex() {
            self.context.is_assignment_target = false;
            self.context.is_binding_element = false;
            let regex = self.next_item()?;
            let lit = match regex.token {
                Token::RegEx(r) => {
                    let flags = if let Some(f) = r.flags { f } else { "" };
                    resast::prelude::RegEx::from(&r.body, flags)
                }
                _ => unreachable!(),
            };
            Ok(Expr::Lit(Lit::RegEx(lit)))
        } else if self.look_ahead.token.is_keyword() {
            if (!self.context.strict
                && ((self.context.allow_yield && self.at_keyword(Keyword::Yield))
                    || self.at_keyword(Keyword::Let)))
                || (!self.context.strict && self.look_ahead.token.is_strict_reserved())
            {
                let ident = self.parse_ident_name()?;
                Ok(Expr::Ident(ident))
            } else {
                self.context.is_assignment_target = false;
                self.context.is_binding_element = false;
                if self.at_keyword(Keyword::Function) {
                    self.parse_function_expr()
                } else if self.at_keyword(Keyword::This) {
                    let _ = self.next_item()?;
                    Ok(Expr::This)
                } else if self.at_keyword(Keyword::Class) {
                    let cls = self.parse_class_decl(true)?;
                    Ok(Expr::Class(cls))
                } else if self.at_import_call() {
                    // TODO: Double check this
                    let ident = self.parse_ident_name()?;
                    Ok(Expr::Ident(ident))
                } else {
                    self.expected_token_error(
                        &self.look_ahead,
                        &["function", "this", "class", "import"],
                    )
                }
            }
        } else {
            self.expected_token_error(
                &self.look_ahead,
                &[
                    "[identifier]",
                    "async",
                    "[Number]",
                    "[String]",
                    "[RegEx]",
                    "yield",
                    "let",
                    "function",
                    "this",
                    "class",
                    "import",
                ],
            )
        }
    }

    #[inline]
    fn parse_group_expr(&mut self) -> Res<Expr<'b>> {
        debug!("{}: parse_group_expr", self.look_ahead.span.start);
        self.expect_punct(Punct::OpenParen)?;
        if self.at_punct(Punct::CloseParen) {
            let _ = self.next_item()?;
            if !self.at_punct(Punct::EqualGreaterThan) {
                self.expect_punct(Punct::EqualGreaterThan)?;
            }
            Ok(Expr::ArrowParamPlaceHolder(Vec::new(), false))
        } else {
            let mut params = Vec::new();
            if self.at_punct(Punct::Ellipsis) {
                let (_, expr) = self.parse_rest_element(&mut params)?;
                let arg = FuncArg::Pat(expr);
                self.expect_punct(Punct::CloseParen)?;
                if !self.at_punct(Punct::EqualGreaterThan) {
                    self.expect_punct(Punct::EqualGreaterThan)?;
                }
                Ok(Expr::ArrowParamPlaceHolder(vec![arg], false))
            } else {
                self.context.is_binding_element = true;
                let (prev_bind, prev_assign, prev_first) = self.inherit_cover_grammar();
                let mut ex = self.parse_assignment_expr()?;
                self.set_inherit_cover_grammar_state(prev_bind, prev_assign, prev_first);
                if self.at_punct(Punct::Comma) {
                    let mut exprs = vec![ex];
                    while !self.look_ahead.token.is_eof() {
                        if !self.at_punct(Punct::Comma) {
                            break;
                        }
                        let _ = self.next_item()?;
                        if self.at_punct(Punct::CloseParen) {
                            let _ = self.next_item()?;
                            return Ok(Expr::ArrowParamPlaceHolder(
                                exprs.into_iter().map(FuncArg::Expr).collect(),
                                false,
                            ));
                        } else if self.at_punct(Punct::Ellipsis) {
                            if !self.context.is_binding_element {
                                return self.expected_token_error(&self.look_ahead, &["not ..."]);
                            }
                            let (_, rest) = self.parse_rest_element(&mut params)?;
                            let mut args = Vec::with_capacity(exprs.len() + 1);
                            for ex in exprs {
                                if Self::is_reinterpret_target(&ex) {
                                    args.push(FuncArg::Pat(self.reinterpret_expr_as_pat(ex)?));
                                } else {
                                    args.push(FuncArg::Expr(ex));
                                }
                            }
                            args.push(FuncArg::Pat(rest));
                            self.expect_punct(Punct::CloseParen)?;
                            return Ok(Expr::ArrowParamPlaceHolder(args, false));
                        } else {
                            let (prev_bind, prev_assign, prev_first) = self.inherit_cover_grammar();
                            exprs.push(self.parse_assignment_expr()?);
                            self.set_inherit_cover_grammar_state(
                                prev_bind,
                                prev_assign,
                                prev_first,
                            );
                        }
                    }
                    ex = Expr::Sequence(exprs);
                }
                self.expect_punct(Punct::CloseParen)?;
                if self.at_punct(Punct::EqualGreaterThan) {
                    if Self::is_ident(&ex) {
                        self.context.is_binding_element = false;
                        return Ok(Expr::ArrowParamPlaceHolder(vec![FuncArg::Expr(ex)], false));
                    }
                    if !self.context.is_binding_element {
                        return self.expected_token_error(&self.look_ahead, &["binding element"]);
                    }
                    if let Expr::Sequence(seq) = ex {
                        let args = seq.into_iter().map(FuncArg::Expr).collect();
                        return Ok(Expr::ArrowParamPlaceHolder(args, false));
                    } else {
                        return Ok(Expr::ArrowParamPlaceHolder(vec![FuncArg::Expr(ex)], false));
                    }
                }
                Ok(ex)
            }
        }
    }

    #[inline]
    fn parse_array_init(&mut self) -> Res<Expr<'b>> {
        debug!("{}: parse_array_init", self.look_ahead.span.start);
        self.expect_punct(Punct::OpenBracket)?;
        let mut elements = Vec::new();
        while !self.at_punct(Punct::CloseBracket) {
            if self.at_punct(Punct::Comma) {
                self.next_item()?;
                elements.push(None);
            } else if self.at_punct(Punct::Ellipsis) {
                let el = self.parse_spread_element()?;
                if !self.at_punct(Punct::CloseBracket) {
                    self.context.is_assignment_target = false;
                    self.context.is_binding_element = false;
                    self.expect_punct(Punct::Comma)?;
                }
                elements.push(Some(el))
            } else {
                let (prev_bind, prev_assign, prev_first) = self.inherit_cover_grammar();
                elements.push(Some(self.parse_assignment_expr()?));
                self.set_inherit_cover_grammar_state(prev_bind, prev_assign, prev_first);
                if !self.at_punct(Punct::CloseBracket) {
                    self.expect_punct(Punct::Comma)?;
                }
            }
        }
        self.expect_punct(Punct::CloseBracket)?;
        Ok(Expr::Array(elements))
    }
    #[inline]
    fn parse_obj_init(&mut self) -> Res<Expr<'b>> {
        debug!("{}: parse_obj_init", self.look_ahead.span.start);
        self.expect_punct(Punct::OpenBrace)?;
        let mut props = Vec::new();
        let mut has_proto = false;
        while !self.at_punct(Punct::CloseBrace) {
            let prop = if self.at_punct(Punct::Ellipsis) {
                let spread = self.parse_spread_element()?;
                ObjProp::Spread(spread)
            } else {
                let (found_proto, prop) = self.parse_obj_prop(has_proto)?;
                has_proto = has_proto || found_proto;
                prop
            };
            props.push(prop);
            if !self.at_punct(Punct::CloseBrace) {
                self.expect_comma_sep()?;
            }
        }
        self.expect_punct(Punct::CloseBrace)?;
        Ok(Expr::Obj(props))
    }

    #[inline]
    fn parse_obj_prop(&mut self, has_proto: bool) -> Res<(bool, ObjProp<'b>)> {
        debug!("{}: parse_obj_prop", self.look_ahead.span.start);
        let start = self.look_ahead.clone();
        let start_pos = self.look_ahead_position;
        let mut has_proto = has_proto;
        let mut at_get = false;
        let mut at_set = false;
        let (key, is_async, computed) = if start.token.is_ident() {
            at_get = start.token.matches_ident_str("get");
            at_set = start.token.matches_ident_str("set");
            let _ = self.next_item()?;
            let computed = self.at_punct(Punct::OpenBracket);
            let is_async = self.context.has_line_term
                && start.token.matches_ident_str("async")
                && !self.at_punct(Punct::Colon)
                && !self.at_punct(Punct::Asterisk)
                && !self.at_punct(Punct::Comma);
            let key = if is_async {
                self.parse_object_property_key()?
            } else {
                PropKey::Expr(Expr::Ident(resast::Ident::from(
                    &self.original[start.span.start..start.span.end],
                )))
            };
            (Some(key), is_async, computed)
        } else if self.at_punct(Punct::Asterisk) {
            self.next_item()?;
            (None, false, false)
        } else {
            let computed = self.at_punct(Punct::OpenBracket);
            let key = self.parse_object_property_key()?;
            (Some(key), false, computed)
        };
        let at_qualified = self.at_qualified_prop_key();
        let prop = if at_get && at_qualified && !is_async {
            ObjProp::Prop(Prop {
                computed: self.at_punct(Punct::OpenBracket),
                key: self.parse_object_property_key()?,
                value: self.parse_getter_method()?,
                kind: PropKind::Get,
                method: false,
                short_hand: false,
                is_static: false,
            })
        } else if at_set && at_qualified && !is_async {
            ObjProp::Prop(Prop {
                computed: self.at_punct(Punct::OpenBracket),
                key: self.parse_object_property_key()?,
                value: self.parse_setter_method()?,
                kind: PropKind::Set,
                method: false,
                short_hand: false,
                is_static: false,
            })
        } else if start.token.matches_punct(Punct::Asterisk) && at_qualified {
            ObjProp::Prop(Prop {
                computed: self.at_punct(Punct::OpenBracket),
                key: self.parse_object_property_key()?,
                value: self.parse_generator_method()?,
                kind: PropKind::Init,
                method: true,
                short_hand: false,
                is_static: false,
            })
        } else if let Some(key) = key {
            let kind = PropKind::Init;
            if self.at_punct(Punct::Colon) && !is_async {
                if !computed && Self::is_proto_(&key) {
                    if has_proto {
                        self.tolerate_error(Error::Redecl(
                            start_pos,
                            "prototype can only be declared once".to_string(),
                        ))?;
                    }
                    has_proto = true;
                }
                let _ = self.next_item()?;
                let (prev_bind, prev_assign, prev_first) = self.get_cover_grammar_state();
                let value = self.parse_assignment_expr()?;
                self.set_inherit_cover_grammar_state(prev_bind, prev_assign, prev_first);
                ObjProp::Prop(Prop {
                    computed,
                    key,
                    value: PropValue::Expr(value),
                    kind,
                    method: false,
                    short_hand: false,
                    is_static: false,
                })
            } else if self.at_punct(Punct::OpenParen) {
                ObjProp::Prop(Prop {
                    computed,
                    key,
                    value: if is_async {
                        self.parse_async_property_method()?
                    } else {
                        self.parse_property_method()?
                    },
                    kind,
                    method: true,
                    short_hand: false,
                    is_static: false,
                })
            } else if start.token.is_ident() {
                if self.at_punct(Punct::Equal) {
                    self.context.first_covert_initialized_name_error =
                        Some(self.look_ahead.clone());
                    let _ = self.next_item()?;
                    let (prev_bind, prev_assign, prev_first) = self.inherit_cover_grammar();
                    let inner = self.parse_assignment_expr()?;
                    let value = if let Token::Ident(_) = &start.token {
                        let p = AssignPat {
                            left: Box::new(Pat::Ident(resast::Ident::from(
                                self.get_string(&start.span)?,
                            ))),
                            right: Box::new(inner),
                        };
                        PropValue::Pat(Pat::Assign(p))
                    } else {
                        PropValue::Expr(inner)
                    };
                    self.set_isolate_cover_grammar_state(prev_bind, prev_assign, prev_first)?;
                    ObjProp::Prop(Prop {
                        computed,
                        key,
                        value,
                        kind,
                        method: false,
                        short_hand: true,
                        is_static: false,
                    })
                } else {
                    let prop = self.reinterpret_prop(Prop {
                        computed,
                        key,
                        value: PropValue::None,
                        kind,
                        method: false,
                        short_hand: true,
                        is_static: false,
                    })?;
                    ObjProp::Prop(prop)
                }
            } else {
                return self.expected_token_error(&start, &["object property value"]);
            }
        } else {
            return self.expected_token_error(&start, &["object property key"]);
        };
        Ok((has_proto, prop))
    }

    #[inline]
    fn is_proto_(key: &PropKey) -> bool {
        match key {
            PropKey::Lit(ref l) => match l {
                Lit::String(ref s) => match s {
                    resast::prelude::StringLit::Single(ref s)
                    | resast::prelude::StringLit::Double(ref s) => s == "__proto__",
                },
                _ => false,
            },
            _ => false,
        }
    }

    #[inline]
    fn at_possible_ident(&self) -> bool {
        self.look_ahead.token.is_ident()
            || self.look_ahead.token.is_keyword()
            // || self.look_ahead.token.is_bool()
            || self.look_ahead.token.is_null()
            || if let Token::Boolean(_) = self.look_ahead.token {
                true
            } else {
                false
            }
    }

    #[inline]
    fn parse_template_lit(&mut self) -> Res<TemplateLit<'b>> {
        debug!("{}: parse_template_Lit", self.look_ahead.span.start);
        if !self.look_ahead.token.is_template_head() {
            return self
                .expected_token_error(&self.look_ahead, &["template head", "template no sub"]);
        }
        let mut expressions = Vec::new();
        let mut quasis = Vec::new();
        let quasi = self.parse_template_element()?;
        let mut breaking = quasi.tail;
        quasis.push(quasi);
        while !breaking {
            expressions.push(self.parse_expression()?);
            let quasi = self.parse_template_element()?;
            breaking = quasi.tail;
            quasis.push(quasi);
        }
        Ok(TemplateLit {
            expressions,
            quasis,
        })
    }

    #[inline]
    fn parse_template_element(&mut self) -> Res<TemplateElement<'b>> {
        debug!("{}: parse_template_element", self.look_ahead.span.start);
        let item = self.next_item()?;
        if let Token::Template(t) = item.token {
            let raw = self.get_string(&item.span)?;
            let (cooked, tail) = match t {
                Template::Head(c) => (c, false),
                Template::Middle(c) => (c, false),
                Template::Tail(c) => (c, true),
                Template::NoSub(c) => (c, true),
            };
            Ok(TemplateElement::from(tail, cooked, raw))
        } else {
            self.expected_token_error(&self.look_ahead, &["Template part"])
        }
    }

    #[inline]
    fn parse_function_expr(&mut self) -> Res<Expr<'b>> {
        debug!("{}: parse_function_expr", self.look_ahead.span.start);
        let is_async = self.at_contextual_keyword("async");
        if is_async {
            let _ = self.next_item()?;
        }
        self.expect_keyword(Keyword::Function)?;
        let is_gen = self.at_punct(Punct::Asterisk);
        if is_gen {
            let _ = self.next_item()?;
        }
        let prev_await = self.context.allow_await;
        let prev_yield = self.context.allow_yield;
        self.context.allow_await = is_async;
        self.context.allow_yield = !is_gen;
        let mut found_restricted = false;
        let id = if !self.at_punct(Punct::OpenParen) {
            let item = self.look_ahead.clone();
            let id = self.parse_fn_name(is_gen)?;
            if item.token.is_restricted() {
                if self.context.strict {
                    if !self.config.tolerant {
                        return self
                            .unexpected_token_error(&item, "restricted ident in strict context");
                    }
                } else {
                    found_restricted = true;
                }
            }
            if item.token.is_strict_reserved() {
                found_restricted = true;
            }
            Some(id)
        } else {
            None
        };
        let formal_params = self.parse_formal_params()?;
        found_restricted = found_restricted || formal_params.found_restricted;
        let prev_strict = self.context.strict;
        let prev_strict_dir = formal_params.simple;
        let start = self.look_ahead.clone();
        let body = self.parse_function_source_el()?;
        if self.context.strict && found_restricted {
            //TODO: Double check this
            if !self.config.tolerant {
                return self.unexpected_token_error(&start, "restricted ident in strict context");
            }
        }
        self.context.strict = prev_strict;
        self.context.allow_strict_directive = prev_strict_dir;
        self.context.allow_yield = prev_yield;
        self.context.allow_await = prev_await;
        let func = Func {
            id,
            params: formal_params.params,
            body,
            generator: is_gen,
            is_async,
        };
        Ok(Expr::Func(func))
    }

    #[inline]
    fn parse_fn_name(&mut self, is_gen: bool) -> Res<resast::Ident<'b>> {
        debug!("{}: parse_fn_name", self.look_ahead.span.start);
        if self.context.strict && !is_gen && self.at_keyword(Keyword::Yield) {
            self.parse_ident_name()
        } else {
            self.parse_var_ident(false)
        }
    }

    #[inline]
    fn parse_ident_name(&mut self) -> Res<resast::Ident<'b>> {
        debug!("{}: parse_ident_name", self.look_ahead.span.start);
        let ident = self.next_item()?;
        let ret = self.get_string(&ident.span)?;

        Ok(resast::Ident::from(ret))
    }

    #[inline]
    fn parse_var_ident(&mut self, is_var: bool) -> Res<resast::Ident<'b>> {
        debug!("{}: parse_var_ident", self.look_ahead.span.start);
        let ident = self.next_item()?;
        if ident.token.matches_keyword(Keyword::Yield)
            && (self.context.strict || !self.context.allow_yield)
        {
            return self.expected_token_error(&ident, &["variable identifier"]);
        } else if !ident.token.is_ident() {
            if self.context.strict && ident.token.is_strict_reserved() {
                return self.expected_token_error(&ident, &["variable identifier"]);
            }
            if self.context.strict || ident.token.matches_keyword(Keyword::Let) || !is_var {
                return self.expected_token_error(&ident, &["variable identifier"]);
            }
        } else if (self.context.is_module || self.context.allow_await)
            && &self.original[ident.span.start..ident.span.end] == "await"
        {
            return self.expected_token_error(&ident, &["variable identifier"]);
        }
        let i = match ident.token {
            Token::Ident(_) | Token::Keyword(_) => {
                let s = self.get_string(&ident.span)?;
                resast::Ident::from(s)
            }
            _ => self.expected_token_error(&ident, &["variable identifier"])?,
        };
        Ok(i)
    }

    #[inline]
    fn parse_formal_params(&mut self) -> Res<FormalParams<'b>> {
        debug!("{}: parse_formal_params", self.look_ahead.span.start);
        self.expect_punct(Punct::OpenParen)?;
        let mut args = Vec::new();
        let mut simple: bool = true;
        let mut found_restricted = false;
        if !self.at_punct(Punct::CloseParen) {
            while !self.look_ahead.token.is_eof() {
                let (s, r, arg) = self.parse_formal_param(simple)?;
                simple = simple && s;
                found_restricted = found_restricted || r;
                args.push(arg);
                if self.at_punct(Punct::CloseParen) {
                    break;
                }
                self.expect_punct(Punct::Comma)?;
                if self.at_punct(Punct::CloseParen) {
                    break;
                }
            }
        }
        self.expect_punct(Punct::CloseParen)?;

        Ok(FormalParams {
            params: args,
            strict: false,
            found_restricted,
            simple,
        })
    }

    #[inline]
    fn parse_formal_param(&mut self, simple: bool) -> Res<(bool, bool, FuncArg<'b>)> {
        debug!("{}: parse_formal_param", self.look_ahead.span.start);
        let mut params: Vec<Item<Token<&'b str>>> = Vec::new();
        let (found_restricted, param) = if self.at_punct(Punct::Ellipsis) {
            let (found_restricted, pat) = self.parse_rest_element(&mut params)?;
            (found_restricted, FuncArg::Pat(pat))
        } else {
            let (found_restricted, pat) = self.parse_pattern_with_default(&mut params)?;
            (found_restricted, FuncArg::Pat(pat))
        };
        let simple = simple && Self::is_simple(&param);
        Ok((simple, found_restricted, param))
    }

    #[inline]
    fn parse_rest_element(
        &mut self,
        params: &mut Vec<Item<Token<&'b str>>>,
    ) -> Res<(bool, Pat<'b>)> {
        debug!("{}: parse_rest_element", self.look_ahead.span.start);
        self.expect_punct(Punct::Ellipsis)?;
        let (restricted, arg) = self.parse_pattern(None, params)?;
        let ret = Pat::RestElement(Box::new(arg));
        if self.at_punct(Punct::Equal) {
            return self.expected_token_error(&self.look_ahead, &["not assignment"]);
        }
        if !self.at_punct(Punct::CloseParen) {
            return self.expected_token_error(&self.look_ahead, &[")"]);
        }
        Ok((restricted, ret))
    }

    #[inline]
    fn parse_binding_rest_el(
        &mut self,
        params: &mut Vec<Item<Token<&'b str>>>,
    ) -> Res<(bool, Pat<'b>)> {
        debug!("{}: parse_binding_rest_el", self.look_ahead.span.start);
        self.expect_punct(Punct::Ellipsis)?;
        let (b, pat) = self.parse_pattern(None, params)?;
        Ok((b, Pat::RestElement(Box::new(pat))))
    }

    #[inline]
    fn parse_pattern_with_default(
        &mut self,
        params: &mut Vec<Item<Token<&'b str>>>,
    ) -> Res<(bool, Pat<'b>)> {
        debug!("{}: parse_pattern_with_default", self.look_ahead.span.start);
        let (is_restricted, ret) = self.parse_pattern(None, params)?;
        if self.at_punct(Punct::Equal) {
            let _assign = self.next_item()?;
            let prev_yield = self.context.allow_yield;
            self.context.allow_yield = true;
            let (prev_bind, prev_assign, prev_first) = self.isolate_cover_grammar();
            let right = self.parse_assignment_expr()?;
            self.set_isolate_cover_grammar_state(prev_bind, prev_assign, prev_first)?;
            self.context.allow_yield = prev_yield;
            return Ok((
                is_restricted,
                Pat::Assign(AssignPat {
                    left: Box::new(ret),
                    right: Box::new(right),
                }),
            ));
        }
        Ok((is_restricted, ret))
    }

    #[inline]
    fn parse_pattern(
        &mut self,
        kind: Option<VarKind>,
        params: &mut Vec<Item<Token<&'b str>>>,
    ) -> Res<(bool, Pat<'b>)> {
        debug!("{}: parse_pattern", self.look_ahead.span.start);
        if self.at_punct(Punct::OpenBracket) {
            let kind = kind.unwrap_or(VarKind::Var);
            self.parse_array_pattern(params, kind)
        } else if self.at_punct(Punct::OpenBrace) {
            self.parse_object_pattern()
        } else {
            let is_var = if let Some(kind) = kind {
                match kind {
                    VarKind::Const | VarKind::Let => {
                        if self.at_keyword(Keyword::Let) {
                            return self.expected_token_error(&self.look_ahead, &["identifier"]);
                        }
                        false
                    }
                    VarKind::Var => true,
                }
            } else {
                true
            };
            let ident = self.parse_var_ident(is_var)?;
            let restricted = ident.name == "eval" || ident.name == "arguments";
            params.push(self.look_ahead.clone());
            Ok((restricted, Pat::Ident(ident)))
        }
    }

    #[inline]
    fn parse_array_pattern(
        &mut self,
        params: &mut Vec<Item<Token<&'b str>>>,
        _kind: VarKind,
    ) -> Res<(bool, Pat<'b>)> {
        debug!("{}: parse_array_pattern", self.look_ahead.span.start);
        self.expect_punct(Punct::OpenBracket)?;
        let mut elements = Vec::new();
        while !self.at_punct(Punct::CloseBracket) {
            if self.at_punct(Punct::Comma) {
                let _ = self.next_item()?;
                elements.push(None);
            } else {
                if self.at_punct(Punct::Ellipsis) {
                    let (_, el) = self.parse_binding_rest_el(params)?;
                    elements.push(Some(ArrayPatPart::Pat(el)));
                    break;
                } else {
                    let (_, el) = self.parse_pattern_with_default(params)?;
                    elements.push(Some(ArrayPatPart::Pat(el)));
                }
                if !self.at_punct(Punct::CloseBracket) {
                    self.expect_punct(Punct::Comma)?;
                }
            }
        }
        self.expect_punct(Punct::CloseBracket)?;
        Ok((false, Pat::Array(elements)))
    }

    #[inline]
    fn parse_object_pattern(&mut self) -> Res<(bool, Pat<'b>)> {
        debug!("{}: parse_object_pattern", self.look_ahead.span.start);
        self.expect_punct(Punct::OpenBrace)?;
        let mut body = Vec::new();
        while !self.at_punct(Punct::CloseBrace) {
            let el = if self.at_punct(Punct::Ellipsis) {
                self.parse_rest_prop()?
            } else {
                self.parse_property_pattern()?
            };
            body.push(el);
            if !self.at_punct(Punct::CloseBrace) {
                self.expect_punct(Punct::Comma)?;
            }
        }
        self.expect_punct(Punct::CloseBrace)?;
        Ok((false, Pat::Obj(body)))
    }

    #[inline]
    fn parse_rest_prop(&mut self) -> Res<ObjPatPart<'b>> {
        debug!("{}: parse_rest_prop", self.look_ahead.span.start);
        self.expect_punct(Punct::Ellipsis)?;
        let (_, arg) = self.parse_pattern(None, &mut Vec::new())?;
        if self.at_punct(Punct::Equal) {
            //unexpected token
        }
        if !self.at_punct(Punct::CloseBrace) {
            //unable to parse props after rest
        }
        let rest = Pat::RestElement(Box::new(arg));
        let part = ObjPatPart::Rest(Box::new(rest));
        Ok(part)
    }

    #[inline]
    fn parse_property_pattern(&mut self) -> Res<ObjPatPart<'b>> {
        debug!("{}: parse_property_pattern", self.look_ahead.span.start);
        let mut computed = false;
        let mut short_hand = false;
        let method = false;
        let (key, value) = if self.look_ahead.token.is_ident() {
            let ident = self.parse_var_ident(false)?;
            let key = PropKey::Pat(Pat::Ident(ident.clone()));
            let value = if self.at_punct(Punct::Equal) {
                self.expect_punct(Punct::Equal)?;
                short_hand = true;

                let e = self.parse_assignment_expr()?;
                PropValue::Pat(Pat::Assign(AssignPat {
                    left: Box::new(Pat::Ident(ident.clone())),
                    right: Box::new(e),
                }))
            } else if !self.at_punct(Punct::Colon) {
                short_hand = true;
                PropValue::None
            } else {
                self.expect_punct(Punct::Colon)?;
                let (_, p) = self.parse_pattern_with_default(&mut Vec::new())?;
                PropValue::Pat(p)
            };
            (key, value)
        } else {
            computed = self.at_punct(Punct::OpenBracket);
            let key = self.parse_object_property_key()?;
            self.expect_punct(Punct::Colon)?;
            let (_, v) = self.parse_pattern_with_default(&mut Vec::new())?;
            let value = PropValue::Pat(v);
            (key, value)
        };
        Ok(ObjPatPart::Assign(Prop {
            key,
            value,
            computed,
            short_hand,
            method,
            kind: PropKind::Init,
            is_static: false,
        }))
    }

    #[inline]
    fn parse_assignment_expr(&mut self) -> Res<Expr<'b>> {
        debug!("{}: parse_assignment_expr", self.look_ahead.span.start);
        if !self.context.allow_yield && self.at_keyword(Keyword::Yield) {
            self.parse_yield_expr()
        } else {
            let ress::Span { start, end } = self.look_ahead.span;
            let start_pos = self.look_ahead_position;
            let mut current = self.parse_conditional_expr()?;
            let curr_line = self.look_ahead_position.line;
            let start_line = start_pos.line;
            if &self.original[start..end] == "async"
                && curr_line == start_line
                && (self.look_ahead.token.is_ident() || self.at_keyword(Keyword::Yield))
            {
                let arg = self.parse_primary_expression()?;
                let arg = self.reinterpret_expr_as_pat(arg)?;
                let arg = FuncArg::Pat(arg);
                current = Expr::ArrowParamPlaceHolder(vec![arg], true);
            }
            debug!(
                "current expression: {:?} {}",
                current, self.context.allow_yield
            );
            if Self::is_arrow_param_placeholder(&current) || self.at_punct(Punct::EqualGreaterThan)
            {
                self.context.is_assignment_target = false;
                self.context.is_binding_element = false;
                let is_async = Self::is_async(&current);
                if let Some(params) = self.reinterpret_as_cover_formals_list(current.clone())? {
                    eprintln!("before {:?} {}", self.look_ahead.token, self.scanner.pending_new_line);
                    self.expect_punct(Punct::EqualGreaterThan)?;
                    eprintln!("after {:?} {}", self.look_ahead.token, self.scanner.pending_new_line);
                    if self.at_punct(Punct::OpenBrace) {
                        let prev_in = self.context.allow_in;
                        self.context.allow_in = true;
                        let body = self.parse_function_source_el()?;
                        self.context.allow_in = prev_in;
                        current = Expr::ArrowFunc(ArrowFuncExpr {
                            id: None,
                            expression: false,
                            generator: false,
                            is_async,
                            params,
                            body: ArrowFuncBody::FuncBody(body),
                        });
                    } else {
                        let (prev_bind, prev_assign, prev_first) = self.isolate_cover_grammar();
                        let a = self.parse_assignment_expr()?;
                        self.set_isolate_cover_grammar_state(prev_bind, prev_assign, prev_first)?;
                        current = Expr::ArrowFunc(ArrowFuncExpr {
                            id: None,
                            expression: true,
                            generator: false,
                            is_async,
                            params,
                            body: ArrowFuncBody::Expr(Box::new(a)),
                        });
                    };
                }
            } else if self.at_assign() {
                if !self.context.is_assignment_target && !self.config.tolerant {
                    return self
                        .unexpected_token_error(&self.look_ahead, "Not at assignment target");
                }
                if self.context.strict && Self::is_ident(&current) {
                    if let Expr::Ident(ref i) = current {
                        if Self::is_restricted_word(i) || Self::is_strict_reserved(i) {
                            return self.expected_token_error(
                                &self.look_ahead,
                                &[&format!("not {}", i.name)],
                            );
                        }
                    }
                }
                let left = if !self.at_punct(Punct::Equal) {
                    self.context.is_assignment_target = false;
                    self.context.is_binding_element = false;
                    AssignLeft::Expr(Box::new(current))
                } else if !Self::is_ident(&current) && Self::is_reinterpret_target(&current) {
                    AssignLeft::Pat(self.reinterpret_expr_as_pat(current)?)
                } else {
                    AssignLeft::Expr(Box::new(current))
                };
                let item = self.next_item()?;
                let op = match item.token {
                    Token::Punct(ref p) => {
                        if let Some(op) = Self::assignment_operator(*p) {
                            op
                        } else {
                            return self.expected_token_error(
                                &item,
                                &[
                                    "=", "+=", "-=", "/=", "*=", "**=", "|=", "&=", "~=", "%=",
                                    "<<=", ">>=", ">>>=",
                                ],
                            );
                        }
                    }
                    _ => {
                        return self.expected_token_error(
                            &item,
                            &[
                                "=", "+=", "-=", "/=", "*=", "**=", "|=", "&=", "~=", "%=", "<<=",
                                ">>=", ">>>=",
                            ],
                        );
                    }
                };
                let (prev_bind, prev_assign, prev_first) = self.isolate_cover_grammar();
                let right = self.parse_assignment_expr()?;
                self.set_isolate_cover_grammar_state(prev_bind, prev_assign, prev_first)?;
                self.context.first_covert_initialized_name_error = None;
                return Ok(Expr::Assign(AssignExpr {
                    operator: op,
                    left,
                    right: Box::new(right),
                }));
            }
            Ok(current)
        }
    }

    #[inline]
    fn is_async(expr: &Expr) -> bool {
        match expr {
            Expr::Func(ref f) => f.is_async,
            Expr::ArrowFunc(ref f) => f.is_async,
            Expr::ArrowParamPlaceHolder(_, b) => *b,
            _ => false,
        }
    }

    fn assignment_operator(p: Punct) -> Option<AssignOp> {
        match p {
            Punct::Equal => Some(AssignOp::Equal),
            Punct::PlusEqual => Some(AssignOp::PlusEqual),
            Punct::DashEqual => Some(AssignOp::MinusEqual),
            Punct::AsteriskEqual => Some(AssignOp::TimesEqual),
            Punct::ForwardSlashEqual => Some(AssignOp::DivEqual),
            Punct::PercentEqual => Some(AssignOp::ModEqual),
            Punct::DoubleLessThanEqual => Some(AssignOp::LeftShiftEqual),
            Punct::DoubleGreaterThanEqual => Some(AssignOp::RightShiftEqual),
            Punct::TripleGreaterThanEqual => Some(AssignOp::UnsignedRightShiftEqual),
            Punct::PipeEqual => Some(AssignOp::OrEqual),
            Punct::CaretEqual => Some(AssignOp::XOrEqual),
            Punct::AmpersandEqual => Some(AssignOp::AndEqual),
            Punct::DoubleAsteriskEqual => Some(AssignOp::PowerOfEqual),
            _ => None,
        }
    }

    #[inline]
    fn is_arrow_param_placeholder(expr: &Expr) -> bool {
        match expr {
            Expr::ArrowParamPlaceHolder(_, _) => true,
            _ => false,
        }
    }

    fn reinterpret_as_cover_formals_list(
        &mut self,
        expr: Expr<'b>,
    ) -> Res<Option<Vec<FuncArg<'b>>>> {
        let (params, async_arrow) = if Self::is_ident(&expr) {
            (vec![FuncArg::Expr(expr)], false)
        } else if let Expr::ArrowParamPlaceHolder(params, is_async) = expr {
            (params, is_async)
        } else {
            return Ok(None);
        };
        let mut invalid_param = false;
        let mut params2 = Vec::with_capacity(params.len());
        for param in params {
            if Self::is_assignment(&param) {
                match &param {
                    FuncArg::Pat(ref p) => {
                        if let Pat::Assign(ref a) = p {
                            if let Expr::Yield(ref y) = &*a.right {
                                if y.argument.is_some() {
                                    invalid_param = true;
                                } else {
                                    params2.push(FuncArg::Pat(Pat::Ident(resast::Ident::from(
                                        "yield",
                                    ))));
                                    continue;
                                }
                            }
                        }
                    }
                    FuncArg::Expr(ref e) => {
                        if let Expr::Assign(ref a) = e {
                            if let Expr::Yield(ref y) = &*a.right {
                                if y.argument.is_some() {
                                    invalid_param = true;
                                } else {
                                    params2.push(FuncArg::Expr(Expr::Ident(resast::Ident::from(
                                        "yield",
                                    ))));
                                    continue;
                                }
                            }
                        }
                    }
                }
                params2.push(param)
            } else if async_arrow && Self::is_await(&param) {
                invalid_param = true;
                params2.push(param)
            } else if let FuncArg::Expr(e) = param {
                if Self::is_reinterpret_target(&e) {
                    params2.push(FuncArg::Pat(self.reinterpret_expr_as_pat(e)?));
                }
            } else if let FuncArg::Pat(p) = param {
                match p {
                    Pat::Obj(o) => {
                        let mut new_props = Vec::with_capacity(o.len());
                        for part in o {
                            match part {
                                ObjPatPart::Assign(p) => {
                                    new_props.push(ObjPatPart::Assign(self.reinterpret_prop(p)?))
                                }
                                ObjPatPart::Rest(r) => new_props.push(ObjPatPart::Rest(r)),
                            }
                        }
                        params2.push(FuncArg::Pat(Pat::Obj(new_props)))
                    }
                    _ => params2.push(FuncArg::Pat(p.clone())),
                }
            } else {
                params2.push(param)
            }
        }

        if invalid_param {
            return self.expected_token_error(
                &self.look_ahead,
                &["not a yield expression in a function param"],
            );
        }
        if self.context.strict && !self.context.allow_yield {
            for param in params2.iter() {
                if let FuncArg::Expr(ref e) = param {
                    if let Expr::Yield(_) = e {
                        return self.expected_token_error(
                            &self.look_ahead,
                            &["not a yield expression in a function param"],
                        );
                    }
                }
            }
        }
        Ok(Some(params2))
    }

    #[inline]
    fn is_await(arg: &FuncArg) -> bool {
        match arg {
            FuncArg::Expr(ref e) => match e {
                Expr::Ident(ref i) => i.name == "await",
                _ => false,
            },
            FuncArg::Pat(ref p) => match p {
                Pat::Ident(ref i) => i.name == "await",
                _ => false,
            },
        }
    }

    #[inline]
    fn is_assignment(arg: &FuncArg) -> bool {
        match arg {
            FuncArg::Pat(ref p) => match p {
                Pat::Assign(_) => true,
                _ => false,
            },
            FuncArg::Expr(ref e) => match e {
                Expr::Assign(_) => true,
                _ => false,
            },
        }
    }
    #[inline]
    pub fn is_simple(arg: &FuncArg) -> bool {
        match arg {
            FuncArg::Pat(ref p) => match p {
                Pat::Ident(_) => true,
                _ => false,
            },
            FuncArg::Expr(ref e) => match e {
                Expr::Ident(_) => true,
                _ => false,
            },
        }
    }

    fn reinterpret_expr_as_pat(&self, ex: Expr<'b>) -> Res<Pat<'b>> {
        debug!("{}: reinterpret_expr_as_pat", self.look_ahead.span.start);
        match ex {
            Expr::Array(a) => {
                let mut parts = Vec::with_capacity(a.len());
                for e in a {
                    if let Some(ex) = e {
                        let part = if Self::is_reinterpret_target(&ex) {
                            let pat = self.reinterpret_expr_as_pat(ex)?;
                            ArrayPatPart::Pat(pat)
                        } else {
                            ArrayPatPart::Expr(ex)
                        };
                        parts.push(Some(part));
                    } else {
                        parts.push(None);
                    }
                }
                Ok(Pat::Array(parts))
            }
            Expr::Spread(s) => Ok(Pat::RestElement(Box::new(
                self.reinterpret_expr_as_pat(*s)?,
            ))),
            Expr::Obj(o) => {
                let mut patts = Vec::new();
                for expr in o {
                    match expr {
                        ObjProp::Prop(p) => {
                            let prop = self.reinterpret_prop(p)?;
                            patts.push(ObjPatPart::Assign(prop))
                        }
                        ObjProp::Spread(s) => {
                            let p = self.reinterpret_expr_as_pat(s)?;
                            patts.push(ObjPatPart::Rest(Box::new(p)));
                        }
                    }
                }
                Ok(Pat::Obj(patts))
            }
            Expr::Assign(a) => {
                let left = match a.left {
                    AssignLeft::Pat(p) => p,
                    AssignLeft::Expr(e) => self.reinterpret_expr_as_pat(*e)?,
                };
                let ret = AssignPat {
                    left: Box::new(left),
                    right: a.right,
                };
                Ok(Pat::Assign(ret))
            }
            Expr::Ident(i) => Ok(Pat::Ident(i)),
            _ => Err(self.reinterpret_error(&format!("expression: {:?}", ex), "pattern")),
        }
    }

    #[inline]
    fn is_reinterpret_target(ex: &Expr) -> bool {
        match ex {
            Expr::Ident(_) => true,
            Expr::Spread(ref s) => Self::is_reinterpret_target(s),
            Expr::Obj(_) => true,
            Expr::Array(_) => true,
            Expr::Assign(ref a) => match a.left {
                AssignLeft::Expr(ref expr) => Self::is_reinterpret_target(expr),
                _ => true,
            },
            _ => false,
        }
    }

    #[inline]
    fn reinterpret_prop(&self, p: Prop<'b>) -> Res<Prop<'b>> {
        let Prop {
            key,
            computed,
            is_static,
            kind,
            short_hand,
            method,
            value,
        } = p;

        let key = if let PropKey::Expr(expr) = key {
            if Self::is_reinterpret_target(&expr) {
                PropKey::Pat(self.reinterpret_expr_as_pat(expr)?)
            } else {
                PropKey::Expr(expr)
            }
        } else {
            key
        };
        let value = if let PropValue::Expr(expr) = value {
            if Self::is_reinterpret_target(&expr) {
                PropValue::Pat(self.reinterpret_expr_as_pat(expr)?)
            } else {
                PropValue::Expr(expr)
            }
        } else {
            value
        };
        Ok(Prop {
            key,
            computed,
            is_static,
            kind,
            short_hand,
            method,
            value,
        })
    }

    #[inline]
    fn parse_yield_expr(&mut self) -> Res<Expr<'b>> {
        debug!("{}: parse_yield_expr", self.look_ahead.span.start);
        self.expect_keyword(Keyword::Yield)?;
        let mut arg: Option<Box<Expr>> = None;
        let mut delegate = false;
        if !self.context.has_line_term {
            let prev_yield = self.context.allow_yield;
            self.context.allow_yield = false;
            delegate = self.at_punct(Punct::Asterisk);
            if delegate {
                let _start = self.next_item()?;
                arg = Some(Box::new(self.parse_assignment_expr()?));
            } else if self.is_start_of_expr() {
                arg = Some(Box::new(self.parse_assignment_expr()?));
            }
            self.context.allow_yield = prev_yield;
        }
        let y = YieldExpr {
            argument: arg,
            delegate,
        };
        Ok(Expr::Yield(y))
    }

    #[inline]
    fn parse_conditional_expr(&mut self) -> Res<Expr<'b>> {
        debug!("{}: parse_conditional_expr", self.look_ahead.span.start);
        let (prev_bind, prev_assign, prev_first) = self.inherit_cover_grammar();
        let expr = self.parse_binary_expression()?;
        self.set_inherit_cover_grammar_state(prev_bind, prev_assign, prev_first);
        if self.at_punct(Punct::QuestionMark) {
            let _question_mark = self.next_item()?;
            let prev_in = self.context.allow_in;
            self.context.allow_in = true;
            let (prev_bind, prev_assign, prev_first) = self.isolate_cover_grammar();
            let if_true = self.parse_assignment_expr()?;
            self.set_isolate_cover_grammar_state(prev_bind, prev_assign, prev_first)?;
            self.context.allow_in = prev_in;

            self.expect_punct(Punct::Colon)?;
            let (prev_bind, prev_assign, prev_first) = self.isolate_cover_grammar();
            let if_false = self.parse_assignment_expr()?;
            self.set_isolate_cover_grammar_state(prev_bind, prev_assign, prev_first)?;

            let c = ConditionalExpr {
                test: Box::new(expr),
                alternate: Box::new(if_false),
                consequent: Box::new(if_true),
            };
            self.context.is_assignment_target = false;
            self.context.is_binding_element = false;
            return Ok(Expr::Conditional(c));
        }
        Ok(expr)
    }

    #[inline]
    fn parse_binary_expression(&mut self) -> Res<Expr<'b>> {
        debug!("{}: parse_binary_expression", self.look_ahead.span.start);
        let (prev_bind, prev_assign, prev_first) = self.inherit_cover_grammar();
        let mut current = self.parse_exponentiation_expression()?;
        self.set_inherit_cover_grammar_state(prev_bind, prev_assign, prev_first);
        let token = self.look_ahead.clone();
        let mut prec = self.bin_precedence(&token.token);
        if prec > 0 {
            self.next_item()?;
            self.context.is_assignment_target = false;
            self.context.is_binding_element = false;
            let mut left = current.clone();
            let (prev_bind, prev_assign, prev_first) = self.isolate_cover_grammar();
            self.set_isolate_cover_grammar_state(prev_bind, prev_assign, prev_first)?;
            let (prev_bind, prev_assign, prev_first) = self.isolate_cover_grammar();
            let mut right = self.parse_exponentiation_expression()?;
            self.set_isolate_cover_grammar_state(prev_bind, prev_assign, prev_first)?;
            let mut stack = vec![left.clone(), right.clone()];
            let mut ops = vec![token.token.clone()];
            let mut precs = vec![prec];
            loop {
                prec = self.bin_precedence(&self.look_ahead.token);
                if prec < 1 {
                    break;
                }
                debug!(
                    "shifting, stack: {}, ops: {}, last_prec: {} {}",
                    stack.len(),
                    ops.len(),
                    precs[precs.len() - 1],
                    self.context.allow_yield
                );
                while !stack.is_empty() && !ops.is_empty() && prec <= precs[precs.len() - 1] {
                    right = stack.pop().ok_or_else(|| {
                        self.op_error("invalid binary operation, no right expr in stack")
                    })?;
                    debug!("right: {:#?} {}", right, self.context.allow_yield);
                    let op = ops.pop().ok_or_else(|| {
                        self.op_error("invalid binary operation, too few operators")
                    })?;
                    let _ = precs.pop();
                    left = stack.pop().ok_or_else(|| {
                        self.op_error("invalid binary operation, no left expr in stack")
                    })?;
                    debug!("left: {:#?} {}", left, self.context.allow_yield);
                    if op.matches_punct(Punct::DoubleAmpersand)
                        || op.matches_punct(Punct::DoublePipe)
                    {
                        stack.push(Expr::Logical(LogicalExpr {
                            operator: Self::logical_operator(&op).ok_or_else(|| {
                                self.op_error("Unable to convert logical operator")
                            })?,
                            left: Box::new(left),
                            right: Box::new(right),
                        }));
                    } else {
                        let operator = Self::binary_operator(&op)
                            .ok_or_else(|| self.op_error("Unable to convert binary operator"))?;
                        stack.push(Expr::Binary(BinaryExpr {
                            operator,
                            left: Box::new(left),
                            right: Box::new(right),
                        }));
                    }
                }
                ops.push(self.next_item()?.token);
                precs.push(prec);
                let (prev_bind, prev_assign, prev_first) = self.isolate_cover_grammar();
                let exp = self.parse_exponentiation_expression()?;
                self.set_isolate_cover_grammar_state(prev_bind, prev_assign, prev_first)?;
                stack.push(exp);
            }
            current = stack
                .pop()
                .ok_or_else(|| self.op_error("invalid binary operation, too few expressions"))?;

            while !ops.is_empty() && !stack.is_empty() {
                let op = ops
                    .pop()
                    .ok_or_else(|| self.op_error("invalid binary operation, too few operators"))?;
                if op.matches_punct(Punct::DoubleAmpersand) || op.matches_punct(Punct::DoublePipe) {
                    let operator = Self::logical_operator(&op)
                        .ok_or_else(|| self.op_error("Unable to convert logical operator"))?;
                    current = Expr::Logical(LogicalExpr {
                        operator,
                        left: Box::new(stack.pop().ok_or_else(|| {
                            self.op_error("invalid logical operation, too few expressions")
                        })?),
                        right: Box::new(current),
                    })
                } else {
                    let operator = Self::binary_operator(&op)
                        .ok_or_else(|| self.op_error("Unable to convert binary operator"))?;
                    current = Expr::Binary(BinaryExpr {
                        operator,
                        left: Box::new(stack.pop().ok_or_else(|| {
                            self.op_error("invalid binary operation, too few expressions")
                        })?),
                        right: Box::new(current),
                    });
                }
            }
        }
        Ok(current)
    }

    #[inline]
    fn parse_exponentiation_expression(&mut self) -> Res<Expr<'b>> {
        debug!(
            "{}: parse_exponentiation_expression",
            self.look_ahead.span.start
        );
        let (prev_bind, prev_assign, prev_first) = self.inherit_cover_grammar();
        let expr = self.parse_unary_expression()?;
        self.set_inherit_cover_grammar_state(prev_bind, prev_assign, prev_first);
        if self.at_punct(Punct::DoubleAsterisk) {
            let _stars = self.next_item()?;
            self.context.is_assignment_target = false;
            self.context.is_binding_element = false;
            let left = expr;
            let (prev_bind, prev_assign, prev_first) = self.isolate_cover_grammar();
            let right = self.parse_exponentiation_expression()?;
            self.set_isolate_cover_grammar_state(prev_bind, prev_assign, prev_first)?;
            return Ok(Expr::Binary(BinaryExpr {
                operator: BinaryOp::PowerOf,
                left: Box::new(left),
                right: Box::new(right),
            }));
        }

        Ok(expr)
    }

    #[inline]
    fn parse_unary_expression(&mut self) -> Res<Expr<'b>> {
        debug!("{}: parse_unary_expression", self.look_ahead.span.start);
        if self.at_punct(Punct::Plus)
            || self.at_punct(Punct::Dash)
            || self.at_punct(Punct::Tilde)
            || self.at_punct(Punct::Bang)
            || self.at_keyword(Keyword::Delete)
            || self.at_keyword(Keyword::Void)
            || self.at_keyword(Keyword::TypeOf)
        {
            let op = self.next_item()?;
            let (prev_bind, prev_assign, prev_first) = self.inherit_cover_grammar();
            let arg = self.parse_unary_expression()?;
            self.set_inherit_cover_grammar_state(prev_bind, prev_assign, prev_first);
            if op.token.matches_keyword(Keyword::Delete)
                && self.context.strict
                && Self::is_ident(&arg)
                && !self.config.tolerant
            {
                return self.unexpected_token_error(&op, "Cannot delete ident in strict mode");
            }
            self.context.is_assignment_target = false;
            self.context.is_binding_element = false;
            let operator = Self::unary_operator(&op.token)
                .ok_or_else(|| self.op_error("Unable to convert unary operator"))?;
            Ok(Expr::Unary(UnaryExpr {
                prefix: true,
                operator,
                argument: Box::new(arg),
            }))
        } else if self.context.allow_await && self.at_keyword(Keyword::Await) {
            self.parse_await_expr()
        } else {
            self.parse_update_expr()
        }
    }

    fn unary_operator(token: &Token<&str>) -> Option<UnaryOp> {
        match token {
            Token::Punct(ref p) => match p {
                Punct::Dash => Some(UnaryOp::Minus),
                Punct::Plus => Some(UnaryOp::Plus),
                Punct::Bang => Some(UnaryOp::Not),
                Punct::Tilde => Some(UnaryOp::Tilde),
                _ => None,
            },
            Token::Keyword(ref k) => match k {
                Keyword::TypeOf => Some(UnaryOp::TypeOf),
                Keyword::Void => Some(UnaryOp::Void),
                Keyword::Delete => Some(UnaryOp::Delete),
                _ => None,
            },
            _ => None,
        }
    }

    fn binary_operator(token: &Token<&str>) -> Option<BinaryOp> {
        match token {
            Token::Keyword(ref key) => match key {
                Keyword::InstanceOf => Some(BinaryOp::InstanceOf),
                Keyword::In => Some(BinaryOp::In),
                _ => None,
            },
            Token::Punct(ref p) => match p {
                Punct::DoubleEqual => Some(BinaryOp::Equal),
                Punct::BangEqual => Some(BinaryOp::NotEqual),
                Punct::TripleEqual => Some(BinaryOp::StrictEqual),
                Punct::BangDoubleEqual => Some(BinaryOp::StrictNotEqual),
                Punct::LessThan => Some(BinaryOp::LessThan),
                Punct::LessThanEqual => Some(BinaryOp::LessThanEqual),
                Punct::GreaterThan => Some(BinaryOp::GreaterThan),
                Punct::GreaterThanEqual => Some(BinaryOp::GreaterThanEqual),
                Punct::DoubleLessThan => Some(BinaryOp::LeftShift),
                Punct::DoubleGreaterThan => Some(BinaryOp::RightShift),
                Punct::TripleGreaterThan => Some(BinaryOp::UnsignedRightShift),
                Punct::Plus => Some(BinaryOp::Plus),
                Punct::Dash => Some(BinaryOp::Minus),
                Punct::Asterisk => Some(BinaryOp::Times),
                Punct::ForwardSlash => Some(BinaryOp::Over),
                Punct::Percent => Some(BinaryOp::Mod),
                Punct::Ampersand => Some(BinaryOp::And),
                Punct::Pipe => Some(BinaryOp::Or),
                Punct::Caret => Some(BinaryOp::XOr),
                _ => None,
            },
            _ => None,
        }
    }

    fn logical_operator(token: &Token<&str>) -> Option<LogicalOp> {
        match token {
            Token::Punct(ref p) => match p {
                Punct::DoubleAmpersand => Some(LogicalOp::And),
                Punct::DoublePipe => Some(LogicalOp::Or),
                _ => None,
            },
            _ => None,
        }
    }

    #[inline]
    fn parse_await_expr(&mut self) -> Res<Expr<'b>> {
        debug!("{}: parse_await_expr", self.look_ahead.span.start);
        let _await = self.next_item()?;
        let arg = self.parse_unary_expression()?;
        Ok(Expr::Await(Box::new(arg)))
    }

    #[inline]
    fn parse_update_expr(&mut self) -> Res<Expr<'b>> {
        debug!("{}: parse_update_expr", self.look_ahead.span.start);
        let start = self.look_ahead.clone();
        if self.at_punct(Punct::DoublePlus) || self.at_punct(Punct::DoubleDash) {
            let op = self.next_item()?;
            let operator = match op.token {
                Token::Punct(ref p) => match p {
                    Punct::DoublePlus => UpdateOp::Increment,
                    Punct::DoubleDash => UpdateOp::Decrement,
                    _ => unreachable!("Already validated that the next token would be ++ or --"),
                },
                _ => unreachable!("Already validated that the next token would be ++ or --"),
            };
            let start = self.look_ahead.clone();
            let (prev_bind, prev_assign, prev_first) = self.inherit_cover_grammar();
            let ex = self.parse_unary_expression()?;
            self.set_inherit_cover_grammar_state(prev_bind, prev_assign, prev_first);
            if let Expr::Ident(ref i) = ex {
                if Self::is_restricted_word(i) && self.context.strict {
                    return self.unexpected_token_error(&start, "restricted ident");
                }
            }
            if !self.context.is_assignment_target && !self.config.tolerant {
                return self
                    .unexpected_token_error(&op, "Cannot increment when not at assignment target");
            }
            let prefix = true;
            let ret = UpdateExpr {
                operator,
                argument: Box::new(ex),
                prefix,
            };
            self.context.is_assignment_target = false;
            self.context.is_binding_element = false;
            Ok(Expr::Update(ret))
        } else {
            let (prev_bind, prev_assign, prev_first) = self.inherit_cover_grammar();
            let expr = self.parse_left_hand_side_expr_allow_call()?;
            self.set_inherit_cover_grammar_state(prev_bind, prev_assign, prev_first);
            if !self.context.has_line_term
                && self.look_ahead.token.is_punct()
                && (self.at_punct(Punct::DoublePlus) || self.at_punct(Punct::DoubleDash))
            {
                if let Expr::Ident(ref i) = expr {
                    if self.context.strict && Self::is_restricted_word(i) {
                        return self.expected_token_error(&start, &[]);
                    }
                }
                let op = self.next_item()?;
                if !self.context.is_assignment_target && !self.config.tolerant {
                    return self.unexpected_token_error(
                        &op,
                        "Cannot increment when not at assignment target",
                    );
                }
                self.context.is_assignment_target = false;
                self.context.is_binding_element = false;
                let prefix = false;
                let ret = UpdateExpr {
                    operator: if op.token.matches_punct(Punct::DoublePlus) {
                        UpdateOp::Increment
                    } else if op.token.matches_punct(Punct::DoubleDash) {
                        UpdateOp::Decrement
                    } else {
                        return self.expected_token_error(&op, &["++", "--"]);
                    },
                    argument: Box::new(expr),
                    prefix,
                };
                return Ok(Expr::Update(ret));
            }
            Ok(expr)
        }
    }

    #[inline]
    fn is_ident(expr: &Expr) -> bool {
        match expr {
            Expr::Ident(_) => true,
            _ => false,
        }
    }

    #[inline]
    fn parse_left_hand_side_expr(&mut self) -> Res<Expr<'b>> {
        if !self.context.allow_in {
            return Err(Error::InvalidUseOfContextualKeyword(
                self.current_position,
                "in".to_string(),
            ));
        }
        let mut expr = if self.at_keyword(Keyword::Super) && self.context.in_function_body {
            self.parse_super()?
        } else {
            let (prev_bind, prev_assign, prev_first) = self.inherit_cover_grammar();
            let ret = if self.at_keyword(Keyword::New) {
                self.parse_new_expr()?
            } else {
                self.parse_primary_expression()?
            };
            self.set_inherit_cover_grammar_state(prev_bind, prev_assign, prev_first);
            ret
        };
        loop {
            if self.at_punct(Punct::OpenBracket) {
                self.context.is_binding_element = false;
                self.context.is_assignment_target = true;
                self.expect_punct(Punct::OpenBracket)?;
                let (prev_bind, prev_assign, prev_first) = self.isolate_cover_grammar();
                let prop = self.parse_expression()?;
                self.set_isolate_cover_grammar_state(prev_bind, prev_assign, prev_first)?;
                self.expect_punct(Punct::CloseBracket)?;
                let member = MemberExpr {
                    computed: true,
                    object: Box::new(expr),
                    property: Box::new(prop),
                };
                debug!(target: "look_ahead", "{:?}", member);
                expr = Expr::Member(member);
            } else if self.at_punct(Punct::Period) {
                self.context.is_binding_element = false;
                self.context.is_assignment_target = false;
                self.expect_punct(Punct::Period)?;
                let prop = self.parse_ident_name()?;
                let member = MemberExpr {
                    object: Box::new(expr),
                    property: Box::new(Expr::Ident(prop)),
                    computed: false,
                };
                expr = Expr::Member(member);
            } else if self.look_ahead.is_template() {
                let quasi = self.parse_template_lit()?;
                expr = Expr::TaggedTemplate(TaggedTemplateExpr {
                    tag: Box::new(expr),
                    quasi,
                });
            } else {
                break;
            }
        }
        Ok(expr)
    }

    #[inline]
    fn parse_super(&mut self) -> Res<Expr<'b>> {
        self.expect_keyword(Keyword::Super)?;
        if !self.at_punct(Punct::OpenBracket) && !self.at_punct(Punct::Period) {
            return self.expected_token_error(&self.look_ahead, &["[", "."]);
        }
        Ok(Expr::Super)
    }

    #[inline]
    fn parse_left_hand_side_expr_allow_call(&mut self) -> Res<Expr<'b>> {
        debug!(
            "{}: parse_left_hand_side_expr_allow_call",
            self.look_ahead.span.start
        );
        let start_pos = self.look_ahead_position;
        let is_async = self.at_contextual_keyword("async");
        let prev_in = self.context.allow_in;
        self.context.allow_in = true;

        let mut expr = if self.at_keyword(Keyword::Super) && self.context.in_function_body {
            let _ = self.next_item()?;
            if !self.at_punct(Punct::OpenParen)
                && !self.at_punct(Punct::Period)
                && !self.at_punct(Punct::OpenBracket)
            {
                return self.expected_token_error(&self.look_ahead, &["(", ".", "["]);
            }
            Expr::Super
        } else {
            let (prev_bind, prev_assign, prev_first) = self.inherit_cover_grammar();
            let ret = if self.at_keyword(Keyword::New) {
                self.parse_new_expr()?
            } else {
                self.parse_primary_expression()?
            };
            self.set_inherit_cover_grammar_state(prev_bind, prev_assign, prev_first);
            ret
        };
        loop {
            if self.at_punct(Punct::Period) {
                self.context.is_binding_element = false;
                self.context.is_assignment_target = true;
                self.expect_punct(Punct::Period)?;
                let prop = Expr::Ident(self.parse_ident_name()?);
                expr = Expr::Member(MemberExpr {
                    object: Box::new(expr),
                    property: Box::new(prop),
                    computed: false,
                });
                debug!(target: "look_ahead", "1 {:?}", expr);
            } else if self.at_punct(Punct::OpenParen) {
                let current_pos = self.look_ahead_position;
                let async_arrow = is_async && start_pos.line == current_pos.line;
                self.context.is_binding_element = false;
                self.context.is_assignment_target = false;
                let args = if async_arrow {
                    self.parse_async_args()?
                } else {
                    self.parse_args()?
                };
                //TODO: check for bad import call
                if async_arrow && self.at_punct(Punct::EqualGreaterThan) {
                    let args = args.into_iter().map(FuncArg::Expr).collect();
                    expr = Expr::ArrowParamPlaceHolder(args, true);
                } else {
                    let inner = CallExpr {
                        callee: Box::new(expr),
                        arguments: args,
                    };
                    expr = Expr::Call(inner);
                }
            } else if self.at_punct(Punct::OpenBracket) {
                self.context.is_assignment_target = true;
                self.context.is_binding_element = false;
                self.expect_punct(Punct::OpenBracket)?;
                let (prev_bind, prev_assign, prev_first) = self.isolate_cover_grammar();
                let prop = self.parse_expression()?;
                self.set_isolate_cover_grammar_state(prev_bind, prev_assign, prev_first)?;
                self.expect_punct(Punct::CloseBracket)?;
                let member = MemberExpr {
                    object: Box::new(expr),
                    computed: true,
                    property: Box::new(prop),
                };
                debug!(target: "look_ahead", "{:?}", member);
                expr = Expr::Member(member);
            } else if self.look_ahead.token.is_template_head() {
                let quasi = self.parse_template_lit()?;
                let temp = TaggedTemplateExpr {
                    tag: Box::new(expr),
                    quasi,
                };
                expr = Expr::TaggedTemplate(temp);
            } else {
                break;
            }
        }
        self.context.allow_in = prev_in;
        Ok(expr)
    }
    /// Parse the arguments of an async function
    #[inline]
    fn parse_async_args(&mut self) -> Res<Vec<Expr<'b>>> {
        debug!("{}: parse_async_args", self.look_ahead.span.start);
        self.expect_punct(Punct::OpenParen)?;
        let mut ret = Vec::new();
        if !self.at_punct(Punct::CloseParen) {
            loop {
                let arg = if self.at_punct(Punct::Ellipsis) {
                    self.parse_spread_element()?
                } else {
                    let (prev_bind, prev_assign, prev_first) = self.isolate_cover_grammar();
                    let arg = self.parse_async_arg()?;
                    self.set_isolate_cover_grammar_state(prev_bind, prev_assign, prev_first)?;
                    arg
                };
                ret.push(arg);
                if self.at_punct(Punct::CloseParen) {
                    break;
                }
                self.expect_comma_sep()?;
                if self.at_punct(Punct::CloseParen) {
                    break;
                }
            }
        }
        self.expect_punct(Punct::CloseParen)?;
        Ok(ret)
    }
    /// Parse an argument of an async function
    /// note: not sure this is needed
    #[inline]
    fn parse_async_arg(&mut self) -> Res<Expr<'b>> {
        debug!("{}: parse_async_arg", self.look_ahead.span.start);
        let expr = self.parse_assignment_expr()?;
        self.context.first_covert_initialized_name_error = None;
        Ok(expr)
    }
    /// Expect a comma separator,
    /// if parsing with tolerance we can tolerate
    /// a non-existent comma
    #[inline]
    fn expect_comma_sep(&mut self) -> Res<()> {
        debug!("{}: expect_comma_sep", self.look_ahead.span.start);
        self.expect_punct(Punct::Comma)
    }

    /// Parse an expression preceded by the `...` operator
    #[inline]
    fn parse_spread_element(&mut self) -> Res<Expr<'b>> {
        debug!("{}: parse_spread_element", self.look_ahead.span.start);
        self.expect_punct(Punct::Ellipsis)?;
        let (prev_bind, prev_assign, prev_first) = self.inherit_cover_grammar();
        let arg = self.parse_assignment_expr()?;
        self.set_inherit_cover_grammar_state(prev_bind, prev_assign, prev_first);
        Ok(Expr::Spread(Box::new(arg)))
    }
    /// Parse function arguments, expecting to open with `(` and close with `)`
    #[inline]
    fn parse_args(&mut self) -> Res<Vec<Expr<'b>>> {
        debug!("{}: parse_args", self.look_ahead.span.start);
        self.expect_punct(Punct::OpenParen)?;
        let mut args = Vec::new();
        if !self.at_punct(Punct::CloseParen) {
            loop {
                let expr = if self.at_punct(Punct::Ellipsis) {
                    self.parse_spread_element()?
                } else {
                    let (prev_bind, prev_assign, prev_first) = self.isolate_cover_grammar();
                    let expr = self.parse_assignment_expr()?;
                    self.set_isolate_cover_grammar_state(prev_bind, prev_assign, prev_first)?;
                    expr
                };
                args.push(expr);
                if self.at_punct(Punct::CloseParen) {
                    break;
                }
                self.expect_comma_sep()?;
                if self.at_punct(Punct::CloseParen) {
                    break;
                }
            }
        }
        self.expect_punct(Punct::CloseParen)?;
        Ok(args)
    }
    /// This will parse one of two expressions `new Thing()`
    /// or `new.target`. The later is only valid in a function
    /// body
    #[inline]
    fn parse_new_expr(&mut self) -> Res<Expr<'b>> {
        debug!("{}: parse_new_expr", self.look_ahead.span.start);
        self.expect_keyword(Keyword::New)?;
        if self.at_punct(Punct::Period) {
            let _ = self.next_item()?;
            if self.at_contextual_keyword("target") && self.context.in_function_body {
                let property = self.parse_ident_name()?;
                Ok(Expr::MetaProp(MetaProp {
                    meta: resast::Ident::from("new"),
                    property,
                }))
            } else {
                self.expected_token_error(&self.look_ahead, &["[constructor function call]"])
            }
        } else if self.at_keyword(Keyword::Import) {
            self.expected_token_error(&self.look_ahead, &["not import"])
        } else {
            let (prev_bind, prev_assign, prev_first) = self.isolate_cover_grammar();
            let callee = self.parse_left_hand_side_expr()?;
            self.set_isolate_cover_grammar_state(prev_bind, prev_assign, prev_first)?;
            let args = if self.at_punct(Punct::OpenParen) {
                self.parse_args()?
            } else {
                Vec::new()
            };
            self.context.is_assignment_target = false;
            self.context.is_binding_element = false;
            let new = NewExpr {
                callee: Box::new(callee),
                arguments: args,
            };

            Ok(Expr::New(new))
        }
    }
    /// Determine the precedence for a specific token,
    /// this will return zero for all tokens except
    /// `instanceOf`, `in`, or binary punctuation
    fn bin_precedence(&self, tok: &Token<&str>) -> usize {
        match tok {
            Token::Punct(ref p) => Self::determine_precedence(*p),
            Token::Keyword(ref k) => {
                if k == &Keyword::InstanceOf || (self.context.allow_in && k == &Keyword::In) {
                    7
                } else {
                    0
                }
            }
            _ => 0,
        }
    }
    /// Determine the precedence for a specific
    /// punctuation
    fn determine_precedence(p: Punct) -> usize {
        match p {
            Punct::CloseParen
            | Punct::SemiColon
            | Punct::Comma
            | Punct::Equal
            | Punct::CloseBracket => 0,
            Punct::DoublePipe => 1,
            Punct::DoubleAmpersand => 2,
            Punct::Pipe => 3,
            Punct::Caret => 4,
            Punct::Ampersand => 5,
            Punct::DoubleEqual | Punct::BangEqual | Punct::TripleEqual | Punct::BangDoubleEqual => {
                6
            }
            Punct::GreaterThan
            | Punct::LessThan
            | Punct::LessThanEqual
            | Punct::GreaterThanEqual => 7,
            Punct::DoubleLessThan | Punct::DoubleGreaterThan | Punct::TripleGreaterThan => 8,
            Punct::Plus | Punct::Dash => 9,
            Punct::Asterisk | Punct::ForwardSlash | Punct::Percent => 11,
            _ => 0,
        }
    }
    /// Set the state back to the previous state
    /// isolating the previous state
    #[inline]
    fn set_isolate_cover_grammar_state(
        &mut self,
        prev_bind: bool,
        prev_assign: bool,
        prev_first: Option<Item<Token<&'b str>>>,
    ) -> Res<()> {
        if let Some(ref _e) = prev_first {
            //FIXME this needs to do something
            //like an error?
        }
        self.context.is_binding_element = prev_bind;
        self.context.is_assignment_target = prev_assign;
        self.context.first_covert_initialized_name_error = prev_first;
        Ok(())
    }
    /// Get the context state in order to isolate this state from the
    /// following operation
    #[inline]
    fn isolate_cover_grammar(&mut self) -> (bool, bool, Option<Item<Token<&'b str>>>) {
        debug!("{}: isolate_cover_grammar", self.look_ahead.span.start);
        let ret = self.get_cover_grammar_state();
        self.context.is_binding_element = true;
        self.context.is_assignment_target = true;
        self.context.first_covert_initialized_name_error = None;
        ret
    }
    /// Get the context state for cover grammar operations
    fn get_cover_grammar_state(&self) -> (bool, bool, Option<Item<Token<&'b str>>>) {
        (
            self.context.is_binding_element,
            self.context.is_assignment_target,
            self.context.first_covert_initialized_name_error.clone(),
        )
    }
    /// Set the context state to the provided values,
    /// inheriting the previous state
    fn set_inherit_cover_grammar_state(
        &mut self,
        is_binding_element: bool,
        is_assignment: bool,
        first_covert_initialized_name_error: Option<Item<Token<&'b str>>>,
    ) {
        self.context.is_binding_element = self.context.is_binding_element && is_binding_element;
        self.context.is_assignment_target = self.context.is_assignment_target && is_assignment;
        if first_covert_initialized_name_error.is_some() {
            self.context.first_covert_initialized_name_error = first_covert_initialized_name_error;
        }
    }
    /// Capture the context state for a binding_element, assignment and
    /// first_covert_initialized_name
    fn inherit_cover_grammar(&mut self) -> (bool, bool, Option<Item<Token<&'b str>>>) {
        trace!("inherit_cover_grammar");
        let ret = self.get_cover_grammar_state();
        self.context.is_binding_element = true;
        self.context.is_assignment_target = true;
        self.context.first_covert_initialized_name_error = None;
        ret
    }
    /// Request the next token from the scanner
    /// swap the last look ahead with this new token
    /// and return the last token
    fn next_item(&mut self) -> Res<Item<Token<&'b str>>> {
        loop {
            self.context.has_line_term = self.scanner.pending_new_line;
            if let Some(look_ahead) = self.scanner.next() {
                let look_ahead = look_ahead?;
                if cfg!(feature = "debug_look_ahead") {
                    self._look_ahead = format!(
                        "{:?}: {:?}",
                        look_ahead.token,
                        self.scanner.string_for(&look_ahead.span)
                    );
                    debug!("look_ahead: {:?}", self._look_ahead);
                }
                self.look_ahead_position = look_ahead.location.start;
                if look_ahead.token.is_comment() {
                    self.comment_handler.handle_comment(look_ahead);
                    continue;
                }
                self.current_position = self.look_ahead_position;
                let ret = replace(&mut self.look_ahead, look_ahead);
                return Ok(ret);
            } else {
                // if the next item is None, the iterator is spent
                // if the last token was EOF then we want to return that
                // and mark that we have found EOF, if we get here a second
                // time we want to return the ParseAfterEoF error
                if self.look_ahead.token.is_eof() {
                    if self.found_eof {
                        return Err(Error::ParseAfterEoF);
                    } else {
                        self.found_eof = true;
                        return Ok(self.look_ahead.clone());
                    }
                } else {
                    return Err(Error::UnexpectedEoF);
                }
            }
        }
    }

    /// Get the next token and validate that it matches
    /// the punct provided, discarding the result
    /// if it does
    #[inline]
    fn expect_punct(&mut self, p: Punct) -> Res<()> {
        let next = self.next_item()?;
        if !next.token.matches_punct(p) {
            return self.expected_token_error(&next, &[&format!("{:?}", p)]);
        }
        Ok(())
    }
    /// move on to the next item and validate it matches
    /// the keyword provided, discarding the result
    /// if it does
    #[inline]
    fn expect_keyword(&mut self, k: Keyword) -> Res<()> {
        let next = self.next_item()?;
        if !next.token.matches_keyword(k) {
            return self.expected_token_error(&next, &[&format!("{:?}", k)]);
        }
        Ok(())
    }
    #[inline]
    fn at_return_arg(&self) -> bool {
        if self.context.has_line_term {
            return self.look_ahead.is_string() || self.look_ahead.is_template();
        }
        !self.at_punct(Punct::SemiColon)
            && !self.at_punct(Punct::CloseBrace)
            && !self.look_ahead.is_eof()
    }
    #[inline]
    fn at_import_call(&mut self) -> bool {
        debug!("{}: at_import_call", self.look_ahead.span.start);
        if self.at_keyword(Keyword::Import) {
            let state = self.scanner.get_state();
            self.scanner.skip_comments().unwrap();
            let ret = if let Some(next) = self.scanner.next() {
                let next = next.unwrap();
                next.token.matches_punct(Punct::OpenParen)
            } else {
                false
            };
            self.scanner.set_state(state);
            ret
        } else {
            false
        }
    }
    #[inline]
    fn at_qualified_prop_key(&self) -> bool {
        match &self.look_ahead.token {
            Token::Ident(_)
            | Token::String(_)
            | Token::Boolean(_)
            | Token::Null
            | Token::Keyword(_) => true,
            Token::Number(_) => {
                let start = self.look_ahead.span.end + 1;
                if let Some(n) = self.scanner.str_for(&Span {
                    start,
                    end: start + 1,
                }) {
                    n != "n"
                } else {
                    false
                }
            }
            Token::Punct(ref p) => p == &Punct::OpenBracket,
            _ => false,
        }
    }

    /// Lexical declarations require the next token
    /// (not including any comments)
    /// must be an identifier, `let`, `yield`
    /// `{`, or `[`
    #[inline]
    fn at_lexical_decl(&mut self) -> bool {
        let state = self.scanner.get_state();
        self.scanner.skip_comments().unwrap();
        let ret = if let Some(next) = self.scanner.next() {
            let next = next.unwrap();
            next.token.is_ident()
                || next.token.matches_punct(Punct::OpenBracket)
                || next.token.matches_punct(Punct::OpenBrace)
                || next.token.matches_keyword(Keyword::Let)
                || next.token.matches_keyword(Keyword::Yield)
        } else {
            false
        };
        self.scanner.set_state(state);
        ret
    }
    /// Test for if the next token is a specific punct
    #[inline]
    fn at_punct(&self, p: Punct) -> bool {
        self.look_ahead.token.matches_punct(p)
    }
    /// Test for if the next token is a specific keyword
    #[inline]
    fn at_keyword(&self, k: Keyword) -> bool {
        self.look_ahead.token.matches_keyword(k)
    }
    /// This test is for all the operators that might be part
    /// of an assignment statement
    #[inline]
    fn at_assign(&self) -> bool {
        self.look_ahead.token.matches_punct(Punct::Equal)
            || self.look_ahead.token.matches_punct(Punct::AsteriskEqual)
            || self
                .look_ahead
                .token
                .matches_punct(Punct::DoubleAsteriskEqual)
            || self
                .look_ahead
                .token
                .matches_punct(Punct::ForwardSlashEqual)
            || self.look_ahead.token.matches_punct(Punct::PercentEqual)
            || self.look_ahead.token.matches_punct(Punct::PlusEqual)
            || self.look_ahead.token.matches_punct(Punct::DashEqual)
            || self
                .look_ahead
                .token
                .matches_punct(Punct::DoubleLessThanEqual)
            || self
                .look_ahead
                .token
                .matches_punct(Punct::DoubleGreaterThanEqual)
            || self
                .look_ahead
                .token
                .matches_punct(Punct::TripleGreaterThanEqual)
            || self.look_ahead.token.matches_punct(Punct::PipeEqual)
            || self.look_ahead.token.matches_punct(Punct::CaretEqual)
            || self.look_ahead.token.matches_punct(Punct::AmpersandEqual)
    }
    /// The keyword `async` is conditional, that means to decided
    /// if we are actually at an async function we need to check the
    /// next token would need to be on the same line
    #[inline]
    fn at_async_function(&mut self) -> bool {
        debug!("{}: at_async_function", self.look_ahead.span.start);
        if self.at_contextual_keyword("async") {
            !self.scanner.pending_new_line
                && if let Some(peek) = self.scanner.look_ahead() {
                    if let Ok(peek) = peek {
                        peek.token.matches_keyword(Keyword::Function)
                    } else {
                        false
                    }
                } else {
                    false
                }
        } else {
            false
        }
    }
    /// Since semi-colons are options, this function will
    /// check the next token, if it is a semi-colon it will
    /// consume it otherwise we need to either be at a line terminator
    /// EoF or a close brace
    #[inline]
    fn consume_semicolon(&mut self) -> Res<()> {
        if self.at_punct(Punct::SemiColon) {
            let _semi = self.next_item()?;
        } else if !self.context.has_line_term
            && !self.look_ahead.token.is_eof()
            && !self.at_punct(Punct::CloseBrace)
        {
            return self.expected_token_error(&self.look_ahead, &["`;`", "`eof`", "`}`"]);
        }
        Ok(())
    }
    /// Tests if a token matches an &str that might represent
    /// a contextual keyword like `async`
    #[inline]
    fn at_contextual_keyword(&self, s: &str) -> bool {
        if let Some(current) = self.scanner.str_for(&self.look_ahead.span) {
            debug!("at_contextual_keyword {:?} {:?}", s, current);
            current == s
        } else {
            false
        }
    }
    /// Sort of keywords `eval` and `arguments` have
    /// a special meaning and will cause problems
    /// if used in the wrong scope
    #[inline]
    fn is_restricted_word(word: &resast::Ident) -> bool {
        &word.name == "eval" || &word.name == "arguments"
    }
    /// Check if this &str is in the list of reserved
    /// words in the context of 'use strict'
    #[inline]
    fn is_strict_reserved(word: &resast::Ident) -> bool {
        word.name == "implements"
            || word.name == "interface"
            || word.name == "package"
            || word.name == "private"
            || word.name == "protected"
            || word.name == "public"
            || word.name == "static"
            || word.name == "yield"
            || word.name == "let"
    }
    /// Tests if the parser is currently at the
    /// start of an expression. This consists of a
    /// subset of punctuation, keywords or a regex Lit
    #[inline]
    fn is_start_of_expr(&self) -> bool {
        let mut ret = true;
        let token = &self.look_ahead.token;
        if token.is_punct() {
            ret = token.matches_punct(Punct::OpenBracket)
                || token.matches_punct(Punct::OpenParen)
                || token.matches_punct(Punct::OpenBracket)
                || token.matches_punct(Punct::Plus)
                || token.matches_punct(Punct::Dash)
                || token.matches_punct(Punct::Bang)
                || token.matches_punct(Punct::Tilde)
                || token.matches_punct(Punct::DoublePlus)
                || token.matches_punct(Punct::DoubleDash)
        }
        if token.is_keyword() {
            ret = token.matches_keyword(Keyword::Class)
                || token.matches_keyword(Keyword::Delete)
                || token.matches_keyword(Keyword::Function)
                || token.matches_keyword(Keyword::Let)
                || token.matches_keyword(Keyword::New)
                || token.matches_keyword(Keyword::Super)
                || token.matches_keyword(Keyword::This)
                || token.matches_keyword(Keyword::TypeOf)
                || token.matches_keyword(Keyword::Void)
                || token.matches_keyword(Keyword::Yield)
        }
        if token.is_regex() {
            ret = true;
        }
        ret
    }

    #[inline]
    #[inline]
    fn at_big_int_flag(&self) -> bool {
        let Span { start, end } = self.look_ahead.span;
        &self.original[start..end] == "n"
    }

    fn get_string(&self, span: &Span) -> Res<&'b str> {
        self.scanner
            .str_for(span)
            .ok_or_else(|| self.op_error("Unable to get &str from scanner"))
    }

    fn expected_token_error<T>(&self, item: &Item<Token<&'b str>>, expectation: &[&str]) -> Res<T> {
        let bt = backtrace::Backtrace::new();
        error!("{:?}", bt);
        let pos = item.location.start;
        let expectation = expectation
            .iter()
            .enumerate()
            .map(|(i, s)| {
                if i == expectation.len() - 1 && expectation.len() > 1 {
                    format!("or `{}`", s)
                } else {
                    format!("`{}`", s)
                }
            })
            .collect::<Vec<String>>()
            .join(", ");
        Err(Error::UnexpectedToken(
            pos,
            format!("Expected {}; found {:?}", expectation, item.token),
        ))
    }
    fn unexpected_token_error<T>(&self, item: &Item<Token<&'b str>>, msg: &str) -> Res<T> {
        let bt = backtrace::Backtrace::new();
        error!("{:?}", bt);
        let pos = item.location.start;

        let name = self.scanner.string_for(&item.span).unwrap_or_default();
        Err(Error::UnexpectedToken(
            pos,
            format!("Found unexpected token: {}; {}", name, msg),
        ))
    }
    fn tolerate_error(&self, err: Error) -> Result<(), Error> {
        if !self.config.tolerant {
            let bt = backtrace::Backtrace::new();
            error!("{:?}", bt);
            Err(err)
        } else {
            Ok(())
        }
    }
    fn op_error(&self, msg: &str) -> Error {
        Error::OperationError(self.current_position, msg.to_owned())
    }
    fn redecl_error(&self, name: &str) -> Error {
        Error::Redecl(self.current_position, name.to_owned())
    }
    fn reinterpret_error(&self, from: &str, to: &str) -> Error {
        Error::UnableToReinterpret(self.current_position, from.to_owned(), to.to_owned())
    }

    pub fn next_position(&self) -> SourceLocation {
        self.look_ahead.location
    }

    fn next_part(&mut self) -> Res<ProgramPart<'b>> {
        trace!("next_part past_prolog: {}, strict: {}", self.context.past_prolog, self.context.strict);
        if !self.context.past_prolog {
            if self.look_ahead.is_string() {
                let next_part = match self.parse_directive() {
                    Ok(n) => n,
                    Err(e) => {
                        self.context.errored = true;
                        return Err(e);
                    }
                };
                self.context.past_prolog = match &next_part {
                    ProgramPart::Dir(_) => false,
                    _ => true,
                };
                return Ok(next_part);
            } else {
                self.context.past_prolog = true;
            }
        }
        let ret = match self.parse_statement_list_item() {
            Ok(p) => p,
            Err(e) => {
                self.context.errored = true;
                return Err(e);
            }
        };
        trace!(target: "resp:trace", "{:?}", ret);
        Ok(ret)
    }
}

impl<'b, CH> Iterator for Parser<'b, CH>
where
    CH: CommentHandler<'b> + Sized,
{
    type Item = Res<ProgramPart<'b>>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.look_ahead.token.is_eof() || self.context.errored {
            None
        } else {
            Some(self.next_part())
        }
    }
}

#[allow(unused)]
struct FormalParams<'a> {
    simple: bool,
    params: Vec<FuncArg<'a>>,
    strict: bool,
    found_restricted: bool,
}

#[allow(unused)]
struct CoverFormalListOptions<'a> {
    simple: bool,
    params: Vec<FuncArg<'a>>,
    stricted: bool,
    first_restricted: Option<Expr<'a>>,
}
