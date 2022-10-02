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

use resast::{spanned::{
    decl::{
        Decl, DefaultExportDecl, ExportSpecifier, ImportSpecifier, ModExport, ModImport,
        NamedExportDecl, NormalImportSpec, VarDecl, Alias, VarDecls,
    },
    expr::{Expr, Lit, StringLit, TemplateLit, PropMethod, PropKey, PropValue, Prop, NewExpr},
    pat::{Pat, ArrayPatPart, RestPat},
    stmt::{
        BlockStmt, CatchClause, ForInStmt, IfStmt, LoopInit, LoopLeft, Stmt, SwitchCase,
        SwitchStmt, TryStmt, WhileStmt, WithStmt, ForOfStmt, ForStmt, DoWhileStmt, LabeledStmt, CatchArg, FinallyClause,
    },
    Dir, Func, Ident, Node, Program, ProgramPart, Slice, VarKind, FuncArg, Class, FuncBody, ClassBody,
}, PropKind};
use ress::prelude::*;
use ress::Span;

pub use crate::comment_handler::CommentHandler;
pub use crate::comment_handler::DefaultCommentHandler;
pub use crate::error::Error;
use crate::formal_params;
use crate::formal_params::FormalParams;
use crate::lexical_names;
use crate::lexical_names::DeclKind;
use crate::lhs;
use crate::LabelKind;
use crate::{Config, Context};
use std::borrow::Cow;
use std::{
    collections::{HashMap, HashSet},
    mem::replace,
};

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
    look_ahead: Item<&'a str>,
    /// Since we are looking ahead, we need
    /// to make sure we don't miss the eof
    /// by using this flag
    found_eof: bool,
    /// a possible container for tokens, currently
    /// it is unused
    _tokens: Vec<Item<&'a str>>,
    /// a possible container for comments, currently
    /// it is unused
    _comments: Vec<Item<&'a str>>,
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
        debug!(
            "{}: parse_script {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        if self.context.is_module {
            self.context.strict = true;
        }
        let body: Res<Vec<ProgramPart>> = self.collect();
        Ok(if self.context.is_module {
            Program::Mod(body?)
        } else {
            Program::Script(body?)
        })
    }
    /// Parse all of the directives into a single prologue
    fn parse_directive_prologues(&mut self) -> Res<Vec<ProgramPart<'b>>> {
        debug!(
            "{}: parse_directive_prologues {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
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
    fn parse_directive(&mut self) -> Res<ProgramPart<'b>> {
        debug!(
            "{}: parse_directive {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let orig = self.look_ahead.clone();
        let expr = self.parse_expression()?;
        if let Expr::Lit(lit) = expr {
            if let Lit::String(s) = lit {
                if let Token::String(quoted) = &orig.token {
                    let (contents, oct) = match &quoted {
                        ress::prelude::StringLit::Double(inner)
                        | ress::prelude::StringLit::Single(inner) => {
                            (inner.content, inner.contains_octal_escape)
                        }
                    };
                    self.context.strict = self.context.strict || contents == "use strict";
                    self.context.found_directive_octal_escape =
                        self.context.found_directive_octal_escape || oct;
                }
                debug!(
                    "updated context.strict to {}, allowed?: {}",
                    self.context.strict, self.context.allow_strict_directive
                );
                if !self.context.allow_strict_directive && self.context.strict {
                    return self.unexpected_token_error(&orig, "use strict in an invalid location");
                }
                if self.context.strict && self.context.found_directive_octal_escape {
                    return Err(Error::OctalLiteral(orig.location.start));
                }
                self.consume_semicolon()?;
                Ok(ProgramPart::Dir(Dir {
                    dir: s.content.source.clone(),
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
    fn parse_statement_list_item(&mut self, ctx: Option<StmtCtx<'b>>) -> Res<ProgramPart<'b>> {
        debug!("{}: parse_statement_list_item", self.look_ahead.span.start);
        self.context.set_is_assignment_target(true);
        self.context.set_is_binding_element(true);
        let tok = self.look_ahead.token.clone();
        match &tok {
            Token::Keyword(ref k) => match k {
                Keyword::Import(_) => {
                    if self.at_import_call()? {
                        let stmt = self.parse_statement(ctx)?;
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
                Keyword::Export(_) => {
                    let export = self.parse_export_decl()?;
                    let decl = Decl::Export(Box::new(export));
                    Ok(ProgramPart::Decl(decl))
                }
                Keyword::Const(_) => {
                    let decl = self.parse_lexical_decl(false)?;
                    Ok(ProgramPart::Decl(decl))
                }
                Keyword::Function(_) => {
                    let func = self.parse_fn_stmt(ctx.is_none())?;
                    let decl = Decl::Func(func);
                    Ok(ProgramPart::Decl(decl))
                }
                Keyword::Class(_) => {
                    let class = self.parse_class_decl(false, true)?;
                    let decl = Decl::Class(class);
                    Ok(ProgramPart::Decl(decl))
                }
                Keyword::Let(s) => {
                    if s.contains("\\u") {}
                    let part = if self.at_lexical_decl() {
                        let decl = self.parse_lexical_decl(false)?;
                        ProgramPart::Decl(decl)
                    } else {
                        let stmt = self.parse_statement(ctx)?;
                        ProgramPart::Stmt(stmt)
                    };
                    Ok(part)
                }
                Keyword::Var(_) => {
                    let keyword = self.expect_keyword(Keyword::Var(()))?;
                    let decls = self.parse_var_decl_list(false)?;
                    
                    self.consume_semicolon()?;
                    Ok(ProgramPart::Decl(Decl::Var(VarDecls {
                        decls,
                        keyword: VarKind::Var(keyword),
                    })))
                }
                _ => {
                    let stmt = self.parse_statement(ctx)?;
                    Ok(ProgramPart::Stmt(stmt))
                }
            },
            _ => {
                if self.at_async_function() {
                    let func = self.parse_fn_stmt(ctx.is_none())?;
                    let decl = Decl::Func(func);
                    return Ok(ProgramPart::Decl(decl));
                }
                let stmt = self.parse_statement(ctx)?;
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
    fn parse_import_decl(&mut self) -> Res<ModImport<'b>> {
        if let Some(scope) = self.context.lexical_names.last_scope() {
            if !scope.is_top() {
                return Err(Error::InvalidImportError(self.current_position));
            }
        }
        let keyword_import = self.expect_keyword(Keyword::Import(()))?;
        // if the next token is a string we are at an import
        // with not specifiers
        if self.look_ahead.is_string() {
            let source = self.parse_module_specifier()?;
            self.consume_semicolon()?;
            Ok(ModImport {
                keyword_import,
                specifiers: Vec::new(),
                keyword_from: None,
                source,
            })
        } else {
            // If we are at an open brace, this is the named
            //variant
            let specifiers = if self.at_punct(Punct::OpenBrace) {
                self.parse_named_imports()?
            // If we are at a *, this is the namespace variant
            } else if self.at_punct(Punct::Asterisk) {
                vec![self.parse_import_namespace_specifier()?]
            // if we are at an identifier that is not `default` this is the default variant
            } else if self.at_possible_ident() && !self.at_keyword(Keyword::Default(())) {
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
            let keyword_from = self.next_item()?;
            let keyword_from = self.get_slice(&keyword_from)?;
            // capture the source string for where this import
            // comes from
            let source = self.parse_module_specifier()?;
            self.consume_semicolon()?;
            Ok(ModImport { 
                keyword_import,
                specifiers,
                keyword_from: Some(keyword_from),
                source 
            })
        }
    }
    /// This will handle the named variant of imports
    /// ```js
    /// import {Thing} from 'place';
    /// ```
    fn parse_named_imports(&mut self) -> Res<Vec<ImportSpecifier<'b>>> {
        let open_brace = self.expect_punct(Punct::OpenBrace)?;
        let mut ret = Vec::new();
        while !self.at_punct(Punct::CloseBrace) {
            ret.push(self.parse_import_specifier()?);
            if !self.at_punct(Punct::CloseBrace) {
                self.expect_punct(Punct::Comma)?;
            }
        }
        let close_brace = self.expect_punct(Punct::CloseBrace)?;
        
        // Ok(ret)
        todo!()
    }

    fn parse_import_specifier(&mut self) -> Res<NormalImportSpec<'b>> {
        let start = self.look_ahead_position;
        let (imported, local) = if self.look_ahead.token.is_ident() {
            let imported = self.parse_var_ident(false)?;
            let local = if self.at_contextual_keyword("as") {
                let keyword = self.next_item()?;
                let keyword = self.get_slice(&keyword)?;
                let alias = self.parse_var_ident(false)?;
                return Ok(NormalImportSpec {
                    local: imported,
                    alias: Some(Alias {
                        ident: alias,
                        keyword,
                    })
                })
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
        self.context
            .lexical_names
            .declare(local.slice.source.clone(), DeclKind::Lex(true), start)?;
        if local.slice.source == "arguments" || local.slice.source == "eval" {
            return Err(Error::StrictModeArgumentsOrEval(start));
        }
        Ok(NormalImportSpec {
            local,
            alias: None,
        })
    }

    fn parse_import_namespace_specifier(&mut self) -> Res<ImportSpecifier<'b>> {
        self.expect_punct(Punct::Asterisk)?;
        if !self.at_contextual_keyword("as") {
            return self.expected_token_error(&self.look_ahead, &["as"]);
        }
        let _ = self.next_item()?;
        let start = self.look_ahead_position;
        let ident = self.parse_ident_name()?;
        self.context
            .lexical_names
            .declare(ident.slice.source.clone(), DeclKind::Lex(true), start)?;
        // Ok(ImportSpecifier::Namespace(ident))
        todo!()
    }

    fn parse_import_default_specifier(&mut self) -> Res<ImportSpecifier<'b>> {
        let start = self.look_ahead_position;
        let ident = self.parse_ident_name()?;
        self.context
            .lexical_names
            .declare(ident.slice.source.clone(), DeclKind::Lex(true), start)?;
        // Ok(ImportSpecifier::Default(ident))
        todo!()
    }

    fn parse_export_decl(&mut self) -> Res<ModExport<'b>> {
        debug!("{} parse_export_decl", self.look_ahead_position);
        if let Some(scope) = self.context.lexical_names.last_scope() {
            trace!("scope: {:?}", self.context.lexical_names.states);
            if !scope.is_top() {
                return Err(Error::InvalidExportError(self.current_position));
            }
        }
        if !self.context.is_module {
            return Err(Error::UseOfModuleFeatureOutsideOfModule(
                self.current_position,
                "export syntax".to_string(),
            ));
        }
        self.expect_keyword(Keyword::Export(()))?;
        if self.at_keyword(Keyword::Default(())) {
            let keyword = self.next_item()?;
            if let Token::Keyword(k) = &keyword.token {
                if k.has_unicode_escape() {
                    return self.unexpected_token_error(
                        &keyword,
                        "Keyword used with escaped character(s)",
                    );
                }
            }
            self.context.lexical_names.add_export_ident(
                &resast::prelude::Ident::from("default"),
                self.look_ahead_position,
            )?;
            let decl = if self.at_keyword(Keyword::Function(())) {
                return self.parse_export_decl_func(true);
            } else if self.at_keyword(Keyword::Class(())) {
                return self.parse_export_decl_class(true);
            } else if self.at_contextual_keyword("async") {
                if self.at_async_function() {
                    let _start = self.look_ahead_position;
                    let func = self.parse_function_decl(true)?;
                    let decl = Decl::Func(func);
                    DefaultExportDecl::Decl(decl)
                } else {
                    let _start = self.look_ahead_position;
                    let expr = self.parse_assignment_expr()?;
                    self.consume_semicolon()?;
                    DefaultExportDecl::Expr(expr)
                }
            } else {
                if self.at_contextual_keyword("from") {
                    return Err(Error::InvalidUseOfContextualKeyword(
                        self.current_position,
                        "from".to_string(),
                    ));
                }
                let expr = if self.at_punct(Punct::OpenBrace) {
                    self.parse_obj_init()?
                } else if self.at_punct(Punct::OpenBracket) {
                    self.parse_array_init()?
                } else {
                    let expr = self.parse_assignment_expr()?;
                    self.consume_semicolon()?;
                    expr
                };
                DefaultExportDecl::Expr(expr)
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
            if self.look_ahead.token.matches_keyword(Keyword::Let(()))
                || self.look_ahead.token.matches_keyword(Keyword::Const(()))
            {
                let _start = self.look_ahead_position;
                let lex = self.parse_lexical_decl(false)?;
                let decl = NamedExportDecl::Decl(lex);
                self.consume_semicolon()?;
                Ok(ModExport::Named(decl))
            } else if self.look_ahead.token.matches_keyword(Keyword::Var(())) {
                let _ = self.next_item()?;
                let _start = self.look_ahead_position;
                let decls = self.parse_variable_decl_list(false)?;
                let var = Decl::Var(VarKind::Var, decls);
                let decl = NamedExportDecl::Decl(var);
                self.consume_semicolon()?;
                Ok(ModExport::Named(decl))
            } else if self.look_ahead.token.matches_keyword(Keyword::Class(())) {
                self.parse_export_decl_class(false)
            } else if self.look_ahead.token.matches_keyword(Keyword::Function(())) {
                self.parse_export_decl_func(false)
            } else {
                self.expected_token_error(
                    &self.look_ahead,
                    &["let", "var", "const", "class", "function"],
                )
            }
        } else if self.at_async_function() {
            let _start = self.look_ahead_position;
            let func = self.parse_function_decl(false)?;
            let decl = Decl::Func(func);
            let decl = NamedExportDecl::Decl(decl);
            Ok(ModExport::Named(decl))
        } else {
            self.expect_punct(Punct::OpenBrace)?;
            let mut specifiers = Vec::new();
            let mut found_default = false;
            while !self.at_punct(Punct::CloseBrace) {
                let is_default = self.at_keyword(Keyword::Default(()));
                found_default = found_default || is_default;
                let start = self.look_ahead_position;
                let spec = self.parse_export_specifier()?;
                if is_default {
                    self.context
                        .lexical_names
                        .add_export_ident(&resast::prelude::Ident::from("default"), start)?;
                } else {
                    // self.context.lexical_names.add_export_spec(&spec, start)?;
                    todo!()
                }
                specifiers.push(spec);
                if !self.at_punct(Punct::CloseBrace) {
                    self.expect_punct(Punct::Comma)?;
                }
            }
            let _ = self.next_item()?;
            if self.at_contextual_keyword("from") {
                let _ = self.next_item()?;
                let source = self.parse_module_specifier()?;
                self.consume_semicolon()?;
                for spec in &specifiers {
                    // self.context
                    //     .lexical_names
                    //     .removed_undefined_export(&spec.local);
                    todo!()
                }
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

    fn parse_export_decl_func(&mut self, is_default: bool) -> Res<ModExport<'b>> {
        let start = self.look_ahead_position;
        let func = self.parse_function_decl(true)?;
        if let Some(id) = &func.id {
            // self.context.lexical_names.add_export_ident(id, start)?;
            todo!()
        }
        let func = Decl::Func(func);
        if is_default {
            let decl = DefaultExportDecl::Decl(func);
            Ok(ModExport::Default(decl))
        } else {
            let decl = NamedExportDecl::Decl(func);
            Ok(ModExport::Named(decl))
        }
    }

    fn parse_export_decl_class(&mut self, is_default: bool) -> Res<ModExport<'b>> {
        let start = self.look_ahead_position;
        let class = self.parse_class_decl(true, true)?;
        if let Some(id) = &class.id {
            self.context.lexical_names.add_export_ident(id, start)?;
        }
        let decl = Decl::Class(class);
        if is_default {
            let decl = DefaultExportDecl::Decl(decl);
            Ok(ModExport::Default(decl))
        } else {
            let decl = NamedExportDecl::Decl(decl);
            Ok(ModExport::Named(decl))
        }
    }

    fn parse_export_specifier(&mut self) -> Res<ExportSpecifier<'b>> {
        let local = self.parse_ident_name()?;
        let exported = if self.at_contextual_keyword("as") {
            let keyword = self.next_item()?;
            let ident = self.parse_ident_name()?;
            Alias {
                keyword: self.get_slice(&keyword)?,
                ident,
            }
        } else {
            local.clone()
        };
        Ok(ExportSpecifier { local, alias: None })
    }

    fn parse_module_specifier(&mut self) -> Res<Lit<'b>> {
        let item = self.next_item()?;
        match &item.token {
            Token::String(ref sl) => Ok(match sl {
                ress::prelude::StringLit::Double(ref s) => {
                    // self.octal_literal_guard_string(s.contains_octal_escape, item.location.start)?;
                    resast::prelude::Lit::double_string_from(s.content)
                }
                ress::prelude::StringLit::Single(ref s) => {
                    // self.octal_literal_guard_string(s.contains_octal_escape, item.location.start)?;
                    resast::prelude::Lit::single_string_from(s.content)
                }
            }),
            _ => self.expected_token_error(&item, &["[string]"]),
        }
    }

    fn parse_statement(&mut self, ctx: Option<StmtCtx<'b>>) -> Res<Stmt<'b>> {
        debug!(
            "{}: parse_statement {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
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
                    let b = self.parse_block(true)?;
                    Stmt::Block(b)
                }
                Punct::OpenParen => {
                    let expr = self.parse_expression_statement()?;
                    Stmt::Expr(expr)
                }
                Punct::SemiColon => {
                    let semi = self.next_item()?;
                    let slice = self.get_slice(&semi)?;
                    Stmt::Empty(slice)
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
                    if let Some(StmtCtx::Label(name)) = ctx {
                        self.context
                            .label_set
                            .entry(&name)
                            .and_modify(|k| *k = LabelKind::Other);
                    }
                    self.parse_labelled_statement()?
                }
            }
            Token::Keyword(ref k) => match k {
                Keyword::Await(_) if !self.context.is_module => self.parse_labelled_statement()?,
                Keyword::Break(k) => Stmt::Break(self.parse_break_stmt(k)?),
                Keyword::Continue(k) => {
                    if !self.context.in_iteration {
                        return Err(Error::ContinueOutsideOfIteration(self.look_ahead_position));
                    }
                    Stmt::Continue(self.parse_continue_stmt(k)?)
                }
                Keyword::Debugger(k) => self.parse_debugger_stmt(k)?,
                Keyword::Do(_) => {
                    if let Some(StmtCtx::Label(name)) = ctx {
                        self.context
                            .label_set
                            .entry(&name)
                            .and_modify(|k| *k = LabelKind::Iteration);
                    }
                    Stmt::DoWhile(self.parse_do_while_stmt()?)
                }
                Keyword::For(_) => {
                    if let Some(StmtCtx::Label(name)) = ctx {
                        self.context
                            .label_set
                            .entry(&name)
                            .and_modify(|k| *k = LabelKind::Iteration);
                    }
                    self.parse_for_stmt()?
                }
                Keyword::Function(_) => {
                    let f = self.parse_fn_stmt(ctx.is_none())?;
                    let expr = Expr::Func(f);
                    Stmt::Expr(expr)
                }
                Keyword::If(_) => Stmt::If(self.parse_if_stmt()?),
                Keyword::Return(_) => Stmt::Return(self.parse_return_stmt()?),
                Keyword::Switch(_) => Stmt::Switch(self.parse_switch_stmt()?),
                Keyword::Throw(_) => Stmt::Throw(self.parse_throw_stmt()?),
                Keyword::Try(_) => Stmt::Try(self.parse_try_stmt()?),
                Keyword::Var(_) => self.parse_var_stmt()?,
                Keyword::While(_) => {
                    if let Some(StmtCtx::Label(name)) = ctx {
                        self.context
                            .label_set
                            .entry(&name)
                            .and_modify(|k| *k = LabelKind::Iteration);
                    }
                    Stmt::While(self.parse_while_stmt()?)
                }
                Keyword::With(_) => Stmt::With(self.parse_with_stmt()?),
                Keyword::Yield(_) if !self.context.strict => self.parse_labelled_statement()?,
                _ => Stmt::Expr(self.parse_expression_statement()?),
            },
            _ => return self.expected_token_error(&self.look_ahead, &[]),
        };
        Ok(stmt)
    }

    fn parse_with_stmt(&mut self) -> Res<WithStmt<'b>> {
        debug!(
            "{}: parse_with_stmt {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        if self.context.strict {
            self.tolerate_error(Error::NonStrictFeatureInStrictContext(
                self.current_position,
                "with statements".to_string(),
            ))?;
        }
        let keyword = self.expect_keyword(Keyword::With(()))?;
        let open_paren = self.expect_punct(Punct::OpenParen)?;
        let object = self.parse_expression()?;
        Ok(if !self.at_punct(Punct::CloseParen) {
            if !self.config.tolerant {
                self.expected_token_error(&self.look_ahead, &[")"])?;
            }
            WithStmt {
                keyword,
                open_paren: open_paren.clone(),
                object,
                close_paren: open_paren.clone(),
                body: Box::new(Stmt::Empty(open_paren)),
            }
        } else {
            let close_paren = self.expect_punct(Punct::CloseParen)?;
            let body_start = self.look_ahead_position;
            let body = self.parse_statement(Some(StmtCtx::With))?;
            if Self::is_func_decl(&body) || Self::is_labeled_func(&body) {
                return Err(Error::InvalidFuncPosition(
                    body_start,
                    "labeled function declarations cannot be the body of a with statement"
                        .to_string(),
                ));
            }
            WithStmt {
                keyword,
                open_paren,
                object,
                close_paren,
                body: Box::new(body),
            }
        })
    }

    fn parse_while_stmt(&mut self) -> Res<WhileStmt<'b>> {
        debug!(
            "{}: parse_while_stmt {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let keyword = self.expect_keyword(Keyword::While(()))?;
        let open_paren = self.expect_punct(Punct::OpenParen)?;
        let test = self.parse_expression()?;
        let start_pos = self.look_ahead_position;
        let body = if !self.at_punct(Punct::CloseParen) {
            return self.expected_token_error(&self.look_ahead, &[")"]);
        } else {
        };
        let close_paren = self.expect_punct(Punct::CloseParen)?;
        let prev_iter = self.context.in_iteration;
        self.context.in_iteration = true;
        let body = self.parse_statement(Some(StmtCtx::While))?;
        self.context.in_iteration = prev_iter;
        if Self::is_func_decl(&body) || Self::is_labeled_func(&body) {
            Err(Error::InvalidFuncPosition(start_pos, "Function declaration cannot be the body of a do while loop, maybe wrap this in a block statement?".to_string()))
        } else if let Stmt::Expr(Expr::Class(_)) = body {
            Err(Error::InvalidClassPosition(start_pos, "Class declaration cannot be the body of a do while loop, maybe wrap this in a block statement?".to_string()))
        } else {
            Ok(WhileStmt {
                keyword,
                open_paren,
                test,
                close_paren,
                body: Box::new(body),
            })
        }
    }

    fn parse_var_stmt(&mut self) -> Res<Stmt<'b>> {
        debug!(
            "{}: parse_var_stmt {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        self.expect_keyword(Keyword::Var(()))?;
        let decls = self.parse_var_decl_list(false)?;

        let stmt = Stmt::Var(decls);
        self.consume_semicolon()?;
        Ok(stmt)
    }

    fn parse_var_decl_list(&mut self, in_for: bool) -> Res<Vec<VarDecl<'b>>> {
        debug!(
            "{} parse_var_decl_list {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let start = self.look_ahead_position;
        let first = self.parse_var_decl(in_for)?;
        self.declare_pat(
            &first.id,
            lexical_names::DeclKind::Var(self.context.is_module),
            start,
        )?;
        let mut ret = vec![first];
        while self.at_punct(Punct::Comma) {
            let _ = self.next_item()?;
            let start = self.look_ahead_position;
            let next = self.parse_var_decl(in_for)?;
            self.declare_pat(
                &next.id,
                lexical_names::DeclKind::Var(self.context.is_module),
                start,
            )?;
            ret.push(next);
        }
        Ok(ret)
    }

    fn parse_var_decl(&mut self, in_for: bool) -> Res<VarDecl<'b>> {
        let (_, patt) = self.parse_pattern(true, &mut Vec::new())?;
        if self.context.strict && Self::is_restricted(&patt) {
            let patt = match patt {
                Pat::Ident(ident) => ident.slice,
                _ => unreachable!(
                    "restricted patterns should only be reachable by identifer patterns"
                ),
            };
            return Err(Error::NonStrictFeatureInStrictContext(
                self.current_position,
                format!("{:?} as an identifier", patt),
            ));
        }
        let init = if self.at_punct(Punct::Equal) {
            let _ = self.next_item()?;
            let init = self.isolate_cover_grammar(Self::parse_assignment_expr)?;
            Some(init)
        } else if !Self::is_pat_ident(&patt) && !in_for {
            return self.expected_token_error(&self.look_ahead, &["="]);
        } else {
            None
        };
        Ok(VarDecl { id: patt, init })
    }

    fn parse_try_stmt(&mut self) -> Res<TryStmt<'b>> {
        debug!(
            "{}: parse_try_stmt {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let keyword = self.expect_keyword(Keyword::Try(()))?;
        let block = self.parse_block(true)?;
        if !self.context.in_iteration {
            for part in &block.stmts {
                if let ProgramPart::Stmt(Stmt::Continue(_)) = &part {
                    return self.unexpected_token_error(&self.look_ahead, "continue in try catch");
                }
            }
        }
        let handler = if self.at_keyword(Keyword::Catch(())) {
            let handler = self.parse_catch_clause()?;
            if !self.context.in_iteration {
                for part in &handler.body.stmts {
                    if let ProgramPart::Stmt(Stmt::Continue(_)) = &part {
                        return self
                            .unexpected_token_error(&self.look_ahead, "continue in try catch");
                    }
                }
            }
            Some(handler)
        } else {
            None
        };
        let finalizer = if self.at_keyword(Keyword::Finally(())) {
            Some(self.parse_finally_clause()?)
        } else {
            None
        };
        if handler.is_none() && finalizer.is_none() {
            return Err(Error::TryWithNoCatchOrFinally(self.current_position));
        }
        Ok(TryStmt {
            keyword,
            block,
            handler,
            finalizer,
        })
    }

    fn parse_catch_clause(&mut self) -> Res<CatchClause<'b>> {
        debug!(
            "{}: parse_catch_clause {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let keyword = self.expect_keyword(Keyword::Catch(()))?;
        let mut param_pos = self.look_ahead_position;
        let param = if self.at_punct(Punct::OpenParen) {
            let open_paren = self.expect_punct(Punct::OpenParen)?;
            if self.at_punct(Punct::CloseParen) {
                return Err(Error::InvalidCatchArg(self.current_position));
            }
            param_pos = self.look_ahead_position;
            let mut params = Vec::new();
            let (_, param) = self.parse_pattern(false, &mut params)?;
            if self.context.strict && Self::is_restricted(&param) {
                return Err(Error::StrictModeArgumentsOrEval(self.current_position));
            }
            match param {
                Pat::Array(_) | Pat::Obj(_) => {
                    // let mut args = HashSet::new();
                    // if formal_params::update_with_pat(&param, &mut args).is_err() {
                    //     return Err(Error::InvalidCatchArg(param_pos));
                    // }
                    todo!()
                }
                _ => (),
            }
            if !self.at_punct(Punct::CloseParen) {
                return Err(Error::InvalidCatchArg(self.current_position));
            }
            let close_paren = self.expect_punct(Punct::CloseParen)?;
            let catch_arg = CatchArg {
                open_paren,
                param,
                close_paren,
            };
            Some(catch_arg)
        } else {
            None
        };
        if let Some(ref p) = param {
            let (kind, scope) = if let Pat::Ident(_id) = &p.param {
                (
                    lexical_names::DeclKind::SimpleCatch,
                    lexical_names::Scope::SimpleCatch,
                )
            } else {
                (
                    lexical_names::DeclKind::Lex(self.context.is_module),
                    lexical_names::Scope::Catch,
                )
            };
            self.add_scope(scope);
            self.declare_pat(p, kind, param_pos)?;
        } else {
            self.add_scope(lexical_names::Scope::Catch);
        }
        let body = self.parse_block(false)?;
        self.remove_scope();
        Ok(CatchClause { keyword, param, body })
    }

    fn parse_finally_clause(&mut self) -> Res<FinallyClause<'b>> {
        debug!(
            "{}: parse_finally_clause {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let keyword = self.expect_keyword(Keyword::Finally(()))?;
        let body = self.parse_block(true)?;
        Ok(FinallyClause {
            keyword,
            body,
        })
    }

    fn parse_throw_stmt(&mut self) -> Res<Expr<'b>> {
        debug!(
            "{}: parse_throw_stmt {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        self.expect_keyword(Keyword::Throw(()))?;
        if self.context.has_line_term || self.at_punct(Punct::SemiColon) {
            return Err(Error::ThrowWithNoArg(self.current_position));
        }
        let arg = self.parse_expression()?;
        self.consume_semicolon()?;
        Ok(arg)
    }

    fn parse_switch_stmt(&mut self) -> Res<SwitchStmt<'b>> {
        debug!(
            "{}: parse_switch_stmt {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let keyword = self.expect_keyword(Keyword::Switch(()))?;
        let open_paren = self.expect_punct(Punct::OpenParen)?;
        let discriminant = self.parse_expression()?;
        let close_paren = self.expect_punct(Punct::CloseParen)?;
        let open_brace = self.expect_punct(Punct::OpenBrace)?;
        self.add_scope(lexical_names::Scope::Switch);
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
        let close_brace = self.expect_punct(Punct::CloseBrace)?;
        self.remove_scope();
        self.context.in_switch = prev_sw;
        Ok(SwitchStmt {
            keyword,
            open_paren,
            discriminant,
            close_paren,
            open_brace,
            cases,
            close_brace,
        })
    }

    fn parse_switch_case(&mut self) -> Res<SwitchCase<'b>> {
        debug!(
            "{}: parse_switch_case {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let (keyword, test) = if self.at_keyword(Keyword::Default(())) {
            (self.expect_keyword(Keyword::Default(()))?,
            None)
        } else {
            (self.expect_keyword(Keyword::Case(()))?,
            Some(self.parse_expression()?))
        };
        let colon = self.expect_punct(Punct::Colon)?;
        let mut consequent = Vec::new();
        loop {
            if self.at_punct(Punct::CloseBrace)
                || self.at_keyword(Keyword::Default(()))
                || self.at_keyword(Keyword::Case(()))
            {
                break;
            }
            consequent.push(self.parse_statement_list_item(None)?)
        }
        Ok(SwitchCase { keyword, test, colon, consequent })
    }

    fn parse_return_stmt(&mut self) -> Res<Option<Expr<'b>>> {
        debug!(
            "{}: parse_return_stmt {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        if !self.context.in_function_body {
            return self
                .unexpected_token_error(&self.look_ahead, "cannot return in the global context");
        }
        self.expect_keyword(Keyword::Return(()))?;
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

    fn parse_if_stmt(&mut self) -> Res<IfStmt<'b>> {
        debug!(
            "{}: parse_if_stmt {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );

        let keyword = self.expect_keyword(Keyword::If(()))?;
        let open_paren = self.expect_punct(Punct::OpenParen)?;
        let test = self.parse_expression()?;
        if !self.at_punct(Punct::CloseParen) {
            if !self.config.tolerant {
                return self.expected_token_error(&self.look_ahead, &[")"]);
            }
        } 
        let close_paren = self.expect_punct(Punct::CloseParen)?;
        let body_start = self.look_ahead_position;
        let consequent = self.parse_if_clause()?;
        if Self::is_labeled_func(&consequent) {
            return Err(Error::InvalidFuncPosition(
                body_start,
                "If body cannot be a labelled function".to_string(),
            ));
        }
        let alternate = if self.at_keyword(Keyword::Else(())) {
            let _ = self.next_item()?;
            let e = self.parse_if_clause()?;
            if Self::is_labeled_func(&e) {
                return Err(Error::InvalidFuncPosition(
                    body_start,
                    "Else body cannot be a labelled function".to_string(),
                ));
            }
            Some(Box::new(e))
        } else {
            None
        };
        
        Ok(IfStmt {
            keyword,
            open_paren,
            test,
            close_paren,
            consequent,
            alternate,
        })
    }

    fn parse_if_clause(&mut self) -> Res<Stmt<'b>> {
        debug!(
            "{}: parse_if_clause {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        if self.context.strict && self.at_keyword(Keyword::Function(())) && !self.config.tolerant {
            return self.unexpected_token_error(&self.look_ahead, "");
        }
        self.parse_statement(Some(StmtCtx::If))
    }

    fn parse_fn_stmt(&mut self, decl_pos: bool) -> Res<Func<'b>> {
        debug!(
            "{}: parse_fn_stmt {:?} {}",
            self.look_ahead.span.start, self.look_ahead.token, decl_pos,
        );
        let async_keyword = if self.at_contextual_keyword("async") {
            let keyword = self.next_item()?;
            self.slice_from(&keyword)
        } else {
            None
        };
        let keyword = self.expect_keyword(Keyword::Function(()))?;
        let decl = self.parse_func(
            keyword,
            true,
            false,
            !decl_pos,
            async_keyword
        )?;
        Ok(decl)
    }

    fn parse_for_stmt(&mut self) -> Res<Stmt<'b>> {
        debug!(
            "{}: parse_for_stmt {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let keyword_for = self.expect_keyword(Keyword::For(()))?;
        let is_await = if self.at_keyword(Keyword::Await(())) {
            let _ = self.next_item()?;
            true
        } else {
            false
        };
        let open_paren = self.expect_punct(Punct::OpenParen)?;
        self.add_scope(lexical_names::Scope::For);
        if self.at_punct(Punct::SemiColon) {
            // any semi-colon would mean standard C style for loop
            // for (;;) {}
            let stmt = self.parse_for_loop(VarKind::Var)?;
            self.remove_scope();
            return Ok(Stmt::For(stmt));
        }
        let init_start = self.look_ahead_position;
        let ret = if self.at_keyword(Keyword::Var(())) {
            let slice = self.expect_keyword(Keyword::Var(()));
            let kind = VarKind::Var(slice);
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
                if self.at_keyword(Keyword::In(())) {
                    let left = LoopLeft::Variable(kind, decl);
                    let stmt = self.parse_for_in_loop(left, keyword_for, open_paren)?;
                    Ok(Stmt::ForIn(stmt))
                } else if self.at_contextual_keyword("of") {
                    // if !lhs::is_simple_pat(&decl.id) {
                    //     return Err(Error::ForOfNotSimple(init_start));
                    // }
                    let left = LoopLeft::Variable(kind, decl);
                    lhs::check_loop_left(&left, init_start)?;
                    let stmt = self.parse_for_of_loop(left, is_await, keyword_for, open_paren)?;
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
        } else if self.at_keyword(Keyword::Const(())) || self.at_keyword(Keyword::Let(())) {
            let kind = self.next_item()?;
            if kind.token.matches_keyword(Keyword::Let(())) {
                if self.at_punct(Punct::SemiColon) {
                    let ident = self.get_string(&kind.span)?;
                    let ident = resast::Ident::from(ident);
                    let ident = Expr::Ident(ident);
                    let loop_init = LoopInit::Expr(ident);
                    let for_stmt = self.parse_for_loop_cont(Some(loop_init))?;
                    return Ok(Stmt::For(for_stmt));
                } else if self.at_assign() {
                    let left = self.get_string(&kind.span)?;
                    let left = resast::Ident::from(left);
                    let left = Expr::Ident(left);
                    let assign = self.parse_assignment_after_start(left)?;
                    if self.at_punct(Punct::SemiColon) {
                        let init = LoopInit::Expr(Expr::Assign(assign));
                        let loop_stmt = self.parse_for_loop_cont(Some(init))?;
                        self.remove_scope();
                        return Ok(Stmt::For(loop_stmt));
                    }
                }
            }
            let var_kind = match &kind.token {
                Token::Keyword(ref k) => match k {
                    Keyword::Const(_) => VarKind::Const,
                    Keyword::Let(_) => VarKind::Let,
                    _ => return self.expected_token_error(&kind, &["const", "let"]),
                },
                _ => return self.expected_token_error(&kind, &["const", "let"]),
            };
            if !self.context.strict && self.look_ahead.token.matches_keyword(Keyword::In(())) {
                let keyword_in = self.expect_keyword(Keyword::In(()))?;
                //const or let becomes an ident
                let k = match var_kind {
                    VarKind::Var(slice) => slice,
                    VarKind::Let(slice) => slice,
                    VarKind::Const(slice) => slice,
                };
                let left = LoopLeft::Expr(Expr::Ident(Ident { slice: k }));
                let right = self.parse_expression()?;
                let body_start = self.look_ahead_position;
                let close_paren = self.expect_punct(Punct::CloseParen)?;
                let body = self.parse_loop_body()?;
                if Self::is_labeled_func(&body) {
                    return Err(Error::InvalidFuncPosition(
                        body_start,
                        "Loop body cannot be a function or labeled function".to_string(),
                    ));
                }
                Ok(Stmt::ForIn(ForInStmt {
                    keyword_for,
                    open_paren,
                    left,
                    keyword_in,
                    right,
                    close_paren,
                    body: Box::new(body),
                }))
            } else {
                let prev_in = self.context.allow_in;
                self.context.allow_in = false;
                let mut decls = self.parse_binding_list(var_kind, true)?;
                debug!("{:?}", decls);
                self.context.allow_in = prev_in;
                if decls.len() == 1 {
                    let decl = if let Some(d) = decls.pop() {
                        d
                    } else {
                        return self.expected_token_error(&self.look_ahead, &["variable decl"]);
                    };
                    if decl.init.is_none() && self.at_keyword(Keyword::In(())) {
                        let left = LoopLeft::Variable(var_kind, decl);
                        lhs::check_loop_left(&left, init_start)?;
                        let keyword_in = self.expect_keyword(Keyword::In(()))?;
                        let right = self.parse_expression()?;
                        let close_paren = self.expect_punct(Punct::CloseParen)?;
                        let body_start = self.look_ahead_position;
                        let body = self.parse_loop_body()?;
                        if Self::is_labeled_func(&body) || Self::is_labeled_func(&body) {
                            Err(Error::InvalidFuncPosition(
                                body_start,
                                "Loop body cannot be a function or labeled function".to_string(),
                            ))
                        } else {
                            Ok(Stmt::ForIn(ForInStmt {
                                keyword_for,
                                open_paren,
                                left,
                                keyword_in,
                                right,
                                close_paren,
                                body: Box::new(body),
                            }))
                        }
                    } else if decl.init.is_none() && self.at_contextual_keyword("of") {
                        let left = LoopLeft::Variable(var_kind, decl);
                        lhs::check_loop_left(&left, init_start)?;
                        Ok(Stmt::ForOf(self.parse_for_of_loop(left, is_await, keyword_for, open_paren)?))
                    } else {
                        let init = LoopInit::Variable(var_kind, vec![decl]);
                        let stmt = self.parse_for_loop_cont(Some(init), keyword_for, open_paren)?;
                        Ok(Stmt::For(stmt))
                    }
                } else {
                    let init = LoopInit::Variable(var_kind, decls);
                    let stmt = self.parse_for_loop_cont(Some(init))?;
                    Ok(Stmt::For(stmt))
                }
            }
        } else {
            let prev_in = self.context.allow_in;
            self.context.allow_in = false;
            let init = self.inherit_cover_grammar(Self::parse_assignment_expr)?;
            self.context.allow_in = prev_in;
            if self.at_keyword(Keyword::In(())) {
                let keyword_in = self.expect_keyword(Keyword::In(()))?;
                if let Expr::Assign(_) = init {
                    return Err(Error::ForOfInAssign(
                        self.look_ahead_position,
                        "For in loop left hand side cannot contain an assignment".to_string(),
                    ));
                }
                if !lhs::is_simple_expr(&init) {
                    return Err(Error::ForOfNotSimple(init_start));
                }
                lhs::check_loop_head_expr(&init, init_start)?;
                let left = LoopLeft::Expr(init);
                let right = self.parse_expression()?;
                let body_start = self.look_ahead_position;
                let close_paren = self.expect_punct(Punct::CloseParen)?;
                let body = self.parse_loop_body()?;
                if Self::is_labeled_func(&body) {
                    return Err(Error::InvalidFuncPosition(
                        body_start,
                        "Loop body cannot be a function or labeled function".to_string(),
                    ));
                }
                Ok(Stmt::ForIn(ForInStmt {
                    keyword_for,
                    open_paren,
                    left,
                    keyword_in,
                    right,
                    close_paren,
                    body: Box::new(body),
                }))
            } else if self.at_contextual_keyword("of") {
                if !lhs::is_simple_expr(&init) {
                    return Err(Error::ForOfNotSimple(init_start));
                }
                lhs::check_loop_head_expr(&init, init_start)?;
                let keyword_of = self.next_item()?;
                let keyword_of = self.get_slice(&keyword_of)?;
                let left = LoopLeft::Expr(init);
                let right = self.parse_assignment_expr()?;
                let close_paren = self.expect_punct(Punct::CloseParen)?;
                let body_start = self.look_ahead_position;
                let body = self.parse_loop_body()?;
                if Self::is_labeled_func(&body) {
                    Err(Error::InvalidFuncPosition(
                        body_start,
                        "Invalid function position as body of for of loop".to_string(),
                    ))
                } else {
                    Ok(Stmt::ForOf(ForOfStmt {
                        keyword_for,
                        open_paren,
                        left,
                        keyword_of,
                        right,
                        close_paren,
                        body: Box::new(body),
                        is_await,
                    }))
                }
            } else {
                let init = if self.at_punct(Punct::Comma) {
                    let mut seq = vec![init];
                    while self.at_punct(Punct::Comma) {
                        let _comma = self.next_item()?;
                        let el = self.isolate_cover_grammar(Self::parse_assignment_expr)?;
                        seq.push(el);
                    }
                    LoopInit::Expr(Expr::Sequence(seq))
                } else {
                    LoopInit::Expr(init)
                };
                Ok(Stmt::For(self.parse_for_loop_cont(Some(init))?))
            }
        };
        self.remove_scope();
        ret
    }

    fn parse_for_loop(&mut self, kind: VarKind, for_keyword: Slice<'b>, open_paren: Slice<'b>) -> Res<ForStmt<'b>> {
        debug!(
            "{}: parse_for_loop {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let init = if self.at_punct(Punct::SemiColon) {
            None
        } else {
            let list = self.parse_variable_decl_list(true)?;
            Some(LoopInit::Variable(kind, list))
        };
        self.parse_for_loop_cont(init, for_keyword, open_paren)
    }

    fn parse_for_loop_cont(&mut self, init: Option<LoopInit<'b>>, keyword_for: Slice<'b>, open_paren: Slice<'b>) -> Res<ForStmt<'b>> {
        debug!(
            "{}: parse_for_loop_cont {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let semi1 = self.expect_punct(Punct::SemiColon)?;
        let test = if self.at_punct(Punct::SemiColon) {
            None
        } else {
            Some(self.parse_expression()?)
        };
        let semi2 = self.expect_punct(Punct::SemiColon)?;
        let update = if self.at_punct(Punct::CloseParen) {
            None
        } else {
            Some(self.parse_expression()?)
        };
        let start_pos = self.look_ahead_position;
        let close_paren = self.expect_punct(Punct::CloseParen)?;
        let body = self.parse_loop_body()?;
        if Self::is_func_decl(&body) || Self::is_labeled_func(&body) {
            return Err(Error::InvalidFuncPosition(start_pos, "Function declaration cannot be the body of a loop, maybe wrap this in a block statement?".to_string()));
        }
        Ok(ForStmt {
            keyword: keyword_for,
            open_paren,
            init,
            semi1,
            test,
            semi2,
            update,
            close_paren,
            body: Box::new(body),
        })
    }

    fn parse_for_in_loop(&mut self, left: LoopLeft<'b>, keyword_for: Slice<'b>, open_paren: Slice<'b>) -> Res<ForInStmt<'b>> {
        debug!(
            "{}: parse_for_in_loop {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        if let LoopLeft::Variable(
            kind,
            VarDecl {
                ref id,
                init: Some(_),
                ..
            },
        ) = left
        {
            if kind != VarKind::Var || self.context.strict {
                return Err(Error::ForOfInAssign(
                    self.look_ahead_position,
                    "For in loop left hand side cannot contain an assignment".to_string(),
                ));
            }
            match id {
                Pat::Obj(_) | Pat::Array(_) => {
                    return Err(Error::ForOfInAssign(
                        self.look_ahead_position,
                        "For in loop left hand side cannot contain an assignment".to_string(),
                    ))
                }
                _ => (),
            }
        }
        let keyword_in = self.expect_keyword(Keyword::In(()))?;
        let right = self.parse_expression()?;
        let body_start = self.look_ahead_position;
        let close_paren = self.expect_punct(Punct::CloseParen)?;
        let body = self.parse_loop_body()?;
        if Self::is_func_decl(&body) || Self::is_labeled_func(&body) {
            Err(Error::InvalidFuncPosition(
                body_start,
                "Loop body cannot be a function declaration or labeled function declaration"
                    .to_string(),
            ))
        } else {
            Ok(ForInStmt {
                keyword_for,
                open_paren,
                left,
                keyword_in,
                right,
                close_paren,
                body: Box::new(body),
            })
        }
    }

    fn parse_for_of_loop(&mut self, left: LoopLeft<'b>, is_await: bool, keyword_for: Slice<'b>, open_paren: Slice<'b>) -> Res<ForOfStmt<'b>> {
        debug!(
            "{}: parse_for_of_loop {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        if let LoopLeft::Variable(_, VarDecl { init: Some(_), .. }) = left {
            return Err(Error::ForOfInAssign(
                self.look_ahead_position,
                "For in loop left hand side cannot contain an assignment".to_string(),
            ));
        }
        let keyword_of = self.next_item()?;
        let keyword_of = self.get_slice(&keyword_of)?;
        let right = self.parse_assignment_expr()?;
        let close_paren = self.expect_punct(Punct::CloseParen)?;
        let body_start = self.look_ahead_position;
        let body = self.parse_loop_body()?;
        if Self::is_func_decl(&body) || Self::is_labeled_func(&body) {
            Err(Error::InvalidFuncPosition(
                body_start,
                "Loop body cannot be a function declaration or labeled function declaration"
                    .to_string(),
            ))
        } else {
            Ok(ForOfStmt {
                keyword_for,
                open_paren,
                left,
                keyword_of,
                right,
                close_paren,
                body: Box::new(body),
                is_await,
            })
        }
    }

    fn parse_loop_body(&mut self) -> Res<Stmt<'b>> {
        debug!(
            "{}: parse_loop_body {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let prev_iter = self.context.in_iteration;
        self.context.in_iteration = true;
        let ret = self.isolate_cover_grammar(|me| me.parse_statement(Some(StmtCtx::For)))?;
        self.context.in_iteration = prev_iter;
        Ok(ret)
    }

    fn parse_do_while_stmt(&mut self) -> Res<DoWhileStmt<'b>> {
        debug!(
            "{}: parse_do_while_stmt {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let start_pos = self.look_ahead_position;
        let keyword_do = self.expect_keyword(Keyword::Do(()))?;
        let prev_iter = self.context.in_iteration;
        self.context.in_iteration = true;
        let body = self.parse_statement(Some(StmtCtx::Do))?;
        if Self::is_func_decl(&body) || Self::is_labeled_func(&body) {
            return Err(Error::InvalidFuncPosition(start_pos, "Function declaration cannot be the body of a do while loop, maybe wrap this in a block statement?".to_string()));
        }
        self.context.in_iteration = prev_iter;
        let keyword_while = self.expect_keyword(Keyword::While(()))?;
        let open_paren = self.expect_punct(Punct::OpenParen)?;
        let test = self.parse_expression()?;
        let close_paren = self.expect_punct(Punct::CloseParen)?;
        if self.look_ahead.token.matches_punct(Punct::SemiColon) {
            self.expect_punct(Punct::SemiColon)?;
        }
        Ok(DoWhileStmt {
            keyword_do,
            body: Box::new(body),
            keyword_while,
            open_paren,
            test,
            close_paren,
        })
    }

    fn parse_break_stmt(&mut self, _s: &'b str) -> Res<Option<Ident<'b>>> {
        debug!(
            "{}: parse_break_stmt {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        self.parse_optionally_labeled_statement(Keyword::Break(()))
    }

    fn parse_continue_stmt(&mut self, _s: &'b str) -> Res<Option<Ident<'b>>> {
        debug!(
            "{}: parse_continue_stmt {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        self.parse_optionally_labeled_statement(Keyword::Continue(()))
    }

    fn parse_optionally_labeled_statement(
        &mut self,
        k: Keyword<()>,
    ) -> Res<Option<resast::Ident<'b>>> {
        debug!(
            "{}: parse_optionally_labeled_statement",
            self.look_ahead.span.start
        );
        self.expect_keyword(k)?;
        let start = self.look_ahead_position;
        let ret = if self.look_ahead.token.is_ident() && !self.context.has_line_term {
            let id = self.parse_var_ident(false)?;
            if let Some(label_kind) = self.context.label_set.get(&*id.name) {
                if k == Keyword::Continue(()) && label_kind != &LabelKind::Iteration {
                    return Err(Error::ContinueOfNotIterationLabel(
                        start,
                        id.name.to_string(),
                    ));
                }
            } else {
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
            && k == Keyword::Break(())
            && !self.context.in_iteration
            && !self.context.in_switch
        {
            return Err(Error::InvalidOptionalLabel(self.current_position));
        }
        Ok(ret)
    }

    fn parse_debugger_stmt(&mut self, _s: &'b str) -> Res<Stmt<'b>> {
        debug!(
            "{}: parse_debugger_stmt {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        self.expect_keyword(Keyword::Debugger(()))?;
        self.consume_semicolon()?;
        Ok(Stmt::Debugger)
    }

    fn parse_labelled_statement(&mut self) -> Res<Stmt<'b>> {
        debug!("parse_labelled_statement, {:?}", self.look_ahead.token);
        let start = self.look_ahead.span;
        let pos = self.look_ahead_position;
        let ret = self.parse_expression()?;
        if let Expr::Ident(ref ident) = ret {
            if self.context.strict && Self::is_strict_reserved(ident) {
                return Err(Error::NonStrictFeatureInStrictContext(
                    self.current_position,
                    "strict reserved word as identifier".to_string(),
                ));
            }
            if self.at_punct(Punct::Colon) {
                let colon = self.expect_punct(Punct::Colon)?;
                let id = if let Expr::Ident(ident) = ret {
                    ident
                } else {
                    return Err(self.reinterpret_error("expression", "ident"));
                };
                let label_str = self.get_string(&start)?;
                if self
                    .context
                    .label_set
                    .insert(label_str, LabelKind::Unknown)
                    .is_some()
                {
                    return Err(self.redecl_error(&id.name));
                }
                let body = if self.at_keyword(Keyword::Class(())) {
                    let class = self.next_item()?;
                    let keyword = self.get_slice(&class)?;
                    if !self.config.tolerant {
                        return self.unexpected_token_error(&class, "");
                    }
                    let body = self.parse_class_body()?;
                    let cls = Class {
                        keyword,
                        id: None,
                        super_class: None,
                        body,
                    };
                    let expr = Expr::Class(cls);
                    Stmt::Expr(expr)
                } else if self.at_keyword(Keyword::Function(())) {
                    if self.context.strict {
                        return Err(Error::UnexpectedToken(
                            pos,
                            "labeled statement bodies cannot be a function declaration".to_string(),
                        ));
                    }
                    let function = self.expect_keyword(Keyword::Function(()))?;
                    let f = self.parse_func(function, true, true, false, None)?;
                    let expr = Expr::Func(f);
                    Stmt::Expr(expr)
                } else {
                    self.parse_statement(Some(StmtCtx::Label(label_str)))?
                };
                self.context.label_set.remove(&self.get_string(&start)?);
                return Ok(Stmt::Labeled(LabeledStmt {
                    label: id,
                    colon,
                    body: Box::new(body),
                }));
            }
        }
        self.consume_semicolon()?;
        Ok(Stmt::Expr(ret))
    }

    fn parse_expression_statement(&mut self) -> Res<Expr<'b>> {
        debug!(
            "{}: parse_expression_statement {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        self.expr_stmt_guard()?;
        let ret = self.parse_expression()?;
        self.consume_semicolon()?;
        Ok(ret)
    }

    fn expr_stmt_guard(&self) -> Res<()> {
        let start = self.look_ahead_position;
        match &self.look_ahead.token {
            Token::Keyword(Keyword::Let(_)) => {
                if let Some(peek) = self.scanner.look_ahead() {
                    if let Ok(peek) = &peek {
                        if let Token::Punct(Punct::OpenBracket) = &peek.token {
                            return Err(Error::InvalidStartOfExpressionStmt(
                                start,
                                "let [".to_string(),
                            ));
                        }
                    }
                }
            }
            Token::Keyword(Keyword::Function(_)) => {
                return Err(Error::InvalidStartOfExpressionStmt(
                    start,
                    "function".to_string(),
                ));
            }
            Token::Keyword(Keyword::Class(_)) => {
                return Err(Error::InvalidStartOfExpressionStmt(
                    start,
                    "class".to_string(),
                ));
            }
            Token::Punct(Punct::OpenBrace) => {
                return Err(Error::InvalidStartOfExpressionStmt(start, "{".to_string()));
            }
            _ => (),
        }
        if self.at_async_function() {
            return Err(Error::InvalidStartOfExpressionStmt(
                start,
                "async function".to_string(),
            ));
        }
        Ok(())
    }

    fn parse_expression(&mut self) -> Res<Expr<'b>> {
        debug!(
            "{}: parse_expression {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let ret = self.isolate_cover_grammar(Self::parse_assignment_expr)?;
        if self.at_punct(Punct::Comma) {
            let mut list = vec![ret];
            while !self.look_ahead.token.is_eof() {
                if !self.at_punct(Punct::Comma) {
                    break;
                }
                let _comma = self.next_item()?;
                let expr = self.isolate_cover_grammar(Self::parse_assignment_expr)?;
                list.push(expr);
            }
            return Ok(Expr::Sequence(list));
        }
        Ok(ret)
    }

    fn parse_block(&mut self, new_scope: bool) -> Res<BlockStmt<'b>> {
        debug!(
            "{}: parse_block {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let open_brace = self.expect_punct(Punct::OpenBrace)?;
        if new_scope {
            self.add_scope(lexical_names::Scope::Block);
        }
        let mut stmts = Vec::new();
        loop {
            if self.at_punct(Punct::CloseBrace) {
                break;
            }
            let part = self.parse_statement_list_item(None)?;
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
            stmts.push(part);
        }
        let close_brace = self.expect_punct(Punct::CloseBrace)?;
        if new_scope {
            self.remove_scope();
        }
        Ok(BlockStmt {
            open_brace,
            stmts,
            close_brace,
        })
    }

    fn parse_lexical_decl(&mut self, in_for: bool) -> Res<Decl<'b>> {
        debug!(
            "{}: parse_lexical_decl {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let next = self.next_item()?;
        let kind = match next.token {
            Token::Keyword(ref k) => match k {
                Keyword::Let(_) => VarKind::Let(self.get_slice(&next)?),
                Keyword::Const(_) => VarKind::Const(self.get_slice(&next)?),
                _ => return self.expected_token_error(&next, &["let", "const"]),
            },
            _ => return self.expected_token_error(&next, &["let", "const"]),
        };
        let decls = self.parse_binding_list(&kind, in_for)?;
        self.consume_semicolon()?;
        let var_decls = VarDecls {
            keyword: kind,
            decls,
        };
        Ok(Decl::Var(var_decls))
    }

    fn parse_binding_list(&mut self, kind: &VarKind, in_for: bool) -> Res<Vec<VarDecl<'b>>> {
        debug!(
            "{}: parse_binding_list {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let k = if matches!(kind, VarKind::Var(_)) {
            lexical_names::DeclKind::Var(self.context.is_module)
        } else {
            lexical_names::DeclKind::Lex(self.context.is_module)
        };
        let start_pos = self.look_ahead_position;
        let first = self.parse_lexical_binding(kind, in_for)?;
        self.declare_pat(&first.id, k, start_pos)?;
        let mut ret = vec![first];

        while self.at_punct(Punct::Comma) {
            let _comma = self.next_item()?;
            let start_pos = self.look_ahead_position;
            let next = self.parse_lexical_binding(kind, in_for)?;

            self.declare_pat(&next.id, k, start_pos)?;
            ret.push(next);
        }
        Ok(ret)
    }

    fn parse_variable_decl_list(&mut self, in_for: bool) -> Res<Vec<VarDecl<'b>>> {
        debug!(
            "{} parse_variable_decl_list in_for: {}",
            self.look_ahead.span.start, in_for
        );
        let first = self.parse_variable_decl(in_for)?;
        let mut ret = vec![first];
        while self.at_punct(Punct::Comma) {
            let _ = self.next_item()?;
            let next = self.parse_variable_decl(in_for)?;
            ret.push(next);
        }
        Ok(ret)
    }

    fn parse_variable_decl(&mut self, in_for: bool) -> Res<VarDecl<'b>> {
        debug!(
            "{} parse_variable_decl in_for: {}",
            self.look_ahead.span.start, in_for
        );
        let start = self.look_ahead.clone();
        let (_, id) = self.parse_pattern(true, &mut Vec::new())?;
        if self.context.strict && Self::is_restricted(&id) && !self.config.tolerant {
            return self.unexpected_token_error(&start, "restricted word");
        }
        let init = if self.at_punct(Punct::Equal) {
            let _ = self.next_item()?;
            let init = self.isolate_cover_grammar(Self::parse_assignment_expr)?;
            Some(init)
        } else if !Self::is_pat_ident(&id) && !in_for {
            self.expect_punct(Punct::Equal)?;
            None
        } else {
            None
        };
        Ok(VarDecl { id, init })
    }

    fn is_pat_ident(pat: &Pat) -> bool {
        match pat {
            Pat::Ident(_) => true,
            _ => false,
        }
    }

    fn parse_lexical_binding(&mut self, kind: &VarKind, in_for: bool) -> Res<VarDecl<'b>> {
        debug!(
            "{}: parse_lexical_binding {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let start = self.look_ahead.clone();
        let (_, id) = self.parse_pattern(matches!(kind, VarKind::Var(_)), &mut Vec::new())?;
        if self.context.strict && Self::is_restricted(&id) && !self.config.tolerant {
            return self.unexpected_token_error(&start, "restricted word");
        }

        let init = if matches!(kind, VarKind::Const(_)) {
            if self.at_keyword(Keyword::In(())).is_none() && self.at_contextual_keyword("of").is_none() {
                if self.at_punct(Punct::Equal) {
                    let _ = self.next_item()?;
                    let init = self.isolate_cover_grammar(Self::parse_assignment_expr)?;
                    Some(init)
                } else {
                    return self.expected_token_error(&self.look_ahead, &["="]);
                }
            } else {
                None
            }
        } else if !in_for && !Self::is_pat_ident(&id) || self.at_punct(Punct::Equal) {
            self.expect_punct(Punct::Equal)?;
            let init = self.isolate_cover_grammar(Self::parse_assignment_expr)?;
            Some(init)
        } else {
            None
        };
        Ok(VarDecl { id, init })
    }

    fn is_restricted(id: &Pat) -> bool {
        match id {
            Pat::Ident(ref ident) => ident.name == "eval" || ident.name == "arguments",
            _ => false,
        }
    }

    fn parse_function_decl(&mut self, opt_ident: bool) -> Res<Func<'b>> {
        debug!(
            "{}: parse_function_decl {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let start_pos = self.look_ahead_position;
        let keyword_async = if self.at_contextual_keyword("async") {
            let keyword_async = self.next_item()?;
            self.slice_from(&keyword_async)
        } else {
            None
        };
        let keyword = self.expect_keyword(Keyword::Function(()))?;
        let star = if self.at_punct(Punct::Asterisk) {
            let keyword_async = self.next_item()?;
            self.slice_from(&keyword_async)

        } else {
            None
        };
        
        let (id, _first_restricted) = if !opt_ident || !self.at_punct(Punct::OpenParen) {
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
        let prev_super = self.context.allow_super;
        self.context.allow_await = keyword_async.is_none();
        self.context.allow_yield = star.is_none();
        debug!("setting allow_super to {}", false);
        self.context.set_allow_super(false);
        let param_start = self.look_ahead_position;
        self.add_scope(lexical_names::Scope::FuncTop);
        let formal_params = self.parse_formal_params()?;
        if self.context.strict {
            if formal_params::have_duplicates(&formal_params.params) {
                return Err(Error::InvalidParameter(
                    param_start,
                    "Duplicate parameter in strict context".to_string(),
                ));
            }
            if formal_params.found_restricted {
                return Err(Error::StrictModeArgumentsOrEval(param_start));
            }
        }
        let strict = formal_params.strict;
        let params = formal_params.params;
        let prev_strict = self.context.strict;
        let prev_allow_strict = self.context.allow_strict_directive;
        let prev_oct = self.context.found_directive_octal_escape;
        self.context.allow_strict_directive = formal_params.simple;
        let body = self.parse_function_source_el()?;
        if self.context.strict && formal_params.found_restricted {
            return Err(Error::StrictModeArgumentsOrEval(param_start));
        }
        if !prev_strict && self.context.strict {
            if formal_params::have_duplicates(&params) {
                return Err(Error::InvalidParameter(
                    start_pos,
                    "Duplicate parameter in function who's body is strict".to_string(),
                ));
            }
            if formal_params.found_restricted {
                return Err(Error::StrictModeArgumentsOrEval(param_start));
            }
        }
        if self.context.strict && strict {
            return self.expected_token_error(&self.look_ahead, &[]);
        }
        self.context.strict = prev_strict;
        self.context.found_directive_octal_escape = prev_oct;
        self.context.allow_strict_directive = prev_allow_strict;
        self.context.allow_await = prev_await;
        self.context.allow_yield = prev_yield;
        debug!("setting allow_super to {}", prev_super);
        self.context.set_allow_super(prev_super);
        self.remove_scope();
        Ok(Func {
            keyword,
            id,
            open_paren: formal_params.open_paren,
            params,
            close_paren: formal_params.close_paren,
            body,
            star,
            keyword_async,
        })
    }

    fn parse_func(
        &mut self,
        keyword: Slice<'b>,
        is_stmt: bool,
        opt_id: bool,
        is_hanging: bool,
        keyword_async: Option<Slice<'b>>,
    ) -> Res<Func<'b>> {
        debug!(
            "{} parse_func( is_stmt: {}, opt_id: {}, is_hanging: {}",
            self.look_ahead.span.start, is_stmt, opt_id, is_hanging
        );
        let star = if self.at_punct(Punct::Asterisk) {
            let star = self.next_item()?;
            self.slice_from(&star)
        } else {
            None
        };
        let mut id_is_restricted = false;
        let id_start = self.look_ahead_position;
        let id = if is_stmt {
            if opt_id && !self.look_ahead.token.is_ident() {
                None
            } else {
                id_is_restricted = self.look_ahead.token.is_restricted();
                let start = self.look_ahead_position;
                if self.context.strict && id_is_restricted {
                    return Err(Error::RestrictedIdent(start));
                }
                let id = self.parse_var_ident(false)?;
                if !is_hanging {
                    trace!(
                        "function not hanging, strict: {}, generator: {}, async: {}",
                        self.context.strict,
                        star.is_some(),
                        keyword_async.is_some()
                    );
                    trace!(
                        "last scope: {:?}\n{:?}",
                        self.context.lexical_names.last_scope(),
                        self.context.lexical_names.states
                    );
                    let kind = if self.context.strict || star.is_some() || keyword_async.is_some() {
                        if self
                            .context
                            .lexical_names
                            .current_funcs_as_var(self.context.is_module)
                        {
                            lexical_names::DeclKind::Var(self.context.is_module)
                        } else {
                            lexical_names::DeclKind::Lex(self.context.is_module)
                        }
                    } else {
                        lexical_names::DeclKind::Func(self.context.is_module)
                    };
                    self.context
                        .lexical_names
                        .declare(id.name.clone(), kind, start)?;
                }
                Some(id)
            }
        } else if self.look_ahead.token.is_ident() {
            let id = self.parse_var_ident(false)?;
            Some(id)
        } else {
            None
        };
        let prev_await = self.context.allow_await;
        let prev_yield = self.context.allow_yield;
        let prev_super = self.context.allow_super;
        self.context.allow_await = keyword_async.is_none();
        self.context.allow_yield = star.is_none();
        self.context.set_allow_super(false);
        let param_start = self.look_ahead_position;
        self.add_scope(lexical_names::Scope::FuncTop);
        let params = self.parse_func_params()?;
        debug!(
            "any params restricted? {}, {}",
            params.found_restricted, params.strict
        );
        let prev_strict = self.context.strict;
        let prev_oct = self.context.found_directive_octal_escape;
        let prev_allow_strict = self.context.allow_strict_directive;
        self.context.allow_strict_directive = params.simple;

        let body = self.parse_function_source_el()?;
        if !prev_strict && self.context.strict {
            if formal_params::have_duplicates(&params.params) {
                return Err(Error::InvalidParameter(
                    param_start,
                    "Duplicate parameter in function who's body is strict".to_string(),
                ));
            }
            if params.found_restricted {
                return Err(Error::StrictModeArgumentsOrEval(param_start));
            }
            if id_is_restricted {
                return Err(Error::StrictModeArgumentsOrEval(id_start));
            }
        }
        self.context.strict = prev_strict;
        self.context.found_directive_octal_escape = prev_oct;
        self.context.allow_strict_directive = prev_allow_strict;
        self.context.allow_await = prev_await;
        self.context.allow_yield = prev_yield;
        self.context.set_allow_super(prev_super);
        self.remove_scope();
        let f = Func {
            keyword,
            id,
            open_paren: params.open_paren,
            params: params.params,
            close_paren: params.close_paren,
            body,
            keyword_async,
            star,
        };

        Ok(f)
    }
    fn remove_scope(&mut self) {
        trace!("{} remove_scope", self.look_ahead.span.start);
        self.context.lexical_names.remove_child();
    }
    fn add_scope(&mut self, scope: lexical_names::Scope) {
        trace!("{} add_scope {:?}", self.look_ahead.span.start, scope);
        self.context.lexical_names.new_child(scope);
    }
    fn declare_pat(&mut self, pat: &Pat<'b>, kind: DeclKind, pos: Position) -> Res<()> {
        info!(
            "{} declare_pat {:?} {:?}",
            self.look_ahead.span.start, pat, pos
        );
        self.context.lexical_names.declare_pat(pat, kind, pos)
    }

    fn parse_func_params(&mut self) -> Res<FormalParams<'b>> {
        let start = self.look_ahead_position;
        let formal_params = self.parse_formal_params()?;
        if self.context.strict {
            if formal_params.found_restricted {
                return Err(Error::StrictModeArgumentsOrEval(start));
            }
            if formal_params::have_duplicates(&formal_params.params) {
                return Err(Error::InvalidParameter(
                    start,
                    "Duplicate parameter in strict context".to_string(),
                ));
            }
        }
        Ok(formal_params)
    }

    fn parse_function_source_el(&mut self) -> Res<FuncBody<'b>> {
        debug!(
            "{}: parse_function_source_el {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let open_brace = self.expect_punct(Punct::OpenBrace)?;
        let mut body = self.parse_directive_prologues()?;
        let prev_label = self.context.label_set.clone();
        let prev_iter = self.context.in_iteration;
        let prev_switch = self.context.in_switch;
        let prev_in_fn = self.context.in_function_body;
        self.context.label_set = HashMap::new();
        self.context.in_iteration = false;
        self.context.in_switch = false;
        self.context.in_function_body = true;
        while !self.look_ahead.token.is_eof() {
            if self.at_punct(Punct::CloseBrace) {
                break;
            }
            body.push(self.parse_statement_list_item(None)?)
        }
        let close_brace = self.expect_punct(Punct::CloseBrace)?;
        self.context.label_set = prev_label;
        self.context.in_iteration = prev_iter;
        self.context.in_switch = prev_switch;
        self.context.in_function_body = prev_in_fn;
        Ok(FuncBody {
            open_brace,
            stmts: body,
            close_brace,
        })
    }

    fn parse_class_decl(&mut self, opt_ident: bool, check_id: bool) -> Res<Class<'b>> {
        debug!(
            "{}: parse_class_decl {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let prev_strict = self.context.strict;
        let prev_oct = self.context.found_directive_octal_escape;
        self.context.strict = true;
        let keyword = self.expect_keyword(Keyword::Class(()))?;
        let start = self.look_ahead_position;
        let mut super_class = if self.at_keyword(Keyword::Extends(())) {
            let _ = self.next_item()?;
            let super_class =
                self.isolate_cover_grammar(Self::parse_left_hand_side_expr_allow_call)?;
            Some(Box::new(super_class))
        } else {
            None
        };
        let id = if self.at_keyword(Keyword::Await(())) {
            if self.context.is_module {
                return Err(Error::InvalidUseOfContextualKeyword(
                    start,
                    "await is an invalid class name in modules".to_string(),
                ));
            }
            let s = self.get_string(&self.look_ahead.span)?;
            let _ = self.next_item()?;
            Some(resast::Ident::from(s))
        } else if opt_ident && !self.look_ahead.token.is_ident() {
            None
        } else {
            Some(self.parse_var_ident(false)?)
        };
        if super_class.is_none() && self.at_keyword(Keyword::Extends(())) {
            let _ = self.next_item()?;
            let new_super =
                self.isolate_cover_grammar(Self::parse_left_hand_side_expr_allow_call)?;
            super_class = Some(Box::new(new_super))
        }
        if check_id {
            if let Some(ref i) = id {
                self.context.lexical_names.declare(
                    i.name.clone(),
                    lexical_names::DeclKind::Lex(self.context.is_module),
                    start,
                )?;
            }
        }
        self.context.set_allow_super(true);
        let body = self.parse_class_body()?;

        self.context.strict = prev_strict;
        self.context.found_directive_octal_escape = prev_oct;
        Ok(Class {
            keyword,
            id,
            super_class,
            body,
        })
    }

    fn parse_class_body(&mut self) -> Res<ClassBody<'b>> {
        debug!(
            "{}: parse_class_body {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let mut props = Vec::new();
        let mut has_ctor = false;
        let open_brace = self.expect_punct(Punct::OpenBrace)?;
        while !self.at_punct(Punct::CloseBrace) {
            if self.at_punct(Punct::SemiColon) {
                let _ = self.next_item()?;
            } else {
                let (ctor, el) = self.parse_class_el(has_ctor)?;
                has_ctor = ctor;
                props.push(el)
            }
        }
        let close_brace = self.expect_punct(Punct::CloseBrace)?;
        Ok(ClassBody {
            open_brace,
            props,
            close_brace,
        })
    }

    fn parse_class_el(&mut self, has_ctor: bool) -> Res<(bool, Prop<'b>)> {
        debug!(
            "{}: parse_class_el {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let mut token = self.look_ahead.token.clone();
        let mut has_ctor = has_ctor;
        let mut is_ctor = false;
        let prev_super_call = self.context.allow_super_call;
        let mut key: Option<PropKey> = None;
        let mut value: Option<PropValue> = None;
        let mut computed = false;
        let mut is_static = false;
        let mut keyword_async = if self.at_contextual_keyword("async") {
            let keyword_async = self.next_item()?;
            self.slice_from(&keyword_async)
        } else {
            None
        };
        let star = if self.at_punct(Punct::Asterisk) {
            debug!("found leading asterisk");
            let star = self.next_item()?;
            self.slice_from(&star)
        } else {
            None
        };
        
        computed = self.at_punct(Punct::OpenBracket);
        let new_key = self.parse_object_property_key()?;
        //else {


            if Self::is_static(&new_key)
                && (Self::qualified_prop_name(&self.look_ahead.token)
                    || self.at_punct(Punct::Asterisk))
            {
                if self.at_contextual_keyword("async") {
                    let keyword = self.next_item()?;
                    keyword_async = self.slice_from(&keyword);
                }
                token = self.look_ahead.token.clone();
                computed = self.at_punct(Punct::OpenBracket);
                is_static = true;
                if self.at_punct(Punct::Asterisk) {
                    let _ = self.next_item()?;
                } else {
                    key = Some(self.parse_object_property_key()?);
                }
            } else {
                is_ctor = Self::is_key(&new_key, "constructor");
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
            if is_ctor {
                self.context.allow_super_call = self.context.allow_super;
            } else {
                self.context.allow_super_call = false;
            }
            kind = Some(PropKind::Method);
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
            if !is_static && is_ctor {
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
                    has_ctor = is_ctor;
                }
                kind = PropKind::Ctor;
            }
        }

        let value = if let Some(v) = value {
            v
        } else {
            return self.expected_token_error(&self.look_ahead, &[]);
        };
        self.context.allow_super_call = prev_super_call;
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

    /// Compares `key` with `other` to see if they
    /// match, this takes into account all of the
    /// different shapes that `key` could be, including
    /// identifiers and literals
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

    fn parse_async_property_method(&mut self, keyword_async: Slice<'b>) -> Res<PropValue<'b>> {
        debug!(
            "{}: parse_property_method_async_fn",
            self.look_ahead.span.start
        );
        let start = self.look_ahead_position;
        let prev_yield = self.context.allow_yield;
        let prev_await = self.context.allow_await;
        self.context.allow_yield = false;
        self.context.allow_await = false;
        self.add_scope(lexical_names::Scope::FuncTop);
        let params = self.parse_formal_params()?;
        if formal_params::have_duplicates(&params.params) {
            return Err(Error::InvalidParameter(
                start,
                "Method arguments cannot contain duplicates".to_string(),
            ));
        }
        let body = self.parse_property_method_body(params.simple, params.found_restricted)?;
        self.context.allow_yield = prev_yield;
        self.context.allow_await = prev_await;
        let func = PropMethod {
            keyword_async,
            star: None,
            id: None,
            open_paren: params.open_paren,
            params: params.params,
            close_paren: params.close_paren,
            body,
        };
        Ok(PropValue::Method(Expr::Func(func)))
    }

    fn parse_property_method(&mut self) -> Res<PropValue<'b>> {
        debug!(
            "{}: parse_property_method {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let start = self.look_ahead_position;
        let prev_yield = self.context.allow_yield;
        let prev_strict = self.context.allow_strict_directive;
        self.context.allow_yield = !self.context.strict;
        self.add_scope(lexical_names::Scope::FuncTop);
        let params = self.parse_formal_params()?;
        if formal_params::have_duplicates(&params.params) {
            return Err(Error::InvalidParameter(
                start,
                "Method arguments cannot contain duplicates".to_string(),
            ));
        }
        self.context.allow_strict_directive = params.simple;
        let body = self.parse_property_method_body(params.simple, params.found_restricted)?;
        self.context.allow_yield = prev_yield;
        self.context.allow_strict_directive = prev_strict;
        Ok(PropValue::Method(
            PropMethod {
                keyword_async: None,
                star: None,
                id: None,
                open_paren: params.open_paren,
                params: params.params,
                close_paren: params.close_paren,
                body,
            }
        ))
    }

    fn parse_generator_method(&mut self, star: Slice<'b>) -> Res<PropValue<'b>> {
        debug!(
            "{}: pares_generator_method {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let start = self.look_ahead_position;
        let prev_yield = self.context.allow_yield;
        self.context.allow_yield = true;
        self.add_scope(lexical_names::Scope::FuncTop);
        let params = self.parse_formal_params()?;
        if formal_params::have_duplicates(&params.params) {
            return Err(Error::InvalidParameter(
                start,
                "Method arguments cannot contain duplicates".to_string(),
            ));
        }
        self.context.allow_yield = false;
        let body = self.parse_method_body(params.simple, params.found_restricted)?;
        self.remove_scope();
        self.context.allow_yield = prev_yield;
        let func = PropMethod {
            keyword_async: None,
            star,
            id: None,
            open_paren: params.open_paren,
            params: params.params,
            close_paren: params.close_paren,
            body,
        };
        Ok(PropValue::Method(func))
    }

    fn parse_getter_method(&mut self) -> Res<PropValue<'b>> {
        debug!(
            "{}: parse_getter_method {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let star = None;
        let start = self.look_ahead_position;
        let prev_yield = self.context.allow_yield;
        let start_position = self.look_ahead_position;
        self.add_scope(lexical_names::Scope::FuncTop);
        let formal_params = self.parse_formal_params()?;
        if formal_params::have_duplicates(&formal_params.params) {
            return Err(Error::InvalidParameter(
                start,
                "Method arguments cannot contain duplicates".to_string(),
            ));
        }
        if !formal_params.params.is_empty() {
            self.tolerate_error(Error::InvalidGetterParams(start_position))?;
        }
        let body = self.parse_method_body(formal_params.simple, formal_params.found_restricted)?;
        self.remove_scope();
        self.context.allow_yield = prev_yield;
        Ok(PropValue::Expr(Expr::Func(Func {
            id: None,
            params: formal_params.params,
            body,
            star,
            keyword_async: None,
        })))
    }

    fn parse_method_body(&mut self, simple: bool, found_restricted: bool) -> Res<FuncBody<'b>> {
        debug!(
            "{}: parse_method_body {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        self.context.set_is_assignment_target(false);
        self.context.set_is_binding_element(false);
        let prev_strict = self.context.strict;
        let prev_oct = self.context.found_directive_octal_escape;
        let prev_allow_strict = self.context.allow_strict_directive;
        self.context.allow_strict_directive = simple;
        let start = self.look_ahead.clone();
        let body = self.isolate_cover_grammar(Self::parse_function_source_el)?;
        if self.context.strict && found_restricted && !self.config.tolerant {
            self.unexpected_token_error(&start, "restricted ident")?;
        }
        self.context.strict = prev_strict;
        self.context.found_directive_octal_escape = prev_oct;
        self.context.allow_strict_directive = prev_allow_strict;
        Ok(body)
    }

    fn parse_setter_method(&mut self) -> Res<PropValue<'b>> {
        debug!(
            "{}: parse_setter_method {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let start = self.look_ahead_position;
        let prev_allow = self.context.allow_yield;
        self.context.allow_yield = true;
        self.add_scope(lexical_names::Scope::FuncTop);
        let params = self.parse_formal_params()?;
        if formal_params::have_duplicates(&params.params) {
            return Err(Error::InvalidParameter(
                start,
                "Method arguments cannot contain duplicates".to_string(),
            ));
        }
        self.context.allow_yield = prev_allow;
        if params.params.len() != 1 {
            self.tolerate_error(Error::InvalidSetterParams(start))?;
        } else if let Some(ref param) = params.params.get(0) {
            if Self::is_rest(param) {
                self.tolerate_error(Error::InvalidSetterParams(start))?;
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
        PropValue::
        Ok(PropValue::Expr(Expr::Func(func)))
    }

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

    fn parse_property_method_body(
        &mut self,
        simple: bool,
        found_restricted: bool,
    ) -> Res<FuncBody<'b>> {
        debug!(
            "{}: parse_property_method_fn {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        self.context.set_is_assignment_target(false);
        self.context.set_is_binding_element(false);
        let prev_strict = self.context.strict;
        let prev_oct = self.context.found_directive_octal_escape;
        let prev_allow = self.context.allow_strict_directive;
        self.context.allow_strict_directive = simple;
        let start_pos = self.look_ahead_position;
        let ret = self.isolate_cover_grammar(Self::parse_function_source_el)?;
        if self.context.strict && found_restricted {
            self.tolerate_error(Error::NonStrictFeatureInStrictContext(
                start_pos,
                "restriced ident".to_string(),
            ))?;
        }
        self.context.strict = prev_strict;
        self.context.found_directive_octal_escape = prev_oct;
        self.context.allow_strict_directive = prev_allow;
        self.remove_scope();
        Ok(ret)
    }

    fn qualified_prop_name(tok: &Token<&str>) -> bool {
        debug!("qualified_prop_name",);
        tok.is_ident()
            || tok.is_keyword()
            || tok.is_literal()
            || tok.matches_punct(Punct::OpenBracket)
    }

    fn parse_object_property_key(&mut self) -> Res<PropInitKey<'b>> {
        debug!(
            "{}: parse_object_property_key {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let item = self.next_item()?;
        if matches!(item.token, Token::String(_) | Token::Number(_))
        {
            let id = match &item.token {
                Token::String(ref sl) => match sl {
                    ress::prelude::StringLit::Single(s) => {
                        self.octal_literal_guard_string(
                            s.contains_octal_escape,
                            item.location.start,
                        )?;
                        Lit::single_string_from(s.content)
                    }
                    ress::prelude::StringLit::Double(s) => {
                        self.octal_literal_guard_string(
                            s.contains_octal_escape,
                            item.location.start,
                        )?;
                        Lit::double_string_from(s.content)
                    }
                },
                Token::Number(_) => {
                    if self.at_big_int_flag() {
                        return self.unexpected_token_error(
                            &self.look_ahead,
                            "BigInt cannot be uses as an object Lit key",
                        );
                    }
                    self.octal_literal_guard(&item.span)?;
                    Lit::number_from(self.get_string(&item.span)?)
                }
                _ => return Err(self.reinterpret_error("number or string", "Lit")),
            };
            Ok(PropInitKey {
                brackets: None,
                value: PropKey::Lit(id),
            })
        } else if matches!(item.token, Token::Ident(_) | Token::Null(_) | Token::Keyword(_) | Token::Boolean(_))
        {
            let slice = self.get_slice(&item.span)?;
            let ident = resast::spanned::Ident { slice };
            let expr = Expr::Ident(ident);
            let value = PropKey::Expr(expr);
            let init = PropInitKey {
                brackets: None,
                value,
            }
            Ok(init)
        } else if item.token.matches_punct(Punct::OpenBracket) {
            
            let key = self.isolate_cover_grammar(Self::parse_assignment_expr)?;
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

    fn octal_literal_guard_string(&self, has_octal: bool, pos: Position) -> Res<()> {
        if self.context.strict && has_octal {
            return Err(Error::OctalLiteral(pos));
        }
        Ok(())
    }

    fn octal_literal_guard(&mut self, span: &Span) -> Res<()> {
        if self.context.strict {
            let mut chars = self.get_string(span)?.chars();
            if let Some(first) = chars.next() {
                if first == '0' {
                    if let Some(second) = chars.next() {
                        if second.is_digit(10) {
                            return self.unexpected_token_error(
                                &self.look_ahead,
                                "numbers cannot have a leading zero in strict mode",
                            );
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn is_valid_property_key_lit(expr: &Expr) -> bool {
        match expr {
            Expr::Lit(ref l) => match l {
                Lit::String(_) | Lit::Number(_) | Lit::Boolean(_) => true,
                _ => false,
            },
            _ => false,
        }
    }

    fn parse_primary_expression(&mut self) -> Res<Expr<'b>> {
        debug!(
            "{}: parse_primary_expression {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        if self.context.strict && self.look_ahead.token.is_strict_reserved() {
            return Err(Error::NonStrictFeatureInStrictContext(
                self.look_ahead_position,
                "strict mode reserved word as an identifer".to_string(),
            ));
        }
        if self.look_ahead.token.is_ident()
            || (self.at_keyword(Keyword::Await(())) && self.context.allow_await)
        {
            if ((self.context.is_module) && self.at_keyword(Keyword::Await(())))
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
            self.context.set_is_assignment_target(false);
            self.context.set_is_binding_element(false);
            let item = self.next_item()?;
            let lit = match item.token {
                Token::Number(_) => {
                    let mut span = item.span;
                    if self.at_big_int_flag() {
                        span.end += 1;
                        // Consume the ident
                        let _n = self.next_item();
                    }
                    self.octal_literal_guard(&span)?;
                    let inner = self.get_string(&span)?;
                    Lit::number_from(inner)
                }
                Token::String(sl) => {
                    let inner = match sl {
                        ress::prelude::StringLit::Single(ref s) => {
                            self.octal_literal_guard_string(
                                s.contains_octal_escape,
                                item.location.start,
                            )?;
                            resast::prelude::StringLit::single_from(s.content)
                        }
                        ress::prelude::StringLit::Double(ref d) => {
                            self.octal_literal_guard_string(
                                d.contains_octal_escape,
                                item.location.start,
                            )?;
                            resast::prelude::StringLit::double_from(d.content)
                        }
                    };
                    Lit::String(inner)
                }
                _ => unreachable!(),
            };
            Ok(Expr::Lit(lit))
        } else if self.look_ahead.token.is_boolean() {
            self.context.set_is_assignment_target(false);
            self.context.set_is_binding_element(false);
            let item = self.next_item()?;
            let lit = match item.token {
                Token::Boolean(b) => Lit::Boolean(b.into()),
                _ => unreachable!(),
            };
            Ok(Expr::Lit(lit))
        } else if self.look_ahead.token.is_null() {
            self.context.set_is_assignment_target(false);
            self.context.set_is_binding_element(false);
            let _ = self.next_item()?;
            Ok(Expr::Lit(Lit::Null))
        } else if self.look_ahead.is_template() {
            let lit = self.parse_template_lit(false)?;
            Ok(Expr::Lit(Lit::Template(lit)))
        } else if self.look_ahead.token.is_punct() {
            let expr = if self.at_punct(Punct::OpenParen) {
                self.context.set_is_binding_element(false);
                self.inherit_cover_grammar(Self::parse_group_expr)?
            } else if self.at_punct(Punct::OpenBracket) {
                self.inherit_cover_grammar(Self::parse_array_init)?
            } else if self.at_punct(Punct::OpenBrace) {
                self.inherit_cover_grammar(Self::parse_obj_init)?
            } else {
                return self.expected_token_error(&self.look_ahead, &["{", "[", "("]);
            };
            Ok(expr)
        } else if self.look_ahead.token.is_regex() {
            self.context.set_is_assignment_target(false);
            self.context.set_is_binding_element(false);
            let regex = self.next_item()?;
            let lit = match regex.token {
                Token::RegEx(r) => {
                    let flags = if let Some(f) = r.flags { f } else { "" };
                    let re = resast::prelude::RegEx::from(&r.body, flags);
                    crate::regex::validate_regex(self.get_string(&regex.span)?)?;
                    re
                }
                _ => unreachable!(),
            };
            Ok(Expr::Lit(Lit::RegEx(lit)))
        } else if self.look_ahead.token.is_keyword() {
            if (!self.context.strict
                && ((self.context.allow_yield && self.at_keyword(Keyword::Yield(())))
                    || self.at_keyword(Keyword::Let(()))))
                || (!self.context.strict && self.look_ahead.token.is_strict_reserved())
            {
                let ident = self.parse_ident_name()?;
                Ok(Expr::Ident(ident))
            } else if self.at_keyword(Keyword::Await(())) {
                self.parse_await_expr()
            } else {
                self.context.set_is_assignment_target(false);
                self.context.set_is_binding_element(false);
                if self.at_keyword(Keyword::Function(())) {
                    self.parse_function_expr()
                } else if self.at_keyword(Keyword::This(())) {
                    let _ = self.next_item()?;
                    Ok(Expr::This)
                } else if self.at_keyword(Keyword::Class(())) {
                    let cls = self.parse_class_decl(true, false)?;
                    Ok(Expr::Class(cls))
                } else if self.at_import_call()? {
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

    fn parse_group_expr(&mut self) -> Res<Expr<'b>> {
        debug!(
            "{}: parse_group_expr {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
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
                self.context.set_is_binding_element(true);
                let mut ex = self.inherit_cover_grammar(Self::parse_assignment_expr)?;
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
                            let el = self.inherit_cover_grammar(Self::parse_assignment_expr)?;
                            exprs.push(el);
                        }
                    }
                    ex = Expr::Sequence(exprs);
                }
                self.expect_punct(Punct::CloseParen)?;
                if self.at_punct(Punct::EqualGreaterThan) {
                    if Self::is_ident(&ex) {
                        self.context.set_is_binding_element(false);
                        return Ok(Expr::ArrowParamPlaceHolder(vec![FuncArg::Expr(ex)], false));
                    }
                    if !self.context.is_binding_element {
                        return self.expected_token_error(&self.look_ahead, &["binding element"]);
                    }
                    if let Expr::Sequence(seq) = ex {
                        let args = if self.context.strict {
                            let args: Res<Vec<FuncArg<'b>>> = seq
                                .into_iter()
                                .map(|arg| self.convert_expr_to_func_arg_strict(arg))
                                .collect();
                            args?
                        } else {
                            seq.into_iter().map(FuncArg::Expr).collect()
                        };

                        return Ok(Expr::ArrowParamPlaceHolder(args, false));
                    } else {
                        return Ok(Expr::ArrowParamPlaceHolder(vec![FuncArg::Expr(ex)], false));
                    }
                }
                if let Expr::Obj(_) = &ex {
                    if let Some(item) = &self.context.first_covert_initialized_name_error {
                        return Err(Error::UnexpectedToken(
                            item.location.start,
                            item.token.to_string(),
                        ));
                    }
                }
                Ok(ex)
            }
        }
    }

    fn convert_expr_to_func_arg_strict(&self, expr: Expr<'b>) -> Res<FuncArg<'b>> {
        if !self.context.strict {
            return Ok(FuncArg::Expr(expr));
        }
        if let Expr::Ident(ref ident) = expr {
            if Self::is_strict_reserved(ident) {
                Err(Error::NonStrictFeatureInStrictContext(
                    self.current_position,
                    "strict reserved word as an identifier".to_string(),
                ))
            } else {
                Ok(FuncArg::Expr(expr))
            }
        } else {
            Ok(FuncArg::Expr(expr))
        }
    }

    fn parse_array_init(&mut self) -> Res<Expr<'b>> {
        debug!(
            "{}: parse_array_init {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        self.expect_punct(Punct::OpenBracket)?;
        let mut elements = Vec::new();
        while !self.at_punct(Punct::CloseBracket) {
            if self.at_punct(Punct::Comma) {
                self.next_item()?;
                elements.push(None);
            } else if self.at_punct(Punct::Ellipsis) {
                let el = self.parse_spread_element()?;
                if !self.at_punct(Punct::CloseBracket) {
                    self.context.set_is_assignment_target(false);
                    self.context.set_is_binding_element(false);
                    self.expect_punct(Punct::Comma)?;
                }
                elements.push(Some(el))
            } else {
                let el = self.inherit_cover_grammar(Self::parse_assignment_expr)?;
                elements.push(Some(el));
                if !self.at_punct(Punct::CloseBracket) {
                    self.expect_punct(Punct::Comma)?;
                }
            }
        }
        self.expect_punct(Punct::CloseBracket)?;
        Ok(Expr::Array(elements))
    }
    fn parse_obj_init(&mut self) -> Res<Expr<'b>> {
        debug!(
            "{}: parse_obj_init {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let start_pos = self.look_ahead_position;
        self.expect_punct(Punct::OpenBrace)?;
        let mut props = Vec::new();
        let mut proto_ct = 0;
        let prev_super = self.context.allow_super;
        self.context.set_allow_super(true);
        while !self.at_punct(Punct::CloseBrace) {
            let prop = if self.at_punct(Punct::Ellipsis) {
                let spread = self.parse_spread_element()?;
                ObjProp::Spread(spread)
            } else {
                let (found_proto, prop) = self.parse_obj_prop()?;
                if found_proto {
                    proto_ct += 1;
                    debug!("found proto: {}", proto_ct);
                }
                prop
            };
            props.push(prop);
            if !self.at_punct(Punct::CloseBrace) {
                self.expect_comma_sep()?;
            }
        }
        self.context.set_allow_super(prev_super);
        self.expect_punct(Punct::CloseBrace)?;
        if !self.at_punct(Punct::Equal) && proto_ct > 1 {
            Err(Error::Redecl(
                start_pos,
                "Multiple prototypes in object initializer is ot allowed".to_string(),
            ))
        } else {
            Ok(Expr::Obj(props))
        }
    }

    fn parse_obj_prop(&mut self) -> Res<(bool, ObjProp<'b>)> {
        debug!(
            "{}: parse_obj_prop {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let start = self.look_ahead.clone();
        let mut is_proto = false;
        let mut at_get = false;
        let mut at_set = false;
        let (key, is_async, computed) = if self.look_ahead.token.is_ident()
            || (!self.context.strict && self.look_ahead.token.matches_keyword(Keyword::Let(())))
        {
            at_get = self.look_ahead.token.matches_ident_str("get");
            at_set = self.look_ahead.token.matches_ident_str("set");
            let ident = self.next_item()?;
            let computed = self.at_punct(Punct::OpenBracket);
            let is_async = !self.context.has_line_term
                && ident.token.matches_ident_str("async")
                && !self.at_punct(Punct::Colon)
                && !self.at_punct(Punct::Asterisk)
                && !self.at_punct(Punct::Comma);
            let key = if is_async {
                if self.at_contextual_keyword("async") {
                    return Err(Error::UnexpectedToken(
                        ident.location.start,
                        "`async async` is not a valid property name".to_string(),
                    ));
                }
                self.parse_object_property_key()?
            } else {
                let s = self.get_string(&ident.span)?;
                PropKey::Expr(Expr::Ident(resast::Ident::from(s)))
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
        let prev_super = self.context.allow_super;
        let prop = if at_get && at_qualified && !is_async {
            let computed = self.at_punct(Punct::OpenBracket);
            let key = self.parse_object_property_key()?;
            self.context.set_allow_super(true);
            let value = self.parse_getter_method()?;
            self.context.set_allow_super(prev_super);
            ObjProp::Prop(Prop {
                computed,
                key,
                value,
                kind: PropKind::Get,
                method: false,
                short_hand: false,
                is_static: false,
            })
        } else if at_set && at_qualified && !is_async {
            let computed = self.at_punct(Punct::OpenBracket);
            let key = self.parse_object_property_key()?;
            self.context.set_allow_super(true);
            let value = self.parse_setter_method()?;
            self.context.set_allow_super(prev_super);
            ObjProp::Prop(Prop {
                computed,
                key,
                value,
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
                    is_proto = true;
                }
                let _ = self.next_item()?;
                let value = self.inherit_cover_grammar(Self::parse_assignment_expr)?;
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
                self.context.set_allow_super(true);
                let value = if is_async {
                    self.parse_async_property_method()?
                } else {
                    self.parse_property_method()?
                };
                self.context.set_allow_super(prev_super);
                ObjProp::Prop(Prop {
                    computed,
                    key,
                    value,
                    kind: PropKind::Method,
                    method: true,
                    short_hand: false,
                    is_static: false,
                })
            } else if start.token.is_ident()
                || start.token == Token::Keyword(Keyword::Yield("yield"))
                || (!self.context.strict && start.token.matches_keyword(Keyword::Let(())))
            {
                if self.at_punct(Punct::Equal) {
                    self.context.first_covert_initialized_name_error =
                        Some(self.look_ahead.clone());
                    let _ = self.next_item()?;
                    let inner = self.isolate_cover_grammar(Self::parse_assignment_expr)?;
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
                    // self.set_isolate_cover_grammar_state(prev_bind, prev_assign, prev_first)?;
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
        log::debug!("prop: {:?}", prop);
        Ok((is_proto, prop))
    }

    fn is_proto_(key: &PropKey) -> bool {
        trace!("is_proto {:?}", key);
        match key {
            PropKey::Lit(ref l) => match l {
                Lit::String(ref s) => match s {
                    resast::prelude::StringLit::Single(ref s)
                    | resast::prelude::StringLit::Double(ref s) => s == "__proto__",
                },
                _ => false,
            },
            PropKey::Expr(Expr::Ident(ref ident)) | PropKey::Pat(Pat::Ident(ref ident)) => {
                ident.name == "__proto__"
            }
            _ => false,
        }
    }

    fn at_possible_ident(&self) -> bool {
        self.look_ahead.token.is_ident()
            || self.look_ahead.token.is_keyword()
            || self.look_ahead.token.is_null()
            || if let Token::Boolean(_) = self.look_ahead.token {
                true
            } else {
                false
            }
    }

    fn parse_template_lit(&mut self, is_tagged: bool) -> Res<TemplateLit<'b>> {
        debug!(
            "{}: parse_template_Lit {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        if !self.look_ahead.token.is_template_head() {
            return self
                .expected_token_error(&self.look_ahead, &["template head", "template no sub"]);
        }
        let mut expressions = Vec::new();
        let mut quasis = Vec::new();
        let quasi = self.parse_template_element(is_tagged)?;
        let mut breaking = quasi.tail;
        quasis.push(quasi);
        while !breaking {
            expressions.push(self.parse_expression()?);
            let quasi = self.parse_template_element(is_tagged)?;
            breaking = quasi.tail;
            quasis.push(quasi);
        }
        Ok(TemplateLit {
            expressions,
            quasis,
        })
    }

    fn parse_template_element(&mut self, is_tagged: bool) -> Res<TemplateElement<'b>> {
        debug!(
            "{}: parse_template_element {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let item = self.next_item()?;
        if let Token::Template(t) = item.token {
            let raw = self.get_string(&item.span)?;
            let (cooked, tail) = match t {
                Template::Head(c) => (c, false),
                Template::Middle(c) => (c, false),
                Template::Tail(c) => (c, true),
                Template::NoSub(c) => (c, true),
            };
            if !is_tagged && cooked.contains_octal_escape {
                return Err(Error::OctalLiteral(item.location.start));
            }

            if !is_tagged
                && (cooked.contains_invalid_unicode_escape || cooked.contains_invalid_hex_escape)
            {
                return Err(Error::InvalidEscape(
                    item.location.start,
                    "Invalid unicode escape in template literal".to_string(),
                ));
            }
            Ok(TemplateElement::from(tail, cooked.content, raw))
        } else {
            self.expected_token_error(&self.look_ahead, &["Template part"])
        }
    }

    fn parse_function_expr(&mut self) -> Res<Expr<'b>> {
        debug!(
            "{}: parse_function_expr {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let start_pos = self.look_ahead_position;
        let is_async = self.at_contextual_keyword("async");
        if is_async {
            let _ = self.next_item()?;
        }
        self.expect_keyword(Keyword::Function(()))?;
        let is_gen = self.at_punct(Punct::Asterisk);
        if is_gen {
            let _ = self.next_item()?;
        }
        let prev_await = self.context.allow_await;
        let prev_yield = self.context.allow_yield;
        let prev_super = self.context.allow_super;
        debug!("setting allow_super to {}", false);
        self.context.set_allow_super(false);
        debug!("setting allow_await to {}", is_async);
        self.context.allow_await = !is_async;
        self.context.allow_yield = !is_gen;
        let mut found_restricted = false;
        self.add_scope(lexical_names::Scope::FuncTop);
        let id = if !self.at_punct(Punct::OpenParen) {
            let id_pos = self.look_ahead_position;
            let item = self.look_ahead.clone();
            let id = self.parse_fn_name(is_gen)?;
            self.context.lexical_names.declare(
                id.name.clone(),
                lexical_names::DeclKind::Func(self.context.is_module),
                id_pos,
            )?;
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
        if self.context.strict && formal_params::have_duplicates(&formal_params.params) {
            return Err(Error::NonStrictFeatureInStrictContext(
                start_pos,
                "duplicate function parameter names".to_string(),
            ));
        }
        found_restricted = found_restricted || formal_params.found_restricted;
        let prev_strict = self.context.strict;
        let prev_oct = self.context.found_directive_octal_escape;
        let prev_allow_strict_directive = self.context.allow_strict_directive;
        self.context.allow_strict_directive = formal_params.simple;
        let start = self.look_ahead.clone();
        let body = self.parse_function_source_el()?;
        if self.context.strict && found_restricted {
            return self.unexpected_token_error(&start, "restricted ident in strict context");
        }
        if !prev_strict && self.context.strict {
            if formal_params::have_duplicates(&formal_params.params) {
                return Err(Error::NonStrictFeatureInStrictContext(
                    start_pos,
                    "duplicate function parameter names".to_string(),
                ));
            }
            if formal_params.found_restricted {
                return Err(Error::StrictModeArgumentsOrEval(start.location.start));
            }
        }
        self.context.strict = prev_strict;
        self.context.found_directive_octal_escape = prev_oct;
        self.context.allow_strict_directive = prev_allow_strict_directive;
        self.context.allow_yield = prev_yield;
        self.context.allow_await = prev_await;
        debug!("setting allow_super to {}", prev_super);
        self.context.set_allow_super(prev_super);
        self.remove_scope();
        let func = Func {
            id,
            params: formal_params.params,
            body,
            generator: is_gen,
            is_async,
        };
        Ok(Expr::Func(func))
    }

    fn parse_fn_name(&mut self, is_gen: bool) -> Res<resast::spanned::Ident<'b>> {
        debug!(
            "{}: parse_fn_name {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        if self.context.strict && !is_gen && self.at_keyword(Keyword::Yield(())) {
            self.parse_ident_name()
        } else {
            self.parse_var_ident(false)
        }
    }

    fn parse_ident_name(&mut self) -> Res<resast::spanned::Ident<'b>> {
        debug!(
            "{}: parse_ident_name {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let ident = self.next_item()?;
        match &ident.token {
            Token::Ident(_) | Token::Keyword(_) | Token::Boolean(_) | Token::Null => (),
            _ => return self.expected_token_error(&ident, &["identifier"]),
        }
        let slice = self.slice_from(&ident.span)?;

        Ok(resast::spanned::Ident { slice })
    }

    fn parse_var_ident(&mut self, is_var: bool) -> Res<resast::spanned::Ident<'b>> {
        debug!(
            "{}: parse_var_ident {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let ident = self.next_item()?;
        if ident.token.matches_keyword(Keyword::Yield(()))
            && (self.context.strict || !self.context.allow_yield)
        {
            return Err(Error::InvalidYield(ident.location.start));
        } else if !ident.token.is_ident() && !ident.token.matches_keyword(Keyword::Await(())) {
            if self.context.strict && ident.token.is_keyword() && ident.token.is_strict_reserved() {
                return Err(Error::NonStrictFeatureInStrictContext(
                    ident.location.start,
                    format!(
                        "{} is a strict reserved word",
                        self.get_slice(&ident.span)?.source,
                    ),
                ));
            } else if self.context.strict
                || (!ident.token.is_strict_reserved()
                    && !ident.token.matches_keyword(Keyword::Let(()))
                    && !ident.token.matches_keyword(Keyword::Await(())))
                || !is_var
            {
                return self.expected_token_error(&ident, &["variable identifier", "let", "await"]);
            }
        } else if (self.context.is_module || !self.context.allow_await)
            && &self.original[ident.span.start..ident.span.end] == "await"
        {
            debug!(
                "invalid await await: {}, module: {}",
                self.context.allow_await, self.context.is_module
            );
            return self.expected_token_error(&ident, &["variable identifier"]);
        }
        let i = match ident.token {
            Token::Ident(_) => {
                let slice = self.get_slice(&ident)?;
                resast::spanned::Ident {
                    slice,
                }
            }
            Token::Keyword(ref k) => {
                if k.is_reserved()
                    || k == &Keyword::Enum(())
                    || (self.context.strict && k.is_strict_reserved())
                {
                    return self.unexpected_token_error(&ident, "reserved word as ident");
                } else {
                    let slice = self.get_slice(&ident)?;
                    resast::spanned::Ident {
                        slice,
                    }
                }
            }
            _ => self.expected_token_error(&ident, &["variable identifier"])?,
        };
        Ok(i)
    }

    fn parse_formal_params(&mut self) -> Res<FormalParams<'b>> {
        debug!(
            "{}: parse_formal_params {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let open_paren = self.expect_punct(Punct::OpenParen)?;
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
        let close_paren = self.expect_punct(Punct::CloseParen)?;

        Ok(FormalParams {
            open_paren,
            params: args,
            close_paren,
            strict: false,
            found_restricted,
            simple,
        })
    }

    fn parse_formal_param(&mut self, simple: bool) -> Res<(bool, bool, FuncArg<'b>)> {
        debug!(
            "{}: parse_formal_param {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let start = self.look_ahead_position;
        let mut params: Vec<Item<&'b str>> = Vec::new();
        let (found_restricted, param) = if self.at_punct(Punct::Ellipsis) {
            self.parse_rest_element(&mut params)?
        } else {
            self.parse_pattern_with_default(&mut params)?
        };
        self.declare_pat(&param, DeclKind::Var(self.context.is_module), start)?;
        let param = FuncArg::Pat(param);
        let simple = simple && Self::is_simple(&param);
        Ok((simple, found_restricted, param))
    }

    fn parse_rest_element(&mut self, params: &mut Vec<Item<&'b str>>) -> Res<(bool, Pat<'b>)> {
        debug!(
            "{}: parse_rest_element {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        self.expect_punct(Punct::Ellipsis)?;
        let (restricted, arg) = self.parse_pattern(false, params)?;
        let ret = Pat::RestElement(Box::new(arg));
        if self.at_punct(Punct::Equal) {
            return self.expected_token_error(&self.look_ahead, &["not assignment"]);
        }
        if !self.at_punct(Punct::CloseParen) {
            return self.expected_token_error(&self.look_ahead, &[")"]);
        }
        Ok((restricted, ret))
    }

    fn parse_binding_rest_el(&mut self, params: &mut Vec<Item<&'b str>>) -> Res<RestPat<'b>> {
        debug!(
            "{}: parse_binding_rest_el {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let dots = self.expect_punct(Punct::Ellipsis)?;
        let (b, pat) = self.parse_pattern(false, params)?;
        Ok(RestPat {
            dots,
            pat
        })
    }

    fn parse_pattern_with_default(
        &mut self,
        params: &mut Vec<Item<&'b str>>,
    ) -> Res<(bool, Pat<'b>)> {
        debug!(
            "{}: parse_pattern_with_default {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let (is_restricted, ret) = self.parse_pattern(false, params)?;
        if self.at_punct(Punct::Equal) {
            let assign = self.next_item()?;
            let eq = self.get_slice(&assign)?;
            let prev_yield = self.context.allow_yield;
            self.context.allow_yield = true;
            let right = self.isolate_cover_grammar(Self::parse_assignment_expr)?;
            self.context.allow_yield = prev_yield;
            return Ok((
                is_restricted,
                Pat::Assign(AssignPat {
                    left: Box::new(ret),
                    eq,
                    right: Box::new(right),
                }),
            ));
        }
        Ok((is_restricted, ret))
    }

    fn parse_pattern(
        &mut self,
        is_var: bool,
        params: &mut Vec<Item<&'b str>>,
    ) -> Res<(bool, Pat<'b>)> {
        debug!(
            "{}: parse_pattern {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        if let Some(slice) = self.at_punct(Punct::OpenBracket) {
            self.parse_array_pattern(params)
        } else if self.at_punct(Punct::OpenBrace) {
            self.parse_object_pattern()
        } else {
            let ident = self.parse_var_ident(is_var)?;
            if !is_var && ident.slice.source == "let" {
                return self.expected_token_error(&self.look_ahead, &["identifier"]);
            }
            let restricted = ident.slice.source == "eval" || ident.slice.source == "arguments";
            params.push(self.look_ahead.clone());
            Ok((restricted, Pat::Ident(ident)))
        }
    }

    fn parse_array_pattern(
        &mut self,
        params: &mut Vec<Item<&'b str>>,
    ) -> Res<(bool, Pat<'b>)> {
        debug!(
            "{}: parse_array_pattern {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let open_bracket = self.expect_punct(Punct::OpenBracket)?;
        let mut elements = Vec::new();
        while !self.at_punct(Punct::CloseBracket) {
            if self.at_punct(Punct::Comma) {
                let comma = self.slice_from(&self.look_ahead);
                let _ = self.next_item()?;
                elements.push(ArrayElement {
                    part: None,
                    comma,
                });
            } else {
                if self.at_punct(Punct::Ellipsis) {
                    let el = self.parse_binding_rest_el(params)?;
                    let comma = if self.at_punct(Punct::Comma) {
                        self.slice_from(&self.look_ahead);
                    } else {
                        None
                    };
                    let ele = ArrayElement {
                        part: Some(ArrayPatPart::Rest(el)),
                        comma,
                    };
                    elements.push(ele);
                    break;
                } else {
                    let (_, el) = self.parse_pattern_with_default(params)?;
                    let comma = if self.at_punct(Punct::Comma) {
                        self.slice_from(&self.look_ahead);
                    } else {
                        None
                    };
                    let ele = ArrayElement {
                        part: Some(ArrayPatPart::Pat(el)),
                        comma,
                    };
                    elements.push(ele);
                }
                if !self.at_punct(Punct::CloseBracket) {
                    self.expect_punct(Punct::Comma)?;
                }
            }
        }
        self.expect_punct(Punct::CloseBracket)?;
        Ok((false, Pat::Array(elements)))
    }

    fn parse_object_pattern(&mut self) -> Res<(bool, Pat<'b>)> {
        debug!(
            "{}: parse_object_pattern {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let open_brace = self.expect_punct(Punct::OpenBrace)?;
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

    fn parse_rest_prop(&mut self) -> Res<ObjPatPart<'b>> {
        debug!(
            "{}: parse_rest_prop {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        self.expect_punct(Punct::Ellipsis)?;
        let (_, arg) = self.parse_pattern(false, &mut Vec::new())?;
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

    fn parse_property_pattern(&mut self) -> Res<ObjPatPart<'b>> {
        debug!(
            "{}: parse_property_pattern {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
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

    fn parse_assignment_expr(&mut self) -> Res<Expr<'b>> {
        debug!(
            "{}: parse_assignment_expr {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        if !self.context.allow_yield && self.at_keyword(Keyword::Yield(())) {
            self.parse_yield_expr()
        } else {
            let ress::Span { start, end } = self.look_ahead.span;
            let start_pos = self.look_ahead_position;
            let mut current = self.parse_conditional_expr()?;
            let curr_line = self.look_ahead_position.line;
            let start_line = start_pos.line;
            if &self.original[start..end] == "async"
                && curr_line == start_line
                && (self.look_ahead.token.is_ident() || self.at_keyword(Keyword::Yield(())))
            {
                let arg = self.parse_primary_expression()?;
                if self.context.strict {
                    if let Expr::Ident(ref ident) = arg {
                        if Self::is_strict_reserved(ident) {
                            return Err(Error::NonStrictFeatureInStrictContext(
                                self.current_position,
                                "strict reserved word as an identifier".to_string(),
                            ));
                        }
                    }
                }
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
                self.context.set_is_assignment_target(false);
                self.context.set_is_binding_element(false);
                let is_async = Self::is_async(&current);
                let prev_strict = self.context.allow_strict_directive;
                let prev_await = self.context.allow_await;
                self.context.allow_await = !is_async;
                self.add_scope(lexical_names::Scope::FuncTop);
                if let Some(params) =
                    self.reinterpret_as_cover_formals_list(current.clone(), start_pos)?
                {
                    let mut simple = true;
                    for arg in &params {
                        if self.context.strict && Self::check_arg_strict_mode(arg) {
                            return Err(Error::StrictModeArgumentsOrEval(self.current_position));
                        }
                        if !Self::is_simple(arg) {
                            simple = false;
                        }
                        if Self::is_invalid_await(arg) {
                            return Err(Error::InvalidParameter(
                                start_pos,
                                "Await used as the right hand side of a default pattern"
                                    .to_string(),
                            ));
                        }
                    }
                    if formal_params::have_duplicates(&params) {
                        return Err(Error::InvalidParameter(
                            start_pos,
                            "duplicate parameter name".to_string(),
                        ));
                    }
                    self.expect_fat_arrow()?;
                    if self.at_punct(Punct::OpenBrace) {
                        let prev_in = self.context.allow_in;
                        let prev_strict = self.context.allow_strict_directive;
                        self.context.allow_in = true;
                        self.context.allow_strict_directive = simple;
                        let body = self.parse_function_source_el()?;
                        self.remove_scope();
                        self.context.allow_await = prev_await;
                        self.context.allow_in = prev_in;
                        self.context.allow_strict_directive = prev_strict;
                        current = Expr::ArrowFunc(ArrowFuncExpr {
                            id: None,
                            expression: false,
                            generator: false,
                            is_async,
                            params,
                            body: ArrowFuncBody::FuncBody(body),
                        });
                    } else {
                        let a = self.isolate_cover_grammar(Self::parse_assignment_expr)?;
                        self.context.allow_await = prev_await;
                        self.remove_scope();
                        current = Expr::ArrowFunc(ArrowFuncExpr {
                            id: None,
                            expression: true,
                            generator: false,
                            is_async,
                            params,
                            body: ArrowFuncBody::Expr(Box::new(a)),
                        });
                    };
                    self.context.allow_strict_directive = prev_strict;
                }
            } else if self.at_assign() {
                if !self.context.is_assignment_target && !self.config.tolerant {
                    return self
                        .unexpected_token_error(&self.look_ahead, "Not at assignment target");
                }
                let assign = self.parse_assignment_after_start(current)?;
                return Ok(Expr::Assign(assign));
            }
            Ok(current)
        }
    }
    fn parse_assignment_after_start(&mut self, start: Expr<'b>) -> Res<AssignExpr<'b>> {
        if self.context.strict && Self::is_ident(&start) {
            if let Expr::Ident(ref i) = start {
                if Self::is_restricted_word(i) || Self::is_strict_reserved(i) {
                    return self
                        .expected_token_error(&self.look_ahead, &[&format!("not {}", i.name)]);
                }
            }
        }
        let left = if !self.at_punct(Punct::Equal) {
            self.context.set_is_assignment_target(false);
            self.context.set_is_binding_element(false);
            AssignLeft::Expr(Box::new(start))
        } else if let Expr::Func(_) = &start {
            return Err(Error::InvalidLHS(self.look_ahead_position));
        } else if !Self::is_ident(&start) && Self::is_reinterpret_target(&start) {
            AssignLeft::Pat(self.reinterpret_expr_as_pat(start)?)
        } else {
            AssignLeft::Expr(Box::new(start))
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
                            "=", "+=", "-=", "/=", "*=", "**=", "|=", "&=", "~=", "%=", "<<=",
                            ">>=", ">>>=",
                        ],
                    );
                }
            }
            _ => {
                return self.expected_token_error(
                    &item,
                    &[
                        "=", "+=", "-=", "/=", "*=", "**=", "|=", "&=", "~=", "%=", "<<=", ">>=",
                        ">>>=",
                    ],
                );
            }
        };
        let right = self.isolate_cover_grammar(Self::parse_assignment_expr)?;
        self.context.first_covert_initialized_name_error = None;
        Ok(AssignExpr {
            operator: op,
            left,
            right: Box::new(right),
        })
    }
    /// Check if an arg is a strict mode restricted identifier
    /// returns true if the arg _is_ an error
    fn check_arg_strict_mode(arg: &FuncArg<'b>) -> bool {
        match arg {
            FuncArg::Expr(Expr::Ident(ref ident)) | FuncArg::Pat(Pat::Ident(ref ident)) => {
                ident.name == "arguments" || ident.name == "eval"
            }
            _ => false,
        }
    }

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

    fn is_arrow_param_placeholder(expr: &Expr) -> bool {
        match expr {
            Expr::ArrowParamPlaceHolder(_, _) => true,
            _ => false,
        }
    }
    /// Returns a pair with first element indicating
    /// that an argument is not simple and the second
    /// being the formalized arguments list
    fn reinterpret_as_cover_formals_list(
        &mut self,
        expr: Expr<'b>,
        pos: Position,
    ) -> Res<Option<Vec<FuncArg<'b>>>> {
        let (params, async_arrow) = if let Expr::Ident(ref ident) = expr {
            if self.context.strict && Self::is_strict_reserved(ident) {
                return Err(Error::NonStrictFeatureInStrictContext(
                    self.current_position,
                    "strict reserved word as an identifier".to_string(),
                ));
            }
            (vec![FuncArg::Expr(expr)], false)
        } else if let Expr::ArrowParamPlaceHolder(params, is_async) = expr {
            (params, is_async)
        } else {
            return Ok(None);
        };
        let mut invalid_param = false;
        let param_len = params.len();
        let mut params2 = Vec::with_capacity(param_len);
        for param in params {
            match &param {
                FuncArg::Pat(pat) => {
                    self.declare_pat(pat, DeclKind::Var(self.context.is_module), pos)?
                }
                FuncArg::Expr(expr) => self.context.lexical_names.declare_expr(
                    expr,
                    DeclKind::Var(self.context.is_module),
                    pos,
                )?,
            }

            if Self::is_assignment(&param) {
                match &param {
                    FuncArg::Pat(ref p) => {
                        if let Pat::Assign(ref a) = p {
                            self.context.allow_super_call = false;
                            match &*a.right {
                                Expr::Yield(ref y) => {
                                    if y.argument.is_some() {
                                        invalid_param = true;
                                    } else {
                                        if self.context.strict {
                                            return Err(Error::NonStrictFeatureInStrictContext(
                                                self.current_position,
                                                "strict reserved word as an identifier".to_string(),
                                            ));
                                        }
                                        params2.push(FuncArg::Pat(Pat::Ident(
                                            resast::Ident::from("yield"),
                                        )));
                                        continue;
                                    }
                                }
                                Expr::Await(_) => return Err(Error::UnexpectedToken(
                                    pos,
                                    "await is invalid in a default expression for a function arg"
                                        .to_string(),
                                )),
                                _ => (),
                            }
                        }
                    }
                    FuncArg::Expr(ref e) => {
                        if let Expr::Assign(ref a) = e {
                            match &*a.right {
                                Expr::Yield(ref y) => {
                                    if y.argument.is_some() {
                                        invalid_param = true;
                                    } else {
                                        if self.context.strict {
                                            return Err(Error::NonStrictFeatureInStrictContext(
                                                self.current_position,
                                                "strict reserved word as an identifier".to_string(),
                                            ));
                                        }
                                        params2.push(FuncArg::Expr(Expr::Ident(
                                            resast::Ident::from("yield"),
                                        )));
                                        continue;
                                    }
                                }
                                Expr::Await(_) => return Err(Error::UnexpectedToken(
                                    pos,
                                    "await is invalid in a default expression for a function arg"
                                        .to_string(),
                                )),
                                _ => (),
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
        let mut found_non_simple = false;
        for param in params2.iter() {
            if let FuncArg::Expr(ref e) = param {
                if let Expr::Yield(_) = e {
                    if self.context.strict && !self.context.allow_yield {
                        return self.expected_token_error(
                            &self.look_ahead,
                            &["not a yield expression in a function param"],
                        );
                    }
                }
            }
            if !found_non_simple && !Self::is_simple(param) {
                found_non_simple = true;
            }
        }
        if found_non_simple {
            self.context.allow_strict_directive = false;
        }
        Ok(Some(params2))
    }

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

    fn is_invalid_await(arg: &FuncArg) -> bool {
        match arg {
            FuncArg::Expr(Expr::Assign(AssignExpr { right, .. }))
            | FuncArg::Pat(Pat::Assign(AssignPat { right, .. })) => match &**right {
                Expr::Ident(id) => id.name == "await",
                Expr::Func(Func { id, params, .. })
                | Expr::ArrowFunc(ArrowFuncExpr { id, params, .. }) => {
                    id.as_ref().map(|id| id.name == "await").unwrap_or(false)
                        || params.iter().any(|param| Self::is_await(param))
                }
                Expr::Spread(expr) => {
                    if let Expr::Ident(id) = expr.as_ref() {
                        id.name == "await"
                    } else {
                        false
                    }
                }
                _ => false,
            },
            _ => false,
        }
    }

    fn reinterpret_expr_as_pat(&self, ex: Expr<'b>) -> Res<Pat<'b>> {
        debug!(
            "{}: reinterpret_expr_as_pat {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
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

    fn parse_yield_expr(&mut self) -> Res<Expr<'b>> {
        debug!(
            "{}: parse_yield_expr {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let keyword = self.expect_keyword(Keyword::Yield(()))?;
        let mut argument: Option<Box<Expr>> = None;
        let mut star = None;
        if !self.context.has_line_term {
            let prev_yield = self.context.allow_yield;
            self.context.allow_yield = false;
            if self.at_punct(Punct::Asterisk) {
                let start = self.next_item()?;
                star = Some(self.get_slice(&start)?);
                arg = Some(Box::new(self.parse_assignment_expr()?));
            } else if self.is_start_of_expr() {
                arg = Some(Box::new(self.parse_assignment_expr()?));
            }
            self.context.allow_yield = prev_yield;
        }
        let y = YieldExpr {
            keyword,
            argument,
            star,
        };
        Ok(Expr::Yield(y))
    }

    fn parse_conditional_expr(&mut self) -> Res<Expr<'b>> {
        debug!(
            "{}: parse_conditional_expr {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let expr = self.inherit_cover_grammar(Self::parse_binary_expression)?;
        if self.at_punct(Punct::QuestionMark) {
            let _question_mark = self.next_item()?;
            let prev_in = self.context.allow_in;
            self.context.allow_in = true;
            let if_true = self.isolate_cover_grammar(Self::parse_assignment_expr)?;
            self.context.allow_in = prev_in;

            self.expect_punct(Punct::Colon)?;
            let if_false = self.isolate_cover_grammar(Self::parse_assignment_expr)?;

            let c = ConditionalExpr {
                test: Box::new(expr),
                alternate: Box::new(if_false),
                consequent: Box::new(if_true),
            };
            self.context.set_is_assignment_target(false);
            self.context.set_is_binding_element(false);
            return Ok(Expr::Conditional(c));
        }
        Ok(expr)
    }

    fn parse_binary_expression(&mut self) -> Res<Expr<'b>> {
        debug!(
            "{}: parse_binary_expression {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let mut current = self.inherit_cover_grammar(Self::parse_exponentiation_expression)?;
        let token = self.look_ahead.clone();
        let mut prec = self.bin_precedence(&token.token);
        if prec > 0 {
            self.next_item()?;
            self.context.set_is_assignment_target(false);
            self.context.set_is_binding_element(false);
            let mut left = current.clone();
            let mut right = self.isolate_cover_grammar(Self::parse_exponentiation_expression)?;
            let mut stack = vec![left.clone(), right.clone()];
            let mut ops = vec![token];
            let mut precs = vec![prec];
            loop {
                prec = self.bin_precedence(&self.look_ahead.token);
                if prec < 1 {
                    break;
                }
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
                            operator: self.logical_operator(&op).ok_or_else(|| {
                                self.op_error("Unable to convert logical operator")
                            })?,
                            left: Box::new(left),
                            right: Box::new(right),
                        }));
                    } else {
                        let operator = self.binary_operator(&op)
                            .ok_or_else(|| self.op_error("Unable to convert binary operator"))?;
                        stack.push(Expr::Binary(BinaryExpr {
                            operator,
                            left: Box::new(left),
                            right: Box::new(right),
                        }));
                    }
                }
                ops.push(self.next_item()?);
                precs.push(prec);
                let exp = self.isolate_cover_grammar(Self::parse_exponentiation_expression)?;
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
                    let operator = self.logical_operator(&op)
                        .ok_or_else(|| self.op_error("Unable to convert logical operator"))?;
                    current = Expr::Logical(LogicalExpr {
                        operator,
                        left: Box::new(stack.pop().ok_or_else(|| {
                            self.op_error("invalid logical operation, too few expressions")
                        })?),
                        right: Box::new(current),
                    })
                } else {
                    let operator = self.binary_operator(&op)
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

    fn parse_exponentiation_expression(&mut self) -> Res<Expr<'b>> {
        debug!(
            "{}: parse_exponentiation_expression",
            self.look_ahead.span.start
        );
        let expr = self.inherit_cover_grammar(Self::parse_unary_expression)?;
        if self.at_punct(Punct::DoubleAsterisk) {
            if let Expr::Unary(_) = expr {
                return Err(Error::OperationError(
                    self.look_ahead_position,
                    "Unary operation cannot be the left hand side of an exponentiation expression."
                        .to_string(),
                ));
            }
            let stars = self.next_item()?;
            let stars = self.get_slice(&stars)?;
            self.context.set_is_assignment_target(false);
            self.context.set_is_binding_element(false);
            let left = expr;
            let right = self.isolate_cover_grammar(Self::parse_exponentiation_expression)?;
            return Ok(Expr::Binary(BinaryExpr {
                operator: BinaryOp::PowerOf(stars),
                left: Box::new(left),
                right: Box::new(right),
            }));
        }

        Ok(expr)
    }

    fn parse_unary_expression(&mut self) -> Res<Expr<'b>> {
        debug!(
            "{}: parse_unary_expression {:?} allow_await: {}",
            self.look_ahead.span.start, self.look_ahead.token, self.context.allow_await
        );
        if self.at_punct(Punct::Plus)
            || self.at_punct(Punct::Dash)
            || self.at_punct(Punct::Tilde)
            || self.at_punct(Punct::Bang)
            || self.at_keyword(Keyword::Delete(()))
            || self.at_keyword(Keyword::Void(()))
            || self.at_keyword(Keyword::TypeOf(()))
        {
            let op = self.next_item()?;
            let arg = self.inherit_cover_grammar(Self::parse_unary_expression)?;
            if op.token.matches_keyword(Keyword::Delete(()))
                && self.context.strict
                && Self::is_ident(&arg)
                && !self.config.tolerant
            {
                return self.unexpected_token_error(&op, "Cannot delete ident in strict mode");
            }
            self.context.set_is_assignment_target(false);
            self.context.set_is_binding_element(false);
            let operator = self.unary_operator(op)
                .ok_or_else(|| self.op_error("Unable to convert unary operator"))?;
            Ok(Expr::Unary(UnaryExpr {
                operator,
                argument: Box::new(arg),
            }))
        } else if !self.context.allow_await && self.at_keyword(Keyword::Await(())) {
            debug!("parsing await expr");
            self.parse_await_expr()
        } else {
            self.parse_update_expr()
        }
    }

    fn unary_operator(&self, token: Item<&str>) -> Option<UnaryOp> {
        let slice = self.slice_from(&token)?;
        match item.token {
            Token::Punct(ref p) => match p {
                Punct::Dash => Some(UnaryOp::Minus(slice)),
                Punct::Plus => Some(UnaryOp::Plus(slice)),
                Punct::Bang => Some(UnaryOp::Not(slice)),
                Punct::Tilde => Some(UnaryOp::Tilde(slice)),
                _ => None,
            },
            Token::Keyword(ref k) => match k {
                Keyword::TypeOf(_) => Some(UnaryOp::TypeOf(slice)),
                Keyword::Void(_) => Some(UnaryOp::Void(slice)),
                Keyword::Delete(_) => Some(UnaryOp::Delete(slice)),
                _ => None,
            },
            _ => None,
        }
    }

    fn binary_operator(&self, token: &Item<&str>) -> Option<BinaryOp> {
        let slice = self.slice_from(token)?;
        match &token.token {
            Token::Keyword(ref key) => match key {
                Keyword::InstanceOf(_) => Some(BinaryOp::InstanceOf(slice)),
                Keyword::In(_) => Some(BinaryOp::In(slice)),
                _ => None,
            },
            Token::Punct(ref p) => match p {
                Punct::DoubleEqual => Some(BinaryOp::Equal(slice)),
                Punct::BangEqual => Some(BinaryOp::NotEqual(slice)),
                Punct::TripleEqual => Some(BinaryOp::StrictEqual(slice)),
                Punct::BangDoubleEqual => Some(BinaryOp::StrictNotEqual(slice)),
                Punct::LessThan => Some(BinaryOp::LessThan(slice)),
                Punct::LessThanEqual => Some(BinaryOp::LessThanEqual(slice)),
                Punct::GreaterThan => Some(BinaryOp::GreaterThan(slice)),
                Punct::GreaterThanEqual => Some(BinaryOp::GreaterThanEqual(slice)),
                Punct::DoubleLessThan => Some(BinaryOp::LeftShift(slice)),
                Punct::DoubleGreaterThan => Some(BinaryOp::RightShift(slice)),
                Punct::TripleGreaterThan => Some(BinaryOp::UnsignedRightShift(slice)),
                Punct::Plus => Some(BinaryOp::Plus(slice)),
                Punct::Dash => Some(BinaryOp::Minus(slice)),
                Punct::Asterisk => Some(BinaryOp::Times(slice)),
                Punct::ForwardSlash => Some(BinaryOp::Over(slice)),
                Punct::Percent => Some(BinaryOp::Mod(slice)),
                Punct::Ampersand => Some(BinaryOp::And(slice)),
                Punct::Pipe => Some(BinaryOp::Or(slice)),
                Punct::Caret => Some(BinaryOp::XOr(slice)),
                _ => None,
            },
            _ => None,
        }
    }

    fn logical_operator(&self, token: &Item<&str>) -> Option<LogicalOp> {
        let slice = self.slice_from(token)?;
        match token {
            Token::Punct(ref p) => match p {
                Punct::DoubleAmpersand => Some(LogicalOp::And(slice)),
                Punct::DoublePipe => Some(LogicalOp::Or(slice)),
                _ => None,
            },
            _ => None,
        }
    }

    fn parse_await_expr(&mut self) -> Res<Expr<'b>> {
        debug!(
            "{}: parse_await_expr {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        if self.context.allow_await {
            self.unexpected_token_error(&self.look_ahead, "await is not valid in this context")?;
        }
        let keyword = self.next_item()?;
        let keyword = self.get_slice(&keyword)?;
        let expr = self.parse_unary_expression()?;
        let ret = AwaitExpr {
            keyword,
            expr,
        };
        Ok(Expr::Await(Box::new(ret)))
    }

    fn parse_update_expr(&mut self) -> Res<Expr<'b>> {
        debug!(
            "{}: parse_update_expr {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let start = self.look_ahead.clone();
        if self.at_punct(Punct::DoublePlus) || self.at_punct(Punct::DoubleDash) {
            let op = self.next_item()?;
            let slice = self.get_slice(&op)?;
            let operator = match op.token {
                Token::Punct(ref p) => match p {
                    Punct::DoublePlus => UpdateOp::Increment(slice),
                    Punct::DoubleDash => UpdateOp::Decrement(slice),
                    _ => unreachable!("Already validated that the next token would be ++ or --"),
                },
                _ => unreachable!("Already validated that the next token would be ++ or --"),
            };
            let start = self.look_ahead.clone();
            let ex = self.inherit_cover_grammar(Self::parse_unary_expression)?;
            if let Expr::Ident(ref i) = ex {
                if Self::is_restricted_word(i) && self.context.strict {
                    return self.unexpected_token_error(&start, "restricted ident");
                }
            }
            if !self.context.is_assignment_target && !self.config.tolerant {
                return self
                    .unexpected_token_error(&op, "Cannot increment when not at assignment target");
            }
            let ret = UpdateExpr {
                operator,
                argument: Box::new(ex),
            };
            self.context.set_is_assignment_target(false);
            self.context.set_is_binding_element(false);
            Ok(Expr::Update(ret))
        } else {
            let expr = self.inherit_cover_grammar(Self::parse_left_hand_side_expr_allow_call)?;
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
                self.context.set_is_assignment_target(false);
                self.context.set_is_binding_element(false);
                let slice = self.get_slice(&op)?;
                let ret = UpdateExpr {
                    operator: if op.token.matches_punct(Punct::DoublePlus) {
                        UpdateOp::Increment(slice)
                    } else if op.token.matches_punct(Punct::DoubleDash) {
                        UpdateOp::Decrement(slice)
                    } else {
                        return self.expected_token_error(&op, &["++", "--"]);
                    },
                    argument: Box::new(expr),
                };
                return Ok(Expr::Update(ret));
            }
            Ok(expr)
        }
    }

    fn is_func_decl(stmt: &Stmt) -> bool {
        if let Stmt::Expr(Expr::Func(_)) = stmt {
            true
        } else {
            false
        }
    }

    fn is_labeled_func(stmt: &Stmt) -> bool {
        match stmt {
            Stmt::Labeled(stmt) => {
                Self::is_func_decl(&stmt.body) || Self::is_labeled_func(&stmt.body)
            }
            _ => false,
        }
    }

    fn is_ident(expr: &Expr) -> bool {
        match expr {
            Expr::Ident(_) => true,
            _ => false,
        }
    }

    fn parse_left_hand_side_expr(&mut self) -> Res<Expr<'b>> {
        if !self.context.allow_in {
            return Err(Error::InvalidUseOfContextualKeyword(
                self.current_position,
                "in".to_string(),
            ));
        }
        let mut expr = if self.at_keyword(Keyword::Super(())) && self.context.in_function_body {
            self.parse_super()?
        } else {
            let ret = if self.at_keyword(Keyword::New(())) {
                self.inherit_cover_grammar(Self::parse_new_expr)?
            } else {
                self.inherit_cover_grammar(Self::parse_primary_expression)?
            };
            ret
        };
        loop {
            if self.at_punct(Punct::OpenBracket) {
                self.context.set_is_binding_element(false);
                self.context.set_is_assignment_target(true);
                self.expect_punct(Punct::OpenBracket)?;
                let prop = self.isolate_cover_grammar(Self::parse_expression)?;
                self.expect_punct(Punct::CloseBracket)?;
                let member = MemberExpr {
                    computed: true,
                    object: Box::new(expr),
                    property: Box::new(prop),
                };
                debug!(target: "look_ahead", "{:?}", member);
                expr = Expr::Member(member);
            } else if self.at_punct(Punct::Period) {
                self.context.set_is_binding_element(false);
                self.context.set_is_assignment_target(false);
                self.expect_punct(Punct::Period)?;
                let prop = self.parse_ident_name()?;
                let member = MemberExpr {
                    object: Box::new(expr),
                    property: Box::new(Expr::Ident(prop)),
                    computed: false,
                };
                expr = Expr::Member(member);
            } else if self.look_ahead.is_template() {
                let quasi = self.parse_template_lit(true)?;
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

    /// Will parse a pending super expression.
    ///
    /// > note: This will handle any invalid super expression
    /// scenarios
    fn parse_super(&mut self) -> Res<Expr<'b>> {
        let super_position = self.look_ahead_position;
        if !self.context.allow_super {
            return Err(Error::InvalidSuper(super_position));
        }
        let keyword = self.expect_keyword(Keyword::Super(()))?;
        if self.at_punct(Punct::OpenParen) && !self.context.allow_super_call {
            return Err(Error::InvalidSuper(super_position));
        }
        if !self.at_punct(Punct::OpenBracket)
            && !self.at_punct(Punct::Period)
            && !self.at_punct(Punct::OpenParen)
        {
            return self.expected_token_error(&self.look_ahead, &["[", ".", "("]);
        }
        Ok(Expr::Super(slice))
    }

    fn parse_left_hand_side_expr_allow_call(&mut self) -> Res<Expr<'b>> {
        debug!(
            "{}: parse_left_hand_side_expr_allow_call",
            self.look_ahead.span.start
        );
        let start_pos = self.look_ahead_position;
        let is_async = self.at_contextual_keyword("async");
        let prev_in = self.context.allow_in;
        self.context.allow_in = true;

        let mut expr = if self.at_keyword(Keyword::Super(())) {
            self.parse_super()?
        } else {
            let ret = if self.at_keyword(Keyword::New(())) {
                self.inherit_cover_grammar(Self::parse_new_expr)?
            } else {
                self.inherit_cover_grammar(Self::parse_primary_expression)?
            };
            ret
        };
        loop {
            if self.at_punct(Punct::Period) {
                self.context.set_is_binding_element(false);
                self.context.set_is_assignment_target(true);
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
                self.context.set_is_binding_element(false);
                self.context.set_is_assignment_target(false);
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
                self.context.set_is_assignment_target(true);
                self.context.set_is_binding_element(false);
                self.expect_punct(Punct::OpenBracket)?;
                let prop = self.isolate_cover_grammar(Self::parse_expression)?;
                self.expect_punct(Punct::CloseBracket)?;
                let member = MemberExpr {
                    object: Box::new(expr),
                    computed: true,
                    property: Box::new(prop),
                };
                debug!(target: "look_ahead", "{:?}", member);
                expr = Expr::Member(member);
            } else if self.look_ahead.token.is_template_head() {
                let quasi = self.parse_template_lit(true)?;
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
    fn parse_async_args(&mut self) -> Res<Vec<Expr<'b>>> {
        debug!(
            "{}: parse_async_args {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        self.expect_punct(Punct::OpenParen)?;
        let prev_await = self.context.allow_await;
        self.context.allow_await = false;
        let mut ret = Vec::new();
        if !self.at_punct(Punct::CloseParen) {
            loop {
                let (arg, spread) = if self.at_punct(Punct::Ellipsis) {
                    (self.parse_spread_element()?, true)
                } else {
                    let arg = self.isolate_cover_grammar(Self::parse_async_arg)?;
                    (arg, false)
                };
                ret.push(arg);
                if self.at_punct(Punct::CloseParen) {
                    break;
                }
                let comma_position = self.look_ahead_position;
                self.expect_comma_sep()?;
                if self.at_punct(Punct::CloseParen) {
                    if spread {
                        return Err(Error::UnexpectedToken(
                            comma_position,
                            "trailing comma in function args after a rest parameter is illegal"
                                .to_string(),
                        ));
                    }
                    break;
                }
            }
        }
        self.expect_punct(Punct::CloseParen)?;
        self.context.allow_await = prev_await;
        Ok(ret)
    }
    /// Parse an argument of an async function
    /// note: not sure this is needed
    fn parse_async_arg(&mut self) -> Res<Expr<'b>> {
        debug!(
            "{}: parse_async_arg {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let expr = self.parse_assignment_expr()?;
        self.context.first_covert_initialized_name_error = None;
        Ok(expr)
    }
    /// Expect a comma separator,
    /// if parsing with tolerance we can tolerate
    /// a non-existent comma
    fn expect_comma_sep(&mut self) -> Res<()> {
        debug!(
            "{}: expect_comma_sep {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        self.expect_punct(Punct::Comma)
    }

    /// Parse an expression preceded by the `...` operator
    fn parse_spread_element(&mut self) -> Res<Expr<'b>> {
        debug!(
            "{}: parse_spread_element {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        self.expect_punct(Punct::Ellipsis)?;
        let arg = self.inherit_cover_grammar(Self::parse_assignment_expr)?;
        Ok(Expr::Spread(Box::new(arg)))
    }
    /// Parse function arguments, expecting to open with `(` and close with `)`
    fn parse_args(&mut self) -> Res<(Slice<'b>, Vec<Expr<'b>>, Slice<'b>)> {
        debug!(
            "{}: parse_args {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let open_paren = self.expect_punct(Punct::OpenParen)?;
        let mut args = Vec::new();
        if !self.at_punct(Punct::CloseParen) {
            loop {
                let expr = if self.at_punct(Punct::Ellipsis) {
                    self.parse_spread_element()?
                } else {
                    let expr = self.isolate_cover_grammar(Self::parse_assignment_expr)?;
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
        let close_paren = self.expect_punct(Punct::CloseParen)?;
        Ok((open_paren, args, close_paren))
    }
    /// This will parse one of two expressions `new Thing()`
    /// or `new.target`. The later is only valid in a function
    /// body
    fn parse_new_expr(&mut self) -> Res<Expr<'b>> {
        debug!(
            "{}: parse_new_expr {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        let item = self.next_item()?;
        if let Token::Keyword(ref key) = &item.token {
            if key.has_unicode_escape() {
                return self.unexpected_token_error(&item, "`new` cannot contain unicode escapes");
            }
        } else {
            return self.expected_token_error(&item, &["new"]);
        }
        let keyword = self.get_slice(&item)?;
        if self.at_punct(Punct::Period) {
            let _ = self.next_item()?;
            if self.at_contextual_keyword("target") && self.context.in_function_body {
                let property = self.parse_ident_name()?;
                Ok(Expr::MetaProp(MetaProp {
                    meta: resast::spanned::Ident::from { slice: keyword },
                    property,
                }))
            } else {
                self.expected_token_error(&self.look_ahead, &["[constructor function call]"])
            }
        } else if self.at_keyword(Keyword::Import(())) {
            self.expected_token_error(&self.look_ahead, &["not import"])
        } else {
            let callee = self.isolate_cover_grammar(Self::parse_left_hand_side_expr)?;
            let (open_paren, arguments, close_paren) = if self.at_punct(Punct::OpenParen) {
                self.parse_args()?
            } else {
                Vec::new()
            };
            self.context.set_is_assignment_target(false);
            self.context.set_is_binding_element(false);
            let new = NewExpr {
                callee: Box::new(callee),
                open_paren,
                arguments,
                close_paren,
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
                if k == &Keyword::InstanceOf(()) || (self.context.allow_in && k == &Keyword::In(()))
                {
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

    fn isolate_cover_grammar<T>(&mut self, f: impl Fn(&mut Self) -> Res<T>) -> Res<T> {
        let is_binding = self.context.set_is_binding_element(true);
        let is_assign = self.context.set_is_assignment_target(true);
        let first_covert = self.context.first_covert_initialized_name_error.take();
        let ret = f(self)?;
        if let Some(_name_err) = &self.context.first_covert_initialized_name_error {
            //TODO: throwUnexpectedToken
        }
        self.context.set_is_binding_element(is_binding);
        self.context.set_is_assignment_target(is_assign);
        self.context.first_covert_initialized_name_error = first_covert;
        Ok(ret)
    }

    fn inherit_cover_grammar<T>(&mut self, f: impl Fn(&mut Self) -> Res<T>) -> Res<T> {
        let is_binding = self.context.set_is_binding_element(true);
        let is_assign = self.context.set_is_assignment_target(true);
        let prev_first = self.context.first_covert_initialized_name_error.take();
        let ret = f(self)?;
        self.context
            .set_is_binding_element(self.context.is_binding_element && is_binding);
        self.context
            .set_is_assignment_target(self.context.is_assignment_target && is_assign);
        if prev_first.is_some() {
            self.context.first_covert_initialized_name_error = prev_first;
        }
        Ok(ret)
    }

    /// Request the next token from the scanner
    /// swap the last look ahead with this new token
    /// and return the last token
    fn next_item(&mut self) -> Res<Item<&'b str>> {
        trace!("next_item {}", self.context.has_line_term);
        let mut comment_line_term = false;
        loop {
            self.context.has_line_term = comment_line_term || self.scanner.has_pending_new_line();
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
                    trace!(
                        "next_item comment {} {:?}",
                        self.context.has_line_term,
                        look_ahead.token
                    );
                    if let Token::Comment(ref inner) = look_ahead.token {
                        if inner.is_multi_line() {
                            comment_line_term =
                                self.context.has_line_term || Self::comment_has_line_term(inner);
                        }
                        if self.context.is_module
                            && (inner.is_html() || inner.tail_content.is_some())
                        {
                            return Err(Error::HtmlCommentInModule(self.look_ahead_position));
                        }
                    }
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

    fn comment_has_line_term(comment: &Comment<&'b str>) -> bool {
        if let ress::tokens::CommentKind::Multi = comment.kind {
            comment
                .content
                .chars()
                .any(|c| c == '\n' || c == '\r' || c == '\u{2028}' || c == '\u{2029}')
        } else {
            false
        }
    }

    /// Get the next token and validate that it matches
    /// the punct provided, discarding the result
    /// if it does
    fn expect_punct(&mut self, p: Punct) -> Res<Slice<'b>> {
        let next = self.next_item()?;
        if !next.token.matches_punct(p) {
            return self.expected_token_error(&next, &[&format!("{:?}", p)]);
        }

        self.slice_from(&next).ok_or_else(|| self.op_error("extracting slice from item"))
    }
    fn expect_fat_arrow(&mut self) -> Res<()> {
        if self.look_ahead.token.matches_punct(Punct::EqualGreaterThan) {
            if self.context.has_line_term {
                Err(Error::NewLineAfterFatArrow(self.look_ahead_position))
            } else {
                let _ = self.next_item()?;
                Ok(())
            }
        } else {
            self.expected_token_error(&self.look_ahead, &["=>"])
        }
    }
    /// move on to the next item and validate it matches
    /// the keyword provided, discarding the result
    /// if it does
    fn expect_keyword(&mut self, k: Keyword<()>) -> Res<Slice<'b>> {
        let next = self.next_item()?;
        if !next.token.matches_keyword(k) {
            return self.expected_token_error(&next, &[&format!("{:?}", k)]);
        }
        Ok(Slice {
            loc: resast::spanned::SourceLocation {
                start: resast::spanned::Position {
                    line: next.location.start.line,
                    column: next.location.start.column,
                },
                end: resast::spanned::Position {
                    line: next.location.end.line,
                    column: next.location.end.column,
                },
            },
            source: Cow::Borrowed(Cow::Borrowed(self.get_string(&next.span)?))
        })
    }
    fn at_return_arg(&self) -> bool {
        if self.context.has_line_term {
            return self.look_ahead.is_string() || self.look_ahead.is_template();
        }
        !self.at_punct(Punct::SemiColon)
            && !self.at_punct(Punct::CloseBrace)
            && !self.look_ahead.is_eof()
    }
    fn at_import_call(&mut self) -> Res<bool> {
        debug!(
            "{}: at_import_call {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        if self.at_keyword(Keyword::Import(())) {
            let state = self.scanner.get_state();
            self.scanner.skip_comments()?;
            let ret = if let Some(next) = self.scanner.next() {
                let next = next?;
                next.token.matches_punct(Punct::OpenParen)
            } else {
                false
            };
            self.scanner.set_state(state);
            Ok(ret)
        } else {
            Ok(false)
        }
    }
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
    fn at_lexical_decl(&mut self) -> bool {
        if let Token::Keyword(Keyword::Let(ref s)) = self.look_ahead.token {
            if s.contains("\\u") {
                return false;
            }
        }
        let state = self.scanner.get_state();
        self.scanner.skip_comments().unwrap();
        let ret = if let Some(next) = self.scanner.next() {
            let next = next.unwrap();
            next.token.is_ident()
                || next.token.matches_punct(Punct::OpenBracket)
                || next.token.matches_punct(Punct::OpenBrace)
                || next.token.matches_keyword(Keyword::Let(()))
                || next.token.matches_keyword(Keyword::Yield(()))
        } else {
            false
        };
        self.scanner.set_state(state);
        ret
    }
    /// Test for if the next token is a specific punct
    fn at_punct(&self, p: Punct) -> bool {
        self.look_ahead.token.matches_punct(p)
    }
    /// Test for if the next token is a specific keyword
    fn at_keyword(&self, k: Keyword<()>) -> bool {
        self.look_ahead.token.matches_keyword(k) 
    }
    /// This test is for all the operators that might be part
    /// of an assignment statement
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
    fn at_async_function(&mut self) -> bool {
        debug!(
            "{}: at_async_function {:?}",
            self.look_ahead.span.start, self.look_ahead.token
        );
        if self.at_contextual_keyword("async") {
            !self.scanner.has_pending_new_line()
                && if let Some(peek) = self.scanner.look_ahead() {
                    if let Ok(peek) = peek {
                        peek.token.matches_keyword(Keyword::Function(()))
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
    fn consume_semicolon(&mut self) -> Res<()> {
        trace!("consume_semicolon {}", self.context.has_line_term);
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
    fn at_contextual_keyword(&self, s: &str) -> bool {
        debug!("at_contextual_keyword {:?}", s);
        let slice = self.slice_from(&self.look_ahead)?;
        slice.source == s
    }

    fn slice_from(&self, item: &Item<&str>) -> Option<Slice<'b>> {
        let slice = self.scanner.str_for(&item.span)?;
        Some(Slice {
            loc: resast::spanned::SourceLocation {
                start: resast::spanned::Position {
                    line: item.location.start.line,
                    column: item.location.start.column,
                },
                end: resast::spanned::Position { 
                    line: item.location.end.line,
                    column: item.location.end.column,
                }
            },
            source: Cow::Borrowed(slice),
        })
    }
    /// Sort of keywords `eval` and `arguments` have
    /// a special meaning and will cause problems
    /// if used in the wrong scope
    fn is_restricted_word(word: &resast::spanned::Ident) -> bool {
        &word.name == "eval" || &word.name == "arguments"
    }
    /// Check if this &str is in the list of reserved
    /// words in the context of 'use strict'
    fn is_strict_reserved(word: &resast::spanned::Ident) -> bool {
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
            ret = token.matches_keyword(Keyword::Class(()))
                || token.matches_keyword(Keyword::Delete(()))
                || token.matches_keyword(Keyword::Function(()))
                || token.matches_keyword(Keyword::Let(()))
                || token.matches_keyword(Keyword::New(()))
                || token.matches_keyword(Keyword::Super(()))
                || token.matches_keyword(Keyword::This(()))
                || token.matches_keyword(Keyword::TypeOf(()))
                || token.matches_keyword(Keyword::Void(()))
                || token.matches_keyword(Keyword::Yield(()))
        }
        if token.is_regex() {
            ret = true;
        }
        ret
    }

    fn at_big_int_flag(&self) -> bool {
        let Span { start, end } = self.look_ahead.span;
        &self.original[start..end] == "n"
    }

    fn get_slice(&self, item: &Item<&str>) -> Res<Slice<'b>> {
        self
            .slice_from(item)
            .ok_or_else(|| self.op_error("Unable to get slice from scanner"))
    }

    fn expected_token_error<T>(&self, item: &Item<&'b str>, expectation: &[&str]) -> Res<T> {
        if cfg!(feature = "error_backtrace") {
            let bt = backtrace::Backtrace::new();
            error!("{:?}", bt);
        }
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
    fn unexpected_token_error<T>(&self, item: &Item<&'b str>, msg: &str) -> Res<T> {
        if cfg!(feature = "error_backtrace") {
            let bt = backtrace::Backtrace::new();
            error!("{:?}", bt);
        }
        let pos = item.location.start;

        let name = self.scanner.string_for(&item.span).unwrap_or_default();
        Err(Error::UnexpectedToken(
            pos,
            format!("Found unexpected token: {}; {}", name, msg),
        ))
    }
    fn tolerate_error(&self, err: Error) -> Result<(), Error> {
        if !self.config.tolerant {
            if cfg!(feature = "error_backtrace") {
                let bt = backtrace::Backtrace::new();
                error!("{:?}", bt);
            }
            Err(err)
        } else {
            Ok(())
        }
    }
    fn op_error(&self, msg: &str) -> Error {
        if cfg!(feature = "error_backtrace") {
            let bt = backtrace::Backtrace::new();
            error!("{:?}", bt);
        }
        Error::OperationError(self.current_position, msg.to_owned())
    }
    fn redecl_error(&self, name: &str) -> Error {
        if cfg!(feature = "error_backtrace") {
            let bt = backtrace::Backtrace::new();
            error!("{:?}", bt);
        }
        Error::Redecl(self.current_position, name.to_owned())
    }
    fn reinterpret_error(&self, from: &str, to: &str) -> Error {
        if cfg!(feature = "error_backtrace") {
            let bt = backtrace::Backtrace::new();
            error!("{:?}", bt);
        }
        Error::UnableToReinterpret(self.current_position, from.to_owned(), to.to_owned())
    }

    pub fn next_position(&self) -> SourceLocation {
        self.look_ahead.location
    }

    fn next_part(&mut self) -> Res<ProgramPart<'b>> {
        trace!(
            "next_part past_prolog: {}, strict: {}",
            self.context.past_prolog,
            self.context.strict
        );
        if self.context.is_module {
            self.context.strict = true;
        }
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
        let ret = match self.parse_statement_list_item(None) {
            Ok(p) => {
                if self.context.is_module && self.look_ahead.is_eof() {
                    if self.context.lexical_names.has_undefined_exports() {
                        let names = self.context.lexical_names.get_undefined_exports();
                        self.context.errored = true;
                        self.found_eof = true;
                        return Err(Error::UndefinedExports(names));
                    }
                }
                p
            }
            Err(e) => {
                self.context.errored = true;
                return Err(e);
            }
        };
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

#[derive(Clone, Copy, Debug, PartialEq)]
enum StmtCtx<'a> {
    Do,
    For,
    If,
    Label(&'a str),
    While,
    With,
}
