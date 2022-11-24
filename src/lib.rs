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

use ress::prelude::*;
pub use ress::Span;

mod comment_handler;
mod error;
mod formal_params;
mod lexical_names;
mod lhs;
mod regex;
pub mod spanned;

pub use crate::comment_handler::CommentHandler;
pub use crate::comment_handler::DefaultCommentHandler;
pub use crate::error::Error;

use resast::prelude::*;

use std::collections::HashMap;

/// The current configuration options.
/// This will most likely increase over time
struct Config {
    /// whether or not to tolerate a subset of errors
    tolerant: bool,
}
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum LabelKind {
    Iteration,
    Other,
    Unknown,
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
    /// if super is allowed as a keyword
    allow_super: bool,
    /// if super is allowed to be part of a call expression
    /// allow_super should always be true when this is true
    /// but not the other way around. This is only valid in a
    /// constructor
    allow_super_call: bool,
    /// If we have found any possible naming errors
    /// which are not yet resolved
    first_covert_initialized_name_error: Option<Item<&'a str>>,
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
    label_set: HashMap<&'a str, LabelKind>,
    /// If the current scope has a `'use strict';` directive
    /// in the prelude
    strict: bool,
    lexical_names: lexical_names::DuplicateNameDetector<'a>,
    /// If the scanner has a pending line terminator
    /// before the next token
    has_line_term: bool,
    /// If we have passed the initial prelude where a valid
    /// `'use strict'` directive would exist
    past_prolog: bool,
    /// If we encounter an error, the iterator should stop
    errored: bool,
    /// If we find a directive with an octal escape
    /// we need to error if a 'use strict' directive
    /// is then found
    found_directive_octal_escape: bool,
}

impl Default for Config {
    fn default() -> Self {
        log::trace!("default config");
        Self { tolerant: false }
    }
}

impl<'a> Default for Context<'a> {
    fn default() -> Self {
        log::trace!("default context",);
        Self {
            is_module: false,
            allow_await: true,
            allow_in: true,
            allow_strict_directive: true,
            allow_yield: true,
            allow_super: false,
            allow_super_call: false,
            first_covert_initialized_name_error: None,
            is_assignment_target: false,
            is_binding_element: false,
            in_function_body: false,
            in_iteration: false,
            in_switch: false,
            label_set: HashMap::new(),
            strict: false,
            lexical_names: lexical_names::DuplicateNameDetector::default(),
            has_line_term: false,
            past_prolog: false,
            errored: false,
            found_directive_octal_escape: false,
        }
    }
}
impl<'a> Context<'a> {
    #[tracing::instrument(level = "trace", skip(self))]
    pub fn set_allow_super(&mut self, value: bool) {
        self.allow_super = value;
    }
    #[tracing::instrument(level = "trace", skip(self))]
    pub fn set_is_assignment_target(&mut self, value: bool) -> bool {
        let old = self.is_assignment_target;
        self.is_assignment_target = value;
        old
    }
    #[tracing::instrument(level = "trace", skip(self))]
    pub fn set_is_binding_element(&mut self, value: bool) -> bool {
        let old = self.is_binding_element;
        self.is_binding_element = value;
        old
    }
}
/// This is used to create a `Parser` using
/// the builder method
#[derive(Default)]
pub struct Builder<'b> {
    inner: crate::spanned::Builder<'b>,
}

impl<'b> Builder<'b> {
    pub fn new() -> Self {
        Self::default()
    }
    /// Enable or disable error tolerance
    /// default: `false`
    pub fn set_tolerant(&mut self, value: bool) {
        self.inner.set_tolerant(value);
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
        self.inner.set_module(value);
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
        self.inner.set_js(js);
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
        let inner = self.inner.build()?;
        Ok(Parser { inner })
    }
}

impl<'b> Builder<'b> {
    pub fn with_comment_handler<CH>(self, handler: CH) -> Res<Parser<'b, CH>>
    where
        CH: CommentHandler<'b>,
    {
        let inner = self.inner.with_comment_handler(handler)?;
        Ok(Parser { inner })
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
    inner: crate::spanned::Parser<'a, CH>,
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
        let inner = crate::spanned::Parser::new(text)?;
        Ok(Self { inner })
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
        let ret = self.inner.parse()?;
        Ok(ret.into())
    }

    pub fn next_position(&self) -> SourceLocation {
        self.inner.next_position()
    }

    pub fn comment_handler(&self) -> &CH {
        &self.inner.comment_handler
    }
    pub fn comment_handler_mut(&mut self) -> &mut CH {
        &mut self.inner.comment_handler
    }
}

impl<'b, CH> Iterator for Parser<'b, CH>
where
    CH: CommentHandler<'b> + Sized,
{
    type Item = Res<ProgramPart<'b>>;
    fn next(&mut self) -> Option<Self::Item> {
        let ret = self.inner.next()?;
        Some(ret.map(Into::into))
    }
}
