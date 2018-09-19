#![allow(unused)]
extern crate ress;
#[macro_use]
extern crate log;
extern crate env_logger;

use ress::{Item, Keyword, Punct, Scanner, Template, Token, Span};
mod error;
pub mod node;
use error::Error;
use node::Position;
use std::{collections::HashSet, mem::replace};

static mut DEBUG: bool = false;
#[allow(unused)]
struct Config {
    tolerant: bool,
    comments: bool,
}
#[allow(unused)]
struct Context {
    is_module: bool,
    allow_in: bool,
    allow_strict_directive: bool,
    allow_yield: bool,
    await: bool,
    first_covert_initialized_name_error: Option<Item>,
    is_assignment_target: bool,
    is_binding_element: bool,
    in_function_body: bool,
    in_iteration: bool,
    in_switch: bool,
    label_set: HashSet<String>,
    strict: bool,
    has_line_term: bool,
    past_prolog: bool,
}

impl Default for Config {
    fn default() -> Self {
        debug!(target: "resp:debug", "default");
        Self {
            tolerant: false,
            comments: false,
        }
    }
}

impl Default for Context {
    fn default() -> Self {
        debug!(target: "resp:debug", "default");
        Self {
            is_module: false,
            await: false,
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
        }
    }
}

pub struct Builder {
    tolerant: bool,
    is_module: bool,
    comments: bool,
    js: String,
}

impl Builder {
    pub fn new() -> Self {
        Self {
            tolerant: false,
            is_module: false,
            comments: false,
            js: String::new(),
        }
    }

    pub fn set_tolerant(&mut self, value: bool) {
        self.tolerant = value;
    }

    pub fn tolerant(&mut self, value: bool) -> &mut Self {
        self.set_tolerant(value);
        self
    }

    pub fn set_module(&mut self, value: bool) {
        self.is_module = false;
    }

    pub fn module(&mut self, value: bool) -> &mut Self {
        self.set_module(value);
        self
    }

    pub fn set_comments(&mut self, value: bool) {
        self.comments = value;
    }

    pub fn comments(&mut self, value: bool) -> &mut Self {
        self.set_comments(value);
        self
    }

    pub fn set_js(&mut self, js: impl Into<String>) {
        self.js = js.into();
    }

    pub fn js(&mut self, js: impl Into<String>) -> &mut Self {
        self.set_js(js);
        self
    }

    pub fn build(&self) -> Res<Parser> {
        let is_module = self.is_module;
        let comments = self.comments;
        let tolerant = self.tolerant;
        let lines = get_lines(&self.js);
        let scanner = Scanner::new(self.js.clone());
        Parser::build(is_module, tolerant, comments, scanner, lines)
    }
}

#[allow(unused)]
pub struct Parser {
    context: Context,
    config: Config,
    scanner: Scanner,
    lines: Vec<Line>,
    look_ahead: Item,
    found_eof: bool,
    tokens: Vec<Item>,
    comments: Vec<Item>,
    current_position: node::Position,
}
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Line {
    start: usize,
    end: usize,
}

type Res<T> = Result<T, Error>;

fn get_lines(text: &str) -> Vec<Line> {
    let mut line_start = 0;
    let mut byte_position = 0;
    let mut ret: Vec<Line> = text
        .chars()
        .filter_map(|c| {

            let ret = match c {
                '\r' => {
                    // look ahead 1 char to see if it is a newline pair
                    // if so, don't include it, it will get included in the next
                    // iteration
                    if let Some(next) = text.get(byte_position..byte_position + 2) {
                        if next == "\r\n" {
                            None
                        } else {
                            let ret = Line {
                                start: line_start,
                                end: byte_position,
                            };
                            line_start = byte_position + 1;
                            Some(ret)
                        }
                    } else {
                        None
                    }
                }
                '\n' | '\u{2028}' | '\u{2029}' => {
                    let ret = Line {
                        start: line_start,
                        end: byte_position,
                    };
                    line_start = byte_position + 1;
                    Some(ret)
                }
                _ => None,
            };
            byte_position += c.len_utf8();
            ret
        }).collect();

    ret.push(Line {
        start: line_start,
        end: text.len(),
    });
    ret
}

impl Parser {
    pub fn new(text: &str) -> Res<Self> {
        let lines = get_lines(text);
        let s = Scanner::new(text);
        let config = Config::default();
        let context = Context::default();
        Self::_new(s, lines, config, context)
    }

    pub(crate) fn build(is_module: bool, tolerant: bool, comments: bool, scanner: Scanner, lines: Vec<Line>) -> Res<Self> {
        let config = Config {
            tolerant,
            comments,
            ..Default::default()
        };
        let context = Context {
            is_module,
            ..Default::default()
        };
        Self::_new(scanner, lines, config, context)
    }

    fn _new(scanner: Scanner, lines: Vec<Line>, config: Config, context: Context) -> Res<Self> {
        let mut ret = Self {
            scanner,
            look_ahead: Item {
                token: Token::EoF,
                span: Span {
                    start: 0,
                    end: 0,
                }
            },
            lines,
            found_eof: false,
            config,
            context,
            tokens: vec![],
            comments: vec![],
            current_position: node::Position::start(),
        };
        let _ = ret.next_item()?;
        Ok(ret)
    }

    pub fn parse(&mut self) -> Res<node::Program> {
        debug!(target: "resp:debug", "parse_script");
        let mut body = vec![];
        while let Some(part) = self.next() {
            match part {
                Ok(part) => body.push(part),
                Err(e) => return Err(e)
            }
        }
        Ok(if self.context.is_module {
            node::Program::Module(body)
        } else {
            node::Program::Script(body)
        })
    }

    fn parse_directive_prologues(&mut self) -> Res<Vec<node::ProgramPart>> {
        debug!(target: "resp:debug", "parse_directive_prologues");
        let mut ret = vec![];
        loop {
            if !self.look_ahead.token.is_string() {
                break;
            }
            if let Some(dir) = self.parse_directive()? {
                if dir.directive == "use strict" {
                    if !self.context.allow_strict_directive {
                        return self.error(&self.look_ahead, &[]);
                    }
                    self.context.strict = true;
                }
                ret.push(node::ProgramPart::Directive(dir));
            } else {
                break;
            }
        }
        Ok(ret)
    }

    fn parse_directive(&mut self) -> Res<Option<node::Directive>> {
        debug!(target: "resp:debug", "parse_directive");
        if self.look_ahead.token.matches_string_content("use strict") {
            let dir = self.next_item()?;
            let ret = match dir.token {
                Token::String(s) => {
                    let quoted = s.to_string();
                    let directive = s.no_quote();
                    let expression = node::Literal::String(quoted);
                    Ok(Some(node::Directive {
                        expression,
                        directive,
                    }))
                }
                _ => unreachable!(),
            };
            self.consume_semicolon()?;
            ret
        } else {
            Ok(None)
        }
    }

    fn parse_expression(&mut self) -> Res<node::Expression> {
        debug!(target: "resp:debug", "parse_expression");
        let ret = self.isolate_cover_grammar(&Self::parse_assignment_expr)?;
        if self.at_punct(Punct::Comma) {
            let mut list = vec![ret];
            while !self.look_ahead.token.is_eof() {
                if !self.at_punct(Punct::Comma) {
                    break;
                }
                let _comma = self.next_item()?;
                list.push(self.isolate_cover_grammar(&Self::parse_assignment_expr)?);
            }
            return Ok(node::Expression::Sequence(list));
        }
        Ok(ret)
    }

    fn parse_statement_list_item(&mut self) -> Res<node::ProgramPart> {
        debug!(target: "resp:debug", "parse_statement_list_item_script");
        self.context.is_assignment_target = true;
        self.context.is_binding_element = true;
        let tok = self.look_ahead.token.clone();
        let ret = match &tok {
            Token::Keyword(ref k) => match k {
                &Keyword::Import => {
                    if self.at_import_call() {
                        let stmt = self.parse_statement()?;
                        Ok(node::ProgramPart::Statement(stmt))
                    } else {
                        if !self.context.is_module {
                            //Error
                        }
                        let import = self.parse_import_decl()?;
                        let decl = node::Declaration::Import(Box::new(import));
                        Ok(node::ProgramPart::Decl(decl))
                    }
                },
                &Keyword::Export => {
                    let export = self.parse_export_decl()?;
                    let decl = node::Declaration::Export(Box::new(export));
                    Ok(node::ProgramPart::Decl(decl))
                },
                &Keyword::Const => {
                    let decl = self.parse_lexical_decl(false)?;
                    Ok(node::ProgramPart::Decl(decl))
                }
                &Keyword::Function => {
                    let func = self.parse_function_decl(true)?;
                    let decl = node::Declaration::Function(func);
                    Ok(node::ProgramPart::Decl(decl))
                }
                &Keyword::Class => {
                    let class = self.parse_class_decl(false)?;
                    let decl = node::Declaration::Class(class);
                    Ok(node::ProgramPart::Decl(decl))
                }
                &Keyword::Let => Ok(if self.at_lexical_decl() {
                    let decl = self.parse_lexical_decl(false)?;
                    node::ProgramPart::Decl(decl)
                } else {
                    let stmt = self.parse_statement()?;
                    node::ProgramPart::Statement(stmt)
                }),
                _ => {
                    let stmt = self.parse_statement()?;
                    Ok(node::ProgramPart::Statement(stmt))
                }
            },
            _ => {
                let stmt = self.parse_statement()?;
                Ok(node::ProgramPart::Statement(stmt))
            }
        };
        ret
    }

    fn parse_import_decl(&mut self) -> Res<node::ModuleImport> {
        if self.context.in_function_body {
            //error
        }
        self.expect_keyword(Keyword::Import)?;
        if self.look_ahead.token.is_ident() {
            let source = self.parse_module_specifier()?;
            self.consume_semicolon()?;
            Ok(node::ModuleImport {
                specifiers: vec![],
                source,
            })
        } else {
            let specifiers = if self.at_punct(Punct::OpenBrace) {
                self.parse_named_imports()?
            } else if self.at_punct(Punct::Asterisk) {
                vec![self.parse_import_namespace_specifier()?]
            } else if self.at_possible_ident() && !self.at_keyword(Keyword::Default) {
                let mut specifiers = vec![self.parse_import_default_specifier()?];
                if self.at_punct(Punct::Comma) {
                    let _ = self.next_item()?;
                    if self.at_punct(Punct::Asterisk) {
                        specifiers.push(self.parse_import_namespace_specifier()?);
                    } else if self.at_punct(Punct::OpenBrace) {
                        specifiers.append(&mut self.parse_named_imports()?);
                    } else {
                        return self.error(&self.look_ahead, &["{", "*"]);
                    }
                }
                specifiers
            } else {
                return self.error(&self.look_ahead, &["{", "*", "[ident]"])
            };
            if !self.at_contextual_keyword("from") {
                return self.error(&self.look_ahead, &["from"]);
            }
            let _ = self.next_item()?;
            let source = self.parse_module_specifier()?;
            self.consume_semicolon()?;
            Ok(node::ModuleImport {
                specifiers,
                source,
            })
        }
    }

    fn at_possible_ident(&self) -> bool {
        self.look_ahead.token.is_ident()
        || self.look_ahead.token.is_keyword()
        || self.look_ahead.token.is_boolean()
        || self.look_ahead.token.is_null()
    }

    fn parse_named_imports(&mut self) -> Res<Vec<node::ImportSpecifier>> {
        self.expect_punct(Punct::OpenBrace)?;
        let mut ret = vec![];
        while !self.at_punct(Punct::CloseBrace) {
            ret.push(self.parse_import_specifier()?);
            if !self.at_punct(Punct::CloseBrace) {
                self.expect_punct(Punct::Comma)?;
            }
        }
        Ok(ret)
    }

    fn parse_import_specifier(&mut self) -> Res<node::ImportSpecifier> {
        let imported = if self.look_ahead.token.is_ident() {
            self.parse_var_ident(false)?
        } else {
            let imported = self.parse_ident_name()?;
            if !self.at_contextual_keyword("as") {
                return self.error(&self.look_ahead, &["as"]);
            }
            imported
        };
        let local = self.parse_local_import_ident()?;
        Ok(node::ImportSpecifier::Normal(imported, local))
    }

    fn parse_local_import_ident(&mut self) -> Res<Option<node::Identifier>> {
        Ok(if self.at_contextual_keyword("as") {
            Some(self.parse_var_ident(false)?)
        } else {
            None
        })
    }

    fn parse_import_namespace_specifier(&mut self) -> Res<node::ImportSpecifier> {
        self.expect_punct(Punct::Asterisk)?;
        if !self.at_contextual_keyword("as") {
            return self.error(&self.look_ahead, &["as"]);
        }
        let _ = self.next_item()?;
        let ident = self.parse_ident_name()?;
        Ok(node::ImportSpecifier::Namespace(ident))
    }

    fn parse_import_default_specifier(&mut self) -> Res<node::ImportSpecifier> {
        let ident = self.parse_ident_name()?;
        Ok(node::ImportSpecifier::Default(ident))
    }

    fn parse_export_decl(&mut self) -> Res<node::ModuleExport> {
        if self.context.in_function_body {
            //error
        }
        self.expect_keyword(Keyword::Export)?;
        if self.at_keyword(Keyword::Default) {
            let _ = self.next_item()?;
            let decl = if self.at_keyword(Keyword::Function) {
                let func = node::Declaration::Function(self.parse_function_decl(true)?);
                node::DefaultExportDecl::Decl(func)
            } else if self.at_keyword(Keyword::Class) {
                let class = node::Declaration::Class(self.parse_class_decl(true)?);
                node::DefaultExportDecl::Decl(class)
            } else if self.at_contextual_keyword("async") {
                if self.at_async_function() {
                    let func = self.parse_function_decl(true)?;
                    let decl = node::Declaration::Function(func);
                    node::DefaultExportDecl::Decl(decl)
                } else {
                    let expr = self.parse_assignment_expr()?;
                    node::DefaultExportDecl::Expr(expr)
                }
            } else {
                if self.at_contextual_keyword("from") {
                    //error
                }
                if self.at_punct(Punct::OpenBrace) {
                    let expr = self.parse_obj_init()?;
                    node::DefaultExportDecl::Expr(expr)
                } else if self.at_punct(Punct::OpenBracket) {
                    let expr = self.parse_array_init()?;
                    node::DefaultExportDecl::Expr(expr)
                } else {
                    let expr = self.parse_assignment_expr()?;
                    node::DefaultExportDecl::Expr(expr)
                }
            };
            Ok(node::ModuleExport::Default(decl))
        } else if self.at_punct(Punct::Asterisk) {
            let _ = self.next_item()?;
            if !self.at_contextual_keyword("from") {
                //error
            }
            let _ = self.next_item()?;
            let source = self.parse_module_specifier()?;
            Ok(node::ModuleExport::All(source))
        } else if self.look_ahead.token.is_keyword() {
            if self.look_ahead.token.matches_keyword(Keyword::Let) || self.look_ahead.token.matches_keyword(Keyword::Const) {
                let lex = self.parse_lexical_decl(false)?;
                let decl = node::NamedExportDecl::Decl(lex);
                Ok(node::ModuleExport::Named(decl))
            } else if self.look_ahead.token.matches_keyword(Keyword::Var) {
                let var = node::Declaration::Variable(node::VariableKind::Var, vec![self.parse_variable_decl(false)?]);
                let decl = node::NamedExportDecl::Decl(var);
                Ok(node::ModuleExport::Named(decl))
            } else if self.look_ahead.token.matches_keyword(Keyword::Class) {
                let class = self.parse_class_decl(true)?;
                let decl = node::Declaration::Class(class);
                let decl = node::NamedExportDecl::Decl(decl);
                Ok(node::ModuleExport::Named(decl))
            } else if  self.look_ahead.token.matches_keyword(Keyword::Function) {
                let func = self.parse_function_decl(true)?;
                let decl = node::Declaration::Function(func);
                let decl = node::NamedExportDecl::Decl(decl);
                Ok(node::ModuleExport::Named(decl))
            } else {
                return self.error(&self.look_ahead, &["let", "var", "const", "class", "function"]);
            }
        } else if self.at_async_function() {
            let func = self.parse_function_decl(false)?;
            let decl = node::Declaration::Function(func);
            let decl = node::NamedExportDecl::Decl(decl);
            Ok(node::ModuleExport::Named(decl))
        } else {
            self.expect_punct(Punct::OpenBrace)?;
            let mut specifiers = vec![];
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
            if self.at_contextual_keyword("from") {
                let _ = self.next_item()?;
                let source = self.parse_module_specifier()?;
                self.consume_semicolon()?;
                let decl = node::NamedExportDecl::Specifier(specifiers, Some(source));
                Ok(node::ModuleExport::Named(decl))
            } else if found_default {
                self.error(&self.look_ahead, &[""])
            } else {
                self.consume_semicolon()?;
                let decl = node::NamedExportDecl::Specifier(specifiers, None);
                Ok(node::ModuleExport::Named(decl))
            }
        }
    }

    fn parse_export_specifier(&mut self) -> Res<node::ExportSpecifier> {
        let local = self.parse_ident_name()?;
        let exported = if self.at_contextual_keyword("as") {
            Some(self.parse_ident_name()?)
        } else {
            None
        };
        Ok(node::ExportSpecifier {
            local,
            exported,
        })
    }

    fn parse_module_specifier(&mut self) -> Res<node::Literal> {
        let item = self.next_item()?;
        match &item.token {
            Token::String(ref s) => {
                Ok(node::Literal::String(s.to_string()))
            },
            _ => self.error(&item, &["[string]"])
        }
    }

    fn parse_statement(&mut self) -> Res<node::Statement> {
        debug!(target: "resp:debug", "parse_statement");
        let lh = self.look_ahead.token.clone();
        let stmt = match lh {
            Token::Boolean(_)
            | Token::Null
            | Token::Numeric(_)
            | Token::String(_)
            | Token::RegEx(_) => {
                let expr = self.parse_expression_statement()?;
                node::Statement::Expr(expr)
            }
            Token::Punct(ref p) => match p {
                Punct::OpenBrace => {
                    let b = self.parse_block()?;
                    node::Statement::Block(b)
                }
                Punct::OpenParen => {
                    let expr = self.parse_expression_statement()?;
                    node::Statement::Expr(expr)
                }
                Punct::SemiColon => {
                    let _ = self.next_item()?;
                    node::Statement::Empty
                }
                _ => {
                    let expr = self.parse_expression_statement()?;
                    node::Statement::Expr(expr)
                }
            },
            Token::Ident(_) => {
                if self.at_async_function() {
                    let f = self.parse_function_decl(true)?;
                    node::Statement::Expr(node::Expression::Function(f))
                } else {
                    self.parse_labelled_statement()?
                }
            }
            Token::Keyword(ref k) => match k {
                Keyword::Break => node::Statement::Break(self.parse_break_stmt()?),
                Keyword::Continue => node::Statement::Continue(self.parse_continue_stmt()?),
                Keyword::Debugger => self.parse_debugger_stmt()?,
                Keyword::Do => node::Statement::DoWhile(self.parse_do_while_stmt()?),
                Keyword::For => self.parse_for_stmt()?,
                Keyword::Function => node::Statement::Expr(self.parse_fn_stmt()?),
                Keyword::If => node::Statement::If(self.parse_if_stmt()?),
                Keyword::Return => node::Statement::Return(self.parse_return_stmt()?),
                Keyword::Switch => node::Statement::Switch(self.parse_switch_stmt()?),
                Keyword::Throw => node::Statement::Throw(self.parse_throw_stmt()?),
                Keyword::Try => node::Statement::Try(self.parse_try_stmt()?),
                Keyword::Var => self.parse_var_stmt()?,
                Keyword::While => node::Statement::While(self.parse_while_stmt()?),
                Keyword::With => node::Statement::With(self.parse_with_stmt()?),
                _ => node::Statement::Expr(self.parse_expression_statement()?),
            },
            _ => return self.error(&self.look_ahead, &[]),
        };
        Ok(stmt)
    }

    fn parse_with_stmt(&mut self) -> Res<node::WithStatement> {
        debug!(target: "resp:debug", "parse_with_stmt");
        if self.context.strict {
            // error
        }
        self.expect_keyword(Keyword::With)?;
        self.expect_punct(Punct::OpenParen)?;
        let obj = self.parse_expression()?;
        Ok(
            if !self.at_punct(Punct::CloseParen) && self.config.tolerant {
                //tolerate error
                node::WithStatement {
                    object: obj,
                    body: Box::new(node::Statement::Empty),
                }
            } else {
                self.expect_punct(Punct::CloseParen)?;
                node::WithStatement {
                    object: obj,
                    body: Box::new(self.parse_statement()?),
                }
            },
        )
    }

    fn parse_while_stmt(&mut self) -> Res<node::WhileStatement> {
        debug!(target: "resp:debug", "parse_while_stmt");
        self.expect_keyword(Keyword::While)?;
        self.expect_punct(Punct::OpenParen)?;
        let test = self.parse_expression()?;
        let body = if !self.at_punct(Punct::CloseParen) && self.config.tolerant {
            //tolerate error
            node::Statement::Empty
        } else {
            self.expect_punct(Punct::CloseParen)?;
            let prev_iter = self.context.in_iteration;
            let body = self.parse_statement()?;
            self.context.in_iteration = prev_iter;
            body
        };
        Ok(node::WhileStatement {
            test,
            body: Box::new(body),
        })
    }

    fn parse_var_stmt(&mut self) -> Res<node::Statement> {
        debug!(target: "resp:debug", "parse_var_stmt");
        self.expect_keyword(Keyword::Var)?;
        let decls = self.parse_var_decl_list(false)?;
        let stmt = node::Statement::Var(decls);
        self.consume_semicolon()?;
        Ok(stmt)
    }

    fn parse_var_decl_list(&mut self, in_for: bool) -> Res<Vec<node::VariableDecl>> {
        let mut ret = vec![self.parse_var_decl(in_for)?];
        while self.at_punct(Punct::Comma) {
            let _ = self.next_item()?;
            ret.push(self.parse_var_decl(in_for)?)
        }
        Ok(ret)
    }

    fn parse_var_decl(&mut self, in_for: bool) -> Res<node::VariableDecl> {
        let (_, patt) = self.parse_pattern(Some(node::VariableKind::Var), &mut vec![])?;
        if self.context.strict && patt.is_restricted() {
            //error
        }
        let init = if self.at_punct(Punct::Assign) {
            let _ = self.next_item()?;
            Some(self.isolate_cover_grammar(&Self::parse_assignment_expr)?)
        } else if !patt.is_ident() && !in_for {
            return self.error(&self.look_ahead, &["="]);
        } else {
            None
        };
        Ok(node::VariableDecl {
            id: patt,
            init,
        })
    }
    fn parse_try_stmt(&mut self) -> Res<node::TryStatement> {
        debug!(target: "resp:debug", "parse_try_stmt");
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
            //error: one or the other must be declared
        }
        Ok(node::TryStatement {
            block,
            handler,
            finalizer,
        })
    }

    fn parse_catch_clause(&mut self) -> Res<node::CatchClause> {
        debug!(target: "resp:debug", "parse_catch_clause");
        self.expect_keyword(Keyword::Catch)?;
        self.expect_punct(Punct::OpenParen)?;
        if self.at_punct(Punct::CloseParen) {
            //error variable named required
        }
        let mut params = vec![];
        let (_, param) = self.parse_pattern(None, &mut params)?;
        self.expect_punct(Punct::CloseParen)?;
        let body = self.parse_block()?;
        Ok(node::CatchClause { param, body })
    }

    fn parse_finally_clause(&mut self) -> Res<node::BlockStatement> {
        debug!(target: "resp:debug", "parse_finally_clause");
        self.expect_keyword(Keyword::Finally)?;
        self.parse_block()
    }

    fn parse_throw_stmt(&mut self) -> Res<node::Expression> {
        debug!(target: "resp:debug", "parse_throw_stmt");
        self.expect_keyword(Keyword::Throw)?;
        if self.context.has_line_term {
            //error: no new line allowed after throw
        }
        let arg = self.parse_expression()?;
        self.consume_semicolon()?;
        Ok(arg)
    }

    fn parse_switch_stmt(&mut self) -> Res<node::SwitchStatement> {
        debug!(target: "resp:debug", "parse_switch_stmt");
        self.expect_keyword(Keyword::Switch)?;
        self.expect_punct(Punct::OpenParen)?;
        let discriminant = self.parse_expression()?;
        self.expect_punct(Punct::CloseParen)?;
        self.expect_punct(Punct::OpenBrace)?;

        let prev_sw = self.context.in_switch;
        self.context.in_switch = true;
        let mut found_default = false;
        let mut cases = vec![];
        loop {
            if self.at_punct(Punct::CloseBrace) {
                break;
            }
            let case = self.parse_switch_case()?;
            if case.test.is_none() {
                if found_default {
                    return self.error(&self.look_ahead, &[]);
                }
                found_default = true;
            }
            cases.push(case);
        }
        self.expect_punct(Punct::CloseBrace)?;
        self.context.in_switch = prev_sw;
        Ok(node::SwitchStatement {
            discriminant,
            cases,
        })
    }

    fn parse_switch_case(&mut self) -> Res<node::SwitchCase> {
        debug!(target: "resp:debug", "parse_switch_case");
        let test = if self.at_keyword(Keyword::Default) {
            self.expect_keyword(Keyword::Default)?;
            None
        } else {
            self.expect_keyword(Keyword::Case)?;
            Some(self.parse_expression()?)
        };
        self.expect_punct(Punct::Colon)?;
        let mut consequent = vec![];
        loop {
            if self.at_punct(Punct::CloseBrace)
                || self.at_keyword(Keyword::Default)
                || self.at_keyword(Keyword::Case)
            {
                break;
            }
            consequent.push(self.parse_statement_list_item()?)
        }
        Ok(node::SwitchCase { test, consequent })
    }

    fn parse_return_stmt(&mut self) -> Res<Option<node::Expression>> {
        debug!(target: "resp:debug", "parse_return_stmt");
        if !self.context.in_function_body {
            //tolerate error
        }
        self.expect_keyword(Keyword::Return)?;
        // if we are at a semi-colon,or close curly brace or eof
        //the return doesn't have an arg. If we are at a line term
        //we need to account for a string literal or template literal
        //since they both can have new lines
        let ret = if (self.context.has_line_term && (self.look_ahead.token.is_string() || self.look_ahead.token.is_template()))
            || (!self.context.has_line_term
            && !self.at_punct(Punct::SemiColon)
            && !self.at_punct(Punct::CloseBrace)
            && !self.look_ahead.token.is_eof())
        {
            Some(self.parse_expression()?)
        } else {
            None
        };
        debug!(target: "resp:debug", "return statement: {:?}", ret);
        self.consume_semicolon()?;
        Ok(ret)
    }

    fn parse_if_stmt(&mut self) -> Res<node::IfStatement> {
        debug!(target: "resp:debug", "parse_if_stmt");
        self.expect_keyword(Keyword::If)?;
        self.expect_punct(Punct::OpenParen)?;
        let test = self.parse_expression()?;
        let (consequent, alternate) = if !self.at_punct(Punct::CloseParen) && self.config.tolerant {
            //tolerate error
            (Box::new(node::Statement::Empty), None)
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
        Ok(node::IfStatement {
            test,
            consequent,
            alternate,
        })
    }

    fn parse_if_clause(&mut self) -> Res<node::Statement> {
        debug!(target: "resp:debug", "parse_if_clause");
        if self.context.strict && self.at_keyword(Keyword::Function) {
            //tolerate error
        }
        self.parse_statement()
    }

    fn parse_fn_stmt(&mut self) -> Res<node::Expression> {
        debug!(target: "resp:debug", "parse_fn_stmt");
        let decl = self.parse_function_decl(true)?;
        Ok(node::Expression::Function(decl))
    }

    fn parse_for_stmt(&mut self) -> Res<node::Statement> {
        debug!(target: "resp:debug", "parse_for_stmt");

        self.expect_keyword(Keyword::For)?;
        let is_await = if self.at_keyword(Keyword::Await) {
            let _ = self.next_item()?;
            true
        // for await ([lookahead ≠ let] LeftHandSideExpression [?Yield, ?Await] of AssignmentExpression [+In, ?Yield, ?Await]) Statement [?Yield, ?Await, ?Return]
        // for await (var ForBinding [?Yield, ?Await] of AssignmentExpression [+In, ?Yield, ?Await]) Statement [?Yield, ?Await, ?Return]
        // for await (ForDeclaration [?Yield, ?Await] of AssignmentExpression [+In, ?Yield, ?Await]) Statement[?Yield, ?Await, ?Return]
        } else {
            false
        };
        self.expect_punct(Punct::OpenParen)?;
        if self.at_punct(Punct::SemiColon) {
            // any semi-colon would mean standard C style for loop
            // for (;;) {}
            let stmt = self.parse_for_loop(node::VariableKind::Var)?;
            return Ok(node::Statement::For(stmt));
        }

        if self.at_keyword(Keyword::Var) {
            let kind = node::VariableKind::Var;
            let _ = self.next_item()?;
            let prev_in = self.context.allow_in;
            self.context.allow_in = false;
            let mut bindings = self.parse_variable_decl_list(true)?;
            self.context.allow_in = prev_in;
            if bindings.len() == 1 {
                let decl = if let Some(d) = bindings.pop() {
                    d
                } else {
                    return self.error(&self.look_ahead, &["variable decl"]);
                };
                if self.at_keyword(Keyword::In) {
                    let left = node::LoopLeft::Variable(decl);
                    let stmt = self.parse_for_in_loop(left)?;
                    return Ok(node::Statement::ForIn(stmt));
                } else if self.at_contextual_keyword("of") {
                    let left = node::LoopLeft::Variable(decl);
                    let stmt = self.parse_for_of_loop(left, is_await)?;
                    return Ok(node::Statement::ForOf(stmt));
                } else {
                    let init = node::LoopInit::Variable(vec![decl]);
                    let stmt = self.parse_for_loop_cont(Some(init))?;
                    return Ok(node::Statement::For(stmt));
                }
            } else {
                let init = node::LoopInit::Variable(bindings);
                let stmt = self.parse_for_loop_cont(Some(init))?;
                return Ok(node::Statement::For(stmt));
            }
        } else if self.at_keyword(Keyword::Const) || self.at_keyword(Keyword::Let) {
            let kind = self.next_item()?;
            let kind = match &kind.token {
                Token::Keyword(ref k) => match k {
                    Keyword::Const => node::VariableKind::Const,
                    Keyword::Let => node::VariableKind::Let,
                    _ => unreachable!(),
                },
                _ => return self.error(&kind, &["const", "let"]),
            };
            if !self.context.strict && self.look_ahead.token.matches_keyword(Keyword::In) {
                let _in = self.next_item()?;
                //const or let becomes an ident
                let left = node::LoopLeft::Variable(node::VariableDecl {
                    id: node::Pattern::Identifier(kind.to_string()),
                    init: None,
                });
                let right = self.parse_expression()?;
                Ok(node::Statement::ForIn(node::ForInStatement {
                    left,
                    right,
                    body: Box::new(self.parse_loop_body()?),
                }))
            } else {
                let prev_in = self.context.allow_in;
                self.context.allow_in = false;
                let mut decls = self.parse_binding_list(&kind, true)?;
                if decls.len() == 1 {
                    let decl = if let Some(d) = decls.pop() {
                        d
                    } else {
                        return self.error(&self.look_ahead, &["variable decl"]);
                    };
                    if decl.init.is_none() && self.at_keyword(Keyword::In) {
                        let left = node::LoopLeft::Variable(decl);
                        let _in = self.next_item()?;
                        let right = self.parse_expression()?;
                        return Ok(node::Statement::ForIn(node::ForInStatement {
                            left,
                            right,
                            body: Box::new(self.parse_loop_body()?),
                        }));
                    } else if decl.init.is_none() && self.at_contextual_keyword("of") {
                        let left = node::LoopLeft::Variable(decl);
                        return Ok(
                            node::Statement::ForOf(
                                self.parse_for_of_loop(left, is_await)?
                            )
                        );
                    } else {
                        let init = node::LoopInit::Variable(vec![decl]);
                        let stmt = self.parse_for_loop_cont(Some(init))?;
                        return Ok(node::Statement::For(stmt));
                    }
                } else {
                    let init = node::LoopInit::Variable(decls);
                    let stmt = self.parse_for_loop_cont(Some(init))?;
                    return Ok(node::Statement::For(stmt));
                }
            }
        } else {
            let start = self.look_ahead.clone();
            let prev_in = self.context.allow_in;
            self.context.allow_in = false;
            let init = self.inherit_cover_grammar(&Self::parse_assignment_expr)?;
            self.context.allow_in = prev_in;
            if self.at_keyword(Keyword::In) {
                let _ = self.next_item()?;
                let pat = self.reinterpret_expr_as_pat(init)?;
                let left = node::LoopLeft::Pattern(pat);
                let right = self.parse_expression()?;
                return Ok(node::Statement::ForIn(node::ForInStatement {
                    left,
                    right,
                    body: Box::new(self.parse_loop_body()?),
                }));
            } else if self.at_contextual_keyword("of") {
                let _ = self.next_item()?;
                let p = self.reinterpret_expr_as_pat(init)?;
                let left = node::LoopLeft::Pattern(p);
                let right = self.parse_assignment_expr()?;
                return Ok(
                    node::Statement::ForOf(
                        self.parse_for_of_loop(left, is_await)?
                    )
                );
            } else {
                let init = if self.at_punct(Punct::Comma) {
                    let mut seq = vec![init];
                    while self.at_punct(Punct::Comma) {
                        let _comma = self.next_item()?;
                        seq.push(self.inherit_cover_grammar(&Self::parse_assignment_expr)?)
                    }
                    node::LoopInit::Expr(node::Expression::Sequence(seq))
                } else {
                    node::LoopInit::Expr(init)
                };
                return Ok(node::Statement::For(self.parse_for_loop_cont(Some(init))?));
            }
        }
    }

    fn parse_for_loop(&mut self, kind: node::VariableKind) -> Res<node::ForStatement> {
        debug!(target: "resp:debug", "parse_for_loop");
        let init = if self.at_punct(Punct::SemiColon) {
            None
        } else {
            let list = self.parse_variable_decl_list(true)?;
            Some(node::LoopInit::Variable(list))
        };
        self.parse_for_loop_cont(init)
    }

    fn parse_for_loop_cont(&mut self, init: Option<node::LoopInit>) -> Res<node::ForStatement> {
        debug!(target: "resp:debug", "parse_for_loop_cont");
        self.expect_punct(Punct::SemiColon)?;
        let test = if self.at_punct(Punct::SemiColon) {
            None
        } else {
            Some(self.parse_expression()?)
        };
        self.expect_punct(Punct::SemiColon)?;
        let update = if self.at_punct(Punct::CloseParen) {
            None
        } else {
            Some(self.parse_expression()?)
        };
        let body = self.parse_loop_body()?;
        Ok(node::ForStatement {
            init,
            test,
            update,
            body: Box::new(body),
        })
    }

    fn parse_for_in_loop(&mut self, left: node::LoopLeft) -> Res<node::ForInStatement> {
        debug!(target: "resp:debug", "parse_for_in_loop");
        let _ = self.next_item()?;
        let right = self.parse_expression()?;
        let body = self.parse_loop_body()?;
        Ok(node::ForInStatement {
            left,
            right,
            body: Box::new(body),
        })
    }

    fn parse_for_of_loop(&mut self, left: node::LoopLeft, is_await: bool) -> Res<node::ForOfStatement> {
        debug!(target: "resp:debug", "parse_for_of_loop");
        let _ = self.next_item()?;
        let right = self.parse_assignment_expr()?;
        let body = self.parse_loop_body()?;
        Ok(
            node::ForOfStatement {
                left,
                right,
                body: Box::new(body),
                is_await,
            }
        )
    }

    fn parse_loop_body(&mut self) -> Res<node::Statement> {
        debug!(target: "resp:debug", "parse_loop_body");
        self.expect_punct(Punct::CloseParen)?;
        let prev_iter = self.context.in_iteration;
        self.context.in_iteration = true;
        let ret = self.isolate_cover_grammar(&Self::parse_statement)?;
        self.context.in_iteration = prev_iter;
        Ok(ret)
    }

    fn parse_do_while_stmt(&mut self) -> Res<node::DoWhileStatement> {
        debug!(target: "resp:debug", "parse_do_while_stmt");
        self.expect_keyword(Keyword::Do)?;
        let prev_iter = self.context.in_iteration;
        self.context.in_iteration = true;
        let body = self.parse_statement()?;
        self.context.in_iteration = prev_iter;
        self.expect_keyword(Keyword::While)?;
        self.expect_punct(Punct::OpenParen)?;
        let test = self.parse_expression()?;
        self.expect_punct(Punct::CloseParen)?;
        Ok(node::DoWhileStatement {
            test,
            body: Box::new(body),
        })
    }

    fn parse_break_stmt(&mut self) -> Res<Option<node::Identifier>> {
        debug!(target: "resp:debug", "parse_break_stmt");
        self.parse_optionally_labeled_statement(Keyword::Break)
    }

    fn parse_continue_stmt(&mut self) -> Res<Option<node::Identifier>> {
        debug!(target: "resp:debug", "parse_continue_stmt");
        self.parse_optionally_labeled_statement(Keyword::Continue)
    }

    fn parse_optionally_labeled_statement(&mut self, k: Keyword) -> Res<Option<node::Identifier>> {
        debug!(target: "resp:debug", "parse_optionally_labeled_statement");
        self.expect_keyword(k)?;
        let ret = if self.look_ahead.token.is_ident() && !self.context.has_line_term {
            let id = self.parse_var_ident(false)?;
            if !self.context.label_set.contains(&id) {
                //error: unknown label
            }
            Some(id)
        } else {
            None
        };
        self.consume_semicolon()?;
        if ret.is_some() && !self.context.in_iteration && !self.context.in_switch {
            //error: invalid break
        }
        Ok(ret)
    }

    fn parse_debugger_stmt(&mut self) -> Res<node::Statement> {
        debug!(target: "resp:debug", "parse_debugger_stmt");
        self.expect_keyword(Keyword::Debugger)?;
        self.consume_semicolon()?;
        Ok(node::Statement::Debugger)
    }

    fn parse_labelled_statement(&mut self) -> Res<node::Statement> {
        debug!(target: "resp:debug", "parse_labelled_statement");
        let ret = self.parse_expression()?;
        if ret.is_ident() && self.at_punct(Punct::Colon) {
            let _colon = self.next_item()?;
            let id = if let node::Expression::Ident(ref ident) = ret {
                ident.clone()
            } else {
                return Err(self.reinterpret_error("expression", "ident"))
            };
            if !self.context.label_set.insert(format!("${}", id)) {
                //error, multiple ids in this scope.
            }
            let body = if self.at_keyword(Keyword::Class) {
                let body = self.parse_class_body()?;
                let cls = node::Class {
                    id: None,
                    super_class: None,
                    body,
                };
                let expr = node::Expression::Class(cls);
                node::Statement::Expr(expr)
            } else if self.at_keyword(Keyword::Function) {
                let f = self.parse_function_decl(true)?;
                let expr = node::Expression::Function(f);
                node::Statement::Expr(expr)
            } else {
                self.parse_statement()?
            };
            Ok(node::Statement::Labeled(node::LabeledStatement {
                label: id,
                body: Box::new(body),
            }))
        } else {
            self.consume_semicolon()?;
            Ok(node::Statement::Expr(ret))
        }
    }

    fn parse_expression_statement(&mut self) -> Res<node::Expression> {
        debug!(target: "resp:debug", "parse_expression_statement");
        let ret = self.parse_expression()?;
        self.consume_semicolon()?;
        Ok(ret)
    }

    fn parse_block(&mut self) -> Res<node::BlockStatement> {
        debug!(target: "resp:debug", "parse_block");
        self.expect_punct(Punct::OpenBrace)?;
        let mut ret = vec![];
        loop {
            if self.at_punct(Punct::CloseBrace) {
                break;
            }
            let part = self.parse_statement_list_item()?;
            if part.is_export() {
                //error
            }
            if part.is_import() {
                //error
            }
            ret.push(part);
        }
        self.expect_punct(Punct::CloseBrace)?;
        Ok(ret)
    }

    fn parse_lexical_decl(&mut self, in_for: bool) -> Res<node::Declaration> {
        debug!(target: "resp:debug", "parse_lexical_decl");
        let next = self.next_item()?;
        debug!(target: "resp:debug", "next: {:?}", next);
        let kind = match &next.token {
            &Token::Keyword(ref k) => match k {
                Keyword::Let => node::VariableKind::Let,
                Keyword::Const => node::VariableKind::Const,
                _ => return self.error(&next, &["let", "const"]),
            },
            _ => return self.error(&next, &["let", "const"]),
        };
        let decl = self.parse_binding_list(&kind, in_for)?;
        self.consume_semicolon()?;
        Ok(node::Declaration::Variable(kind, decl))
    }

    fn parse_binding_list(
        &mut self,
        kind: &node::VariableKind,
        in_for: bool,
    ) -> Res<Vec<node::VariableDecl>> {
        debug!(target: "resp:debug", "parse_binding_list");
        let mut ret = vec![self.parse_lexical_binding(kind, in_for)?];
        while self.at_punct(Punct::Comma) {
            let _comma = self.next_item()?;
            ret.push(self.parse_lexical_binding(kind, in_for)?)
        }
        Ok(ret)
    }

    fn parse_variable_decl_list(&mut self, in_for: bool) -> Res<Vec<node::VariableDecl>> {
        let mut ret = vec![self.parse_variable_decl(in_for)?];
        while self.at_punct(Punct::Comma) {
            let _ = self.next_item()?;
            ret.push(self.parse_variable_decl(in_for)?);
        }
        Ok(ret)
    }

    fn parse_variable_decl(&mut self, in_for: bool) -> Res<node::VariableDecl> {
        let (_, id) = self.parse_pattern(Some(node::VariableKind::Var), &mut vec![])?;
        if self.context.strict && id.is_restricted() {
            //tolerate error
        }
        let init = if self.at_punct(Punct::Assign) {
            let _ = self.next_item()?;
            Some(self.isolate_cover_grammar(&Self::parse_assignment_expr)?)
        } else if id.is_ident() && !in_for {
            self.expect_punct(Punct::Assign)?;
            None
        } else {
            None
        };
        Ok(node::VariableDecl {
            id,
            init,
        })
    }

    fn parse_lexical_binding(
        &mut self,
        kind: &node::VariableKind,
        in_for: bool,
    ) -> Res<node::VariableDecl> {
        debug!(target: "resp:debug", "parse_lexical_binding");
        let is_var = match kind {
            node::VariableKind::Const | node::VariableKind::Let => false,
            _ => true,
        };
        let id = self.parse_var_ident(is_var)?;
        if self.context.strict && Self::is_restricted_word(&id) {
            return self.error(&self.look_ahead, &["not eval", "not arguments"]);
        }
        let init = if kind == &node::VariableKind::Const {
            if !self.at_keyword(Keyword::In) && !self.at_contextual_keyword("of") {
                if self.at_punct(Punct::Assign) {
                    let _ = self.next_item()?;
                    Some(self.isolate_cover_grammar(&Self::parse_assignment_expr)?)
                } else {
                    return self.error(&self.look_ahead, &["="]);
                }
            } else {
                None
            }
        } else if in_for || self.at_punct(Punct::Assign) {
            self.expect_punct(Punct::Assign)?;
            Some(self.isolate_cover_grammar(&Self::parse_assignment_expr)?)
        } else {
            None
        };
        Ok(node::VariableDecl {
            id: node::Pattern::Identifier(id),
            init,
        })
    }

    fn parse_function_decl(&mut self, opt_ident: bool) -> Res<node::Function> {
        debug!(target: "resp:debug", "parse_function_decl");
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
                return self.error(&start, &[]);
            }
            let mut first_restricted = if !self.context.strict {
                if start.token.is_restricted() {
                    Some(start)
                } else if start.token.is_strict_reserved() {
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
        let prev_await = self.context.await;
        let prev_yield = self.context.allow_yield;
        self.context.await = is_async;
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
                return self.error(item, &[]);
            }
        }
        if self.context.strict && strict {
            return self.error(&self.look_ahead, &[]);
        }
        self.context.strict = prev_strict;
        self.context.allow_strict_directive = prev_allow_strict;
        self.context.await = prev_await;
        self.context.allow_yield = prev_yield;
        Ok(node::Function {
            id,
            params,
            body,
            generator: is_gen,
            is_async,
        })
    }

    fn parse_function_source_el(&mut self) -> Res<node::FunctionBody> {
        debug!(target: "resp:debug", "parse_function_source_el");
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
        Ok(body)
    }

    fn parse_class_decl(&mut self, opt_ident: bool) -> Res<node::Class> {
        debug!(target: "resp:debug", "parse_class_decl");
        let prev_strict = self.context.strict;
        self.context.strict = true;
        self.expect_keyword(Keyword::Class)?;
        let id = if opt_ident && !self.look_ahead.token.is_ident() {
            None
        } else {
            Some(self.parse_var_ident(false)?)
        };
        let super_class = if self.at_contextual_keyword("extends") {
            let _ = self.next_item()?;
            Some(Box::new(self.isolate_cover_grammar(
                &Self::parse_left_hand_side_expr_allow_call,
            )?))
        } else {
            None
        };
        let body = self.parse_class_body()?;

        self.context.strict = prev_strict;
        Ok(node::Class {
            id,
            super_class,
            body,
        })
    }

    fn parse_class_body(&mut self) -> Res<Vec<node::Property>> {
        debug!(target: "resp:debug", "parse_class_body");
        let mut ret = vec![];
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
        Ok(ret)
    }

    fn parse_class_el(&mut self, has_ctor: bool) -> Res<(bool, node::Property)> {
        debug!(target: "resp:debug", "parse_class_el");
        let mut has_ctor = has_ctor;
        let mut key: Option<node::PropertyKey> = None;
        let mut value: Option<node::PropertyValue> = None;
        let mut computed = false;
        let mut is_static = false;
        let is_async = false;
        if self.at_punct(Punct::Asterisk) {
            let _ = self.next_item()?;
        } else {
            computed = self.at_punct(Punct::OpenBracket);
            let new_key = self.parse_object_property_key()?;
            if new_key.is_static()
                && (self.at_punct(Punct::Asterisk)
                    || Self::qualified_prop_name(&self.look_ahead.token))
            {
                is_static = true;
                computed = self.at_punct(Punct::OpenBracket);
                if self.at_punct(Punct::Asterisk) {
                    key = Some(new_key);
                    let _ = self.next_item()?;
                } else {
                    key = Some(self.parse_object_property_key()?);
                }
            } else {
                key = Some(new_key);
            }
            if self.look_ahead.token.is_ident()
                && !self.context.has_line_term
                && self.at_contextual_keyword("async")
            {
                if !self.look_ahead.token.matches_punct(Punct::Colon)
                    && !self.look_ahead.token.matches_punct(Punct::OpenParen)
                    && !self.look_ahead.token.matches_punct(Punct::Asterisk)
                {
                    return self.error(&self.look_ahead, &[":", "(", "*"]);
                }
            }
        }

        let mut kind: Option<node::PropertyKind> = None;
        let mut method = false;

        let look_ahead_prop_key = self.at_possible_ident();
        if self.look_ahead.token.is_ident() {
            let (at_get, at_set) = if let Some(ref k) = key {
                (k.matches("get") && look_ahead_prop_key, k.matches("set") && look_ahead_prop_key)
            } else {
                (false, false)
            };

            if at_get {
                    kind = Some(node::PropertyKind::Get);
                    computed = self.at_punct(Punct::OpenBracket);
                    self.context.allow_yield = false;
                    key = Some(self.parse_object_property_key()?);
                    value = Some(self.parse_getter_method()?);
            } else if at_set {
                    kind = Some(node::PropertyKind::Set);
                    computed = self.at_punct(Punct::OpenBracket);
                    key = Some(self.parse_object_property_key()?);
                    value = Some(self.parse_setter_method()?);
            }
        } else if self.look_ahead.token.matches_punct(Punct::Asterisk) && look_ahead_prop_key {
            kind = Some(node::PropertyKind::Init);
            computed = self.at_punct(Punct::OpenBracket);
            key = Some(self.parse_object_property_key()?);
            value = Some(self.parse_generator_method()?);
            method = true;
        }

        if kind.is_none() && key.is_some() && self.at_punct(Punct::OpenParen) {
            kind = Some(node::PropertyKind::Init);
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
            return self.error(&self.look_ahead, &[]);
        };

        if kind == node::PropertyKind::Init {
            kind = node::PropertyKind::Method;
        }

        let key = if let Some(k) = key {
            k
        } else {
            return self.error(&self.look_ahead, &[]);
        };
        if !computed {
            if is_static && key.matches("prototype") {
                return self.error(&self.look_ahead, &[]);
            }
            if !is_static && key.matches("constructor") {
                println!("kind: {:?}, method: {}", kind, method);
                if kind != node::PropertyKind::Method || !method {
                    return self.error(&self.look_ahead, &["[constructor declaration]"]);
                }
                if let Some(ref v) = value {
                    if v.is_generator() {
                        return self.error(&self.look_ahead, &["[non-generator function declaration]"]);
                    }
                }
                if has_ctor {
                    return self.error(&self.look_ahead, &[]);
                } else {
                    has_ctor = true;
                }
                kind = node::PropertyKind::Ctor;
            }
        }

        let value = if let Some(v) = value {
            v
        } else {
            return self.error(&self.look_ahead, &[]);
        };

        Ok((
            has_ctor,
            node::Property {
                key,
                value,
                kind,
                method,
                computed,
                short_hand: false,
            },
        ))
    }

    fn parse_async_property_method(&mut self) -> Res<node::PropertyValue> {
        debug!(target: "resp:debug", "parse_property_method_async_fn");
        let prev_yield = self.context.allow_yield;
        let prev_await = self.context.await;
        self.context.allow_yield = false;
        self.context.await = true;
        let params = self.parse_formal_params()?;
        let body = self.parse_property_method_body(params.simple, params.found_restricted)?;
        self.context.allow_yield = prev_yield;
        self.context.await = prev_await;
        let func = node::Function {
            id: None,
            params: params.params,
            is_async: true,
            generator: false,
            body,
        };
        Ok(node::PropertyValue::Expr(node::Expression::Function(func)))
    }



    fn parse_property_method(&mut self) -> Res<node::PropertyValue> {
        debug!(target: "resp:debug", "parse_property_method");
        let prev_yield = self.context.allow_yield;
        self.context.allow_yield = false;
        let params = self.parse_formal_params()?;
        let body = self.parse_property_method_body(params.simple, params.found_restricted)?;
        self.context.allow_yield = prev_yield;
        let func = node::Function {
            id: None,
            params: params.params,
            is_async: false,
            generator: false,
            body,
        };
        Ok(node::PropertyValue::Expr(node::Expression::Function(func)))
    }

    fn parse_generator_method(&mut self) -> Res<node::PropertyValue> {
        debug!(target: "resp:debug", "pares_generator_method");
        let prev_yield = self.context.allow_yield;
        self.context.allow_yield = true;
        let params = self.parse_formal_params()?;
        self.context.allow_yield = false;
        let body = self.parse_method_body(params.simple, params.found_restricted)?;
        self.context.allow_yield = prev_yield;
        let func = node::Function {
            id: None,
            params: params.params,
            is_async: false,
            generator: true,
            body,
        };
        Ok(node::PropertyValue::Expr(node::Expression::Function(func)))
    }

    fn parse_getter_method(&mut self) -> Res<node::PropertyValue> {
        debug!(target: "resp:debug", "parse_getter_method");
        let mut is_gen = false;
        let prev_yield = self.context.allow_yield;
        let formal_params = self.parse_formal_params()?;
        if formal_params.params.len() > 0 {
            //tolerate error
        }
        let body = self.parse_method_body(formal_params.simple, formal_params.found_restricted)?;
        self.context.allow_yield = prev_yield;
        Ok(
            node::PropertyValue::Expr(
                node::Expression::Function(
                    node::Function {
                        id: None,
                        params: formal_params.params,
                        body,
                        generator: is_gen,
                        is_async: false,
                    }
                )
            )
        )
    }

    fn parse_method_body(&mut self, simple: bool, found_restricted: bool) -> Res<Vec<node::ProgramPart>> {
        debug!(target: "resp:debug", "parse_method_body");
        self.context.is_assignment_target = false;
        self.context.is_binding_element = false;
        let prev_strict = self.context.strict;
        let prev_allow_strict = self.context.allow_strict_directive;
        self.context.allow_strict_directive = simple;
        let body = self.isolate_cover_grammar(&Self::parse_function_source_el)?;
        if self.context.strict && found_restricted {
            //tolerate error
        }
        self.context.strict = prev_strict;
        self.context.allow_strict_directive = prev_allow_strict;
        Ok(body)
    }

    fn parse_setter_method(&mut self) -> Res<node::PropertyValue> {
        debug!(target: "resp:debug", "parse_setter_method");
        let prev_allow = self.context.allow_yield;
        self.context.allow_yield = true;
        let params = self.parse_formal_params()?;
        if params.params.len() != 1 {
            //tolerate error
        } else if let Some(ref param) = params.params.get(0) {
            if param.is_rest() {
                //tolerate error
            }
        }
        let body = self.parse_property_method_body(params.simple, params.found_restricted)?;
        let func = node::Function {
            id: None,
            params: params.params,
            body,
            generator: false,
            is_async: false,
        };
        Ok(node::PropertyValue::Expr(node::Expression::Function(func)))
    }

    fn parse_property_method_body(&mut self, simple: bool, found_restricted: bool) -> Res<node::FunctionBody> {
        debug!(target: "resp:debug", "parse_property_method_fn");
        self.context.is_assignment_target = false;
        self.context.is_binding_element = false;
        let prev_strict = self.context.strict;
        let prev_allow = self.context.allow_strict_directive;
        self.context.allow_strict_directive = simple;
        let ret = self.inherit_cover_grammar(&Self::parse_function_source_el)?;
        if self.context.strict && found_restricted {
            //tolerate error
        }
        self.context.strict = prev_strict;
        self.context.allow_strict_directive = prev_allow;
        Ok(ret)
    }

    fn qualified_prop_name(tok: &Token) -> bool {
        debug!(target: "resp:debug", "qualified_prop_name");
        if tok.is_keyword() || tok.matches_punct(Punct::OpenBracket) {
            true
        } else {
            false
        }
    }

    fn parse_object_property_key(&mut self) -> Res<node::PropertyKey> {
        debug!(target: "resp:debug", "parse_object_property_key");
        let item = self.next_item()?;
        if item.token.is_string() || item.token.is_numeric() {
            if item.token.is_oct_literal() {
                //tolerate error
            }
            let id = node::Literal::from_token(&item.token).ok_or(self.reinterpret_error("number or string", "literal"))?;
            Ok(node::PropertyKey::Literal(id))
        } else if item.token.is_ident()
        || item.token.is_boolean()
        || item.token.is_null()
        || item.token.is_keyword() {
            let id = item.token.to_string();
            Ok(node::PropertyKey::Ident(id))
        } else if item.token.matches_punct(Punct::OpenBracket) {
            let key = self.isolate_cover_grammar(&Self::parse_assignment_expr)?;
            let id = self.reinterpret_expr_as_pat(key)?;
            self.expect_punct(Punct::CloseBracket)?;
            Ok(node::PropertyKey::Pattern(id))
        } else {
            self.error(&item, &["[string]", "[number]", "[ident]", "[boolean]", "null", "[keyword]", "["])
        }
    }

    fn parse_primary_expression(&mut self) -> Res<node::Expression> {
        debug!(target: "resp:debug", "parse_primary_expression");
        if self.look_ahead.token.is_ident() {
            if (self.context.is_module || self.context.await) && self.at_keyword(Keyword::Await) {
                // tolerate unexpected token
            }
            if self.at_async_function() {
                self.parse_function_expr()
            } else {
                let ident = self.next_item()?;
                Ok(node::Expression::Ident(ident.token.to_string()))
            }
        } else if self.look_ahead.token.is_numeric() || self.look_ahead.token.is_string() {
            if self.context.strict && self.look_ahead.token.is_oct_literal() {
                //tolerate unexpected token
            }
            self.context.is_assignment_target = false;
            self.context.is_binding_element = false;
            let item = self.next_item()?;
            let lit = match item.token {
                Token::Numeric(num) => node::Literal::Number(num.to_string()),
                Token::String(s) => node::Literal::String(s.to_string()),
                _ => unreachable!(),
            };
            Ok(node::Expression::Literal(lit))
        } else if self.look_ahead.token.is_boolean() {
            self.context.is_assignment_target = false;
            self.context.is_binding_element = false;
            let item = self.next_item()?;
            let lit = match item.token {
                Token::Boolean(b) => node::Literal::Boolean(b.into()),
                _ => unreachable!(),
            };
            Ok(node::Expression::Literal(lit))
        } else if self.look_ahead.token.is_template() {
            let lit = self.parse_template_literal()?;
            Ok(node::Expression::Literal(node::Literal::Template(lit)))
        } else if self.look_ahead.token.is_punct() {
            if self.at_punct(Punct::OpenParen) {
                Ok(self.inherit_cover_grammar(&Self::parse_group_expr)?)
            } else if self.at_punct(Punct::OpenBracket) {
                Ok(self.inherit_cover_grammar(&Self::parse_array_init)?)
            } else if self.at_punct(Punct::OpenBrace) {
                Ok(self.inherit_cover_grammar(&Self::parse_obj_init)?)
            } else {
                self.error(&self.look_ahead, &["{", "[", "("])
            }
        } else if self.look_ahead.token.is_regex() {
            self.context.is_assignment_target = false;
            self.context.is_binding_element = false;
            let regex = self.next_item()?;
            let lit = match regex.token {
                Token::RegEx(ex) => node::RegEx {
                    pattern: ex.body,
                    flags: ex.flags.unwrap_or(String::new()),
                },
                _ => unreachable!(),
            };
            Ok(node::Expression::Literal(node::Literal::RegEx(lit)))
        } else if self.look_ahead.token.is_keyword() {
            if !self.context.strict
                && ((self.context.allow_yield && self.at_keyword(Keyword::Yield))
                    || self.at_keyword(Keyword::Let))
            {
                let ident = self.parse_ident_name()?;
                Ok(node::Expression::Ident(ident))
            } else {
                self.context.is_assignment_target = false;
                self.context.is_binding_element = false;
                if self.at_keyword(Keyword::Function) {
                    self.parse_function_expr()
                } else if self.at_keyword(Keyword::This) {
                    let _ = self.next_item()?;
                    Ok(node::Expression::ThisExpression)
                } else if self.at_keyword(Keyword::Class) {
                    let cls = self.parse_class_decl(true)?;
                    Ok(node::Expression::Class(cls))
                } else if self.at_import_call() {
                    // TODO: Double check this
                    let ident = self.parse_ident_name()?;
                    Ok(node::Expression::Ident(ident))
                } else {
                    self.error(&self.look_ahead, &["function", "this", "class", "import"])
                }
            }
        } else {
            self.error(
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

    fn at_import_call(&mut self) -> bool {
        debug!(target: "resp:debug", "at_import_call");
        if self.at_keyword(Keyword::Import) {
            let state = self.scanner.get_state();
            self.scanner.skip_comments();
            let ret = if let Some(next) = self.scanner.next() {
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

    fn parse_group_expr(&mut self) -> Res<node::Expression> {
        debug!(target: "resp:debug", "parse_group_expr");
        self.expect_punct(Punct::OpenParen)?;
        if self.at_punct(Punct::CloseParen) {
            let _ = self.next_item()?;
            if !self.at_punct(Punct::FatArrow) {
                self.expect_punct(Punct::FatArrow)?;
            }
            Ok(node::Expression::ArrowParamPlaceHolder(vec![], false))
        } else {
            let mut params = vec![];
            if self.at_punct(Punct::Spread) {
                let (_, expr) = self.parse_rest_element(&mut params)?;
                let arg = node::FunctionArg::Pattern(expr);
                self.expect_punct(Punct::CloseParen)?;
                if !self.at_punct(Punct::FatArrow) {
                    self.expect_punct(Punct::FatArrow)?;
                }
                Ok(node::Expression::ArrowParamPlaceHolder(vec![arg], false))
            } else {
                self.context.is_binding_element = true;
                let mut ex = self.inherit_cover_grammar(&Self::parse_assignment_expr)?;
                if self.at_punct(Punct::Comma) {
                    let mut exprs = vec![ex];
                    while !self.look_ahead.token.is_eof() {
                        if !self.at_punct(Punct::Comma) {
                            break;
                        }
                        let _ = self.next_item()?;
                        if self.at_punct(Punct::CloseParen) {
                            let _ = self.next_item()?;
                            return Ok(node::Expression::ArrowParamPlaceHolder(
                                exprs
                                    .into_iter()
                                    .map(|e| node::FunctionArg::Expr(e))
                                    .collect(),
                                false,
                            ));
                        } else if self.at_punct(Punct::Spread) {
                            if !self.context.is_binding_element {
                                return self.error(&self.look_ahead, &["not ..."]);
                            }
                            let (_, rest) = self.parse_rest_element(&mut params)?;
                            let mut args: Vec<node::FunctionArg> = exprs
                                .into_iter()
                                .map(|e| node::FunctionArg::Expr(e))
                                .collect();
                            args.push(node::FunctionArg::Pattern(rest));
                            self.expect_punct(Punct::CloseParen)?;
                            return Ok(node::Expression::ArrowParamPlaceHolder(args, false));
                        } else {
                            exprs.push(self.inherit_cover_grammar(&Self::parse_assignment_expr)?);
                        }
                    }
                    ex = node::Expression::Sequence(exprs);
                }
                self.expect_punct(Punct::CloseParen)?;
                if self.at_punct(Punct::FatArrow) {
                    if ex.is_ident() {
                        self.context.is_binding_element = false;
                        return Ok(node::Expression::ArrowParamPlaceHolder(
                            vec![node::FunctionArg::Expr(ex)],
                            false,
                        ));
                    }
                    if !self.context.is_binding_element {
                        return self.error(&self.look_ahead, &["binding element"]);
                    }
                    if let node::Expression::Sequence(seq) = ex {
                        let args = seq
                            .into_iter()
                            .map(|e| node::FunctionArg::Expr(e))
                            .collect();
                        return Ok(node::Expression::ArrowParamPlaceHolder(args, false));
                    }
                }
                Ok(ex)
            }
        }
    }

    fn parse_array_init(&mut self) -> Res<node::Expression> {
        debug!(target: "resp:debug", "parse_array_init");
        self.expect_punct(Punct::OpenBracket)?;
        let mut elements = vec![];
        while !self.at_punct(Punct::CloseBracket) {
            if self.at_punct(Punct::Comma) {
                self.next_item()?;
                elements.push(None);
            } else if self.at_punct(Punct::Spread) {
                let el = self.parse_spread_element()?;
                if !self.at_punct(Punct::CloseBracket) {
                    self.context.is_assignment_target = false;
                    self.context.is_binding_element = false;
                    self.expect_punct(Punct::Comma)?;
                }
                elements.push(Some(el))
            } else {
                elements.push(Some(
                    self.inherit_cover_grammar(&Self::parse_assignment_expr)?,
                ));
                if !self.at_punct(Punct::CloseBracket) {
                    self.expect_punct(Punct::Comma)?;
                }
            }
        }
        self.expect_punct(Punct::CloseBracket);
        Ok(node::Expression::Array(elements))
    }
    fn parse_obj_init(&mut self) -> Res<node::Expression> {
        debug!(target: "resp:debug", "parse_obj_init");
        self.expect_punct(Punct::OpenBrace)?;
        let mut props = vec![];
        let mut has_proto = false;
        while !self.at_punct(Punct::CloseBrace) {
            let prop = if self.at_punct(Punct::Spread) {
                let spread = self.parse_spread_element()?;
                node::ObjectProperty::Spread(Box::new(spread))
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
        Ok(node::Expression::Object(props))
    }

    fn parse_obj_prop(&mut self, has_proto: bool) -> Res<(bool, node::ObjectProperty)> {
        debug!(target: "resp:debug", "parse_obj_prop");
        let start = self.look_ahead.clone();
        let mut has_proto = has_proto;
        let (key, is_async, computed) = if let Token::Ident(ref id) = start.token {
            let id = id.clone();
            let _ = self.next_item()?;
            let computed = self.at_punct(Punct::OpenBracket);
            let is_async = self.context.has_line_term
                && id == "async"
                && !self.at_punct(Punct::Colon)
                && !self.at_punct(Punct::Asterisk)
                && !self.at_punct(Punct::Comma);
            let key = if is_async {
                self.parse_object_property_key()?
            } else {
                node::PropertyKey::Ident(id.to_string())
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
        let prop = if start.token.matches_ident_str("get") && at_qualified && !is_async {
            node::ObjectProperty::Property(node::Property {
                computed: self.at_punct(Punct::OpenBracket),
                key: key.unwrap_or(self.parse_object_property_key()?),
                value: self.parse_getter_method()?,
                kind: node::PropertyKind::Get,
                method: false,
                short_hand: false,
            })
        } else if start.token.matches_ident_str("set") && at_qualified && !is_async {
            node::ObjectProperty::Property(node::Property {
                computed: self.at_punct(Punct::OpenBracket),
                key: self.parse_object_property_key()?,
                value: self.parse_setter_method()?,
                kind: node::PropertyKind::Set,
                method: false,
                short_hand: false,
            })
        } else if start.token.matches_punct(Punct::Asterisk) && at_qualified {
            node::ObjectProperty::Property(node::Property {
                computed: self.at_punct(Punct::OpenBracket),
                key: self.parse_object_property_key()?,
                value: self.parse_generator_method()?,
                kind: node::PropertyKind::Init,
                method: true,
                short_hand: false,
            })
        } else {
            if let Some(key) = key {
                let kind = node::PropertyKind::Init;
                if self.at_punct(Punct::Colon) && !is_async {
                    if !computed && key.matches("__proto__") {
                        if has_proto {
                            //tolerate error
                        }
                        has_proto = true;
                    }
                    let _ = self.next_item()?;
                    let value = self.inherit_cover_grammar(&Self::parse_assignment_expr)?;
                    node::ObjectProperty::Property(node::Property {
                        computed,
                        key,
                        value: node::PropertyValue::Expr(value),
                        kind,
                        method: false,
                        short_hand: false,
                    })
                } else if self.at_punct(Punct::OpenParen) {
                    node::ObjectProperty::Property(node::Property {
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
                    })
                } else if start.token.is_ident() {
                    if self.at_punct(Punct::Assign) {
                        self.context.first_covert_initialized_name_error =
                            Some(self.look_ahead.clone());
                        let _ = self.next_item()?;
                        let inner = self.inherit_cover_grammar(&Self::parse_assignment_expr)?;
                        node::ObjectProperty::Property(node::Property {
                            computed,
                            key,
                            value: node::PropertyValue::Expr(inner),
                            kind,
                            method: false,
                            short_hand: true,
                        })
                    } else {
                        node::ObjectProperty::Property(node::Property {
                            computed,
                            key,
                            value: node::PropertyValue::None,
                            kind,
                            method: false,
                            short_hand: true,
                        })
                    }
                } else {
                    return self.error(&start, &["object property value"]);
                }
            } else {
                return self.error(&start, &["object property key"]);
            }
        };
        Ok((has_proto, prop))
    }

    fn at_qualified_prop_key(&self) -> bool {
        match &self.look_ahead.token {
            Token::Ident(_)
            | Token::String(_)
            | Token::Boolean(_)
            | Token::Numeric(_)
            | Token::Null
            | Token::Keyword(_) => true,
            Token::Punct(ref p) => p == &Punct::OpenBracket,
            _ => false,
        }
    }

    fn parse_template_literal(&mut self) -> Res<node::TemplateLiteral> {
        debug!(target: "resp:debug", "parse_template_literal");
        if !self.look_ahead.token.is_template_head() {
            return self.error(&self.look_ahead, &["template head"]);
        }
        let mut expressions = vec![];
        let mut quasis = vec![];
        loop {
            quasis.push(self.parse_template_element()?);
            while !self.look_ahead.token.is_template() {
                expressions.push(self.parse_expression()?)
            }
            if self.look_ahead.token.is_template_tail() {
                quasis.push(self.parse_template_element()?);
                break;
            }
        }
        Ok(node::TemplateLiteral {
            expressions,
            quasis,
        })
    }

    fn parse_template_element(&mut self) -> Res<node::TemplateElement> {
        debug!(target: "resp:debug", "parse_template_element");
        if let Token::Template(t) = self.next_item()?.token {
            let (raw, cooked, tail) = match t {
                Template::Head(cooked) => (format!("`{}{{", cooked), cooked, false),
                Template::Middle(cooked) => (format!("}}{}{{", cooked), cooked, false),
                Template::Tail(cooked) => (format!("}}{}`", cooked), cooked, true),
                Template::NoSub(cooked) => (format!("`{}`", cooked), cooked, true),
            };
            Ok(node::TemplateElement { raw, cooked, tail })
        } else {
            self.error(&self.look_ahead, &["Template part"])
        }
    }

    fn parse_function_expr(&mut self) -> Res<node::Expression> {
        debug!(target: "resp:debug", "parse_function_expr");
        let is_async = self.at_contextual_keyword("async");
        if is_async {
            let _ = self.next_item()?;
        }
        self.expect_keyword(Keyword::Function)?;
        let is_gen = self.at_punct(Punct::Asterisk);
        if is_gen {
            let _ = self.next_item()?;
        }
        let prev_await = self.context.await;
        let prev_yield = self.context.allow_yield;
        self.context.await = is_async;
        self.context.allow_yield = is_gen;
        let mut found_restricted = false;
        let id = if !self.at_punct(Punct::OpenParen) {
            let item = self.look_ahead.clone();
            let id = self.parse_fn_name(is_gen)?;
            if item.token.is_restricted() {
                if self.context.strict {
                    //tolerate unexpected token
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
        found_restricted = formal_params.found_restricted;
        let prev_strict = self.context.strict;
        let prev_strict_dir = formal_params.simple;
        let body = self.parse_function_source_el()?;
        if self.context.strict && found_restricted {
            //throw error about the use of a restricted name
            //or maybe tolerate error? Not super clear
        }
        self.context.strict = prev_strict;
        self.context.allow_strict_directive = prev_strict_dir;
        self.context.allow_yield = prev_yield;
        self.context.await = prev_await;
        let func = node::Function {
            id,
            params: formal_params.params,
            body,
            generator: is_gen,
            is_async,
        };
        Ok(node::Expression::Function(func))
    }

    fn parse_fn_name(&mut self, is_gen: bool) -> Res<node::Identifier> {
        debug!(target: "resp:debug", "parse_fn_name");
        if self.context.strict && !is_gen && self.at_keyword(Keyword::Yield) {
            self.parse_ident_name()
        } else {
            self.parse_var_ident(false)
        }
    }

    fn parse_ident_name(&mut self) -> Res<node::Identifier> {
        debug!(target: "resp:debug", "parse_ident_name");
        let ident = self.next_item()?;
        match ident.token {
            Token::Ident(i) => Ok(i.to_string()),
            Token::Keyword(k) => Ok(k.to_string()),
            Token::Boolean(b) => Ok(b.into()),
            Token::Null => Ok("null".to_string()),
            _ => self.error(&ident, &["identifier name"]),
        }
    }

    fn parse_var_ident(&mut self, is_var: bool) -> Res<node::Identifier> {
        debug!(target: "resp:debug", "parse_var_ident");
        let ident = self.next_item()?;
        if ident.token.matches_keyword(Keyword::Yield) {
            if self.context.strict || !self.context.allow_yield {
                return self.error(&ident, &["variable identifier"]);
            }
        } else if !ident.token.is_ident() {
            if self.context.strict && ident.token.is_strict_reserved() {
                return self.error(&ident, &["variable identifier"]);
            }
            if self.context.strict || ident.token.matches_keyword(Keyword::Let) || !is_var {
                return self.error(&ident, &["variable identifier"]);
            }
        } else if (self.context.is_module || self.context.await)
            && ident.token.matches_ident_str("await")
        {
            return self.error(&ident, &["variable identifier"]);
        }
        match &ident.token {
            &Token::Ident(ref i) => Ok(i.to_string()),
            _ => self.error(&ident, &["variable identifier"]),
        }
    }

    fn parse_formal_params(&mut self) -> Res<FormalParams> {
        debug!(target: "resp:debug", "parse_formal_params");
        self.expect_punct(Punct::OpenParen)?;
        let mut args = vec![];
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

    fn parse_formal_param(&mut self, simple: bool) -> Res<(bool, bool, node::FunctionArg)> {
        debug!(target: "resp:debug", "parse_formal_param");
        let mut params: Vec<Item> = Vec::new();
        let (found_restricted, param) = if self.at_punct(Punct::Spread) {
            let (found_restricted, pat) = self.parse_rest_element(&mut params)?;
            (found_restricted, node::FunctionArg::Pattern(pat))
        } else {
            let (found_restricted, pat) = self.parse_pattern_with_default(&mut params)?;
            (found_restricted, node::FunctionArg::Pattern(pat))
        };
        let simple = simple && param.is_simple();
        Ok((simple, found_restricted, param))
    }

    fn parse_rest_element(&mut self, params: &mut Vec<Item>) -> Res<(bool, node::Pattern)> {
        debug!(target: "resp:debug", "parse_rest_element");
        self.expect_punct(Punct::Spread)?;
        let (restricted, arg) = self.parse_pattern(None, params)?;
        if self.at_punct(Punct::Assign) {
            return self.error(&self.look_ahead, &["not assignment"]);
        }
        if !self.at_punct(Punct::CloseParen) {
            return self.error(&self.look_ahead, &[")"]);
        }
        Ok((restricted, arg))
    }

    fn parse_pattern_with_default(&mut self, params: &mut Vec<Item>) -> Res<(bool, node::Pattern)> {
        debug!(target: "resp:debug", "parse_pattern_with_default");
        let (is_restricted, ret) = self.parse_pattern(None, params)?;
        if self.at_punct(Punct::Assign) {
            let _assign = self.next_item()?;
            let prev_yield = self.context.allow_yield;
            self.context.allow_yield = true;
            let right = self.isolate_cover_grammar(&Self::parse_assignment_expr)?;
            self.context.allow_yield = prev_yield;
            return Ok((
                is_restricted,
                node::Pattern::Assignment(node::AssignmentPattern {
                    left: Box::new(ret),
                    right: Box::new(right),
                }),
            ));
        }
        Ok((is_restricted, ret))
    }

    fn parse_pattern(
        &mut self,
        kind: Option<node::VariableKind>,
        params: &mut Vec<Item>,
    ) -> Res<(bool, node::Pattern)> {
        debug!(target: "resp:debug", "parse_pattern");
        if self.at_punct(Punct::OpenBracket) {
            let kind = kind.unwrap_or(node::VariableKind::Var);
            self.parse_array_pattern(params, kind)
        } else if self.at_punct(Punct::OpenBrace) {
            let kind = kind.unwrap_or(node::VariableKind::Var);
            self.parse_object_pattern()
        } else {
            let is_var = if let Some(kind) = kind {
                match kind {
                    node::VariableKind::Const | node::VariableKind::Let => {
                        if self.at_keyword(Keyword::Let) {
                            return self.error(&self.look_ahead, &["identifier"]);
                        }
                        false
                    }
                    node::VariableKind::Var => true,
                }
            } else {
                false
            };
            let ident = self.parse_var_ident(is_var)?;
            let restricted = &ident == "eval" || &ident == "arguments";
            params.push(self.look_ahead.clone());
            Ok((restricted, node::Pattern::Identifier(ident)))
        }
    }

    fn parse_array_pattern(&mut self, params: &mut Vec<Item>, kind: node::VariableKind) -> Res<(bool, node::Pattern)> {
        debug!(target: "resp:debug", "parse_array_pattern");
        self.expect_punct(Punct::OpenBracket)?;
        let mut elements = vec![];
        while !self.at_punct(Punct::CloseBracket) {
            if self.at_punct(Punct::Comma) {
                let _ = self.next_item()?;
                elements.push(None);
            } else {
                if self.at_punct(Punct::Spread) {
                    let (_, el) = self.parse_rest_element(params)?;
                    elements.push(Some(el));
                    break;
                } else {
                    let (_, el) = self.parse_pattern_with_default(params)?;
                    elements.push(Some(el));
                }
                if !self.at_punct(Punct::CloseBracket) {
                    self.expect_punct(Punct::Comma)?;
                }
            }
        }
        self.expect_punct(Punct::CloseBracket)?;
        Ok((false, node::Pattern::Array(elements)))
    }

    fn parse_object_pattern(&mut self) -> Res<(bool, node::Pattern)> {
        debug!(target: "resp:debug", "parse_object_pattern");
        self.expect_punct(Punct::OpenBrace)?;
        let mut body = vec![];
        while !self.at_punct(Punct::CloseBrace) {
            let el = if self.at_punct(Punct::Spread) {
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
        Ok((false, node::Pattern::Object(body)))
    }

    fn parse_rest_prop(&mut self) -> Res<node::ObjectPatternPart> {
        debug!(target: "resp:debug", "parse_rest_prop");
        self.expect_punct(Punct::Spread)?;
        let (_, arg) = self.parse_pattern(None, &mut vec![])?;
        if self.at_punct(Punct::Assign) {
            //unexpected token
        }
        if !self.at_punct(Punct::CloseBrace) {
            //unable to parse props after rest
        }
        let rest = node::Pattern::RestElement(Box::new(arg));
        let part = node::ObjectPatternPart::Rest(Box::new(rest));
        Ok(part)
    }

    fn parse_property_pattern(&mut self) -> Res<node::ObjectPatternPart> {
        debug!(target: "resp:debug", "parse_property_pattern");
        let mut computed = false;
        let mut short_hand = false;
        let mut method = false;
        let (key, value) = if self.look_ahead.token.is_ident() {
            let key = node::PropertyKey::Ident(self.parse_var_ident(false)?);
            let value = if self.at_punct(Punct::Assign) {
                short_hand = true;
                let e = self.parse_assignment_expr()?;
                node::PropertyValue::Expr(e)
            } else if !self.at_punct(Punct::Colon) {
                short_hand = true;
                node::PropertyValue::None
            } else {
                self.expect_punct(Punct::Colon)?;
                let (_, p) = self.parse_pattern_with_default(&mut vec![])?;
                node::PropertyValue::Pattern(p)
            };
            (key, value)
        } else {
            computed = self.at_punct(Punct::OpenBracket);
            let key = self.parse_object_property_key()?;
            self.expect_punct(Punct::Colon)?;
            let (_, v) = self.parse_pattern_with_default(&mut vec![])?;
            let value = node::PropertyValue::Pattern(v);
            (key, value)
        };
        Ok(node::ObjectPatternPart::Assignment(node::Property {
            key,
            value,
            computed,
            short_hand,
            method,
            kind: node::PropertyKind::Init,
        }))
    }

    fn parse_assignment_expr(&mut self) -> Res<node::Expression> {
        debug!(target: "resp:debug", "parse_assignment_expr");
        if self.context.allow_yield && self.at_keyword(Keyword::Yield) {
            return self.parse_yield_expr();
        } else {
            let start = self.look_ahead.clone();
            let mut current = self.parse_conditional_expr()?;
            let curr_line = self.get_item_position(&self.look_ahead).line;
            let start_line = self.get_item_position(&start).line;
            if start.token.matches_ident_str("async")
                && curr_line == start_line
                && (self.look_ahead.token.is_ident() || self.at_keyword(Keyword::Yield))
            {
                let arg = self.parse_primary_expression()?;
                let arg = self.reinterpret_expr_as_pat(arg)?;
                let arg = node::FunctionArg::Pattern(arg);
                current = node::Expression::ArrowParamPlaceHolder(vec![arg], true);
            }
            debug!(target: "resp:debug", "current expression: {:?}", current);
            if current.is_arrow_param_placeholder() || self.at_punct(Punct::FatArrow) {
                self.context.is_assignment_target = false;
                self.context.is_binding_element = false;
                let is_async = current.is_async();
                if let Some(params) = self.reinterpret_as_cover_formals_list(current.clone())? {
                    self.expect_punct(Punct::FatArrow)?;
                    if self.at_punct(Punct::OpenBrace) {
                        let prev_in = self.context.allow_in;
                        self.context.allow_in = true;
                        let body = self.parse_function_source_el()?;
                        self.context.allow_in = prev_in;
                        current = node::Expression::ArrowFunction(node::ArrowFunctionExpression {
                            id: None,
                            expression: false,
                            generator: false,
                            is_async,
                            params,
                            body: node::ArrowFunctionBody::FunctionBody(body),
                        });
                    } else {
                        let a = self.isolate_cover_grammar(&Self::parse_assignment_expr)?;
                        current = node::Expression::ArrowFunction(node::ArrowFunctionExpression {
                            id: None,
                            expression: true,
                            generator: false,
                            is_async,
                            params,
                            body: node::ArrowFunctionBody::Expr(Box::new(a)),
                        });
                    };
                }
            } else {
                if self.at_assign() {
                    if !self.context.is_assignment_target {
                        return self.error(&self.look_ahead, &["not assignment"]);
                    }
                    if self.context.strict && current.is_ident() {
                        if let node::Expression::Ident(ref i) = current {
                            if Self::is_restricted_word(i) {
                                return self.error(&self.look_ahead, &[&format!("not {}", i)]);
                            }
                            if Self::is_strict_reserved(i) {
                                return self.error(&self.look_ahead, &[&format!("not {}", i)]);
                            }
                        }
                    }
                    let left = if !self.at_punct(Punct::Assign) {
                        self.context.is_assignment_target = false;
                        self.context.is_binding_element = false;
                        node::AssignmentLeft::Expr(Box::new(current))
                    } else {
                        node::AssignmentLeft::Expr(Box::new(current))
                    };
                    let item = self.next_item()?;
                    let op = match &item.token {
                        &Token::Punct(ref p) => {
                            if let Some(op) = node::AssignmentOperator::from_punct(p) {
                                op
                            } else {
                                return self.error(
                                    &item,
                                    &[
                                        "=", "+=", "-=", "/=", "*=", "**=", "|=", "&=", "~=", "%=",
                                        "<<=", ">>=", ">>>=",
                                    ],
                                );
                            }
                        }
                        _ => {
                            return self.error(
                                &item,
                                &[
                                    "=", "+=", "-=", "/=", "*=", "**=", "|=", "&=", "~=", "%=",
                                    "<<=", ">>=", ">>>=",
                                ],
                            )
                        }
                    };
                    let right = self.isolate_cover_grammar(&Self::parse_assignment_expr)?;
                    self.context.first_covert_initialized_name_error = None;
                    return Ok(node::Expression::Assignment(node::AssignmentExpression {
                        operator: op,
                        left,
                        right: Box::new(right),
                    }));
                }
            }
            return Ok(current);
        }
    }

    fn reinterpret_as_cover_formals_list(
        &mut self,
        expr: node::Expression,
    ) -> Res<Option<Vec<node::FunctionArg>>> {
        let (mut params, async_arrow) = if expr.is_ident() {
            (vec![node::FunctionArg::Expr(expr)], false)
        } else if let node::Expression::ArrowParamPlaceHolder(params, is_async) = expr {
            (params, is_async)
        } else {
            return Ok(None);
        };
        let mut simple = true;
        let mut invalid_param = false;
        params = params
            .into_iter()
            .map(|p| {
                if p.is_assignment() {
                    match &p {
                        node::FunctionArg::Pattern(ref p) => match p {
                            node::Pattern::Assignment(ref a) => match &*a.right {
                                node::Expression::Yield(ref y) => if y.argument.is_some() {
                                    invalid_param = true;
                                } else {
                                    return node::FunctionArg::Pattern(node::Pattern::Identifier(
                                        "yield".to_owned(),
                                    ));
                                },
                                _ => (),
                            },
                            _ => (),
                        },
                        node::FunctionArg::Expr(ref e) => match e {
                            node::Expression::Assignment(ref a) => match &*a.right {
                                node::Expression::Yield(ref y) => if y.argument.is_some() {
                                    invalid_param = true;
                                } else {
                                    return node::FunctionArg::Expr(node::Expression::Ident(
                                        "yield".to_owned(),
                                    ));
                                },
                                _ => (),
                            },
                            _ => (),
                        },
                    }
                    p
                } else if async_arrow && p.is_await() {
                    invalid_param = true;
                    p
                } else {
                    p
                }
            }).collect();
        if invalid_param {
            return self.error(
                &self.look_ahead,
                &["not a yield expression in a function param"],
            );
        }
        if self.context.strict && !self.context.allow_yield {
            for param in params.iter() {
                match param {
                    node::FunctionArg::Expr(ref e) => match e {
                        node::Expression::Yield(_) => {
                            return self.error(
                                &self.look_ahead,
                                &["not a yield expression in a function param"],
                            )
                        }
                        _ => (),
                    },
                    _ => (),
                }
            }
        }
        Ok(Some(params))
    }

    fn reinterpret_expr_as_pat(&self, ex: node::Expression) -> Res<node::Pattern> {
        debug!(target: "resp:debug", "reinterpret_expr_as_pat");
        match ex {
            node::Expression::Array(a) => {
                let mut patts = vec![];
                for expr in a {
                    if let Some(e) = expr {
                        let p = self.reinterpret_expr_as_pat(e)?;
                        patts.push(Some(p));
                    } else {
                        patts.push(None)
                    }
                }
                Ok(node::Pattern::Array(patts))
            }
            node::Expression::Spread(s) => Ok(node::Pattern::RestElement(Box::new(
                self.reinterpret_expr_as_pat(*s)?,
            ))),
            node::Expression::Object(o) => {
                let mut patts = vec![];
                for expr in o {
                    match expr {
                        node::ObjectProperty::Property(p) => {
                            patts.push(node::ObjectPatternPart::Assignment(p))
                        }
                        node::ObjectProperty::Spread(s) => {
                            let p = self.reinterpret_expr_as_pat(*s)?;
                            patts.push(node::ObjectPatternPart::Rest(Box::new(p)));
                        }
                    }
                }
                Ok(node::Pattern::Object(patts))
            }
            node::Expression::Assignment(a) => {
                let left = match a.left {
                    node::AssignmentLeft::Pattern(p) => p,
                    node::AssignmentLeft::Expr(e) => self.reinterpret_expr_as_pat(*e)?,
                };
                let ret = node::AssignmentPattern {
                    left: Box::new(left),
                    right: a.right,
                };
                Ok(node::Pattern::Assignment(ret))
            }
            node::Expression::Ident(i) => Ok(node::Pattern::Identifier(i.to_string())),
            _ => Err(self.reinterpret_error("expression", "pattern")),
        }
    }

    fn parse_yield_expr(&mut self) -> Res<node::Expression> {
        debug!(target: "resp:debug", "parse_yield_expr");
        self.expect_keyword(Keyword::Yield)?;
        let mut arg: Option<Box<node::Expression>> = None;
        let mut delegate = false;
        if !self.context.has_line_term {
            let prev_yield = self.context.allow_yield;
            self.context.allow_yield = true;
            delegate = self.at_punct(Punct::Asterisk);
            if delegate {
                let _star = self.next_item()?;
                arg = Some(Box::new(self.parse_assignment_expr()?));
            } else if self.is_start_of_expr() {
                arg = Some(Box::new(self.parse_assignment_expr()?));
            }
            self.context.allow_yield = prev_yield;
        }
        let y = node::YieldExpression {
            argument: arg,
            delegate,
        };
        Ok(node::Expression::Yield(y))
    }

    fn parse_conditional_expr(&mut self) -> Res<node::Expression> {
        debug!(target: "resp:debug", "parse_conditional_expr");
        let start = self.look_ahead.clone();
        let expr = self.inherit_cover_grammar(&Self::parse_binary_expression)?;
        if self.at_punct(Punct::QuestionMark) {
            let _question_mark = self.next_item()?;
            let prev_in = self.context.allow_in;
            self.context.allow_in = true;
            let if_true = self.isolate_cover_grammar(&Self::parse_assignment_expr)?;
            self.context.allow_in = prev_in;

            self.expect_punct(Punct::Colon)?;
            let if_false = self.isolate_cover_grammar(&Self::parse_assignment_expr)?;

            let c = node::ConditionalExpression {
                test: Box::new(expr),
                alternate: Box::new(if_false),
                consequent: Box::new(if_true),
            };
            self.context.is_assignment_target = false;
            self.context.is_binding_element = false;
            return Ok(node::Expression::Conditional(c));
        }
        Ok(expr)
    }

    fn parse_binary_expression(&mut self) -> Res<node::Expression> {
        debug!(target: "resp:debug", "parse_binary_expression");
        let start = self.look_ahead.clone();
        let mut current = self.inherit_cover_grammar(&Self::parse_exponentiation_expression)?;
        let mut token = self.look_ahead.clone();
        let mut prec = self.bin_precedence(&token.token);
        if prec > 0 {
            debug!(target: "resp:debug", "prec: {} > 0", prec);
            self.next_item()?;
            self.context.is_assignment_target = false;
            self.context.is_binding_element = false;
            let mut left = current.clone();
            debug!(target: "resp:debug", "left: {:#?}", left);
            let mut right = self.isolate_cover_grammar(&Self::parse_exponentiation_expression)?;
            debug!(target: "resp:debug", "right: {:#?}", right);
            let mut stack = vec![left.clone(), right.clone()];
            let mut ops = vec![token.token.clone()];
            let mut precs = vec![prec];
            loop {
                prec = self.bin_precedence(&self.look_ahead.token);
                debug!(target: "resp:debug", "prec: {} > 0", prec);
                if prec <= 0 {
                    break;
                }
                debug!(target: "resp:debug", "shifting, stack: {}, ops: {}, last_prec: {}", stack.len(), ops.len(), precs[precs.len() - 1]);
                while stack.len() > 1 && ops.len() > 0 && prec <= precs[precs.len() - 1] {
                    right = stack
                        .pop()
                        .ok_or(self.op_error("invalid binary operation, no right expr in stack"))?;
                    debug!(target: "resp:debug", "right: {:#?}", right);
                    let op = ops
                        .pop()
                        .ok_or(self.op_error("invalid binary operation, too few operators"))?;
                    let _ = precs.pop();
                    left = stack
                        .pop()
                        .ok_or(self.op_error("invalid binary operation, no left expr in stack"))?;
                    debug!(target: "resp:debug", "left: {:#?}", left);
                    if op.matches_punct(Punct::LogicalAnd) || op.matches_punct(Punct::LogicalOr) {
                        stack.push(node::Expression::Logical(node::LogicalExpression {
                            operator: node::LogicalOperator::from_token(&op)
                                .ok_or(self.op_error("Unable to convert logical operator"))?,
                            left: Box::new(left),
                            right: Box::new(right),
                        }));
                    } else {
                        let operator = node::BinaryOperator::from_token(&op)
                            .ok_or(self.op_error("Unable to convert binary operator"))?;
                        stack.push(node::Expression::Binary(node::BinaryExpression {
                            operator,
                            left: Box::new(left),
                            right: Box::new(right),
                        }));
                    }
                }
                ops.push(self.next_item()?.token);
                precs.push(prec);
                stack.push(self.isolate_cover_grammar(&Self::parse_exponentiation_expression)?);
            }
            current = stack
                .pop()
                .ok_or(self.op_error("invalid binary operation, too few expressions"))?;

            while ops.len() > 0 && stack.len() > 0 {
                let op = ops
                    .pop()
                    .ok_or(self.op_error("invalid binary operation, too few operators"))?;
                if op.matches_punct(Punct::LogicalAnd) || op.matches_punct(Punct::LogicalOr) {
                    let operator = node::LogicalOperator::from_token(&op)
                        .ok_or(self.op_error("Unable to convert logical operator"))?;
                    current = node::Expression::Logical(node::LogicalExpression {
                        operator,
                        left: Box::new(stack.pop().ok_or(
                            self.op_error("invalid logical operation, too few expressions"),
                        )?),
                        right: Box::new(current),
                    })
                } else {
                    let operator = node::BinaryOperator::from_token(&op)
                        .ok_or(self.op_error("Unable to convert binary operator"))?;
                    current = node::Expression::Binary(node::BinaryExpression {
                        operator,
                        left: Box::new(stack.pop().ok_or(
                            self.op_error("invalid binary operation, too few expressions"),
                        )?),
                        right: Box::new(current),
                    });
                }
            }
        }
        Ok(current)
    }

    fn parse_exponentiation_expression(&mut self) -> Res<node::Expression> {
        debug!(target: "resp:debug", "parse_exponentiation_expression");
        let start = self.look_ahead.clone();
        let expr = self.inherit_cover_grammar(&Self::parse_unary_expression)?;
        if expr.is_unary() && self.at_punct(Punct::Exponent) {
            let _stars = self.next_item()?;
            self.context.is_assignment_target = false;
            self.context.is_binding_element = false;
            let left = expr;
            let right = self.isolate_cover_grammar(&Self::parse_exponentiation_expression)?;
            return Ok(node::Expression::Binary(node::BinaryExpression {
                operator: node::BinaryOperator::PowerOf,
                left: Box::new(left),
                right: Box::new(right),
            }));
        }

        Ok(expr)
    }

    fn parse_unary_expression(&mut self) -> Res<node::Expression> {
        debug!(target: "resp:debug", "parse_unary_expression");
        if self.at_punct(Punct::Plus)
            || self.at_punct(Punct::Minus)
            || self.at_punct(Punct::BitwiseNot)
            || self.at_punct(Punct::Not)
            || self.at_keyword(Keyword::Delete)
            || self.at_keyword(Keyword::Void)
            || self.at_keyword(Keyword::TypeOf)
        {
            let op = self.next_item()?;
            let arg = self.inherit_cover_grammar(&Self::parse_unary_expression)?;
            if op.token.matches_keyword(Keyword::Delete) && self.context.strict && arg.is_ident() {
                //tolerate error
            }
            self.context.is_assignment_target = false;
            self.context.is_binding_element = false;
            let operator = node::UnaryOperator::from_token(&op.token)
                .ok_or(self.op_error("Unable to convert unary operator"))?;
            Ok(node::Expression::Unary(node::UnaryExpression {
                prefix: true,
                operator,
                argument: Box::new(arg),
            }))
        } else if self.context.await && self.at_contextual_keyword("await") {
            self.parse_await_expr()
        } else {
            self.parse_update_expr()
        }
    }

    fn parse_await_expr(&mut self) -> Res<node::Expression> {
        debug!(target: "resp:debug", "parse_await_expr");
        let _await = self.next_item()?;
        let arg = self.parse_unary_expression()?;
        Ok(node::Expression::Await(Box::new(arg)))
    }

    fn parse_update_expr(&mut self) -> Res<node::Expression> {
        debug!(target: "resp:debug", "parse_update_expr");
        let start = self.look_ahead.clone();
        if self.at_punct(Punct::Increment) || self.at_punct(Punct::Decrement) {
            let next = self.next_item()?;
            let ex = self.inherit_cover_grammar(&Self::parse_unary_expression)?;
            if self.context.strict && ex.is_ident() {
                match &ex {
                    &node::Expression::Ident(ref i) => if Self::is_restricted_word(i) {
                        return self.error(&next, &[]);
                    },
                    _ => (),
                }
            }
            if !self.context.is_assignment_target {
                return self.error(&next, &[]);
            }
            let prefix = true;
            let ret = node::UpdateExpression {
                operator: if start.token.matches_punct(Punct::Increment) {
                    node::UpdateOperator::Increment
                } else if start.token.matches_punct(Punct::Decrement) {
                    node::UpdateOperator::Decrement
                } else {
                    return self.error(&start, &["++", "--"]);
                },
                argument: Box::new(ex),
                prefix,
            };
            self.context.is_assignment_target = false;
            self.context.is_binding_element = false;
            Ok(node::Expression::Update(ret))
        } else {
            let expr = self.inherit_cover_grammar(&Self::parse_left_hand_side_expr_allow_call)?;
            if !self.context.has_line_term && self.look_ahead.token.is_punct() {
                if self.at_punct(Punct::Increment) || self.at_punct(Punct::Decrement) {
                    if self.context.strict {
                        match &expr {
                            &node::Expression::Ident(ref i) => if Self::is_restricted_word(i) {
                                return self.error(&start, &[]);
                            },
                            _ => (),
                        }
                    }
                    if !self.context.is_assignment_target {
                        return self.error(&start, &[]);
                    }
                    self.context.is_assignment_target = false;
                    self.context.is_binding_element = false;
                    let op = self.next_item()?;
                    let prefix = false;
                    let ret = node::UpdateExpression {
                        operator: if op.token.matches_punct(Punct::Increment) {
                            node::UpdateOperator::Increment
                        } else if op.token.matches_punct(Punct::Decrement) {
                            node::UpdateOperator::Decrement
                        } else {
                            return self.error(&op, &["++", "--"]);
                        },
                        argument: Box::new(expr),
                        prefix,
                    };
                    return Ok(node::Expression::Update(ret));
                }
            }
            Ok(expr)
        }
    }

    fn parse_left_hand_side_expr(&mut self) -> Res<node::Expression> {
        if !self.context.allow_in {
            // error
        }
        let mut expr = if self.at_keyword(Keyword::Super) && self.context.in_function_body {
            self.parse_super()?
        } else {
            if self.at_keyword(Keyword::New) {
                self.inherit_cover_grammar(&Self::parse_new_expr)?
            } else {
                self.inherit_cover_grammar(&Self::parse_primary_expression)?
            }
        };
        loop {
            if self.at_punct(Punct::OpenBracket) {
                self.context.is_binding_element = false;
                self.context.is_assignment_target = true;
                self.expect_punct(Punct::OpenBracket)?;
                let prop = self.isolate_cover_grammar(&Self::parse_expression)?;
                self.expect_punct(Punct::CloseBracket)?;
                let member = node::MemberExpression {
                    computed: true,
                    object: Box::new(expr),
                    property: Box::new(prop),
                };
                expr = node::Expression::Member(member);
            } else if self.at_punct(Punct::Period) {
                self.context.is_binding_element = false;
                self.context.is_assignment_target = false;
                self.expect_punct(Punct::Period)?;
                let prop = self.parse_ident_name()?;
                let member = node::MemberExpression {
                    object: Box::new(expr),
                    property: Box::new(node::Expression::Ident(prop)),
                    computed: false,
                };
                expr = node::Expression::Member(member);
            } else if self.look_ahead.token.is_template() {
                let quasi = self.parse_template_literal()?;
                expr = node::Expression::TaggedTemplate(node::TaggedTemplateExpression {
                    tag: Box::new(expr),
                    quasi,
                });
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_super(&mut self) -> Res<node::Expression> {
        self.expect_keyword(Keyword::Super)?;
        if !self.at_punct(Punct::OpenBracket) && !self.at_punct(Punct::Period) {
            return self.error(&self.look_ahead, &["[", "."]);
        }
        Ok(node::Expression::SuperExpression)
    }

    fn parse_left_hand_side_expr_allow_call(&mut self) -> Res<node::Expression> {
        debug!(target: "resp:debug", "parse_left_hand_side_expr_allow_call");
        let start_pos = self.get_item_position(&self.look_ahead);
        let is_async = self.at_contextual_keyword("async");
        let prev_in = self.context.allow_in;
        self.context.allow_in = true;

        let mut expr = if self.at_keyword(Keyword::Super) && self.context.in_function_body {
            let _ = self.next_item()?;
            if !self.at_punct(Punct::OpenParen)
                && !self.at_punct(Punct::Period)
                && !self.at_punct(Punct::OpenBracket)
            {
                return self.error(&self.look_ahead, &["(", ".", "["]);
            }
            node::Expression::SuperExpression
        } else {
            if self.at_keyword(Keyword::New) {
                self.inherit_cover_grammar(&Self::parse_new_expr)?
            } else {
                self.inherit_cover_grammar(&Self::parse_primary_expression)?
            }
        };
        loop {
            if self.at_punct(Punct::Period) {
                self.context.is_binding_element = false;
                self.context.is_assignment_target = true;
                self.expect_punct(Punct::Period)?;
                let prop = node::Expression::Ident(self.parse_ident_name()?);
                expr = node::Expression::Member(node::MemberExpression {
                    object: Box::new(expr),
                    property: Box::new(prop),
                    computed: false,
                })
            } else if self.at_punct(Punct::OpenParen) {
                let current_pos = self.get_item_position(&self.look_ahead);
                let async_arrow = is_async && start_pos.line == current_pos.line;
                self.context.is_binding_element = false;
                self.context.is_assignment_target = false;
                let args = if async_arrow {
                    self.parse_async_args()?
                } else {
                    self.parse_args()?
                };
                //TODO: check for bad import call
                if async_arrow && self.at_punct(Punct::FatArrow) {
                    let args = args.into_iter().map(|a| node::FunctionArg::Expr(a)).collect();
                    expr = node::Expression::ArrowParamPlaceHolder(args, true);
                } else {
                    let inner = node::CallExpression {
                        callee: Box::new(expr),
                        arguments: args,
                    };
                    expr = node::Expression::Call(inner);
                }
            } else if self.at_punct(Punct::OpenBracket) {
                self.context.is_assignment_target = true;
                self.context.is_binding_element = false;
                self.expect_punct(Punct::OpenBracket)?;
                let prop = self.isolate_cover_grammar(&Self::parse_expression)?;
                self.expect_punct(Punct::CloseBracket)?;
                let inner = node::MemberExpression {
                    object: Box::new(expr),
                    computed: true,
                    property: Box::new(prop),
                };
                expr = node::Expression::Member(inner);
            } else if self.look_ahead.token.is_template_head() {
                let quasi = self.parse_template_literal()?;
                let temp = node::TaggedTemplateExpression {
                    tag: Box::new(expr),
                    quasi,
                };
                expr = node::Expression::TaggedTemplate(temp);
            } else {
                break;
            }
        }
        self.context.allow_in = prev_in;
        Ok(expr)
    }

    fn parse_async_args(&mut self) -> Res<Vec<node::Expression>> {
        debug!(target: "resp:debug", "parse_async_args");
        self.expect_punct(Punct::OpenParen)?;
        let mut ret = vec![];
        if !self.at_punct(Punct::CloseParen) {
            loop {
                let arg = if self.at_punct(Punct::Spread) {
                    self.parse_spread_element()?
                } else {
                    self.isolate_cover_grammar(&Self::parse_async_arg)?
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

    fn expect_comma_sep(&mut self) -> Res<()> {
        debug!(target: "resp:debug", "expect_comma_sep");
        if self.config.tolerant {
            if self.at_punct(Punct::Comma) {
                let _ = self.next_item()?;
                Ok(())
            } else if self.at_punct(Punct::SemiColon) {
                //tolerate unexpected
                Ok(())
            } else {
                //tolerate unexpected... differently
                Ok(())
            }
        } else {
            self.expect_punct(Punct::Comma)
        }
    }

    fn parse_async_arg(&mut self) -> Res<node::Expression> {
        debug!(target: "resp:debug", "parse_async_arg");
        let expr = self.parse_assignment_expr()?;
        self.context.first_covert_initialized_name_error = None;
        Ok(expr)
    }

    fn parse_spread_element(&mut self) -> Res<node::Expression> {
        debug!(target: "resp:debug", "parse_spread_element");
        self.expect_punct(Punct::Spread)?;
        let arg = self.inherit_cover_grammar(&Self::parse_assignment_expr)?;
        Ok(
            node::Expression::Spread(Box::new(arg))
        )
    }

    fn parse_args(&mut self) -> Res<Vec<node::Expression>> {
        debug!(target: "resp:debug", "parse_args");
        self.expect_punct(Punct::OpenParen)?;
        let mut args = vec![];
        if !self.at_punct(Punct::CloseParen) {
            loop {
                let expr = if self.at_punct(Punct::Spread) {
                    self.parse_spread_element()?
                } else {
                    self.isolate_cover_grammar(&Self::parse_assignment_expr)?
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

    fn parse_new_expr(&mut self) -> Res<node::Expression> {
        debug!(target: "resp:debug", "parse_new_expr");
        self.expect_keyword(Keyword::New)?;
        if self.at_punct(Punct::Period) {
            if self.look_ahead.token.matches_ident_str("target") && self.context.in_function_body {
                let property = self.parse_ident_name()?;
                Ok(node::Expression::MetaProperty(node::MetaProperty {
                    meta: String::from("new"),
                    property,
                }))
            } else {
                self.error(&self.look_ahead, &["[constructor function call]"])
            }
        } else if self.at_keyword(Keyword::Import) {
            self.error(&self.look_ahead, &["not import"])
        } else {
            let callee = self.isolate_cover_grammar(&Self::parse_left_hand_side_expr)?;
            let args = if self.at_punct(Punct::OpenParen) {
                self.parse_args()?
            } else {
                vec![]
            };
            self.context.is_assignment_target = false;
            self.context.is_binding_element = false;
            let new = node::NewExpression {
                callee: Box::new(callee),
                arguments: args,
            };

            Ok(node::Expression::New(new))
        }
    }

    fn bin_precedence(&self, tok: &Token) -> usize {
        match tok {
            &Token::Punct(ref p) => Self::determine_precedence(p),
            &Token::Keyword(ref k) => {
                if k == &Keyword::InstanceOf || (self.context.allow_in && k == &Keyword::In) {
                    7
                } else {
                    0
                }
            }
            _ => 0,
        }
    }

    fn determine_precedence(p: &Punct) -> usize {
        match p {
            &Punct::CloseParen
            | &Punct::SemiColon
            | &Punct::Comma
            | &Punct::Assign
            | &Punct::CloseBracket => 0,
            &Punct::LogicalOr => 1,
            &Punct::LogicalAnd => 2,
            &Punct::Pipe => 3,
            &Punct::Caret => 4,
            &Punct::And => 5,
            &Punct::Equal | &Punct::NotEqual | &Punct::StrictEquals | &Punct::StrictNotEquals => 6,
            &Punct::GreaterThan
            | &Punct::LessThan
            | &Punct::LessThanEqual
            | &Punct::GreaterThanEqual => 7,
            &Punct::LeftShift | &Punct::RightShift | &Punct::UnsignedRightShift => 8,
            &Punct::Plus | &Punct::Minus => 9,
            &Punct::Asterisk | &Punct::ForwardSlash | &Punct::Modulo => 11,
            _ => 0,
        }
    }

    fn isolate_cover_grammar<T, F>(&mut self, parse_fn: &F) -> Res<T>
    where
        F: Fn(&mut Self) -> Res<T>,
    {
        debug!(target: "resp:debug", "isolate_cover_grammar");
        let prev_bind = self.context.is_binding_element;
        let prev_ass = self.context.is_assignment_target;
        let prev_first = self.context.first_covert_initialized_name_error.clone();
        self.context.is_binding_element = true;
        self.context.is_assignment_target = true;
        self.context.first_covert_initialized_name_error = None;
        let res = parse_fn(self)?;
        if let Some(ref e) = self.context.first_covert_initialized_name_error {
            return self.error(e, &[]);
        }
        self.context.is_binding_element = prev_bind;
        self.context.is_assignment_target = prev_ass;
        self.context.first_covert_initialized_name_error = prev_first;
        Ok(res)
    }

    fn inherit_cover_grammar<T, F>(&mut self, parse_fn: &F) -> Res<T>
    where
        F: Fn(&mut Self) -> Res<T>,
    {
        debug!(target: "resp:debug", "inherit_cover_grammar");
        let prev_bind = self.context.is_binding_element;
        let prev_ass = self.context.is_assignment_target;
        let prev_first = self.context.first_covert_initialized_name_error.clone();

        self.context.is_binding_element = true;
        self.context.is_assignment_target = true;
        self.context.first_covert_initialized_name_error = None;

        let res = parse_fn(self)?;

        self.context.is_binding_element = self.context.is_binding_element && prev_bind;
        self.context.is_assignment_target = self.context.is_assignment_target && prev_ass;
        if prev_first.is_some() {
            self.context.first_covert_initialized_name_error = prev_first
        }

        Ok(res)
    }



    /// Request the next token from the scanner
    /// swap the last look ahead with this new token
    /// and return the last token
    fn next_item(&mut self) -> Res<Item> {
        unsafe {
            if self.scanner.cursor > 2541 {
                    DEBUG = true
            }
        }
        loop {
            if let Some(look_ahead) = self.scanner.next() {
                unsafe {
                    if DEBUG {
                        println!("{:?}", look_ahead);
                    }
                }
                if look_ahead.token.is_comment() {
                    if self.config.comments {
                        self.comments.push(look_ahead);
                    }
                    continue;
                }
                let old_pos = self.get_item_position(&self.look_ahead);
                let new_pos = self.get_item_position(&look_ahead);
                self.context.has_line_term = old_pos.line != new_pos.line;
                self.current_position = old_pos;
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

    fn expect_punct(&mut self, p: Punct) -> Res<()> {
        let next = self.next_item()?;
        if !next.token.matches_punct_str(&p.to_string()) {
            return self.error(&next, &[&format!("{:?}", p)]);
        }
        Ok(())
    }

    fn expect_keyword(&mut self, k: Keyword) -> Res<()> {
        let next = self.next_item()?;
        if !next.token.matches_keyword_str(&k.to_string()) {
            return self.error(&next, &[&format!("{:?}", k)]);
        }
        Ok(())
    }

    fn at_binary_punct(&mut self) -> bool {
        match &self.look_ahead.token {
            Token::Keyword(ref k) => match k {
                Keyword::InstanceOf => true,
                Keyword::In => true,
                _ => false,
            },
            Token::Punct(ref p) => match p {
                Punct::Equal => true,
                Punct::NotEqual => true,
                Punct::StrictEquals => true,
                Punct::StrictNotEquals => true,
                Punct::LessThan => true,
                Punct::LessThanEqual => true,
                Punct::GreaterThan => true,
                Punct::GreaterThanEqual => true,
                Punct::LeftShift => true,
                Punct::RightShift => true,
                Punct::UnsignedRightShift => true,
                Punct::Plus => true,
                Punct::Minus => true,
                Punct::Asterisk => true,
                Punct::ForwardSlash => true,
                Punct::Modulo => true,
                Punct::LogicalAnd => true,
                Punct::LogicalOr => true,
                Punct::Caret => true,
                _ => false,
            },
            _ => false,
        }
    }

    fn at_lexical_decl(&mut self) -> bool {
        let state = self.scanner.get_state();
        self.scanner.skip_comments();
        let ret = if let Some(next) = self.scanner.next() {
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

    fn at_punct(&self, p: Punct) -> bool {
        self.look_ahead.token.matches_punct(p)
    }

    fn at_keyword(&self, k: Keyword) -> bool {
        self.look_ahead.token.matches_keyword(k)
    }

    fn at_assign(&self) -> bool {
        debug!(target: "resp:debug", "at_assign {:#?}", self.look_ahead.token);
        self.look_ahead.token.matches_punct(Punct::Assign)
            || self.look_ahead.token.matches_punct(Punct::MultiplyAssign)
            || self.look_ahead.token.matches_punct(Punct::ExponentAssign)
            || self.look_ahead.token.matches_punct(Punct::DivideAssign)
            || self.look_ahead.token.matches_punct(Punct::ModuloAssign)
            || self.look_ahead.token.matches_punct(Punct::AddAssign)
            || self.look_ahead.token.matches_punct(Punct::SubtractAssign)
            || self.look_ahead.token.matches_punct(Punct::LeftShiftAssign)
            || self.look_ahead.token.matches_punct(Punct::RightShiftAssign)
            || self
                .look_ahead
                .token
                .matches_punct(Punct::UnsignedRightShiftAssign)
            || self.look_ahead.token.matches_punct(Punct::AddAssign)
            || self.look_ahead.token.matches_punct(Punct::BitwiseOrAssign)
            || self.look_ahead.token.matches_punct(Punct::BitwiseXOrAssign)
            || self.look_ahead.token.matches_punct(Punct::BitwiseAndAssign)
    }

    fn at_async_function(&mut self) -> bool {
        if self.at_contextual_keyword("async") {
            if let Some(peek) = self.scanner.look_ahead() {
                let pos = self.get_item_position(&self.look_ahead);
                let next_pos = self.get_item_position(&peek);
                pos.line == next_pos.line && peek.token.matches_keyword(Keyword::Function)
            } else {
                false
            }
        } else {
            false
        }
    }

    fn consume_semicolon(&mut self) -> Res<()> {
        if self.at_punct(Punct::SemiColon) {
            let _semi = self.next_item()?;
        } else if !self.context.has_line_term {
            if !self.look_ahead.token.is_eof() && !self.at_punct(Punct::CloseBrace) {
                return self.error(&self.look_ahead, &["eof", "}"]);
            }
        }
        Ok(())
    }

    fn at_contextual_keyword(&self, s: &str) -> bool {
        self.look_ahead.token.matches_ident_str(s)
    }

    fn is_restricted_word(word: &str) -> bool {
        word == "eval" || word == "arguments"
    }

    fn is_strict_reserved(word: &str) -> bool {
        word == "implements"
            || word == "interface"
            || word == "package"
            || word == "private"
            || word == "protected"
            || word == "public"
            || word == "static"
            || word == "yield"
            || word == "let"
    }

    fn is_start_of_expr(&self) -> bool {
        let mut ret = true;
        let token = &self.look_ahead.token;
        if token.is_punct() {
            ret = token.matches_punct(Punct::OpenBracket)
                || token.matches_punct(Punct::OpenParen)
                || token.matches_punct(Punct::OpenBracket)
                || token.matches_punct(Punct::Plus)
                || token.matches_punct(Punct::Minus)
                || token.matches_punct(Punct::Not)
                || token.matches_punct(Punct::BitwiseNot)
                || token.matches_punct(Punct::Increment)
                || token.matches_punct(Punct::Decrement)
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

    fn get_item_position(&self, item: &Item) -> Position {
        let (idx, line) = if let Some((idx, line)) = self
            .lines
            .iter()
            .enumerate()
            .find(|(_, l)| l.end + 1 >= item.span.start)
        {
            (idx, line)
        } else {
            panic!("Unable to determine item's line number {:?}", item);
        };
        let column = item.span.start.saturating_sub(line.start);
        Position {
            line: idx + 1,
            column,
        }
    }

    fn error<T>(&self, item: &Item, expectation: &[&str]) -> Res<T> {
        let pos = self.get_item_position(item);
        let expectation = expectation
            .iter()
            .enumerate()
            .map(|(i, s)| {
                if i == expectation.len() - 1 && expectation.len() > 1 {
                    format!("or `{}`", s)
                } else {
                    format!("`{}`", s)
                }
            }).collect::<Vec<String>>()
            .join(", ");
        Err(Error::UnexpectedToken(
            pos,
            format!("Expected {}; found {:?}", expectation, item.token),
        ))
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

    fn next_part(&mut self) -> Res<node::ProgramPart> {
        if !self.context.past_prolog {
            if let Some(dir) = self.parse_directive()? {
                return Ok(node::ProgramPart::Directive(dir))
            } else {
                self.context.past_prolog = true;
            }
        }
        self.parse_statement_list_item()
    }
}

impl Iterator for Parser {
    type Item = Res<node::ProgramPart>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.look_ahead.token.is_eof() {
            None
        } else {
            Some(self.next_part())
        }

    }
}

#[allow(unused)]
struct FormalParams {
    simple: bool,
    params: Vec<node::FunctionArg>,
    strict: bool,
    found_restricted: bool,
}

#[allow(unused)]
struct CoverFormalListOptions {
    simple: bool,
    params: Vec<node::FunctionArg>,
    stricted: bool,
    first_restricted: Option<node::Expression>,
}

pub(crate) fn init_logging() {
    let mut b = env_logger::Builder::new();
    b.target(env_logger::Target::Stdout);

    if let Ok(args) = ::std::env::var("RUST_LOG") {
        b.parse(&args);
    }
    let _ = b.try_init();
}