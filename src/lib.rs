#![allow(unused)]
extern crate ress;

use ress::{Scanner, Item, Keyword, Token, Punct};
pub mod node;
mod error;
use node::{Position};
use error::Error;
use std::{
    collections::HashSet,
    mem::replace
};
#[allow(unused)]
struct Config {
    range: bool,
    loc: bool,
    source: Option<String>,
    tokens: bool,
    comment: bool,
    tolerant: bool,
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
    has_line_term: bool
}

impl Default for Config {
    fn default() -> Self {
        Self {
            range: false,
            loc: false,
            source: None,
            tokens: false,
            comment: false,
            tolerant: false,
        }
    }
}

impl Default for Context {
    fn default() -> Self {
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
        }
    }
}
#[allow(unused)]
pub struct Parser {
    context: Context,
    config: Config,
    scanner: Scanner,
    lines: Vec<Line>,
    pub look_ahead: Item,
    found_eof: bool,
    tokens: Vec<Item>,
    comments: Vec<Item>,
}
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Line {
    start: usize,
    end: usize,
}

type Res<T> = Result<T, Error>;

fn get_lines(text: &str) -> Vec<Line> {
    let mut line_start = 0;
    let mut ret: Vec<Line> = text.chars().enumerate().filter_map(|(i, c)| {
        match c {
        '\r' => {
            // look ahead 1 char to see if it is a newline pair
            // if so, don't include it, it will get included in the next
            // iteration
            if let Some(next) = text.get(i..i+2) {
                if next == "\r\n" {
                    None
                } else {
                    let ret = Line {
                        start: line_start,
                        end: i,
                    };
                    line_start = i + 1;
                    Some(ret)
                }
            } else {
                None
            }
        },
        '\n' | '\u{2028}' | '\u{2029}' => {
            let ret = Line {
                start: line_start,
                end: i,
            };
            line_start = i + 1;
            Some(ret)
        },
        _ => None,
    }}).collect();

    ret.push(Line {
        start: line_start,
        end: text.len() - 1,
    });
    ret
}

impl Parser {
    pub fn new(text: &str) -> Res<Parser> {
        let lines = get_lines(text);
        let mut s = Scanner::new(text);
        let look_ahead = if let Some(look_ahead) = s.next() {
            look_ahead
        } else {
            return Err(Error::UnexpectedEoF);
        };
        Ok(Parser {
            scanner: s,
            look_ahead,
            lines,
            found_eof: false,
            config: Config::default(),
            context: Context::default(),
            tokens: vec![],
            comments: vec![],
        })
    }

    pub fn parse_script(&mut self) -> Res<node::Program> {
        let mut body = self.parse_directive_prologues()?;
        while !self.look_ahead.token.is_eof() {
            body.push(self.parse_statement_list_item_script()?)
        }
        Ok(node::Program::Script(body))
    }

    fn parse_directive_prologues(&mut self) -> Res<Vec<node::ScriptPart>> {
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
                ret.push(node::ScriptPart::Directive(dir));
            } else {
                break;
            }
        }
        Ok(ret)
    }

    fn parse_directive(&mut self) -> Res<Option<node::Directive>> {
        if self.look_ahead.token.is_string() {
            let dir = self.next_item()?;
            match dir.token {
                Token::String(s) => {
                    let quoted = s.to_string();
                    let directive = match s.no_quote() {
                        Ok(d) => d,
                        Err(e) => return self.error(&self.look_ahead, &[&e])
                    };
                    let expression = node::Literal::String(quoted);
                    Ok(Some(node::Directive {
                        expression,
                        directive
                    }))
                },
                _ => unreachable!()
            }
        } else {
            Ok(None)
        }
    }

    fn parse_expression(&mut self) -> Res<node::Expression> {
        let expr = self.isolate_cover_grammar(&Self::parse_assignment_expr)?;
        if self.at_punct(Punct::Comma) {
            let mut list = vec![expr];
            while !self.look_ahead.token.is_eof() {
                if !self.at_punct(Punct::Comma) {
                    break;
                }
                let _comma = self.next_item()?;
                list.push(self.isolate_cover_grammar(&Self::parse_assignment_expr)?);
            }
            return Ok(node::Expression::Sequence(list));
        }
        Ok(expr)
    } 

    fn parse_statement_list_item_script(&mut self) -> Res<node::ScriptPart> {
        self.context.is_assignment_target = true;
        self.context.is_binding_element = true;
        let tok = self.look_ahead.token.clone();
        match &tok {
            Token::Keyword(ref k) => {
                match k {
                    &Keyword::Import => self.error(&self.look_ahead, &[]),
                    &Keyword::Export => self.error(&self.look_ahead, &[]),
                    &Keyword::Const => {
                        let decl = self.parse_lexical_decl(false)?;
                        Ok(node::ScriptPart::Decl(decl))
                    },
                    &Keyword::Function => {
                        let func = self.parse_function_decl(false)?;
                        let decl = node::Declaration::Function(func);
                        Ok(node::ScriptPart::Decl(decl))
                    }
                    &Keyword::Class => {
                        let class = self.parse_class_decl(false)?;
                        let decl = node::Declaration::Class(class);
                        Ok(node::ScriptPart::Decl(decl))
                    },
                    &Keyword::Let => {
                        Ok(if self.at_lexical_decl()? {
                            let decl = self.parse_lexical_decl(false)?;
                            node::ScriptPart::Decl(decl)
                        } else {
                            let stmt = self.parse_statement()?;
                            node::ScriptPart::Statement(stmt)
                        })
                    }
                    _ => {
                        let stmt = self.parse_statement()?;
                        Ok(node::ScriptPart::Statement(stmt))
                    }
                }
            },
            _ => {
                let stmt = self.parse_statement()?;
                Ok(node::ScriptPart::Statement(stmt))
            },
        }
    }

    fn parse_statement(&mut self) -> Res<node::Statement> {
        let lh = self.look_ahead.token.clone();
        let stmt = match lh {
            Token::Boolean(_)
            | Token::Null
            | Token::Numeric(_)
            | Token::String(_)
            | Token::RegEx(_) => {
                let expr = self.parse_expression_statement()?;
                node::Statement::Expr(expr)
            },
            Token::Punct(ref p) => {
                match p {
                    Punct::OpenBrace => {
                        let b = self.parse_block()?;
                        node::Statement::Block(b)
                    },
                    Punct::OpenParen => {
                        let expr = self.parse_expression_statement()?;
                        node::Statement::Expr(expr)
                    },
                    Punct::SemiColon => {
                        let _ = self.next_item()?;
                        node::Statement::Empty
                    },
                    _ => {
                        let expr = self.parse_expression_statement()?;
                        node::Statement::Expr(expr)
                    }
                }
            },
            Token::Ident(_) => {
                if self.at_async_function() {
                    let f = self.parse_function_decl(false)?;
                    node::Statement::Expr(node::Expression::Function(f))
                } else {
                    self.parse_labelled_statement()?
                }
            },
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
                _ => node::Statement::Expr(self.parse_expression_statement()?)
            }
            _ => return self.error(&self.look_ahead, &[])
        };
        Ok(stmt)
    }

    fn parse_with_stmt(&mut self) -> Res<node::WithStatement> {
        if self.context.strict {
            // error
        }
        self.expect_keyword(Keyword::With)?;
        self.expect_punct(Punct::OpenParen)?;
        let obj = self.parse_expression()?;
        Ok(if !self.at_punct(Punct::CloseParen) && self.config.tolerant {
            //tolerate error
            node::WithStatement {
                object: obj,
                body: Box::new(node::Statement::Empty),
            }
        } else {
            self.expect_punct(Punct::CloseParen)?;
            node::WithStatement {
                object: obj,
                body: Box::new(self.parse_statement()?)
            }
        })
    }

    fn parse_while_stmt(&mut self) -> Res<node::WhileStatement> {
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
        self.expect_keyword(Keyword::Var)?;
        let decls = self.parse_binding_list(&node::VariableKind::Var, false)?;
        let stmt = node::Statement::Var(decls);
        self.consume_semicolon()?;
        Ok(stmt)
    }

    fn parse_try_stmt(&mut self) -> Res<node::TryStatement> {
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
        self.expect_keyword(Keyword::Catch)?;
        self.expect_punct(Punct::OpenParen)?;
        if self.at_punct(Punct::CloseParen) {
            //error variable named required
        }
        let mut params = vec![];
        let param = self.parse_pattern(None, &mut params)?;
        let body = self.parse_block()?;
        Ok(node::CatchClause {
            param,
            body,
        })
    }

    fn parse_finally_clause(&mut self) -> Res<node::BlockStatement> {
        self.expect_keyword(Keyword::Finally)?;
        self.parse_block()
    }

    fn parse_throw_stmt(&mut self) -> Res<node::Expression> {
        self.expect_keyword(Keyword::Throw)?;
        if self.context.has_line_term {
            //error: no new line allowed after throw
        }
        let arg = self.parse_expression()?;
        self.consume_semicolon()?;
        Ok(arg)
    }

    fn parse_switch_stmt(&mut self) -> Res<node::SwitchStatement> {
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
                    return self.error(&self.look_ahead, &[])
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
        let test = if self.at_keyword(Keyword::Default) {
            None
        } else {
            self.expect_keyword(Keyword::Case)?;
            Some(self.parse_expression()?)
        };
        self.expect_punct(Punct::Colon)?;
        let mut consequent = vec![];
        loop {
            if self.at_punct(Punct::CloseBrace) || self.at_keyword(Keyword::Default) || self.at_keyword(Keyword::Case) {
                break;
            }
            consequent.push(self.parse_statement_list_item_script()?)
        }
        Ok(node::SwitchCase {
            test,
            consequent,
        })
    }

    fn parse_return_stmt(&mut self) -> Res<Option<node::Expression>> {
        if !self.context.in_function_body {
            //tolerate error
        }
        self.expect_keyword(Keyword::Return)?;
        // if we are at a semi-colon,or close curly brace or eof
        //the return doesn't have an arg. If we are at a line term
        //we need to account for a string literal or template literal
        //since they both can have new lines
        let ret = if (!self.at_punct(Punct::SemiColon) 
                    && !self.at_punct(Punct::CloseBrace)
                    && !self.context.has_line_term
                    && !self.look_ahead.token.is_eof())
                    || self.look_ahead.token.is_string() {
            Some(self.parse_expression()?)
        } else {
            None
        };
        self.consume_semicolon()?;
        Ok(ret)
    }

    fn parse_if_stmt(&mut self) -> Res<node::IfStatement> {
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
        if self.context.strict && self.at_keyword(Keyword::Function) {
            //tolerate error
        }
        self.parse_statement()
    }

    fn parse_fn_stmt(&mut self) -> Res<node::Expression> {
        let decl = self.parse_function_decl(false)?;
        Ok(node::Expression::Function(decl))
    }

    fn parse_for_stmt(&mut self) -> Res<node::Statement> {
        self.expect_keyword(Keyword::For)?;
        let await = if self.at_keyword(Keyword::Await) {
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
            return Ok(node::Statement::For(stmt))
        }

        if self.at_keyword(Keyword::Var) {
            let kind = node::VariableKind::Var;
            let _ = self.next_item()?;
            let prev_in = self.context.allow_in;
            self.context.allow_in = false;
            let mut bindings = self.parse_binding_list(&kind, true)?;
            self.context.allow_in = prev_in;
            if bindings.len() == 1 {
                let decl = if let Some(d) = bindings.pop() {
                    d
                } else {
                    return self.error(&self.look_ahead, &["variable decl"])
                };
                if self.at_keyword(Keyword::In) {
                    let left = node::LoopLeft::Variable(decl);
                    let stmt = self.parse_for_in_loop(left)?;
                    return Ok(node::Statement::ForIn(stmt))
                } else if self.at_keyword(Keyword::Of) {
                    let left = node::LoopLeft::Variable(decl);
                    let stmt = self.parse_for_of_loop(left)?;
                    return Ok(node::Statement::ForOf(stmt))
                } else {
                    let init = node::LoopInit::Variable(vec![decl]);
                    let stmt = self.parse_for_loop_cont(Some(init))?;
                    return Ok(node::Statement::For(stmt))
                }
            } else {
                let init = node::LoopInit::Variable(bindings);
                let stmt = self.parse_for_loop_cont(Some(init))?;
                return Ok(node::Statement::For(stmt))
            }
        } else if self.at_keyword(Keyword::Const) || self.at_keyword(Keyword::Let) {
            let kind = self.next_item()?;
            let kind = match &kind.token {
                Token::Keyword(ref k) => match k {
                    Keyword::Const => node::VariableKind::Const,
                    Keyword::Let => node::VariableKind::Let,
                    _ => unreachable!(),
                },
                _ => return self.error(&kind, &["const", "let"])
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
                    body: Box::new(self.parse_loop_body()?)
                }))
            } else {
                let prev_in = self.context.allow_in;
                self.context.allow_in = false;
                let mut decls = self.parse_binding_list(&kind, true)?;
                if decls.len() == 1 {
                    let decl = if let Some(d) = decls.pop() {
                        d
                    } else {
                        return self.error(&self.look_ahead, &["variable decl"])
                    };
                    if decl.init.is_none() && self.at_keyword(Keyword::In) {
                        let left = node::LoopLeft::Variable(decl);
                        let _in = self.next_item()?;
                        let right = self.parse_expression()?;
                        return Ok(node::Statement::ForIn(node::ForInStatement {
                            left,
                            right,
                            body: Box::new(self.parse_loop_body()?)
                        }));
                    } else if decl.init.is_none() && self.at_keyword(Keyword::Of) {
                        let left = node::LoopLeft::Variable(decl);
                        let _of = self.next_item()?;
                        let right = self.parse_assignment_expr()?;
                        return Ok(node::Statement::ForOf(node::ForOfStatement {
                            left,
                            right,
                            body: Box::new(self.parse_loop_body()?),
                            await,
                        }))
                    } else {
                        let init = node::LoopInit::Variable(vec![decl]);
                        let stmt = self.parse_for_loop_cont(Some(init))?;
                        return Ok(node::Statement::For(stmt))
                    }
                } else {
                    let init = node::LoopInit::Variable(decls);
                    let stmt = self.parse_for_loop_cont(Some(init))?;
                    return Ok(node::Statement::For(stmt))
                }
            }
        } else {
            let start = self.look_ahead.clone();
            let prev_in = self.context.allow_in;
            self.context.allow_in = false;
            let init = self.inherit_cover_grammar(&Self::parse_assignment_expr)?;
            self.context.allow_in = prev_in;
            if self.at_keyword(Keyword::In) {
                let _in = self.next_item()?;
                let pat = match init {
                    node::Expression::Assignment(a) => match a.left {
                        node::AssignmentLeft::Pattern(p) => p,
                        _ => return self.error(&start, &["assignment pattern"])
                    },
                    _ => unimplemented!("destructuring in loop binding")
                };
                let left = node::LoopLeft::Pattern(pat);
                let right = self.parse_expression()?;
                return Ok(node::Statement::ForIn(node::ForInStatement {
                    left,
                    right,
                    body: Box::new(self.parse_loop_body()?)
                }))
            } else if self.at_keyword(Keyword::Of) {
                let _of = self.next_item()?;
                let p = match init {
                    node::Expression::Assignment(a) => match a.left {
                        node::AssignmentLeft::Pattern(p) => p,
                        _ =>return self.error(&start, &["assignment pattern"])
                    },
                    _ => unimplemented!("destructuring in loop binding")
                };
                let left = node::LoopLeft::Pattern(p);
                let right = self.parse_assignment_expr()?;
                return Ok(node::Statement::ForOf(node::ForOfStatement {
                    left,
                    right,
                    body: Box::new(self.parse_loop_body()?),
                    await,
                }))
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
                return Ok(node::Statement::For(self.parse_for_loop_cont(Some(init))?))
            }
        }

    }

    fn parse_for_loop(&mut self, kind: node::VariableKind) -> Res<node::ForStatement> {
        let init = if self.at_punct(Punct::SemiColon) {
            None
        } else {
            let list = self.parse_binding_list(&kind, true)?;
            Some(node::LoopInit::Variable(list))
        };
        self.parse_for_loop_cont(init)
    }

    fn parse_for_loop_cont(&mut self, init: Option<node::LoopInit>) -> Res<node::ForStatement> {
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
        unimplemented!("parse_for_in_loop")
    }

    fn parse_for_of_loop(&mut self, left: node::LoopLeft) -> Res<node::ForOfStatement> {
        unimplemented!("parse_for_of_loop")
    }

    fn parse_loop_body(&mut self) -> Res<node::Statement> {
        self.expect_punct(Punct::CloseParen)?;
        let prev_iter = self.context.in_iteration;
        self.context.in_iteration = true;
        let ret = self.isolate_cover_grammar(&Self::parse_statement)?;
        self.context.in_iteration = prev_iter;
        Ok(ret)
    }

    fn parse_do_while_stmt(&mut self) -> Res<node::DoWhileStatement> {
        self.expect_keyword(Keyword::Do)?;
        let prev_iter = self.context.in_iteration;
        self.context.in_iteration = true;
        let body = self.parse_statement()?;
        self.context.in_iteration = prev_iter;
        self.expect_keyword(Keyword::While)?;
        self.expect_punct(Punct::OpenParen)?;
        let test = self.parse_expression()?;
        self.expect_punct(Punct::CloseParen)?;
        Ok(
            node::DoWhileStatement {
                test,
                body: Box::new(body),
            }
        )
    }

    fn parse_break_stmt(&mut self) -> Res<Option<node::Identifier>> {
        self.parse_optionally_labeled_statement(Keyword::Break)
    }

    fn parse_continue_stmt(&mut self) -> Res<Option<node::Identifier>> {
        self.parse_optionally_labeled_statement(Keyword::Continue)
    }

    fn parse_optionally_labeled_statement(&mut self, k: Keyword) -> Res<Option<node::Identifier>> {
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
        self.expect_keyword(Keyword::Debugger)?;
        self.consume_semicolon()?;
        Ok(node::Statement::Debugger)
    }

    fn parse_labelled_statement(&mut self) -> Res<node::Statement> {
        let expr = self.parse_expression()?;
        if expr.is_ident() && self.at_punct(Punct::Colon) {
            let _colon = self.next_item()?;
            let id = expr.as_ident()?;
            if !self.context.label_set.insert(format!("${}", id)) {
                return Err(Error::redecl(&id))
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
                let f = self.parse_function_decl(false)?;
                let expr = node::Expression::Function(f);
                node::Statement::Expr(expr)
            } else {
                self.parse_statement()?
            };
            Ok(
                node::Statement::Labeled(node::LabeledStatement {
                    label: id,
                    body: Box::new(body)
                })
            )
        } else {
            self.consume_semicolon()?;
            Ok(node::Statement::Expr(expr))
        }
    }

    fn parse_expression_statement(&mut self) -> Res<node::Expression> {
        let ret = self.parse_expression()?;
        self.consume_semicolon()?;
        Ok(ret)
    }

    fn parse_block(&mut self) -> Res<node::BlockStatement> {
        self.expect_punct(Punct::OpenBrace)?;
        let mut ret = vec![];
        loop {
            if self.at_punct(Punct::CloseBrace) {
                break;
            }
            match self.parse_statement_list_item_script()? {
                node::ScriptPart::Statement(s) => ret.push(s),
                _ => return self.error(&self.look_ahead, &[])
            }
        }
        self.expect_punct(Punct::CloseBrace)?;
        Ok(ret)
    }

    fn parse_lexical_decl(&mut self, in_for: bool) -> Res<node::Declaration> {
        let next = self.next_item()?;
        let kind = match &next.token {
            &Token::Keyword(ref k) => {
                match k {
                    Keyword::Let => node::VariableKind::Let,
                    Keyword::Const => node::VariableKind::Const,
                    _ => return self.error(&next, &["let", "const"]),
                }
            }
            _ => return self.error(&next, &["let", "const"]),
        };
        let decl = self.parse_binding_list(&kind, in_for)?;
        self.consume_semicolon()?;
        Ok(node::Declaration::Variable(kind, decl))
    }

    fn parse_binding_list(&mut self, kind: &node::VariableKind, in_for: bool) -> Res<Vec<node::VariableDecl>> {
        let mut ret = vec![self.parse_lexical_binding(kind, in_for)?];
        while self.at_punct(Punct::Comma) {
            let _comma = self.next_item()?;
            ret.push(self.parse_lexical_binding(kind, in_for)?)
        }
        Ok(ret)
    }

    fn parse_lexical_binding(&mut self, kind: &node::VariableKind, in_for: bool) -> Res<node::VariableDecl> {
        let is_var = match kind {
            node::VariableKind::Const
            | node::VariableKind::Let => false,
            _ => true,
        };
        let id = self.parse_var_ident(is_var)?;
        if self.context.strict && Self::is_restricted_word(&id) {
            return self.error(&self.look_ahead, &["not eval", "not arguments"])
        }
        let init = if kind == &node::VariableKind::Const {
            if !self.at_keyword(Keyword::In) && ! self.at_keyword(Keyword::Of) {
                if self.at_punct(Punct::Assign) {
                    Some(self.isolate_cover_grammar(&Self::parse_assignment_expr)?)
                } else {
                    return self.error(&self.look_ahead, &["="])
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
            init
        })
    }

    fn parse_function_decl(&mut self, opt_ident: bool) -> Res<node::Function> {
        let is_async = if self.matches_contextual_keyword("async") {
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
            body.push(self.parse_statement_list_item_script()?)
        }
        self.expect_punct(Punct::CloseBrace)?;
        self.context.label_set = prev_label;
        self.context.in_iteration = prev_iter;
        self.context.in_switch = prev_switch;
        self.context.in_function_body = prev_in_fn;
        Ok(body)
    }

    fn parse_class_decl(&mut self, opt_ident: bool) -> Res<node::Class> {
        let prev_strict = self.context.strict;
        self.context.strict = true;
        self.expect_keyword(Keyword::Class)?;
        let id = if opt_ident && !self.look_ahead.token.is_ident() {
            None
        } else {
            Some(self.parse_var_ident(false)?)
        };
        let super_class = if self.matches_contextual_keyword("extends") {
            let _ = self.next_item()?;
            Some(
                Box::new(
                    self.isolate_cover_grammar(&Self::parse_left_hand_side_expr_allow_call)?
                )
            )
        } else {
            None
        };
        let body = self.parse_class_body()?;

        self.context.strict = prev_strict;
        Ok(node::Class {
            id,
            super_class,
            body
        })
    }

    fn parse_class_body(&mut self) -> Res<Vec<node::Property>> {
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
        let mut has_ctor = has_ctor;
        let mut key: Option<node::PropertyKey> = None;
        let mut value: Option<node::PropertyValue> = None;
        let mut computed = false;
        let method = false;
        let mut is_static = false;
        let is_async = false;
        if self.at_punct(Punct::Asterisk) {
            let _ = self.next_item()?;
        } else {
            computed = self.at_punct(Punct::OpenBracket);
            let new_key = self.parse_object_property_key()?;
            if new_key.is_static() && (self.at_punct(Punct::Asterisk)
                || Self::qualified_prop_name(&self.look_ahead.token)) {
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
            if self.look_ahead.token.is_ident() && !self.context.has_line_term && self.matches_contextual_keyword("async") {
                if !self.look_ahead.token.matches_punct(Punct::Colon)
                    && !self.look_ahead.token.matches_punct(Punct::OpenParen)
                    && !self.look_ahead.token.matches_punct(Punct::Asterisk) {
                    return self.error(&self.look_ahead, &[":", "(", "*"]);
                }
            }
        }

        let mut kind: Option<node::PropertyKind> = None;
        let mut method = false;

        let look_ahead_prop_key = Self::qualified_prop_name(&self.look_ahead.token);
        if self.look_ahead.token.is_ident() {
            if self.matches_contextual_keyword("get") && look_ahead_prop_key {
                kind = Some(node::PropertyKind::Get);
                computed = self.at_punct(Punct::OpenBracket);
                key = Some(self.parse_object_property_key()?);
                self.context.allow_yield = false;
                value = Some(self.parse_getter_method()?);
            } else if self.matches_contextual_keyword("set") && look_ahead_prop_key {
                kind = Some(node::PropertyKind::Set);
                computed = self.at_punct(Punct::OpenBracket);
                key = Some(self.parse_object_property_key()?);
                value = Some(self.parse_setter_method()?);
            }
        } else if self.look_ahead.token.matches_punct(Punct::Asterisk) && look_ahead_prop_key {
            kind = Some(node::PropertyKind::Init);
            computed = self.at_punct(Punct::OpenBracket);
            key = Some(self.parse_object_property_key()?);
            value = Some(self.pares_generator_method()?);
            method = true;
        }

        if kind.is_none() && key.is_some() && self.at_punct(Punct::OpenParen) {
            kind = Some(node::PropertyKind::Init);
            value = Some(if is_async {
                self.parse_property_method_async_fn()?
            } else {
                self.parse_property_method_fn()?
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
            if is_static && key.matches_value("prototype") {
                return self.error(&self.look_ahead, &[]);
            }
            if !is_static && key.matches_value("constructor") {
                if kind == node::PropertyKind::Method || !method {
                    return self.error(&self.look_ahead, &[]);
                }
                if let Some(ref v) = value {
                    if v.is_generator() {
                        return self.error(&self.look_ahead, &[]);
                    }
                }
                if has_ctor {
                    return self.error(&self.look_ahead, &[])
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

        Ok((has_ctor, node::Property {
            key,
            value,
            kind,
            method,
            computed,
        }))

    }

    fn parse_property_method_async_fn(&mut self) -> Res<node::PropertyValue> {
        unimplemented!("parse_property_method_async_fn")
    }

    fn parse_property_method_fn(&mut self) -> Res<node::PropertyValue> {
        unimplemented!("parse_property_method_fn")
    }

    fn pares_generator_method(&mut self) -> Res<node::PropertyValue> {
        unimplemented!("pares_generator_method")
    }

    fn parse_getter_method(&mut self) -> Res<node::PropertyValue> {
        unimplemented!("parse_getter_method")
    }

    fn parse_setter_method(&mut self) -> Res<node::PropertyValue> {
        unimplemented!("parse_setter_method")
    }

    fn qualified_prop_name(tok: &Token) -> bool {
        if tok.is_keyword() || tok.matches_punct(Punct::OpenBracket) {
            true
        } else {
            false
        }
    }

    fn parse_object_property_key(&mut self) -> Res<node::PropertyKey> {
        unimplemented!("parse_object_property_key")
    }

    fn parse_primary_expression(&mut self) -> Res<node::Expression> {
        if self.look_ahead.token.is_ident() {
            if self.at_async_function() {

            }
        }
        // match self.look_ahead.token {
        //     Token::Ident(ref i) => {
        //         unimplemented!()
        //     },
        //     Token::Numeric(ref n) => {
        //         unimplemented!()
        //     },
        //     Token::String(ref s) => {
        //         if s.is_template_head() {

        //         }
        //         unimplemented!()
        //     },
        //     Token::Boolean(ref b) => {
        //         unimplemented!()
        //     },
        //     Token::Null => {
        //         unimplemented!()
        //     },
        //     Token::Punct(ref p) => {
        //         unimplemented!()
        //     }
        //     Token::Keyword(ref k) => {
        //         unimplemented!()
        //     },
        //     _ => {
        //         unimplemented!()
        //     }
        // }
        unimplemented!("parse_primary_expression")
    }

    fn parse_function_expr(&mut self) -> Res<node::Expression> {
        let is_async = self.matches_contextual_keyword("async");
        if is_async {
            let _async = self.next_item()?;
        }
        self.expect_keyword(Keyword::Function)?;
        let is_gen = self.at_punct(Punct::Asterisk);
        if is_gen {
            let _get = self.next_item()?;
        }
        let _prev_await = self.context.await;
        let _prev_yield = self.context.allow_yield;
        self.context.await = is_async;
        self.context.allow_yield = is_gen;
        let mut found_restricted = false;
        if !self.at_punct(Punct::OpenParen) {
            let item = self.look_ahead.clone();
            let id = if !self.context.strict && !is_gen && self.at_keyword(Keyword::Yield) {
                self.parse_ident_name()?
            } else {
                self.parse_var_ident(false)?
            };
            if self.context.strict {
                if item.token.is_restricted() {
                    return self.error(&item, &["not strict reserved"]);
                }
            } else {
                if item.token.is_restricted() {
                    found_restricted = true;
                } else if item.token.is_strict_reserved() {
                    found_restricted = true;
                }
            }
        }
        unimplemented!()
    }

    fn parse_ident_name(&mut self) -> Res<node::Identifier> {
        let ident = self.next_item()?;
        match ident.token {
            Token::Ident(i) => Ok(i),
            Token::Keyword(k) => Ok(k.to_string()),
            Token::Boolean(b) => Ok(b.into()),
            Token::Null => Ok("null".to_string()),
            _ => self.error(&ident, &["identifier name"])
        }
    }

    fn parse_var_ident(&mut self, is_var: bool) -> Res<node::Identifier> {
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
        } else if (self.context.is_module || self.context.await) && ident.token.matches_ident_str("await") {
            return self.error(&ident, &["variable identifier"]);
        }
        match &ident.token {
            &Token::Ident(ref i) => {
                Ok(i.to_string())
            },
            _ => self.error(&ident, &["variable identifier"]),
        }
    }

    fn parse_formal_params(&mut self) -> Res<FormalParams> {
        self.expect_punct(Punct::OpenParen)?;
        let mut args = vec![];
        let mut simple: bool = true;
        if !self.at_punct(Punct::CloseBrace) {
            while !self.look_ahead.token.is_eof() {

                if self.at_punct(Punct::CloseParen) {
                    break;
                }
                let (s, arg) = self.parse_formal_param(simple)?;
                simple = s;
                args.push(arg);
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
            first_restricted: None,
            simple,
        })
    }

    fn parse_formal_param(&mut self, simple: bool) -> Res<(bool, node::FunctionArg)> {
        let mut params: Vec<Item> = Vec::new();
        let param: node::FunctionArg = if self.at_punct(Punct::Spread) {
            node::FunctionArg::Pattern(self.parse_rest_element(&mut params)?)
        } else {
            node::FunctionArg::Pattern(self.parse_pattern_with_default(&mut params)?)
        };
        let simple = simple && param.is_simple();
        Ok((simple, param))
    }

    fn parse_rest_element(&mut self, params: &mut Vec<Item>) -> Res<node::Pattern> {
        self.expect_punct(Punct::Spread)?;
        let arg = self.parse_pattern(None, params)?;
        if self.at_punct(Punct::Assign) {
            return self.error(&self.look_ahead, &["not assignment"]);
        }
        if !self.at_punct(Punct::CloseParen) {
            return self.error(&self.look_ahead, &[")"]);
        }
        Ok(arg)
    }

    fn parse_pattern_with_default(&mut self, params: &mut Vec<Item>) -> Res<node::Pattern> {
        let ret = self.parse_pattern(None, params)?;
        if self.at_punct(Punct::Assign) {
            let _assign = self.next_item()?;
            let prev_yield = self.context.allow_yield;
            self.context.allow_yield = true;
            let right = self.isolate_cover_grammar(&Self::parse_assignment_expr)?;
            self.context.allow_yield = prev_yield;
            return Ok(node::Pattern::Assignment(node::AssignmentPattern {
                left: Box::new(ret),
                right: Box::new(right),
            }))
        }
        Ok(ret)
    }

    fn parse_pattern(&mut self, kind: Option<node::VariableKind>, params: &mut Vec<Item>) -> Res<node::Pattern> {
        if self.at_punct(Punct::OpenBracket) {
            self.parse_array_pattern()
        } else if self.at_punct(Punct::OpenBracket) {
            self.parse_object_pattern()
        } else {
            let is_var = if let Some(kind) = kind {
                match kind {
                    node::VariableKind::Const
                    | node::VariableKind::Let => {
                        if self.at_keyword(Keyword::Let) {
                            return self.error(&self.look_ahead, &["identifier"])
                        }
                        false
                    },
                    node::VariableKind::Var => true
                }
            } else {
                false
            };
            let ident = self.parse_var_ident(is_var)?;
            params.push(
                self.look_ahead.clone()
            );
            Ok(node::Pattern::Identifier(ident))
        }
    }

    fn parse_array_pattern(&mut self) -> Res<node::Pattern> {
        unimplemented!("parse_array_pattern")
    }

    fn parse_object_pattern(&mut self) -> Res<node::Pattern> {
        unimplemented!("parse_object_pattern")
    }

    fn parse_assignment_expr(&mut self) -> Res<node::Expression> {
        if !self.context.allow_yield && self.at_keyword(Keyword::Yield) {
            return self.parse_yield_expr()
        } else {
            let start = self.look_ahead.clone();
            let mut expr = self.parse_conditional_expr()?;
            let curr_line = self.get_item_position(&self.look_ahead).line;
            let start_line = self.get_item_position(&start).line;
            if start.token.matches_ident_str("async") && curr_line == start_line &&
                (self.look_ahead.token.is_ident() || self.at_keyword(Keyword::Yield)) {
                let arg = self.parse_primary_expression()?;
                let arg = Self::reinterpret_expr_as_pat(arg)?;
                let arg = node::FunctionArg::Pattern(arg);
                expr = node::Expression::ArrowParamPlaceHolder(vec![arg], true);
            }

            if expr.is_arrow_param_placeholder() || self.at_punct(Punct::FatArrow) {
                self.context.is_assignment_target = false;
                self.context.is_binding_element = false;
                let _is_async = expr.is_async();
                //TODO: see esprima::parser::reinterpretAsCoverFormalsList
            } else {
                if self.at_assign() {
                    if !self.context.is_assignment_target {
                        return self.error(&self.look_ahead, &["not assignment"])
                    }
                    if self.context.strict && expr.is_ident() {
                        if let node::Expression::Ident(ref i) = expr {
                                if Self::is_restricted_word(i) {
                                    return self.error(&self.look_ahead, &[&format!("not {}", i)]);
                                }
                                if Self::is_strict_reserved(i) {
                                    return self.error(&self.look_ahead, &[&format!("not {}", i)])
                                }
                        }
                    }
                    let left = if !self.at_punct(Punct::Assign) {
                        self.context.is_assignment_target = false;
                        self.context.is_binding_element = false;
                        node::AssignmentLeft::Expr(Box::new(expr))
                    } else {
                        let p = Self::reinterpret_expr_as_pat(expr)?;
                        node::AssignmentLeft::Pattern(p)
                        //TODO: reinterpret expression as pattern...
                    };
                    let item = self.next_item()?;
                    let op = match &item.token {
                        &Token::Punct(ref p) => {
                            if let Some(op) = node::AssignmentOperator::from_punct(p) {
                                op
                            } else {
                                return self.error(&item, &["=", "+=", "-=", "/=", "*=", "**=", "|=", "&=", "~=", "%=", "<<=", ">>=", ">>>="]);
                            }
                        },
                        _ => return self.error(&item, &["=", "+=", "-=", "/=", "*=", "**=", "|=", "&=", "~=", "%=", "<<=", ">>=", ">>>="]),
                    };
                    let right = self.isolate_cover_grammar(&Self::parse_assignment_expr)?;
                    self.context.first_covert_initialized_name_error = None;
                    return Ok(node::Expression::Assignment(node::AssignmentExpression {
                        operator: op,
                        left,
                        right: Box::new(right),
                    }))
                }
            }
            return Ok(expr)
        }
    }

    fn reinterpret_expr_as_pat(expr: node::Expression) -> Res<node::Pattern> {
        match expr {
            node::Expression::Array(a) => {
                let mut patts = vec![];
                for expr in a {
                    if let Some(e) = expr {
                        let p = Self::reinterpret_expr_as_pat(e)?;
                        patts.push(Some(p));
                    } else {
                        patts.push(None)
                    }
                }
                Ok(node::Pattern::Array(patts))
            },
            node::Expression::Spread(s) => Ok(node::Pattern::RestElement(Box::new(Self::reinterpret_expr_as_pat(*s)?))),
            node::Expression::Object(o) => {
                let mut patts = vec![];
                for expr in o {
                    match expr {
                        node::ObjectProperty::Property(p) => patts.push(node::ObjectPatternPart::Assignment(p)),
                        node::ObjectProperty::Spread(s) => {
                            let p = Self::reinterpret_expr_as_pat(*s)?;
                            patts.push(node::ObjectPatternPart::Rest(Box::new(p)));
                        },
                    }
                }
                Ok(node::Pattern::Object(patts))
            },
            node::Expression::Assignment(a) => {
                let left = match a.left {
                    node::AssignmentLeft::Pattern(p) => p,
                    node::AssignmentLeft::Expr(e) => Self::reinterpret_expr_as_pat(*e)?,
                };
                let ret = node::AssignmentPattern {
                    left: Box::new(left),
                    right: a.right,
                };
                Ok(node::Pattern::Assignment(ret))
            },
            node::Expression::Ident(i) => Ok(node::Pattern::Identifier(i.to_string())),
            _ => Err(Error::UnableToReinterpret("expression".into(), "pattern".into()))
        }
    }


    fn parse_yield_expr(&mut self) -> Res<node::Expression> {
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
            return Ok(node::Expression::Conditional(c))
        }
        Ok(expr)
    }

    fn parse_binary_expression(&mut self) -> Res<node::Expression> {
        let start = self.look_ahead.clone();
        let expr = self.inherit_cover_grammar(&Self::parse_exponentiation_expression)?;
        let precedence = self.bin_precedence(&self.look_ahead.token);
        if precedence > 0 {

        }
        Ok(expr)
    }

    fn parse_exponentiation_expression(&mut self) -> Res<node::Expression> {
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
        if self.at_punct(Punct::Plus) || self.at_punct(Punct::Minus) || self.at_punct(Punct::BitwiseNot) || self.at_punct(Punct::Not) ||
            self.at_keyword(Keyword::Delete) || self.at_keyword(Keyword::Void) || self.at_keyword(Keyword::TypeOf) {
            let next = self.next_item()?;
            let expr = self.inherit_cover_grammar(&Self::parse_unary_expression)?;
            match &expr {
                &node::Expression::Unary(ref u) => {
                    if self.context.strict && u.has_operator(&node::UnaryOperator::Delete) && u.has_ident_arg() {
                        return self.error(&self.look_ahead, &[])
                    }
                    self.context.is_assignment_target = false;
                    self.context.is_binding_element = false;
                }
                _ => return self.error(&self.look_ahead, &["unary expression"]),
            }
            Ok(expr)
        } else if self.context.await && self.matches_contextual_keyword("await") {
            self.parse_await_expr()
        } else {
            self.parse_update_expr()
        }
    }

    fn parse_await_expr(&mut self) -> Res<node::Expression> {
        let _await = self.next_item()?;
        let arg = self.parse_unary_expression()?;
        Ok(node::Expression::Await(Box::new(arg)))
    }

    fn parse_update_expr(&mut self) -> Res<node::Expression> {
        let start = self.look_ahead.clone();
        if self.at_punct(Punct::Increment) || self.at_punct(Punct::Decrement) {
            let next = self.next_item()?;
            let expr = self.inherit_cover_grammar(&Self::parse_unary_expression)?;
            if self.context.strict && expr.is_ident() {
                match &expr {
                    &node::Expression::Ident(ref i) => if Self::is_restricted_word(i) {
                        return self.error(&next, &[]);
                    },
                    _ => ()
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
                    return self.error(&start, &["++", "--"])
                },
                argument: Box::new(expr),
                prefix,
            };
            self.context.is_assignment_target = false;
            self.context.is_binding_element = false;
            Ok(node::Expression::Update(ret))
        } else {
            let expr = self.inherit_cover_grammar(&Self::parse_left_hand_side_expr_allow_call)?;
            if self.context.has_line_term && self.look_ahead.token.is_punct() {
                if self.at_punct(Punct::Increment) || self.at_punct(Punct::Decrement) {
                    if self.context.strict {
                        match &expr {
                            &node::Expression::Ident(ref i) => if Self::is_restricted_word(i) {
                                return self.error(&start, &[])
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
                            return self.error(&op, &["++", "--"])
                        },
                        argument: Box::new(expr),
                        prefix,
                    };
                    return Ok(node::Expression::Update(ret))
                }
            }
            Ok(expr)
        }
    }

    fn parse_left_hand_side_expr_allow_call(&mut self) -> Res<node::Expression> {
        let start_pos = self.get_item_position(&self.look_ahead);
        let is_async = self.matches_contextual_keyword("async");
        let prev_in = self.context.allow_in;
        self.context.allow_in = true;

        let mut expr = if self.at_keyword(Keyword::Super) && self.context.in_function_body {
            let _ = self.next_item()?;
            if !self.at_punct(Punct::OpenParen) && !self.at_punct(Punct::Period) && !self.at_punct(Punct::OpenBracket) {
                return self.error(&self.look_ahead, &["(", ".", "["])
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
                    computed: false
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
                unimplemented!("parse_left_hand_side_expr_allow_call -> while -> CallExpression");
                if async_arrow && self.at_punct(Punct::FatArrow) {
                    unimplemented!("arrow function def")
                }
            } else if self.at_punct(Punct::OpenBracket) {
                unimplemented!("parse_left_hand_side_expr_allow_call -> while -> OpenBracket")
            } else if self.look_ahead.token.is_template_head() {
                unimplemented!("parse_left_hand_side_expr_allow_call -> while -> TemplateHead")
            } else {
                break;
            }
        }
        self.context.allow_in = prev_in;
        Ok(expr)
    }

    fn parse_async_args(&mut self) -> Res<Vec<node::FunctionArg>> {
        unimplemented!("parse_async_args")
    }

    fn parse_args(&mut self) -> Res<Vec<node::FunctionArg>> {
        unimplemented!("parse_args")
    }

    fn parse_new_expr(&mut self) -> Res<node::Expression> {
        unimplemented!("parse_new_expr")
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
            },
            _ => 0
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
            &Punct::Equal
            | &Punct::NotEqual
            | &Punct::StrictEquals
            | &Punct::StrictNotEquals => 6,
            &Punct::GreaterThan
            | &Punct::LessThan
            | &Punct::LessThanEqual
            | &Punct::GreaterThanEqual => 7,
            &Punct::LeftShift
            | &Punct::RightShift
            | &Punct::UnsignedRightShift => 8,
            &Punct::Plus
            | &Punct::Minus => 9,
            &Punct::Asterisk
            | &Punct::ForwardSlash
            | &Punct::Modulo => 11,
            _ => 0,
        }
    }

    fn isolate_cover_grammar<T, F>(&mut self, parse_fn: &F) -> Res<T>
    where F: Fn(&mut Self) -> Res<T> {
        let prev_bind = self.context.is_binding_element;
        let prev_ass = self.context.is_assignment_target;
        let prev_first = self.context.first_covert_initialized_name_error.clone();
        self.context.is_binding_element = true;
        self.context.is_assignment_target = true;
        self.context.first_covert_initialized_name_error = None;
        let res = parse_fn(self)?;
        if let Some(ref e) = self.context.first_covert_initialized_name_error {
            return self.error(e, &[])
        }
        self.context.is_binding_element = prev_bind;
        self.context.is_assignment_target = prev_ass;
        self.context.first_covert_initialized_name_error = prev_first;
        Ok(res)
    }

    fn inherit_cover_grammar<T, F>(&mut self, parse_fn: &F) -> Res<T>
    where F: Fn(&mut Self) -> Res<T>
    {
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
        loop {
            if let Some(look_ahead) = self.scanner.next() {
                if look_ahead.token.is_comment() {
                    if self.config.comment {
                        self.comments.push(look_ahead);
                    }
                    continue;
                }
                let old_pos = self.get_item_position(&self.look_ahead);
                let new_pos = self.get_item_position(&look_ahead);
                self.context.has_line_term = old_pos.line != new_pos.line;
                if self.config.tokens {
                    self.tokens.push(look_ahead.clone())
                }
                let ret = replace(&mut self.look_ahead, look_ahead);
                return Ok(ret)
            } else {
                // if the next item is None, the iterator is spent
                // if the last token was EOF then we want to return that
                // and mark that we have found EOF, if we get here a second
                // time we want to return the ParseAfterEoF error
                if self.look_ahead.token.is_eof() {
                    if self.found_eof {
                        return Err(Error::ParseAfterEoF)
                    } else {
                        self.found_eof = true;
                        return Ok(self.look_ahead.clone())
                    }
                } else {
                    return Err(Error::UnexpectedEoF)
                }
            }
        }
    }

    fn expect_punct(&mut self, p: Punct) -> Res<()> {
        let next = self.next_item()?;
        if !next.token.matches_punct_str(&p.to_string()) {
            return self.error(&next, &[&format!("{:?}", p)])
        }
        Ok(())
    }

    fn expect_keyword(&mut self, k: Keyword) -> Res<()> {
        let next = self.next_item()?;
        if !next.token.matches_keyword_str(&k.to_string()) {
            return self.error(&next, &[&format!("{:?}", k)])
        }
        Ok(())
    }

    fn at_lexical_decl(&mut self) -> Res<bool> {
        let state = self.scanner.get_state();
        self.scanner.skip_comments();
        let next = self.next_item()?;
        self.scanner.set_state(state);
        Ok(
            next.token.is_ident()
            || next.token.matches_punct(Punct::OpenBracket)
            || next.token.matches_punct(Punct::OpenBrace)
            || next.token.matches_keyword(Keyword::Let)
            || next.token.matches_keyword(Keyword::Yield)
        )
    }

    fn at_punct(&self, p: Punct) -> bool {
        self.look_ahead.token.matches_punct(p)
    }

    fn at_keyword(&self, k: Keyword) -> bool {
        self.look_ahead.token.matches_keyword(k)
    }

    fn at_assign(&self) -> bool {
        self.look_ahead.token.matches_punct(Punct::Assign)
        || self.look_ahead.token.matches_punct(Punct::MultiplyAssign)
        || self.look_ahead.token.matches_punct(Punct::ExponentAssign)
        || self.look_ahead.token.matches_punct(Punct::DivideAssign)
        || self.look_ahead.token.matches_punct(Punct::ModuloAssign)
        || self.look_ahead.token.matches_punct(Punct::AddAssign)
        || self.look_ahead.token.matches_punct(Punct::SubtractAssign)
        || self.look_ahead.token.matches_punct(Punct::LeftShiftAssign)
        || self.look_ahead.token.matches_punct(Punct::RightShiftAssign)
        || self.look_ahead.token.matches_punct(Punct::UnsignedRightShiftAssign)
        || self.look_ahead.token.matches_punct(Punct::AddAssign)
        || self.look_ahead.token.matches_punct(Punct::BitwiseOrAssign)
        || self.look_ahead.token.matches_punct(Punct::BitwiseXOrAssign)
    }

    fn at_async_function(&mut self) -> bool {
        if self.matches_contextual_keyword("async") {
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
                return self.error(&self.look_ahead, &["eof", "}"])
            }
        }
        Ok(())
    }

    fn matches_contextual_keyword(&self, s: &str) -> bool {
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
        let (idx, line) = if let Some((idx, line)) = self.lines.iter().enumerate().find(|(_, l)| l.end + 1 >= item.span.start) {
            (idx, line)
        } else {
            panic!("Unable to determine item's line number {:?}", item);
        };
        let column = item.span.start.saturating_sub(line.start);
        Position {
            line: idx+1,
            column,
        }
    }

    fn error<T>(&self, item: &Item, expectation: &[&str]) -> Res<T> {
        let pos = self.get_item_position(item);
        let expectation = expectation.iter().enumerate().map(|(i, s)| {
            if i == expectation.len() - 1 && expectation.len() > 1 {
                format!("or `{}`", s)
            } else {
                format!("`{}`", s)
            }
        }).collect::<Vec<String>>().join(", ");
        Err(Error::UnexpectedToken(pos, format!("Expected {}; found {:?}", expectation, item.token)))
    }
}

#[allow(unused)]
struct FormalParams {
    simple: bool,
    params: Vec<node::FunctionArg>,
    strict: bool,
    first_restricted: Option<Item>,

}
#[allow(unused)]
struct CoverFormalListOptions {
    simple: bool,
    params: Vec<node::FunctionArg>,
    stricted: bool,
    first_restricted: Option<node::Expression>,
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn directive() {
        let js = "'use strict'";
        let mut p = Parser::new(js).unwrap();
        let script = p.parse_script().unwrap();
        println!("{:?}", script);
    }
    #[test]
    fn script() {
        let js = include_str!("../test.js");
        let mut p = Parser::new(js).unwrap();
        let script = p.parse_script().unwrap();
        println!("{:?}", script);
    }
}
