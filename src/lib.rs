extern crate ress;

use ress::{Scanner, Item, Keyword, Token, Punct};
pub mod node;
mod error;
use node::{Position};
use error::Error;
use std::mem::replace;
struct Config {
    range: bool,
    loc: bool,
    source: Option<String>,
    tokens: bool,
    comment: bool,
    tolerant: bool,
}

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
    label_set: String,
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
            label_set: String::new(),
            strict: false,
            has_line_term: false,
        }
    }
}

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
type ExpRes = Result<(), Error>;

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

    fn parse_primary_expression(&mut self) -> Res<node::Expression> {
        if self.look_ahead.token.is_ident() {
            if self.matches_async_function() {

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
        unimplemented!()
    }

    fn parse_function_expr(&mut self) -> Res<node::Expression> {
        let is_async = self.matches_contextual_keyword("async");
        if is_async {
            let _async = self.next_item()?;
        }
        self.expect_keyword(Keyword::Function)?;
        let is_gen = self.matches_punct(Punct::Asterisk);
        if is_gen {
            let _get = self.next_item()?;
        }
        let prev_await = self.context.await;
        let prev_yield = self.context.allow_yield;
        self.context.await = is_async;
        self.context.allow_yield = is_gen;
        let mut found_restricted = false;
        if !self.matches_punct(Punct::OpenParen) {
            let item = self.look_ahead.clone();
            let id = if !self.context.strict && !is_gen && self.matches_keyword(Keyword::Yield) {
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

    fn parse_formal_params(&mut self) -> Res<()> {
        self.expect_punct(Punct::OpenParen)?;
        if !self.matches_punct(Punct::CloseBrace) {
            while !self.look_ahead.token.is_eof() {

                if self.matches_punct(Punct::CloseParen) {
                    break;
                }
                self.expect_punct(Punct::Comma)?;
                if self.matches_punct(Punct::CloseParen) {
                    break;
                }
            }
        }
        self.expect_punct(Punct::CloseParen)?;
        Ok(())
    }

    fn parse_formal_param(&mut self, simple: bool) -> Res<(bool, node::Pattern)> {
        let mut params: Vec<Item> = Vec::new();
        let param: node::Pattern = if self.matches_punct(Punct::Spread) {
            self.parse_rest_element(&mut params)?
        } else {
            unimplemented!()
        };
        let simple = simple && match &param {
            &node::Pattern::Identifier(_) => true,
            _ => false,
        };
        Ok((simple, param))
    }

    fn parse_rest_element(&mut self, params: &mut Vec<Item>) -> Res<node::Pattern> {
        self.expect_punct(Punct::Spread)?;
        let arg = self.parse_pattern(None, params)?;
        if self.matches_punct(Punct::Assign) {
            return self.error(&self.look_ahead, &["not assignment"]);
        }
        if !self.matches_punct(Punct::CloseParen) {
            return self.error(&self.look_ahead, &[")"]);
        }
        Ok(arg)
    }

    fn parse_pattern_with_default(&mut self, params: &mut Vec<Item>) -> Res<node::Pattern> {
        let ret = self.parse_pattern(None, params)?;
        if self.matches_punct(Punct::Assign) {
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
        if self.matches_punct(Punct::OpenBracket) {
            self.parse_array_pattern()
        } else if self.matches_punct(Punct::OpenBracket) {
            self.parse_object_pattern()
        } else {
            let is_var = if let Some(kind) = kind {
                match kind {
                    node::VariableKind::Const
                    | node::VariableKind::Let => {
                        if self.matches_keyword(Keyword::Let) {
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
            params.push(self.look_ahead.clone());
            Ok(node::Pattern::Identifier(ident))
        }
    }

    fn parse_array_pattern(&mut self) -> Res<node::Pattern> {
        unimplemented!()
    }

    fn parse_object_pattern(&mut self) -> Res<node::Pattern> {
        unimplemented!()
    }

    fn parse_assignment_expr(&mut self) -> Res<node::Expression> {
        if !self.context.allow_yield && self.matches_keyword(Keyword::Yield) {
            return self.parse_yield_expr()
        } else {
            let start = self.look_ahead.clone();
            let mut expr = self.parse_conditional_expr()?;
            let curr_line = self.get_item_position(&self.look_ahead).line;
            let start_line = self.get_item_position(&start).line;
            if start.token.matches_ident_str("async") && curr_line == start_line &&
                (self.look_ahead.token.is_ident() || self.matches_keyword(Keyword::Yield)) {
                let arg = self.parse_primary_expression()?;
                let arg = Self::reinterpret_expr_as_pat(arg)?;
                let arg = node::FunctionArg::Pattern(arg);
                expr = node::Expression::ArrowParamPlaceHolder(vec![arg], true);
            }

            if expr.is_arrow_param_placeholder() || self.matches_punct(Punct::FatArrow) {
                self.context.is_assignment_target = false;
                self.context.is_binding_element = false;
                let _is_async = expr.is_async();
                //TOOD: see esprima::parser::reinterpretAsCoverFormalsList
            } else {
                if self.matches_assign() {
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
                    let left = if !self.matches_punct(Punct::Assign) {
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
            delegate = self.matches_punct(Punct::Asterisk);
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
        if self.matches_punct(Punct::QuestionMark) {
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
        let prec = self.bin_precedence(&self.look_ahead.token);
        if prec > 0 {

        }
        Ok(expr)
    }

    fn parse_exponentiation_expression(&mut self) -> Res<node::Expression> {
        let start = self.look_ahead.clone();
        let expr = self.inherit_cover_grammar(&Self::parse_unary_expression)?;
        if expr.is_unary() && self.matches_punct(Punct::Exponent) {
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
        if self.matches_punct(Punct::Plus) || self.matches_punct(Punct::Minus) || self.matches_punct(Punct::BitwiseNot) || self.matches_punct(Punct::Not) ||
            self.matches_keyword(Keyword::Delete) || self.matches_keyword(Keyword::Void) || self.matches_keyword(Keyword::TypeOf) {
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
        if self.matches_punct(Punct::Increment) || self.matches_punct(Punct::Decrement) {
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
                if self.matches_punct(Punct::Increment) || self.matches_punct(Punct::Decrement) {
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
        unimplemented!()
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

    fn expect_punct(&mut self, p: Punct) -> ExpRes {
        let next = self.next_item()?;
        if !next.token.matches_punct_str(&p.to_string()) {
            return self.error(&next, &[&format!("{:?}", p)])
        }
        Ok(())
    }

    fn expect_keyword(&mut self, k: Keyword) -> ExpRes {
        let next = self.next_item()?;
        if !next.token.matches_keyword_str(&k.to_string()) {
            return self.error(&next, &[&format!("{:?}", k)])
        }
        Ok(())
    }

    fn matches_punct(&self, p: Punct) -> bool {
        self.look_ahead.token.matches_punct(p)
    }

    fn matches_keyword(&self, k: Keyword) -> bool {
        self.look_ahead.token.matches_keyword(k)
    }

    fn matches_assign(&self) -> bool {
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

    fn matches_async_function(&mut self) -> bool {
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
        if self.matches_punct(Punct::SemiColon) {
            let _semi = self.next_item()?;
        } else if !self.context.has_line_term {
            if !self.look_ahead.token.is_eof() && !self.matches_punct(Punct::CloseBrace) {
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

struct CoverFormalListOptions {
    simple: bool,
    params: Vec<node::FunctionArg>,
    stricted: bool,
    first_restricted: Option<node::Expression>,
}

#[cfg(test)]
mod tests {
    use super::*;

}
