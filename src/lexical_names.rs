use super::{Error, Position, Res};
use hash_chain::ChainMap;
use resast::prelude::*;
use std::borrow::Cow;
type LexMap<'a> = ChainMap<Cow<'a, str>, Position>;
type VarMap<'a> = ChainMap<Cow<'a, str>, Vec<Position>>;
#[derive(Clone, Copy, Debug)]
pub enum DeclKind {
    Lex,
    Var(bool),
    Func(bool),
    SimpleCatch,
}

pub struct DuplicateNameDetector<'a> {
    states: Vec<Scope>,
    lex: LexMap<'a>,
    var: VarMap<'a>,
    func: LexMap<'a>,
    first_lexes: Vec<Cow<'a, str>>,
}

impl<'a> Default for DuplicateNameDetector<'a> {
    fn default() -> Self {
        Self {
            states: vec![Scope::default()],
            lex: LexMap::default(),
            var: VarMap::default(),
            func: LexMap::default(),
            first_lexes: Vec::new(),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Scope {
    Top,
    FuncTop,
    SimpleCatch,
    Other,
}
impl Default for Scope {
    fn default() -> Self {
        Self::Top
    }
}

impl Scope {
    pub fn is_top(self) -> bool {
        self == Scope::Top
    }
    pub fn is_func_top(self) -> bool {
        self == Scope::FuncTop
    }
    pub fn is_simple_catch(self) -> bool {
        self == Scope::SimpleCatch
    }

    pub fn funcs_as_var(self, is_module: bool) -> bool {
        self.is_func_top() || !is_module && self.is_top()
    }
}

impl<'a> DuplicateNameDetector<'a> {
    pub fn declare(&mut self, i: &Cow<'a, str>, kind: DeclKind, pos: Position) -> Res<()> {
        log::trace!("DuplicateNameDetector::declare {} {:?}", i, kind);
        match kind {
            DeclKind::Lex => {
                self.check_var(i, pos)?;
                self.check_func(i, pos)?;
                if self.first_lexes.len() < self.states.len() {
                    self.first_lexes.push(i.clone());
                }
                self.add_lex(i, pos)
            }
            DeclKind::Var(is_module) => {
                for (idx, scope) in self.states.iter().enumerate().rev() {
                    let errored = if self.lex.has_at(idx, i) && !scope.is_simple_catch() {
                        if let Some(lex) = self.first_lexes.get(idx) {
                            i != lex
                        } else {
                            true
                        }
                    } else { !scope.funcs_as_var(is_module) && self.func.has_at(idx, i) };
                    if errored {
                        let ret = match self.lex.get_before(idx + 1, i) {
                            Some(p) => Err(Error::LexicalRedecl(
                                pos,
                                format!("previously declared lexical value at {}", p),
                            )),
                            None => Err(Error::LexicalRedecl(
                                pos,
                                format!("couldn't find {} before {}", i, idx),
                            )),
                        };
                        return ret;
                    }
                    if scope.is_func_top() || scope.is_top() {
                        break;
                    }
                }
                self.add_var(i, pos);
                Ok(())
            }
            DeclKind::Func(is_module) => {
                let state = if let Some(state) = self.states.last() {
                    *state
                } else {
                    Scope::default()
                };
                self.check_lex(i, pos)?;
                if !state.funcs_as_var(is_module) {
                    self.check_var(i, pos)?;
                }
                self.add_func(i, pos)
            }
            DeclKind::SimpleCatch => {
                self.lex.insert(i.clone(), pos);
                Ok(())
            }
        }
    }
    pub fn declare_pat(&mut self, pat: &Pat<'a>, kind: DeclKind, pos: Position) -> Res<()> {
        log::debug!("declare_pat {:?} {:?}", kind, pat);
        match pat {
            Pat::Ident(ref i) => {
                log::trace!("add_pat ident {:?}", i.name);
                self.declare(&i.name, kind, pos)
            }
            Pat::Array(ref a) => {
                for part in a {
                    if let Some(ref i) = part {
                        match i {
                            ArrayPatPart::Expr(ex) => self.declare_expr(ex, kind, pos)?,
                            ArrayPatPart::Pat(pat) => self.declare_pat(pat, kind, pos)?,
                        }
                    }
                }
                Ok(())
            }
            Pat::Assign(ref a) => self.declare_pat(&*a.left, kind, pos),
            Pat::Obj(ref o) => {
                for part in o {
                    match part {
                        ObjPatPart::Assign(ref prop) => match prop.key {
                            PropKey::Expr(ref ex) => self.declare_expr(ex, kind, pos)?,
                            PropKey::Pat(ref pat) => self.declare_pat(pat, kind, pos)?,
                            PropKey::Lit(ref _lit) => unreachable!("literal as identifier"),
                        },
                        ObjPatPart::Rest(ref pat) => self.declare_pat(pat, kind, pos)?,
                    }
                }
                Ok(())
            }
            Pat::RestElement(ref r) => self.declare_pat(&*r, kind, pos),
        }
    }
    fn declare_expr(&mut self, expr: &Expr<'a>, kind: DeclKind, pos: Position) -> Res<()> {
        if let Expr::Ident(ref i) = expr {
            log::trace!("add_expr ident {:?}", i.name);
            self.declare(&i.name, kind, pos)
        } else {
            Ok(())
        }
    }
    fn check_var(&mut self, i: &Cow<'a, str>, pos: Position) -> Res<()> {
        if self.var.last_has(i) {
            if let Some(poses) = self.var.get(i) {
                if let Some(old_pos) = poses.last() {
                    if *old_pos < pos {
                        return Err(Error::LexicalRedecl(
                            pos,
                            format!("{} was previously declared ({})", i, old_pos),
                        ));
                    }
                }
            }
        }
        Ok(())
    }

    fn check_func(&mut self, i: &Cow<'a, str>, pos: Position) -> Res<()> {
        check(&mut self.func, i, pos)
    }
    fn add_func(&mut self, i: &Cow<'a, str>, pos: Position) -> Res<()> {
        add(&mut self.func, i, pos)
    }
    fn check_lex(&mut self, i: &Cow<'a, str>, pos: Position) -> Res<()> {
        check(&mut self.lex, i, pos)
    }

    fn add_var(&mut self, i: &Cow<'a, str>, pos: Position) {
        if let Some(v) = self.var.get_mut(i) {
            v.push(pos);
        } else {
            self.var.insert(i.clone(), vec![pos]);
        }
    }

    fn add_lex(&mut self, i: &Cow<'a, str>, pos: Position) -> Res<()> {
        add(&mut self.lex, i, pos)?;
        Ok(())
    }
    pub fn new_child(&mut self, scope: Scope) {
        self.lex.new_child();
        self.var.new_child();
        self.func.new_child();
        self.states.push(scope)
    }
    pub fn remove_child(&mut self) {
        let _ = self.lex.remove_child();
        let _ = self.var.remove_child();
        let _ = self.func.remove_child();
        let _ = self.states.pop();
    }
}

/// check the last tier in the chain map for an identifier
fn check<'a>(map: &mut LexMap<'a>, i: &Cow<'a, str>, pos: Position) -> Res<()> {
    if map.last_has(i) {
        if let Some(old_pos) = map.get(i) {
            if *old_pos < pos {
                return Err(Error::LexicalRedecl(
                    pos,
                    format!("{} was previously declared ({})", i, old_pos),
                ));
            }
        }
    }
    Ok(())
}

pub fn add<'a>(map: &mut LexMap<'a>, i: &Cow<'a, str>, start: Position) -> Res<()> {
    if let Some(old_pos) = map.insert(i.clone(), start) {
        if old_pos < start {
            Err(Error::LexicalRedecl(
                start,
                format!("{} was previously declared ({})", i, old_pos),
            ))
        } else {
            Ok(())
        }
    } else {
        Ok(())
    }
}
