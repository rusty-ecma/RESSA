use super::{Error, Position, Res};
use hash_chain::ChainMap;
use resast::prelude::*;
use std::borrow::Cow;
type LexMap<'a> = ChainMap<Cow<'a, str>, Position>;
#[derive(Clone, Copy, Debug)]
pub enum DeclKind {
    Lex,
    Var(bool),
    Func(bool),
}
#[derive(Default)]
pub struct DuplicateNameDetector<'a> {
    states: Vec<ScopeState>,
    lex: LexMap<'a>,
    var: LexMap<'a>,
    func: LexMap<'a>,
}
#[derive(Clone, Copy)]
pub struct ScopeState {
    top: bool,
    top_of_func: bool,
}

impl Default for ScopeState {
    fn default() -> Self {
        Self {
            top: true,
            top_of_func: false,
        }
    }
}

impl<'a> DuplicateNameDetector<'a> {
    pub fn declare(&mut self, i: &Cow<'a, str>, kind: DeclKind, pos: Position) -> Res<()> {
        log::trace!("DuplicateNameDetector::declare {} {:?}", i, kind);
        match kind {
            DeclKind::Lex => {
                self.check_var(i, pos)?;
                self.check_func(i, pos)?;
                self.add_lex(i, pos)
            }
            DeclKind::Var(is_module) => {
                check_exhaustive(&self.lex, i, pos)?;
                self.check_func_exhaustive(i, pos, is_module)?;
                self.add_var(i, pos)
            }
            DeclKind::Func(is_module) => {
                let state = if let Some(state) = self.states.last() {
                    *state
                } else {
                    ScopeState::default()
                };
                self.check_lex(i, pos)?;
                if state.top_of_func || (!is_module && state.top) {
                    self.check_var(i, pos)?;
                }
                self.add_func(i, pos)
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
        check(&mut self.var, i, pos)
    }
    fn add_var(&mut self, i: &Cow<'a, str>, pos: Position) -> Res<()> {
        add(&mut self.var, i, pos)
    }

    fn check_func(&mut self, i: &Cow<'a, str>, pos: Position) -> Res<()> {
        check(&mut self.func, i, pos)
    }
    fn check_func_exhaustive(
        &mut self,
        i: &Cow<'a, str>,
        pos: Position,
        is_module: bool,
    ) -> Res<()> {
        let mut scopes = Vec::new();
        let mut ret = Ok(());
        for state in self.states.iter().rev() {
            if let Some(scope) = self.func.pop() {
                if let Some(old_pos) = scope.get(i) {
                    if !(state.top_of_func && (!is_module && state.top)) {
                        ret = Err(Error::LexicalRedecl(
                            pos,
                            format!("{} was previously declared ({})", i, old_pos),
                        ));
                    }
                    break;
                }
                scopes.push(scope);
            } else {
                break;
            }
        }
        for scope in scopes {
            self.func.new_child_with(scope);
        }
        ret
    }
    fn add_func(&mut self, i: &Cow<'a, str>, pos: Position) -> Res<()> {
        add(&mut self.func, i, pos)
    }
    fn check_lex(&mut self, i: &Cow<'a, str>, pos: Position) -> Res<()> {
        check(&mut self.lex, i, pos)
    }
    fn add_lex(&mut self, i: &Cow<'a, str>, pos: Position) -> Res<()> {
        add(&mut self.lex, i, pos)
    }
    pub fn new_child(&mut self, top_of_func: bool) {
        self.lex.new_child();
        self.var.new_child();
        self.func.new_child();
        self.states.push(ScopeState {
            top: false,
            top_of_func,
        })
    }
    pub fn remove_child(&mut self) {
        let _ = self.lex.pop();
        let _ = self.var.pop();
        let _ = self.func.pop();
        let _ = self.states.pop();
    }
}

/// check the last tier in the chain map for an identifier
fn check<'a>(map: &mut LexMap<'a>, i: &Cow<'a, str>, pos: Position) -> Res<()> {
    let child = if let Some(ch) = map.pop() {
        ch
    } else {
        return Err(Error::LexicalRedecl(
            pos,
            "Invalid lexical map state".to_string(),
        ));
    };
    let ret = if let Some(old_pos) = child.get(i) {
        Err(Error::LexicalRedecl(
            pos,
            format!("{} was previously declared ({})", i, old_pos),
        ))
    } else {
        Ok(())
    };
    map.new_child_with(child);
    ret
}
/// check the full chain map for an identifier
fn check_exhaustive<'a>(map: &LexMap<'a>, i: &Cow<'a, str>, pos: Position) -> Res<()> {
    if let Some(old_pos) = map.get(i) {
        Err(Error::LexicalRedecl(
            pos,
            format!("{} was previously declared ({})", i, old_pos),
        ))
    } else {
        Ok(())
    }
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
