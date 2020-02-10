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
    pub states: Vec<Scope>,
    lex: LexMap<'a>,
    var: VarMap<'a>,
    func: LexMap<'a>,
    first_lexes: Vec<Option<Cow<'a, str>>>,
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

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Scope {
    Top,
    FuncTop,
    SimpleCatch,
    For,
    Catch,
    Switch,
    Block,
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
    pub fn current_funcs_as_var(&self, is_module: bool) -> bool {
        if let Some(&scope) = self.states.last() {
            scope.funcs_as_var(is_module)
        } else {
            false
        }
    }
    pub fn last_scope(&self) -> Option<Scope> {
        self.states.last().copied()
    }
    pub fn declare(&mut self, i: Cow<'a, str>, kind: DeclKind, pos: Position) -> Res<()> {
        log::trace!("DuplicateNameDetector::declare {} {:?}", i, kind);
        match kind {
            DeclKind::Lex => {
                self.check_var(i.clone(), pos)?;
                self.check_func(i.clone(), pos)?;
                if let Some(first) = self.first_lexes.last_mut() {
                    if first.is_none() {
                        *first = Some(i.clone());
                    }
                }
                self.add_lex(i, pos)
            }
            DeclKind::Var(is_module) => {
                //if (scope.lexical.indexOf(name) > -1 && !((scope.flags & SCOPE_SIMPLE_CATCH) && scope.lexical[0] === name) ||
                // !this.treatFunctionsAsVarInScope(scope) && scope.functions.indexOf(name) > -1) {
                //     redeclared = true
                //     break
                // }
                for (idx, scope) in self.states.iter().enumerate().rev() {
                    trace!("checking scope {}", idx);
                    let error = if self.lex.has_at(idx, &i) && !scope.is_simple_catch() {
                        if let Some(Some(lex)) = self.first_lexes.get(idx) {
                            &i != lex
                        } else {
                            true
                        }
                    } else {
                        trace!(
                            "looking for dupe in {} funcs_as_var: {}, funcs_has {}",
                            idx,
                            scope.funcs_as_var(is_module),
                            self.func.has_at(idx, &i)
                        );
                        !scope.funcs_as_var(is_module) && self.func.has_at(idx, &i)
                    };
                    if error {
                        let ret = match self.lex.get_before(idx + 1, &i) {
                            Some(orig) => Err(Error::LexicalRedecl(*orig, pos, i.to_string())),
                            None => Err(Error::OperationError(
                                pos,
                                format!("lexical map couldn't find {} before {}", i, idx),
                            )),
                        };
                        return ret;
                    }
                    if scope.is_func_top() || scope.is_top() {
                        break;
                    }
                }
                self.add_var(i.clone(), pos);
                Ok(())
            }
            DeclKind::Func(is_module) => {
                let state = if let Some(state) = self.states.last() {
                    trace!("last state found {:?}", state);
                    *state
                } else {
                    Scope::default()
                };
                self.check_lex(i.clone(), pos)?;
                trace!("not in lexical decls");
                if !state.funcs_as_var(is_module) {
                    trace!("state does not indicate functions should be treated as vars");
                    self.check_var(i.clone(), pos)?;
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
                self.declare(i.name.clone(), kind, pos)
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
                        ObjPatPart::Assign(ref prop) => self.declare_prop(prop, kind, pos)?,
                        ObjPatPart::Rest(ref pat) => self.declare_pat(pat, kind, pos)?,
                    }
                }
                Ok(())
            }
            Pat::RestElement(ref r) => self.declare_pat(&*r, kind, pos),
        }
    }

    fn declare_prop(&mut self, prop: &Prop<'a>, kind: DeclKind, pos: Position) -> Res<()> {
        match &prop.value {
            PropValue::Expr(expr) => self.declare_expr(expr, kind, pos),
            PropValue::Pat(pat) => self.declare_pat(pat, kind, pos),
            PropValue::None => match &prop.key {
                PropKey::Lit(lit) => self.declare_literal_ident(lit, kind, pos),
                PropKey::Expr(expr) => self.declare_expr(expr, kind, pos),
                PropKey::Pat(pat) => self.declare_pat(pat, kind, pos),
            },
        }
    }
    fn declare_literal_ident(&mut self, lit: &Lit<'a>, kind: DeclKind, pos: Position) -> Res<()> {
        match lit {
            Lit::String(s) => match s {
                StringLit::Double(id) | StringLit::Single(id) => {
                    self.declare(id.clone(), kind, pos)
                }
            },
            _ => Err(Error::RestrictedIdent(pos)),
        }
    }
    fn declare_expr(&mut self, expr: &Expr<'a>, kind: DeclKind, pos: Position) -> Res<()> {
        if let Expr::Ident(ref i) = expr {
            log::trace!("add_expr ident {:?}", i.name);
            self.declare(i.name.clone(), kind, pos)
        } else {
            Ok(())
        }
    }
    fn check_var(&mut self, i: Cow<'a, str>, pos: Position) -> Res<()> {
        if self.var.last_has(&i) {
            if let Some(poses) = self.var.get(&i) {
                if let Some(old_pos) = poses.last() {
                    if *old_pos < pos {
                        return Err(Error::LexicalRedecl(*old_pos, pos, i.to_string()));
                    }
                }
            }
        }
        Ok(())
    }

    fn check_func(&mut self, i: Cow<'a, str>, pos: Position) -> Res<()> {
        check(&mut self.func, i, pos)
    }
    fn add_func(&mut self, i: Cow<'a, str>, pos: Position) -> Res<()> {
        let _ = self.func.insert(i, pos);
        Ok(())
    }
    fn check_lex(&mut self, i: Cow<'a, str>, pos: Position) -> Res<()> {
        check(&mut self.lex, i, pos)
    }

    fn add_var(&mut self, i: Cow<'a, str>, pos: Position) {
        if let Some(v) = self.var.get_mut(&i) {
            v.push(pos);
        } else {
            self.var.insert(i.clone(), vec![pos]);
        }
    }

    fn add_lex(&mut self, i: Cow<'a, str>, pos: Position) -> Res<()> {
        add(&mut self.lex, i, pos)?;
        Ok(())
    }
    pub fn new_child(&mut self, scope: Scope) {
        self.lex.new_child();
        self.var.new_child();
        self.func.new_child();
        self.states.push(scope);
        self.first_lexes.push(None);
    }
    pub fn remove_child(&mut self) {
        let _ = self.lex.remove_child();
        let _ = self.var.remove_child();
        let _ = self.func.remove_child();
        if self.states.len() == 1 {
            panic!("attempted to pop state at bottom of stack")
        }
        let _ = self.states.pop();
        self.first_lexes.pop();
    }
}

/// check the last tier in the chain map for an identifier
fn check<'a>(map: &mut LexMap<'a>, i: Cow<'a, str>, pos: Position) -> Res<()> {
    if map.last_has(&i) {
        if let Some(old_pos) = map.get(&i) {
            if *old_pos < pos {
                return Err(Error::LexicalRedecl(*old_pos, pos, i.to_string()));
            }
        }
    }
    Ok(())
}

pub fn add<'a>(map: &mut LexMap<'a>, i: Cow<'a, str>, start: Position) -> Res<()> {
    if let Some(old_pos) = map.insert(i.clone(), start) {
        if old_pos < start {
            Err(Error::LexicalRedecl(old_pos, start, i.to_string()))
        } else {
            Ok(())
        }
    } else {
        Ok(())
    }
}
