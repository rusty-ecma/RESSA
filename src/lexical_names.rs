use super::{Error, Position, Res};
use hash_chain::ChainMap;
use resast::prelude::*;
use std::{borrow::Cow, collections::HashMap};
type LexMap<'a> = ChainMap<Cow<'a, str>, Position>;
type VarMap<'a> = ChainMap<Cow<'a, str>, Vec<Position>>;
#[derive(Clone, Copy, Debug)]
pub enum DeclKind {
    Lex(bool),
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
    /// Hashmap of identifiers exported
    /// from this module and a flag for if they
    /// have a corresponding declaration
    module_exports: HashMap<Cow<'a, str>, bool>,
}

impl<'a> Default for DuplicateNameDetector<'a> {
    fn default() -> Self {
        Self {
            states: vec![Scope::default()],
            lex: LexMap::default(),
            var: VarMap::default(),
            func: LexMap::default(),
            first_lexes: Vec::new(),
            module_exports: HashMap::new(),
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
            DeclKind::Lex(is_module) => {
                self.check_var(i.clone(), pos)?;
                self.check_func(i.clone(), pos)?;
                if let Some(first) = self.first_lexes.last_mut() {
                    if first.is_none() {
                        *first = Some(i.clone());
                    }
                }
                if is_module {
                    self.module_exports
                        .entry(i.clone())
                        .and_modify(|e| *e = true)
                        .or_insert(true);
                }
                self.add_lex(i, pos)
            }
            DeclKind::Var(is_module) => {
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
                if is_module {
                    self.module_exports
                        .entry(i.clone())
                        .and_modify(|e| *e = true)
                        .or_insert(true);
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
        let _ = self.func.remove_child();
        if let Some(old_scope) = self.states.pop() {
            self.remove_var_child(old_scope);
        } else {
            panic!("attempted to pop state at bottom of stack")
        }
        self.first_lexes.pop();
    }
    fn remove_var_child(&mut self, scope: Scope) {
        if scope.is_func_top() {
            let _ = self.var.remove_child();
        } else {
            if let Some(old) = self.var.remove_child() {
                for (key, mut value) in old {
                    if self.var.last_has(&key) {
                        if let Some(list) = self.var.get_mut(&key) {
                            list.append(&mut value);
                        }
                    } else {
                        self.var.insert(key, value);
                    }
                }
            }
        }
    }

    pub fn add_module_export(
        &mut self,
        id: Cow<'a, str>,
        is_resolved: bool,
        pos: Position,
    ) -> Res<()> {
        if self.module_exports.contains_key(&id) {
            return Err(Error::DuplicateExport(pos, id.to_string()));
        }
        self.module_exports.insert(id, is_resolved);
        Ok(())
    }

    pub fn add_module_export_expr(&mut self, expr: &Expr<'a>, pos: Position) -> Res<()> {
        match expr {
            Expr::Ident(ref id) => self.add_module_export(id.name.clone(), false, pos),
            Expr::Assign(ref assign) => match &assign.left {
                AssignLeft::Expr(ref expr) => self.add_module_export_expr_(expr, pos, true),
                AssignLeft::Pat(ref pat) => self.add_module_export_pat_(pat, pos, true),
            },
            _ => Ok(()),
        }
    }
    fn add_module_export_expr_(
        &mut self,
        expr: &Expr<'a>,
        pos: Position,
        is_resolved: bool,
    ) -> Res<()> {
        match expr {
            Expr::Ident(id) => self.add_module_export(id.name.clone(), is_resolved, pos),
            _ => Ok(()),
        }
    }
    pub fn add_module_export_pat(&mut self, pat: &Pat<'a>, pos: Position) -> Res<()> {
        match pat {
            Pat::Ident(id) => self.add_module_export(id.name.clone(), false, pos),
            _ => Ok(()),
        }
    }
    fn add_module_export_pat_(
        &mut self,
        pat: &Pat<'a>,
        pos: Position,
        is_resolved: bool,
    ) -> Res<()> {
        match pat {
            Pat::Ident(id) => self.add_module_export(id.name.clone(), is_resolved, pos),
            _ => Ok(()),
        }
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
