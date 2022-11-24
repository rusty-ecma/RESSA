use super::{Error, Position, Res};
use hash_chain::ChainMap;
use resast::spanned::{
    decl::ExportSpecifier,
    expr::{Expr, Lit, Prop, PropKey, PropValue},
    pat::{ArrayPatPart, ObjPatPart, Pat},
    Ident,
};

use std::{borrow::Cow, collections::HashSet};
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
    undefined_module_exports: HashSet<Cow<'a, str>>,
    exports: HashSet<Cow<'a, str>>,
}

impl<'a> Default for DuplicateNameDetector<'a> {
    fn default() -> Self {
        Self {
            states: vec![Scope::default()],
            lex: LexMap::default(),
            var: VarMap::default(),
            func: LexMap::default(),
            first_lexes: Vec::new(),
            undefined_module_exports: HashSet::new(),
            exports: HashSet::new(),
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
        log::trace!("DuplicateNameDetector::declare {} {:?} {:?}", i, kind, pos);
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
                    self.undefined_module_exports.remove(&i);
                }
                self.add_lex(i, pos)
            }
            DeclKind::Var(is_module) => {
                for (idx, scope) in self.states.iter().enumerate().rev() {
                    log::trace!("checking scope {}", idx);
                    let error = if self.lex.has_at(idx, &i) && !scope.is_simple_catch() {
                        if let Some(Some(lex)) = self.first_lexes.get(idx) {
                            &i != lex
                        } else {
                            true
                        }
                    } else {
                        log::trace!(
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
                    self.undefined_module_exports.remove(&i);
                }
                self.add_var(i.clone(), pos);
                Ok(())
            }
            DeclKind::Func(is_module) => {
                let state = if let Some(state) = self.states.last() {
                    log::trace!("last state found {:?}", state);
                    *state
                } else {
                    Scope::default()
                };
                self.check_lex(i.clone(), pos)?;
                log::trace!("not in lexical decls");
                if !state.funcs_as_var(is_module) {
                    log::trace!("state does not indicate functions should be treated as vars");
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
        log::trace!("declare_pat {:?} {:?} {:?}", pat, kind, pos);
        match pat {
            Pat::Ident(ref i) => {
                log::trace!("add_pat ident {:?}", i.slice.source);
                self.declare(i.slice.source.clone(), kind, pos)
            }
            Pat::Array(ref a) => {
                for part in &a.elements {
                    if let Some(ref i) = part.item {
                        match i {
                            ArrayPatPart::Expr(ex) => self.declare_expr(ex, kind, pos)?,
                            ArrayPatPart::Pat(pat) => self.declare_pat(pat, kind, pos)?,
                            ArrayPatPart::Rest(rest) => self.declare_pat(&rest.pat, kind, pos)?,
                        }
                    }
                }
                Ok(())
            }
            Pat::Assign(ref a) => self.declare_pat(&*a.left, kind, pos),
            Pat::Obj(ref o) => {
                for part in &o.props {
                    match &part.item {
                        ObjPatPart::Assign(prop) => self.declare_prop(prop, kind, pos)?,
                        ObjPatPart::Rest(pat) => self.declare_pat(&pat.pat, kind, pos)?,
                    }
                }
                Ok(())
            }
        }
    }

    fn declare_prop(&mut self, prop: &Prop<'a>, kind: DeclKind, pos: Position) -> Res<()> {
        log::trace!("declare_prop {:?} {:?} {:?}", prop, kind, pos);
        match &prop {
            Prop::Init(prop) => match &prop.value {
                Some(value) => match value {
                    PropValue::Expr(expr) => self.declare_expr(expr, kind, pos),
                    PropValue::Pat(pat) => self.declare_pat(pat, kind, pos),
                    PropValue::Method(_) => Ok(()),
                },
                None => match &prop.key.value {
                    PropKey::Lit(lit) => self.declare_literal_ident(lit, kind, pos),
                    PropKey::Expr(expr) => self.declare_expr(expr, kind, pos),
                    PropKey::Pat(pat) => self.declare_pat(pat, kind, pos),
                },
            },
            _ => Ok(()),
        }
    }
    fn declare_literal_ident(&mut self, lit: &Lit<'a>, kind: DeclKind, pos: Position) -> Res<()> {
        log::trace!("declare_literal_ident {:?} {:?} {:?}", lit, kind, pos);
        match lit {
            Lit::String(s) => self.declare(s.content.source.clone(), kind, pos),
            _ => Err(Error::RestrictedIdent(pos)),
        }
    }
    pub fn declare_expr(&mut self, expr: &Expr<'a>, kind: DeclKind, pos: Position) -> Res<()> {
        log::trace!("declare_expr {:?} {:?} {:?}", expr, kind, pos);
        if let Expr::Ident(ref i) = expr {
            log::trace!("add_expr ident {:?}", i.slice.source);
            self.declare(i.slice.source.clone(), kind, pos)
        } else {
            Ok(())
        }
    }
    fn check_var(&mut self, i: Cow<'a, str>, pos: Position) -> Res<()> {
        log::trace!("check_var {:?} {:?}", i, pos);
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
        log::trace!("check_func {:?} {:?}", i, pos);
        check(&mut self.func, i, pos)
    }
    fn add_func(&mut self, i: Cow<'a, str>, pos: Position) -> Res<()> {
        log::trace!("add_func {:?} {:?}", i, pos);
        let _ = self.func.insert(i, pos);
        Ok(())
    }
    fn check_lex(&mut self, i: Cow<'a, str>, pos: Position) -> Res<()> {
        log::trace!("check_lex {:?} {:?}", i, pos);
        check(&mut self.lex, i, pos)
    }

    fn add_var(&mut self, i: Cow<'a, str>, pos: Position) {
        log::trace!("add_var {:?} {:?}", i, pos);
        if let Some(v) = self.var.get_mut(&i) {
            v.push(pos);
        } else {
            self.var.insert(i.clone(), vec![pos]);
        }
    }

    fn add_lex(&mut self, i: Cow<'a, str>, pos: Position) -> Res<()> {
        log::trace!("add_lex {:?} {:?}", i, pos);
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
        } else if let Some(old) = self.var.remove_child() {
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

    pub fn add_export_spec(&mut self, spec: &ExportSpecifier<'a>, pos: Position) -> Res<()> {
        log::trace!("add_export_spec {:?} {:?}", spec, pos);
        self.add_export_ident(&spec.local.slice.source, pos)?;
        self.undefined_module_export_guard(spec.local.slice.source.clone());
        Ok(())
    }

    pub fn removed_undefined_export(&mut self, id: &Ident<'a>) {
        log::trace!("removed_undefined_export {:?}", id);
        self.undefined_module_exports.remove(&id.slice.source);
    }

    pub fn add_export_ident(&mut self, id: &Cow<'a, str>, pos: Position) -> Res<()> {
        log::trace!("add_export_ident {:?} {:?}", id, pos);
        if !self.exports.insert(id.clone()) {
            Err(Error::DuplicateExport(pos, id.to_string()))
        } else {
            Ok(())
        }
    }

    pub fn undefined_module_export_guard(&mut self, id: Cow<'a, str>) {
        log::trace!("add_module_export: {}", id);
        if !self.var.has_at(0, &id) && !self.lex.has_at(0, &id) {
            self.undefined_module_exports.insert(id);
        }
    }

    pub fn has_undefined_exports(&self) -> bool {
        !self.undefined_module_exports.is_empty()
    }

    pub fn get_undefined_exports(&self) -> Vec<String> {
        self.undefined_module_exports
            .iter()
            .map(|n| n.to_string())
            .collect()
    }
}

/// check the last tier in the chain map for an identifier
fn check<'a>(map: &mut LexMap<'a>, i: Cow<'a, str>, pos: Position) -> Res<()> {
    log::trace!("check {:?} {:?} {:?}", map, i, pos);
    log::trace!("checking for {}", i);
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
    log::trace!("add {:?} {:?} {:?}", map, i, start);
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
