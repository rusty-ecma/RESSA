#![allow(dead_code)]
// ^^^^^^^^^^^^^^^^^
// most of this file is prep for a re-write

use crate::{Error, Position};
use resast::spanned::{
    expr::{ArrayExpr, AssignExpr, AssignLeft, Expr, ObjExpr, ObjProp, Prop, PropKey},
    pat::{ArrayPatPart, ObjPatPart, Pat, RestPat},
    stmt::LoopLeft,
    Ident, ListEntry, VarKind,
};
use std::{borrow::Cow, collections::HashSet};
type Res = Result<(), Error>;

pub fn is_simple_expr<'a>(expr: &Expr<'a>) -> bool {
    log::trace!("is_simple_expr {:?}", expr);
    match expr {
        Expr::This(_) => false,
        _ => true,
    }
}

pub fn is_simple_pat<'a>(pat: &Pat<'a>) -> bool {
    log::trace!("is_simple_pat {:?}", pat);
    match pat {
        Pat::Ident(ref id) => match &*id.slice.source {
            "this" => false,
            _ => true,
        },
        _ => true,
    }
}
pub fn check_lhs_expr<'a>(expr: &Expr<'a>, allow_let: bool, pos: Position, strict: bool) -> Res {
    match expr {
        Expr::Ident(ref id) => check_ident(id, allow_let, pos, strict),
        Expr::Obj(ref obj) => check_obj_expr(obj, allow_let, pos, strict),
        Expr::Assign(ref assign) => check_assign_expr(assign, allow_let, pos, strict),
        Expr::Array(ref a) => check_array_expr(a, allow_let, pos, strict),
        Expr::Spread(ref s) => check_lhs_expr(&s.expr, allow_let, pos, strict),
        _ => Err(Error::InvalidLHS(pos)),
    }
}

pub fn check_lhs_pat<'a>(pat: &Pat<'a>, allow_let: bool, pos: Position, strict: bool) -> Res {
    match pat {
        Pat::Ident(ref id) => check_ident(id, allow_let, pos, strict),
        Pat::Obj(ref obj) => check_obj_pat(&obj.props, allow_let, pos, strict),
        Pat::Assign(ref assign) => check_assign_pat(assign, allow_let, pos, strict),
        Pat::Array(ref a) => check_array_pat(&a.elements, allow_let, pos, strict),
    }
}

fn check_ident<'a>(id: &Ident<'a>, allow_let: bool, pos: Position, strict: bool) -> Res {
    if !allow_let && &id.slice.source == "let" {
        Err(Error::InvalidUseOfContextualKeyword(pos, "let".to_string()))
    } else if strict && is_strict_reserved(&id) || is_restricted_word(&id) {
        Err(Error::RestrictedIdent(pos))
    } else {
        Ok(())
    }
}

fn check_obj_expr<'a>(obj: &ObjExpr<'a>, allow_let: bool, pos: Position, strict: bool) -> Res {
    for part in &obj.props {
        check_obj_prop(&part.item, allow_let, pos, strict)?;
    }
    Ok(())
}

fn check_obj_pat<'a>(
    obj: &[ListEntry<'a, ObjPatPart<'a>>],
    allow_let: bool,
    pos: Position,
    strict: bool,
) -> Res {
    for part in obj {
        check_obj_pat_part(&part.item, allow_let, pos, strict)?;
    }
    Ok(())
}

fn check_assign_expr<'a>(
    assign: &AssignExpr<'a>,
    allow_let: bool,
    pos: Position,
    strict: bool,
) -> Res {
    match &assign.left {
        AssignLeft::Expr(ref e) => check_lhs_expr(e, allow_let, pos, strict),
        AssignLeft::Pat(ref p) => check_lhs_pat(p, allow_let, pos, strict),
    }
}

fn check_assign_pat<'a>(
    assign: &resast::spanned::pat::AssignPat<'a>,
    allow_let: bool,
    pos: Position,
    strict: bool,
) -> Res {
    check_lhs_pat(&*assign.left, allow_let, pos, strict)
}

fn check_array_expr<'a>(array: &ArrayExpr, allow_let: bool, pos: Position, strict: bool) -> Res {
    for part in &array.elements {
        if let Some(expr) = &part.item {
            check_lhs_expr(expr, allow_let, pos, strict)?;
        }
    }
    Ok(())
}

fn check_array_pat<'a>(
    array: &[ListEntry<'a, Option<ArrayPatPart<'a>>>],
    allow_let: bool,
    pos: Position,
    strict: bool,
) -> Res {
    for part in array {
        if let Some(part) = &part.item {
            match part {
                ArrayPatPart::Expr(expr) => check_lhs_expr(expr, allow_let, pos, strict)?,
                ArrayPatPart::Pat(pat) => check_lhs_pat(pat, allow_let, pos, strict)?,
                ArrayPatPart::Rest(RestPat { pat, .. }) => {
                    check_lhs_pat(pat, allow_let, pos, strict)?
                }
            }
        }
    }
    Ok(())
}

fn check_obj_prop<'a>(prop: &ObjProp<'a>, allow_let: bool, pos: Position, strict: bool) -> Res {
    match prop {
        ObjProp::Prop(ref p) => check_prop(p, allow_let, pos, strict),
        ObjProp::Spread(ref p) => check_lhs_expr(&p.expr, allow_let, pos, strict),
    }
}

fn check_obj_pat_part<'a>(
    part: &ObjPatPart<'a>,
    allow_let: bool,
    pos: Position,
    strict: bool,
) -> Res {
    match part {
        ObjPatPart::Assign(prop) => check_prop(prop, allow_let, pos, strict),
        _ => Err(Error::InvalidLHS(pos)),
    }
}

fn check_prop<'a>(prop: &Prop<'a>, allow_let: bool, pos: Position, strict: bool) -> Res {
    match prop {
        Prop::Init(value) => check_prop_key(&value.key.value, allow_let, pos, strict),
        _ => Err(Error::InvalidLHS(pos)),
    }
}

pub fn check_prop_key<'a>(key: &PropKey<'a>, allow_let: bool, pos: Position, strict: bool) -> Res {
    match &key {
        PropKey::Lit(_value) => Err(Error::InvalidLHS(pos)),
        PropKey::Expr(value) => check_lhs_expr(value, allow_let, pos, strict),
        PropKey::Pat(value) => check_lhs_pat(value, allow_let, pos, strict),
    }
}

pub fn check_loop_left<'a>(left: &LoopLeft<'a>, pos: Position) -> Res {
    let mut set = HashSet::new();
    match left {
        LoopLeft::Expr(expr) => check_loop_left_expr(expr, pos, &mut set),
        LoopLeft::Pat(pat) => check_loop_left_pat(pat, pos, &mut set),
        LoopLeft::Variable(kind, decls) => {
            if matches!(kind, VarKind::Var(_)) {
                Ok(())
            } else {
                check_loop_left_pat(&decls.id, pos, &mut set)
            }
        }
    }
}

pub fn check_loop_head_expr<'a>(left: &Expr<'a>, pos: Position) -> Res {
    log::debug!("check_loop_head_expr");
    let mut set = HashSet::new();
    match left {
        Expr::Array(ref a) => check_binding_array(&a.elements, pos, &mut set),
        Expr::Obj(ref o) => check_binding_obj(&o.props, pos, &mut set),
        Expr::Assign(ref a) => check_loop_left_expr(&*a.right, pos, &mut set),
        _ => Ok(()),
    }
}

fn check_binding_obj<'a>(
    obj: &[ListEntry<'a, ObjProp<'a>>],
    pos: Position,
    set: &mut HashSet<Cow<'a, str>>,
) -> Res {
    log::debug!("check_binding_obj");
    for part in obj {
        if let ObjProp::Prop(prop) = &part.item {
            match prop {
                Prop::Init(inner) => {
                    check_loop_left_prop_key(&inner.key.value, pos, set)?;
                }
                _ => return Err(Error::InvalidLHS(pos)),
            }
        }
    }
    Ok(())
}

pub fn check_binding_array<'a>(
    a: &[ListEntry<'a, Option<Expr<'a>>>],
    pos: Position,
    set: &mut HashSet<Cow<'a, str>>,
) -> Res {
    log::debug!("check_binding_array");
    for part in a {
        if let Some(part) = &part.item {
            if let Expr::Sequence(_) = part {
                return Err(Error::InvalidLHS(pos));
            }
            check_loop_left_expr(part, pos, set)?;
        }
    }
    Ok(())
}

fn check_loop_left_prop_key<'a>(
    prop: &PropKey<'a>,
    pos: Position,
    set: &mut HashSet<Cow<'a, str>>,
) -> Res {
    log::debug!("check_loop_left_prop_key");
    match prop {
        PropKey::Expr(expr) => check_loop_left_expr(expr, pos, set),
        PropKey::Pat(pat) => check_loop_left_pat(pat, pos, set),
        _ => Err(Error::InvalidLHS(pos)),
    }
}

fn check_loop_left_expr<'a>(
    expr: &Expr<'a>,
    pos: Position,
    set: &mut HashSet<Cow<'a, str>>,
) -> Res {
    log::debug!("check_loop_left_expr");
    match expr {
        Expr::Ident(ident) => {
            if !set.insert(ident.slice.source.clone()) {
                Err(Error::InvalidLHS(pos))
            } else {
                Ok(())
            }
        }
        _ => Ok(()),
    }
}

fn check_loop_left_pat<'a>(pat: &Pat<'a>, pos: Position, set: &mut HashSet<Cow<'a, str>>) -> Res {
    log::debug!("check_loop_left_pat");
    match pat {
        Pat::Ident(ident) => {
            if !set.insert(ident.slice.source.clone()) {
                Err(Error::InvalidLHS(pos))
            } else {
                Ok(())
            }
        }
        Pat::Array(a) => {
            for p in &a.elements {
                if let Some(p) = &p.item {
                    match p {
                        ArrayPatPart::Expr(expr) => check_loop_left_expr(expr, pos, set)?,
                        ArrayPatPart::Pat(pat) => check_loop_left_pat(pat, pos, set)?,
                        ArrayPatPart::Rest(RestPat { dots: _, pat }) => {
                            check_loop_left_pat(pat, pos, set)?
                        }
                    }
                }
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

#[inline]
fn is_restricted_word(word: &Ident) -> bool {
    &word.slice.source == "eval" || &word.slice.source == "arguments"
}
/// Check if this &str is in the list of reserved
/// words in the context of 'use strict'
#[inline]
fn is_strict_reserved(word: &Ident) -> bool {
    word.slice.source == "implements"
        || word.slice.source == "interface"
        || word.slice.source == "package"
        || word.slice.source == "private"
        || word.slice.source == "protected"
        || word.slice.source == "public"
        || word.slice.source == "static"
        || word.slice.source == "yield"
        || word.slice.source == "let"
}
