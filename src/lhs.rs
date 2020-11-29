#![allow(dead_code)]
// ^^^^^^^^^^^^^^^^^
// most of this file is prep for a re-write

use crate::{Error, Position};
use resast::prelude::*;
use std::{borrow::Cow, collections::HashSet};
type Res = Result<(), Error>;

pub fn is_simple_expr<'a>(expr: &Expr<'a>) -> bool {
    trace!("is_simple_expr {:?}", expr);
    !matches!(expr, Expr::This)
}

pub fn is_simple_pat<'a>(pat: &Pat<'a>) -> bool {
    trace!("is_simple_pat {:?}", pat);
    match pat {
        Pat::Ident(ref id) => !matches!(&*id.name, "this"),
        _ => true,
    }
}
pub fn check_lhs_expr<'a>(expr: &Expr<'a>, allow_let: bool, pos: Position, strict: bool) -> Res {
    match expr {
        Expr::Ident(ref id) => check_ident(id, allow_let, pos, strict),
        Expr::Obj(ref obj) => check_obj_expr(obj, allow_let, pos, strict),
        Expr::Assign(ref assign) => check_assign_expr(assign, allow_let, pos, strict),
        Expr::Array(ref a) => check_array_expr(a, allow_let, pos, strict),
        Expr::Spread(ref s) => check_lhs_expr(&*s, allow_let, pos, strict),
        _ => Err(Error::InvalidLHS(pos)),
    }
}

pub fn check_lhs_pat<'a>(pat: &Pat<'a>, allow_let: bool, pos: Position, strict: bool) -> Res {
    match pat {
        Pat::Ident(ref id) => check_ident(id, allow_let, pos, strict),
        Pat::Obj(ref obj) => check_obj_pat(obj, allow_let, pos, strict),
        Pat::Assign(ref assign) => check_assign_pat(assign, allow_let, pos, strict),
        Pat::Array(ref a) => check_array_pat(a, allow_let, pos, strict),
        Pat::RestElement(ref p) => check_lhs_pat(p, allow_let, pos, strict),
    }
}

fn check_ident<'a>(id: &Ident<'a>, allow_let: bool, pos: Position, strict: bool) -> Res {
    if !allow_let && &id.name == "let" {
        Err(Error::InvalidUseOfContextualKeyword(pos, "let".to_string()))
    } else if strict && is_strict_reserved(&id) || is_restricted_word(&id) {
        Err(Error::RestrictedIdent(pos))
    } else {
        Ok(())
    }
}

fn check_obj_expr<'a>(obj: &ObjExpr<'a>, allow_let: bool, pos: Position, strict: bool) -> Res {
    for part in obj {
        check_obj_prop(part, allow_let, pos, strict)?;
    }
    Ok(())
}

fn check_obj_pat<'a>(obj: &[ObjPatPart<'a>], allow_let: bool, pos: Position, strict: bool) -> Res {
    for part in obj {
        check_obj_pat_part(part, allow_let, pos, strict)?;
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
    assign: &AssignPat<'a>,
    allow_let: bool,
    pos: Position,
    strict: bool,
) -> Res {
    check_lhs_pat(&*assign.left, allow_let, pos, strict)
}

fn check_array_expr<'a>(
    array: &[Option<Expr<'a>>],
    allow_let: bool,
    pos: Position,
    strict: bool,
) -> Res {
    for part in array {
        if let Some(expr) = part {
            check_lhs_expr(expr, allow_let, pos, strict)?;
        }
    }
    Ok(())
}

fn check_array_pat<'a>(
    array: &[Option<ArrayPatPart<'a>>],
    allow_let: bool,
    pos: Position,
    strict: bool,
) -> Res {
    for part in array {
        if let Some(part) = part {
            match part {
                ArrayPatPart::Expr(expr) => check_lhs_expr(expr, allow_let, pos, strict)?,
                ArrayPatPart::Pat(pat) => check_lhs_pat(pat, allow_let, pos, strict)?,
            }
        }
    }
    Ok(())
}

fn check_obj_prop<'a>(prop: &ObjProp<'a>, allow_let: bool, pos: Position, strict: bool) -> Res {
    match prop {
        ObjProp::Prop(ref p) => check_prop(p, allow_let, pos, strict),
        ObjProp::Spread(ref p) => check_lhs_expr(p, allow_let, pos, strict),
    }
}

fn check_obj_pat_part<'a>(
    part: &ObjPatPart<'a>,
    allow_let: bool,
    pos: Position,
    strict: bool,
) -> Res {
    match part {
        ObjPatPart::Assign(ref a) => check_prop(a, allow_let, pos, strict),
        _ => Err(Error::InvalidLHS(pos)),
    }
}

fn check_prop<'a>(prop: &Prop<'a>, allow_let: bool, pos: Position, strict: bool) -> Res {
    match prop.key {
        PropKey::Expr(ref ex) => check_lhs_expr(ex, allow_let, pos, strict),
        PropKey::Pat(ref p) => check_lhs_pat(p, allow_let, pos, strict),
        _ => Err(Error::InvalidLHS(pos)),
    }
}

pub fn check_loop_left<'a>(left: &LoopLeft<'a>, pos: Position) -> Res {
    let mut set = HashSet::new();
    match left {
        LoopLeft::Expr(expr) => check_loop_left_expr(expr, pos, &mut set),
        LoopLeft::Pat(pat) => check_loop_left_pat(pat, pos, &mut set),
        LoopLeft::Variable(kind, decls) => {
            if let VarKind::Var = kind {
                Ok(())
            } else {
                check_loop_left_pat(&decls.id, pos, &mut set)
            }
        }
    }
}

pub fn check_loop_head_expr<'a>(left: &Expr<'a>, pos: Position) -> Res {
    debug!("check_loop_head_expr");
    let mut set = HashSet::new();
    match left {
        Expr::Array(ref a) => check_binding_array(a, pos, &mut set),
        Expr::Obj(ref o) => check_binding_obj(o, pos, &mut set),
        Expr::Assign(ref a) => check_loop_left_expr(&*a.right, pos, &mut set),
        _ => Ok(()),
    }
}

fn check_binding_obj<'a>(
    obj: &[ObjProp<'a>],
    pos: Position,
    set: &mut HashSet<Cow<'a, str>>,
) -> Res {
    debug!("check_binding_obj");
    for part in obj {
        if let ObjProp::Prop(prop) = part {
            if prop.method || prop.kind == PropKind::Method {
                return Err(Error::InvalidLHS(pos));
            }
            check_loop_left_prop_key(&prop.key, pos, set)?;
        }
    }
    Ok(())
}

pub fn check_binding_array<'a>(
    a: &[Option<Expr<'a>>],
    pos: Position,
    set: &mut HashSet<Cow<'a, str>>,
) -> Res {
    debug!("check_binding_array");
    for part in a {
        if let Some(part) = &part {
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
    debug!("check_loop_left_prop_key");
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
    debug!("check_loop_left_expr");
    match expr {
        Expr::Ident(ident) => {
            if !set.insert(ident.name.clone()) {
                Err(Error::InvalidLHS(pos))
            } else {
                Ok(())
            }
        }
        _ => Ok(()),
    }
}

fn check_loop_left_pat<'a>(pat: &Pat<'a>, pos: Position, set: &mut HashSet<Cow<'a, str>>) -> Res {
    debug!("check_loop_left_pat");
    match pat {
        Pat::Ident(ident) => {
            if !set.insert(ident.name.clone()) {
                Err(Error::InvalidLHS(pos))
            } else {
                Ok(())
            }
        }
        Pat::Array(a) => {
            for p in a {
                if let Some(p) = p {
                    match p {
                        ArrayPatPart::Expr(expr) => check_loop_left_expr(expr, pos, set)?,
                        ArrayPatPart::Pat(pat) => check_loop_left_pat(pat, pos, set)?,
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
    &word.name == "eval" || &word.name == "arguments"
}
/// Check if this &str is in the list of reserved
/// words in the context of 'use strict'
#[inline]
fn is_strict_reserved(word: &Ident) -> bool {
    word.name == "implements"
        || word.name == "interface"
        || word.name == "package"
        || word.name == "private"
        || word.name == "protected"
        || word.name == "public"
        || word.name == "static"
        || word.name == "yield"
        || word.name == "let"
}
