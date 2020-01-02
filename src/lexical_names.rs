use resast::prelude::*;
use std::borrow::Cow;
use hash_chain::ChainMap;
use super::{Res, Position, Error};
type LexMap<'a> = ChainMap<Cow<'a, str>, ()>;


pub fn check_for_ident<'a>(map: &LexMap<'a>, i: &Ident<'a>, start: Position) -> Res<()> {
    if map.get(&i.name).is_some() {
        Err(Error::LexicalRedecl(start, format!("{} was previously declared", i.name)))
    } else {
        Ok(())
    }
}
pub fn add_ident<'a>(map: &mut LexMap<'a>, i: &Ident<'a>, start: Position) -> Res<()> {
            if map.insert(i.name.clone(), ()).is_some() {
                Err(Error::LexicalRedecl(start, format!("{} was previously declared", i.name)))
            } else {
                Ok(())
            }
}
pub fn add_pat<'a>(map: &mut LexMap<'a>, pat: &Pat<'a>, start: Position) -> Res<()> {
    match pat {
        Pat::Ident(ref i) => {
            log::trace!("add_pat ident {:?}", i.name);
            add_ident(map, i, start)
        },
        Pat::Array(ref a) => {
            for part in a {
                if let Some(ref i) = part {
                    match i {
                        ArrayPatPart::Expr(ex) => add_expr(map, ex, start)?,
                        ArrayPatPart::Pat(pat) => add_pat(map, pat, start)?,
                    }
                }
            }
            Ok(())
        },
        Pat::Assign(ref a) => {
            add_pat(map, &*a.left, start)
        },
        Pat::Obj(ref o) => {
            for part in o {
                match part {
                    ObjPatPart::Assign(ref prop) => {
                        match prop.key {
                            PropKey::Expr(ref ex) => add_expr(map, ex, start)?,
                            PropKey::Pat(ref pat) => add_pat(map, pat, start)?,
                            PropKey::Lit(ref _lit) => unreachable!("literal as identifier"),
                        }
                    },
                    ObjPatPart::Rest(ref pat) => add_pat(map, pat, start)?,
                }
            }
            Ok(())
        },
        Pat::RestElement(ref r) => {
            add_pat(map, &*r, start)
        }
    }
}

pub fn add_expr<'a>(map: &mut LexMap<'a>, expr: &Expr<'a>, start: Position) -> Res<()> {
    if let Expr::Ident(ref i) = expr {
        log::trace!("add_expr ident {:?}", i.name);
        add_ident(map, i, start)
    } else {
        Ok(())
    }
}
