use resast::spanned::expr::{Lit, ObjProp, Prop, PropKey, PropValue};
use resast::spanned::pat::{ArrayPatPart, ObjPatPart, Pat};
use resast::spanned::{FuncArg, ListEntry, Slice};
use std::borrow::Cow;
use std::collections::HashSet;

type Param<'a> = ListEntry<'a, FuncArg<'a>>;

pub struct FormalParams<'a> {
    pub simple: bool,
    pub open_paren: Slice<'a>,
    pub params: Vec<Param<'a>>,
    pub close_paren: Slice<'a>,
    pub strict: bool,
    pub found_restricted: bool,
}

pub struct FormalsList<'a> {
    pub keyword_async: Option<Slice<'a>>,
    pub open_paren: Option<Slice<'a>>,
    pub params: Vec<Param<'a>>,
    pub close_paren: Option<Slice<'a>>,
}

pub fn have_duplicates<'a>(params: &[Param<'a>]) -> bool {
    if let Err(first_dupe) = find_duplicate(params) {
        log::error!("Found duplicate parameter: {}", first_dupe);
        true
    } else {
        false
    }
}
pub fn find_duplicate<'a>(params: &[Param<'a>]) -> Result<(), Cow<'a, str>> {
    let mut set = HashSet::new();
    for param in params.iter() {
        match &param.item {
            FuncArg::Expr(expr) => {
                update_with_expr(expr, &mut set)?;
            }
            FuncArg::Pat(pat) => {
                update_with_pat(pat, &mut set)?;
            }
            FuncArg::Rest(rest_pat) => {
                update_with_pat(&rest_pat.pat, &mut set)?;
            }
        }
    }
    Ok(())
}
pub fn update_with_expr<'a>(
    expr: &resast::spanned::expr::Expr<'a>,
    set: &mut HashSet<Cow<'a, str>>,
) -> Result<(), Cow<'a, str>> {
    use resast::spanned::expr::{AssignExpr, AssignLeft};
    log::trace!("update_with_expr {:?} {:?}", expr, set);
    match expr {
        resast::spanned::expr::Expr::Ident(id) => {
            if !set.insert(id.slice.source.clone()) {
                return Err(id.slice.source.clone());
            }
        }
        resast::spanned::expr::Expr::Assign(AssignExpr { left, .. }) => match left {
            AssignLeft::Expr(assign) => {
                update_with_expr(assign, set)?;
            }
            AssignLeft::Pat(pat) => {
                update_with_pat(pat, set)?;
            }
        },
        resast::spanned::expr::Expr::Obj(obj) => {
            for prop in &obj.props {
                match &prop.item {
                    ObjProp::Prop(prop) => {
                        update_with_prop(prop, set)?;
                    }
                    ObjProp::Spread(expr) => {
                        update_with_expr(&expr.expr, set)?;
                    }
                }
            }
        }
        _ => (),
    }
    Ok(())
}
pub fn update_with_pat<'a>(
    pat: &resast::spanned::pat::Pat<'a>,
    set: &mut HashSet<Cow<'a, str>>,
) -> Result<(), Cow<'a, str>> {
    log::trace!("update_with_pat {:?} {:?}", pat, set);
    match pat {
        Pat::Ident(id) => {
            if !set.insert(id.slice.source.clone()) {
                return Err(id.slice.source.clone());
            }
        }
        Pat::Array(arr) => {
            for part in &arr.elements {
                if let Some(part) = &part.item {
                    match part {
                        ArrayPatPart::Pat(pat) => {
                            update_with_pat(pat, set)?;
                        }
                        ArrayPatPart::Expr(expr) => {
                            update_with_expr(expr, set)?;
                        }
                        ArrayPatPart::Rest(rest) => update_with_pat(&rest.pat, set)?,
                    }
                }
            }
        }
        Pat::Obj(obj) => {
            for part in &obj.props {
                match &part.item {
                    ObjPatPart::Assign(prop) => {
                        update_with_prop(prop, set)?;
                    }
                    ObjPatPart::Rest(pat) => {
                        update_with_pat(&pat.pat, set)?;
                    }
                }
            }
        }
        Pat::Assign(assign) => {
            update_with_pat(&*assign.left, set)?;
        }
    }
    Ok(())
}

fn update_with_prop<'a>(
    prop: &Prop<'a>,
    set: &mut HashSet<Cow<'a, str>>,
) -> Result<(), Cow<'a, str>> {
    match prop {
        Prop::Init(value) => {
            if let Some(value) = &value.value {
                update_with_prop_value(value, set)
            } else {
                update_with_prop_key(&value.key.value, set)
            }
        }
        Prop::Method(value) => update_with_prop_key(&value.id.value, set),
        Prop::Ctor(_value) => Ok(()),
        Prop::Get(value) => update_with_prop_key(&value.id.value, set),
        Prop::Set(value) => update_with_prop_key(&value.id.value, set),
    }
}

fn update_with_prop_value<'a>(
    prop: &PropValue<'a>,
    set: &mut HashSet<Cow<'a, str>>,
) -> Result<(), Cow<'a, str>> {
    log::trace!("update_with_prop {:?}, {:?}", prop, set);
    match &prop {
        PropValue::Expr(expr) => {
            update_with_expr(expr, set)?;
        }
        PropValue::Pat(pat) => {
            update_with_pat(pat, set)?;
        }
        PropValue::Method(_) => {}
    }
    Ok(())
}

fn update_with_prop_key<'a>(
    key: &PropKey<'a>,
    set: &mut HashSet<Cow<'a, str>>,
) -> Result<(), Cow<'a, str>> {
    match key {
        PropKey::Lit(lit) => update_with_lit(lit, set),
        PropKey::Expr(expr) => update_with_expr(expr, set),
        PropKey::Pat(pat) => update_with_pat(pat, set),
    }
}

fn update_with_lit<'a>(lit: &Lit<'a>, set: &mut HashSet<Cow<'a, str>>) -> Result<(), Cow<'a, str>> {
    log::trace!("update_with_lit {:?}, {:?}", lit, set);
    if let Lit::String(s) = lit {
        if !set.insert(s.content.source.clone()) {
            return Err(s.content.source.clone());
        }
    }
    Ok(())
}
