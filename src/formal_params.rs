use resast::prelude::*;
use std::borrow::Cow;
use std::collections::HashSet;

pub struct FormalParams<'a> {
    pub simple: bool,
    pub params: Vec<FuncArg<'a>>,
    pub strict: bool,
    pub found_restricted: bool,
}

pub fn have_duplicates<'a>(params: &[FuncArg<'a>]) -> bool {
    if let Err(first_dupe) = find_duplicate(params) {
        error!("Found duplicate parameter: {}", first_dupe);
        true
    } else {
        false
    }
}
pub fn find_duplicate<'a>(params: &[FuncArg<'a>]) -> Result<(), Cow<'a, str>> {
    let mut set = std::collections::HashSet::new();
    for param in params.iter() {
        match param {
            FuncArg::Expr(expr) => {
                update_with_expr(expr, &mut set)?;
            }
            FuncArg::Pat(pat) => {
                update_with_pat(pat, &mut set)?;
            }
        }
    }
    Ok(())
}
fn update_with_expr<'a>(
    expr: &Expr<'a>,
    set: &mut HashSet<Cow<'a, str>>,
) -> Result<(), Cow<'a, str>> {
    trace!("update_with_expr {:?} {:?}", expr, set);
    match expr {
        Expr::Ident(id) => {
            if !set.insert(id.name.clone()) {
                return Err(id.name.clone());
            }
        }
        Expr::Assign(AssignExpr { left, .. }) => match left {
            AssignLeft::Expr(assign) => {
                update_with_expr(assign, set)?;
            }
            AssignLeft::Pat(pat) => {
                update_with_pat(pat, set)?;
            }
        },
        Expr::Obj(obj) => {
            for prop in obj {
                match prop {
                    ObjProp::Prop(prop) => {
                        update_with_prop(prop, set)?;
                    }
                    ObjProp::Spread(expr) => {
                        update_with_expr(expr, set)?;
                    }
                }
            }
        }
        _ => (),
    }
    Ok(())
}
fn update_with_pat<'a>(pat: &Pat<'a>, set: &mut HashSet<Cow<'a, str>>) -> Result<(), Cow<'a, str>> {
    trace!("update_with_pat {:?} {:?}", pat, set);
    match pat {
        Pat::Ident(id) => {
            if !set.insert(id.name.clone()) {
                return Err(id.name.clone());
            }
        }
        Pat::Array(arr) => {
            for part in arr {
                if let Some(part) = part {
                    match part {
                        ArrayPatPart::Pat(pat) => {
                            update_with_pat(pat, set)?;
                        }
                        ArrayPatPart::Expr(expr) => {
                            update_with_expr(expr, set)?;
                        }
                    }
                }
            }
        }
        Pat::Obj(obj) => {
            for part in obj {
                match part {
                    ObjPatPart::Assign(prop) => {
                        update_with_prop(prop, set)?;
                    }
                    ObjPatPart::Rest(pat) => {
                        update_with_pat(&*pat, set)?;
                    }
                }
            }
        }
        Pat::RestElement(inner) => {
            update_with_pat(&*inner, set)?;
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
    trace!("update_with_prop {:?}, {:?}", prop, set);
    match &prop.value {
        PropValue::Expr(expr) => {
            update_with_expr(expr, set)?;
        }
        PropValue::Pat(pat) => {
            update_with_pat(pat, set)?;
        }
        PropValue::None => match &prop.key {
            PropKey::Lit(lit) => {
                update_with_lit(lit, set)?;
            }
            PropKey::Expr(expr) => {
                update_with_expr(expr, set)?;
            }
            PropKey::Pat(pat) => {
                update_with_pat(pat, set)?;
            }
        },
    }
    Ok(())
}

fn update_with_lit<'a>(lit: &Lit<'a>, set: &mut HashSet<Cow<'a, str>>) -> Result<(), Cow<'a, str>> {
    trace!("update_with_lit {:?}, {:?}", lit, set);
    if let Lit::String(s) = lit {
        match s {
            resast::prelude::StringLit::Double(inner)
            | resast::prelude::StringLit::Single(inner) => {
                if !set.insert(inner.clone()) {
                    return Err(inner.clone());
                }
            }
        }
    }
    Ok(())
}
