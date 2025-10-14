use super::*;

/// Checks if two iterators match, where wildcards in the first iterator, identified with the
/// `is_match` predicate, can match any number of arguments in the second iterator, while other
/// arguments in the first iterator have to match exactly.
/// E.g. `(_, 1, 2, _, 3)` matches `(1, 2, 3)` and `(2, 1, 2, 4, 3)` but not `(1, 3, 2, 3)`.
///
/// This function is used to match arguments of signatures, function calls and method calls.
pub fn match_iter_with_holes<'a, I, T: 'a>(
    it1: I,
    it2: I,
    is_wild: &impl Fn(&T) -> bool,
    is_match: &impl Fn(&T, &T) -> Option<Highlights>,
) -> Option<Highlights>
where
    I: Iterator<Item = &'a T> + Clone,
{
    let mut it1 = it1;
    let mut it2 = it2;
    match it1.next() {
        Some(it1_elem) => {
            if is_wild(it1_elem) {
                // Given
                //  - it1 = [_, a2, a3, ..]
                //  - it2 = [b1, b2, b3, ..]
                // it1 matches it2 if:
                //  -    [a2, a3, ..] matches [b1, b2, b3, ..]
                //  - or [a2, a3, ..] matches [b2, b3, ..]
                //  - or [a2, a3, ..] matches [b3, ..]
                //  - etc.
                loop {
                    match match_iter_with_holes(it1.clone(), it2.clone(), is_wild, is_match) {
                        Some(hls) => return Some(hls),
                        None => match it2.next() {
                            Some(_) => {}
                            None => return yes_if!(it1.next().is_none()),
                        },
                    }
                }
            } else {
                match it2.next() {
                    Some(it2_elem) => {
                        and!(
                            is_match(it1_elem, it2_elem),
                            match_iter_with_holes(it1, it2, is_wild, is_match)
                        )
                    }
                    None => no!(),
                }
            }
        }
        None => yes_if!(it2.next().is_none()),
    }
}

/// Any spanned thing is a wildcard iff its corresponding source is `_`.
pub fn is_wildcard<S: Spanned>(item: S) -> bool {
    item.span().source_text().is_some_and(|s| s == "_")
}

pub fn binop_matches(bop1: &syn::BinOp, bop2: &syn::BinOp) -> bool {
    use syn::BinOp::*;
    bop1 == bop2
        || matches!(
            (bop1, bop2),
            (Eq(_), ExtEq(_))
                | (Eq(_), BigEq(_))
                | (BigEq(_), Eq(_))
                | (Ne(_), BigNe(_))
                | (BigNe(_), Ne(_))
                | (ExtEq(_), ExtDeepEq(_))
                | (ExtDeepEq(_), ExtEq(_))
        )
}

pub fn matches_signature(
    query: &syn::Signature,
    sig: &Signature,
    impl_type: Option<&syn::Type>,
) -> Option<Highlights> {
    let mode_matches = match (&query.mode, &sig.mode) {
        (syn::FnMode::Default, _) => yes!(),
        (syn::FnMode::Spec(_), syn::FnMode::Spec(_)) => yes!(),
        (syn::FnMode::Spec(_), syn::FnMode::SpecChecked(_)) => yes!(),
        (syn::FnMode::SpecChecked(_), syn::FnMode::Spec(_)) => None,
        (syn::FnMode::SpecChecked(_), syn::FnMode::SpecChecked(_)) => yes!(),
        (syn::FnMode::Proof(_), syn::FnMode::Proof(_)) => yes!(),
        (syn::FnMode::Exec(_), syn::FnMode::Exec(_)) => yes!(),
        (syn::FnMode::Exec(_), syn::FnMode::Default) => yes!(), // TODO: Is default always
        // exec?
        _ => None,
    };
    let broadcast_matches = yes_if!(query.broadcast.is_some() == sig.broadcast);
    let qname = query.ident.to_string();
    let sname = sig.ident.to_string();
    let name_matches = if qname == *"any" {
        yes!()
    } else {
        match sname.match_indices(&qname).next() {
            Some((i, s)) => {
                let low = sig.ident.span_bounds().0 + i;
                Some(vec![(low, low + s.len())])
            }
            None => no!(),
        }
    };
    let args_match = match_iter_with_holes(
        // query.inputs.clone().into_iter().collect::<Vec<syn::FnArg>>().iter(),
        query.inputs.iter(),
        sig.inputs.iter(),
        &|arg: &syn::FnArg| match arg.kind {
            syn::FnArgKind::Typed(syn::PatType {
                ref pat, ref ty, ..
            }) => is_wildcard(pat) && is_wildcard(ty),
            _ => false,
        },
        &|arg1, arg2| {
            match (&arg1.kind, &arg2.kind) {
                // TODO: maybe don't ignore patterns
                (syn::FnArgKind::Typed(ty1p), syn::FnArgKind::Typed(ty2p)) => {
                    yes_if!(type_matches(&ty1p.ty, &ty2p.ty, impl_type), with_span: arg2.kind.span_bounds())
                }
                (syn::FnArgKind::Typed(ty1p), syn::FnArgKind::Receiver(receiver)) => {
                    if let Some(ty) = impl_type {
                        match (&receiver.reference, &*ty1p.ty) {
                            (None, _) => yes_if!(type_matches(&ty1p.ty, ty, impl_type), with_span: arg2.kind.span_bounds()),
                            (Some(_), syn::Type::Reference(reference)) => {
                                yes_if!(
                                    (receiver.mutability.is_some() == reference.mutability.is_some())
                                    && type_matches(&reference.elem, ty, impl_type), with_span: arg2.kind.span_bounds())
                            }
                            _ => no!(),
                        }
                    } else {
                        no!()
                    }
                }
                (syn::FnArgKind::Receiver(_), _) => panic!("Query does not support `self` argument but normal arguments can match a `self` argument. E.g. a query signature `fn foo(_: Seq<_>)` matches `fn foo(self)` if it is defined on `Seq` (i.e. in an impl block)."),
            }
        },
    );
    let retv_matches = or!(
        yes_if!(is_wildcard(&query.output)),
        match (&query.output, &sig.output) {
            (syn::ReturnType::Default, _) => yes!(),
            (syn::ReturnType::Type(_, _, _, ty1), syn::ReturnType::Type(_, _, _, ty2)) => {
                yes_if!(type_matches(ty1, ty2, impl_type), with_span: ty2.span_bounds())
            }
            (syn::ReturnType::Type(_, _, _, _), _) => None,
        }
    );
    and!(
        name_matches,
        mode_matches,
        retv_matches,
        args_match,
        broadcast_matches
    )
}

pub fn contains_match_stmts<'a, I>(stmts: I, query: &syn::Expr) -> Option<Highlights>
where
    I: Iterator<Item = &'a syn::Stmt>,
{
    let matches = stmts
        .map(|stmt| contains_match_stmt(stmt, query))
        .filter(|x| x.is_some())
        .collect::<Vec<_>>();
    if matches.is_empty() {
        None
    } else {
        Some(matches.into_iter().flatten().flatten().collect())
    }
}

fn contains_match_stmt(stmt: &syn::Stmt, expr: &syn::Expr) -> Option<Highlights> {
    match stmt {
        syn::Stmt::Local(l) => match &l.init {
            Some(syn::LocalInit { expr: e, .. }) => contains_match_expr(expr, e),
            None => None,
        },
        syn::Stmt::Item(_i) => unimplemented!(),
        syn::Stmt::Expr(e, _) => contains_match_expr(expr, e),
        syn::Stmt::Macro(_) => no!(),
    }
}

fn contains_match_impl_item(
    item: &syn::ImplItem,
    query: &Query,
    file: &str,
    impl_type: &syn::Type,
) -> Option<Match> {
    match item {
        syn::ImplItem::Fn(m) => {
            contains_match_signature(&Signature::from(&m.sig), query, Some(impl_type)).map(
                |highlights| Match::ImplItem {
                    item: Box::new(m.clone()),
                    file: file.to_string(),
                    impl_type: Box::new(impl_type.clone()),
                    highlights,
                },
            )
        }
        _ => None,
    }
}

pub fn get_matches_item(item: &syn::Item, query: &Query, file: &str) -> Vec<Match> {
    match item {
        syn::Item::Fn(i) => contains_match_signature(&Signature::from(&i.sig), query, None).map_or(
            vec![],
            |highlights| {
                vec![Match::Item {
                    item: Box::new(i.clone()),
                    file: file.to_string(),
                    highlights,
                }]
            },
        ),
        syn::Item::AssumeSpecification(i) => {
            contains_match_signature(&Signature::from(i), query, None).map_or(
                vec![],
                |highlights| {
                    vec![Match::AssumeSpec {
                        item: Box::new(i.clone()),
                        file: file.to_string(),
                        highlights,
                    }]
                },
            )
        }
        syn::Item::Impl(i) => i
            .items
            .iter()
            .filter_map(|item| contains_match_impl_item(item, query, file, &i.self_ty))
            .collect(),
        syn::Item::Macro(m) => {
            let outer_last_segment = m.mac.path.segments.last().map(|s| s.ident.to_string());
            if outer_last_segment == Some(String::from("verus")) {
                // Rejoin tokens like !is, etc.
                let tokens = syn::rejoin_tokens(m.mac.tokens.clone());
                let macro_content: syn::File = verus_syn::parse2(tokens)
                    .map_err(|e| {
                        dbg!(&e.span().start(), &e.span().end());
                        format!("failed to parse file macro contents: {} {:?}", e, e.span())
                    })
                    .expect("unexpected verus! macro content");
                macro_content
                    .items
                    .iter()
                    .flat_map(|item| get_matches_item(item, query, file))
                    .collect()
            } else {
                vec![]
            }
        }
        syn::Item::Mod(syn::ItemMod {
            content: Some((_, items)),
            ..
        }) => items
            .iter()
            .flat_map(|item| get_matches_item(item, query, file))
            .collect(),
        // syn::Item::Trait(ItemTrait) => {},
        _ => vec![],
    }
}

/// Functions and methods use `syn::Signature` but `AssumeSpecification` does not, hence we use our
/// own type which keeps just the necessary fields for matching.
pub struct Signature {
    mode: syn::FnMode,
    ident: syn::Ident,
    inputs: syn::punctuated::Punctuated<syn::FnArg, syn::token::Comma>,
    output: syn::ReturnType,
    requires: Option<syn::Requires>,
    ensures: Option<syn::Ensures>,
    broadcast: bool,
}

impl From<&syn::Signature> for Signature {
    fn from(sig: &syn::Signature) -> Signature {
        Signature {
            mode: sig.mode.clone(),
            ident: sig.ident.clone(),
            inputs: sig.inputs.clone(),
            output: sig.output.clone(),
            requires: sig.spec.requires.clone(),
            ensures: sig.spec.ensures.clone(),
            broadcast: sig.broadcast.is_some(),
        }
    }
}

impl From<&syn::AssumeSpecification> for Signature {
    fn from(asm: &syn::AssumeSpecification) -> Signature {
        let ident = asm.path.segments.last().unwrap().ident.clone();
        Signature {
            mode: syn::FnMode::Exec(syn::ModeExec {
                exec_token: syn::token::Exec {
                    span: Span::call_site(),
                },
            }),
            ident,
            inputs: asm.inputs.clone(),
            output: asm.output.clone(),
            requires: asm.requires.clone(),
            ensures: asm.ensures.clone(),
            broadcast: false,
        }
    }
}

fn contains_match_signature(
    sig: &Signature,
    query: &Query,
    impl_type: Option<&syn::Type>,
) -> Option<Highlights> {
    // If the reqens argument is present, req and ens are both None
    let reqens_matches = query.reqens().map_or(yes!(), |q| {
        or!(
            sig.requires
                .as_ref()
                .and_then(|req| contains_match_exprs(q, req.exprs.exprs.iter())),
            sig.ensures
                .as_ref()
                .and_then(|ens| contains_match_exprs(q, ens.exprs.exprs.iter()))
        )
    });
    let req_matches = query.req().map_or(yes!(), |qreq| {
        sig.requires
            .as_ref()
            .and_then(|req| contains_match_exprs(qreq, req.exprs.exprs.iter()))
    });
    let ens_matches = query.ens().map_or(yes!(), |qens| {
        sig.ensures
            .as_ref()
            .and_then(|ens| contains_match_exprs(qens, ens.exprs.exprs.iter()))
    });
    let sig_matches = query
        .sig()
        .map_or(yes!(), |qsig| matches_signature(qsig, sig, impl_type));
    and!(reqens_matches, req_matches, ens_matches, sig_matches)
}

pub fn args_match<'a, I>(args1: I, args2: I) -> Option<Highlights>
where
    I: Iterator<Item = &'a syn::Expr> + Clone,
{
    match_iter_with_holes(args1, args2, &|a| is_wildcard(a), &|a, b| {
        expr_matches(a, b)
    })
}

pub fn path_matches(path1: &syn::Path, path2: &syn::Path) -> bool {
    // Only comparing identifier, i.e. ignoring generics
    let path1 = &path1.segments;
    let path2 = &path2.segments;
    path1.len() <= path2.len()
        && path1
            .iter()
            .rev()
            .zip(path2.iter().rev())
            .all(|(s1, s2)| s1.ident == s2.ident)
}

/// Check if `ty1` matches `ty2`. If an argument has type `Self` and `self_type` is `Some`, we
/// match against that type instead.
fn type_matches(ty1: &syn::Type, ty2: &syn::Type, self_type: Option<&syn::Type>) -> bool {
    ty1 == ty2
        || match (ty1, ty2) {
            (syn::Type::Infer(_), _) => true,
            (_, syn::Type::Paren(ty2p)) => type_matches(ty1, &ty2p.elem, self_type),
            (syn::Type::Paren(ty1p), _) => type_matches(&ty1p.elem, ty2, self_type),
            (syn::Type::Reference(ty1p), syn::Type::Reference(ty2p)) => {
                ty2p.mutability == ty1p.mutability
                    && type_matches(&ty1p.elem, &ty2p.elem, self_type)
            }
            (syn::Type::Path(ty1p), syn::Type::Path(ty2p)) => {
                if ty2p.path.segments.len() == 1 && ty2p.path.segments[0].ident == "Self" {
                    if let Some(sty2) = self_type {
                        type_matches(ty1, sty2, None)
                    } else {
                        false
                    }
                } else {
                    path_matches(&ty1p.path, &ty2p.path)
                }
            }
            _ => false,
        }
}
