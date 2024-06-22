use proc_macro2::Span;
use syn::spanned::Spanned;
use syn_verus as syn;

macro_rules! yes {
    () => {
        Some(vec![])
    };
}

macro_rules! no {
    () => {
        None
    };
}

macro_rules! yes_if {
    ($e1:expr) => {
        if $e1 {
            yes!()
        } else {
            None
        }
    };
    ($e1:expr, with_span: $e2:expr) => {
        if $e1 {
            Some(vec![$e2])
        } else {
            None
        }
    };
}

macro_rules! and {
    () => (yes!());
    ($e:expr) => ($e);
    ($e1:expr $( , $es:expr )*) => (
        match $e1 {
            Some(hl1) => {
                match and!($( $es ), *) {
                    Some(hl2) => Some(vec![hl1,hl2].concat()),
                    None      => None
                }
            },
            None => None
        })
}

macro_rules! or {
    () => (no!());
    ($e:expr) => ($e);
    ($e1:expr $( , $es:expr )*) => (
        match $e1 {
            Some(hl1) => {
                match or!($( $es ), *) {
                    Some(hl2) => Some(vec![hl1,hl2].concat()),
                    None      => Some(hl1)
                }
            },
            None => or!($( $es ), *),
        })
}

// Alternatively could short-circuit the or:
// (i.e. highlight only the first match but do less extra work)
//macro_rules! or {
//    () => (no!());
//    ($e:expr) => ($e);
//    ($e1:expr $( , $es:expr )*) => (
//        match $e1 {
//            Some(hl1) => Some(hl1),
//            None => or!($( $es ), *),
//        })
//}

pub struct Query {
    req: Option<syn::Expr>,
    ens: Option<syn::Expr>,
    _body: Option<syn::Expr>,
    sig: Option<syn::Signature>,
}

pub fn sig_matches(
    query: &syn::Signature,
    sig: &syn::Signature,
    impl_type: Option<&syn::Type>,
) -> Option<Vec<Span>> {
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
    let qname = query.ident.to_string();
    let sname = sig.ident.to_string();
    let name_matches = yes_if!((qname == "any".to_string()) || sname.contains(&qname));
    let args_match = match_iter_with_holes(
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
                    yes_if!(type_matches(&ty1p.ty, &ty2p.ty, impl_type), with_span: arg2.kind.span())
                }
                (syn::FnArgKind::Typed(ty1p), syn::FnArgKind::Receiver(receiver)) => {
                    if let Some(ty) = impl_type {
                        match (&receiver.reference, &*ty1p.ty) {
                            (None, _) => yes_if!(type_matches(&ty1p.ty, ty, impl_type), with_span: arg2.kind.span()),
                            (Some(_), syn::Type::Reference(reference)) => {
                                yes_if!(
                                    (receiver.mutability.is_some() == reference.mutability.is_some())
                                    && type_matches(&reference.elem, ty, impl_type), with_span: arg2.kind.span())
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
                yes_if!(type_matches(ty1, &ty2, impl_type), with_span: ty2.span())
            }
            (syn::ReturnType::Type(_, _, _, _), _) => None,
        }
    );
    and!(name_matches, mode_matches, retv_matches, args_match)
}

impl Query {
    pub fn new(
        req: Option<syn::Expr>,
        ens: Option<syn::Expr>,
        body: Option<syn::Expr>,
        sig: Option<syn::Signature>,
    ) -> Self {
        Query {
            req,
            ens,
            _body: body,
            sig,
        }
    }

    pub fn req(&self) -> Option<&syn::Expr> {
        self.req.as_ref()
    }

    pub fn ens(&self) -> Option<&syn::Expr> {
        self.ens.as_ref()
    }

    #[allow(dead_code)]
    pub fn body(&self) -> Option<&syn::Expr> {
        self._body.as_ref()
    }

    pub fn sig(&self) -> Option<&syn::Signature> {
        self.sig.as_ref()
    }
}

/// Checks if e1 matches a subexpression in any expr in exprs and remembers the first match it
/// encounters.
pub fn exprs_contain_match<'a, I>(e1: &syn::Expr, exprs: I) -> Option<Vec<Span>>
where
    I: IntoIterator<Item = &'a syn::Expr>,
{
    exprs
        .into_iter()
        .find_map(|expr| expr_contains_match(e1, expr))
}

/// Checks if e1 matches e2 (modulo some syntax that is ignored, e.g. parentheses and
/// attributes)
pub fn expr_matches(e1: &syn::Expr, e2: &syn::Expr) -> Option<Vec<Span>> {
    //println!("expr_matches:\n{:?}\n{:?}\n", e1, e2);
    // TODO: figure out how to get exhaustiveness checking for this match
    match (e1, e2) {
        (syn::Expr::Array(_), _) => panic!("Query does not support: syn::Expr::Array"),
        (syn::Expr::Assign(_), _) => panic!("Query does not support: syn::Expr::Assign"),
        (syn::Expr::AssignOp(_), _) => panic!("Query does not support: syn::Expr::AssignOp"),
        (syn::Expr::Async(_), _) => panic!("Query does not support: syn::Expr::Async"),
        (syn::Expr::Await(_), _) => panic!("Query does not support: syn::Expr::Await"),
        (syn::Expr::Block(_), _) => panic!("Query does not support: syn::Expr::Block"),
        (syn::Expr::Box(_), _) => panic!("Query does not support: syn::Expr::Box"), // TODO:?
        (syn::Expr::Break(_), _) => panic!("Query does not support: syn::Expr::Break"),
        (syn::Expr::Cast(_), _) => panic!("Query does not support: syn::Expr::Cast"),
        (syn::Expr::Closure(_), _) => panic!("Query does not support: syn::Expr::Closure"), // TODO:
        (syn::Expr::Continue(_), _) => panic!("Query does not support: syn::Expr::Continue"),
        (syn::Expr::Field(_), _) => panic!("Query does not support: syn::Expr::Field"), // TODO:
        (syn::Expr::ForLoop(_), _) => panic!("Query does not support: syn::Expr::ForLoop"),
        (syn::Expr::Group(_), _) => panic!("Query does not support: syn::Expr::Group"),
        (syn::Expr::If(_), _) => panic!("Query does not support: syn::Expr::If"),
        (syn::Expr::Let(_), _) => panic!("Query does not support: syn::Expr::Let"),
        (syn::Expr::Loop(_), _) => panic!("Query does not support: syn::Expr::Loop"),
        (syn::Expr::Macro(_), _) => panic!("Query does not support: syn::Expr::Macro"), // TODO:
        (syn::Expr::Match(_), _) => panic!("Query does not support: syn::Expr::Match"),
        (syn::Expr::Range(_), _) => panic!("Query does not support: syn::Expr::Range"),
        (syn::Expr::Reference(_), _) => panic!("Query does not support: syn::Expr::Reference"),
        (syn::Expr::Repeat(_), _) => panic!("Query does not support: syn::Expr::Repeat"),
        (syn::Expr::Return(_), _) => panic!("Query does not support: syn::Expr::Return"),
        (syn::Expr::Struct(_), _) => panic!("Query does not support: syn::Expr::Struct"), // TODO:
        (syn::Expr::Try(_), _) => panic!("Query does not support: syn::Expr::Try"),
        (syn::Expr::TryBlock(_), _) => panic!("Query does not support: syn::Expr::TryBlock"),
        (syn::Expr::Tuple(_), _) => panic!("Query does not support: syn::Expr::Tuple"), // TODO:
        (syn::Expr::Type(_), _) => panic!("Query does not support: syn::Expr::Type"),
        (syn::Expr::Unsafe(_), _) => panic!("Query does not support: syn::Expr::Unsafe"),
        (syn::Expr::While(_), _) => panic!("Query does not support: syn::Expr::While"),
        (syn::Expr::Yield(_), _) => panic!("Query does not support: syn::Expr::Yield"),
        (syn::Expr::Assume(_), _) => panic!("Query does not support: syn::Expr::Assume"),
        (syn::Expr::Assert(_), _) => panic!("Query does not support: syn::Expr::Assert"),
        (syn::Expr::AssertForall(_), _) => {
            panic!("Query does not support: syn::Expr::AssertForall")
        }
        (syn::Expr::RevealHide(_), _) => {
            panic!("Query does not support: syn::Expr::RevealHide")
        }
        (syn::Expr::BigAnd(_), _) => panic!("Query does not support: syn::Expr::BigAnd"), // TODO:
        (syn::Expr::BigOr(_), _) => panic!("Query does not support: syn::Expr::BigOr"),   // TODO:
        (syn::Expr::Is(_), _) => panic!("Query does not support: syn::Expr::Is"),         // TODO:
        (syn::Expr::Has(_), _) => panic!("Query does not support: syn::Expr::Has"),       // TODO:
        (syn::Expr::Matches(_), _) => panic!("Query does not support: syn::Expr::Matches"), // TODO:
        (syn::Expr::GetField(_), _) => panic!("Query does not support: syn::Expr::GetField"), // TODO:
        (syn::Expr::Verbatim(ts), _) => {
            // Always match if lhs is a wildcard (i.e. `_`)
            if is_wildcard(ts) {
                yes!()
            } else {
                panic!("Query contains unexpected syn::Expr::Verbatim")
            }
        }
        (syn::Expr::Unary(eb1), ref mut e2) => {
            // Does "unop1 _" match "unop2 _"?
            if let syn::UnOp::Deref(_) = eb1.op {
                // We use deref/asterisk to denote nested matching, e.g.
                // "_ + _" does not match "1 * (1 + 1)" but
                // "*(_ + _)" does match "1 * (1 + 1)" but
                expr_contains_match(&eb1.expr, e2)
            } else {
                match e2 {
                    syn::Expr::Unary(eb2) => {
                        and!(
                            yes_if!(eb1.op == eb2.op),
                            expr_matches(&eb1.expr, &eb2.expr)
                        )
                    }
                    _ => None,
                }
            }
        }
        (_, syn::Expr::Cast(eb2)) => expr_matches(e1, &eb2.expr),
        (syn::Expr::Paren(eb1), _) => expr_matches(&eb1.expr, e2),
        (_, syn::Expr::Paren(eb2)) => expr_matches(e1, &eb2.expr),
        (_, syn::Expr::Block(_eb2)) => None,
        (syn::Expr::Index(eb1), syn::Expr::Index(eb2)) => {
            and!(
                expr_matches(&eb1.expr, &eb2.expr),
                expr_matches(&eb1.index, &eb2.index)
            )
        }
        (syn::Expr::Index(eb1), syn::Expr::MethodCall(eb2)) => {
            // TODO: should find a better way than to hardcode these correspondences with
            // calls/methodcalls
            // "_[_]" should match "_.index(_)"
            and!(
                yes_if!(eb2.method == "index" && eb2.args.len() == 1),
                expr_matches(&eb1.expr, &eb2.receiver),
                expr_matches(&eb1.index, &eb2.args[0])
            )
        }
        (syn::Expr::Index(_eb1), _) => None,
        (syn::Expr::Binary(eb1), syn::Expr::Binary(eb2)) => {
            // Does "_ binop1 _" match "_ binop2 _"?
            and!(
                yes_if!(binop_matches(&eb1.op, &eb2.op)),
                expr_matches(&eb1.left, &eb2.left),
                expr_matches(&eb1.right, &eb2.right)
            )
        }
        (syn::Expr::Binary(eb1), syn::Expr::Call(eb2)) => {
            // "_ == _" should match "equal(_, _)"
            and!(
                yes_if!(
                    (match eb1.op {
                        syn::BinOp::Eq(_) => true,
                        _ => false,
                    }) && (match &*eb2.func {
                        syn::Expr::Path(syn::ExprPath {
                            path: syn::Path { segments, .. },
                            ..
                        }) => segments.len() == 1 && segments.first().unwrap().ident == "equal",
                        _ => false,
                    }) && eb2.args.len() == 2
                ),
                expr_matches(&eb1.left, &eb2.args[0]),
                expr_matches(&eb1.right, &eb2.args[1])
            )
        }
        (syn::Expr::Binary(_), _) => None,
        (syn::Expr::Call(eb1), syn::Expr::Call(eb2)) => {
            and!(
                expr_matches(&eb1.func, &eb2.func),
                args_match(eb1.args.iter(), eb2.args.iter())
            )
        }
        (syn::Expr::Call(eb1), syn::Expr::Binary(eb2)) => {
            // "equal(_, _)" should match "_ == _"
            and!(
                yes_if!(
                    (match &*eb1.func {
                        syn::Expr::Path(syn::ExprPath {
                            path: syn::Path { segments, .. },
                            ..
                        }) => segments.len() == 1 && segments.first().unwrap().ident == "equal",
                        _ => false,
                    }) && (match eb2.op {
                        syn::BinOp::Eq(_) => true,
                        _ => false,
                    }) && eb1.args.len() == 2
                ),
                expr_matches(&eb1.args[0], &eb2.left),
                expr_matches(&eb1.args[1], &eb2.right)
            )
        }
        (syn::Expr::Call(_), _) => None,
        (syn::Expr::MethodCall(eb1), syn::Expr::MethodCall(eb2)) => {
            and!(
                yes_if!(eb1.method == eb2.method),
                expr_matches(&eb1.receiver, &eb2.receiver),
                args_match(eb1.args.iter(), eb2.args.iter())
            )
        }
        (syn::Expr::MethodCall(eb1), syn::Expr::Index(eb2)) => {
            // "_.index(_)" should match "_[_]"
            and!(
                yes_if!(eb1.method == "index" && eb1.args.len() == 1),
                expr_matches(&eb2.expr, &eb1.receiver),
                expr_matches(&eb2.index, &eb1.args[0])
            )
        }
        (syn::Expr::MethodCall(_), _) => None,
        (syn::Expr::Path(eb1), syn::Expr::Path(eb2)) => {
            yes_if!(path_matches(&eb1.path, &eb2.path))
        }
        (syn::Expr::Path(_), _) => None,
        (syn::Expr::Lit(eb1), syn::Expr::Lit(eb2)) => {
            yes_if!(eb1.lit == eb2.lit)
        }
        (syn::Expr::Lit(_), _) => None,
        (syn::Expr::View(eb1), syn::Expr::View(eb2)) => expr_matches(&eb1.expr, &eb2.expr),
        (syn::Expr::View(_), _) => None,
        _ => unimplemented!("unknown expression"),
    }
}

fn args_match<'a, I>(args1: I, args2: I) -> Option<Vec<Span>>
where
    I: Iterator<Item = &'a syn::Expr> + Clone,
{
    match_iter_with_holes(args1, args2, &|a| is_wildcard(a), &|a, b| {
        expr_matches(a, b)
    })
}

/// Checks if e1 matches an expression in any statement in `stmts` and adds the pointer to the first match to `highlights`.
fn stmts_contain_match<'a, I>(e1: &syn::Expr, stmts: I) -> Option<Vec<Span>>
where
    I: IntoIterator<Item = &'a syn::Stmt>,
{
    stmts
        .into_iter()
        .find_map(|expr| stmt_contains_match(e1, expr))
}

fn stmt_contains_match(e1: &syn::Expr, stmt: &syn::Stmt) -> Option<Vec<Span>> {
    match stmt {
        syn::Stmt::Local(l) => match l.init {
            Some((_, ref e)) => expr_contains_match(e1, e),
            None => None,
        },
        syn::Stmt::Item(_i) => unimplemented!(),
        syn::Stmt::Expr(e) => expr_contains_match(e1, e),
        syn::Stmt::Semi(e, _) => expr_contains_match(e1, e),
    }
}

fn expr_contains_match(e1: &syn::Expr, e2: &syn::Expr) -> Option<Vec<Span>> {
    //println!("expr_contains_match:\n{:?}\n", e2);
    match expr_matches(e1, e2) {
        Some(hl) => Some(vec![hl, vec![e2.span()]].concat()),
        None => match e2 {
            syn::Expr::Array(_) => {
                panic!("Recursion into this expr is not yet implemented: syn::Expr::Array")
            }
            syn::Expr::Assign(_) => {
                panic!("Recursion into this expr is not yet implemented: syn::Expr::Assign")
            }
            syn::Expr::AssignOp(_) => {
                panic!("Recursion into this expr is not yet implemented: syn::Expr::AssignOp")
            }
            syn::Expr::Async(_) => {
                panic!("Recursion into this expr is not yet implemented: syn::Expr::Async")
            }
            syn::Expr::Await(_) => {
                panic!("Recursion into this expr is not yet implemented: syn::Expr::Await")
            }
            syn::Expr::Binary(eb2) => {
                or!(
                    expr_contains_match(e1, &eb2.left),
                    expr_contains_match(e1, &eb2.right)
                )
            }
            syn::Expr::Block(eb2) => stmts_contain_match(e1, eb2.block.stmts.iter()),
            syn::Expr::Box(eb2) => expr_contains_match(e1, &eb2.expr),
            syn::Expr::Break(_) => None,
            syn::Expr::Call(eb2) => {
                or!(
                    expr_contains_match(e1, &eb2.func),
                    exprs_contain_match(e1, eb2.args.iter())
                )
            }
            syn::Expr::Cast(eb2) => expr_contains_match(e1, &eb2.expr),
            syn::Expr::Closure(eb2) => expr_contains_match(e1, &eb2.body),
            syn::Expr::Continue(_) => None,
            syn::Expr::Field(eb2) => expr_contains_match(e1, &eb2.base),
            syn::Expr::ForLoop(_) => {
                panic!("Recursion into this expr is not yet implemented: syn::Expr::ForLoop")
            }
            syn::Expr::Group(_) => {
                panic!("Recursion into this expr is not yet implemented: syn::Expr::Group")
            }
            syn::Expr::If(eb2) => {
                or!(
                    expr_contains_match(e1, &eb2.cond),
                    or!(
                        stmts_contain_match(e1, eb2.then_branch.stmts.iter()),
                        eb2.else_branch
                            .as_ref()
                            .map_or(None, |(_, e)| expr_contains_match(e1, e))
                    )
                )
            }
            syn::Expr::Index(eb2) => {
                or!(
                    expr_contains_match(e1, &eb2.expr),
                    expr_contains_match(e1, &eb2.index)
                )
            }
            syn::Expr::Let(eb2) => expr_contains_match(e1, &eb2.expr),
            syn::Expr::Lit(_) => None,
            syn::Expr::Loop(_) => {
                panic!("Recursion into this expr is not yet implemented: syn::Expr::Loop")
            }
            syn::Expr::Macro(_) => None, // TODO:?
            syn::Expr::Match(eb2) => {
                or!(
                    expr_contains_match(e1, &eb2.expr),
                    eb2.arms
                        .iter()
                        .find_map(|arm| expr_contains_match(e1, &arm.body))
                )
            }
            syn::Expr::MethodCall(eb2) => {
                or!(
                    expr_contains_match(e1, &eb2.receiver),
                    exprs_contain_match(e1, eb2.args.iter())
                )
            }
            syn::Expr::Paren(eb2) => expr_contains_match(e1, &eb2.expr),
            syn::Expr::Path(_) => None,
            syn::Expr::Range(_) => {
                panic!("Recursion into this expr is not yet implemented: syn::Expr::Range")
            }
            syn::Expr::Reference(eb2) => expr_contains_match(e1, &eb2.expr),
            syn::Expr::Repeat(_) => {
                panic!("Recursion into this expr is not yet implemented: syn::Expr::Repeat")
            }
            syn::Expr::Return(eb2) => eb2
                .expr
                .as_ref()
                .map_or(None, |e| expr_contains_match(e1, e)),
            syn::Expr::Struct(eb2) => {
                or!(
                    eb2.fields
                        .iter()
                        .find_map(|fv| expr_contains_match(e1, &fv.expr)),
                    eb2.rest
                        .as_ref()
                        .map_or(None, |expr| expr_contains_match(e1, &expr))
                )
            }
            syn::Expr::Try(_) => {
                panic!("Recursion into this expr is not yet implemented: syn::Expr::Try")
            }
            syn::Expr::TryBlock(_) => {
                panic!("Recursion into this expr is not yet implemented: syn::Expr::TryBlock")
            }
            syn::Expr::Tuple(eb2) => eb2
                .elems
                .iter()
                .find_map(|expr| expr_contains_match(e1, &expr)),
            syn::Expr::Type(_) => {
                panic!("Recursion into this expr is not yet implemented: syn::Expr::Type")
            }
            syn::Expr::Unary(eb2) => expr_contains_match(e1, &eb2.expr),
            syn::Expr::Unsafe(_) => {
                panic!("Recursion into this expr is not yet implemented: syn::Expr::Unsafe")
            }
            syn::Expr::Verbatim(_) => {
                panic!("Recursion into this expr is not yet implemented: syn::Expr::Verbatim")
            }
            syn::Expr::While(_) => {
                panic!("Recursion into this expr is not yet implemented: syn::Expr::While")
            }
            syn::Expr::Yield(_) => {
                panic!("Recursion into this expr is not yet implemented: syn::Expr::Yield")
            }
            syn::Expr::Assume(_) => {
                panic!("Recursion into this expr is not yet implemented: syn::Expr::Assume")
            }
            syn::Expr::Assert(_) => {
                panic!("Recursion into this expr is not yet implemented: syn::Expr::Assert")
            }
            syn::Expr::AssertForall(_) => {
                panic!("Recursion into this expr is not yet implemented: syn::Expr::AssertForall")
            }
            syn::Expr::RevealHide(_) => {
                panic!("Recursion into this expr is not yet implemented: syn::Expr::RevealHide")
            }
            syn::Expr::View(eb2) => expr_contains_match(e1, &eb2.expr),
            syn::Expr::BigAnd(eb2) => eb2
                .exprs
                .iter()
                .find_map(|expr| expr_contains_match(e1, &expr.1)),
            syn::Expr::BigOr(eb2) => eb2
                .exprs
                .iter()
                .find_map(|expr| expr_contains_match(e1, &expr.1)),
            syn::Expr::Is(eb2) => expr_contains_match(e1, &eb2.base),
            syn::Expr::Has(eb2) => {
                and!(
                    expr_contains_match(e1, &eb2.lhs),
                    expr_contains_match(e1, &eb2.rhs)
                )
            }
            syn::Expr::Matches(_) => {
                panic!("Recursion into this expr is not yet implemented: syn::Expr::Matches")
            }
            syn::Expr::GetField(eb2) => expr_contains_match(e1, &eb2.base),
            _ => unimplemented!("unknown expression"),
        },
    }
}

fn path_matches(path1: &syn::Path, path2: &syn::Path) -> bool {
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
            (
                syn::Type::Path(syn::TypePath {
                    path: ref path1, ..
                }),
                syn::Type::Path(syn::TypePath {
                    path: ref path2, ..
                }),
            ) => {
                if path2.segments.len() == 1 && path2.segments[0].ident == "Self" {
                    if let Some(sty2) = self_type {
                        type_matches(ty1, sty2, None)
                    } else {
                        false
                    }
                } else {
                    path_matches(path1, path2)
                }
            }
            _ => false,
        }
}

/// Checks if two iterators match, where wildcards in the first iterator, identified with the
/// `is_match` predicate, can match any number of arguments in the second iterator, while other
/// arguments in the first iterator have to match exactly.
/// E.g. `(_, 1, 2, _, 3)` matches `(1, 2, 3)` and `(2, 1, 2, 4, 3)` but not `(1, 3, 2, 3)`.
///
/// This function is used to match arguments of signatures, function calls and method calls.
fn match_iter_with_holes<'a, I, T: 'a>(
    it1: I,
    it2: I,
    is_wild: &impl Fn(&T) -> bool,
    is_match: &impl Fn(&T, &T) -> Option<Vec<Span>>,
) -> Option<Vec<Span>>
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
fn is_wildcard<S: Spanned>(item: S) -> bool {
    item.span().source_text().map_or(false, |s| s == "_")
}

fn binop_matches(bop1: &syn::BinOp, bop2: &syn::BinOp) -> bool {
    use syn::BinOp::*;
    bop1 == bop2
        || match (bop1, bop2) {
            (Eq(_), ExtEq(_)) => true,
            (Eq(_), BigEq(_)) => true,
            (BigEq(_), Eq(_)) => true,
            (Ne(_), BigNe(_)) => true,
            (BigNe(_), Ne(_)) => true,
            (ExtEq(_), ExtDeepEq(_)) => true,
            (ExtDeepEq(_), ExtEq(_)) => true,
            _ => false,
        }
}

fn add_highlights<S: Spanned>(item: S, highlights: &Vec<Span>) -> String {
    // Extracts start and end of the span. I'm sure there's a proper way of doing this but alas, I
    // don't know what it is.
    fn span_bounds(span: &Span) -> (usize, usize) {
        let x = format!("{:?}", span);
        let x: Vec<_> = x.split(|c| c == '(' || c == '.' || c == ')').collect();
        (x[1].parse().unwrap(), x[3].parse().unwrap())
    }

    fn cmp_bounds((l1, h1): (usize, usize), (l2, h2): (usize, usize)) -> std::cmp::Ordering {
        if l1 < l2 {
            std::cmp::Ordering::Less
        } else if l1 == l2 {
            if h1 < h2 {
                std::cmp::Ordering::Greater
            } else if h1 == h2 {
                std::cmp::Ordering::Equal
            } else {
                std::cmp::Ordering::Less
            }
        } else {
            std::cmp::Ordering::Greater
        }
    }

    fn highlight_seq(depth: usize) -> String {
        let mut acc = "".to_string();
        //acc.push_str(&format!("X{}X\x1b[0m", depth));
        acc.push_str("\x1b[0m");
        if depth == 0 {
        } else if depth % 3 == 1 {
            acc.push_str("\x1b[1m\x1b[93m");
        } else if depth % 3 == 2 {
            acc.push_str("\x1b[1m\x1b[91m");
        } else {
            acc.push_str("\x1b[1m\x1b[94m");
        }
        acc
    }
    // We first convert the `highlights` spans to (usize, usize) tuples and adjust them to index
    // from the start of the item we're printing.
    let source = item.span().source_text().unwrap();
    let start = span_bounds(&item.span()).0;
    let mut highlights: Vec<_> = highlights
        .iter()
        .map(&span_bounds)
        .map(|(l, h)| (l - start, h - start))
        .collect();
    //#[cfg(test)] // Check that highlights are nested
    //highlights.clone().into_iter().flat_map(|x| std::iter::repeat(x).zip(highlights.clone())).for_each(|((l1, h1), (l2, h2))| {
    //    assert!((l1 <= l2 && h1 >= h2) || (l2 <= l1 && h2 >= h1));
    //});
    highlights.sort_by(|b1, b2| cmp_bounds(*b1, *b2));

    let mut acc = "".to_string();
    let mut ends = vec![];
    let mut idx = 0;
    for (l, h) in highlights {
        while ends.len() > 0 && *ends.last().unwrap() <= l {
            let end = *ends.last().unwrap();
            acc.push_str(&source[idx..end]);
            idx = end;
            ends.pop();
            acc.push_str(&highlight_seq(ends.len()));
        }
        acc.push_str(&source[idx..l]);
        idx = l;
        ends.push(h);
        acc.push_str(&highlight_seq(ends.len()));
    }
    while ends.len() > 0 {
        let end = *ends.last().unwrap();
        if end <= idx {
            assert!(end == idx);
            acc.push_str(&highlight_seq(ends.len()));
            ends.pop();
        } else {
            acc.push_str(&highlight_seq(ends.len()));
            ends.pop();
            acc.push_str(&source[idx..end]);
            idx = end;
        }
    }
    acc.push_str(&highlight_seq(0));
    acc.push_str(&source[idx..]);
    acc
}

pub fn print_fn_with_highlights(file: &str, item: syn::ItemFn, highlights: &Vec<Span>) {
    println!(
        "\x1b[32m\x1b[1m{}:{}:\x1b[0m\n{}",
        file,
        item.sig.span().start().line,
        add_highlights(&item.sig, highlights)
    );
}

pub fn print_method_with_highlights(
    file: &str,
    impl_type: &syn::Type,
    item: syn::ImplItemMethod,
    highlights: &Vec<Span>,
) {
    // TODO: probably want to unindent the result?
    // TODO: the impl message here might be wrong for traits and the like (if those are
    // ImplItemMethod?)
    println!(
        "\x1b[32m\x1b[1m{}:{}: (in `impl {}`)\x1b[0m\n{}",
        file,
        item.sig.span().start().line,
        impl_type.span().source_text().unwrap(),
        add_highlights(&item.sig, highlights)
    );
}

/// Finds and prints the first match in each item in `items`. Recurses into the `verus` macro.
fn find_and_print_matches_in_impl_items<'a, I>(
    items: I,
    file: &str,
    impl_type: &syn::Type,
    query: &Query,
) where
    I: IntoIterator<Item = syn::ImplItem>,
{
    items.into_iter().for_each(|item| match item {
        syn::ImplItem::Const(_) => {}
        syn::ImplItem::Method(m) => {
            find_and_print_matches_in_implitem_method(m, file, impl_type, query)
        }
        syn::ImplItem::Type(_) => {}
        syn::ImplItem::Macro(m) => {
            let outer_last_segment = m.mac.path.segments.last().map(|s| s.ident.to_string());
            if outer_last_segment == Some(String::from("verus")) {
                let macro_content: syn::File = syn_verus::parse2(m.mac.tokens.clone())
                    .map_err(|e| {
                        dbg!(&e.span().start(), &e.span().end());
                        format!("failed to parse file macro contents: {} {:?}", e, e.span())
                    })
                    .expect("unexpected verus! macro content");
                find_and_print_matches_in_items(macro_content.items.into_iter(), file, query);
            }
        }
        syn::ImplItem::Verbatim(_) => {}
        syn::ImplItem::BroadcastGroup(_) => {}
        _ => {}
    });
}

/// Finds and prints the first match in each item in `items`. Recurses into the `verus` macro.
pub fn find_and_print_matches_in_items<'a, I>(items: I, file: &str, query: &Query)
where
    I: Iterator<Item = syn::Item>,
{
    items.for_each(|item| {
        match item {
            // syn::Item::Const(ItemConst) => {},
            // syn::Item::Enum(ItemEnum) => {},
            // syn::Item::ExternCrate(ItemExternCrate) => {},
            syn::Item::Fn(i) => find_and_print_matches_in_item_fn(i, file, query),
            // syn::Item::ForeignMod(ItemForeignMod) => {},
            syn::Item::Impl(i) => {
                find_and_print_matches_in_impl_items(i.items, file, &i.self_ty, query)
            }
            syn::Item::Macro(m) => {
                let outer_last_segment = m.mac.path.segments.last().map(|s| s.ident.to_string());
                if outer_last_segment == Some(String::from("verus")) {
                    let macro_content: syn::File = syn_verus::parse2(m.mac.tokens.clone())
                        .map_err(|e| {
                            dbg!(&e.span().start(), &e.span().end());
                            format!("failed to parse file macro contents: {} {:?}", e, e.span())
                        })
                        .expect("unexpected verus! macro content");
                    find_and_print_matches_in_items(macro_content.items.into_iter(), file, query);
                }
            }
            // syn::Item::Macro2(ItemMacro2) => {},
            syn::Item::Mod(syn::ItemMod { content: Some((_, items)), .. }) => {
                find_and_print_matches_in_items(items.into_iter(), file, query)
            },
            // syn::Item::Static(ItemStatic) => {},
            // syn::Item::Struct(ItemStruct) => {},
            // syn::Item::Trait(ItemTrait) => {},
            // syn::Item::TraitAlias(ItemTraitAlias) => {},
            // syn::Item::Type(ItemType) => {},
            // syn::Item::Union(ItemUnion) => {},
            // syn::Item::Use(ItemUse) => {},
            // syn::Item::Verbatim(TokenStream) => {},
            // syn::Item::Global(Global) => {},
            _ => {}
        }
    });
}

/// Checks if `req_expr` matches an expression in `i`'s `requires` clause and if `ens_expr` matches
/// an expression in its `ensures` clause. If so, prints the matches. Both search expressions are
/// optional and only considered if `Some`.
fn find_and_print_matches_in_implitem_method(
    i: syn::ImplItemMethod,
    file: &str,
    impl_type: &syn::Type,
    query: &Query,
) {
    let req_matches = query.req().map_or(yes!(), |qreq| {
        i.sig
            .requires
            .as_ref()
            .and_then(|req| exprs_contain_match(qreq, req.exprs.exprs.iter()))
    });
    let ens_matches = query.ens().map_or(yes!(), |qens| {
        i.sig
            .ensures
            .as_ref()
            .and_then(|ens| exprs_contain_match(qens, ens.exprs.exprs.iter()))
    });
    let sig_matches = query
        .sig()
        .map_or(yes!(), |qsig| sig_matches(qsig, &i.sig, Some(impl_type)));
    match and!(req_matches, ens_matches, sig_matches) {
        Some(hls) => print_method_with_highlights(file, impl_type, i, &hls),
        None => {}
    }
}

/// Checks if `req_expr` matches an expression in `i`'s `requires` clause and if `ens_expr` matches
/// an expression in its `ensures` clause. If so, prints the matches. Both search expressions are
/// optional and only considered if `Some`.
fn find_and_print_matches_in_item_fn(i: syn::ItemFn, file: &str, query: &Query) {
    let req_matches = query.req().map_or(yes!(), |qreq| {
        i.sig
            .requires
            .as_ref()
            .and_then(|req| exprs_contain_match(qreq, req.exprs.exprs.iter()))
    });
    let ens_matches = query.ens().map_or(yes!(), |qens| {
        i.sig
            .ensures
            .as_ref()
            .and_then(|ens| exprs_contain_match(qens, ens.exprs.exprs.iter()))
    });
    let sig_matches = query
        .sig()
        .map_or(yes!(), |qsig| sig_matches(qsig, &i.sig, None));
    match and!(req_matches, ens_matches, sig_matches) {
        Some(hls) => print_fn_with_highlights(file, i, &hls),
        None => {}
    }
}

mod test {
    use super::*;

    #[allow(dead_code)]
    fn check_sig_matches(query: &str, sig: &str, impl_type: Option<&str>, expect: bool) {
        let query_ast: syn::Signature =
            syn::parse_str(&query).expect(&format!("Failed to parse \"{}\" into signature", query));
        let sig_ast: syn::Signature =
            syn::parse_str(&sig).expect(&format!("Failed to parse \"{}\" into signature", sig));
        let impl_type_ast: Option<syn::Type> = impl_type
            .map(|ty| syn::parse_str(&ty).expect(&format!("Failed to parse \"{}\" into type", ty)));
        let res = sig_matches(&query_ast, &sig_ast, impl_type_ast.as_ref());
        if res.is_some() != expect {
            if expect {
                println!(
                    "Query:\n{:?} should match {:?} with impl_type {:?}\n",
                    query, sig, impl_type
                );
            } else {
                println!(
                    "Query:\n{:?} shouldn't match {:?} with impl_type {:?}\n",
                    query, sig, impl_type
                );
            }
            println!("Query AST:\n{:?}\n", query_ast);
            println!("Sig AST:\n{:?}\n", sig_ast);
        }
        assert!(res.is_some() == expect);
    }

    #[allow(dead_code)]
    fn check_expr_matches(query: &str, expr: &str, expect: bool) {
        let query_ast: syn::Expr = syn::parse_str(&query)
            .expect(&format!("Failed to parse \"{}\" into expression", query));
        let expr_ast: syn::Expr =
            syn::parse_str(&expr).expect(&format!("Failed to parse \"{}\" into expression", expr));
        let res = expr_matches(&query_ast, &expr_ast);
        if res.is_some() != expect {
            if expect {
                println!("Query:\n{:?} should match {:?}\n", query, expr);
            } else {
                println!("Query:\n{:?} shouldn't match {:?}\n", query, expr);
            }
            println!("Query AST:\n{:?}\n", query_ast);
            println!("Expr AST:\n{:?}\n", expr_ast);
        }
        assert!(res.is_some() == expect);
    }

    #[test]
    fn test_binop() {
        let query = "_ + _";
        let expr = "foo(a) + 3";
        check_expr_matches(query, expr, true);

        let query = "_ - _";
        let expr = "foo(a) + 3";
        check_expr_matches(query, expr, false);

        let query = "_ + _";
        let expr = "foo(a) + (3 * 3)";
        check_expr_matches(query, expr, true);

        let query = "x + _";
        let expr = "foo(a) + (3 * 3)";
        check_expr_matches(query, expr, false);
    }

    #[test]
    fn test_binop_nested() {
        let query = "_ + (_ * _)";
        let expr = "foo(a) + (3 * 3)";
        check_expr_matches(query, expr, true);

        let query = "_ + (_ * _)";
        let expr = "foo(a) + 3";
        check_expr_matches(query, expr, false);

        let query = "_ + (_ * _)";
        let expr = "foo(a) + (3 - (3 * 3))";
        check_expr_matches(query, expr, false);
    }

    #[test]
    fn test_methodcall() {
        let query = "_.finite()";
        let expr = "foo.finite()";
        check_expr_matches(query, expr, true);

        let query = "_.finite()";
        let expr = "foo.bar()";
        check_expr_matches(query, expr, false);

        let query = "_.foo().bar()";
        let expr = "x.foo().bar()";
        check_expr_matches(query, expr, true);

        let query = "y.foo().bar()";
        let expr = "x.foo().bar()";
        check_expr_matches(query, expr, false);

        let query = "_.foo().bar()";
        let expr = "x.bar().bar()";
        check_expr_matches(query, expr, false);
    }

    #[test]
    fn test_methodcall_wildcard() {
        let query = "foo.bar(_)";
        let expr = "foo.bar(x)";
        check_expr_matches(query, expr, true);

        let query = "foo.bar(_)";
        let expr = "foo.bar(x, y)";
        check_expr_matches(query, expr, true);

        let query = "_.bar(_)";
        let expr = "foo.bar(x, y)";
        check_expr_matches(query, expr, true);

        let query = "foo.bar(_)";
        let expr = "foo.bar()";
        check_expr_matches(query, expr, true);

        let query = "foo.bar(_, y)";
        let expr = "foo.bar(x, y)";
        check_expr_matches(query, expr, true);

        let query = "foo.bar(_, y)";
        let expr = "foo.bar(a, b, y)";
        check_expr_matches(query, expr, true);

        let query = "foo.bar(_, y)";
        let expr = "foo.bar(a, y, b)";
        check_expr_matches(query, expr, false);

        let query = "foo.bar(_, y, _)";
        let expr = "foo.bar(a, y, b)";
        check_expr_matches(query, expr, true);

        let query = "foo.bar(_, y, _)";
        let expr = "foo.bar(a, y, b, c)";
        check_expr_matches(query, expr, true);

        let query = "foo.bar(_, y, b, _)";
        let expr = "foo.bar(a, y, a, y, b)";
        check_expr_matches(query, expr, true);

        let query = "foo.bar(_, a)";
        let expr = "foo.bar(a)";
        check_expr_matches(query, expr, true);

        let query = "foo.bar(_, a)";
        let expr = "foo.bar(b)";
        check_expr_matches(query, expr, false);

        let query = "foo.bar(_, a)";
        let expr = "foo.bar()";
        check_expr_matches(query, expr, false);

        let query = "foo.bar(*1, a)";
        let expr = "foo.bar(1 + 1, a)";
        check_expr_matches(query, expr, true);

        let query = "foo.bar(1, a)";
        let expr = "foo.bar(1 + 1, a)";
        check_expr_matches(query, expr, false);
    }

    #[test]
    fn test_star_nested() {
        let query = "_ + *(_ * _)";
        let expr = "1 + (2 * 3)";
        check_expr_matches(query, expr, true);

        let query = "_ + *(_ * _)";
        let expr = "1 + (1 - (2 * 3))";
        check_expr_matches(query, expr, true);
    }

    #[test]
    fn test_funcall() {
        let query = "_()";
        let expr = "foo()";
        check_expr_matches(query, expr, true);

        let query = "finite()";
        let expr = "finite()";
        check_expr_matches(query, expr, true);

        let query = "finite()";
        let expr = "bar()";
        check_expr_matches(query, expr, false);

        let query = "foo(bar())";
        let expr = "foo(bar())";
        check_expr_matches(query, expr, true);

        let query = "foo(baz())";
        let expr = "foo(bar())";
        check_expr_matches(query, expr, false);

        let query = "foo(1)";
        let expr = "foo(1, 2)";
        check_expr_matches(query, expr, false);

        let query = "foo(1, 2)";
        let expr = "foo(1)";
        check_expr_matches(query, expr, false);
    }

    #[test]
    fn test_funcall_wildcard() {
        let query = "foo(_)";
        let expr = "foo(x)";
        check_expr_matches(query, expr, true);

        let query = "foo(_)";
        let expr = "foo(x, y)";
        check_expr_matches(query, expr, true);

        let query = "foo(_)";
        let expr = "foo()";
        check_expr_matches(query, expr, true);

        let query = "_(_)";
        let expr = "foo(x, y)";
        check_expr_matches(query, expr, true);

        let query = "foo(_, y)";
        let expr = "foo(x, y)";
        check_expr_matches(query, expr, true);

        let query = "foo(_, y)";
        let expr = "foo(a, b, y)";
        check_expr_matches(query, expr, true);

        let query = "foo(_, y)";
        let expr = "foo(a, y, b)";
        check_expr_matches(query, expr, false);

        let query = "foo(_, y, _)";
        let expr = "foo(a, y, b)";
        check_expr_matches(query, expr, true);

        let query = "foo(_, y, _)";
        let expr = "foo(a, y, b, c)";
        check_expr_matches(query, expr, true);

        let query = "foo(_, y, b, _)";
        let expr = "foo(a, y, a, y, b)";
        check_expr_matches(query, expr, true);

        let query = "foo(_, a)";
        let expr = "foo(a)";
        check_expr_matches(query, expr, true);

        let query = "foo(_, a)";
        let expr = "foo(b)";
        check_expr_matches(query, expr, false);

        let query = "foo(_, a)";
        let expr = "foo()";
        check_expr_matches(query, expr, false);

        let query = "foo(*1, a)";
        let expr = "foo(1 + 1, a)";
        check_expr_matches(query, expr, true);

        let query = "foo(1, a)";
        let expr = "foo(1 + 1, a)";
        check_expr_matches(query, expr, false);
    }

    #[test]
    fn test_skipped_syntax() {
        let query = "_ + _";
        let expr = "(1 + 2)";
        check_expr_matches(query, expr, true);

        let query = "(_)";
        let expr = "1";
        check_expr_matches(query, expr, true);

        let query = "1";
        let expr = "(1)";
        check_expr_matches(query, expr, true);

        let query = "1 + 1 * 1";
        let expr = "1 + (1 * 1)";
        check_expr_matches(query, expr, true);

        let query = "foo(3)";
        let expr = "foo(3 as nat)";
        check_expr_matches(query, expr, true);

        let query = "foo(3)";
        let expr = "foo(3 as gobbledygook)";
        check_expr_matches(query, expr, true);
    }

    #[test]
    fn test_wildcard() {
        let query = "_";
        let expr = "3 * 7";
        check_expr_matches(query, expr, true);

        let query = "x";
        let expr = "y";
        check_expr_matches(query, expr, false);
    }

    #[ignore]
    #[test]
    fn test_unification() {
        let query = "x + x";
        let expr = "1 + 1";
        check_expr_matches(query, expr, true);

        let query = "x + x";
        let expr = "1 + 2";
        check_expr_matches(query, expr, true);

        let query = "x + *(x + x)";
        let expr = "1 + (3 * (1 + 1))";
        check_expr_matches(query, expr, true);

        let query = "x + *(x + x)";
        let expr = "1 + (3 * (2 + 2))";
        check_expr_matches(query, expr, false);
    }

    #[test]
    fn test_path() {
        let query = "A::x";
        let expr = "A::x";
        check_expr_matches(query, expr, true);

        let query = "x";
        let expr = "A::x";
        check_expr_matches(query, expr, true);

        let query = "A::x";
        let expr = "x";
        check_expr_matches(query, expr, false);
    }

    #[ignore]
    #[test]
    fn test_macros_collections() {
        let query = "vec![]";
        let expr = "vec![]";
        check_expr_matches(query, expr, true);

        let query = "vec![]";
        let expr = "vec![1]";
        check_expr_matches(query, expr, false);

        let query = "vec![_]";
        let expr = "vec![1]";
        check_expr_matches(query, expr, true);

        let query = "vec![_]";
        let expr = "vec![1,2]";
        check_expr_matches(query, expr, true);

        let query = "vec![3,_]";
        let expr = "vec![3,4]";
        check_expr_matches(query, expr, true);

        let query = "vec![_ + _]";
        let expr = "vec![1 + 3]";
        check_expr_matches(query, expr, true);

        //let query = "vec![_,_]";
        //let expr = "vec![3]";
        //check_expr_matches(query, expr, false);

        //let query = "vec![_,_]";
        //let expr = "vec![3,4,5]";
        //check_expr_matches(query, expr, false);

        let query = "seq![1,2]";
        let expr = "seq![1,2]";
        check_expr_matches(query, expr, true);

        let query = "set![1,2]";
        let expr = "set![1,2]";
        check_expr_matches(query, expr, true);

        let query = "seq![1,2]";
        let expr = "vec![1,2]";
        check_expr_matches(query, expr, false);
    }

    #[test]
    fn test_literals() {
        let query = "1";
        let expr = "1";
        check_expr_matches(query, expr, true);

        let query = "1";
        let expr = "2";
        check_expr_matches(query, expr, false);

        let query = "true";
        let expr = "true";
        check_expr_matches(query, expr, true);

        let query = "true";
        let expr = "1";
        check_expr_matches(query, expr, false);
    }

    #[test]
    fn test_equalities() {
        let query = "_ == _";
        let expr = "1 == 1";
        check_expr_matches(query, expr, true);

        let query = "_ == _";
        let expr = "1 =~= 1";
        check_expr_matches(query, expr, true);

        let query = "_ == _";
        let expr = "1 === 1";
        check_expr_matches(query, expr, true);

        let query = "_ != _";
        let expr = "1 !== 1";
        check_expr_matches(query, expr, true);

        // TODO: Maybe searching for anything but normal ==/!=/equal should require an exact match
        let query = "_ !== _";
        let expr = "1 != 1";
        check_expr_matches(query, expr, true);

        let query = "_ === _";
        let expr = "1 == 1";
        check_expr_matches(query, expr, true);

        let query = "_ =~= _";
        let expr = "1 =~~= 1";
        check_expr_matches(query, expr, true);

        let query = "_ =~~= _";
        let expr = "1 =~= 1";
        check_expr_matches(query, expr, true);

        // TODO: What behavior do we want here?
        //
        //// Type information would be useful here
        //let query = "_ == _";
        //let expr = "1 <==> 1";
        //assert!(!check_expr_matches(query, expr));
        //
        //let query = "_ <==> _";
        //let expr = "1 == 1";
        //assert!(!check_expr_matches(query, expr));
        //
        //let query = "_ =~= _";
        //let expr = "1 == 1";
        //assert!(!check_expr_matches(query, expr));
    }

    #[test]
    fn test_equalities_equal() {
        let query = "_ == _";
        let expr = "equal(1, 1)";
        check_expr_matches(query, expr, true);

        let query = "1 == _";
        let expr = "equal(2, 1)";
        check_expr_matches(query, expr, false);

        let query = "equal(_, _)";
        let expr = "1 == 1";
        check_expr_matches(query, expr, true);

        let query = "equal(2, _)";
        let expr = "1 == 1";
        check_expr_matches(query, expr, false);

        // TODO: add(_,_) <--> _+_, etc.

        // TODO: ?
        //let query = "!equal(_, _)";
        //let expr = "1 != 1";
        //check_expr_matches(query, expr, true);
        //
        //let query = "_ != _";
        //let expr = "!equal(1, 1)";
        //check_expr_matches(query, expr, true);
    }

    #[test]
    fn test_recurse() {
        let query = "*1";
        let expr = "1 == 1";
        check_expr_matches(query, expr, true);

        let query = "*1";
        let expr = "{ 1 }";
        check_expr_matches(query, expr, true);

        let query = "*1";
        let expr = "(1)";
        check_expr_matches(query, expr, true);

        let query = "*1";
        let expr = "(1,2)";
        check_expr_matches(query, expr, true);

        let query = "*1";
        let expr = "Foo(1,2)";
        check_expr_matches(query, expr, true);

        let query = "*1";
        let expr = "Foo { a: 1, b: 2 }";
        check_expr_matches(query, expr, true);

        let query = "*1";
        let expr = "{ let x = 1; () }";
        check_expr_matches(query, expr, true);

        let query = "*1";
        let expr = "box 1";
        check_expr_matches(query, expr, true);

        //let query = "*1";
        //let expr = "{ if 1 then { } else { } }";
        //check_expr_matches(query, expr, true);
        //
        //let query = "*1";
        //let expr = "if b then { 1 }";
        //check_expr_matches(query, expr, true);
        //
        //let query = "*1";
        //let expr = "if b then { x } else { 1 }";
        //check_expr_matches(query, expr, true);
    }

    #[test]
    fn test_index() {
        let query = "x[y]";
        let expr = "x[y]";
        check_expr_matches(query, expr, true);

        let query = "x[y]";
        let expr = "x.index(y)";
        check_expr_matches(query, expr, true);

        let query = "x.index(y)";
        let expr = "x[y]";
        check_expr_matches(query, expr, true);
    }

    #[test]
    fn test_sig() {
        let query = "spec fn foo()";
        let sig = "spec fn foo()";
        let impl_type = None;
        check_sig_matches(query, sig, impl_type, true);

        let query = "spec fn foo()";
        let sig = "spec fn foo()";
        let impl_type = Some("B::Vec<T>");
        check_sig_matches(query, sig, impl_type, true);

        let query = "spec fn foo(x: nat)";
        let sig = "spec fn foo()";
        let impl_type = None;
        check_sig_matches(query, sig, impl_type, false);

        let query = "spec fn foo(x: nat)";
        let sig = "spec fn foo(x: nat)";
        let impl_type = None;
        check_sig_matches(query, sig, impl_type, true);

        let query = "spec fn foo(x: _)";
        let sig = "spec fn foo(x: nat)";
        let impl_type = None;
        check_sig_matches(query, sig, impl_type, true);

        let query = "spec fn foo(x: _)";
        let sig = "spec fn foo(x: nat, y: nat)";
        let impl_type = None;
        check_sig_matches(query, sig, impl_type, false);

        let query = "spec fn foo(_: _)";
        let sig = "spec fn foo(x: nat, y: nat)";
        let impl_type = None;
        check_sig_matches(query, sig, impl_type, true);

        let query = "spec fn foo(_: _)";
        let sig = "proof fn foo(x: nat, y: nat)";
        let impl_type = None;
        check_sig_matches(query, sig, impl_type, false);

        let query = "fn foo(_: _)";
        let sig = "proof fn foo(x: nat, y: nat)";
        let impl_type = None;
        check_sig_matches(query, sig, impl_type, true);

        let query = "fn foo(_: Vec<_>)";
        let sig = "proof fn foo<T>(v: Vec<T>)";
        let impl_type = None;
        check_sig_matches(query, sig, impl_type, true);

        let query = "fn foo(_: Vec<_>)";
        let sig = "proof fn foo<T>(self)";
        let impl_type = Some("Vec<T>");
        check_sig_matches(query, sig, impl_type, true);

        let query = "fn foo(_: Vec<_>)";
        let sig = "proof fn foo<T>(self)";
        let impl_type = Some("A::B::Vec<T>");
        check_sig_matches(query, sig, impl_type, true);

        let query = "fn foo(_: A::B::Vec<_>)";
        let sig = "proof fn foo<T>(self)";
        let impl_type = Some("B::Vec<T>");
        check_sig_matches(query, sig, impl_type, false);

        let query = "fn foo(_: Vec<_>, _: Vec<_>)";
        let sig = "proof fn foo<T>(self, x: Self)";
        let impl_type = Some("Vec<T>");
        check_sig_matches(query, sig, impl_type, true);

        let query = "fn foo()";
        let sig = "fn foo() -> nat";
        let impl_type = None;
        check_sig_matches(query, sig, impl_type, true);

        let query = "fn foo() -> nat";
        let sig = "fn foo() -> nat";
        let impl_type = None;
        check_sig_matches(query, sig, impl_type, true);

        let query = "fn foo() -> nat";
        let sig = "fn foo() -> int";
        let impl_type = None;
        check_sig_matches(query, sig, impl_type, false);

        let query = "fn foo() -> _";
        let sig = "fn foo() -> int";
        let impl_type = None;
        check_sig_matches(query, sig, impl_type, true);

        //let query = "fn foo(_: int)";
        //let sig = "fn foo(x: &int)";
        //let impl_type = None;
        //check_sig_matches(query, sig, impl_type, true);
    }
}
