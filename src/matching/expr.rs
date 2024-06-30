use super::*;

pub fn contains_match_exprs<'a, I>(query: &syn::Expr, exprs: I) -> Option<Highlights>
where
    I: Iterator<Item = &'a syn::Expr>,
{
    let matches = exprs
        .map(|expr| contains_match_expr(query, expr))
        .filter(|x| x.is_some())
        .collect::<Vec<_>>();
    if matches.is_empty() {
        None
    } else {
        Some(matches.into_iter().flatten().flatten().collect())
    }
}

/// Checks if e1 matches e2 (modulo some syntax that is ignored, e.g. parentheses and
/// attributes)
pub fn expr_matches(e1: &syn::Expr, e2: &syn::Expr) -> Option<Highlights> {
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
        (syn::Expr::Tuple(_), _) => panic!("Query does not support: syn::Expr::Tuple"),
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
                contains_match_expr(&eb1.expr, e2)
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
        // Beginning of syntax that is ignored
        (_, syn::Expr::Cast(eb2)) => expr_matches(e1, &eb2.expr),
        (syn::Expr::Paren(eb1), _) => expr_matches(&eb1.expr, e2),
        (_, syn::Expr::Paren(eb2)) => expr_matches(e1, &eb2.expr),
        (_, syn::Expr::Type(eb2)) => expr_matches(e1, &eb2.expr),
        // Beginning of syntax that is ignored
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
                    matches!(eb1.op, syn::BinOp::Eq(_))
                        && (match &*eb2.func {
                            syn::Expr::Path(syn::ExprPath {
                                path: syn::Path { segments, .. },
                                ..
                            }) => segments.len() == 1 && segments.first().unwrap().ident == "equal",
                            _ => false,
                        })
                        && eb2.args.len() == 2
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
                    }) && matches!(eb2.op, syn::BinOp::Eq(_))
                        && eb1.args.len() == 2
                ),
                expr_matches(&eb1.args[0], &eb2.left),
                expr_matches(&eb1.args[1], &eb2.right)
            )
        }
        (syn::Expr::Call(_), _) => None,
        (syn::Expr::Field(eb1), syn::Expr::Field(eb2)) => {
            and!(
                yes_if!(eb1.member == eb2.member),
                expr_matches(&eb1.base, &eb2.base)
            )
        }
        (syn::Expr::Field(_), _) => None,
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
                expr_matches(&eb1.receiver, &eb2.expr),
                expr_matches(&eb1.args[0], &eb2.index)
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
        // Tuples should maybe use arg matching instead?
        //(syn::Expr::Tuple(eb1), syn::Expr::Tuple(eb2)) => exprs_match(&eb1.elems, &eb2.elems),
        //(syn::Expr::Tuple(_), _) => None,
        (syn::Expr::View(eb1), syn::Expr::View(eb2)) => expr_matches(&eb1.expr, &eb2.expr),
        (syn::Expr::View(_), _) => None,
        _ => unimplemented!("unknown expression"),
    }
}

// Check if e2 contains match for e1
pub fn contains_match_expr(e1: &syn::Expr, e2: &syn::Expr) -> Option<Vec<Span>> {
    //println!("contains_match_expr:\n{:?}", e1);
    //println!("contains_match_expr:\n{:?}\n", e2);
    if let Some(hl) = expr_matches(e1, e2) {
        Some([hl, vec![e2.span()]].concat())
    } else {
        match e2 {
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
                    contains_match_expr(e1, &eb2.left),
                    contains_match_expr(e1, &eb2.right)
                )
            }
            syn::Expr::Block(eb2) => contains_match_stmts(eb2.block.stmts.iter(), e1),
            syn::Expr::Box(eb2) => contains_match_expr(e1, &eb2.expr),
            syn::Expr::Break(_) => None,
            syn::Expr::Call(eb2) => {
                or!(
                    contains_match_expr(e1, &eb2.func),
                    contains_match_exprs(e1, eb2.args.iter())
                )
            }
            syn::Expr::Cast(eb2) => contains_match_expr(e1, &eb2.expr),
            syn::Expr::Closure(eb2) => contains_match_expr(e1, &eb2.body),
            syn::Expr::Continue(_) => None,
            syn::Expr::Field(eb2) => contains_match_expr(e1, &eb2.base),
            syn::Expr::ForLoop(_) => {
                panic!("Recursion into this expr is not yet implemented: syn::Expr::ForLoop")
            }
            syn::Expr::Group(_) => {
                panic!("Recursion into this expr is not yet implemented: syn::Expr::Group")
            }
            syn::Expr::If(eb2) => {
                or!(
                    contains_match_expr(e1, &eb2.cond),
                    or!(
                        contains_match_stmts(eb2.then_branch.stmts.iter(), e1),
                        eb2.else_branch
                            .as_ref()
                            .and_then(|(_, e)| contains_match_expr(e1, e))
                    )
                )
            }
            syn::Expr::Index(eb2) => {
                or!(
                    contains_match_expr(e1, &eb2.expr),
                    contains_match_expr(e1, &eb2.index)
                )
            }
            syn::Expr::Let(eb2) => contains_match_expr(e1, &eb2.expr),
            syn::Expr::Lit(_) => None,
            syn::Expr::Loop(_) => {
                panic!("Recursion into this expr is not yet implemented: syn::Expr::Loop")
            }
            syn::Expr::Macro(_) => None, // TODO:?
            syn::Expr::Match(eb2) => {
                or!(
                    contains_match_expr(e1, &eb2.expr),
                    eb2.arms
                        .iter()
                        .find_map(|arm| contains_match_expr(e1, &arm.body))
                )
            }
            syn::Expr::MethodCall(eb2) => {
                or!(
                    contains_match_expr(&eb2.receiver, e1),
                    contains_match_exprs(e1, eb2.args.iter())
                )
            }
            syn::Expr::Paren(eb2) => contains_match_expr(e1, &eb2.expr),
            syn::Expr::Path(_) => None,
            syn::Expr::Range(_) => {
                panic!("Recursion into this expr is not yet implemented: syn::Expr::Range")
            }
            syn::Expr::Reference(eb2) => contains_match_expr(e1, &eb2.expr),
            syn::Expr::Repeat(_) => {
                panic!("Recursion into this expr is not yet implemented: syn::Expr::Repeat")
            }
            syn::Expr::Return(eb2) => eb2.expr.as_ref().and_then(|e| contains_match_expr(e1, e)),
            syn::Expr::Struct(eb2) => {
                or!(
                    eb2.fields
                        .iter()
                        .find_map(|fv| contains_match_expr(e1, &fv.expr)),
                    eb2.rest
                        .as_ref()
                        .and_then(|expr| contains_match_expr(e1, expr))
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
                .find_map(|expr| contains_match_expr(e1, expr)),
            syn::Expr::Type(eb2) => expr_matches(e1, &eb2.expr),

            syn::Expr::Unary(eb2) => contains_match_expr(e1, &eb2.expr),
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
            syn::Expr::View(eb2) => contains_match_expr(e1, &eb2.expr),
            syn::Expr::BigAnd(eb2) => eb2
                .exprs
                .iter()
                .find_map(|expr| contains_match_expr(e1, &expr.1)),
            syn::Expr::BigOr(eb2) => eb2
                .exprs
                .iter()
                .find_map(|expr| contains_match_expr(e1, &expr.1)),
            syn::Expr::Is(eb2) => contains_match_expr(e1, &eb2.base),
            syn::Expr::Has(eb2) => {
                and!(
                    contains_match_expr(e1, &eb2.lhs),
                    contains_match_expr(e1, &eb2.rhs)
                )
            }
            syn::Expr::Matches(eb2) => {
                or!(
                    contains_match_expr(e1, &eb2.lhs),
                    match &eb2.op_expr {
                        Some(op_expr) => contains_match_expr(e1, &op_expr.rhs),
                        None => yes!(),
                    }
                )
            }
            syn::Expr::GetField(eb2) => contains_match_expr(e1, &eb2.base),
            _ => unimplemented!("unknown expression"),
        }
    }
}
