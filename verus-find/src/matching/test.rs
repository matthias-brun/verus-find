use super::*;

#[cfg(test)]
fn query_with_reqens(reqens: &str) -> Query {
    let reqens_ast: syn::Expr = syn::parse_str(reqens)
        .unwrap_or_else(|_| panic!("Failed to parse \"{}\" into expression", reqens));
    let mut q = Query::empty();
    q.set_reqens(Some(reqens_ast));
    q
}

#[cfg(test)]
fn check_fun_contains_match(query: Query, fun: &str, expect: bool) {
    let fun_ast: syn::Item =
        syn::parse_str(fun).unwrap_or_else(|_| panic!("Failed to parse \"{}\" into Item", fun));
    let res = get_matches_item(&fun_ast, &query, "Foo");
    if (res.len() > 0) != expect {
        if expect {
            println!("Query:\n{:?} should match {:?}\n", query, fun);
        } else {
            println!("Query:\n{:?} shouldn't match {:?}\n", query, fun);
        }
        println!("Query:\n{:?}\n", query);
        println!("Item AST:\n{:?}\n", fun_ast);
    }
    assert!((res.len() > 0) == expect);
}

#[cfg(test)]
fn check_assume_specification_matches(query: &str, asm: &str, expect: bool) {
    let query_ast: syn::Signature = syn::parse_str(query)
        .unwrap_or_else(|_| panic!("Failed to parse \"{}\" into signature", query));
    let sig_ast: syn::AssumeSpecification = syn::parse_str(asm)
        .unwrap_or_else(|_| panic!("Failed to parse \"{}\" into AssumeSpecification", asm));
    let res = matches_signature(&query_ast, &Signature::from(&sig_ast), None);
    if res.is_some() != expect {
        if expect {
            println!("Query:\n{:?} should match {:?}\n", query, asm);
        } else {
            println!("Query:\n{:?} shouldn't match {:?}\n", query, asm);
        }
        println!("Query AST:\n{:?}\n", query_ast);
        println!("AssumeSpecification AST:\n{:?}\n", sig_ast);
    }
    assert!(res.is_some() == expect);
}

#[cfg(test)]
fn check_sig_matches(query: &str, sig: &str, impl_type: Option<&str>, expect: bool) {
    let query_ast: syn::Signature = syn::parse_str(query)
        .unwrap_or_else(|_| panic!("Failed to parse \"{}\" into signature", query));
    let sig_ast: syn::Signature = syn::parse_str(sig)
        .unwrap_or_else(|_| panic!("Failed to parse \"{}\" into signature", sig));
    let impl_type_ast: Option<syn::Type> = impl_type.map(|ty| {
        syn::parse_str(ty).unwrap_or_else(|_| panic!("Failed to parse \"{}\" into type", ty))
    });
    let res = matches_signature(
        &query_ast,
        &Signature::from(&sig_ast),
        impl_type_ast.as_ref(),
    );
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

#[cfg(test)]
fn check_expr_matches(query: &str, expr: &str, expect: bool) {
    let query_ast: syn::Expr = syn::parse_str(query)
        .unwrap_or_else(|_| panic!("Failed to parse \"{}\" into expression", query));
    let expr_ast: syn::Expr = syn::parse_str(expr)
        .unwrap_or_else(|_| panic!("Failed to parse \"{}\" into expression", expr));
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

#[test]
fn test_macros_generic() {
    let query = "abc![]";
    let expr = "abc![]";
    check_expr_matches(query, expr, true);

    let query = "abc![]";
    let expr = "xyz![]";
    check_expr_matches(query, expr, false);

    let query = "abc![x]";
    let expr = "abc![x]";
    check_expr_matches(query, expr, true);

    let query = "abc![x]";
    let expr = "abc![]";
    check_expr_matches(query, expr, true);
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
    let expr = "foo(1) is A";
    check_expr_matches(query, expr, true);

    let query = "*1";
    let expr = "foo(1) matches A";
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

    let query = "x[_]";
    let expr = "x[y]";
    check_expr_matches(query, expr, true);

    let query = "x[y]";
    let expr = "x.index(y)";
    check_expr_matches(query, expr, true);

    let query = "x[_]";
    let expr = "x.index(y)";
    check_expr_matches(query, expr, true);

    let query = "x.index(y)";
    let expr = "x[y]";
    check_expr_matches(query, expr, true);

    let query = "x.index(_)";
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

const TEST_CONST_ASSUME_SPEC: &str = "
pub assume_specification[ Foo::foo ](f: &mut Foo, value: u64)
    requires
        value % 2 == 0
    ensures
        f@ == old(f)@.foo(value),
;
";

#[test]
fn test_assume_specification_sig() {
    let asm = TEST_CONST_ASSUME_SPEC;

    let query = "spec fn any(_:_)";
    check_assume_specification_matches(query, asm, false);

    let query = "exec fn any(_:_)";
    check_assume_specification_matches(query, asm, true);

    let query = "fn any(_: &mut Foo, _: u64)";
    check_assume_specification_matches(query, asm, true);

    let query = "fn foo(_:_)";
    check_assume_specification_matches(query, asm, true);

    let query = "fn any(_: &mut Bar, _: u64)";
    check_assume_specification_matches(query, asm, false);

    let query = "fn foo(_:_)";
    check_assume_specification_matches(query, asm, true);

    let query = "fn far(_:_)";
    check_assume_specification_matches(query, asm, false);
}

#[test]
fn test_assume_specification_item() {
    let asm = TEST_CONST_ASSUME_SPEC;

    let query = query_with_reqens("_");
    check_fun_contains_match(query, asm, true);

    let query = query_with_reqens("_ % 2 == _");
    check_fun_contains_match(query, asm, true);

    let query = query_with_reqens("_ % 2");
    check_fun_contains_match(query, asm, true);

    let query = query_with_reqens("_ + 2");
    check_fun_contains_match(query, asm, false);

    let query = query_with_reqens("_@");
    check_fun_contains_match(query, asm, true);

    let query = query_with_reqens("_.foo(_)");
    check_fun_contains_match(query, asm, true);

    let query = query_with_reqens("_ > _");
    check_fun_contains_match(query, asm, false);
}

#[test]
fn test_field() {
    let query = "foo.bar";
    let expr = "foo.bar";
    check_expr_matches(query, expr, true);

    let query = "_.bar";
    let expr = "foo.bar";
    check_expr_matches(query, expr, true);

    let query = "_.0";
    let expr = "foo.0";
    check_expr_matches(query, expr, true);

    let query = "_.bar";
    let expr = "foo.baz";
    check_expr_matches(query, expr, false);
}

#[test]
fn test_bug_repros() {
    let query = "*(_.finite())";
    let expr = "bar(seq![x].add(s))";
    check_expr_matches(query, expr, false);
}

#[test]
fn test_bug_repro_wildcard() {
    let item_source = "
proof fn split_helper(r: Foo)
    requires
        r matches A::B(x)
{}
    ";
    let query_source = "_ / _";
    let query_ast: syn::Expr = syn::parse_str(query_source)
        .unwrap_or_else(|_| panic!("Failed to parse \"{}\" into expression", query_source));
    let item_ast: syn::Item = syn::parse_str(item_source)
        .unwrap_or_else(|_| panic!("Failed to parse \"{}\" into expression", item_source));
    let query = Query::new(Some(query_ast), None, None, None, None);
    let matches = get_matches_item(&item_ast, &query, "file");
    println!("{:#?}", matches);
    assert!(matches.is_empty());
}

#[test]
fn test_bug_repro_fmt_panic() {
    let item_source = "
proof fn foo()
    ensures (x + n) % n
{}
    ";
    let query_source = "*(_ + _) % _";
    let query_ast: syn::Expr = syn::parse_str(query_source)
        .unwrap_or_else(|_| panic!("Failed to parse \"{}\" into expression", query_source));
    let item_ast: syn::Item = syn::parse_str(item_source)
        .unwrap_or_else(|_| panic!("Failed to parse \"{}\" into expression", item_source));
    let query = Query::new(Some(query_ast), None, None, None, None);
    let matches = get_matches_item(&item_ast, &query, "file");
    matches.iter().for_each(|m| {
        m.as_fmt_tokens();
    }); // Shouldn't panic
}

/// Fully recurses in every requires and ensures clause in vstd. Mostly to make sure the pattern
/// matches at least support everything we have in vstd.
/// (Can't get static checks due to `non_exhaustive`.)
/// Requires `VSTD_PATH=<.. path to vstd ..>`
#[cfg(feature = "test_vstd")]
#[test]
fn test_vstd() {
    use crate::matching;
    use std::env;
    use std::path::Path;
    use walkdir::WalkDir;

    fn matches_in_file(path: &std::path::Path, query: &matching::Query) {
        let file_content = std::fs::read_to_string(path)
            .map_err(|e| format!("cannot read {} ({})", path.display(), e))
            .unwrap();
        let file = syn::parse_file(&file_content)
            .map_err(|e| {
                dbg!(&e.span().start(), &e.span().end());
                format!("failed to parse file {}: {}", path.display(), e)
            })
            .unwrap();
        let matches = matching::get_matches_file(&file, query, path.to_str().unwrap());
        matches.into_iter().for_each(|m| m.print());
    }

    let vstd_path = env::var_os("VSTD_PATH").unwrap().into_string().unwrap();

    // Very specific query that probably doesn't show up in vstd so we fully recurse into every
    // expression.
    let query_ast: syn::Expr = syn::parse_str("2102081722").unwrap();
    let query = Query::new(Some(query_ast), None, None, None, None);

    for entry in WalkDir::new(vstd_path) {
        let entry = entry.unwrap();
        if entry.file_type().is_file()
            && entry
                .path()
                .file_name()
                .unwrap()
                .to_str()
                .unwrap()
                .ends_with("rs")
        {
            let file_path = Path::new(entry.path().to_str().unwrap());
            println!("Matching in: {}", file_path.display());
            matches_in_file(file_path, &query);
        }
    }
}
