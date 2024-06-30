use vstd::prelude::*;

verus!{

// ---- Searching for functions by patterns in ensures/requires

// _ + _
proof fn foo1()
    ensures 1 + 2 == 3
{}

// same works for requires
proof fn foo2()
    requires 1 + 2 == 3
{}

// _ + _ * _
proof fn foo3()
    ensures 1 + (3 * 4) == 13
{}

// _ + _ *(_ * _)
proof fn foo4()
    ensures 1 + (2 - (3 * 4)) == -9
{}



spec fn x(a: nat) -> bool {
    a > 3
}

// some syntax is ignored:
// x(4)
proof fn foo5()
    ensures x(4 as nat)
{}

mod m {
    use super::x as y;
    use vstd::prelude::*;

    // names and types are just compared as strings
    // need to search for `y(_)` to find this one
    proof fn foo6()
        ensures y(4 as nat)
    {}
}



// ---- Searching for functions by type signature

// The function name "any" is treated as a wildcard.
// "fn any(_: nat) -> nat"
// or to find functions with any return value:
// "fn any(_: nat)"
spec fn bar1(a: nat) -> nat {
    a
}

// using a wildcard to match arbitrary other arguments
// "fn any(_: nat, _:_)"
spec fn bar2(a: nat, b: nat, c: int) -> Seq<nat> {
    arbitrary()
}



struct SomeStruct {
    a: nat,
}

impl SomeStruct {
    // Argument matching also works on the self argument of a method:
    // "fn any(_: &mut SomeStruct, _: _)"
    fn mutate(&mut self, new: nat)
        ensures self.a == new
    {
        self.a = new;
    }

}


// vstd examples:
// "fn any(_: &mut Vec<_>, _:_)"
// "_.finite()"
// "_.subrange(_)[_]"

}
