`Verus-find` can find all functions in a set of [Verus](https://github.com/verus-lang/verus) files
that match some user-provided pattern. It supports searching for functions that match a given
signature and/or whose requires and ensures clauses or function body contain a given expression.

[![Demo](https://asciinema.org/a/MJA4otTMWkblwCtN1rnsUKrWP.svg)](https://asciinema.org/a/MJA4otTMWkblwCtN1rnsUKrWP)

## Input

You can either search in a single Verus file given as `--file <path_to_verus_file>` or in a set of
Verus files given by a dependency file with `--deps-file <path_to_deps_file>`. A dependency file can
be generated for a verus project by running verus with the option `--emit=dep-info`. The dependency
file only needs to be regenerated if files are added or removed to/from a project. Any changes in
the files' content will be picked up without rebuilding the dependency file.

A dependency file for `vstd` can be built using the command `verus --crate-type=lib
<path/to/verus>/source/vstd/vstd.rs --no-vstd --emit=dep-info`. (The verus invocation does not need
to complete successfully for the file to be generated.)

## Searching in requires/ensures

The two flags `--req` and `--ens` search for an expression in the `requires` and `ensures` clauses of
functions respectively. They support the same patterns, which are a subset of valid Verus
expressions. The asterisk `*` is used in patterns to indicate nested matching.

Some examples:

- `3 + _` finds `3 + foo(4)` and `3 * (3 + foo(4))` but not `4 + 3`
- `_ + (_ * _)` finds `3 + 3 * 3` but not `3 + (3 - (3 * 3))`
- `_ + *(_ * _)` finds `3 + 3 * 3` and `3 + (3 - (3 * 3))`

Some of the matching is fuzzy. E.g.:
- `1` matches `(1)`
- `1` matches `1 as nat`
- `_ == _` matches `equal(1, 1)` and `1 =~= 1`

TODO: Explain function argument matching

## Searching by function signature

TBD

- receivers, matching in impl blocks (Self)
- "any"

## Searching in function body

TBD

## (Current) Limitations

- Can't have macros in the query (e.g. `seq![x,y]`)
- No unification (e.g. `x + x` matches `a + a` but not `a + b`
- Can't search for expressions with specific type (e.g. in ensures clauses, search for `x.len()`
  where `x` is a `Set`)
- Generics are ignored
- Functions with `external_fn_specification` should probably be treated specially (but aren't)
