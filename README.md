`Verus-find` can find all functions in a set of [Verus](https://github.com/verus-lang/verus) files
that match some user-provided pattern. It supports searching for functions that match a given
signature and/or whose requires and ensures clauses or function body contain a given expression.

[![Demo](https://asciinema.org/a/MJA4otTMWkblwCtN1rnsUKrWP.svg)](https://asciinema.org/a/MJA4otTMWkblwCtN1rnsUKrWP)


## Setup and input files for the CLI version

Setting up `verus-find` currently requires cloning the repository and running `cargo build` to build
the binary.

You can either search in a single Verus file given as `--file <path_to_verus_file>` or in a set of
Verus files given by a dependency file with `--deps-file <path_to_deps_file>`. A dependency file can
be generated for a verus project by running verus with the option `--emit=dep-info`. The dependency
file only needs to be regenerated if files are added or removed to/from a project. Any changes in
the files' content will be picked up without rebuilding the dependency file.

A dependency file for `vstd` can be built using the command `verus --crate-type=lib
<path/to/verus>/source/vstd/vstd.rs --no-vstd --emit=dep-info`. (The verus invocation does not need
to complete successfully for the file to be generated.)

## Web UI

While you can run `verus-find` locally as a CLI tool, it also has a web UI. An instance for Verus's standard library `vstd` is hosted [here](https://matthias-brun.ch/verus-find/).

## Searching by signature or in pre- and postconditions

The two arguments `--req` and `--ens` take an expression to search for in the `requires` and `ensures` clauses of
functions respectively. They support the same patterns, which are a subset of valid Verus
expressions. The `--sig` argument takes a function signature to search for.

Refer to the [web version's guide](https://matthias-brun.ch/verus-find/guide.html) for information on supported patterns and signatures.

## (Some of the) Limitations

- Can't have macros in the query (e.g. `seq![x,y]`)
- No unification (e.g. such that `x + x` would match `a + a` but not `a + b`)
- Can't search for expressions by type (e.g. in ensures clauses, search for `x.len()`
  where `x` is a `Set`)
- Generics are ignored
