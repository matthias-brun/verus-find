# Verus-find web setup

Verus-find compiles to a wasm file, which includes all Verus files in vstd.
The list of vstd files is in `src/vstd_files.rs`, which is generated by the build script in `build.rs` from the files at the path provided in `VSTD_PATH`.

## Development setup

```
VSTD_PATH=[ .. path to verus .. ]/verus/source/vstd ~/.cargo/bin/trunk serve --open
```

Trunk automatically picks up changes and rebuilds. This causes issues with the build script, which
is set to always rebuild. During the development setup, add the following line to the build script
to avoid trunk repeatedly rebuilding: (this does mean that it doesn't pick up changes in the
available vstd files)

```rust
println!("cargo::rerun-if-changed=build.rs");
```

## Release build

```
~/.cargo/bin/trunk build --release --public-url "/verus-find"
```

The `--public-url` parameter should be set to whatever relative path verus-find will be served at.
