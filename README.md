# ownership-interp

An OCaml interpreter for a small Rust-like language with ownership types.

This README is for installing and running the project. The complementary report
is at `report/main.pdf`. It explains the implementation choices and how the
project maps to the grading criteria.

## Install

The project needs OCaml 5.2 or newer. For a fresh local switch:

```sh
opam switch create . ocaml-base-compiler.5.2.0 -y
eval $(opam env)
opam pin add monadic https://github.com/Denommus/monadic.git#42c0a97c0667ee3daec8a31b26314b696243ee8d -y
opam install . --deps-only --with-test -y
```

## Run

```sh
make build
make test
make run                           # REPL
make run FILE=examples/phase0.lang # run a file
```

Example programs live in `examples/`.
