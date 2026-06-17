(lang dune 3.16)

(generate_opam_files true)

(name ownership_interp)

(package
 (name ownership_interp)
 (synopsis "Interpreter for a Rust-like language with ownership types")
 (description
  "An OCaml interpreter for a small Rust-like language with ownership types")
 (depends
  (ocaml
   (>= 5.2))
  accessor
  async
  base
  linenoise
  monadic
  ppx_accessor
  ppx_jane))
