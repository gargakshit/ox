(lang dune 3.10)

(name ox)

(generate_opam_files true)

(source
 (github gargakshit/ox))

(authors "Akshit Garg")

(maintainers "Akshit Garg")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name ox)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune sexplib ppx_deriving ppx_sexp_conv)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
