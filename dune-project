(lang dune 3.0)

(name ocaml_lang)

(generate_opam_files true)

(source
 (github atlx/ocaml_lang))

(authors "atlx")

(maintainers "atlx")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name ocaml_lang)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
