(lang dune 3.20)

(name oxqr)

(generate_opam_files true)

(source
 (github benmandrew/oxqr))

(authors "Ben Andrew <benmandrew@gmail.com>")

(maintainers "Ben Andrew <benmandrew@gmail.com>")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name oxqr)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune ppx_expect js_of_ocaml)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
