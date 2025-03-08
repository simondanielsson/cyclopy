(lang dune 3.17)

(name circular_imports)

(cram enable)
(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license "MIT")

(documentation https://url/to/documentation)

(package
 (name circular_imports)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
   (ocaml
     (>= 5.0.0))
   dune
   core
   (odoc :with-doc)
   (earlybird
     (>= 1.3.3))
   ocamlformat
   pyre-ast
   base
   cmdliner
   (pyml
     (>= 20231101))
   (expect_test_helpers_core :with-test)
   )
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
