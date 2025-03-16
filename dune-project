(lang dune 3.17)

(name circular_imports)

(generate_opam_files true)

(source
 (github simondanielsson/cyclopy))

(authors "Simon Danielsson <simon.danielsson99@hotmail.com>")

(maintainers "Simon Danielsson <simon.danielsson99@hotmail.com>")

(license "MIT")

(documentation https://url/to/documentation)

(package
 (name circular_imports)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
   (ocaml
     (<= 5.0.0))
   core
   (earlybird
     (>= 1.3.3))
   ocamlformat
   pyre-ast
   base
   cmdliner
   (expect_test_helpers_core :with-test)
   )
 (tags
  ("add topics" "to describe" your project)))
