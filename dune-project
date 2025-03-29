(lang dune 3.17)

(name cyclopy)

(generate_opam_files true)

(source
 (github simondanielsson/cyclopy))

(authors "Simon Danielsson <simon.danielsson99@hotmail.com>")

(maintainers "Simon Danielsson <simon.danielsson99@hotmail.com>")

(license "MIT")

(package
 (name cyclopy)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
   (ocaml
     (<= 5.0.0))
   core
   (core_unix (>= v0.16.0))
   (logs (>= 0.8.0))
   fmt
   (earlybird
     (>= 1.3.3))
   ocamlformat
   pyre-ast
   base
   cmdliner
   (expect_test_helpers_core :with-test)
   )
 (tags
  ("python" "circular imports" imports)))
