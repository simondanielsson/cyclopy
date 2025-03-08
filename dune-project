(lang dune 3.17)

(name circular_imports)

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
     (>= 5.2.1))
   dune
   (odoc :with-doc)
   ocamlformat
   pyre-ast
   base
   cmdliner)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
