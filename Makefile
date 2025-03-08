.PHONY: install_dev build e venv

install_dev:
	@opam install . --deps-only --with-test --with-doc
	@pre-commit install

build:
	@dune build

venv:
	@eval $(opam env)
