.PHONY: default venv

default:
	@dune build && dune exec circular_imports

venv:
	@eval $(opam env)
