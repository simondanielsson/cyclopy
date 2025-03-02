.PHONY: build e venv

build:
	@dune build

e: build
	./_build/default/bin/main.exe @a

venv:
	@eval $(opam env)
