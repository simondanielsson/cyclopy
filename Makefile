.PHONY: install_dev watch run runbc test venv
	#
# This allows passing arguments to the run target
%:
	@:

install_dev:
	@opam install . --deps-only --with-test --with-doc
	@pre-commit install

watch:
	@opam exec -- dune build -w

run:
	@opam exec -- dune exec ./bin/main.exe -- $(filter-out $@,$(MAKECMDGOALS))

runbc:
	@opam exec -- dune exec ./bin/main.bc -- $(filter-out $@,$(MAKECMDGOALS))

test:
	@dune runtest

venv:
	@eval $(opam env)
