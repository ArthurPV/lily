build:
	@dune build

test_programs:
	./test.py

watch:
	@dune build -w

format:
	ocamlformat --inplace src/analysis/*.ml
	ocamlformat --inplace src/analysis/*.mli
	ocamlformat --inplace src/cli/*.ml
	ocamlformat --inplace src/cli/*.mli
	ocamlformat --inplace src/command/*.ml
	# ocamlformat --inplace src/command/*.mli
	ocamlformat --inplace src/core/bytecode/*.ml
	ocamlformat --inplace src/core/bytecode/*.mli
	ocamlformat --inplace src/common/*.ml
	ocamlformat --inplace src/common/*.mli
	ocamlformat --inplace src/lexer/*.ml
	ocamlformat --inplace src/lexer/*.mli
	ocamlformat --inplace src/parser/*.ml
	ocamlformat --inplace src/parser/*.mli
	ocamlformat --inplace src/*.ml
	# ocamlformat --inplace tests/analysis/*.ml
	ocamlformat --inplace tests/lexer/*.ml
	ocamlformat --inplace tests/parser/*.ml
	ocamlformat --inplace tests/vm/*.ml

clean:
	rm -rf _build

test:
	@dune runtest -f
