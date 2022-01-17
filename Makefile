build:
	@dune build
test_programs:
	./test.py
test:
	@dune runtest -f
