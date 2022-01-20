build:
	@dune build

test_programs:
	./test.py

watch:
	@dune build -w

clean:
	rm -rf _build

test:
	@dune runtest -f
