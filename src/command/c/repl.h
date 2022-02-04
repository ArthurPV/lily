#ifndef LILY_REPL_H
#define LILY_REPL_H

#include <caml/mlvalues.h>
#include <caml/alloc.h>

typedef struct repl {
	char *line_str;
	int line;
	int pos;
} repl;

CAMLprim value new_repl(value unit);
CAMLprim value run(struct repl *repl);

#endif // LILY_REPL_H
