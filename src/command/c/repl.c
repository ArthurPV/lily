#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdlib.h>

#include "repl.h"

CAMLprim value new_repl(value unit) {
	struct repl *repl = malloc(sizeof(struct repl));
	repl->line_str = "";
	repl->line = 1;
	repl->pos = 1;
	return repl;
}

CAMLprim value run(struct repl *repl) {
	char *prompt;

	while (true) {
		printf("lily:%d> ", repl->line);
		scanf("%s", &prompt);
		repl->line_str = prompt;
	}
}
