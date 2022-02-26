#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdlib.h>

#include "repl.h"

static CAMLprim value new_line(struct repl *repl);
static CAMLprim value begin_line(struct repl *repl);
static CAMLprim value end_line(struct repl *repl);
static CAMLprim value clear(struct repl *repl);
static CAMLprim value exit_repl(struct repl *repl);

static CAMLprim value free_repl(struct repl *repl) {
	free(repl->line_str);
	free(repl);
}

CAMLprim value new_repl(value unit) {
	struct repl *repl = malloc(sizeof(struct repl));
	repl->line_str = malloc(sizeof(char) + 1);
	repl->line_str = '\0';
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
		repl->line++;
	}

	free_repl(repl);
}
