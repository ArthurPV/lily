/*
 * MIT License
 *
 * Copyright (c) 2022-2023 ArthurPV
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include <base/new.h>

#include <cli/option/run.h>

#include <stdlib.h>
#include <string.h>

// Convert option in RunOption struct.
static RunOption *
get__RunOption(const char *option);

CONSTRUCTOR(RunOption *, RunOption, enum RunOptionKind kind) {
	RunOption *self = malloc(sizeof(RunOption));

	self->kind = kind;

	return self;
}

VARIANT_CONSTRUCTOR(RunOption *, RunOption, error, const char *error) {
	RunOption *self = malloc(sizeof(RunOption));

	self->kind = RUN_OPTION_KIND_ERROR;
	self->error = error;

	return self;
}

VARIANT_CONSTRUCTOR(RunOption *, RunOption, filename, const char *filename) {
	RunOption *self = malloc(sizeof(RunOption));

	self->kind = RUN_OPTION_KIND_FILENAME;
	self->filename = filename;

	return self;
}

RunOption *
get__RunOption(const char *option)
{
	if (!strcmp(option, "-h") || !strcmp(option, "--help"))
		return NEW(RunOption, RUN_OPTION_KIND_HELP);
	else
		return NEW_VARIANT(RunOption, error, option);
}

Vec *
parse__RunOption(const char **options, const Usize options_size)
{
	Vec *res = NEW(Vec);

	for (Usize i = 0; i < options_size; i++) {
		if (options[i][0] == '-') {
			push__Vec(res, get__RunOption(options[i]));
		} else {
			push__Vec(res, NEW_VARIANT(RunOption, filename, options[i]));
		}
	}

	return res;
}
