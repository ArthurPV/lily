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

#include <cli/option/build.h>

#include <stdlib.h>

CONSTRUCTOR(BuildOption *, BuildOption, enum BuildOptionKind kind)
{
  BuildOption *self = malloc(sizeof(BuildOption));

  self->kind =  kind;

  return self;
}

VARIANT_CONSTRUCTOR(BuildOption *, BuildOption, error, char *error)
{
  BuildOption *self = malloc(sizeof(BuildOption));

  self->kind = kind;
  self->error = error;

  return self;
}

VARIANT_DESTRUCTOR(BuildOption, error, BuildOption* self)
{
  free(self->error);
  free(self);
}

DESTRUCTOR(BuildOption, BuildOption *self)
{
  switch (self->kind) {
  case BUILD_OPTION_ERROR:
    FREE_VARIANT(BuildOption, error, self);
    break;
  default:
    free(self);
  }
}
