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

#include <core/lily/ir/llvm/scope.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

CONSTRUCTOR(LilyLlvmValue *, LilyLlvmValue, String *name, LLVMValueRef value)
{
    LilyLlvmValue *self = lily_malloc(sizeof(LilyLlvmValue));

    self->name = name;
    self->value = value;

    return self;
}

CONSTRUCTOR(LilyLlvmScope *, LilyLlvmScope, LilyLlvmScope *parent)
{
    LilyLlvmScope *self = lily_malloc(sizeof(LilyLlvmScope));

    self->values = NEW(Vec);
    self->parent = parent;

    return self;
}

LilyLlvmValue *
search__LilyLlvmScope(LilyLlvmScope *self, String *name)
{
    for (Usize i = 0; i < self->values->len; ++i) {
        LilyLlvmValue *value = get__Vec(self->values, i);

        if (!strcmp(value->name->buffer, name->buffer)) {
            return value;
        }
    }

    UNREACHABLE("the analysis or the codegen have a bug!!");
}

DESTRUCTOR(LilyLlvmScope, LilyLlvmScope *self)
{
    FREE_BUFFER_ITEMS(self->values->buffer, self->values->len, LilyLlvmValue);
    FREE(Vec, self->values);
    lily_free(self);
}