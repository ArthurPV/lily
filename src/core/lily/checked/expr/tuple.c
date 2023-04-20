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

#include <base/macros.h>
#include <base/new.h>

#include <core/lily/checked/expr.h>
#include <core/lily/checked/expr/tuple.h>

#ifdef ENV_DEBUG

#include <base/format.h>

#include <stdio.h>
#include <stdlib.h>

String *
IMPL_FOR_DEBUG(to_string,
               LilyCheckedExprTuple,
               const LilyCheckedExprTuple *self)
{
    String *res = from__String("LilyCheckedExprTuple{ items =");

    DEBUG_VEC_STRING(self->items, res, LilyCheckedExprTuple);

    {
        char *s =
          format(", data_type = {Sr} }",
                 to_string__Debug__LilyCheckedDataType(self->data_type));

        PUSH_STR_AND_FREE(res, s);
    }

    return res;
}
#endif

DESTRUCTOR(LilyCheckedExprTuple, const LilyCheckedExprTuple *self)
{
    FREE_BUFFER_ITEMS(
      self->items->buffer, self->items->len, LilyCheckedExprTuple);
    FREE(Vec, self->items);
    FREE(LilyCheckedDataType, self->data_type);
}
