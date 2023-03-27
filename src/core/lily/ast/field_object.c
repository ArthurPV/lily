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

#include <base/alloc.h>

#include <core/lily/ast/field_object.h>

#ifdef ENV_DEBUG
#include <base/format.h>
#endif

CONSTRUCTOR(LilyAstFieldObject *,
            LilyAstFieldObject,
            String *name,
            LilyAstDataType *data_type,
            LilyAstExpr *optional_expr,
            enum LilyVisibility visibility)
{
    LilyAstFieldObject *self = lily_malloc(sizeof(LilyAstFieldObject));

    self->name = name;
    self->data_type = data_type;
    self->optional_expr = optional_expr;
    self->visibility = visibility;

    return self;
}

#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, LilyAstFieldObject, const LilyAstFieldObject *self)
{
    String *res = format__String(
      "LilyAstFieldObject{{ name = {S}, data_type = {Sr}, optional_expr =",
      self->name,
      to_string__Debug__LilyAstDataType(self->data_type));

    if (self->optional_expr) {
        String *s = to_string__Debug__LilyAstExpr(self->optional_expr);

        APPEND_AND_FREE(res, s);
    } else {
        push_str__String(res, " NULL");
    }

    {
        char *s = format("visibility = {s} }",
                         to_string__Debug__LilyVisibility(self->visibility));

        PUSH_STR_AND_FREE(res, s);
    }

    return res;
}
#endif

DESTRUCTOR(LilyAstFieldObject, LilyAstFieldObject *self)
{
    FREE_MOVE(self->name, FREE(String, self->name));
    FREE(LilyAstDataType, self->data_type);

    if (self->optional_expr) {
        FREE(LilyAstExpr, self->optional_expr);
    }

    lily_free(self);
}