/*
 * MIT License
 *
 * Copyright (c) 2022-2024 ArthurPV
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

#include <core/lily/parser/ast/body/fun.h>
#include <core/lily/parser/ast/stmt/try.h>

#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, LilyAstStmtTry, const LilyAstStmtTry *self)
{
    String *res = from__String("LilyAstStmtTry{ try_body =");

    DEBUG_VEC_STRING(self->try_body, res, LilyAstBodyFunItem);

    if (self->catch_expr) {
        String *s = to_string__Debug__LilyAstExpr(self->catch_expr);

        push_str__String(res, ", catch_expr = ");
        APPEND_AND_FREE(res, s);

        push_str__String(res, ", catch_body =");
        DEBUG_VEC_STRING(self->catch_body, res, LilyAstBodyFunItem);

        push_str__String(res, " }");
    } else {
        push_str__String(res, " }");
    }

    return res;
}
#endif

DESTRUCTOR(LilyAstStmtTry, const LilyAstStmtTry *self)
{
    FREE_BUFFER_ITEMS(
      self->try_body->buffer, self->try_body->len, LilyAstBodyFunItem);
    FREE(Vec, self->try_body);

    if (self->catch_body) {
        if (self->catch_expr) {
            FREE(LilyAstExpr, self->catch_expr);
        }

        FREE_BUFFER_ITEMS(
          self->catch_body->buffer, self->catch_body->len, LilyAstBodyFunItem);
        FREE(Vec, self->catch_body);
    }
}
