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
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, PATTERNESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include <base/new.h>

#include <core/lily/analysis/checked/expr.h>
#include <core/lily/analysis/checked/pattern.h>
#include <core/lily/analysis/checked/pattern/error.h>

bool
eq__LilyCheckedPatternError(const LilyCheckedPatternError *self,
                            const LilyCheckedPatternError *other)
{
    // TODO: check if the both id are equal
    return self->pattern && other->pattern
             ? eq__LilyCheckedPattern(self->pattern, other->pattern)
           : !self->pattern && !other->pattern ? true
                                               : false;
}

#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string,
               LilyCheckedPatternError,
               const LilyCheckedPatternError *self)
{
    if (self->pattern) {
        return format__String(
          "LilyCheckedPatternError{{ name = {Sr}, pattern = {Sr} }",
          to_string__Debug__LilyCheckedExpr(self->id),
          to_string__Debug__LilyCheckedPattern(self->pattern));
    }

    return format__String(
      "LilyCheckedPatternError{{ name = {Sr}, pattern = NULL }",
      to_string__Debug__LilyCheckedExpr(self->id));
}
#endif

DESTRUCTOR(LilyCheckedPatternError, const LilyCheckedPatternError *self)
{
    FREE(LilyCheckedExpr, self->id);

    if (self->pattern) {
        FREE(LilyCheckedPattern, self->pattern);
    }
}
