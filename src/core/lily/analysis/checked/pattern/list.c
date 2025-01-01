/*
 * MIT License
 *
 * Copyright (c) 2022-2025 ArthurPV
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

#include <core/lily/analysis/checked/pattern.h>
#include <core/lily/analysis/checked/pattern/list.h>

bool
eq__LilyCheckedPatternList(const LilyCheckedPatternList *self,
                           const LilyCheckedPatternList *other)
{
    // TODO: improve
    if (self->table->len != other->table->len || self->len != other->len ||
        self->must_eq != other->must_eq) {
        return false;
    }

    for (Usize i = 0; i < self->table->len; ++i) {
        if (!eq__LilyCheckedPattern(
              CAST(LilyCheckedPatternTableItem *, get__Vec(self->table, i))
                ->value,
              CAST(LilyCheckedPatternTableItem *, get__Vec(other->table, i))
                ->value)) {
            return false;
        }
    }

    return true;
}

#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string,
               LilyCheckedPatternList,
               const LilyCheckedPatternList *self)
{
    String *res = format__String(
      "LilyCheckedPatternList{{ len = {zu}, must_eq = {b}, table = {{ ",
      self->len,
      self->must_eq);

    DEBUG_VEC_STR(self->table, res, LilyCheckedPatternTableItem);

    push_str__String(res, "} }");

    return res;
}
#endif

DESTRUCTOR(LilyCheckedPatternList, LilyCheckedPatternList *self)
{
    FREE_PATTERN_TABLE(self->table);
}
