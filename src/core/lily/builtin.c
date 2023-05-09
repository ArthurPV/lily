
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

#include <core/lily/builtin.h>

LilyBuiltinFun *
load_builtins__LilyBuiltin()
{
    LilyBuiltinFun *builtins =
      lily_malloc(sizeof(LilyBuiltinFun) * BUILTINS_COUNT);

    return builtins;
}

#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, LilyBuiltinFun, const LilyBuiltinFun *self)
{
	String *res = format__String(
      "LilyBuiltinFun{{ name = {s}, return_data_type = {Sr}, params =",
      self->name,
      to_string__Debug__LilyCheckedDataType(self->return_data_type));

    DEBUG_VEC_STRING(self->params, res, LilyCheckedDataType);

    push_str__String(res, " }");

    return res;
}
#endif

DESTRUCTOR(LilyBuiltinFun, const LilyBuiltinFun *self)
{
    FREE(LilyCheckedDataType, self->return_data_type);
    FREE_BUFFER_ITEMS(
      self->params->buffer, self->params->len, LilyCheckedDataType);
    FREE(Vec, self->params);
}
