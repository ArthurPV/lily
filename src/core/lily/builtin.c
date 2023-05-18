
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

#include <string.h>

LilyBuiltinFun *
load_builtins__LilyBuiltin()
{
    LilyBuiltinFun *builtins =
      lily_malloc(sizeof(LilyBuiltinFun) * BUILTINS_COUNT);

    builtins[0] = (LilyBuiltinFun){
        .name = "max",
        .real_name = from__String("__max__$Int8"),
        .return_data_type =
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_INT8, NULL),
        .params = init__Vec(
          2,
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_INT8, NULL),
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_INT8, NULL))
    };

    builtins[1] = (LilyBuiltinFun){
        .name = "max",
        .real_name = from__String("__max__$Int16"),
        .return_data_type =
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_INT16, NULL),
        .params = init__Vec(
          2,
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_INT16, NULL),
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_INT16, NULL))
    };

    builtins[2] = (LilyBuiltinFun){
        .name = "max",
        .real_name = from__String("__max__$Int32"),
        .return_data_type =
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_INT32, NULL),
        .params = init__Vec(
          2,
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_INT32, NULL),
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_INT32, NULL))
    };

    builtins[3] = (LilyBuiltinFun){
        .name = "max",
        .real_name = from__String("__max__$Int64"),
        .return_data_type =
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_INT64, NULL),
        .params = init__Vec(
          2,
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_INT64, NULL),
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_INT64, NULL))
    };

    builtins[4] = (LilyBuiltinFun){
        .name = "max",
        .real_name = from__String("__max__$Isize"),
        .return_data_type =
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_ISIZE, NULL),
        .params = init__Vec(
          2,
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_ISIZE, NULL),
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_ISIZE, NULL))
    };

    builtins[5] = (LilyBuiltinFun){
        .name = "max",
        .real_name = from__String("__max__$Uint8"),
        .return_data_type =
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_UINT8, NULL),
        .params = init__Vec(
          2,
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_UINT8, NULL),
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_UINT8, NULL))
    };

    builtins[6] = (LilyBuiltinFun){
        .name = "max",
        .real_name = from__String("__max__$Uint16"),
        .return_data_type =
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_UINT16, NULL),
        .params = init__Vec(
          2,
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_UINT16, NULL),
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_UINT16, NULL))
    };

    builtins[7] = (LilyBuiltinFun){
        .name = "max",
        .real_name = from__String("__max__$Uint32"),
        .return_data_type =
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_UINT32, NULL),
        .params = init__Vec(
          2,
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_UINT32, NULL),
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_UINT32, NULL))
    };

    builtins[8] = (LilyBuiltinFun){
        .name = "max",
        .real_name = from__String("__max__$Uint64"),
        .return_data_type =
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_UINT64, NULL),
        .params = init__Vec(
          2,
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_UINT64, NULL),
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_UINT64, NULL))
    };

    builtins[9] = (LilyBuiltinFun){
        .name = "max",
        .real_name = from__String("__max__$Usize"),
        .return_data_type =
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_USIZE, NULL),
        .params = init__Vec(
          2,
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_USIZE, NULL),
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_USIZE, NULL))
    };

    builtins[10] = (LilyBuiltinFun){
        .name = "max",
        .real_name = from__String("__max__$Float32"),
        .return_data_type =
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_FLOAT32, NULL),
        .params = init__Vec(
          2,
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_FLOAT32, NULL),
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_FLOAT32, NULL))
    };

    builtins[11] = (LilyBuiltinFun){
        .name = "max",
        .real_name = from__String("__max__$Float64"),
        .return_data_type =
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_FLOAT64, NULL),
        .params = init__Vec(
          2,
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_FLOAT64, NULL),
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_FLOAT64, NULL))
    };

    builtins[12] = (LilyBuiltinFun){
        .name = "min",
        .real_name = from__String("__min__$Int8"),
        .return_data_type =
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_INT8, NULL),
        .params = init__Vec(
          2,
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_INT8, NULL),
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_INT8, NULL))
    };

    builtins[13] = (LilyBuiltinFun){
        .name = "min",
        .real_name = from__String("__min__$Int16"),
        .return_data_type =
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_INT16, NULL),
        .params = init__Vec(
          2,
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_INT16, NULL),
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_INT16, NULL))
    };

    builtins[14] = (LilyBuiltinFun){
        .name = "min",
        .real_name = from__String("__min__$Int32"),
        .return_data_type =
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_INT32, NULL),
        .params = init__Vec(
          2,
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_INT32, NULL),
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_INT32, NULL))
    };

    builtins[15] = (LilyBuiltinFun){
        .name = "min",
        .real_name = from__String("__min__$Int64"),
        .return_data_type =
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_INT64, NULL),
        .params = init__Vec(
          2,
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_INT64, NULL),
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_INT64, NULL))
    };

    builtins[16] = (LilyBuiltinFun){
        .name = "min",
        .real_name = from__String("__min__$Isize"),
        .return_data_type =
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_ISIZE, NULL),
        .params = init__Vec(
          2,
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_ISIZE, NULL),
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_ISIZE, NULL))
    };

    builtins[17] = (LilyBuiltinFun){
        .name = "min",
        .real_name = from__String("__min__$Uint8"),
        .return_data_type =
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_UINT8, NULL),
        .params = init__Vec(
          2,
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_UINT8, NULL),
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_UINT8, NULL))
    };

    builtins[18] = (LilyBuiltinFun){
        .name = "min",
        .real_name = from__String("__min__$Uint16"),
        .return_data_type =
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_UINT16, NULL),
        .params = init__Vec(
          2,
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_UINT16, NULL),
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_UINT16, NULL))
    };

    builtins[19] = (LilyBuiltinFun){
        .name = "min",
        .real_name = from__String("__min__$Uint32"),
        .return_data_type =
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_UINT32, NULL),
        .params = init__Vec(
          2,
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_UINT32, NULL),
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_UINT32, NULL))
    };

    builtins[20] = (LilyBuiltinFun){
        .name = "min",
        .real_name = from__String("__min__$Uint64"),
        .return_data_type =
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_UINT64, NULL),
        .params = init__Vec(
          2,
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_UINT64, NULL),
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_UINT64, NULL))
    };

    builtins[21] = (LilyBuiltinFun){
        .name = "min",
        .real_name = from__String("__min__$Usize"),
        .return_data_type =
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_USIZE, NULL),
        .params = init__Vec(
          2,
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_USIZE, NULL),
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_USIZE, NULL))
    };

    builtins[22] = (LilyBuiltinFun){
        .name = "min",
        .real_name = from__String("__min__$Float32"),
        .return_data_type =
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_FLOAT32, NULL),
        .params = init__Vec(
          2,
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_FLOAT32, NULL),
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_FLOAT32, NULL))
    };

    builtins[23] = (LilyBuiltinFun){
        .name = "min",
        .real_name = from__String("__min__$Float64"),
        .return_data_type =
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_FLOAT64, NULL),
        .params = init__Vec(
          2,
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_FLOAT64, NULL),
          NEW(LilyCheckedDataType, LILY_CHECKED_DATA_TYPE_KIND_FLOAT64, NULL))
    };

    return builtins;
}

bool
is_builtin_function__LilyBuiltin(const char *name)
{
    if (!strcmp(name, "max") || !strcmp(name, "min")) {
        return true;
    }

    return false;
}

const LilyBuiltinFun *
get_builtin__LilyBuiltin(LilyBuiltinFun *builtins,
                         Vec *params,
                         LilyCheckedDataType *return_data_type)
{
    for (Usize i = 0; i < BUILTINS_COUNT; ++i) {
    next_builtin : {
        const LilyBuiltinFun *builtin = &builtins[i];

        if (eq__LilyCheckedDataType(builtin->return_data_type,
                                    return_data_type)) {
            if (params->len != builtin->params->len) {
                continue;
            }

            for (Usize i = 0; i < builtin->params->len; ++i) {
                if (!eq__LilyCheckedDataType(get__Vec(builtin->params, i),
                                             get__Vec(params, i))) {
                    goto next_builtin;
                }
            }

            return builtin;
        }
    }
    }

    return NULL;
}

#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, LilyBuiltinFun, const LilyBuiltinFun *self)
{
    String *res = format__String(
      "LilyBuiltinFun{{ name = {s}, real_name = {S}, return_data_type = {Sr}, "
      "params =",
      self->name,
      self->real_name,
      to_string__Debug__LilyCheckedDataType(self->return_data_type));

    DEBUG_VEC_STRING(self->params, res, LilyCheckedDataType);

    push_str__String(res, " }");

    return res;
}
#endif

DESTRUCTOR(LilyBuiltinFun, const LilyBuiltinFun *self)
{
    FREE(String, self->real_name);
    FREE(LilyCheckedDataType, self->return_data_type);
    FREE_BUFFER_ITEMS(
      self->params->buffer, self->params->len, LilyCheckedDataType);
    FREE(Vec, self->params);
}
