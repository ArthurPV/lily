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

#ifndef LILY_CORE_LILY_FUNCTIONS_BUILTIN_H
#define LILY_CORE_LILY_FUNCTIONS_BUILTIN_H

#include <base/macros.h>
#include <base/vec.h>

#include <core/lily/analysis/checked/data_type.h>

#define BUILTINS_COUNT 29

#define __max__$Int8 0
#define __max__$Int16 1
#define __max__$Int32 2
#define __max__$Int64 3
#define __max__$Isize 4
#define __max__$Uint8 5
#define __max__$Uint16 6
#define __max__$Uint32 7
#define __max__$Uint64 8
#define __max__$Usize 9
#define __max__$Float32 10
#define __max__$Float64 11
#define __min__$Int8 12
#define __min__$Int16 13
#define __min__$Int32 14
#define __min__$Int64 15
#define __min__$Isize 16
#define __min__$Uint8 17
#define __min__$Uint16 18
#define __min__$Uint32 19
#define __min__$Uint64 20
#define __min__$Usize 21
#define __min__$Float32 22
#define __min__$Float64 23
#define __len__$CStr 24
#define __align__$Alloc 25
#define __alloc__$Alloc 26
#define __resize__$Alloc 27
#define __free__$Alloc 28

typedef struct LilyBuiltinFun
{
    const char *name;
    // The `real_name` is the name that was given in the C include file at the
    // path: `lib/builtin/*.h`
    String *real_name;
    LilyCheckedDataType *return_data_type;
    Vec *params; // Vec<LilyCheckedDataType*>*
} LilyBuiltinFun;

/**
 *
 * @brief Function to load Lily builtin.
 */
LilyBuiltinFun *
load_builtins__LilyBuiltin();

/**
 *
 * @brief Return true if is a builtin function.
 */
bool
is_builtin_function__LilyBuiltin(const char *name);

/**
 *
 * @brief Get builtin function depending on the parameters of the function and
 * the type of return.
 */
const LilyBuiltinFun *
get_builtin__LilyBuiltin(LilyBuiltinFun *builtins,
                         const char *name,
                         Vec *params);

/**
 *
 * @brief Check if both builtin funs are equal.
 */
bool
eq__LilyBuiltin(const LilyBuiltinFun *self, const LilyBuiltinFun *other);

/**
 *
 * @brief Convert LilyBuiltinFun in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, LilyBuiltinFun, const LilyBuiltinFun *self);
#endif

/**
 *
 * @brief Free LilyBuiltinFun type.
 */
DESTRUCTOR(LilyBuiltinFun, const LilyBuiltinFun *self);

#endif // LILY_CORE_LILY_FUNCTIONS_BUILTIN_H
