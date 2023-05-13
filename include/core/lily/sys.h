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

#ifndef LILY_CORE_LILY_SYS_H
#define LILY_CORE_LILY_SYS_H

#include <core/lily/checked/data_type.h>

#include <stdbool.h>

#define SYSS_COUNT 4

typedef struct LilySysFun
{
    const char *name;
    // The `real_name` is the name that was given in the C include file at the
    // path: `lib/sys.h`
    String *real_name;
    LilyCheckedDataType *return_data_type;
    Vec *params; // Vec<LilyCheckedDataType*>*
} LilySysFun;

/**
 *
 * @brief Function to load Lily sys.
 */
LilySysFun *
load_syss__LilySys();

/**
 *
 * @brief Return true if is a sys function.
 */
bool
is_sys_function__LilySys(const char *name);

/**
 *
 * @brief Get the signature of a sys function according the name.
 */
const LilySysFun *
get_sys__LilySys(LilySysFun *syss, const char *name);

/**
 *
 * @brief Convert LilySysFun in String.
 * @note This function is only used to debug.
 */
#ifdef ENV_DEBUG
String *
IMPL_FOR_DEBUG(to_string, LilySysFun, const LilySysFun *self);
#endif

/**
 *
 * @brief Free LilySysFun type.
 */
DESTRUCTOR(LilySysFun, const LilySysFun *self);

#endif // LILY_CORE_LILY_SYS_H
